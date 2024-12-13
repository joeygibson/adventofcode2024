(ql:quickload :cl-ppcre)
(ql:quickload :split-sequence)
(ql:quickload :lisp-utils)
(ql:quickload :alexandria)

(require :sb-concurrency)
(use-package :sb-concurrency)

(use-package :lisp-utils)

(defconstant *a-cost* 3)
(defconstant *b-cost* 1)

(defclass claw ()
  ((a-x :initarg :a-x :reader a-x :type integer)
   (a-y :initarg :a-y :reader a-y :type integer)
   (b-x :initarg :b-x :reader b-x :type integer)
   (b-y :initarg :b-y :reader b-y :type integer)
   (prize-x :initarg :prize-x :reader prize-x :type integer)
   (prize-y :initarg :prize-y :reader prize-y :type integer)))

(defmethod print-object ((self claw) stream)
  (print-unreadable-object (self stream)
    (format stream "~s A(~a, ~a), B(~a, ~a), Prize(~a, ~a)"
              (type-of self) (a-x self) (a-y self)
              (b-x self) (b-y self)
              (prize-x self) (prize-y self))))


(defun make-claw (a b prize multiplier)
  (let ((p-x (parse-integer (car prize)))
        (p-y (parse-integer (cadr prize))))
    (make-instance 'claw :a-x (parse-integer (car a))
                               :a-y (parse-integer (cadr a))
                               :b-x (parse-integer (car b))
                               :b-y (parse-integer (cadr b))
                               :prize-x (+ p-x multiplier)
                               :prize-y (+ p-y multiplier))))

(defun create-claw-machines (file-name &key is-part-2)
  (let* ((specs (split-file-into-sections file-name)))
    (mapcar (lambda (section)
              (let* ((a (cl-ppcre:all-matches-as-strings "(\\d+)" (first section)))
                     (b (cl-ppcre:all-matches-as-strings "(\\d+)" (second section)))
                     (prize (cl-ppcre:all-matches-as-strings "(\\d+)" (third section))))
                (make-claw a b prize (if is-part-2
                                         10000000000000
                                         0))))
            specs)))

(defmethod determine-button-presses ((self claw) max-iterations)
  (with-slots (a-x a-y b-x b-y prize-x prize-y) self
    (let ((res (loop named outer
                     for n-a below max-iterations
                     do (loop named inner
                              for n-b below max-iterations
                              do (let* ((current-x (+ (* n-a a-x)
                                                      (* n-b b-x)))
                                        (current-y (+ (* n-a a-y)
                                                      (* n-b b-y))))
                                   (when (and (= current-x prize-x)
                                              (= current-y prize-y))
                                     (return-from outer (list n-a n-b))))))))
      res)))

; this function borrowed from https://pastebin.com/TRW9CyX7
(defmethod determine-button-presses-huge ((self claw))
  (with-slots (a-x a-y b-x b-y prize-x prize-y) self
    (let* ((n-b (truncate (- (* a-y prize-x)
                             (* a-x prize-y))
                          (- (* a-y b-x)
                             (* a-x b-y))))
           (n-a (truncate (- prize-x (* n-b b-x)) a-x)))
      (if (and (= (+ (* n-a a-x) (* n-b b-x)) prize-x)
               (= (+ (* n-a a-y) (* n-b b-y)) prize-y))
          (+ (* 3 n-a) n-b)
          0))))

(defun compute-cost (result)
  (let* ((a (car result))
         (b (cadr result)))
    (+ (* a *a-cost*)
       (* b *b-cost*))))

(defun part1 (file-name)
  (let* ((machines (create-claw-machines file-name))
         (winners nil))
    (dolist (machine machines)
      (let ((result (determine-button-presses machine 100)))
        (when result
          (push result winners))))
    (reduce #'+ (mapcar #'compute-cost winners))))

(defun part2 (file-name)
  (let* ((machines (create-claw-machines file-name :is-part-2 t))
         (winners nil))
    (dolist (machine machines)
      (let ((result (determine-button-presses-huge machine)))
        (when result
          (push result winners))))
    (reduce #'+ winners)))

(time (print (part1 "input0.txt")))
(time (print (part1 "input1.txt")))

(time (print (part2 "input0.txt")))
(time (print (part2 "input1.txt")))



