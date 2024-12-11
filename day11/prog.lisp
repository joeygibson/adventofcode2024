(ql:quickload :cl-ppcre)
(ql:quickload :split-sequence)
(ql:quickload :lisp-utils)
(ql:quickload :alexandria)
(use-package :lisp-utils)

(defclass stone ()
  ((num :initarg :num :accessor num)))

(defun make-stone (num)
  (make-instance 'stone :num num))

(defmethod has-even-digits ((self stone))
  (= (mod (length (num self)) 2) 0))

(defmethod split-stone ((self stone))
  (let* ((middle (/ (length (num self)) 2))
         (left (subseq (num self) 0 middle))
         (right (string-left-trim "0 "(subseq (num self) middle))))
    (values (make-stone left)
            (make-stone (if (not (equal right ""))
                            right
                            "0")))))

(defmethod print-object ((self stone) stream)
  (print-unreadable-object (self stream)
    (format stream "~s num: ~a" (type-of self) (num self))))

(defmethod multiply ((self stone))
  (let* ((num-as-num (parse-integer (num self)))
         (new-num (write-to-string (* num-as-num 2024))))
    (setf (num self) new-num)
    self))

(defun parse-to-grid (file-name &optional (fn #'identity))
  (let* ((lines (uiop:read-file-lines file-name))
         (grid (make-hash-table :test #'equal)))
    (loop for row in lines
          for r from 0
          do (loop for col in (cl-ppcre:split "" row)
                   for c from 0
                   do (setf (gethash (cons r c) grid) (funcall fn col))))
    grid))

(let ((l (make-stone "2345")))
  (multiple-value-bind (left right) (split-stone l)
    (print left)
    (print right)))

(defun parse (file-name)
  (let* ((lines (uiop:read-file-lines file-name)))
    (cl-ppcre:split " " (first lines))))

(defun part1 (file-name times)
  (let* ((stones (mapcar #'make-stone (parse file-name))))
    (loop repeat times
          do (let ((new-stones nil))
               (loop for stone in stones
                     do (cond ((equal (num stone) "0")
                               (push (make-stone "1") new-stones))
                              ((has-even-digits stone)
                               (multiple-value-bind (left right) (split-stone stone)
                                 (push left new-stones)
                                 (push right new-stones)))
                              (t (push (multiply stone) new-stones)))
                        )
               (setf stones (reverse new-stones))))
    (length stones)))

(defun part2 (file-name)
  (let* ((data (parse file-name)))))

(print (part1 "input1.txt" 25))


                                        ; (print (part1 "input1.txt"))

                                        ; (print (part2 "input0.txt"))
                                        ; (print (part2 "input1.txt"))
