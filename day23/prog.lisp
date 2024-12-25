;(require :sb-concurrency)
;(use-package :sb-concurrency)

(require :cl-ppcre)
(require :split-sequence)
(require :lisp-utils)
(require :alexandria)
;(require :lisp-utils)
;(use-package :lisp-utils)

(defun parse (file-name)
  (let* ((lines (uiop:read-file-lines file-name)))
    (mapcar (lambda (line)
              (cl-ppcre:split "-" line))
            lines)))

(defun make-combination-generator (list n)
  "Create a generator that lazily produces combinations of N elements from LIST."
  (let ((stack (list (list '() list n)))) ; Initialize the stack with the first state
    (lambda ()
      "Return the next combination or NIL when done."
      (loop while stack do
            (destructuring-bind (prefix rest k) (pop stack)
              (cond
                ;; If k = 0, we have a complete combination; return it.
                ((= k 0)
                 (return prefix))
                ;; If rest is empty but k > 0, skip this state.
                ((null rest)
                 nil)
                ;; Otherwise, push new states onto the stack:
                (t
                 ;; Push state excluding the first element.
                 (push (list prefix (cdr rest) k) stack)
                 ;; Push state including the first element.
                 (push (list (cons (car rest) prefix) (cdr rest) (1- k)) stack))))))))

(defun part1 (file-name)
  (let* ((map (make-hash-table :test #'equal)))
    (loop for (a b) in (parse file-name)
          do (progn
               (pushnew b (gethash a map) :test #'equal)
               (pushnew a (gethash b map) :test #'equal)))
    (let ((combo-generator (make-combination-generator (alexandria:hash-table-keys map) 3))
          (matches nil))
      (loop for combo = (funcall combo-generator)
            while combo
            do (progn
                 (let* ((a (first combo))
                       (b (second combo))
                       (c (third combo))
                       (a-conns (gethash a map))
                       (b-conns (gethash b map))
                       (c-conns (gethash c map)))
                  (when (and (member a b-conns :test #'equal)
                             (member a c-conns :test #'equal)
                             (member b a-conns :test #'equal)
                             (member b c-conns :test #'equal)
                             (member c a-conns :test #'equal)
                             (member c b-conns :test #'equal))
                    (push combo matches)))))
      (length (remove-if-not (lambda (combo)
                               (find-if (lambda (machine)
                                          (char= (char machine 0) #\t))
                                        combo))
                             matches)))))

(time (format t "~&part1: ~a~%" (part1 "input0.txt")))
(time (format t "~&part1: ~a~%" (part1 "input1.txt")))



