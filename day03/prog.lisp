(ql:quickload :cl-ppcre)
(ql:quickload :split-sequence)
(ql:quickload :lisp-utils)
(ql:quickload :alexandria)

; (ql:quickload :queues)

; (require :queues.simple-queue)


(use-package :lisp-utils)

(defun parse (file-name)
  (let* ((lines (uiop:read-file-lines file-name)))
    lines))

(defun part1 (file-name)
  (let* ((data (parse file-name)))
    (apply #'+ (mapcar (lambda (line)
                         (let ((products nil))
                           (cl-ppcre:do-register-groups (match) ("(mul\\(\\d+,\\d+\\))" line)
                             (destructuring-bind (a b) (cl-ppcre:all-matches-as-strings "\\d+" match)
                               (push (* (parse-integer a)
                                        (parse-integer b))
                                     products)))
                           (apply #'+ products)))
                       data))))

(defun part2 (file-name)
  (let* ((data (parse file-name))
         (enabled t))
    (apply #'+ (mapcar (lambda (line)
                         (let ((products nil))
                           (cl-ppcre:do-register-groups (match) ("(mul\\(\\d+,\\d+\\)|do\\(\\)|don't\\(\\))" line)
                             (cond ((equal match "do()")
                                    (setf enabled t))
                                   ((equal match "don't()")
                                    (setf enabled nil))
                                   (t
                                    (when enabled
                                      (destructuring-bind (a b) (cl-ppcre:all-matches-as-strings "\\d+" match)
                                        (push (* (parse-integer a)
                                                 (parse-integer b))
                                              products))))))
                           (apply #'+ products)))
                       data))))

(print (part1 "input0.txt"))
(print (part1 "input1.txt"))

(print (part2 "input2.txt"))
(print (part2 "input1.txt"))



