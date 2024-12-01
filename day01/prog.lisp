(ql:quickload :cl-ppcre)
(ql:quickload :split-sequence)
(ql:quickload :lisp-utils)
(ql:quickload :alexandria)

(use-package :lisp-utils)

(defun parse (file-name)
  (let* ((lines (uiop:read-file-lines file-name))
         (list0 nil)
         (list1 nil))
    (dolist (line lines)
      (let ((chunks (cl-ppcre:split "\\s+" line)))
        (push (parse-integer (first chunks)) list0)
        (push (parse-integer (second chunks)) list1)))
    (list (sort list0 #'<)
          (sort list1 #'<))))

(defun compute-diffs (list0 list1)
  (loop for i0 in list0
        for i1 in list1
        collecting (abs (- i0 i1))))

(defun part1 (file-name)
  (destructuring-bind (list0 list1) (parse file-name)
    (apply #'+ (compute-diffs list0 list1))))

(defun part2 (file-name)
  (let ((count-occurrences (lambda (num lst)
                             (count num lst))))
    (destructuring-bind (list0 list1) (parse file-name)
      (apply #'+ (mapcar (lambda (num)
                           (let ((occurrences (funcall count-occurrences num list1)))
                             (* num occurrences)))
                         list0)))))

(print (part1 "input0.txt"))
(print (part1 "input1.txt"))

(print (part2 "input0.txt"))
(print (part2 "input1.txt"))


