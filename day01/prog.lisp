(ql:quickload :cl-ppcre)
(ql:quickload :split-sequence)
(ql:quickload :lisp-utils)
(ql:quickload :alexandria)

; (ql:quickload :queues)

; (require :queues.simple-queue)


(use-package :lisp-utils)

(defun parse (lines)
  (let ((list0 nil)
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
  (let* ((lines (uiop:read-file-lines file-name)))
    (destructuring-bind (list0 list1) (parse lines)
      (apply #'+ (compute-diffs list0 list1)))))

(defvar cache (make-hash-table))

(defun count-occurrences (num lst)
  (let ((val (gethash num cache)))
    (if val
        val
        (progn
          (let ((val (count num lst)))
            (setf (gethash num cache) val)
            val)))))


(defun part2 (file-name)
  (let* ((lines (uiop:read-file-lines file-name)))
    (destructuring-bind (list0 list1) (parse lines)
      (apply #'+ (mapcar (lambda (num)
                           (let ((occurrences (count-occurrences num list1)))
                             (* num occurrences)))
                         list0)))))


(part1 "input0.txt")
(part1 "input1.txt")

(part2 "input0.txt")
(part2 "input1.txt")


