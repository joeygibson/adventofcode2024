;(require :sb-concurrency)
;(use-package :sb-concurrency)

(require :cl-ppcre)
(require :split-sequence)
(require :lisp-utils)
(require :alexandria)
(require :lisp-utils)
(use-package :lisp-utils)

(defun parse-to-grid (file-name &optional (fn #'identity))
  (let* ((lines (uiop:read-file-lines file-name))
         (grid (make-hash-table :test #'equal)))
    (loop for row in lines
          for r from 0
          do (loop for col in (cl-ppcre:split "" row)
                   for c from 0
                   do (setf (gethash (cons r c) grid) (funcall fn col))))
    grid))

(defun parse (file-name)
  (let* ((lines (uiop:read-file-lines file-name)))))

(defun part1 (file-name)
  (let* ((data (parse file-name)))))

(defun part2 (file-name)
  (let* ((data (parse file-name)))))

(time (format t "~&part1: ~a~%" (part1 "input0.txt")))
(time (format t "~&part1: ~a~%" (part1 "input1.txt")))
(time (format t "~&part2: ~a~%" (part2 "input0.txt")))
(time (format t "~&part2: ~a~%" (part2 "input1.txt")))


