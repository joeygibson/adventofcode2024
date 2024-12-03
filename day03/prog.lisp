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
    (mapcar (lambda (line)
              (let ((matches nil)
                    (hits (cl-ppcre:scan-to-strings "mul(\\d+,\\d+)" line)))
                (print hits)))
            data)))

(defun part2 (file-name)
  (let* ((data (parse file-name)))))

(print (part1 "input0.txt"))
; (print (part1 "input1.txt"))

; (print (part2 "input0.txt"))
; (print (part2 "input1.txt"))



