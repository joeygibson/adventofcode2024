;(require :sb-concurrency)
;(use-package :sb-concurrency)

(require :cl-ppcre)
(require :split-sequence)
(require :lisp-utils)
(require :alexandria)
(require :lisp-utils)
(use-package :lisp-utils)

(defun make-thing (lines)
  (let* ((thing (make-array 5 :initial-element 0)))
    (loop for line in (rest lines)
          for i from 1
          do (loop for c in (cl-ppcre:split "" line)
                   for col from 0
                   do (when (string= c "#")
                        (setf (aref thing col) i))))
    thing))

(defun fits-p (lock key)
  (every (lambda (l k)
           (<= (+ l k) 5))
         lock key))

(defun parse (file-name)
  (let* ((sections (split-file-into-sections file-name))
         (locks nil)
         (keys nil))
    (loop for section in sections
          do (if (every (lambda (c) (char= c #\#)) (first section))
                 (push (make-thing section) locks)
                 (push (make-thing (reverse section)) keys)))
    (values locks keys)))

(defun part1 (file-name)
  (multiple-value-bind (locks keys) (parse file-name)
    (let ((fits 0))
      (loop for lock in locks
            do (loop for key in keys
                     do (when (fits-p lock key)
                          (incf fits))))
      fits)))

(time (format t "~&part1: ~a~%" (part1 "input1.txt")))

