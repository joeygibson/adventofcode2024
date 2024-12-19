;(require :sb-concurrency)
;(use-package :sb-concurrency)

(require :cl-ppcre)
(require :split-sequence)
(require :lisp-utils)
(require :alexandria)
(require :lisp-utils)
(use-package :lisp-utils)

(defun parse (file-name)
  (let* ((lines (uiop:read-file-lines file-name)))
    (values (cl-ppcre:split ", " (first lines))
            (cddr lines))))

(defun-memo is-possible (towels design i)
  (if (= i (length design))
      1
      (let ((res 0))
        ;(format t "~&design: ~a, i: ~a, s: ~a~%" design i (subseq design i))
        (loop for towel in towels
              do (when (cl-ppcre:scan (format nil "^~a" towel)
                                      (subseq design i))
                   (incf res (is-possible towels design (+ i (length towel))))))
        res)))

(defun part1 (file-name)
  (multiple-value-bind (towels designs) (parse file-name)
    (let ((possible 0))
      (loop for design in designs
            do (let ((res (is-possible towels design 0)))
                 ;(format t "~&design: ~a, res: ~a~%" design res)
                 (when (> res 0)
                   (incf possible))))
      possible)))

(defun part2 (file-name)
  (let* ((data (parse file-name)))))

;(time (format t "~&part1: ~a~%" (part1 "input0.txt")))
(time (format t "~&part1: ~a~%" (part1 "input1.txt")))
;; (time (format t "~&part2: ~a~%" (part2 "input0.txt")))
;; (time (format t "~&part2: ~a~%" (part2 "input1.txt")))


