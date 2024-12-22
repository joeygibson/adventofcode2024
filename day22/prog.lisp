;(require :sb-concurrency)
;(use-package :sb-concurrency)

(require :cl-ppcre)
(require :split-sequence)
(require :lisp-utils)
(require :alexandria)
(require :lisp-utils)
(use-package :lisp-utils)

(defun parse (file-name)
  (let* ((lines (mapcar #'parse-integer (uiop:read-file-lines file-name))))
    lines))

(defun step1 (num)
  (mod (logxor num (* num 64)) 16777216))

(defun step2 (num)
  (mod (logxor num (floor num 32)) 16777216))

(defun step3 (num)
  (mod (logxor num (* num 2048)) 16777216))

(defun-memo compute-next-secret (vendor secret)
  (step3 (step2 (step1 secret))))

(defun compute-secrets (vendor secret count)
  (let* ((secrets (list secret)))
    (loop for i below count
          do (progn
               (pushnew (compute-next-secret vendor (first secrets)) secrets)))
    secrets))

(defun part1 (file-name)
  (let ((secrets (parse file-name)))
    (reduce #'+ (loop for vendor from 0
                      for secret in secrets
                      collecting (first (compute-secrets vendor secret 2000))))))



;(time (format t "~&part1: ~a~%" (part1 "input0.txt")))
(time (format t "~&part1: ~a~%" (part1 "input1.txt")))


