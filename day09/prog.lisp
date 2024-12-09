(ql:quickload :cl-ppcre)
(ql:quickload :split-sequence)
(ql:quickload :lisp-utils)
(ql:quickload :alexandria)

(use-package :lisp-utils)

(defun parse (file-name)
  (let* ((lines (uiop:read-file-lines file-name)))
    (cl-ppcre:split "" (first lines))))

(defun create-compact-map (disk-map)
  (let ((compact-map nil)
        (is-file-identifier t)
        (file-id 0))
    (loop for disk-length in disk-map
          do (progn
               (loop for l below (parse-integer disk-length)
                     do (progn
                          (if is-file-identifier
                              (push (write-to-string file-id) compact-map)
                              (push "." compact-map))))
               (when is-file-identifier
                 (incf file-id))
               (setf is-file-identifier (not is-file-identifier))))
    (nreverse compact-map)))

(defun is-compact-p (map)
  (let ((head (take-while map (lambda (chunk)
                                (not (equal chunk ".")))))
        (tail (take-while (reverse map) (lambda (chunk)
                                          (equal chunk ".")))))
    (= (+ (length head) (length tail)) (length map))))

(defun compact (map)
  (let ((reversed-map (reverse map)))
    (loop while (not (is-compact-p map))
          do (progn
               ;(print map)
               (loop for chunk in reversed-map
                    for chunk-index downfrom (1- (length map))
                    when (not (equal chunk "."))
                      do (progn
                           ;(break)
                           (let ((empty-index (position "." map :test #'equal)))
                             (when (and empty-index
                                        (< empty-index chunk-index))
                               (rotatef (nth chunk-index map) (nth empty-index map))))))))
    map))

(defun part1 (file-name)
  (let* ((disk-map (parse file-name))
         (compact-map (compact (create-compact-map disk-map))))
    (loop for chunk in compact-map
          for i from 0
          if (not (equal chunk "."))
          summing (* (parse-integer chunk) i))))

(defun part2 (file-name)
  (let* (( (parse file-name)))))

(print (part1 "input0.txt"))
(print (part1 "input1.txt"))

; (print (part2 "input0.txt"))
; (print (part2 "input1.txt"))



