(ql:quickload :cl-ppcre)
(ql:quickload :split-sequence)
(ql:quickload :lisp-utils)
(ql:quickload :alexandria)

(use-package :alexandria)
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
                           (let ((empty-index (position "." map :test #'equal)))
                             (when (and empty-index
                                        (< empty-index chunk-index))
                               (rotatef (nth chunk-index map) (nth empty-index map))))))))
    map))

(defun find-chunks (map)
  (let* ((chunks nil)
         (current-val (first map))
         (current-chunk-start 0)
         (current-chunk (list current-val)))
    (loop for i from 1 to (length map)
          do (let ((chr (nth i map)))
               (if (equal chr current-val)
                   (push chr current-chunk)
                   (progn
                     (push (cons (reverse current-chunk)
                                 (cons current-chunk-start (1- i)))
                           chunks)
                     (setf current-val chr)
                     (setf current-chunk (list chr))
                     (setf current-chunk-start i)))))
    (reverse chunks)))

(defun chunk-len (chunk)
  (1+ (- (cddr chunk)
         (cadr chunk))))

(defun max-file-id (map)
  (let* ((ids (remove-if (lambda (id)
                           (equal id "."))
                         map))
         (numeric-ids (mapcar #'parse-integer ids)))
    (apply #'max numeric-ids)))

(defun compact-2 (map)
  (let* ((file-chunks (partition (find-chunks map)
                                 (lambda (chunk)
                                   (not (equal (caar chunk) "."))))))
    (loop for chunk in (reverse file-chunks)
          do (progn
               (print (caar chunk))
               (let* ((chunks (find-chunks map)))
                (multiple-value-bind (_ empty-chunks)
                    (partition chunks (lambda (chunk)
                                        (not (equal (caar chunk) "."))))
                  (when-let ((empty (first (remove-if (lambda (chnk)
                                                        (< (chunk-len chnk)
                                                           (chunk-len chunk)))
                                                      empty-chunks))))
                    (when (< (cddr empty)
                             (cadr chunk))
                      (rotatef (subseq map (cadr empty) (1+ (cddr empty)))
                               (subseq map (cadr chunk) (1+ (cddr chunk))))))))))
    map))

;(print (part2 "input0.txt"))

(defun part1 (file-name)
  (let* ((disk-map (parse file-name))
         (compact-map (compact (create-compact-map disk-map))))
    (loop for chunk in compact-map
          for i from 0
          if (not (equal chunk "."))
          summing (* (parse-integer chunk) i))))

(defun part2 (file-name)
  (let* ((disk-map (parse file-name))
         (compact-map (compact-2 (create-compact-map disk-map))))
    (print "part2")
    (loop for chunk in compact-map
          for i from 0
          if (not (equal chunk "."))
            summing (* (parse-integer chunk) i))))

;;(print (part1 "input0.txt"))
;;(print (part1 "input1.txt"))

;(print (part2 "input0.txt"))
(print (part2 "input1.txt"))




;; too high: 8551696246309


