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

(defun chunk-len (start end)
  (1+ (- end start)))

(defun compact-2 (map)
  (let* ((chunks (find-chunks map)))
    (multiple-value-bind (file-chunks empty-chunks)
        (partition chunks (lambda (chunk)
                            (not (equal (caar chunk) "."))))
     (loop for file-chunk in (reverse file-chunks)
           do (let* ((chunk-start (cadr file-chunk))
                     (chunk-end (cddr file-chunk))
                     (chunk-length (chunk-len chunk-start chunk-end))
                     (available-empty-chunk (loop for empty-chunk in empty-chunks
                                                  if (>= (chunk-len (cadr empty-chunk)
                                                                    (cddr empty-chunk))
                                                         chunk-length)
                                                    return empty-chunk)))
                (when available-empty-chunk
                  (let* ((empty-start (cadr available-empty-chunk))
                         (empty-end (cddr available-empty-chunk))
                         (empty-length (chunk-len empty-start empty-end)))
                    (rotatef (subseq map chunk-start (1+ chunk-end))
                             (subseq map empty-start (+ empty-start chunk-length)))
                    (if (= chunk-length empty-length)
                        (setf empty-chunks (remove available-empty-chunk empty-chunks))
                        (let* ((empty-pos (position available-empty-chunk empty-chunks))
                               (new-start (+ empty-start chunk-length))
                               (new-empty-chunk (cons (cadr available-empty-chunk)
                                                      (cons new-start (cddr available-empty-chunk)))))
                          (setf (nth empty-pos empty-chunks) new-empty-chunk))))))))
    map))

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
    (loop for chunk in compact-map
          for i from 0
          if (not (equal chunk "."))
            summing (* (parse-integer chunk) i))))

;;(print (part1 "input0.txt"))
;;(print (part1 "input1.txt"))

(print (part2 "input0.txt"))
(print (part2 "input1.txt"))

;; too high: 8551696246309


