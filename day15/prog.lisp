(ql:quickload :cl-ppcre)
(ql:quickload :split-sequence)
(ql:quickload :lisp-utils)
(ql:quickload :alexandria)

(require :sb-concurrency)
(use-package :sb-concurrency)

(use-package :lisp-utils)

(defun join-strings (strings &optional (delimiter ""))
  (format nil "~{~a~}" strings delimiter))

(defun parse (file-name &optional (fn #'identity))
  (let* ((lines (uiop:read-file-lines file-name))
         (grid (make-hash-table :test #'equal)))
    (destructuring-bind (map moves) (split-sequence-into-sections lines)
      (loop for row in map
            for r from 0
            do (loop for col in (cl-ppcre:split "" row)
                     for c from 0
                     do (setf (gethash (cons r c) grid) (funcall fn col))))
      (let ((moves-in-one-line (cl-ppcre:split "" (join-strings moves))))
        (values grid moves-in-one-line)))))

(defun swap-items (from to grid)
  (rotatef (gethash from grid)
           (gethash to grid)))

(defun print-grid (grid max-x max-y)
  "Prints the grid stored in the hash table."
  (terpri)
  (loop for y from 0 below max-y do
        (loop for x from 0 below max-x do
              (let ((value (gethash (cons y x) grid nil)))
                (format t "~a " (or value "."))))
        (terpri)))

(defun move-forward (pos dir grid)
  (let* ((next-pos (cond ((equal dir "<")
                          (cons (car pos)
                                (1- (cdr pos))))
                         ((equal dir "^")
                          (cons (1- (car pos))
                                (cdr pos)))
                         ((equal dir ">")
                          (cons (car pos)
                                (1+ (cdr pos))))
                         ((equal dir "v")
                          (cons (1+ (car pos))
                                (cdr pos)))))
         (next (gethash next-pos grid)))
    (if next
        (cond ((equal next ".")
               (swap-items pos next-pos grid)
               next-pos)
              ((equal next "O")
               (let ((ret-pos (move-forward next-pos dir grid)))
                 (if (not (equal ret-pos next-pos))
                     (progn
                       (swap-items pos next-pos grid)
                       next-pos)
                     pos)))
              ((equal next "#")
               pos)))))

(defun compute-gps (grid)
  (loop for pos being the hash-keys of grid
        using (hash-value val)
        if (equal val "O")
          summing (+ (* (car pos) 100)
                     (cdr pos))))

(defun part1 (file-name)
  (multiple-value-bind (grid moves) (parse file-name)
    (let* ((robot (loop for pos being the hash-keys of grid
                        using (hash-value val)
                        if (equal val "@")
                          return pos)))
      (print-grid grid 8 8)
      (loop for dir in moves
            do (let ((new-pos (move-forward robot dir grid)))
                 (setf robot new-pos)
                 ;(print-grid grid 8 8)
                 ))
      (print-grid grid 8 8)
      (compute-gps grid))))

(defun part2 (file-name)
  (let* ((data (parse file-name)))))

(time (print (part1 "input0.txt")))
(time (print (part1 "input1.txt")))

; (time (print (part2 "input0.txt")))
; (time (print (part2 "input1.txt")))



