(ql:quickload :cl-ppcre)
(ql:quickload :split-sequence)
(ql:quickload :lisp-utils)
(ql:quickload :alexandria)

; (ql:quickload :queues)

; (require :queues.simple-queue)


(use-package :lisp-utils)

(defun parse (file-name)
  (let* ((lines (uiop:read-file-lines file-name))
         (puzzle (make-hash-table :test #'equal)))
    (loop for line in lines
          for j from 0
          do (loop for ch in (cl-ppcre:split "" line)
                   for i from 0
                   do (setf (gethash (cons i j) puzzle) ch)))
    puzzle))

(defun find-all-starts (puzzle letter)
  (let ((xs nil))
    (maphash (lambda (k v)
               (when (equal v letter)
                 (push k xs)))
             puzzle)
    (nreverse xs)))

(defun get-neighbors (spot)
  (let* ((neighbors nil)
         (x (car spot))
         (y (cdr spot)))
    (mapc (lambda (lst)
            (push lst neighbors))
          (multiple-value-list (loop for i from 0 to 3
                                     collecting (cons (+ x i) y) into right
                                     collecting (cons (- x i) y) into left
                                     collecting (cons x (- y i)) into up
                                     collecting (cons x (+ y i)) into down
                                     collecting (cons (- x i) (- y i)) into up-left
                                     collecting (cons (+ x i) (- y i)) into up-right
                                     collecting (cons (- x i) (+ y i)) into down-left
                                     collecting (cons (+ x i) (+ y i)) into down-right
                                     finally (return (values right left up down up-left up-right down-left down-right)))))))

(defstruct x-mas top-left top-right bottom-left bottom-right)

(defun get-neighbors-part-2 (spot)
  (let* ((neighbors nil)
         (x (car spot))
         (y (cdr spot))
         (top-left-corner (cons (- x 1) (- y 1)))
         (top-right-corner (cons (+ x 1) (- y 1)))
         (bottom-left-corner (cons (- x 1) (+ y 1)))
         (bottom-right-corner (cons (+ x 1) (+ y 1))))
    (push (make-x-mas :top-left top-left-corner
                      :top-right top-right-corner
                      :bottom-left bottom-left-corner
                      :bottom-right bottom-right-corner)
          neighbors)
    neighbors))

(defun xmas-p (puzzle coords letters)
  (let ((matches 0))
    (loop for l in letters
          for spot in coords
          do (when (equal (gethash spot puzzle ".") l)
               (incf matches)))
    (eq matches (length letters))))

(defun part1 (file-name)
  (let* ((puzzle (parse file-name))
         (xs (find-all-starts puzzle "X"))
         (matches nil))
    (dolist (spot xs)
      (let ((neighbors (get-neighbors spot)))
        (loop for neighbor in neighbors
              if (xmas-p puzzle neighbor '("X" "M" "A" "S"))
                do (push neighbor matches))))
    (length matches)))

(defun check-xmas (puzzle xmas)
  (let* ((top-left (gethash (x-mas-top-left xmas) puzzle "."))
         (top-right (gethash (x-mas-top-right xmas) puzzle "."))
         (bottom-left (gethash (x-mas-bottom-left xmas) puzzle "."))
         (bottom-right (gethash (x-mas-bottom-right xmas) puzzle ".")))
    (cond ((and (and (equal top-left "M")
                     (equal bottom-right "S"))
                (and (equal top-right "M")
                     (equal bottom-left "S")))
           t)
          ((and (and (equal top-left "S")
                     (equal bottom-right "M"))
                (and (equal top-right "S")
                     (equal bottom-left "M")))
           t)
          ((and (and (equal top-left "M")
                     (equal bottom-right "S"))
                (and (equal top-right "S")
                     (equal bottom-left "M")))
           t)
          ((and (and (equal top-left "S")
                     (equal bottom-right "M"))
                (and (equal top-right "M")
                     (equal bottom-left "S")))
           t)
          (t nil))))

(defun part2 (file-name)
  (let* ((puzzle (parse file-name))
         (xs (find-all-starts puzzle "A"))
         (matches nil))
    (dolist (spot xs)
      (let ((neighbors (get-neighbors-part-2 spot)))
        (loop for neighbor in neighbors
              if (check-xmas puzzle neighbor)
                do (push neighbor matches))))
    (length matches)))

(print (part1 "input0.txt"))
(print (part1 "input1.txt"))

(print (part2 "input0.txt"))
(print (part2 "input1.txt"))




