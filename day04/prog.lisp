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
    (values puzzle (length (first lines)) (length lines))))

(defun find-all-xs (puzzle)
  (let ((xs nil))
    (maphash (lambda (k v)
               (when (equal v "X")
                 (push k xs)))
             puzzle)
    (nreverse xs)))

(defun get-neighbors (spot width height)
  (let* ((neighbors nil)
         (x (car spot))
         (y (cdr spot))
         (up-room (>= (- y 3) 0))
         (down-room (< (+ y 3) height))
         (left-room (>= (- x 3) 0))
         (right-room (< (+ x 3) width)))
    (when right-room
      (push (loop for i from 0 to 3
                  collecting (cons (+ x i) y))
            neighbors))
    (when left-room
      (push (loop for i from 0 to 3
                  collecting (cons (- x i) y))
            neighbors))
    (when up-room
      (push (loop for i from 0 to 3
                  collecting (cons x (- y i)))
            neighbors))
    (when down-room
      (push (loop for i from 0 to 3
                  collecting (cons x (+ y i)))
            neighbors))
    (when (and up-room left-room) ;; up, diagonal, left
      (push (loop for i from 0 to 3
                  collecting (cons (- x i) (- y i)))
            neighbors))
    (when (and up-room right-room) ;; up, diagonal, right
      (push (loop for i from 0 to 3
                  collecting (cons (+ x i) (- y i)))
            neighbors))
    (when (and down-room left-room) ;; down, diagonal, left
      (push (loop for i from 0 to 3
                  collecting (cons (- x i) (+ y i)))
            neighbors))
    (when (and down-room right-room) ;; down, diagonal, right
      (push (loop for i from 0 to 3
                  collecting (cons (+ x i) (+ y i)))
            neighbors))
    neighbors))

(defun xmas-p (puzzle coords)
  (let ((matches 0))
    (loop for l in '("X" "M" "A" "S")
          for spot in coords
          do (when (equal (gethash spot puzzle) l)
               (incf matches)))
    (eq matches 4)))

(defun part1 (file-name)
  (multiple-value-bind (puzzle width height) (parse file-name)
    (let* ((xs (find-all-xs puzzle))
           (matches nil))
      (dolist (spot xs)
        (let ((neighbors (get-neighbors spot width height)))
          (loop for neighbor in neighbors
                if (xmas-p puzzle neighbor)
                  do (push neighbor matches))))
      (length matches))))

(defun part2 (file-name)
  (let* ((data (parse file-name)))))

(print (part1 "input0.txt"))
(print (part1 "input1.txt"))

; (print (part2 "input0.txt"))
; (print (part2 "input1.txt"))

(multiple-value-bind (puzzle width height) (parse "input0.txt")
  (let* ((xs (find-all-xs puzzle))
         (neighbors (get-neighbors (first xs) width height)))
    (print (get-neighbors (cons 5 0) width height))))


