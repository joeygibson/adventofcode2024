(ql:quickload :cl-ppcre)
(ql:quickload :split-sequence)
(ql:quickload :lisp-utils)
(ql:quickload :alexandria)

; (ql:quickload :queues)

; (require :queues.simple-queue)


(use-package :lisp-utils)

(defun parse (file-name)
  (let* ((lines (uiop:read-file-lines file-name))
         (map (make-hash-table :test #'equal))
         (antennas nil)
         (height (length lines))
         (width (length (first lines))))
    (loop for row in lines
          for r from 0
          do (loop for col in (cl-ppcre:split "" row)
                   for c from 0
                   do (let ((spot (cons c r)))
                        (when (not (equal col "."))
                          (push (cons spot col) antennas))
                        (setf (gethash spot map) (list col)))))
    (values map antennas width height)))

(defun taxicab-distance (spot1 spot2)
  (let* ((x1 (car spot1))
         (y1 (cdr spot1))
         (x2 (car spot2))
         (y2 (cdr spot2)))
    (+ (abs (- x2 x1))
       (abs (- y2 y1)))))

(defun plot-new-points (spot1 spot2 distance)
  (let* ((x1 (car spot1))
         (y1 (cdr spot1))
         (x2 (car spot2))
         (y2 (cdr spot2))
         (dx (- x2 x1))
         (dy (- y2 y1))
         (vec-length (sqrt (+ (* dx dx)
                              (* dy dy))))
         (unit-dx (/ dx vec-length))
         (unit-dy (/ dy vec-length)))
    (list (cons (round (- x1 (* distance unit-dx))) (floor (- y1 (* distance unit-dy))))
          (cons (round (+ x2 (* distance unit-dx))) (floor (+ y2 (* distance unit-dy)))))))


(defun part1 (file-name)
  (multiple-value-bind (map antennas width height) (parse file-name)
    (let* ((unique-antennas (remove-duplicates antennas))
           (antinodes nil))
      (format t "~&width: ~a, height: ~a~%" width height)
      (loop for ua in unique-antennas
            do (let* ((this-antenna (cdr ua))
                      (this-antenna-loc (car ua))
                      (other-antennas (remove-if-not (lambda (other)
                                                       (and (equal (cdr other) this-antenna)
                                                            (not (equal (car other) this-antenna-loc))))
                                                     antennas)))
                 (loop for oa in other-antennas
                       do (let* ((other-antenna-loc (car oa))
                                 (distance (taxicab-distance this-antenna-loc other-antenna-loc))
                                 (new-points (plot-new-points this-antenna-loc other-antenna-loc distance)))
                            (format t "~&~a -> ~a -- d: ~a, new: ~a~%" ua oa distance  new-points)
                            (dolist (point new-points)
                              (when (and (<= 0 (car point) width)
                                         (<= 0 (cdr point) height))
                                (push point antinodes)))))))
      (print antinodes)
      (length (remove-duplicates antinodes :test #'equal)))))

(defun part2 (file-name)
  (let* ((map (parse file-name)))
    map))

(print (part1 "input2.txt"))

(print (part1 "input0.txt"))
; (print (part1 "input1.txt"))

; (print (part2 "input0.txt"))
; (print (part2 "input1.txt"))



