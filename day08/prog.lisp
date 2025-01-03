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

(defun euclidean-distance-integers (point-a point-b)
  (let ((x1 (car point-a))
        (y1 (cdr point-a))
        (x2 (car point-b))
        (y2 (cdr point-b)))
    (round (sqrt (+ (expt (- x2 x1) 2)
                    (expt (- y2 y1) 2))))))

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
    (list (cons (round (- x1 (* distance unit-dx))) (round (- y1 (* distance unit-dy))))
          (cons (round (+ x2 (* distance unit-dx))) (round (+ y2 (* distance unit-dy)))))))

(defun plot-new-points-2 (new-spots spot1 spot2 distance width height)
  (let* ((x1 (car spot1))
         (y1 (cdr spot1))
         (x2 (car spot2))
         (y2 (cdr spot2))
         (dx (- x2 x1))
         (dy (- y2 y1))
         (vec-length (sqrt (+ (* dx dx)
                              (* dy dy))))
         (unit-dx (/ dx vec-length))
         (unit-dy (/ dy vec-length))
         (new-a (cons (round (- x1 (* distance unit-dx))) (round (- y1 (* distance unit-dy)))))
         (new-b (cons (round (+ x2 (* distance unit-dx))) (round (+ y2 (* distance unit-dy))))))
    (when (and (in-bounds new-a width height) (not (member new-a new-spots :test #'equal)))
      (push new-a new-spots)
      (setf new-spots (append new-spots (plot-new-points-2 new-spots spot1 new-a distance width height))))
    (when (and (in-bounds new-b width height) (not (member new-b new-spots :test #'equal)))
      (push new-b new-spots)
      (setf new-spots (append new-spots (plot-new-points-2 new-spots spot2 new-b distance width height))))
    new-spots))

(defun part1 (file-name)
  (multiple-value-bind (map antennas width height) (parse file-name)
    (let* ((antinodes nil))
      (loop for ua in antennas
            do (let* ((this-antenna (cdr ua))
                      (this-antenna-loc (car ua))
                      (other-antennas (remove-if-not (lambda (other)
                                                       (and (equal (cdr other) this-antenna)
                                                            (not (equal (car other) this-antenna-loc))))
                                                     antennas)))
                 (loop for oa in other-antennas
                       do (let* ((other-antenna-loc (car oa))
                                 (distance (euclidean-distance-integers this-antenna-loc other-antenna-loc))
                                 (new-points (plot-new-points this-antenna-loc other-antenna-loc distance)))
                            (dolist (point new-points)
                              (when (and (and (>= (car point) 0)
                                              (< (car point) width))
                                         (and (>= (cdr point) 0)
                                              (< (cdr point) height)))
                                (push point antinodes)))))))
      (length (remove-duplicates antinodes :test #'equal)))))

(defun in-bounds (point width height)
  (and (and (>= (car point) 0)
            (< (car point) width))
       (and (>= (cdr point) 0)
            (< (cdr point) height))))

(defun part2a (file-name)
  (multiple-value-bind (map antennas width height) (parse file-name)
    (let* ((antinodes nil))
      (loop for ua in antennas
            do (let* ((this-antenna (cdr ua))
                      (this-antenna-loc (car ua))
                      (other-antennas (remove-if-not (lambda (other)
                                                       (and (equal (cdr other) this-antenna)
                                                            (not (equal (car other) this-antenna-loc))))
                                                     antennas)))
                 (loop for oa in other-antennas
                       do (let* ((other-antenna-loc (car oa))
                                 (distance (euclidean-distance-integers this-antenna-loc other-antenna-loc))
                                 (new-points (plot-new-points-2 () this-antenna-loc other-antenna-loc distance width height)))
                            (dolist (point new-points)
                              (when (in-bounds point width height)
                                (push point antinodes)))))))
      (length (remove-duplicates antinodes :test #'equal)))))

(defun part2 (lines)
  (let ((antennas (make-hash-table :test #'equal))
        (n (length lines))
        (m (length (first lines)))
        (antinodes nil)
        (harmonics nil))
    (loop for row in lines
          for r from 0
          do (loop for col in (cl-ppcre:split "" row)
                   for c from 0
                   do (when (not (equal col "."))
                        (let* ((cur-value (gethash col antennas))
                               (new-value (cons (cons r c) cur-value)))
                          (setf (gethash col antennas) new-value)))))
    (dolist (nodes (alexandria:hash-table-values antennas))
      (print (combinations nodes 2))
      (loop for (a b) in (combinations nodes 2)
            do (let ((diff (cons (- (car b) (car a))
                                 (- (cdr b) (cdr a)))))
                 (loop for c in (list (cons (- (car a) (car diff))
                                            (- (cdr a) (cdr diff)))
                                      (cons (+ (car b) (car diff))
                                            (+ (cdr b) (cdr diff))))
                       do (if (and (<= 0 (car c) n)
                                   (<= 0 (cdr c) m))
                              (pushnew c antinodes :test #'equal)))
                 (loop for thing in (list (cons a -1) (cons b 1))
                       do (let* ((c (car thing))
                                 (sign (cdr thing)))
                            (loop while (and (<= 0 (car c) n)
                                             (<= 0 (cdr c) m))
                                  do (progn
                                       (pushnew c harmonics :test #'equal)
                                       (setf c (cons (+ (car c) (* sign (car diff)))
                                                     (+ (cdr c) (* sign (cdr diff))))))))))))
    (format t "~&antinodes: ~d~%" (length antinodes))
    (format t "~&harmomics: ~d~%" (length harmonics))))

;(print (part1 "input2.txt"))

;(print (part1 "input0.txt"))
;(print (part1 "input1.txt"))

(print (part2 (uiop:read-file-lines "input0.txt")))
;(print (part2 "input1.txt"))





