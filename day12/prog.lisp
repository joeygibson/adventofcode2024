(ql:quickload :cl-ppcre)
(ql:quickload :split-sequence)
(ql:quickload :lisp-utils)
(ql:quickload :alexandria)

(require :sb-concurrency)
(use-package :sb-concurrency)
(use-package :lisp-utils)

(defun parse-to-grid (file-name &optional (fn #'identity))
  (let* ((lines (uiop:read-file-lines file-name))
         (grid (make-hash-table :test #'equal))
         (height (length lines))
         (width (length (first lines))))
    (loop for row in lines
          for r from 0
          do (loop for col in (cl-ppcre:split "" row)
                   for c from 0
                   do (setf (gethash (cons r c) grid) (funcall fn col))))
    (values grid width height)))

(defun flood-fill (map start rows cols)
  (let* ((original-crop (gethash start map))
         (directions (list (cons -1 0)
                           (cons 1 0)
                           (cons 0 -1)
                           (cons 0 1)))
         (queue (make-queue :initial-contents (list start)))
         (visited nil)
         (total-sides 0)
         (region nil))
    (push start region)
    (loop for plot = (dequeue queue)
          while plot
          do (let* ((row (car plot))
                    (col (cdr plot)))
               (when (not (member plot visited :test #'equal))
                 (pushnew plot visited :test #'equal)
                 
                 (loop for (dr . dc) in directions
                       do (let* ((r (+ row dr))
                                 (c (+ col dc)))
                            (when (or (not (and (<= 0 r rows)
                                                (<= 0 c cols)))
                                      (not (equal (gethash (cons r c) map) original-crop)))
                              (incf total-sides))

                            (when (and (<= 0 r rows)
                                       (<= 0 c cols)
                                       (equal (gethash (cons r c) map) original-crop)
                                       (not (member (cons r c) visited :test #'equal)))
                              (pushnew (cons r c) region :test #'equal)
                              (enqueue (cons r c) queue)))))))
    (values region total-sides)))

(defun in-region-p (regions plot)
  (loop for region in regions
        if (member plot region :test #'equal)
          return t))

(defun part1 (file-name)
  (multiple-value-bind (map width height) (parse-to-grid file-name)
    (let* ((regions nil)
           (sides nil))
      (loop for plot being the hash-keys of map
            if (not (in-region-p regions plot))
            do (multiple-value-bind (region region-sides) (flood-fill map plot width height)
                 (push region regions)
                 (push region-sides sides)))
      (reduce #'+ (loop for region in regions
             for side in sides
             collecting (* (length region) side))))))

(defun part2 (file-name)
  (let* ((data (parse file-name)))))

(time (print (part1 "input0.txt")))
(time (print (part1 "input2.txt")))
(time (print (part1 "input1.txt")))

; (time (print (part2 "input0.txt")))
; (time (print (part2 "input1.txt")))



