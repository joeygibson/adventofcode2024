(ql:quickload :cl-ppcre)
(ql:quickload :split-sequence)
(ql:quickload :lisp-utils)
(ql:quickload :alexandria)
(ql:quickload :queues)

(require :queues.simple-queue)
(use-package :queues)
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

(defun flood-fill (map start width height)
  (let* ((original-crop (gethash start map))
         (directions (list (cons -1 0)
                           (cons 1 0)
                           (cons 0 -1)
                           (cons 0 1)))
         (queue (queues:make-queue :simple-queue))
         (visited nil)
         (total-sides 0)
         (region nil))
    (format t "~&start: ~a~%" start)
    (qpush queue start)
    (loop for plot = (qpop queue)
          while plot
          do (progn
               (when (not (member plot visited :test #'equal))
                 (pushnew plot visited :test #'equal)
                 (dolist (dir directions)
                   (format t "~&plot: ~a~%" plot)
                   (let* ((next-coords (cons (+ (car plot) (car dir))
                                             (+ (cdr plot) (cdr dir))))
                          (next (gethash next-coords map)))
                     (when next
                       (when (or (not (and (<= 0 (car next-coords) height)
                                           (<= 0 (cdr next-coords) width)))
                                 (not (equal next original-crop)))
                         (incf total-sides))
                       (when (and (and (<= 0 (car next-coords) height)
                                       (<= 0 (cdr next-coords) width)
                                       (equal next original-crop)
                                       (not (member next visited :test #'equal))))
                         (push plot region)
                         (qpush queue next-coords))))))))
    (values region total-sides)))

(let ((q (make-queue :simple-queue)))
  (qpush q (cons 1 3))
  (qpop q))

(defun in-region-p (regions plot)
  (loop for region in regions
        if (member plot region :test #'equal)
          return t))

(defun part1 (file-name)
  (multiple-value-bind (map width height) (parse-to-grid file-name)
    (let* ((regions nil)
           (sides nil))
      (loop for plot being the hash-keys of map
            while (not (in-region-p regions plot))
            do (multiple-value-bind (region region-sides) (flood-fill map plot width height)
                 (print region)
                 (print region-sides)
                 (push region regions)
                 (push region-sides sides))))))

(defun part2 (file-name)
  (let* ((data (parse file-name)))))

(time (print (part1 "input0.txt")))
; (time (print (part1 "input1.txt")))

; (time (print (part2 "input0.txt")))
; (time (print (part2 "input1.txt")))



