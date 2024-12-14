(ql:quickload :cl-ppcre)
(ql:quickload :split-sequence)
(ql:quickload :lisp-utils)
(ql:quickload :alexandria)

(require :sb-concurrency)
(use-package :sb-concurrency)

(use-package :lisp-utils)

(defconstant room-width 101)
(defconstant room-height 103)
(defconstant vertical-room-divider (floor room-width 2))
(defconstant horizontal-room-divider (floor room-height 2))

(defclass robot ()
  ((x :initarg :x :accessor x :type integer)
   (y :initarg :y :accessor y :type integer)
   (dx :initarg :dx :accessor dx :type integer)
   (dy :initarg :dy :accessor dy :type integer)))

(defun make-robot (numbers)
  (make-instance 'robot :x (first numbers)
                        :y (second numbers)
                        :dx (third numbers)
                        :dy (fourth numbers)))

(defmethod print-object ((self robot) stream)
  (print-unreadable-object (self stream)
    (with-slots (x y dx dy) self
        (format stream "~s (~a, ~a), dx: ~a, dy: ~a" (type-of self) x y dx dy))))

(defmethod move ((self robot))
  (with-slots (x y dx dy) self
    (let* ((new-x (mod (+ x dx) room-width))
           (new-y (mod (+ y dy) room-height)))
      (setf x new-x)
      (setf y new-y))))

(defun parse (file-name)
  (let* ((lines (uiop:read-file-lines file-name)))
    (mapcar (lambda (line)
              (let* ((numbers (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings "(-?\\d+)" line))))
                (make-robot numbers)))
            lines)))

(defun part1 (file-name)
  (let* ((robots (parse file-name))
         (quads (make-hash-table :test #'equal)))
    (loop for i below 100
          do (loop for r in robots
                   do (move r)))
    (loop for r in robots
          do (progn
               ;(print r)
               (let ((x-quad (cond ((< (x r) vertical-room-divider)
                                   0)
                                  ((> (x r) vertical-room-divider)
                                   1)))
                    (y-quad (cond ((< (y r) horizontal-room-divider)
                                   0)
                                  ((> (y r) horizontal-room-divider)
                                   1))))
                (when (and x-quad y-quad)
                  (incf (gethash (cons x-quad y-quad) quads 0))))))
    (print (alexandria:hash-table-values quads))
    (reduce #'* (alexandria:hash-table-values quads))))

(defun part2 (file-name)
  (let* ((data (parse file-name)))))

(time (print (part1 "input0.txt")))
(time (print (part1 "input1.txt")))

; (time (print (part2 "input0.txt")))
; (time (print (part2 "input1.txt")))


