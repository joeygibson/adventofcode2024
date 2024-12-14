(ql:quickload :cl-ppcre)
(ql:quickload :split-sequence)
(ql:quickload :lisp-utils)
(ql:quickload :alexandria)

(require :sb-concurrency)
(use-package :sb-concurrency)

(use-package :lisp-utils)

(defconstant room-width 101)
(defconstant room-height 103)
(defconstant vertical-room-divider 51)
(defconstant horizontal-room-divider 52)

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
    (let* ((new-x (+ x dx))
           (new-y (+ y dy)))
      (cond ((< new-x 0)
             (setf new-x (- room-width new-x)))
            ((> new-x room-width)
             (setf new-x (- new-x room-width))))
      (cond ((< new-y 0)
             (setf new-y (- room-height new-y)))
            ((> new-y room-height)
             (setf new-y (- new-y room-height))))
      (setf x new-x)
      (setf y new-y))))

(defmethod in-quadrant-p ((self robot) start-x end-x start-y end-y)
  (with-slots (x y) self
    (and (<= start-x x end-x)
         (<= start-y y end-y))))

(defun parse (file-name)
  (let* ((lines (uiop:read-file-lines file-name)))
    (mapcar (lambda (line)
              (let* ((numbers (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings "(-?\\d+)" line))))
                (make-robot numbers)))
            lines)))

(defun part1 (file-name)
  (let* ((robots (parse file-name))
         (top-left nil)
         (top-right nil)
         (bottom-left nil)
         (bottom-right))
    ;(format t "~&~{~a~%~}" robots)
    (loop for i upto 100
          do (loop for r in robots
                   do (move r)))
    ;(format t "~&~{~a~%~}" robots)
    (loop for r in robots
          do (cond ((and (>= (x r) 0)
                         (< (x r) vertical-room-divider)
                         (>= (y r) 0)
                         (< (y r) horizontal-room-divider))
                    (push r top-left))
                   ((and (> (x r) vertical-room-divider)
                         (<= (x r) room-width)
                         (>= (y r) 0)
                         (< (y r) horizontal-room-divider))
                    (push r top-right))
                   ((and (>= (x r) 0)
                         (< (x r) vertical-room-divider)
                         (> (y r) horizontal-room-divider)
                         (<= (y r) room-height))
                    (push r bottom-left))
                   ((and (> (x r) vertical-room-divider)
                         (<= (x r) room-width)
                         (> (y r) horizontal-room-divider)
                         (<= (y r) room-height))
                    (push r bottom-right))))
    (reduce #'+ (list (length top-left)
                      (length top-right)
                      (length bottom-left)
                      (length bottom-right)))))

(defun part2 (file-name)
  (let* ((data (parse file-name)))))

(time (print (part1 "input0.txt")))
(time (print (part1 "input1.txt")))

; (time (print (part2 "input0.txt")))
; (time (print (part2 "input1.txt")))

(let ((r (make-robot '(100 102 2 2))))
  (print r)
  (move r)
  (print r))




