(ql:quickload :cl-ppcre)
(ql:quickload :split-sequence)
(ql:quickload :lisp-utils)
(ql:quickload :alexandria)

(use-package :lisp-utils)

(defclass guard ()
  ((direction :initform :up :accessor direction)
   (x :initform 0 :accessor guard-x)
   (y :initform 0 :accessor guard-y)))

(defmethod print-object ((self guard) stream)
  (print-unreadable-object (self stream)
    (format stream "~s, direction: ~a, x: ~d, y: ~d"
            (type-of self)
            (direction self)
            (guard-x self)
            (guard-y self))))

(defclass furniture ()
  ())

(defclass floor-tile ()
  ((visited :initform nil :accessor visited)
   (visit-count :initform 0 :accessor visit-count)))

(defclass obstacle ()
  ())

(defmethod add-visit ((self floor-tile))
  (incf (visit-count self)))

(defun create-object (chr)
  (cond ((equal chr "^")
         (make-instance 'guard))
        ((equal chr ".")
         (make-instance 'floor-tile))
        ((equal chr "#")
         (make-instance 'furniture))))

(defun parse (file-name)
  (let* ((lines (uiop:read-file-lines file-name))
         (the-map (make-hash-table :test #'equal))
         (guard nil))
    (loop for row in lines
          for r from 0
          do (loop for col in (cl-ppcre:split "" row)
                   for c from 0
                   do (let ((obj (create-object col)))
                        (when (typep obj 'guard)
                          (setf (guard-x obj) c)
                          (setf (guard-y obj) r)
                          (setf guard obj)
                          (setf obj (create-object ".")))
                        (setf (gethash (cons c r) the-map) obj))))
    (values the-map guard)))

(defun play (the-map guard)
  (let ((in-bounds t))
    (loop while in-bounds
          do (let* ((guard-direction (direction guard))
                    (old-x (guard-x guard))
                    (old-y (guard-y guard))
                    (x (cond ((eql (direction guard) :left)
                              (1- old-x))
                             ((eql (direction guard) :right)
                              (1+ old-x))
                             (t old-x)))
                    (y (cond ((eql (direction guard) :up)
                              (1- old-y))
                             ((eql (direction guard) :down)
                              (1+ old-y))
                             (t old-y))))
               (let ((next-spot (gethash (cons x y) the-map)))
                 (cond ((null next-spot) ; out of bounds
                        (setf in-bounds nil))
                       ((typep next-spot 'floor-tile) ; floor-tile
                        ; (print (visit-count next-spot))
                        (when (> (visit-count next-spot) 5)
                          (print "LOOP")
                          (return 'in-a-loop))
                        (setf (visited next-spot) t)
                        (add-visit next-spot)
                        (setf (guard-x guard) x)
                        (setf (guard-y guard) y))
                       (t
                        (let ((new-direction (cond ((eql guard-direction :up) :right)
                                                   ((eql guard-direction :right) :down)
                                                   ((eql guard-direction :down) :left)
                                                   ((eql guard-direction :left) :up))))
                          (setf (direction guard) new-direction)))))))))



(defun part1 (file-name)
  (multiple-value-bind (the-map guard) (parse file-name)
    (play the-map guard)
    (loop for spot being the hash-value of the-map
          count (and (typep spot 'floor-tile)
                     (visited spot)))))

(defun find-obstacle (the-map)
  (maphash (lambda (k v)
             (when (typep v 'obstacle)
               (format t "~&obstacle at ~a~%" k)))
           the-map))

(defun print-visit-counts (the-map)
  (maphash (lambda (k v)
             (when (and (typep v 'floor-tile)
                        (> (visit-count v) 0))
              (format t "~&~a visited ~a~%" k (visit-count v))))
           the-map))

(defun part2 (file-name)
  (multiple-value-bind (the-map guard) (parse file-name)
    (let* ((guard-pos (cons (guard-x guard)
                            (guard-y guard)))
           (loop-count 0))
      (loop for pos being the hash-keys of the-map
            do (progn
                 (multiple-value-bind (map-copy guard) (parse file-name)
                   (when (and (not (equal pos guard-pos))
                              (typep (gethash pos the-map) 'floor-tile))
                     (print-visit-counts map-copy)
                     (setf (gethash pos map-copy) (make-instance 'obstacle))
                     (let ((results (play map-copy guard)))
                       ;(format t "~&~a -> ~a~%" pos results)
                       (when (equal results 'in-a-loop)
                         (incf loop-count)))))))
      loop-count)))

(print (part1 "input0.txt"))
(print (part1 "input1.txt"))

(print (part2 "input0.txt"))
(print (part2 "input1.txt"))



