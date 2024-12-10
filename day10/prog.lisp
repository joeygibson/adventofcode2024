(ql:quickload :cl-ppcre)
(ql:quickload :split-sequence)
(ql:quickload :lisp-utils)
(ql:quickload :alexandria)
;(ql:quickload :queues)
(ql:quickload :damn-fast-priority-queue)
;(require :queues.priority-queue)
(use-package :lisp-utils)

(defun parse (file-name)
  (let* ((lines (uiop:read-file-lines file-name))
         (map (make-hash-table :test #'equal)))
    (loop for row in lines
          for r from 0
          do (loop for col in (cl-ppcre:split "" row)
                   for c from 0
                   do (setf (gethash (cons r c) map) (parse-integer col))))
    map))

(defun find-spots (map fn)
  (let ((spots nil))
    (maphash (lambda (k v)
               (when (funcall fn v)
                 (push k spots)))
             map)
    spots))

(defun find-starts (map)
  (find-spots map (lambda (v)
                    (= v 0))))

(defun find-ends (map)
  (find-spots map (lambda (v)
                    (= v 9))))

(defun update-item-in-queue (map neighbor f-score)
  (let ((updated nil))
    (damn-fast-priority-queue:do-queue (obj map)
      (when (equal obj neighbor)
        (setf updated t)))
    updated))

(defun a* (map start end neighbors-fn cost-fn heuristic-fn)
  (let ((open-set (damn-fast-priority-queue:make-queue))
        (came-from (make-hash-table :test #'equal))
        (g-score (make-hash-table :test #'equal)))

    (setf (gethash start g-score) 0)
    (damn-fast-priority-queue:enqueue open-set start 0)

    (labels ((reconstruct-path (current)
               (let ((parent (gethash current came-from)))
                 (if parent
                     (cons current (reconstruct-path parent))
                     (list current)))))
      
      (loop while (> (damn-fast-priority-queue:size open-set) 0)
            do (multiple-value-bind (current _) (damn-fast-priority-queue:dequeue open-set)
                 (when (equal current goal)
                   (return-from a*
                     (nreverse (reconstruct-path current))))
                 
                 (dolist (neighbor (funcall neighbors-fn current))
                   (let* ((tentative-g-score (+ (gethash current g-score most-positive-fixnum)
                                                (funcall cost-fn current neighbor))))
                     (when (< tentative-g-score (gethash neighbor g-score most-positive-fixnum))
                       (setf (gethash neighbor came-from) current)
                       (let ((f-score (+ tentative-g-score (funcall heuristic-fn neighbor goal))))
                         (unless (update-item-in-queue open-set neighbor f-score)
                           (damn-fast-priority-queue:enqueue open-set neighbor f-score)))))))))))

(defun part1 (file-name)
  (let* ((map (parse file-name))
         (starts (find-starts map))
         (ends (find-ends map))
         (trailheads (make-hash-table :test #'equal)))
    (loop for start in starts
          do (loop for end in ends
                   do (a* start end )))))

(defun part2 (file-name)
  (let* ((data (parse file-name)))))

(print (part1 "input1.txt"))
; (print (part1 "input1.txt"))

; (print (part2 "input0.txt"))
; (print (part2 "input1.txt"))



