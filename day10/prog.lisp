(ql:quickload :cl-ppcre)
(ql:quickload :split-sequence)
(ql:quickload :lisp-utils)
(ql:quickload :alexandria)
(use-package :lisp-utils)

;; Solution borrowed from https://www.reddit.com/r/adventofcode/comments/1hau6hl/2024_day_10_solutions/m1cii4n/

(defun parse (file-name)
  (let* ((lines (uiop:read-file-lines file-name))
         (map (make-hash-table :test #'equal)))
    (loop for row in lines
          for r from 0
          do (loop for col in (cl-ppcre:split "" row)
                   for c from 0
                   do (setf (gethash (cons r c) map) (parse-integer col))))
    map))

(defun find-starts (map)
  (let ((spots nil))
    (maphash (lambda (k v)
               (when (= v 0)
                 (push k spots)))
             map)
    spots))

(defun get-neighbors (pos)
  "Return up, down, left, and right of pos"
  (let ((r (car pos))
        (c (cdr pos)))
    (list (cons (1- r) c)
          (cons (1+ r) c)
          (cons r (1- c))
          (cons r (1+ c)))))

(defun find-path (map pos ends)
  "Recursively traverse the map, as long as the next spot's value
is no more than 1 higher than the current value."
  (let* ((current (gethash pos map)))
    (if (= current 9)
        (incf (gethash pos ends 0))
        (dolist (neighbor (get-neighbors pos))
          (let ((nv (gethash neighbor map -1)))
            (when (= nv (1+ current))
              (find-path map neighbor ends)))))))

(defun discover-trails (map starts)
  "For each starting point, find the trails that go forth from it,
scoring and rating them as we go."
  (let ((score 0)
        (rating 0))
    (dolist (start starts)
      (let ((ends (make-hash-table :test #'equal)))
        (find-path map start ends)
        (incf score (length (alexandria:hash-table-keys ends)))
        (maphash (lambda (k v)
                   (declare (ignore k))
                   (incf rating v))
                 ends)))
    (values score rating)))

(defun main (file-name)
  (let* ((map (parse file-name))
         (starts (find-starts map)))
    (multiple-value-bind (score rating) (discover-trails map starts)
      (format t "~&part1: ~d~%" score)
      (format t "~&part2: ~d~%" rating))))

(main "input1.txt")


