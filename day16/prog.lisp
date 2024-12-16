(ql:quickload :cl-ppcre)
(ql:quickload :split-sequence)
(ql:quickload :lisp-utils)
(ql:quickload :alexandria)

(require :sb-concurrency)
(use-package :sb-concurrency)

(use-package :lisp-utils)

(defun reconstruct-path (came-from current)
  (labels ((trace-path (node path)
             (if (null node)
                 (reverse path)
                 (trace-path (gethash node came-from)
                             (cons node path)))))
    (trace-path current nil)))

(defun manhattan-distance (a b)
  (+ (abs (- (car a) (car b)))
     (abs (- (cdr a) (cdr b)))))

(defun grid-neighbors (node)
  (list (cons (1+ (car node)) (cdr node))
        (cons (1- (car node)) (cdr node))
        (cons (car node) (1+ (cdr node)))
        (cons (car node) (1- (cdr node)))))

(defun find-start-and-end (maze)
  (let ((start nil)
        (end nil))
    (loop for key being the hash-keys of maze
          using (hash-value val)
          do (progn
               (when (equal val "S")
                 (setf start key))
               (when (equal val "E")
                 (setf end key))
               (when (and start end)
                 (return-from find-start-and-end (values start end)))))))

(defun parse-to-grid (file-name &optional (fn #'identity))
  (let* ((lines (uiop:read-file-lines file-name))
         (grid (make-hash-table :test #'equal)))
    (loop for row in lines
          for r from 0
          do (loop for col in (cl-ppcre:split "" row)
                   for c from 0
                   do (setf (gethash (cons r c) grid) (funcall fn col))))
    grid))

(defun a* (start goal maze neighbors cost heuristic)
  (let* ((open-set (list (cons start 0)))
         (closed-set (make-hash-table :test #'equal))
         (came-from (make-hash-table :test #'equal))
         (g-score (make-hash-table :test #'equal))
         (f-score (make-hash-table :test #'equal))
         (previous (cons (car start)
                         (1- (cdr start)))))
    (setf (gethash start g-score) 0)
    (setf (gethash start f-score) (funcall heuristic start goal))

    (loop while open-set
          do (let* ((current-pair (reduce (lambda (a b)
                                            (if (< (cdr a) (cdr b)) a b))
                                          open-set))
                    (current (car current-pair)))

               (when (equal current goal)
                 (return-from a*
                   (reconstruct-path came-from current)))

               (setf open-set (remove current-pair open-set :test #'equal))

               (setf (gethash current closed-set) t)

               (dolist (neighbor (funcall neighbors current))
                 (when (and (gethash neighbor maze)
                            (not (gethash neighbor closed-set))
                            (not (equal (gethash neighbor maze) "#")))
                   (let* ((tentative-g-score
                            (+ (gethash current g-score)
                               (funcall cost previous current neighbor)))
                          (neighbor-in-open (assoc neighbor open-set :test #'equal)))

                     (when (< tentative-g-score
                              (or (gethash neighbor g-score) most-positive-fixnum))
                       (setf (gethash neighbor came-from) current)
                       (setf (gethash neighbor g-score) tentative-g-score)

                       (let ((neighbor-f-score
                               (+ tentative-g-score
                                  (funcall heuristic neighbor goal))))

                         (if neighbor-in-open
                             (setf (cdr neighbor-in-open) neighbor-f-score)
                             (push (cons neighbor neighbor-f-score) open-set)))))))
               (setf previous current)))
    nil))

(defun direction (from to)
  (cond ((= (car from) (car to))
         (if (> (cdr to) (cdr from)) 'east 'west))
        ((= (cdr from) (cdr to))
         (if (> (car to)) (car from)) 'north 'south)))

(defun is-straight-or-turn (previous current next)
  (let ((prev-dir (direction previous current))
        (curr-dir (direction current next)))
    (cond ((or (null prev-dir)
               (null curr-dir))
           (format t "~&p: ~a, c: ~a, n: ~a, pd: ~a, cd: ~a -> ~a~%" previous current next prev-dir curr-dir "invalid")
           :invalid)
          ((eq prev-dir curr-dir)
           (format t "~&p: ~a, c: ~a, n: ~a, pd: ~a, cd: ~a -> ~a~%" previous current next prev-dir curr-dir "straight")
           :staight)
          (t
           (format t "~&p: ~a, c: ~a, n: ~a, pd: ~a, cd: ~a -> ~a~%" previous current next prev-dir curr-dir "turn")
           :turn))))

(defun compute-cost (previous current neighbor)
  (format t "~&~tCC| p: ~a, c: ~a, n: ~a~%" previous current neighbor)
  (if (null previous)
      1
      (let ((dir (is-straight-or-turn previous current neighbor)))
        (cond ((equal dir :straight)
               1)
              ((equal dir :turn)
               1001)
              (t
               1)))))

(defun part1 (file-name)
  (let* ((maze (parse-to-grid file-name)))
    (multiple-value-bind (start end) (find-start-and-end maze)
      (format t "~&start: ~a, end: ~a~%" start end)
      (let ((path (reverse (a* start end maze #'grid-neighbors #'compute-cost #'manhattan-distance))))
        (print path)
        (print (length path))
        (let ((previous (cons (car start)
                              (1- (cdr start))))
              (total-cost 0))
          (loop for (a b) in (pairwise path)
                do (let* ((cost (compute-cost previous a b)))
                     (format t "~&p: ~a, a: ~a, b: ~a, cost: ~a" previous a b cost)
                     (incf total-cost cost)
                     (setf previous a)))
          total-cost)))))

(time (print (part1 "input2.txt")))
; (time (print (part1 "input1.txt")))

; (time (print (part2 "input0.txt")))
; (time (print (part2 "input1.txt")))



