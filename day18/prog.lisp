(require :cl-ppcre)
(require :lisp-utils)
(use-package :lisp-utils)

(defun parse (file-name)
  (mapcar (lambda (line)
            (mapcar #'parse-integer (cl-ppcre:split "," line)))
          (uiop:read-file-lines file-name)))

(defun print-grid (grid max-x max-y)
  "Prints the grid stored in the hash table."
  (terpri)
  (loop for y from 0 upto max-y do
        (loop for x from 0 upto max-x do
              (let ((value (gethash (cons x y) grid nil)))
                (format t "~a " (or value "."))))
        (terpri)))

(defun create-ram-map (size)
  (let ((ram (make-hash-table :test #'equal)))
    (loop for r upto size
          do (loop for c upto size
                   do (setf (gethash (cons c r) ram) ".")))
    ram))

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

(defun a-star (start goal maze neighbors cost heuristic)
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
                 (return-from a-star
                   (reconstruct-path came-from current)))

               (setf open-set (remove current-pair open-set :test #'equal))

               (setf (gethash current closed-set) t)

               (dolist (neighbor (funcall neighbors current))
                 (when (and (gethash neighbor maze)
                            (not (gethash neighbor closed-set))
                            (not (equal (gethash neighbor maze) "#")))
                   (let* ((tentative-g-score
                            (+ (gethash current g-score)
                               (funcall cost current neighbor)))
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

(defun compute-cost (a b)
  (declare (ignore a b))
  1)

(defun part1 (file-name)
  (let* ((byte-data (parse file-name))
         (size (if (equal file-name "input0.txt")
                   6
                   70))
         (bytes-to-map (if (equal file-name "input0.txt")
                           12
                           1024))
         (start (cons 0 0))
         (end (cons size size))
         (ram (create-ram-map size)))
    (loop for (x y) in (take byte-data bytes-to-map)          
          do (setf (gethash (cons x y) ram) "#"))
    (1- (length (a-star start end ram #'grid-neighbors #'compute-cost #'manhattan-distance)))))

(defun part2 (file-name)
  (let* ((byte-data (parse file-name))
         (size (if (equal file-name "input0.txt")
                   6
                   70))
         (start (cons 0 0))
         (end (cons size size))
         (ram (create-ram-map size)))
    (loop named byte-fall
          for (x y) in byte-data
          do (progn
               (setf (gethash (cons x y) ram) "#")
               (if (not (a-star start end ram #'grid-neighbors #'compute-cost #'manhattan-distance))
                   (return-from byte-fall (format nil "~a,~a" x y)))))))

(time (format t "~&part1: ~a~%" (part1 "input0.txt")))
(time (format t "~&part1: ~a~%" (part1 "input1.txt")))
(time (format t "~&part2: ~a~%" (part2 "input0.txt")))
(time (format t "~&part2: ~a~%" (part2 "input1.txt")))


