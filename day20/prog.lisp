;(require :sb-concurrency)
;(use-package :sb-concurrency)

(require :cl-ppcre)
(require :split-sequence)
(require :lisp-utils)
(require :alexandria)
(require :lisp-utils)
(use-package :lisp-utils)

(defun parse-to-grid (file-name &optional (fn #'identity))
  (let* ((lines (uiop:read-file-lines file-name))
         (grid (make-hash-table :test #'equal))
         (start nil)
         (end nil))
    (loop for row in lines
          for r from 0
          do (loop for col in (cl-ppcre:split "" row)
                   for c from 0
                   do (progn
                        (cond ((equal col "S")
                               (setf start (cons c r)))
                              ((equal col "E")
                               (setf end (cons c r))))
                        (setf (gethash (cons c r) grid) (funcall fn (if (equal col "#")
                                                                        "#"
                                                                        "."))))))
    (values grid
            start
            end
            (length lines)
            (length (first lines)))))

(defun cost (a b)
  (declare (ignore a b))
  1)

(defun excluded (a)
  (equal a "#"))

(defun get-walls (map nodes)
  (remove-if-not (lambda (node)
                   (let ((value (gethash node map)))
                     (and value
                          (equal value "#"))))
                 nodes))

(defun is-horizontal-wall (map node neighbors)
  ;(format t "~&ihw: node: ~a, neighbors: ~a~%" node neighbors)
  (let ((right (cons (1+ (car node)) (cdr node)))
        (left (cons (1- (car node)) (cdr node))))
    (and (and (equal (gethash right map) ".")
              (member right neighbors :test #'equal))
         (and (equal (gethash left map) ".")
              (member left neighbors :test #'equal)))))

(defun is-vertical-wall (map node neighbors)
  ;(format t "~&ivw: node: ~a, neighbors: ~a~%" node neighbors)
  (let ((down (cons (car node) (1+ (cdr node))))
        (up (cons (car node) (1+ (cdr node)))))
    (and (and (equal (gethash down map) ".")
              (member down neighbors :test #'equal))
         (and (equal (gethash up map) ".")
              (member up neighbors :test #'equal)))))

(defun find-cheatable-walls (map node)
  (let* ((neighbors (grid-neighbors node))
         (walls (get-walls map neighbors))
         (interior-walls (remove-if-not (lambda (neighbor)
                                          (let ((n-neighbors (grid-neighbors neighbor)))
                                            (or (is-horizontal-wall map neighbor n-neighbors)
                                                (is-vertical-wall map neighbor n-neighbors))))
                                        walls)))
    ;(format t "~&node: ~a, neighbors: ~a, walls: ~a, interior-walls: ~a~%" node neighbors walls interior-walls)
    interior-walls))

(defun break-wall (map wall)
  (let ((map-copy (alexandria:copy-hash-table map)))
    (setf (gethash wall map-copy) ".")
    map-copy))

(defun part1 (file-name)
  (multiple-value-bind (map start end height width) (parse-to-grid file-name)
    (let* ((path (reverse (a-star start end map #'grid-neighbors #'(lambda (a b) 1) #'manhattan-distance #'(lambda (a) (equal a "#")))))
           (path-length (1- (length path)))
           (cheats (make-hash-table :test #'equal)))
      (format t "~&path: ~a~%path-length: ~a~%" path path-length)
      (loop for node in path
            do (progn
                 (loop for wall in (find-cheatable-walls map node)
                       do (let* ((map-copy (break-wall map wall))
                                 (new-path (a-star start end map-copy #'grid-neighbors #'(lambda (a b) 1) #'manhattan-distance #'(lambda (a) (equal a "#"))))
                                 (new-path-length (1- (length new-path))))
                                        ;(print-grid map-copy (1- width) (1- height))
                                        ;(format t "~&new-path: ~a~%new-path-length: ~a~%" new-path new-path-length)
                            ;(format t "~&cheatable-wall: ~a, diff: ~a~%" wall (- path-length new-path-length))
                            (setf (gethash wall cheats) new-path-length)))))
      (let* ((keys (alexandria:hash-table-keys cheats)))
        (loop for k in keys
              do (when (>= (- path-length (gethash k cheats)) 100)
                   (format t "~&X~a: ~a, diff: ~a~%" k (gethash k cheats) (- path-length (gethash k cheats)))))))))

;(remove-if-not (lambda (a) (equal (cons 7 1) a)) path)

(time (format t "~&part1: ~a~%" (part1 "input1.txt")))
;; (time (format t "~&part1: ~a~%" (part1 "input1.txt")))
;; (time (format t "~&part2: ~a~%" (part2 "input0.txt")))
;; (time (format t "~&part2: ~a~%" (part2 "input1.txt")))


