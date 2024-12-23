;(require :sb-concurrency)
;(use-package :sb-concurrency)

(require :cl-ppcre)
(require :split-sequence)
(require :lisp-utils)
(require :alexandria)
(require :lisp-utils)
(use-package :lisp-utils)

(defun parse (file-name)
  (let* ((lines (mapcar #'parse-integer (uiop:read-file-lines file-name))))
    lines))

(defun step1 (num)
  (mod (logxor num (* num 64)) 16777216))

(defun step2 (num)
  (mod (logxor num (floor num 32)) 16777216))

(defun step3 (num)
  (mod (logxor num (* num 2048)) 16777216))

(defun-memo compute-next-secret (vendor secret)
  (step3 (step2 (step1 secret))))

(defun extract-real-price (num)
  (mod num 10))

(defun compute-secrets (vendor secret count)
  (let* ((secrets (list secret)))
    (loop for i below count
          do (progn
               (pushnew (compute-next-secret vendor (first secrets)) secrets)))
    secrets))

(defun compute-differences (numbers)
  (let* ((pairs (pairwise numbers)))
    (loop for (a b) in pairs
          collecting (- b a))))

(defun find-pattern (numbers diffs)
  (let* ((max-value (apply #'max numbers))
         (max-pos (second (loop for value in numbers
                                for i from 0
                                if (= value max-value)
                                  collecting i))))
    (subseq diffs (- max-pos 3) (1+ max-pos))))

(defun part1 (file-name)
  (let ((secrets (parse file-name)))
    (reduce #'+ (loop for vendor from 0
                      for secret in secrets
                      collecting (first (compute-secrets vendor secret 2000))))))

(defun handle-vendor (vendor secret)
  (let* ((secrets (reverse (compute-secrets vendor secret 2000)))
         (real-prices (mapcar (lambda (secret) (extract-real-price secret)) secrets))
         (diffs (compute-differences (cons (first real-prices) real-prices)))
         (pattern (find-pattern real-prices diffs))
         (vendor (make-hash-table)))
    ;; (format t "~&secrets: ~a~%" (take secrets 10))
    ;; (format t "~&prices : ~a~%" (take real-prices 10))
    ;; (format t "~&diffs  : ~a~%" (take diffs 10))
    ;; (format t "~&pattern: ~a~%" pattern)
    (setf (gethash 'secrets vendor) secrets)
    (setf (gethash 'prices vendor) real-prices)
    (setf (gethash 'diffs vendor) diffs)
    (setf (gethash 'pattern vendor) pattern)
    vendor))

(defun find-sequencexx (pattern list)
  (when (subsetp pattern list)
    (let ((start-index (position (car pattern) list :test #'equal)))
      (print 'BAR)
      (when (equal pattern (subseq list start-index (+ start-index (length pattern))))
        (print 'FOO)
        (+ start-index 4)))))

(defun find-sequence (subset list)
  "Find the starting index of SUBSET in LIST if it appears sequentially and in order.
   Returns NIL if SUBSET is not found."
  (let ((subset-length (length subset)))
    (loop for i from 0 to (- (length list) subset-length)
          for sublist = (subseq list i (+ i subset-length))
          when (equal sublist subset)
          do (return i))))

(defun part2 (file-name)
  (let* ((initial-secrets (parse file-name))
         (vendors (make-hash-table))
         (best-value 0)
         (best-pattern nil))
    
    (loop for secret in initial-secrets
          for i from 0
          do (setf (gethash i vendors) (handle-vendor i secret)))

    (loop for vendor in (alexandria:hash-table-values vendors)
          do (let ((pattern (gethash 'pattern vendor))
                   (current-value 0))
               (format t "~&vendor: ~a, pattern: ~a~%" (gethash 'pattern vendor) pattern)
               (loop for other-vendor in (alexandria:hash-table-values vendors)
                     do (let ((other-pos (find-sequence pattern (gethash 'diffs other-vendor))))
                          (when other-pos
                            (incf current-value (nth other-pos (gethash 'prices other-vendor)))
                            (print current-value))))
               (when (> current-value best-value)
                 (setf best-value current-value)
                 (setf best-pattern pattern))))
    best-value))

(time (format t "~&part1: ~a~%" (part2 "input0.txt")))

(let ((vendor (handle-vendor 1 1)))
  (print (gethash 'pattern vendor))
  (print (find-sequence (gethash 'pattern vendor)
                        (gethash 'diffs vendor)))
  (print (nth 2 (gethash 'prices vendor))))

;; (let* ((secrets (reverse (compute-secrets 1 123 2000)))
;;        (real-prices (mapcar (lambda (secret) (extract-real-price secret)) (take secrets 10)))
;;        (diffs (compute-differences (cons (first real-prices) real-prices)))
;;        (pattern (find-pattern real-prices diffs)))
;;   (format t "~&secrets: ~a~%" (take secrets 10))
;;   (format t "~&prices : ~a~%" (take real-prices 10))
;;   (format t "~&diffs  : ~a~%" (take diffs 10))
;;   (format t "~&pattern: ~a~%" pattern))



;(time (format t "~&part1: ~a~%" (part1 "input0.txt")))
;(time (format t "~&part1: ~a~%" (part1 "input1.txt")))





