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

(defun find-pattern (prices diffs)
  (let* ((max-price (apply #'max prices))
         (max-positions (loop for price in prices
                              for index from 0
                              when (= price max-price)
                                collecting index))
         (max-pos (if (>= (first max-positions) 4)
                      (first max-positions)
                      (second max-positions))))
    (format t "~&~a, ~a~%" max-price max-pos)
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
    (setf (gethash 'secrets vendor) secrets)
    (setf (gethash 'prices vendor) real-prices)
    (setf (gethash 'diffs vendor) diffs)
    (setf (gethash 'pattern vendor) pattern)
    vendor))

(defun find-sequence (subset list)
  "Find the starting index of SUBSET in LIST if it appears sequentially and in order.
   Returns NIL if SUBSET is not found."
  (let ((subset-length (length subset)))
    (loop for i from 0 to (- (length list) subset-length)
          for sublist = (subseq list i (+ i subset-length))
          when (equal sublist subset)
          do (return (+ i 3)))))

(defun part2 (file-name)
  (let* ((initial-secrets (parse file-name))
         (vendors (make-hash-table))
         (best-value 0)
         (best-pattern nil))
    
    (loop for secret in initial-secrets
          for i from 0
          do (setf (gethash i vendors) (handle-vendor i secret)))

    (loop for vendor in (alexandria:hash-table-values vendors)
          for vi from 0
          do (let ((pattern (gethash 'pattern vendor))
                   (current-value 0))
               (loop for other in (alexandria:hash-table-values vendors)
                     for oi from 0
                     do (let* ((index (find-sequence pattern (gethash 'diffs other))))
                          (format t "~&v: ~a, p: ~a, o: ~a, i: ~a~%" vi pattern oi index)
                          (when index
                            (incf current-value (nth index (gethash 'prices other))))))
               (format t "~&v: ~a, cv: ~a~%" vi current-value)
               (when (> current-value best-value)
                 (setf best-value current-value)
                 (setf best-pattern pattern))))
    best-value))

(time (format t "~&part1: ~a~%" (part2 "input0.txt")))

(let* ((vendor (handle-vendor 999 2024))
       (max-price (apply #'max (gethash 'prices vendor)))
       (positions (loop for price in (gethash 'prices vendor)
                        for i from 0
                        when (= price max-price)
                          collect i)))
  (loop for pos in positions
        when (> pos 4)
        do (format t "~&~a, ~a, ~a~%" pos (nth pos (gethash 'prices vendor))
                   (subseq (gethash 'diffs vendor) (- pos 3) (1+ pos)))))





