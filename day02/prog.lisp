(ql:quickload :cl-ppcre)
(ql:quickload :split-sequence)
(ql:quickload :lisp-utils)
(ql:quickload :alexandria)

; (ql:quickload :queues)

; (require :queues.simple-queue)


(use-package :lisp-utils)

(defun parse (file-name)
  (let* ((lines (uiop:read-file-lines file-name)))
    (mapcar (lambda (line)
              (mapcar #'parse-integer (cl-ppcre:split "\\s+" line)))
            lines)))

(defun all-increasing-or-decreasing (pairs)
  (let* ((first-pair (car pairs))
         (rest-pairs (cdr pairs))
         (increasing (> (second first-pair) (first first-pair)))
         (changed nil))
    (dolist (pair rest-pairs)
      (cond ((> (second pair) (first pair))
             (if (not increasing)
                 (progn
                   (setf increasing t)
                   (setf changed t))))
            ((> (first pair) (second pair))
             (if increasing
                 (progn
                   (setf increasing nil)
                   (setf changed t))))
            ((= (first pair) (second pair))
             (setf changed t))))
    (not changed)))

(defun gradual-change-p (pairs)
  (let ((changes (loop for pair in pairs
                       collecting (abs (- (second pair) (first pair))))))
    (every (lambda (diff)
             (and (> diff 0)
                  (< diff 4)))
           changes)))

(defun check-report (report)
  (let* ((pairs (pairwise report)))
    (if (all-increasing-or-decreasing pairs)
        (gradual-change-p pairs)
        nil)))

(defun part1 (file-name)
  (let* ((reports (parse file-name)))
    (count t (mapcar #'check-report reports))))

(print (part1 "input0.txt"))
(print (part1 "input1.txt"))




