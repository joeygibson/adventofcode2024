(ql:quickload :cl-ppcre)
(ql:quickload :split-sequence)
(ql:quickload :lisp-utils)
(ql:quickload :alexandria)

(use-package :lisp-utils)

(defun all-combinations (values num-items)
  "Generate all combinations for NUM-ITEMS, each of which can take any of the VALUES."
  (if (= num-items 0)
      '(())
      (let ((rest-combinations (all-combinations values (- num-items 1))))
        (mapcan (lambda (value)
                  (mapcar (lambda (combination)
                            (cons value combination))
                          rest-combinations))
                values))))

(defun interleave (list0 list1)
  (cond ((and (null list0) (null list1)) nil)
        ((null list0) list1)
        ((null list1) list0)
        (t (cons (car list0)
                 (cons (car list1)
                       (interleave (cdr list0) (cdr list1)))))))

(defun parse (file-name)
  (let* ((lines (uiop:read-file-lines file-name)))
    (mapcar (lambda (line)
              (mapcar #'parse-integer
                      (cl-ppcre:all-matches-as-strings "(\\d+)" line)))
            lines)))

(defun solve (ops)
  (let* ((op0 (first ops))
         (op1 (second ops))
         (op2 (third ops))
         (remaining-ops (cdddr ops))
         (solution (cond ((equal op1 "*")
                          (* op0 op2))
                         ((equal op1 "+")
                          (+ op0 op2))
                         ((equal op1 "||")
                          (parse-integer (format nil "~a~a" op0 op2))))))
    (if remaining-ops
        (solve (cons solution remaining-ops))
        solution)))

(defun part1 (file-name)
  (let* ((equations (parse file-name))
         (solvable nil))
    (dolist (equation equations)
      (let* ((answer (car equation))
             (operands (cdr equation))
             (operators (all-combinations '("+" "*") (1- (length operands)))))
        (loop for opset in operators
              when (let ((solution (solve (interleave operands opset))))
                     (eq solution answer))
                do (progn
                     (push answer solvable)
                     (return 'done)))))
    (reduce #'+ solvable)))

(defun part2 (file-name)
  (let* ((equations (parse file-name))
         (solvable nil))
    (dolist (equation equations)
      (let* ((answer (car equation))
             (operands (cdr equation))
             (operators (all-combinations '("+" "*" "||") (1- (length operands)))))
        (loop for opset in operators
              when (let ((solution (solve (interleave operands opset))))
                     (eq solution answer))
                do (progn
                     (push answer solvable)
                     (return 'done)))))
    (reduce #'+ solvable)))

(print (part1 "input0.txt"))
(print (part1 "input1.txt"))

(print (part2 "input0.txt"))
(print (part2 "input1.txt"))



