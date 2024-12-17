(ql:quickload :cl-ppcre)
(ql:quickload :split-sequence)
(ql:quickload :lisp-utils)
(ql:quickload :alexandria)

(require :sb-concurrency)
(use-package :sb-concurrency)

(use-package :lisp-utils)

(defun parse (file-name)
  (let* ((lines (uiop:read-file-lines file-name))
         (a-val (parse-integer (first (cl-ppcre:all-matches-as-strings "(\\d+)" (first lines)))))
         (b-val (parse-integer (first (cl-ppcre:all-matches-as-strings "(\\d+)" (second lines)))))
         (c-val (parse-integer (first (cl-ppcre:all-matches-as-strings "(\\d+)" (third lines)))))
         (instructions (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings "(\\d+)" (fifth lines)))))
    (values instructions a-val b-val c-val)))

(defun join-strings (strings)
  (format nil "~{~a~^,~}" strings))

(defparameter *A* nil)
(defparameter *B* nil)
(defparameter *C* nil)
(defparameter *INT-PTR* 0)
(defparameter *PROGRAM* nil)
(defparameter *PROGRAM-LENGTH* 0)
(defparameter *OUTPUT-BUFFER* nil)

(defun handle-operand (operand)
  (cond ((<= 0 operand 3)
         operand)
        ((= operand 4)
         *A*)
        ((= operand 5)
         *B*)
        ((= operand 6)
         *C*)
        ((= operand 7)
         0)))

(defun adv (operand)
  (let* ((numerator *A*)
         (power (handle-operand operand))
         (denominator (expt 2 power)))
    (setf *A* (floor numerator denominator))))

(defun bxl (operand)
  (setf *B* (logxor *B* operand)))

(defun bst (operand)
  (setf *B* (mod (handle-operand operand) 8)))

(defun jnz (operand)
  (if (equal *A* 0)
      2
      operand))

(defun bxc ()
  (setf *B* (logxor *B* *C*)))

(defun out (operand)
  (push (mod (handle-operand operand) 8) *OUTPUT-BUFFER*)
  (print *OUTPUT-BUFFER*))

(defun bdv (operand)
  (let* ((numerator *A*)
         (power (handle-operand operand))
         (denominator (expt 2 power)))
    (setf *B* (floor numerator denominator))))

(defun cdv (operand)
  (let* ((numerator *A*)
         (power (handle-operand operand))
         (denominator (expt 2 power)))
    (setf *C* (floor numerator denominator))))

(defun process (opcode operand)
  (format t "~&opcode: ~a, operand: ~a~%" opcode operand)
  (let ((ptr-inc (cond ((= opcode 0)
                        (adv operand))
                       ((= opcode 1)
                        (bxl operand))
                       ((= opcode 2)
                        (bst operand))
                       ((= opcode 3)
                        (jnz operand))
                       ((= opcode 4)
                        (bxc))
                       ((= opcode 5)
                        (out operand))
                       ((= opcode 6)
                        (bdv operand))
                       ((= opcode 7)
                        (cdv operand)))))
    (if (and (= opcode 3)
             (not (= ptr-inc 2)))
        (setf *INT-PTR* ptr-inc)
        (incf *INT-PTR* 2))))

(defun interpret ()
  (loop named interpreter
        do (progn
             (when (>= *INT-PTR* *PROGRAM-LENGTH*)
               (return-from interpreter))
             (let* ((opcode (nth *INT-PTR* *PROGRAM*))
                    (operand (nth (1+ *INT-PTR*) *PROGRAM*)))
               (process opcode operand)
               (format t "~&INT: ~a~%" *INT-PTR*))))
  (if *OUTPUT-BUFFER*
      (format t "~&OUTPUT: ~a~%" (join-strings (reverse *OUTPUT-BUFFER*)))))

(defun part1 (file-name)
  (multiple-value-bind (instructions a-val b-val c-val) (parse file-name)
    (setf *A* a-val)
    (setf *B* b-val)
    (setf *C* c-val)
    (setf *PROGRAM* instructions)
    (setf *PROGRAM-LENGTH* (length instructions))
    
    (interpret)
    (format t "~&A: ~a, B: ~a, C: ~a~%" *A* *B* *C*)))

(time (print (part1 "input1.txt")))
; (time (print (part1 "input1.txt")))

; (time (print (part2 "input0.txt")))
; (time (print (part2 "input1.txt")))



