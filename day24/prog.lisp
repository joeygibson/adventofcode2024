;(require :sb-concurrency)
;(use-package :sb-concurrency)

(require :cl-ppcre)
(require :split-sequence)
(require :lisp-utils)
(require :alexandria)
(require :lisp-utils)
(use-package :lisp-utils)

(defclass wire ()
  ((name :initarg :name :type string :reader name)
   (value :initarg :value :type integer :accessor value)
   (tripped :initform nil :type boolean :accessor tripped)))

(defmethod is-on ((self wire))
  (= (value self) 1))

(defmethod turn-on ((self wire))
  (when (char= (char (name self) 0) #\z)
    (format t "~&wire: ~a, turning ON~%" self))
  (setf (value self) 1)
  (setf (tripped self) t))

(defmethod turn-off ((self wire))
  (when (char= (char (name self) 0) #\z)
    (format t "~&wire: ~a, turning OFF~%" self))
  (setf (value self) 0)
  (setf (tripped self) t))

(defmethod print-object ((self wire) stream)
  (print-unreadable-object (self stream)
    (format t "~s: name: ~a, value: ~a" (type-of self) (name self) (value self))))

(defun make-wire (name value)
  (make-instance 'wire :name name :value value))

(defclass gate ()
  ((input1 :initarg :input1 :type wire :reader input1)
   (input2 :initarg :input2 :type wire :reader input2)
   (output :initarg :output :type wire :reader output)
   (tripped :initform nil :type boolean :accessor tripped)))

(defclass and-gate (gate) ())
(defclass or-gate (gate) ())
(defclass xor-gate (gate) ())

(defgeneric trip (gate))

(defmethod trip ((self and-gate))
  (with-accessors ((i1 input1)
                   (i2 input2)
                   (o output))
      self
    (if (and (is-on i1)
             (is-on i2))
        (turn-on o)
        (turn-off o))))

(defmethod trip ((self or-gate))
  (with-accessors ((i1 input1)
                   (i2 input2)
                   (o output))
      self
    (if (or (is-on i1)
            (is-on i2))
        (turn-on o)
        (turn-off o))))

(defmethod trip ((self xor-gate))
  (with-accessors ((i1 input1)
                   (i2 input2)
                   (o output))
      self
    (if (or (and (is-on i1)
                 (not (is-on i2)))
            (and (not (is-on i1))
                 (is-on i2)))
        (turn-on o)
        (turn-off o))))

(defun make-gate (gate-type input1 input2 output)
  (cond ((equal gate-type "AND")
         (make-instance 'and-gate :input1 input1 :input2 input2 :output output))
        ((equal gate-type "OR")
         (make-instance 'or-gate :input1 input1 :input2 input2 :output output))
        ((equal gate-type "XOR")
         (make-instance 'xor-gate :input1 input1 :input2 input2 :output output))))

(defun parse (file-name)
  (let* ((sections (split-file-into-sections file-name))
         (wires (make-hash-table :test #'equal))
         (gates (make-hash-table :test #'equal)))
    (loop for line in (first sections)
          do (let* ((chunks (cl-ppcre:split ": " line))
                    (wire (make-wire (first chunks) (parse-integer (second chunks)))))
               (setf (gethash (name wire) wires) wire)))
    (loop for line in (second sections)
          do (let* ((chunks (cl-ppcre:split " " line))
                    (input1 (gethash (first chunks) wires
                                     (make-wire (first chunks) 0)))
                    (input2 (gethash (third chunks) wires
                                     (make-wire (third chunks) 0)))
                    (gate-type (second chunks))
                    (output (gethash (nth 4 chunks) wires
                                     (make-wire (nth 4 chunks) 0)))
                    (gate (make-gate gate-type input1 input2 output)))
               (setf (gethash gate gates) gate)
               (when (not (gethash (name output) wires))
                 (setf (gethash (name output) wires) output))))
    (values wires gates)))

(defun get-outputs (wires)
  (remove-if-not (lambda (wire)
                   (char= (char (name wire) 0) #\z))
                 (alexandria:hash-table-values wires)))

(defun get-output-values (wires)
  (let* ((sorted-wires (sort wires (lambda (a b)
                                     (string< (name a)
                                              (name b))))))
    (parse-integer (join "" (mapcar (lambda (i)
                                      (write-to-string i))
                                    (reverse (mapcar #'value sorted-wires))))
                   :radix 2)))

(defun done (wires)
  (every (lambda (wire)
           (tripped wire))
         wires))

(defun part1 (file-name)
  (multiple-value-bind (wires gates) (parse file-name)
    (let* ((outputs (get-outputs wires)))
      (loop while (not (done outputs))
            do (loop for gate in (alexandria:hash-table-values gates)
                     do (progn
                          ;(format t "~&gate: ~a~%" gate)
                          (trip gate))))
      (print "getting output values")
      (get-output-values outputs))))

(time (format t "~&part1: ~a~%" (part1 "input1.txt")))




