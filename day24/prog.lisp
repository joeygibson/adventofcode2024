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
   (value :initarg :value :accessor value)))

(defmethod is-energized ((self wire))
  (not (null (value self))))

(defmethod is-on ((self wire))
  (= (value self) 1))

(defmethod turn-on ((self wire))
  (setf (value self) 1))

(defmethod turn-off ((self wire))
  (setf (value self) 0))

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

(defmethod print-object ((self gate) stream)
  (print-unreadable-object (self stream)
    (format t "~s: tripped: ~a" (type-of self) (tripped self))))


(defclass and-gate (gate) ())
(defclass or-gate (gate) ())
(defclass xor-gate (gate) ())

(defgeneric trip (gate))

(defmethod trip ((self and-gate))
  (with-accessors ((i1 input1)
                   (i2 input2)
                   (o output))
      self
    (when (and (is-energized i1)
               (is-energized i2))
      (setf (tripped self) t)
      (if (and (is-on i1)
               (is-on i2))
          (turn-on o)
          (turn-off o)))))

(defmethod trip ((self or-gate))
  (with-accessors ((i1 input1)
                   (i2 input2)
                   (o output))
      self
    (when (and (is-energized i1)
               (is-energized i2))
      (setf (tripped self) t)
      (if (or (is-on i1)
              (is-on i2))
          (turn-on o)
          (turn-off o)))))

(defmethod trip ((self xor-gate))
  (with-accessors ((i1 input1)
                   (i2 input2)
                   (o output))
      self
    (when (and (is-energized i1)
               (is-energized i2))
      (setf (tripped self) t)
      (if (or (and (is-on i1)
                   (not (is-on i2)))
              (and (not (is-on i1))
                   (is-on i2)))
          (turn-on o)
          (turn-off o)))))

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
                                     (make-wire (first chunks) nil)))
                    (input2 (gethash (third chunks) wires
                                     (make-wire (third chunks) nil)))
                    (gate-type (second chunks))
                    (output (gethash (nth 4 chunks) wires
                                     (make-wire (nth 4 chunks) nil)))
                    (gate (make-gate gate-type input1 input2 output)))
               (setf (gethash gate gates) gate)
               (when (not (gethash (name output) wires))
                 (setf (gethash (name output) wires) output))))
    (values wires gates)))

(defun get-output-values (wires)
  (let* ((output-wires (remove-if-not (lambda (wire)
                                        (char= (char (name wire) 0) #\z))
                                      (alexandria:hash-table-values wires)))
         (sorted-wires (sort output-wires (lambda (a b)
                                            (string< (name a)
                                                     (name b))))))
    (parse-integer (join "" (mapcar (lambda (i)
                                      (write-to-string i))
                                    (reverse (mapcar #'value sorted-wires))))
                   :radix 2)))

(defun part1 (file-name)
  (multiple-value-bind (wires gates) (parse file-name)
    (let* ((untripped-gates (remove-if (lambda (gate)
                                         (tripped gate))
                                       (alexandria:hash-table-values gates))))
      (loop while untripped-gates
            do (progn (loop for gate in (alexandria:hash-table-values gates)
                            do (progn
                                 ;(format t "~&gate: ~a~%" gate)
                                 (trip gate)))
                      (setf untripped-gates (remove-if (lambda (gate)
                                                         (tripped gate))
                                                       untripped-gates))))
      (print "getting output values")
      ;; (loop for gate in (alexandria:hash-table-values gates)
      ;;       do (print gate))
      (get-output-values wires))))

(time (format t "~&part1: ~a~%" (part1 "input2.txt")))




