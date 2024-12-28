(require :sb-concurrency)
(use-package :sb-concurrency)

(require :cl-ppcre)
(require :split-sequence)
(require :lisp-utils)
(require :alexandria)
(require :lisp-utils)
(use-package :lisp-utils)

(defun make-numeric-keypad ()
  (let ((keypad (make-hash-table :test #'equal)))
    (setf (gethash (cons 0 0) keypad) "")
    (setf (gethash (cons 1 0) keypad) "0")
    (setf (gethash (cons 2 0) keypad) "A")
    (setf (gethash (cons 0 1) keypad) "1")
    (setf (gethash (cons 1 1) keypad) "2")
    (setf (gethash (cons 2 1) keypad) "3")
    (setf (gethash (cons 0 2) keypad) "4")
    (setf (gethash (cons 1 2) keypad) "5")
    (setf (gethash (cons 2 2) keypad) "6")
    (setf (gethash (cons 0 3) keypad) "7")
    (setf (gethash (cons 1 3) keypad) "8")
    (setf (gethash (cons 2 3) keypad) "9")
    keypad))

(defun make-directional-keyboard ()
  (let ((keypad (make-hash-table :test #'equal)))
    (setf (gethash (cons 0 0) keypad) "<")
    (setf (gethash (cons 1 0) keypad) "v")
    (setf (gethash (cons 2 0) keypad) ">")
    (setf (gethash (cons 0 1) keypad) "")
    (setf (gethash (cons 1 1) keypad) "^")
    (setf (gethash (cons 2 1) keypad) "A")
    keypad))

(defun parse (file-name)
  (uiop:read-file-lines file-name))

(defun exclude-gap (key)
  (equal key ""))

(defun find-key (keypad key)
  (loop for k being the hash-keys of keypad
          using (hash-value v)
        if (equal v key)
          return k))

(defun convert-moves (start end)
  (cons (- (car end)
           (car start))
        (- (cdr end)
           (cdr start))))

(defun enter-code (keypad code)
  (let* ((keys (cons "A" (cl-ppcre:split "" code)))
         (pairs (pairwise keys))
         (all-moves nil))
    ;(format t "~&---~%code: ~a -> pairs: ~a~%" code pairs)
    (loop for (a b) in pairs
          do (let* ((a-key (find-key keypad a))
                    (b-key (find-key keypad b))
                    (path (reverse (a-star a-key b-key keypad #'grid-neighbors #'(lambda (a b) 1) #'manhattan-distance #'exclude-gap)))                    
                    (moves (mapcar (lambda (pair)
                                     ;(format t "~&pair: ~a~%" pair)
                                     (convert-moves (car pair) (cadr pair)))
                                   (pairwise path)))
                    (tmp-move nil))
               ;; (format t "~&a: ~a, b: ~a~%" a b)
               ;; (format t "~&a-key: ~a, b-key: ~a~%" a-key b-key)
               ;; (format t "~&path: ~a~%" path)
               ;(format t "~&moves: ~a~%" moves)
               (loop for move in moves
                     do (progn
                          (cond ((< (car move) 0)
                                 (push (mul-string (abs (car move)) #\<) tmp-move))
                                ((> (car move) 0)
                                 (push (mul-string (abs (car move)) #\>) tmp-move)))
                          (cond ((< (cdr move) 0)
                                 (push (mul-string (abs (cdr move)) #\v) tmp-move))
                                ((> (cdr move) 0)
                                 (push (mul-string (abs (cdr move)) #\^) tmp-move)))))               
               (push (join "" (reverse tmp-move)) all-moves)))
    (format nil "~aA" (join "A" (reverse all-moves)))))

(defun part1 (file-name)
  (let* ((codes (parse file-name))
         (numeric-keypad (make-numeric-keypad))
         (dir-keypad (make-directional-keyboard))
         (complexities nil))
    (loop for code in codes
          do (let* ((code-moves (enter-code numeric-keypad code))
                    (dir-1-moves (enter-code dir-keypad code-moves))
                    (dir-2-moves (enter-code dir-keypad dir-1-moves))
                    (numeric-portion (parse-integer (remove #\A code)))
                    (complexity (* numeric-portion (length dir-2-moves))))
               (format t "~&~a~%~a~%~t~a~%~t~t~a~%~t~t~tcomplexity: ~a~%~%"
                       code
                       code-moves
                       dir-1-moves
                       dir-2-moves
                       complexity)
               (push complexity complexities)))
    (reduce #'+ complexities)))

(time (format t "~&part1: ~a~%" (part1 "input1.txt")))



