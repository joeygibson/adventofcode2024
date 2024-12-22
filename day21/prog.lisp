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
    (setf (gethash "0" keypad) (cons 1 0))
    (setf (gethash "A" keypad) (cons 2 0))
    (setf (gethash "1" keypad) (cons 0 1))
    (setf (gethash "2" keypad) (cons 1 1))
    (setf (gethash "3" keypad) (cons 1 2))
    (setf (gethash "4" keypad) (cons 0 2))
    (setf (gethash "5" keypad) (cons 1 2))
    (setf (gethash "6" keypad) (cons 2 2))
    (setf (gethash "7" keypad) (cons 0 3))
    (setf (gethash "8" keypad) (cons 1 3))
    (setf (gethash "9" keypad) (cons 2 3))
    keypad))

(defun make-directional-keyboard ()
  (let ((keypad (make-hash-table :test #'equal)))
    (setf (gethash "<" keypad) (cons 0 0))
    (setf (gethash "v" keypad) (cons 1 0))
    (setf (gethash ">" keypad) (cons 2 0))
    (setf (gethash "^" keypad) (cons 1 1))
    (setf (gethash "A" keypad) (cons 2 1))
    keypad))

(defun parse (file-name)
  (uiop:read-file-lines file-name))

(defun find-moves (start end)
  (cons (- (car end)
           (car start))
        (- (cdr end)
           (cdr start))))

(defun enter-code (keypad code)
  (let* ((keys (cons "A" (cl-ppcre:split "" code)))
         (pairs (pairwise keys))
         (all-moves nil))
    (format t "~&---~%code: ~a -> pairs: ~a~%" code pairs)
    (loop for (a b) in pairs
          do (let* ((key-a (gethash a keypad))
                    (key-b (gethash b keypad))
                    (moves (find-moves key-a key-b))
                    (tmp-move nil))
               (cond ((< (car moves) 0)
                      (push (mul-string (abs (car moves)) #\<) tmp-move))
                     ((> (car moves) 0)
                      (push (mul-string (abs (car moves)) #\>) tmp-move)))
               (cond ((< (cdr moves) 0)
                      (push (mul-string (abs (cdr moves)) #\v) tmp-move))
                     ((> (cdr moves) 0)
                      (push (mul-string (abs (cdr moves)) #\^) tmp-move)))
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
               (format t "~&~a~%~t~a~%~t~t~a~%~t~t~tcomplexity: ~a~%"
                       code-moves
                       dir-1-moves
                       dir-2-moves
                       complexity)
               (push complexity complexities)))
    (reduce #'+ complexities)))

(time (format t "~&part1: ~a~%" (part1 "input0.txt")))



