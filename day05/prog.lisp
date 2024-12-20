(ql:quickload :cl-ppcre)
(ql:quickload :split-sequence)
(ql:quickload :lisp-utils)
(ql:quickload :alexandria)

(use-package :lisp-utils)

(defun parse (file-name)
  (let* ((sections (split-file-into-sections file-name))
         (rules (mapcar (lambda (line)
                          (mapcar #'parse-integer (cl-ppcre:split "\\|" line)))
                        (first sections)))
         (updates (mapcar (lambda (line)
                            (mapcar #'parse-integer (cl-ppcre:split "," line)))
                          (second sections))))
    (values rules updates)))

(defun is-valid (page rules update)
  (let* ((previous-pages (gethash page rules))
         (included-pages (intersection previous-pages update))
         (page-position (position page update)))
    (every (lambda (previous-page)
             (let ((previous-page-position (position previous-page update)))
               (< previous-page-position page-position)))
           included-pages)))

(defun make-rule-table (rules)
  (let ((rule-map (make-hash-table)))
    (loop for rule in rules
          do (let* ((page (second rule))
                    (previous-page (first rule)))
               (setf (gethash page rule-map)
                     (push previous-page (gethash page rule-map)))))
    rule-map))

(defun validate-updates (rule-map updates)
  (remove-if-not (lambda (update)
                   (every (lambda (page)
                            (is-valid page rule-map update))
                          update))
                 updates))

(defun part1 (file-name)
  (multiple-value-bind (rules updates) (parse file-name)
    (let* ((rule-map (make-rule-table rules))
           (valid-updates (validate-updates rule-map updates)))      
      (reduce #'+ (mapcar (lambda (update)
                            (nth (floor (length update) 2) update))
                          valid-updates)))))

(defun part2 (file-name)
  (multiple-value-bind (rules updates) (parse file-name)
    (let* ((rule-map (make-rule-table rules))
           (valid-updates (validate-updates rule-map updates))
           (invalid-updates (remove-if (lambda (update)
                                         (position update valid-updates))
                                       updates))
           (answer 0))
      (dolist (update invalid-updates)
        (let* ((counts (make-hash-table)))
          (maphash (lambda (page previous-pages)
                     (loop for previous-page in previous-pages
                           do (when (and (member page update)
                                         (member previous-page update))
                                (incf (gethash page counts 0)))))
                   rule-map)
          (loop named loop
                for page in update
                do (when (= (gethash page counts 0) (floor (length update) 2))
                     (incf answer page)
                     (return-from loop)))))
      answer)))

(print (part1 "input0.txt"))
(print (part1 "input1.txt"))

(print (part2 "input0.txt"))
(print (part2 "input1.txt"))
