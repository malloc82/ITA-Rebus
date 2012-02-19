(in-package :ita-rebus)

(defun dump-table (table filename)
  (with-open-file (dump filename
                        :direction :output
                        :if-exists :supersede)
    (maphash #'(lambda (key value)
                 (format dump "~10@a : ~a~%"
                         (coerce key 'string) value))
             table)))

(defun print-pair-table (entry)
  (let ((lut (getf (gethash (string-to-chars entry) *pair-index*) :lut)))
    (maphash #'(lambda (key value)
                 (format t "~10@a : ~a" (coerce key 'string) value))
             lut)))

(defun print-table (table)
  (maphash #'(lambda (key value)
               (format t "~10@a : ~a~%" (coerce key 'string) value))
             table))

(defun reset-tables ()
  (setq *index-hash*     (make-hash-table :test 'equal))
  (setq *pictures-table* (make-hash-table :test 'equal))
  (setq *pair-index*     (make-hash-table :test 'equal))
  (setq *pattern-table*  (make-hash-table :test 'equal))
  ;; test
  (setq *subtraction-pattern* (make-hash-table :test 'equal))
  nil)


(defun testrun (filename)
  (let ((setup-output nil))
    (reset-tables)

    (format t "Indexing *index-hash*~%")
    (time (setq setup-output (setup-index-hash filename)))

    (format t "Setting up *pair-index*")
    (time (setup-pair-index))

    ;; test
    (loop for key being the hash-key of *subtraction-pattern* do
         (if (= (getf (gethash key *subtraction-pattern*) :count) 1)
             (remhash key *subtraction-pattern*)))
    
    (format t "~d~%" setup-output)
    (dump-table *pair-index*    "pair.txt")
    (dump-table *index-hash*    "index.txt")
    (dump-table *pattern-table* "pattern.txt")
    (dump-table *subtraction-pattern* "sub-pattern.txt")))
