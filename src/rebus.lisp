;; (ql:quickload :split-sequence)
(ql:quickload :cl-ppcre)
(defpackage :ita-rebus (:use :cl :cl-ppcre))
(in-package :ita-rebus)

(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
        (multiple-value-bind (val win) (gethash args cache)
          (if win
              val
              (setf (gethash args cache) (apply fn args)))))))

(defun quicksort-fn (lst &key (fn #'values))
  (if (or (null lst) (null (cdr lst)))
      lst
      (let ((pivot (funcall fn (first lst))))
        (append (quicksort-fn (remove-if-not #'(lambda (x)
                                                 (< (funcall fn x) pivot))
                                             lst) :fn fn)
                (remove-if-not #'(lambda (x) (= (funcall fn x) pivot)) lst)
                (quicksort-fn (remove-if-not #'(lambda (x)
                                                 (> (funcall fn x) pivot))
                                             lst) :fn fn)))))

(defun combinations (s k &key (slot nil))
  (labels ((rec- (s k offset)
             (cond ((= s k)
                    (list (loop for i from 0 to (1- s) collect (+ i offset))))
                   ((= k 1)
                    (let ((end (1- s)))
                      (loop for i from 0 to end
                         collect (let ((item (list (+ i offset))))
                                   (if (/= i end) (nconc item '(-)))
                                   (if (/= i 0) (push '- item))
                                   item))))
                   (t (append
                       (mapcar #'(lambda (index)
                                   (if (or (equal '- (first index))
                                           (= (1+ offset) (first index)))
                                       (append (list offset) index)
                                       (append (list offset) '(-) index)))
                               (rec- (1- s) (1- k) (1+ offset)))
                       (mapcar #'(lambda (index-lst)
                                   (if (equal (first index-lst) '-)
                                       index-lst
                                       (append '(-) index-lst)))
                               (rec- (1- s) k (1+ offset)))))))
           (rec (s k offset)
             (cond ((= s k)
                    (list (loop for i from 0 to (1- s) collect (+ i offset))))
                   ((= k 1)
                      (loop for i from 0 to (1- s) collect (list (+ i offset))))
                   (t (append
                       (mapcar #'(lambda (index)
                                       (append (list offset) index))
                               (rec (1- s) (1- k) (1+ offset)))
                       (rec (1- s) k (1+ offset)))))))
    (if (null slot)
        (rec s k 0)
        (rec- s k 0))))

(defun binomial-exp (k &key (start 1) (end k) (slot nil))
  (let ((result nil))
    (labels ((rec (i)
               (setq result (append (combinations k i :slot slot) result))
               (if (not (equal i end))
                   (rec (1+ i)))))
      (when (and (> k 0) (> start 0) (> end 0) (>= end start)) ;; valid arguments
        (rec start)))
    result))
(setq binomial-exp-cache-fn (memoize #'binomial-exp))

(defun end-index (length)
  (append
   (loop for j from 1 upto (1- length) collect
        (append '(-) (loop for k from j upto (1- length) collect k)))
   (loop
      for i from 2 upto length append
        (loop
           for j from i upto length collect
             (append
              (loop for k from 0 upto (- i 2) collect k)
              '(-)
              (loop for k from j upto (1- length) collect k))))))
(setq end-index-cache-fn (memoize #'end-index))

(defun calc-score (str)
  (let ((score 0))
    (loop for char being the elements of str
       do (if (not (equal char #\-))
              (if (find char "aeiou" :test 'equal)
                  (setq score (1+ score))
                  (setq score (+ score 5)))))
    score))

(defun string-to-chars (str &optional pos-lst)
  (if (null pos-lst)
      (mapcar #'(lambda (index) (schar str index))
              (loop for i from 0 upto (1- (length str)) collect i))
      (mapcar #'(lambda (index)
                  (if (numberp index) (schar str index) #\-))
              pos-lst)))

(defun string-inverse-pos (str pos-lst)
  (loop
     for char across str
     for i from 0 upto (1- (length str))
     if (or (null pos-lst)
            (/= i (car pos-lst))) collect char
     else do (pop pos-lst)))

(defun list-inverse-pos (lst pos)
  (let ((result nil))
    (loop
       for elem in lst
       for i from 0 to (1- (length lst)) do
         (if (null pos)
             (push elem result)
             (if (not (equal i (first pos)))
                 (progn
                   (when (equal (first pos) '-)
                     (pop pos))
                   (push elem result))
                 (pop pos)))
       finally (return (nreverse result)))))

(defun list-pos (lst pos)
  (let ((result nil))
    (loop
       for elem in lst
       for i from 0 to (1- (length lst)) do
         (when pos
           (if (equal i (first pos))
               (progn
                 (pop pos) (push elem result))
               (if (equal '- (first pos))
                   (progn
                     (pop pos) (push #\- result)))))
       finally (return (nreverse result)))))

;; index tables 
(defvar *pictures-table* (make-hash-table :test 'equal))

(defvar *index-hash* (make-hash-table :test 'equal))

(defun add-partials (word)
  (let* ((word-len (length word))
         (pos-lst (funcall binomial-exp-cache-fn
                           word-len :start 1 :end (1- word-len))))
    (mapc #'(lambda (pos)
              (let* ((pos-chars (string-to-chars word pos))
                     (subtraction (string-inverse-pos word pos))
                     (partial-word (list :word word
                                         :socre (calc-score subtraction)
                                         :subtraction subtraction
                                         :key-index pos)))
                (symbol-macrolet ((index-entry (getf (gethash pos-chars *index-hash*) :partial)))
                  (unless (and (equal (getf (first index-entry) :word) word)
                               (equal (getf (first index-entry) :subtraction) subtraction))
                    (push partial-word index-entry)))))
          pos-lst)))

(defun add-full-word (word)
  (let ((index-key (string-to-chars word)))
    (unless (getf (gethash index-key *index-hash*) :full)
      (setf (getf (gethash index-key *index-hash*) :full) word))))

(defvar *pattern-table* (make-hash-table :test 'equal))
(defvar *pair-index* (make-hash-table :test #'equal))

(defun add-full-pairs (index-key)
  (let ((full-word (getf (gethash index-key *index-hash*) :full))
        (full-pair-list nil))
    (mapc #'(lambda (partial-word)
              (let ((pair-table-index (getf partial-word :subtraction))
                    (full-pair (list :first (getf partial-word :word)
                                     :second full-word)))
                (symbol-macrolet ((full-pair-entry
                                   (getf (gethash pair-table-index *pair-index*) :full-pair)))
                  (unless (equal full-pair (first full-pair-entry))
                    (push full-pair full-pair-entry)
                    (push pair-table-index full-pair-list)))))
          (getf (gethash index-key *index-hash*) :partial))
    full-pair-list))

(defun add-partial-pairs (full-pair)
  (let ((len (length full-pair)))
    (mapc #'(lambda (diff-pos)
              (let* ((diff-pattern  (list-pos full-pair diff-pos))
                     (partial-match (list-inverse-pos full-pair diff-pos))
                     (partial-pair  (list :full-pair full-pair
                                          :diff-pattern diff-pattern
                                          :socre (calc-score diff-pattern))))
                (symbol-macrolet ((partial-pair-entry
                                   (getf (gethash partial-match *pair-index*) :partial-pair)))
                  (unless (and (equal (getf (first partial-pair-entry) :full-pair) full-pair)
                               (equal (getf (first partial-pair-entry) :diff-pattern) diff-pattern))
                    (push partial-pair partial-pair-entry)
                    (push (list :full-pair full-pair :partial-match partial-match) ;; pattern table
                          (gethash diff-pattern *pattern-table*))))))
          (funcall binomial-exp-cache-fn len :start 1 :end (1- len) :slot t))))

(defun reset-tables ()
  (setq *index-hash* (make-hash-table :test 'equal))
  (setq *pictures-table* (make-hash-table :test 'equal))
  (setq *pair-index* (make-hash-table :test 'equal))
  nil)

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

(defun testrun (filename)
  (let ((setup-output nil))
    (reset-tables)
    (time (setq setup-output (setup-index-hash filename)))
    (format t "~d~%" setup-output)
    (dump-table *pair-index* "pair.txt")
    (dump-table *index-hash* "index.txt")))

(defun setup-index-hash (filename)
  (with-open-file (stream filename)
    (do ((line (read-line stream nil)
               (read-line stream nil)))
        ((null line))
      (let ((picture-words (cl-ppcre:split "\\s+" line)))
        (mapcar #'(lambda (word)
                    (push (first picture-words) (gethash word *pictures-table*))
                    (add-full-word word)
                    (add-partials word))
                (cdr picture-words)))))
  ;; sort each entry of *index-hash*
  (maphash #'(lambda (key value)
               (setf (getf (gethash key *index-hash*) :partial)
                     (quicksort-fn (getf value :partial) :fn #'(lambda (x) (getf x :subtraction-score)))))
           *index-hash*))

;; smaller hash-table for each partial matches?

(defun setup-pair-index ()
  (when (> (hash-table-count *index-hash*) 0)
    (loop for word being the hash-key of *pictures-table* do 
         (mapc #'add-partial-pairs (add-full-pairs (string-to-chars word))))
    (maphash #'(lambda (pair-key pair-matches)
                 (symbol-macrolet ((other-pair-lut (getf (gethash pair-key *pair-index*) :other-pair)))
                   (setf other-pair-lut (make-hash-table :test #'equal))
                   (mapc #'(lambda (partial-pair)
                             (let ((diff-pattern (getf partial-pair :diff-pattern)))
                               (loop for partial-match in (gethash diff-pattern *pattern-table*) do
                                    (symbol-macrolet ((other-pair-entry
                                                       (gethash (getf partial-match :partial-match)
                                                                other-pair-lut)))
                                      (let ((match-info (list :full-pair (getf partial-match :full-pair)
                                                              :diff-pattern diff-pattern)))
                                        (unless (equal (first other-pair-entry) match-info)
                                          (push match-info other-pair-entry)))))))
                         (getf pair-matches :partial-pair))))
             *pair-index*)
    (maphash #'(lambda (pair-key pair-matches)
                 (setf (getf (gethash pair-key *pair-index*) :partial-pair)
                       (quicksort-fn (getf pair-matches :partial-pair)
                                     :fn #'(lambda (x) (getf x :score)))))
             *pair-index*)))
