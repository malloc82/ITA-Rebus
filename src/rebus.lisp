;; (ql:quickload :split-sequence)
(ql:quickload :cl-ppcre)
(ql:quickload :iterate)
(use-package :iterate)

(defvar *index-hash* (make-hash-table :test 'equal))
(defvar *pictures-table* (make-hash-table :test 'equal))
(defvar *pair-index* (make-hash-table :test #'equal))

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
    (time (setq setup-output (setup-index filename)))
    (format t "~d~%" setup-output)
    (dump-table *pair-index* "pair.txt")
    (dump-table *index-hash* "index.txt")))

(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
        (multiple-value-bind (val win) (gethash args cache)
          (if win
              val
              (setf (gethash args cache) (apply fn args)))))))

(defun quicksort (lst)
  (if (or (null lst) (null (cdr lst)))
      lst
      (let ((pivot (first lst)))
        (append (quicksort (remove-if-not #'(lambda (x) (< x pivot)) lst))
                (remove-if-not #'(lambda (x) (= x pivot)) lst)
                (quicksort (remove-if-not #'(lambda (x) (> x pivot)) lst))))))

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
                                   (if (/= i end)
                                       (nconc item '(-)))
                                   (if (/= i 0)
                                       (push '- item))
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

(defun calc-score (str)
  (let ((score 0))
    (loop for char being the elements of str
       do (if (not (equal char #\-))
              (if (find char "aeiou" :test 'equal)
                  (setq score (1+ score))
                  (setq score (+ score 5)))))
    score))

(defmacro string-to-chars (str &optional pos-lst)
  (if (null pos-lst)
      `(mapcar #'(lambda (index) (schar ,str index))
           (loop for i from 0 upto (1- (length ,str)) collect i))
      `(mapcar #'(lambda (index)
                   (if (numberp index) (schar ,str index) #\-))
               ,pos-lst)))

(defmacro string-inverse-pos (str pos-lst)
  `(let ((pos-lst ,pos-lst))
     (loop
        for char across ,str
        for i from 0 upto (1- (length ,str))
        if (or (null pos-lst)
               (/= i (car pos-lst))) collect char
        else do (pop pos-lst))))

(defmacro list-inverse-pos (lst pos)
  `(let ((pos ,pos))
     (iterate (for elem in ,lst)
              (for i from 0 to (1- (length ,lst)))
              (if (null pos)
                  (collect elem into result)
                  (if (not (equal i (first pos)))
                      (progn
                        (when (equal (first pos) '-)
                          (pop pos))
                        (collect elem into result))
                      (pop pos)))
              (finally (return result)))))

(defmacro list-pos (lst pos)
  `(let ((pos ,pos))
     (iterate (for elem in ,lst)
              (for i from 0 to (1- (length ,lst)))
              (when pos
                (if (equal i (first pos))
                    (progn
                      (pop pos)
                      (collect elem into result))
                    (if (equal '- (first pos))
                        (progn
                          (pop pos)
                          (collect #\- into result)))))
              (finally (return result)))))

(defun add-partial-word (word pos)
  (let* ((pos-chars (string-to-chars word pos))
         (subtraction (string-inverse-pos word pos))
         (first-entry (first (getf (gethash pos-chars *index-hash*) :partial))))
    (unless (and (equal (getf first-entry :word) word)
                 (equal (getf first-entry :subtraction) subtraction))
      (push (list :word word
                  :subtraction-score (calc-score subtraction)
                  :subtraction subtraction
                  :key-index pos)
            (getf (gethash pos-chars *index-hash*) :partial)))))

(defun add-full-word (word)
  (let ((index-key (string-to-chars word)))
    (unless (getf (gethash index-key *index-hash*) :full)
      (push (list :word word
                  :score 0)
            (getf (gethash index-key *index-hash*) :full)))))

(setq end-index-cache-fn (memoize #'end-index))
(setq binomial-exp-cache-fn (memoize #'binomial-exp))

(defun setup-index (filename)
  (with-open-file (stream filename)
    (let ((scratch-table (make-hash-table :test #'equal))) 
      (do ((line (read-line stream nil)
                 (read-line stream nil)))
          ((null line))
        (let ((picture-words (cl-ppcre:split "\\s+" line)))
          (mapcar #'(lambda (word)
                      (let* ((word-len (length word))
                             (pos-lst (funcall binomial-exp-cache-fn
                                               word-len :start 1 :end (1- word-len))))
                        (push (first picture-words) (gethash word *pictures-table*))
                        (mapcar #'(lambda (pos)
                                    (add-partial-word word pos))
                                pos-lst) ;; excluding combination with all chars
                        (add-full-word word)))
                  (cdr picture-words))))
      ;; sort each entry of *index-hash*
      (maphash #'(lambda (key value)
                   (setf (getf (gethash key *index-hash*) :partial)
                         (quicksort-fn (getf value :partial) :fn #'(lambda (x) (getf x :subtraction-score)))))
               *index-hash*)

      ;; setting up *pair-index*
      (maphash #'(lambda (key value)
                   (when (getf value :full)
                     ;; (if (getf value :partial) (print (coerce key 'string)))
                     (mapc #'(lambda (components)
                               (let* ((chars (getf components :subtraction))
                                      (chars-len (length chars)))
                                 (symbol-macrolet ((exact-pair-words (list
                                                                      :first (getf components :word)
                                                                      :second (getf (first (getf value :full))
                                                                              :word)))
                                                   (pair-entry (getf (gethash chars *pair-index*)
                                                                     :full-pair))
                                                   (all-combo-of-subtraction-part
                                                    (funcall binomial-exp-cache-fn chars-len :start 1 :end (1- chars-len) :slot t)))
                                   (push exact-pair-words pair-entry)
                                   ;; (if (equal chars '(#\d #\e))
                                   ;;     (format t "~d~%" chars-len))
                                   (mapc #'(lambda (pos)
                                             (let ((distance-chars (list-pos chars pos))
                                                   (partial-chars  (list-inverse-pos chars pos)))
                                               ;; (if (equal chars '(#\d #\e))
                                               ;;     (format t "~d~%" pos))
                                                 (push (list :pair-key chars :val partial-chars)
                                                       (gethash distance-chars scratch-table))                                                 
                                                 (push (list :pair-key chars
                                                             :pair-distance distance-chars
                                                             ;; :pos pos
                                                             ;; :partial-chars partial-chars
                                                             :pair-score (calc-score distance-chars))
                                                       (getf (gethash partial-chars *pair-index*) :partial-pair))))
                                         all-combo-of-subtraction-part))))
                           ;; (funcall end-index-cache-fn chars-len))))
                           (getf value :partial))))
               *index-hash*)

      (dump-table scratch-table "scratch-table.txt")
      ;; setup lookup table for each pair-entry 
      (maphash #'(lambda (key value)
                   (symbol-macrolet ((other-pair-lut (getf (gethash key *pair-index*) :other-pair)))
                     (setf other-pair-lut (make-hash-table :test #'equal))
                     (mapc #'(lambda (partial-key)
                               (let ((pattern (getf partial-key :pair-distance)))
                                 (loop for elem being the elements of (gethash pattern scratch-table)
                                    do (symbol-macrolet ((other-pair-entry
                                                          (gethash (getf elem :val) other-pair-lut)))
                                         (let ((match-info (list :pair-key (getf elem :pair-key)
                                                                 :distance pattern)))
                                           (unless (equal match-info (first other-pair-entry))
                                             (push match-info other-pair-entry)))))))
                           (getf value :partial-pair))))
               *pair-index*)

      ;; sort each entry of *pair-index*
      (maphash #'(lambda (key value)
                   (setf (getf (gethash key *pair-index*) :partial-pair)
                         (quicksort-fn (getf value :partial-pair) :fn #'(lambda (x) (getf x :pair-score)))))
               *pair-index*)

      ;; (maphash #'(lambda (key value)
      ;;              (when (getf value :partial)
      ;;                (setf (getf value :partial)
      ;;                      (mapcar #'(lambda (item)
      ;;                                  (if (gethash (getf item :subtraction) *pair-index*)
      ;;                                      (setf (getf item :pic-distance) )))
      ;;                              (getf value :partial))))))
      
      (list :index-hash-count (hash-table-count *index-hash*)
            :pair-index-count (hash-table-count *pair-index*)))))
