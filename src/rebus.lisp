;; (ql:quickload :split-sequence)
(ql:quickload :cl-ppcre)
(defpackage :ita-rebus (:use :cl :cl-ppcre))
(in-package :ita-rebus)

(defvar *pair-pattern-table* (make-hash-table :test 'equal))
(defvar *pictures-table* (make-hash-table :test 'equal))

(defvar *index-hash* (make-hash-table :test 'equal))
(defstruct vocabulary  ;; for index-hash
  (full    nil :type list)
  (partial nil :type list))

(defstruct pword-info
  (word        ""  :type string)
  (score       0   :type integer)
  (subtraction nil :type list)
  (match-index nil :type list))

(defmethod push-full ((word string))
  (let ((key (string-to-chars word)))
    (symbol-macrolet ((index-entry (gethash key *index-hash*)))
      (if index-entry
          (push full-word (vocabulary-full index-entry))
          (setf index-entry (make-vocabulary :full (list full-word)))))))

(defmethod push-partial ((word string))
  (let ((word-len (length word)))
    (mapc #'(lambda (pos)
              (let* ((key (string-to-chars word pos))
                     (subtraction (string-inverse-pos word pos))
                     (pword (make-pword-info :word word
                                             :score (calc-score subtraction)
                                             :subtraction subtraction
                                             :match-index pos)))
                (symbol-macrolet ((index-entry (gethash key *index-hash*))
                                  (first-entry (first (vocabulary-partial index-entry))))
                  (if index-entry
                      (unless (and (equal (pword-info-word first-entry) word)
                                   (equal (pword-info-subtraction first-entry) subtraction))
                        (push pword (vocabulary-partial index-entry)))
                      (setf index-entry (make-vocabulary :partial (list pword)))))))
          (funcall binomial-exp-cache-fn
                   word-len :start 1 :end (1- word-len)))))

(defvar *pair-index* (make-hash-table :test #'equal))
(defstruct pair ;; for pair-index
  (full    nil :type list)
  (partial nil :type list)
  (other   (make-hash-table :test #'equal) :type hash-table))

(defstruct full-pair
  (first  nil :type list)
  (second nil :type list))

(defstruct partial-pair
  (key      nil :type list)
  (distance nil :type list)
  (score    0   :type integer))

(defmethod push-full ((first list) (second pword-info))
  (let ((chars (pword-info-substraction second))
        (pair-pic (make-full-pair :first (pword-info-word second)
                                  :second (coerce first 'string))))
    (symbol-macrolet ((pair-entry (gethash chars *pair-index*)))
      (if pair-entry
          (push pair-pic (pair-full pair-entry))
          (setf pair-entry (make-pair :full (list pair-pic)))))))

(defmethod push-partial ((first list) (second pword-info))
  (let ((chars (pword-info-subtraction second))
        (chars-len (length chars)))
    (symbol-macrolet ((all-combos-of-subtraction
                       (funcall binomial-exp-cache-fn chars-len :start 1 :end (1- chars-len) :slot t)))
      (loop for pos in all-combos-of-subtraction do
           (let* ((distance-pattern (list-pos chars pos))
                  (partial-key      (list-inverse-pos chars pos))
                  (partial-pair-pic (make-partial-pair :key chars
                                                       :distance distance-pattern
                                                       :score (calc-score distance-pattern))))
             (push (list :pair-key chars :val partial-key)
                   (gethash distance-pattern *pair-pattern-table*))
             (symbol-macrolet ((partial-entry (gethash partial-key *pair-index*)))
               (if partial-entry
                   (unless (and (equal (partial-pair-key (first (pair-partial partial-entry))))))
                   (push partial-pair-pic ))))))))

(defmethod push-partial (key (ppair partial-pair))
  (symbol-macrolet ((pair-entry (gethash key *pair-index*)))
    (if pair-entry
        (push ppair (pair-partial pair-entry))
        (setf pair-entry (make-pair :partial (list ppair))))))

(defmethod )

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
  `(let ((pos ,pos)
         (result nil))
     (loop
        for elem in ,lst
        for i from 0 to (1- (length ,lst))
        do
          (if (null pos)
              (push elem into result)
              (if (not (equal i (first pos)))
                  (progn
                    (when (equal (first pos) '-)
                      (pop pos))
                    (push elem into result))
                  (pop pos)))
        finally (return (nreverse result)))))

(defmacro list-pos (lst pos)
  `(let ((pos ,pos)
         (result nil))
     (loop
        for elem in ,lst
        for i from 0 to (1- (length ,lst))
        do
          (when pos
            (if (equal i (first pos))
                (progn
                  (pop pos)
                  (push elem into result))
                (if (equal '- (first pos))
                    (progn
                      (pop pos)
                      (push #\- reuslt)))))
        finally (return (nreverse result)))))

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

(defun add-partial-word (word pos)
  (let* ((pos-chars (string-to-chars word pos))
         (subtraction (string-inverse-pos word pos))
         ())
    (symbol-macrolet ((pos-entry (getf pos-chars *index-hash*)))
      
      )
    (if (getf ))))

(defun add-full-word (word)
  (let ((index-key (string-to-chars word)))
    (unless (getf (gethash index-key *index-hash*) :full)
      (push (list :word word :score 0)
            (getf (gethash index-key *index-hash*) :full)))))

(defun add-vocabularies (word)
  (let ((word-len (length word)))
    (loop for pos in (funcall binomial-exp-cache-fn
                           word-len :start 1 :end (1- word-len))
       do (add-partial-word word pos)) ;; excluding combination with all chars
    (add-full-word word)))

(setq end-index-cache-fn (memoize #'end-index))
(setq binomial-exp-cache-fn (memoize #'binomial-exp))

(defun setup-index (filename)
  (let ((scratch-table (make-hash-table :test #'equal))) 
    (with-open-file (stream filename)
      (do ((line (read-line stream nil)
                 (read-line stream nil)))
          ((null line))
        (let ((picture-words (cl-ppcre:split "\\s+" line)))
          (mapcar #'(lambda (word)
                      (push (first picture-words) (gethash word *pictures-table*))
                      (add-vocabularies word))
                  (cdr picture-words)))))
      ;; sort each entry of *index-hash*
    (maphash #'(lambda (key value)
                 (setf (vocabulary-partial (gethash key *index-hash*))
                       (quicksort-fn (vocabulary-partial value)
                                     :fn #'(lambda (x) (vocabulary-score x)))))
             *index-hash*)

    ;; setting up *pair-index*
    (maphash #'(lambda (key hash-entry)
                 (when (vocabulary-full hash-entry)
                   ;; (if (getf value :partial) (print (coerce key 'string)))
                   (loop for components in (vocabulary-partial hash-entry) do
                        (let* ((chars (pword-info-subtraction components))
                               (chars-len (length chars))
                               (exact-pair-words (make-full-pair
                                                  :first (vocabulary-word components)
                                                  :second (pword-info-word
                                                           (first (vocabulary-full hash-entry))))))
                          (symbol-macrolet ((pair-entry (gethash chars *pair-index*))
                                            (all-combo-of-subtraction-part
                                             (funcall binomial-exp-cache-fn chars-len :start 1 :end (1- chars-len) :slot t)))
                            (push-full chars exact-pair-words)
                            ;; (if (equal chars '(#\d #\e))
                            ;;     (format t "~d~%" chars-len))
                            (loop for pos in all-combo-of-subtraction-part do 
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
                                         (getf (gethash partial-chars *pair-index*) :partial-pair)))))))))
             *index-hash*)

    (dump-table scratch-table "scratch-table.txt")
    ;; setup lookup table for each pair-entry 
    (maphash #'(lambda (key value)
                 (symbol-macrolet ((other-pair-lut (getf (gethash key *pair-index*) :other-pair)))
                   (setf other-pair-lut (make-hash-table :test #'equal))
                   (loop for partial-key in (getf value :partial-pair) do
                        (let ((diff-pattern (getf partial-key :pair-distance)))
                          (loop for elem being the elements of (gethash diff-pattern scratch-table)
                             do (symbol-macrolet ((other-pair-entry
                                                   (gethash (getf elem :val) other-pair-lut)))
                                  (let ((match-info (list :pair-key (getf elem :pair-key)
                                                          :distance diff-pattern)))
                                    (unless (equal match-info (first other-pair-entry))
                                      (push match-info other-pair-entry)))))))))
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
          :pair-index-count (hash-table-count *pair-index*))))
