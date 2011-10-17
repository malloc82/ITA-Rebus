;; (ql:quickload :split-sequence)
(ql:quickload :cl-ppcre)

(defvar *index-hash* (make-hash-table :test 'equal))
(defvar *pictures-table* (make-hash-table :test 'equal))

(defun reset-tables ()
  (setq *index-hash* (make-hash-table :test 'equal))
  (setq *pictures-table* (make-hash-table :test 'equal))
  nil)

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
      (let ((pivot (first lst)))
        (append (quicksort-fn (remove-if-not #'(lambda (x)
                                                 (< (funcall fn x) (funcall fn pivot)))
                                             lst) :fn fn)
                (remove-if-not #'(lambda (x) (= (funcall fn x) (funcall fn pivot))) lst)
                (quicksort-fn (remove-if-not #'(lambda (x)
                                                 (> (funcall fn x) (funcall fn pivot)))
                                             lst) :fn fn)))))
;; (defun permutation (s k)
;;   (let ((perm-lst nil))
;;     (labels ((rec (s k entry)
;;                (cond ((equal k 0)
;;                       (push (reverse entry) perm-lst))
;;                      (t (loop
;;                            for i from 1 to s
;;                            do (unless (member i entry)
;;                                 (rec s (1- k) (cons i entry))))))))
;;       (loop
;;            for i from 1 to s
;;            do (rec s (1- k) (list i)))
;;       (nreverse perm-lst))
;;   ))

;; (defun permutation (bag)
;;   (if (null bag)
;;       '(())
;;       (mapcan #'(lambda (e)
;;                   (mapcar #'(lambda (p) (cons e p))
;;                           (permutation
;;                            (remove e bag :count 1))))
;;               bag)))

;; (defun permutation (bag n)
;;   (if (or (null bag) (zerop n))
;;       '(())
;;       (mapcan #'(lambda (e)
;;                   (mapcar #'(lambda (p) (cons e p))
;;                           (permutation2
;;                            (remove e bag :count 1)
;;                            (1- n))))
;;               bag)))

;; (defun combinations2 (s k)
;;   (let ((combinations nil))
;;     (labels ((rec (s k offset entry)
;;                (cond ((equal s k)
;;                       (push (reverse (loop for i from 0 to (1- s) do
;;                                           (push (+ i offset) entry)
;;                                         finally (return entry)))
;;                             combinations))
;;                      ((equal k 0)
;;                       (push (reverse entry) combinations))
;;                      (t (rec (1- s) (1- k) (1+ offset) (cons offset entry))
;;                         (rec (1- s) k (1+ offset) entry)))))
;;       (rec s k 0 nil))
;;     (nreverse combinations)))

(defun combinations (s k)
  (labels ((rec (s k offset)
             (cond ((= s k)
                    (list (loop for i from 0 to (1- s) collect (+ i offset))))
                   ((= k 1)
                    (loop for i from 0 to (1- s) collect (list (+ i offset))))
                   (t (append
                       (mapcar #'(lambda (index)
                                   (append (list offset) index))
                               (rec (1- s) (1- k) (1+ offset)))
                       (rec (1- s) k (1+ offset)))))))
    (rec s k 0)))

(defun binomial-exp (k &key (start 1) (end k))
  (let ((result nil))
    (labels ((rec (i)
               ;; (print end)
               (setq result (append (combinations k i) result))
               (if (not (equal i end))
                   (rec (1+ i)))))
      (rec start))
    result))

;; (defun substrs (str len)
;;   (let ((strlen (length str))
;;         (first-char (schar str 0)))
;;     (cond ((= strlen len)
;;            (list str))
;;            ;; (list (loop for c being the elements of str collect c)))
;;           ((= len 1)
;;            (loop for char across str collect (string char)))
;;            ;; (loop for char across str collect (list char)))
;;           (t (append
;;               (mapcar #'(lambda (str)
;;                           (concatenate 'string (string first-char) str))
;;                           ;; (append (list first-char) str))
;;                       (substrs (subseq str 1) (1- len)))
;;               (substrs (subseq str 1) len))))))

;; (defun all-substrs (str &key (start 1) (end (length str)))
;;   (let ((result nil))
;;     (labels ((rec (i)
;;                (setq result (append (substrs str i) result))
;;                (if (< i end)
;;                    (rec (1+ i)))))
;;              (rec start))
;;       result))

(defun calc-score (str)
  (let ((score 0))
    (loop for char being the elements of str
       do (if (find char "aeiou" :test 'equal)
              (setq score (1+ score))
              (setq score (+ score 5))))
    score))

(defmacro string-to-chars (str &optional pos-lst)
  (if (null pos-lst)
      `(mapcar #'(lambda (index) (schar ,str index))
           (loop for i from 0 upto (1- (length ,str)) collect i))
      `(mapcar #'(lambda (index) (schar ,str index))
               ,pos-lst)))

(defmacro string-inverse-pos (str pos-lst)
  `(let ((pos-lst ,pos-lst))
     (loop
        for char across ,str
        for i from 0 upto (1- (length ,str))
        if (or (null pos-lst)
               (/= i (car pos-lst))) collect char
        else do (pop pos-lst))))

;; (defun inverse-pos (word index-lst &key (form nil))
;;   (let ((char-lst nil))
;;     (loop
;;        for char being the elements of word
;;        for i from 0 upto (1- (length word))
;;        do (if (or (null index-lst)
;;                   (/= i (car index-lst)))
;;               (push char char-lst)
;;               (pop index-lst))
;;        finally (return (if (null form)
;;                            (nreverse char-lst)
;;                            (coerce (nreverse char-lst) form))))))

(defun createItem (word pos)
  (let* ((s (string-inverse-pos word pos)))
    (list :pos-str (mapcar #'(lambda (index) (schar word index)) pos)
          :word word
          :score (calc-score s)
          ;; :score (if (= len 0) 0
          ;;            (float (/ (calc-score s) (length s))))
          :pos pos
          :inverse-str s)))

(defun get-components (word)
  (let ((len (length word)))
  (mapcar #'(lambda (pos)
              (createItem word pos))
          (binomial-exp len))))

(defun add-partial-word (word pos)
  (let* ((inverse-str (string-inverse-pos word pos))
         (pos-str (string-to-chars word pos))
         (first-entry (first (getf (gethash pos-str *index-hash*) :partial))))
    (unless (and (equal (getf first-entry :word) word)
                 (equal (getf first-entry :inverse-str) inverse-str))
      (push (list :word word
                     :score (calc-score s)
                     :pos pos
                     :inverse-str inverse-str)
            (getf (gethash key *index-hash*) :partial)))))

(defun add-full-word (word pos-lst)
  (let ((index-key (string-to-chars word))
        (components-table (make-hash-table :test #'equal)))
    (mapc #'(lambda (pos)
              (setf (gethash (string-to-chars word pos) components-table)
                    (string-inverse-pos word pos)))
          pos-lst)
    (push (list :word word
                :score 0
                :components components-table)
          (getf (gethash index-key *index-hash*) :full))))

(defun setup-index (filename)
  (with-open-file (stream filename)
    (let ((word-lst nil)) 
      (do ((line (read-line stream nil)
                 (read-line stream nil)))
          ((null line))
        (let ((picture-words (cl-ppcre:split "\\s+" line)))
          (mapcar #'(lambda (word)
                      (let ((pos-lst (binomial-exp :start 1 :end (1- (length word)))))
                        (push (first picture-words) (gethash word *pictures-table*))
                        (mapcar #'(lambda (combination)
                                    (add-partial-word combination))
                                pos-lst) ;; excluding combination with all chars
                        (add-full-word word pos-lst)))
                  (cdr picture-words))
          (mapcar #'(lambda (word) (push (cons word (first picture-words)) word-lst))
                  (cdr picture-words))))
      (maphash #'(lambda (key value)
                   (setf (getf (gethash key *index-hash*) :partial)
                         (quicksort-fn (getf value :partial) :fn #'(lambda (x) (getf x :score)))))
               *index-hash*)
      nil)))



(defun setup-tables-2 (filename)
  (with-open-file (stream filename)
    (let ((word-lst nil)) 
      (do ((line (read-line stream nil)
                 (read-line stream nil)))
          ((null line))
        (let ((picture-words (cl-ppcre:split "\\s+" line)))
          (mapcar #'(lambda (word)
                      (let ((components-pos (binomial-exp (length word))))
                        (push (first picture-words) (gethash word *pictures-table*))
                        (mapcar #'(lambda (combination)
                                    (add-partial-word combination))
                                (cdr components-pos)) ;; excluding combination with all chars
                        (add-full-word word components-pos)))
                  (cdr picture-words))
          (mapcar #'(lambda (word) (push (cons word (first picture-words)) word-lst))
                  (cdr picture-words))))
      (maphash #'(lambda (key value)
                   (setf (getf (gethash key *index-hash*) :partial)
                         (quicksort-fn (getf value :partial) :fn #'(lambda (x) (getf x :score)))))
               *index-hash*)
      nil)))

;; try multiple values

;; (defun test-search (word)
;;   (let* ((total-score (calc-score (string-to-chars word)))
;;          (len (length word))
;;          (pos-lst (binomial-exp len :start (ash len -1) :end len)))
;;     (remove-if #'(lambda (x) (equal x "NOT FOUND"))
;;                (mapcar #'(lambda (comb)
;;                            (let* ((key (mapcar #'(lambda (index) (schar word index)) comb)) 
;;                                   (best-match (gethash key *index-hash*)))
;;                              (if best-match
;;                                  (cons (list :query word :comb key)
;;                                        (list (first (gethash key *index-hash*))))
;;                                  "NOT FOUND")))
;;                        pos-lst))))



;; (defun search-word2 (word)
;;   (let* ((word (string-downcase word))
;;          (len (length word))
;;          (components (mapcar  #'(lambda (pos-lst)
;;                                   (mapcar #'(lambda (pos)
;;                                               (schar word pos))
;;                                           pos-lst))
;;                               (binomial-exp len)))
;;          (result nil))
;;     (dolist (item components)
;;       (let ((match (gethash item *index-hash*)))
;;         (when match
;;           (push match result))))
;;     result))

(defun search-index (word)
  (let* ((word (string-downcase word))
         (word-len (length word))
         ;; (total-score (calc-score (string-to-chars word)))
         (search-table (make-hash-table :test #'equal))
         (match (gethash (string-to-chars word) *index-hash*))
         (result nil))
    
    (when (null match)
        (mapcar #'(lambda (pos-lst)
                    (let ((search-key (string-to-chars word pos-lst))
                          (inverse-str (string-inverse-pos word pos-lst)))
                      (when (gethash search-key *index-hash*)
                        (let ((full-match    (getf (gethash search-key *index-hash*) :full))
                              (partial-match (getf (gethash search-key *index-hash*) :partial)))
                          (setf (gethash search-key search-table)
                                (list :query (list :query-inverse-pos inverse-str
                                                   :query-score (calc-score inverse-str))
                                      :match (if (null full-match)
                                                 partial-match
                                                 full-match)))))))
                (binomial-exp word-len :start 1 :end (1- word-len)))
        
        (labels ((pop-top-matches ()
                   (let ((best-matches nil))
                     (maphash #'(lambda (key value)
                                  (push (list :score (+ (getf (getf value :query) :query-score)
                                                        (getf (getf value :match) :score))
                                              :key key
                                              :query (getf (getf value :query) :query-inverse-pos)
                                              :match (pop (getf (gethash key search-table) :match)))
                                        best-matches)
                                  (if (null (getf (gethash key search-table) :match))
                                            (remhash key search-table)))
                              search-table))))
          
          (do ((search-list (pop-top-matches) (pop-top-matches)))
              ((= (hash-table-count search-table) 0) result)
            (quicksort-fn search-list :fn #'(lambda (x) (getf x :score)))
            (return search-list)
            )))))
  


