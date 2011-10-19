;; (ql:quickload :split-sequence)
(ql:quickload :cl-ppcre)

(defvar *index-hash* (make-hash-table :test 'equal))
(defvar *pictures-table* (make-hash-table :test 'equal))
(defvar *pair-index* (make-hash-table :test #'equal))

(defun reset-tables ()
  (setq *index-hash* (make-hash-table :test 'equal))
  (setq *pictures-table* (make-hash-table :test 'equal))
  (setq *pair-index* (make-hash-table :test 'equal))  
  nil)

(defun print-table (table)
  (let ((result nil))
    (maphash #'(lambda (key value)
                 (push (list :key key
                             :val value)
                       result))
             table)
    result))

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


(defvar *binomial-cache* (make-hash-table :test #'equal))

(defmacro clear-cache ()
  (setq *binomial-cache* (make-hash-table :test #'equal)))

(defun binomial-exp (k &key (start 1) (end k))
  (let ((result nil))
    (labels ((rec (i)
               ;; (print end)
               (setq result (append (combinations k i) result))
               (if (not (equal i end))
                   (rec (1+ i)))))
      (when (and (> k 0) (> start 0) (> end 0) (> end start)) ;; valid arguments
        (let* ((key (list k start end))
               (val (gethash key *binomial-cache*)))
          (if (null val)
              (progn
                (rec start)
                (setf (gethash key *binomial-cache*) result)
                result)
              val))))))

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

(defmacro list-inverse-pos (lst pos)
  `(let ((pos ,pos))
     (loop
        for elem being the elements of ,lst
        for i from 0 upto (1- (length ,lst))
        if (or (null pos)
               (/= i (first pos))) collect elem
        else do (pop pos))))

(defmacro list-pos (lst pos)
  `(let ((pos ,pos))
     (loop
        for elem being the elements of ,lst
        for i from 0 upto (1- (length ,lst))
        when (and pos (= i (first pos)))
        do (pop pos)
        and collect elem)))

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

(defun add-partial-word (word pos)
  (let* ((pos-chars (string-to-chars word pos))
         (inverse-chars (string-inverse-pos word pos))
         (first-entry (first (getf (gethash pos-chars *index-hash*) :partial))))
    (unless (and (equal (getf first-entry :word) word)
                 (equal (getf first-entry :inverse-chars) inverse-chars))
      (push (list :word word
                  :score (calc-score inverse-chars)
                  :inverse-chars inverse-chars
                  :pos pos)
            (getf (gethash pos-chars *index-hash*) :partial)))))

(defun add-full-word (word)
  (let ((index-key (string-to-chars word)))
    (unless (getf (gethash index-key *index-hash*) :full)
      (push (list :word word
                  :score 0)
            (getf (gethash index-key *index-hash*) :full)))))

(defun setup-index (filename)
  (with-open-file (stream filename)
    (let () 
      (do ((line (read-line stream nil)
                 (read-line stream nil)))
          ((null line))
        (let ((picture-words (cl-ppcre:split "\\s+" line)))
          (mapcar #'(lambda (word)
                      (let* ((word-len (length word))
                             (pos-lst (binomial-exp word-len :start 1 :end (1- word-len))))
                        (push (first picture-words) (gethash word *pictures-table*))
                        (mapcar #'(lambda (pos)
                                    (add-partial-word word pos))
                                pos-lst) ;; excluding combination with all chars
                        (add-full-word word)))
                  (cdr picture-words))))
      ;; sort each entry of *index-hash*
      (maphash #'(lambda (key value)
                   (setf (getf (gethash key *index-hash*) :partial)
                         (quicksort-fn (getf value :partial) :fn #'(lambda (x) (getf x :score)))))
               *index-hash*)

      ;; setting up *pair-index*
      (maphash #'(lambda (key value)
                   (when (getf value :full)
                     (if (getf value :partial) (print (coerce key 'string)))
                     (mapc #'(lambda (components)
                               (let* ((chars (getf components :inverse-chars))
                                      (chars-len (length chars)))
                                 (if (getf (gethash chars *pair-index*) :partial-pair)
                                     (remf (gethash chars *pair-index*) :partial-pair))
                                 (push (list :first (getf components :word)
                                             :second (coerce key 'string)
                                             ;; (getf (first (getf value :full)) :word)
                                             )
                                       (getf (gethash chars *pair-index*) :full-pair))
                                 (mapc #'(lambda (pos)
                                           (let ((partial-chars (list-pos chars pos))
                                                 (distance-chars (list-inverse-pos chars pos)))
                                             (unless (or (getf (gethash partial-chars *index-hash*) :full)
                                                         (getf (gethash partial-chars *pair-index*) :full-pair))
                                                 (push (list :pair-key chars
                                                             :distance distance-chars
                                                             :score (calc-score distance-chars))
                                                       (getf (gethash partial-chars *pair-index*) :partial-pair)))))
                                       (binomial-exp chars-len :start 1 :end (1- chars-len)))))
                           (getf value :partial))))
               *index-hash*)

      ;; sort each entry of *pair-index*
      (maphash #'(lambda (key value)
                   (setf (getf (gethash key *pair-index*) :partial-pair)
                         (quicksort-fn (getf value :partial-pair) :fn #'(lambda (x) (getf x :score)))))
               *pair-index*)

      
      (hash-table-count *index-hash*))))

;; try multiple values

(defun search-index (word)
  (let* ((word (string-downcase word))
         (word-len (length word))
         ;; (total-score (calc-score (string-to-chars word)))
         (search-table (make-hash-table :test #'equal))
         (match (getf (gethash (string-to-chars word) *index-hash*) :full))
         (result nil))
    
    (cond ((null match)
           (mapcar #'(lambda (pos-lst)
                      (let ((search-chars (string-to-chars word pos-lst)))
                        (when (gethash search-chars *index-hash*)
                          (let ((inverse-chars (string-inverse-pos word pos-lst))
                                (full-match    (getf (gethash search-chars *index-hash*) :full)))
                            (setf (gethash search-chars search-table)
                                  (list :query (list :query-inverse-pos inverse-chars
                                                     :query-score (calc-score inverse-chars))
                                        :match (if (null full-match)
                                                   (getf (gethash search-chars *index-hash*) :partial)
                                                   full-match)))))))
                  (binomial-exp word-len :start 1 :end word-len))
        
          (labels ((pop-top-matches ()
                     (let ((best-matches nil))
                       (maphash #'(lambda (key value)
                                    (push (list :score (+ (getf (getf value :query) :query-score)
                                                          (getf (first (getf value :match)) :score))
                                                :key key
                                                :query-inverse (getf (getf value :query) :query-inverse-pos)
                                                :match (pop (getf (gethash key search-table) :match)))
                                          best-matches)
                                    (if (null (getf (gethash key search-table) :match))
                                        (remhash key search-table)))
                                search-table)
                       best-matches)))

            (let ((serach-list (quicksort-fn (pop-top-matches) :fn #'(lambda (x) (getf x :score)))))
              )
            (do ((search-list (pop-top-matches) (pop-top-matches)))
                ((= (hash-table-count search-table) 0) result)
              (return (list :first (quicksort-fn search-list :fn #'(lambda (x) (getf x :score)))
                            :second (quicksort-fn (pop-top-matches) :fn #'(lambda (x) (getf x :score)))))
            ;; (return (list :first (subseq (quicksort-fn search-list :fn #'(lambda (x) (getf x :score))) 0 5)
            ;;               :second (subseq (quicksort-fn (pop-top-matches) :fn #'(lambda (x) (getf x :score))) 0 5)))
            ;; (return search-list)
            ))
           )
          (t (getf (first match) :word)))))


(defun search-pairs (chars)
  (let ((match (gethash chars *index-hash*)))
    (if (null match)  nil
        (macrolet ((full-word-from-inverse (word)
                     `(getf (gethash (getf ,word :inverse-chars) *index-hash*) :full)))
          (loop
             for word being the elements of (getf match :partial)
             if (full-word-from-inverse word) do
               (return (list :first (getf word :word)
                             :second (first (full-word-from-inverse word))))
             finally (return nil)))
        )
    )
  )