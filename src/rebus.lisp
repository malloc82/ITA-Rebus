(in-package :ita-rebus)

(defun add-partials (word)
  (let* ((word-len (length word))
         (pos-lst (funcall cached-binomial-exp
                           word-len :start 1 :end (1- word-len))))
    (mapc #'(lambda (pos)
              (let* ((pos-chars (string-to-chars word pos))
                     (subtraction (string-inverse-pos word pos))
                     (partial-word (list :word word
                                         :score (calc-score subtraction)
                                         :subtraction subtraction
                                         :key-index pos)))
                (symbol-macrolet ((index-entry       (getf (gethash pos-chars *index-hash*) :partial))
                                  (sub-pattern-entry (gethash subtraction *subtraction-pattern*)))
                  (unless (and (equal (getf (first index-entry) :word) word)
                               (equal (getf (first index-entry) :subtraction) subtraction))
                    (push (cons (coerce pos-chars 'string) word) (getf sub-pattern-entry :common-parts))
                    (incf (getf sub-pattern-entry :count 0))
                    (push partial-word index-entry)))))
          pos-lst)))

(defun add-full-word (word)
  (let ((index-key (string-to-chars word)))
    (unless (getf (gethash index-key *index-hash*) :full)
      (setf (getf (gethash index-key *index-hash*) :full) word))))

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
                                          :score (calc-score diff-pattern))))
                (symbol-macrolet ((partial-pair-entry
                                   (getf (gethash partial-match *pair-index*) :partial-pair)))
                  (unless (and (equal (getf (first partial-pair-entry) :full-pair) full-pair)
                               (equal (getf (first partial-pair-entry) :diff-pattern) diff-pattern))
                    (push partial-pair partial-pair-entry)
                    (push (list :full-pair full-pair :partial-match partial-match) ;; pattern table
                          (gethash diff-pattern *pattern-table*))))))
          (funcall cached-binomial-exp len :start 1 :end (1- len) :slot t))))


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
                     (quicksort-fn (getf value :partial) :fn #'(lambda (x) (getf x :score)))))
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

;; (defun rebus-search (word)
;;   (let* ((word-len (length word))
;;          (query-word (string-to-chars (string-downcase word)))
;;          (result (list :pic-word nil :add nil :remove nil :score nil)))
;;     (if (gethash word *pictures-table*)
;;         (concatenate 'string ":" word)
;;         (loop for pos in (funcall cached-binomial-exp word-len :start 1 :end (1- word-len)) do
;;              (let ((common (string-to-chars word pos))
;;                    (add    (string-inverse-pos word pos)))
;;                ())))
;;     )
;;   )