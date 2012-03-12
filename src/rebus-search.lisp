(in-package :ita-rebus)

(defun search-rebus-pairs (pair1 pair2)
  (macrolet ((get-full-pair (partial-pair)
               `(first (getf (gethash (getf ,partial-pair :full-pair) *pair-index*) :full-pair))))
    (let ((pair1-entry (gethash pair1 *pair-index*)))
      (when pair1-entry
        (let ((second-pairs (gethash pair2 (getf pair1-entry :second-pair-candidates))))
          (when second-pairs
            (loop for sp-candidate in second-pairs do
                 (let* ((sp-pattern (getf sp-candidate :diff-pattern))
                        (two-pairs
                         (loop for pair in (getf pair1-entry :partial-pair) do
                              (if (equal (getf pair :diff-pattern) sp-pattern)
                                  (return (list (get-full-pair pair)
                                                (get-full-pair sp-candidate)))))))
                   (if two-pairs
                       (return two-pairs))))))))))