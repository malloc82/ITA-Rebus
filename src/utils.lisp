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

(defun mk-combinations-fn ()
  (let ((combination-cache      (make-hash-table :test #'equal))
        (combination-slot-cache (make-hash-table :test #'equal)))
    #'(lambda (s k &key (slot nil))
        (macrolet ((cached-rec (s k)
                     `(or (gethash (list ,s ,k) combination-cache)
                          (rec ,s ,k)))
                   (cached-rec- (s k)
                     `(or (gethash (list ,s ,k) combination-slot-cache)
                          (rec- ,s ,k))))
          (labels ((rec (s k)
                     (let ((s-1 (1- s)))
                       (cond ((= s k)
                              (setf (gethash (list s k) combination-cache)
                                    (list (loop for i from s-1 downto 0 collect i))))
                             ((= k 1)
                              (setf (gethash (list s k) combination-cache)
                                    (loop for i from s-1 downto 0 collect (list i))))
                             (t (setf (gethash (list s k) combination-cache)
                                      (append
                                       (mapcar #'(lambda (index)
                                                   (append (list s-1) index))
                                               (cached-rec s-1 (1- k)))
                                       (cached-rec s-1 k)))))))
                   (rec- (s k)
                     (let ((s-1 (1- s)))
                       (cond ((= s k)
                              (setf (gethash (list s k) combination-slot-cache)
                                    (list (loop for i from s-1 downto 0 collect i))))
                             ((= k 1)
                              (setf (gethash (list s k) combination-slot-cache)
                                    (loop for i from s-1 downto 0 collect (if (= i 0)
                                                                              (list i)
                                                                              (list i '-)))))
                             (t (setf (gethash (list s k) combination-slot-cache)
                                      (append
                                       (mapcar #'(lambda (index)
                                                   (if (or (equal (1- s-1) (first index))
                                                           (equal (first index) '-))
                                                       (append (list s-1) index)
                                                       (append (list s-1) '(-) index)))
                                               (cached-rec- s-1 (1- k)))
                                       ;; (cached-rec- s-1 k)
                                       (mapcar #'(lambda (index-lst)
                                                   (if (equal (first index-lst) '-)
                                                       index-lst
                                                       (append '(-) index-lst)))
                                               (cached-rec- s-1 k)))))))))
            (if (null slot)
                (mapcar #'reverse (cached-rec  s k))
                (mapcar #'reverse (cached-rec- s k))))))))

(defvar combinations (mk-combinations-fn))


(defun binomial-exp (k &key (start 1) (end k) (slot nil))
  (let ((result nil))
    (labels ((rec (i)
               (setq result (append (funcall combinations k i :slot slot) result))
               (if (not (equal i start))
                   (rec (1- i)))))
      (when (and (> k 0) (> start 0) (> end 0) (>= end start)) ;; valid arguments
        (rec end)))
    result))
(defvar cached-binomial-exp (memoize #'binomial-exp))

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
(defvar cached-end-index (memoize #'end-index))

(defun calc-score (str)
  (let ((score 0))
    (loop
       :for char being the elements of str
       :do (if (not (equal char #\-))
               (if (find char "aeiou" :test 'equal)
                   (setq score (1+ score))
                   (setq score (+ score 5)))))
    score))

(defun string-to-chars (str &optional pos-lst)
  (if (null pos-lst)
      (let ((str-len-1 (1- (length str))))
        (loop for i from 0 to str-len-1 collect (schar str i)))
      (loop for i in pos-lst
           if (numberp i) collect (schar str i)
           else collect #\-)))

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
