(in-package :ita-rebus)

;; index tables 
(defvar *pictures-table*      (make-hash-table :test 'equal))
(defvar *subtraction-pattern* (make-hash-table :test #'equal))
(defvar *index-hash*          (make-hash-table :test 'equal))

(defvar *pattern-table* (make-hash-table :test 'equal))
(defvar *pair-index*    (make-hash-table :test #'equal))
