(in-package :ita-rebus)

;; index tables

;; picture-word look up table
(defvar *pictures-table*      (make-hash-table :test 'equal))

;; word-index table
(defvar *word-index*          (make-hash-table :test 'equal))

;; word 
(defvar *subtraction-pattern* (make-hash-table :test #'equal))


(defvar *pattern-table* (make-hash-table :test 'equal))
(defvar *pair-index*    (make-hash-table :test #'equal))
