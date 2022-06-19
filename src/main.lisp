(defpackage markov-chain-namegen
  (:use :cl :iterate))

(in-package :markov-chain-namegen)

(defvar file-path #P"/mnt/Data/code/common-lisp/markov-chain-namegen/src/resources/names.txt")

(alexandria:read-file-into-string file-path)

'( "skata" "faf" "lo")

(defparameter data (remove-if-not (lambda (x) (< 2 (length x)))  (remove-duplicates (uiop:read-file-lines "/mnt/Data/code/common-lisp/markov-chain-namegen/src/resources/names.txt"))))

(defvar  sylables '())

(iter (for  word in data)
   (iter (for idx index-of-string word)
         (with max = (length word))
     (when (= max ( + 2 idx))
         (terminate))
     (setq sylables (cons (map 'string (lambda (x) (char word (+ x  idx))) '( 0 1 2)) sylables))))

(print sylables)
