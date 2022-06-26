(defpackage markov-chain-namegen
  (:import-from :metatilities :spy)
  ; (:import-from :cl-ppcre :split)
  (:use :cl :iterate))
; (ql:quickload ')

; (ql:quickload 'metatilities)

(in-package :markov-chain-namegen)

(defvar file-path #P"/mnt/Data/code/common-lisp/markov-chain-namegen/src/resources/names.txt")
(alexandria:read-file-into-string file-path)


(defparameter data (remove-if-not (lambda (x) (< 2 (length x)))  (remove-duplicates (uiop:read-file-lines "/mnt/Data/code/common-lisp/markov-chain-namegen/src/resources/names.txt"))))



(defclass sylable ()
 ((sylable
   :initarg :sylable
   :accessor sylable)
  (neighbors-frequency
   :initform '()
   :accessor neighbors-frequency)))


(defun occurrences (lst)
  (let ((table (make-hash-table)))                   ; [1]
    (iter (for sylable in lst)
     (incf (gethash sylable table 0)))
    table))


(defun get-sylables (sylable-length word)
  (iter (for idx index-of-string word)
    (with max = (length word))
    (unless (<= max ( + (- sylable-length  1 ) idx))
      (collect (map 'string (lambda (x) (char word (+ x  idx))) (alexandria:iota sylable-length))))))




(defun occurrences-in-word (sylable word)
 (cl-ppcre:count-matches sylable word))




(defun get-letter-occurances (sylable text)
 (iter (for  word in text)
   (let ((letters (iter (for c in (cl-ppcre:split sylable word))
                     (unless ( = 0 (length c))
                       (collect (schar c 0)))))
         (splitted? (= (length (first  sylables)) (length word))))
    (appending
     (unless  splitted?
       (iter (for letter in letters)
         (collect letter)))))))


(defun remove-empty-strings (s-list)
 (iter (for s in  s-list)
     (unless (string= s "")
       (collect s)
       (collect (map 'string (lambda (x) (char word (+ x  idx))) (alexandria:iota sylable-length))))))



(defun save-occurancies-matrix (text)
 (let ((sylables (remove-duplicates (mapcan  (lambda (word) (get-sylables  3 word))  text))))
   (with-open-file (stream "markov-matrix.txt" :direction :output :if-does-not-exist :create :if-exists :rename)
     (iter (for sylable in sylables)
      (format stream  "~a ~a" sylable #\Newline)))))

(save-occurancies-matrix data)


