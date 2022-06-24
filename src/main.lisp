(defpackage markov-chain-namegen
  (:import-from :metatilities :spy)
  (:use :cl :iterate))
; (ql:quickload 'cl-ppcre)

; (ql:quickload 'metatilities)

(in-package :markov-chain-namegen)

(defvar file-path #P"/mnt/Data/code/common-lisp/markov-chain-namegen/src/resources/names.txt")
(alexandria:read-file-into-string file-path)


(defparameter data (remove-if-not (lambda (x) (< 2 (length x)))  (remove-duplicates (uiop:read-file-lines "/mnt/Data/code/common-lisp/markov-chain-namegen/src/resources/names.txt"))))

(defvar  sylables '())


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
  (let ((splitted-text  (uiop:split-string word :separator sylable)))
   (print splitted-text)
   (length
    (iter (for  s in splitted-text)
      (if (string= sylable  s)
        (collect s))))))


'(defun get-probability (sylable text)
  (iter (for  word in text)
        (with (first-sylable (letter &letters)  & sylables ) = (uiop:split-string word :separator sylable))
    (unless (string= word (car sylable))
      (print first-sylable)
      (print letter))))


; (cl-ppcre:split "as" "fdasfsdf")

; (length "fdasfsdf")

(defun get-occurances (sylable text)
 (iter (for  word in text)
   (let ((letters (iter (for c in (cl-ppcre:split sylable word))
                     (unless ( = 0 (length c))
                       (collect (schar c 0)))))
         (splitted? (= (length (first  sylables)) (length word))))
    (appending
     (unless  splitted?
       (iter (for letter in letters)
         (collect letter)))))))



(iter (for  word in data)
   (iter (for idx index-of-string word)
         (with max = (length word))
     (when (= max ( + 2 idx))
         (terminate))
     (setq sylables (cons (map 'string (lambda (x) (char word (+ x  idx))) '( 0 1 2)) sylables))))

(setq sylables (remove-duplicates sylables))



(defun remove-empty-strings (s-list)
 (iter (for s in  s-list)
     (unless (string= s "")
       (collect s))))




(with-open-file (stream "markov-matrix.txt" :direction :output :if-does-not-exist :create)
 (let (uniq-sylables (remove-duplicates sylables))

  (iter (for sylable in uniq-sylables)
   (format stream  "~a " sylable))

  (iter (for sylable in uniq-sylables)
   (format stream  "~a ~a" sylable #\Newline))))


(second sylables)
(iter (for (k v) in-hashtable (occurrences sylables))
  (unless (= 1 v)
   (format t "key: ~a, val:~a ~a" k v #\Newline)))

;(open "/some/file/name.txt" :direction :output :if-exists :supersede)
;(asdf:load-system "markov-chain-namegen")
