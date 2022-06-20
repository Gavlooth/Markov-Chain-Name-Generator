(defpackage markov-chain-namegen
  (:use :cl :iterate))

(in-package :markov-chain-namegen)

(defvar file-path #P"/mnt/Data/code/common-lisp/markov-chain-namegen/src/resources/names.txt")

(alexandria:read-file-into-string file-path)

'( "skata" "faf" "lo")

(defparameter data (remove-if-not (lambda (x) (< 2 (length x)))  (remove-duplicates (uiop:read-file-lines "/mnt/Data/code/common-lisp/markov-chain-namegen/src/resources/names.txt"))))

(defvar  sylables '())


(defclass sylable ()
 ((sylable
   :initarg :sylable
   :accessor sylable)
  (associated-sylables
   :initform nil
   :accessor lisper)))


(defun get-sylables (word)
  (iter (for idx index-of-string word)
    (with max = (length word))
    (unless (=< max ( + 2 idx))
      (print idx)
      (collect (map 'string (lambda (x) (char word (+ x  idx))) '( 0 1 2))))))

(get-sylables "afkslfdflkdafdkafkjafkj")

(iter (for  word in data)
   (iter (for idx index-of-string word)
         (with max = (length word))
     (when (= max ( + 2 idx))
         (terminate))
     (setq sylables (cons (map 'string (lambda (x) (char word (+ x  idx))) '( 0 1 2)) sylables))))

(setq sylables (remove-duplicates sylables))


(uiop:split-string "fdasfsdf" :separator "d")

(defun remove-empty-strings (s-list)
 (iter (for s in  s-list)
     (unless (string= s "")
       (collect s))))

(defun get-probability (sylable)
 (iter (for  word in data)
       (with after-sylable-body = (uiop:split-string word :separator sylable))
   (unless (string= word (car sylable)))))


(second (uiop:split-string "ii.iii":separator "."))


(with-open-file (stream "markov-matrix.txt" :direction :output :if-does-not-exist :create)
 (let (uniq-sylables (remove-duplicates sylables))

  (iter (for sylable in uniq-sylables)
   (format stream  "~a " sylable))

  (iter (for sylable in uniq-sylables)
   (format stream  "~a ~a" sylable #\Newline))))

;(open "/some/file/name.txt" :direction :output :if-exists :supersede)
;(asdf:load-system "markov-chain-namegen")
