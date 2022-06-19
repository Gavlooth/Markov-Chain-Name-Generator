(defpackage markov-chain-namegen/tests/main
  (:use :cl
        :markov-chain-namegen
        :rove))
(in-package :markov-chain-namegen/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :markov-chain-namegen)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
