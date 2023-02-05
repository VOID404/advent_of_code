(defpackage advent-of-lisp/tests/main
  (:use :cl
        :advent-of-lisp
        :rove))
(in-package :advent-of-lisp/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :advent-of-lisp)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
