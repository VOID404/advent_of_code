(defpackage advent-of-lisp/tests/day01
  (:use :cl :day01 :rove))
(in-package :advent-of-lisp/tests/day01)

;; NOTE: To run this test file, execute `(asdf:test-system :advent-of-lisp)' in your Lisp.

(deftest basic
  (testing "should work for sample"
    (let ((res (with-input-from-string (is sample)
                 (day01::basic is))))
      (ok (= 24000 res))))
  (testing "should work for real"
    (let ((res (with-open-file (is "../inputs/day01.txt")
                 (day01::basic is))))
      (ok (= 71780 res)))))

(deftest bonus
  (testing "should work for bonus"
    (let ((res (with-input-from-string (is sample)
                 (day01::bonus is))))
      (ok (= 45000 res))))
  (testing "should work for real"
    (let ((res (with-open-file (is "../inputs/day01.txt")
                 (day01::bonus is))))
      (ok (= 212489 res)))))
