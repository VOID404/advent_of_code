(defpackage advent-of-lisp/tests/day11
  (:use :cl :day11 :rove))
(in-package :advent-of-lisp/tests/day11)

;; NOTE: To run this test file, execute `(asdf:test-system :advent-of-lisp)' in your Lisp.
(defparameter monkey-str "Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0
")


(deftest parsing
  (testing "should parse a monkey"
           (let ((res (with-input-from-string (is)))))))

;; (deftest basic
;;   (testing "should work for sample"
;;     (let ((res (with-input-from-string (is sample)
;;                  (day01::basic is))))
;;       (ok (= 24000 res))))
;;   (testing "should work for real"
;;     (let ((res (with-open-file (is "../inputs/day01.txt")
;;                  (day01::basic is))))
;;       (ok (= 71780 res)))))

;; (deftest bonus
;;   (testing "should work for bonus"
;;     (let ((res (with-input-from-string (is sample)
;;                  (day01::bonus is))))
;;       (ok (= 45000 res))))
;;   (testing "should work for real"
;;     (let ((res (with-open-file (is "../inputs/day01.txt")
;;                  (day01::bonus is))))
;;       (ok (= 212489 res)))))
