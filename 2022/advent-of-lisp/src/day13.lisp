(defpackage day13
  (:use :cl :iterate)
  (:export))

(in-package :day13)
(declaim (optimize (safety 3))) ;; (debug 0) (speed 3)))

(defun read-list (&optional (stream *standard-input*) (eof-error-p t)
                    (eof-value nil) (recursive-p nil))
  (let ((*readtable* (copy-readtable)))
    (set-macro-character #\[ (lambda (stream char)
                               (declare (ignore char))
                               (read-delimited-list #\] stream)))
    (set-macro-character #\, (lambda (stream char)
                               (declare (ignore stream char))
                               (values)))
    (set-macro-character #\] (get-macro-character #\)))
    (read stream eof-error-p eof-value recursive-p)))

(defun compare-ints (a b)
  (declare (type integer a b))
  (cond
    ((< a b) :good)
    ((> a b) :bad)
    ((= a b) :neutral)))

(defun compare-lists (as bs)
  (declare (type list as bs))
  (iter
    (for a in as)
    (for b in bs)
    (for cmp = (compare a b))
    (cond
      ((eql cmp :good) (return :good))
      ((eql cmp :bad) (return :bad)))
    (finally (let ((la (length as))
                   (lb (length bs)))
               (return (cond
                         ((< la lb) :good)
                         ((> la lb) :bad)
                         ((= la lb) :neutral)))))))

(defun is-list (a)
  (typecase a
    (list a)
    (t (list a))))

(defun compare (a b)
  (cond
    ((and (integerp a)
          (integerp b))
     (compare-ints a b))
    ((and (listp a)
          (listp b))
     (compare-lists a b))
    (t (compare-lists (is-list a) (is-list b)))))

(defun basic (&optional (stream *standard-input*))
  (iter
    (for a = (read-list stream nil :eof))
    (for b = (read-list stream nil :eof))
    (for i from 1)
    (until (or (eql a :eof)
               (eql b :eof)))

    (for cmp = (compare a b))
    (when (eql cmp :good)
      (summing i))))

(defun packet-sort (a b)
  (the boolean (eq :good (compare a b))))

(defun bonus (&optional (stream *standard-input*))
  (iter
    (for packet = (read-list stream nil :eof))
    (until (eql packet :eof))
    (collecting packet into packets)
    (finally
     (let* ((sorted (sort (nconc packets (list '((2)) '((6))))
                         #'packet-sort))
            (p1 (1+ (position '((2)) sorted :test #'equal)))
            (p2 (1+ (position '((6)) sorted :test #'equal))))
       (return (* p1 p2))))))


(defparameter *sample* "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]")
