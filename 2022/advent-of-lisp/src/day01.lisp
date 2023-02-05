(defpackage day01
  (:use :cl)
  (:export sample basic bonus))

(in-package :day01)

(defparameter sample "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000")

(defun basic (&optional (is *standard-input*))
  "Sum elves separated by empty lines, returning the maximized sum"
  (loop
    :with sum = 0
    :for line = (read-line is nil "")
    :until (and (zerop sum)
                (equal line ""))

    :if (equal line "")
      :maximize sum
      :and :do (setf sum 0)
    :else
      :do (incf sum (parse-integer line))))

(defun bonus (&optional (is *standard-input*))
  "Sum elves, returning sum of top 3"
  (loop
    :with sum = 0
    :for line = (read-line is nil "")
    :until (and (zerop sum)
                (equal line ""))

    :if (equal line "")
      :collect sum :into elves
      :and :do (setf sum 0)
    :else
      :do (incf sum (parse-integer line))
    :finally (return
               (loop :with 3sorted = (subseq (sort elves #'>) 0 3)
                     :for x :in 3sorted
                     :sum x))))
