(defpackage day12
  (:use :cl :iterate)
  (:export))

(in-package :day12)
(declaim (optimize (safety 3))) ;; (debug 0) (speed 3)))

(defparameter sample "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi")

(defvar *width*)
(defvar *height*)
(defvar *map*)
(defvar *raw-map*)
(defvar *start*)
(defvar *as*)
(defvar *end*)

(defun seq->vec (p &optional (width *width*))
  (cons (mod p width)
        (floor p width)))

(defun show-state ()
  (format t "~ax~a~%~a~%~a -> ~a~%"
          *width* *height*
          *map* *start* *end*))

(defun parse-str (str)
  (declare (type string str))
  (let* ((width (position #\newline str))
         (height (1+ (count #\newline (string-trim '(#\newline) str))))
         (nums (map 'vector (lambda (char) (- (char-code char) (char-code #\a)))
                    (remove #\newline str)))
         (start (position -14 nums))
         (end (position -28 nums)))
    (setf (aref nums start) 0)
    (setf (aref nums end) (- (char-code #\z)
                             (char-code #\a)))
    (setf *width* width)
    (setf *height* height)
    (setf *raw-map* nums)
    (setf *map* (make-array (list height width)
                            :displaced-to nums))
    (setf *start* (seq->vec start))
    (setf *end* (seq->vec end))))

(defun clamp (num low up)
  (min up (max low num)))

(defun between (num low up)
  (and (> up num)
       (<= low num)))

(defun move (point dir)
  (let ((x (car point))
        (y (cdr point)))
    (cond ((eql :up dir) (cons x (1+ y)))
          ((eql :down dir) (cons x (1- y)))
          ((eql :left dir) (cons (1- x) y))
          ((eql :right dir) (cons (1+ x) y)))))

(defun in-range (point)
  "Check if point is not out of bounds"
  (let ((x (car point))
        (y (cdr point)))
    (and (between x 0 *width*)
         (between y 0 *height*))))

(defun neighbours (point)
  (iter
    (for d in '(:left :right :up :down))
    (for p = (move point d))
    (if (and (not (equal point p))
             (in-range p))
        (collect p))))

(defmacro at (point &optional (map '*map*))
  `(aref ,map (cdr ,point) (car ,point)))

(defun dijkstra (start)
  (let ((distances (make-hash-table :test #'equal))
        (come-from (make-hash-table :test #'equal)))
    (setf (gethash start distances) 0)
    (setf (gethash start come-from) nil)
    (iter
      (with open = (list start))
      (until (null open))
      (for p = (pop open))
      (iter
        (with dist0 = (gethash p distances))
        (with val0 = (at p))

        (for n in (neighbours p))
        (for dist = (gethash n distances))
        (for val = (at n))

        (when (and (>= (- val val0) -1)
                   (or (not dist)
                       (< (1+ dist0) dist)))
          (setf (gethash n distances) (1+ dist0))
          (setf (gethash n come-from) p)
          (collecting n into to-open))
        (finally
         (setf open (nconc open to-open)))))
    (values come-from distances)))

(defun positions (item sequence &key (test #'eql))
  (iterate
    (for i :in-vector sequence)
    (for pos :from 0)
    (if (funcall test i item)
        (collect pos))))

(defun basic ()
  (multiple-value-bind (come-from distance) (dijkstra *end*)
    (gethash *start* distance)))

(defun bonus ()
  (iter
    (with starts = (mapcar #'seq->vec (positions 0 *raw-map*)))
    (with distances = (nth-value 1 (dijkstra *end*)))
    (for pos in starts)
    (for dist = (gethash pos distances))
    (when dist
      (minimizing dist))))

(defun draw-path (points map)
  (iter
    (for p in points)
    (setf (at p map) #\#)
    (finally (return map))))

(defun s-to-e (map)
  (parse-str map)
  (loop
    :with prev = (dijkstra *end*)
    :with m = (make-array (list *height* *width*)
                          :element-type 'character
                          :displaced-to (remove #\newline map))
    :with p = *start*
    :for pr = (gethash p prev)
    :while pr
    :collect p :into points
    :do (setf p pr)
    :finally (let ((pretty-map (draw-path points m)))
               (dotimes (y *height*)
                 (dotimes (x *width*)
                   (format t "~a" (aref pretty-map y x)))
                 (terpri)))))
