(defpackage day14
  (:use :cl :iterate)
  (:export))

(in-package :day14)
(declaim (optimize (safety 3)))

(defvar *max-y* 0)

(defparameter *sample* "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9")

(defstruct point
  (x 0 :type fixnum)
  (y 0 :type fixnum))

(defstruct bounds
  (x-min nil :type fixnum)
  (x-max nil :type fixnum)
  (y-min nil :type fixnum)
  (y-max nil :type fixnum))

(defun bounds-merge (a b)
  "Returns new bounds, that fit both arguments"
  (declare (type bounds a b))
  (let ((x-max (max (bounds-x-max a)
                    (bounds-x-max b)))
        (x-min (min (bounds-x-min a)
                    (bounds-x-min b)))
        (y-max (max (bounds-y-max a)
                    (bounds-y-max b)))
        (y-min (min (bounds-y-min a)
                    (bounds-y-min b))))
    (make-bounds :x-min x-min :x-max x-max
                 :y-min y-min :y-max y-max)))
(defun bounds-width (bound)
  (declare (type bounds bound))
  (the fixnum (- (bounds-x-max bound)
                 (bounds-x-min bound)
                 -1)))

(defun bounds-height (bound)
  (declare (type bounds bound))
  (the fixnum (- (bounds-y-max bound)
                 (bounds-y-min bound)
                 -1)))

(defun read-points (&optional (stream *standard-input*))
  "Returns list of points and bounds"
  (let ((*readtable* (copy-readtable)))
    (set-macro-character #\, (lambda (stream char)
                               (declare (ignore stream char))
                               (values)))
    (iter
      (for s = (read stream nil :eof))
      (if (or (eq s '->)
              (eq s :eof))
          (let* ((x (car buf))
                 (y (cadr buf))
                 (point (make-point :x x :y y)))
            (collect point into points)
            (maximizing x into max-x)
            (maximizing y into max-y)
            (minimizing x into min-x)
            (minimizing y into min-y)
            (setf buf nil))
          (collect s into buf))
      (when (eq s :eof)
        (return (values points
                        (make-bounds :x-min min-x :x-max max-x
                                     :y-min min-y :y-max max-y)))))))

(defun read-obstacles (&optional (stream *standard-input*))
  "Returns list of lists of points and bounds"
  (iter
    (with bounds = 0)
    (for l = (read-line stream nil :eof))
    (until (eq l :eof))
    (unless (string= l "")
      (multiple-value-bind (p b) (with-input-from-string (s l) (read-points s))
        (collect p into points)
        (if (first-time-p)
            (setf bounds b)
            (setf bounds (bounds-merge bounds b)))))
    (finally (return (values points bounds)))))

(defun obstacle->lines (obstacle)
  (iter
    (for a in obstacle)
    (for b in (cdr obstacle))
    (collecting (cons a b))))

(defun point-distance (a b)
  (+ (abs (- (point-x a) (point-x b)))
     (abs (- (point-y a) (point-y b)))))

(defun obstacle-collides (p obstacles)
  "Check if point collides with any obstacle"
  (iter
    (for o in obstacles)
    (thereis
     (iter
       (for l in (obstacle->lines o))
       (for a = (car l))
       (for b = (cdr l))
       (for dist0 = (point-distance a b))
       (for dist1 = (point-distance p a))
       (for dist2 = (point-distance p b))
       (thereis (= dist0 (+ dist1 dist2)))))))

(defun obstacles->map (obstacles bounds)
  (declare (type bounds bounds))
  (declare (type list obstacles))
  (let* ((min-x (bounds-x-min bounds))
         (width (bounds-width bounds))
         (height (1+ (bounds-y-max bounds)))
         (len (* height width))
         (map (make-array len ;:element-type 'boolean
                              :initial-element 0)))
    (iter
      (for i index-of-vector map)
      (for x = (+ min-x (mod i width)))
      (for y = (floor i width))
      (for p = (make-point :x x :y y))
      (when (obstacle-collides p obstacles)
        (setf (aref map i) 1)))
    (make-array (list height width)
                :element-type 'boolean
                :displaced-to map)))

(defun basic (&optional (stream *standard-input*))
  (multiple-value-bind (obstacles bounds) (read-obstacles stream)
    (obstacles->map obstacles bounds)))
