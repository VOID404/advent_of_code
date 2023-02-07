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

(defun point-add (a b)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type point a b))
  (let ((x1 (point-x a))
        (y1 (point-y a))
        (x2 (point-x b))
        (y2 (point-y b)))
    (make-point :x (the fixnum (+ x1 x2))
                :y (the fixnum (+ y1 y2)))))

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

(defun map-bounds (map)
  (let ((height (array-dimension map 0))
        (width (array-dimension map 1)))
    (make-bounds :x-min 0 :x-max (1- width)
                 :y-min 0 :y-max (1- height))))

(defun point-in-bounds (point bounds)
  (declare (type bounds bounds)
           (type point point))
  (let ((x (point-x point))
        (y (point-y point)))
    (and (<= x (bounds-x-max bounds))
         (>= x (bounds-x-min bounds))
         (<= y (bounds-y-max bounds))
         (>= y (bounds-y-min bounds)))))

;; ---------------------Read input-----------------------------

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
         (map (make-array len :element-type 'boolean
                              :initial-element nil)))
    (iter
      (for i index-of-vector map)
      (for x = (+ min-x (mod i width)))
      (for y = (floor i width))
      (for p = (make-point :x x :y y))
      (when (obstacle-collides p obstacles)
        (setf (aref map i) t)))
    (make-array (list height width)
                :element-type 'boolean
                :displaced-to map)))

;; ------------------------------------------------------------

(defparameter +down+ (make-point :y 1))
(defparameter +up+ (make-point :y -1))
(defparameter +left+ (make-point :x -1))
(defparameter +right+ (make-point :x 1))

(defun widen (map n)
  "Widen map by n on left and right"
  (let* ((width (+ (* 2 n)
                   (array-dimension map 1)))
         (height (array-dimension map 0))
         (new-map (make-array (list height width)
                              :element-type 'boolean
                              :initial-element nil)))
    (iter
      (for y from 0 below height)
      (iter
        (for x from 0 below width)
        (setf (aref new-map y x) (if (array-in-bounds-p map y (- x n))
                                     (aref map y (- x n))
                                     nil)))
      (finally (return new-map)))))

(defmacro widenf (map n)
  "Widen map by n on left and right"
  (let ((num (gensym)))
    `(let ((,num ,n))
      (setf ,map (widen ,map ,num)))))

(defun print-map (map)
  (dotimes (y (array-dimension map 0))
    (dotimes (x (array-dimension map 1))
      (if (aref map y x)
          (princ #\#)
          (princ #\.)))
    (fresh-line)))

(defvar *bounds*)

(defun open-space (point map &optional (bounds *bounds*))
  "Return t if space is empty"
  (let ((x (point-x point))
        (y (point-y point)))
    (not (and
          (point-in-bounds point bounds)
          (the boolean (aref map y x))))))

(defun drop (point map bounds)
  (iter
    (with *bounds* = bounds)
    (initially
     (unless (open-space point map)
       (return nil)))
    (for p initially point then next)
    (with next = nil)
    (for down = (point-add p +down+))
    (for left = (point-add down +left+))
    (for right = (point-add down +right+))
    (cond
      ((open-space down map) (setf next down))
      ((open-space left map) (setf next left))
      ((open-space right map) (setf next right))
      (t (return p)))
    (unless (point-in-bounds next *bounds*)
      (return nil))))

(defun simulate (generator map &optional (bounds (map-bounds map)))
  (iter
    (for d = (drop generator map bounds))
    (while d)

    (counting t)
    (let ((x (point-x d))
          (y (point-y d)))
      (setf (aref map y x) t))))

(defun basic (&optional (stream *standard-input*))
  (multiple-value-bind (obstacles bounds) (read-obstacles stream)
    (let* ((map (obstacles->map obstacles bounds))
           (fixed-x (- 500 (bounds-x-min bounds)))
           (generator (make-point :x fixed-x)))
      (simulate generator map))))

(defun add-floorf (map)
  (let ((width (array-dimension map 1))
        (height (array-dimension map 0)))
    (iter
      (with y = (1- height))
      (for x from 0 below width)
      (setf (aref map y x) t))))

(defun bonus (&optional (stream *standard-input*))
  (multiple-value-bind (obstacles bounds) (read-obstacles stream)
    (let* ((map (obstacles->map obstacles bounds))
           (height (array-dimension map 0))
           (width (array-dimension map 1))
           (margin 1000)
           (fixed-x (+ margin (- 500 (bounds-x-min bounds))))
           (generator (make-point :x fixed-x)))
      (adjust-array map (list (+ 2 height) width) :initial-element nil)
      (widenf map margin)
      (add-floorf map)
      (time (simulate generator map)))))
