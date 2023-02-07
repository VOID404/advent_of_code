(defpackage day14
  (:use :cl :iterate)
  (:export))

(in-package :day14)
(declaim (optimize (safety 3)))

(defvar *max-y* 0)

(defstruct point
  (x 0 :type integer)
  (y 0 :type integer))

(defstruct bottom
  (a nil :type point)
  (b nil :type point))

(defun point= (a b)
  (declare (type point a b))
  (and (= (point-x a)
          (point-x b))
       (= (point-y a)
          (point-y b))))

(defun point-add (a b)
  (declare (type point a b))
  (make-point :x (+ (point-x a)
                    (point-x b))
              :y (+ (point-y a)
                    (point-y b))))

;; https://stackoverflow.com/a/11908158
(defun on-line-p (p a b &optional infinite)
  "Check if p is on line between a and b"
  (declare (type point p a b))
  (let* ((ax (point-x a))
         (ay (point-y a))
         (bx (point-x b))
         (by (point-y b))
         (x (point-x p))
         (y (point-y p))
         (dxc (- x ax))
         (dyc (- y ay))
         (dxl (- bx ax))
         (dyl (- by ay))
         (cross (- (* dxc dyl) (* dyc dxl))))
    (when (= cross 0)
      (or infinite
        (if (>= (abs dxl) (abs dyl))
            (if (< 0 dxl)
                (and (<= ax x)
                     (<= x bx))
                (and (<= bx x)
                     (<= x ax)))
            (if (< 0 dyl)
                (and (<= ay y)
                     (<= y by))
                (and (<= by y)
                     (<= y ay))))))))

(defun poly-collides (pnt arr)
  "Takes a point and list of points, check if point lies on any line"
  (declare (type point pnt))
  (declare (type list arr))
  (iter
    (for a in arr)
    (for b in (cdr arr))
    (thereis (on-line-p pnt a b))))

(defun collides (pnt obstacles)
  "Given a point and list of poly lines, returns true if point on any line"
  (iter
    (for o in obstacles)
    (finding o such-that (typecase o
                           (list (poly-collides pnt o))
                           (point (point= pnt o))
                           (bottom (on-line-p pnt (bottom-a o) (bottom-b o) t))))))

(defparameter +down+ (make-point :y 1))
(defparameter +up+ (make-point :y -1))
(defparameter +left+ (make-point :x -1))
(defparameter +right+ (make-point :x 1))
(defparameter +generator+ (make-point :x 500))

(defparameter *obstacles* (list ;; 498,4 -> 498,6 -> 496,6
                           (list (make-point :x 498 :y 4)
                                 (make-point :x 498 :y 6)
                                 (make-point :x 496 :y 6))
                           (list (make-point :x 503 :y 4)
                                 (make-point :x 502 :y 4)
                                 (make-point :x 502 :y 9)
                                 (make-point :x 494 :y 9))))


;; 12 x 23)
(defun render (obstacles height width)
  (flet ((to-char (o) (etypecase o
                        (point #\o)
                        (list #\#))))
    (iter
      (with canvas = (make-array (list height width)
                                 :initial-element #\.))
      (for i from 0 to (* height width))
      (for x = (mod i width))
      (for y = (floor i width))
      (for c = (collides (make-point :x (+ 490 x)
                                     :y y)
                         obstacles))
      (if c
          (setf (aref canvas y x) (to-char c)))
      (finally (dotimes (y height)
                 (dotimes (x width)
                   (princ (aref canvas y x)))
                 (fresh-line))))))

(defun drop (point obstacles &optional (max-y *max-y*))
  (if (collides point obstacles)
      nil
      (iter
        (for p initially point then next)
        (for next = (point-add p +down+))

        (until (or (= max-y (point-y p))
                   (and (point= p point)
                        (not (first-iteration-p)))))

        (when (collides next obstacles)
          (let ((left (point-add next +left+))
                (right (point-add next +right+)))
            (cond ((not (collides left obstacles)) (setf next left))
                  ((not (collides right obstacles)) (setf next right))
                  (t (return p))))))))

(defun simulate (obstacles)
  (iter
    (for i from 0)
    (for d = (drop +generator+ obstacles))
    (while d)
    (push d obstacles)
    (counting d)))

(defun read-points (&optional (stream *standard-input*))
  (let ((*readtable* (copy-readtable)))
    (set-macro-character #\, (lambda (stream char)
                               (declare (ignore stream char))
                               (values)))
    (iter
      (for s = (read stream nil :eof))
      (cond
        ((eq s '->)
         (collect (make-point :x (car buf)
                              :y (cadr buf)) into out)
         (maximizing (cadr buf) into maxy)
         (setf buf nil))
        ((eq s :eof)
         (collect (make-point :x (car buf)
                              :y (cadr buf)) into out)
         (maximizing (cadr buf) into maxy)
         (finish))
        (t (collect s into buf)))
      (finally (return (values out maxy))))))


(defparameter *sample* "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9")

(defun read-obstacles (&optional (stream *standard-input*))
  (iter
    (for l = (read-line stream nil :eof))
    (until (eq l :eof))
    (unless (string= l "")
      (multiple-value-bind (points max-y) (with-input-from-string (s l) (read-points s))
        (collect points into pnts)
        (maximize max-y into maxy)))
    (finally (return (values pnts maxy)))))

(defun basic (&optional (stream *standard-input*))
  (multiple-value-bind (obstacles *max-y*) (read-obstacles stream)
    (incf *max-y* 2)
    (simulate obstacles)))

(defun bonus (&optional (stream *standard-input*))
  (multiple-value-bind (obstacles *max-y*) (read-obstacles stream)
    (incf *max-y* 2)
    (push (make-bottom :a (make-point :x 0 :y *max-y*)
                       :b (make-point :x 1 :y *max-y*))
          obstacles)
    (simulate obstacles)))
