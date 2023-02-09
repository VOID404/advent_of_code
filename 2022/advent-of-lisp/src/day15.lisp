(defpackage day15
  (:use :cl :iterate :cl-ppcre)
  (:export))

(in-package :day15)
(declaim (optimize (safety 3)))

(declaim (inline make-point))
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
    (make-point :x (+ x1 x2)
                :y (+ y1 y2))))

(defun point= (a b)
  (declare (type point a b))
  (let ((x1 (point-x a))
        (y1 (point-y a))
        (x2 (point-x b))
        (y2 (point-y b)))
    (and (= x1 x2)
         (= y1 y2))))

(defstruct sensor
  (pos nil :type point)
  (distance nil :type fixnum))

(defun manhattan (a b)
  "Returns manhattan distance between 2 points"
  (declare (type point a b))
  (let ((x1 (point-x a))
        (y1 (point-y a))
        (x2 (point-x b))
        (y2 (point-y b)))
    (+ (abs (- x1 x2))
       (abs (- y1 y2)))))

(defmacro ignore-chars (&rest chars)
  (iter
    (for c in chars)
    (collect (list 'set-macro-character c
                   '(lambda (stream char) (declare (ignore stream char)) (values)))
      into cmds)
    (finally
     (return (cons 'progn cmds)))))

(defmacro with-list-elements (binds list &body body)
  (let* ((list-var (gensym))
         (binds (cons (list list-var list)
                      (mapcar (lambda (b)
                                (list (cadr b) (list 'nth (car b) list-var)))
                              binds))))
    `(let* ,binds
       ,@body)))


(defparameter +sample-input+ "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3
")

(defun read-sensors (in)
  (let ((sensors nil)
        (beacons nil))
    (do-register-groups (sx sy bx by) ("Sensor at x=(-?\\d+), y=(-?\\d+): closest beacon is at x=(-?\\d+), y=(-?\\d+)"
                                       in (values sensors beacons) :sharedp t)
      (let ((sensor (make-point :x (parse-integer sx) :y (parse-integer sy)))
            (beacon (make-point :x (parse-integer bx) :y (parse-integer by))))
        (push (make-sensor :pos sensor
                           :distance (manhattan sensor beacon))
              sensors)
        (pushnew beacon beacons :test #'point=)))))

(defun covered (point sensors)
  (iter
    (for s in sensors)
    (for sdist = (sensor-distance s))
    (for spos = (sensor-pos s))
    (for dist = (manhattan spos point))
    (thereis (<= dist sdist))))

(defun count-row-left (row sensors &optional (max-empty 100))
  (iter
    (with covered = 0)
    (with empty = 0)
    (for x downfrom -1)
    (for p = (make-point :x x :y row))
    (while (< empty max-empty))
    (if (covered p sensors)
        (progn
          (setf empty 0)
          (incf covered))
        (incf empty))
    (finally (return covered))))

(defun count-row-right (row sensors &optional (max-empty 100))
  (iter
    (with covered = 0)
    (with empty = 0)
    (for x from 0)
    (for p = (make-point :x x :y row))
    (while (< empty max-empty))
    (if (covered p sensors)
        (progn
          (setf empty 0)
          (incf covered))
        (incf empty))
    (finally (return covered))))

(defun count-row (row sensors &optional (max-empty 100))
  (let ((left (count-row-left row sensors max-empty))
        (right (count-row-right row sensors max-empty)))
    (+ left right)))

(defun basic (row input &optional (max-empty 100))
  (multiple-value-bind (sensors beacons) (read-sensors input)
    (iter
      (with count = (count-row row sensors max-empty))
      (for b in beacons)
      (counting (= row (point-y b)) into row-beacons)
      (finally
       (return (- count row-beacons))))))

(defun bonus (max-val input)
  (multiple-value-bind (sensors beacons) (read-sensors input)
    (iter
      (for y from 0 to max-val)
      (for res = (iter
                   (for x from 0 to max-val)
                   (for point = (make-point :x x :y y))
                   (finding point such-that (not (covered point sensors)))))
      (until res)
      (finally
       (let ((x (point-x res))
             (y (point-y res)))
         (return (+ y (* x 4000000))))))))
