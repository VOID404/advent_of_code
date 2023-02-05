(defpackage day11
  (:use :cl :str)
  (:export))

(in-package :day11)
(declaim (optimize (safety 3)))

(defstruct monkey
  (id nil :type integer)
  (items nil :type list)
  (op nil :type (function (integer) integer))
  (pred nil :type integer) ;; divisible by this number
  (on-true nil :type integer)
  (on-false nil :type integer)
  (inspects 0 :type integer))

(defvar *monkeys* nil)
(defun set-sample ()
  (setf *monkeys* (list
                   (make-monkey :id 0
                                :items '(79 98)
                                :op (lambda (old) (* 19 old))
                                :pred 23
                                :on-true 2
                                :on-false 3)

                   (make-monkey :id 1
                                :items '(54 65 75 74)
                                :op (lambda (old) (+ 6 old))
                                :pred 19
                                :on-true 2
                                :on-false 0)

                   (make-monkey :id 2
                                :items '(79 60 97)
                                :op (lambda (old) (* old old))
                                :pred 13
                                :on-true 1
                                :on-false 3)

                   (make-monkey :id 3
                                :items '(74)
                                :op (lambda (old) (+ 3 old))
                                :pred 17
                                :on-true 0
                                :on-false 1))))

(defun find-monkey (id)
  "Find a monkey with given ID"
  (loop
    :for m :in *monkeys*
    :if (eq id (monkey-id m))
      :return m))

(defun throw-item (item monkey-n)
  "Adds given item to monkey nr n in *monkeys* list"
  (let* ((m (find-monkey monkey-n))
         (items (monkey-items m)))
    (setf (monkey-items m) (cons item items))))

(defun show-state (&optional (monkeys *monkeys*))
  (loop
    :for m :in monkeys
    :for id = (monkey-id m)
    :for items = (monkey-items m)
    :for inspects = (monkey-inspects m)
    :do (format t "~a: ~a [~a]~%" id items inspects)))


(defun parse-op (line)
  (let* ((ins (words (string-trim "Operation: new=old" line)))
         (op (read-from-string (car ins)))
         (num (ignore-errors (parse-integer (cadr ins))))
         (l (if num
                (lambda (n) (funcall op num n))
                (lambda (n) (funcall op n n)))))
    (lambda (n) (funcall l n))))

(defun parse-monkey (&optional (is *standard-input*))
  (let ((id (parse-integer (string-trim "Monkey :" (read-line is))))
        (items (mapcar #'parse-integer (split ", " (string-trim "   Startingitems:" (read-line is)))))
        (op (parse-op (read-line is)))
        (div-by (parse-integer (string-trim "Test: divisible by" (read-line is))))
        (ttarget (parse-integer (string-trim "  If true: throw to monkey" (read-line is))))
        (ftarget (parse-integer (string-trim "  If false: throw to monkey" (read-line is)))))
    (make-monkey :id id
                 :items items
                 :op op
                 :pred div-by
                 :on-true ttarget
                 :on-false ftarget)))

(defun parse-monkeys (&optional (is *standard-input*))
  (loop
    :for next-ch = (peek-char t *standard-input* nil :eof)
    :until (eq next-ch :eof)
    :collect (parse-monkey is) :into monkeys
    :finally (setf *monkeys* monkeys)))

(defun monkey-business ()
  (let* ((b (sort (mapcar #'monkey-inspects *monkeys*) #'>))
         (m1 (car b))
         (m2 (cadr b)))
    (* m1 m2)))


(defun process-monkey (m lcm &optional (div 1))
  "Takes all items from monkey, increases item worry and throws them. Worry is divided by div"
  (loop
    :with items = (monkey-items m)
    :initially
       (incf (monkey-inspects m) (length items))
       (setf (monkey-items m) nil)
    :for i :in items
    :for worry = (mod (floor (funcall (monkey-op m) i) div) lcm)
    :for target = (if (zerop (mod worry (monkey-pred m)))
                      (monkey-on-true m)
                      (monkey-on-false m))
    :do (throw-item worry target)))

(defun simulate (&optional (n 1) div)
  "Simulate n rounds of throwing. If div, divide worry after each inspect"
  (let ((lcm (apply #'lcm (mapcar #'monkey-pred *monkeys*))))
    (dotimes (i n)
      (dolist (m *monkeys*)
        (process-monkey m lcm 3)))))
