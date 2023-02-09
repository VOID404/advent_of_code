(defsystem "advent-of-lisp"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ("str" "iterate" "cl-ppcre")
  :components ((:module "src"
                :components
                ((:file "day01")
                 (:file "day11")
                 (:file "day12")
                 (:file "day13")
                 (:file "day14")
                 (:file "day15"))))
  :description ""
  :in-order-to ((test-op (test-op "advent-of-lisp/tests"))))

(defsystem "advent-of-lisp/tests"
  :author ""
  :license ""
  :depends-on ("advent-of-lisp"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "day01"))))
  :description "Test system for advent-of-lisp"
  :perform (test-op (op c) (symbol-call :rove :run c)))
