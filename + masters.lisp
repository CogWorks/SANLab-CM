(defsystem "MS-Files"
  (:package CL-USER)
  :members ("globals.lisp"
            "common.lisp"
            "properties.lisp"
            "pointers.lisp"
            "activity-type.lisp"
            "distribution.lisp"
            "interactive-routine.lisp"
            "iroutines.lisp"
;            "rb-trees-struct-1.lisp"
            "ms-common.lisp"
            "ms-classes.lisp"
            "ms-queue.lisp"
            "ms-macros.lisp"
            "ms-main.lisp")
  :rules ((:in-order-to :compile :all
           (:requires (:load :previous)))
          (:in-order-to :load :all
           (:requires (:load :previous)))))


(if (not (ignore-errors (symbol-function 'sanlab)))
    (error "Please load \"+ loader.lisp\" prior to loading \"+ masters.lisp\"~%"))

(compile-system "MS-Files")
(load-system "MS-Files")
