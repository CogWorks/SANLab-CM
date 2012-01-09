(clear-all)

(define-model test

(sgp :esc t :trace-detail high :v t)

(chunk-type goal)
(chunk-type done)

(P test
   ?goal>
      buffer empty
   ?manual>
      state free
==>
   +goal>
      isa goal
   +manual>
      isa hand-to-home
)

(P test2
   =goal>
      isa goal
   ?manual>
      state free
==>
   +goal>
      isa done
   +manual>
      isa press-key
      key "d"
)

)

(defmethod device-handle-click ((dev list))
)

(defmethod device-handle-keypress ((dev list) key)
)


(defun run-model ()
  (install-device '())

  (run 2.0))