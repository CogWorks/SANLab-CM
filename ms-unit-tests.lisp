(let ((uid 0))

(defmacro fixed-resource (start end)
  (incf uid)
  `(make-instance 'resource :id ,uid
                  :start-time ,start :end-time ,end
                  :latest-start-time ,start
                  :latest-end-time ,end
                  :earliest-start-time ,start
                  :earliest-end-time ,end
                  :duration (- ,end ,start)))

(defmacro latest-resource (start end)
  (incf uid)
  `(make-instance 'resource :id ,uid
                  :latest-start-time ,start
                  :latest-end-time ,end
                  :duration (- ,end ,start)))

(defmacro earliest-resource (start end)
  (incf uid)
  `(make-instance 'resource :id ,uid
                  :earliest-start-time ,start
                  :earliest-end-time ,end
                  :duration (- ,end ,start)))

)

(let ((r1 (fixed-resource 250 300)))
  (assert (resource= r1 r1)))

(assert (resource= (fixed-resource 250 300) (fixed-resource 250 300)))
(assert (not (resource< (fixed-resource 250 300) (fixed-resource 250 300))))
(assert (resource< (fixed-resource 250 300) (fixed-resource 300 350)))
(assert (not (resource< (fixed-resource 250 300) (fixed-resource 200 250))))

(defparameter *test* nil)
(defparameter *resources* nil)

(let* ((r1 (latest-resource 371008 371058))
       (r2 (latest-resource 371170 371220))
       (r3 (latest-resource 371358 371408))
       (r4 (latest-resource 371840 371890))
       (r5 (latest-resource 371918 371968))
       (r6 (earliest-resource 371438 371488))
       (r7 (earliest-resource 372337 372387))
       (order (list r1 r6 r2 r3 r7 r4 r5))
       (l nil))
  (assert (resource< r1 r2))
  (assert (resource< r2 r3))
  (assert (resource< r3 r4))
  (assert (resource< r4 r5))
  (assert (not (resource< r5 r6)))
  (assert (resource< r6 r7))

  (assert (equal (list r1 r2) (list r1 r2)))

  (dolist (item order)
    (setf l (insert item l nil)))
  (setf *test* l)
  (setf *resources* order)
)
