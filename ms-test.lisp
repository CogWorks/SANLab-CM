

(defclass test-parser (parser)
  ((events :initform nil :accessor test-parser-items)))

(defparameter *event-mapping*
  '((eg-fixation . (:type :routine
                    :routine "Eye Movement"
                    :event-id (2 3)))
    (click-target-slow . (:type :routine
                          :routine "Slow Move-Click"
                          :event-id (3 5 7 9 13)))
))

(defmacro gen-trial (type looks start valid)
  (let ((time (gensym)) (v (eq valid 'valid)))
    (cond ((eq type 'find-target)
           `(progn
              ; trial start
              (setf ht (make-hash-table))
              (setf (gethash :start ht) ,start)
              (push (list 'start-trial ht) (test-parser-items parser))
              
              ; eye movements
              (let ((,time ,start))
                (setf ,time (+ ,time (user-defdist-gaussian 150.0 10.0)))
                (dotimes (i ,looks)
                  (setf ht (make-hash-table))
                  (setf stime (make-hash-table))
                  (setf etime (make-hash-table))
                  (setf (gethash 2 stime) ,time)
                  (setf ,time (+ ,time (user-defdist-gaussian 30.0 10.0)))
                  (setf (gethash 2 etime) ,time)
                  (setf (gethash 3 stime) ,time)
                  (setf ,time (+ ,time (user-defdist-gaussian 100.0 10.0)))
                  (setf (gethash 3 etime) ,time)
                  (setf (gethash :start ht) stime)
                  (setf (gethash :end ht) etime)
                  (push (list 'eg-fixation ht) (test-parser-items parser))
                  (setf ,time (+ ,time (user-defdist-gaussian 100.0 10.0)))
                  )
                (setf ht (make-hash-table) stime (make-hash-table) etime (make-hash-table))
                (setf (gethash 3 stime) ,time)
                (setf ,time (+ ,time (user-defdist-gaussian 100.0 20.0)))
                (setf (gethash 3 etime) ,time (gethash 5 stime) ,time)
                (setf (gethash 5 etime) ,time (gethash 7 stime) ,time)
                (setf ,time (+ ,time (user-defdist-gaussian 30.0 10.0)))
                (setf (gethash 7 etime) ,time (gethash 9 stime) ,time)
                (setf ,time (+ ,time (user-defdist-gaussian 100.0 10.0)))
                (setf (gethash 9 etime) ,time)
                (setf ,time (+ ,time (user-defdist-gaussian 100.0 20.0)))
                (setf (gethash 13 stime) ,time)
                (setf ,time (+ ,time (user-defdist-gaussian 100.0 20.0)))
                (setf (gethash 13 etime) ,time)
                (setf (gethash :start ht) stime)
                (setf (gethash :end ht) etime)
                (push (list 'click-target-slow ht) (test-parser-items parser))
                
                (setf ht (make-hash-table))
                (setf (gethash :end ht) ,time)
                (setf (gethash 'valid-trial ht) ,v)
                (setf ,start ,time)
                (push (list 'end-trial ht) (test-parser-items parser))))))))

(defmethod initialize-parser ((parser test-parser))
  (let (ht stime etime (start 0))
    (setf (test-parser-items parser) nil)

    (gen-trial find-target 3 start valid) ; 1
    (gen-trial find-target 3 start valid) ; 13
    (gen-trial find-target 4 start valid) ; 2
    (gen-trial find-target 4 start invalid)
    (gen-trial find-target 3 start valid) ; 3
    (gen-trial find-target 2 start valid) ; 4
    (gen-trial find-target 3 start valid) ; 5
    (gen-trial find-target 4 start valid) ; 6
    (gen-trial find-target 3 start invalid)
    (gen-trial find-target 4 start valid) ; 7
    (gen-trial find-target 3 start valid) ; 8
    (gen-trial find-target 5 start valid) ; 9
    (gen-trial find-target 2 start valid) ; 10
    (gen-trial find-target 2 start invalid)
    (gen-trial find-target 3 start 'valid) ; 11
    (gen-trial find-target 4 start 'valid) ; 12

    (setf (test-parser-items parser) (reverse (test-parser-items parser)))
    parser))

(defmethod parse-item ((parser test-parser))
  (let ((item (pop (test-parser-items parser))))
    (values (car item) (cadr item))))
