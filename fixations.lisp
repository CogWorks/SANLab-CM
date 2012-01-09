
(defparameter *fix-radius* 35)
(defparameter *num-cameras* 1)
(defparameter *fix-threshold* (* 6 *num-cameras*))

(defmacro defclassic (class supers &rest slots) 
  `(defclass 
	,class 
	,supers    
	,(mapcar 
	  #'(lambda (s)                   ; s  = slot specification 
	     (flet ((build (sn)           ; sn = slot name  
		      (list sn ':accessor sn ':initarg
			    (read-from-string                       
			     (concatenate 'string ":" (symbol-name sn))))))
       
	       (cond ((atom s) (append (build s) '(:initform nil)))
                     ((null (cddr s)) (append (build (first s)) (list ':initform (second s))))
                     ((eql t (second s))
                      (append (build (first s)) 
                              (nthcdr 2 s)
                              (if (not (member :initform (nthcdr 2 s))) '(:initform nil))))
                     (t s))))
	  slots) ))

(defclassic fixation-algorithm ()
  fix-radius 
  num-bad-data
  bad-data-threshold 
  fixation-threshold 
  outlier-threshold 
  present-fix 
  potential-fix 
  (call-count 0)
  )

(defclassic eye-fix-algorithm (fixation-algorithm)
  (fix-radius *fix-radius*)
  (num-bad-data 0) ;; 
  (bad-data-threshold (* 6 *num-cameras*))
  (fixation-threshold (* 6 *num-cameras*))
  (outlier-threshold (* 2 *num-cameras*))
  (present-fix (make-instance 'eye-fixation))
  (potential-fix (make-instance 'eye-fixation)))  

(defclassic fixation ()  
  (samples-in 0)      ;; number of samples in the fixation; samples >= *min-fix-samples*
  (samples-out 0)     ;; number of samples outside of fixation
  (x-fix 0.0)         ;; x-location summation
  (y-fix 0.0)         ;; y-location summation
  (x-cog 0.0)         ;; x-location of center of gravity of fixation 
  (y-cog 0.0)         ;; y-location of center of gravity of fixation
  (begin 0)           ;; beginning time of fixation
  (end 0)             ;; ending time of fixation
  (duration 0)        ;; fixation duration in milliseconds (- end begin)
  (pupil 0)
  (file NIL)
)

(defclassic eye-fixation (fixation))

(defmacro within-current-fixation-p (dev cog radius)
  "calculates the distance from the current center of gravitity (xcog, ycog) to the incoming eye (x,y) data"
 `(<= (sqrt (+ (expt (- (x ,dev) (x-cog ,cog)) 2) (expt (- (y ,dev) (y-cog ,cog)) 2))) ,radius))

(defmacro fixation-in-progress-p (fix)
  `(plusp (samples-in ,fix)))

(defmacro good-data-p (dev)
  `(and (plusp (decency ,dev)) (>= (x ,dev) 0) (>= (y ,dev) 0)))

(defmacro minimum-fixation-p (fix threshold)
  `(>= (samples-in ,fix) ,threshold))

(defmacro outlier-threshold-exceeded-p ( fix threshold)
  `(> (samples-out ,fix)  ,threshold))

(defmacro reset-potential (algo)
  `(setf (potential-fix ,algo) (make-instance 'eye-fixation)))

(defmacro potential>present-p (present potential)
  `(> (samples-in ,potential) (samples-in ,present)))

(defmethod add-new-point ((dev eye-data) (fix fixation))
 "keep running average of pupil-diameter"
  (with-slots (pup-diam) dev
    (with-slots (samples-in pupil) fix
      (when (and (numberp pupil) (numberp pup-diam))
        (setf pupil (if (plusp pupil)
                        (/ (+ (* samples-in pupil) pup-diam) (1+ samples-in))
                      (+ pupil pup-diam))))))
  (call-next-method))
          
(defmethod add-new-point ((dev data) (fix fixation))
  (with-slots (data-time x y) dev
    (with-slots (samples-in samples-out x-fix y-fix x-cog y-cog begin end duration) fix
      (setq samples-in (+ 1 samples-in))
      (setq  x-fix (+ x-fix x) y-fix (+ y-fix y))
      (setq x-cog (/ x-fix samples-in) y-cog (/ y-fix samples-in))
      (dbg "adding point ~S ~S " samples-in samples-out)
      (if (zerop begin) (setq begin data-time))
      (setq end data-time)
      (setq duration (- end begin)))))

(defmethod restore-out-points ((from fixation) (to fixation))
  (with-slots (samples-in x-fix y-fix x-cog y-cog begin end duration pupil) to
    (when (plusp (samples-in from))
      (setq pupil (/ (+ (* pupil samples-in) (* (pupil from) (samples-in from))) (+ samples-in (samples-in from)))) ;;for pupil diameter
      (setq samples-in (+ samples-in (samples-in from))) ;; new # of samples
      (setq x-fix (+ x-fix (x-fix from)) y-fix (+ y-fix (y-fix from))) ;; new fixation accums
      (setq x-cog (/ x-fix samples-in) y-cog (/ y-fix samples-in)) ;recompute center of gravity
      (setq end (+ end (end from))) ;; new end time
      (setq duration (- end begin)) ;; recompute duration  
    )))

(defmethod finish-fixation ((obj eye-fix-algorithm))
  (with-slots (present-fix) obj
      (save-fixation present-fix)
      (setq present-fix (make-instance 'eye-fixation))))

(defmethod potential->present ((obj eye-fix-algorithm))
   (with-slots (present-fix potential-fix) obj
     (setq present-fix potential-fix)
     (setq potential-fix (make-instance 'eye-fixation))))

(defmethod reset-fixations ((obj eye-fix-algorithm))
   (with-slots (present-fix potential-fix) obj
     (setq present-fix (make-instance 'eye-fixation))
     (setq potential-fix (make-instance 'eye-fixation))))

(defmethod finish-fixation ((obj mse-fix-algorithm))
  (with-slots (present-fix) obj
      (save-fixation present-fix)
      (setq present-fix (make-instance 'mouse-fixation))))

(defmethod potential->present ((obj mse-fix-algorithm))
   (with-slots (present-fix potential-fix) obj
     (setq present-fix potential-fix)
     (setq potential-fix (make-instance 'mouse-fixation))))

(defmethod reset-fixations ((obj mse-fix-algorithm))
   (with-slots (present-fix potential-fix) obj
     (setq present-fix (make-instance 'mouse-fixation))
     (setq potential-fix (make-instance 'mouse-fixation))))

(defun dbg (str &rest args)
#+:debug  (apply 'format t (concatenate 'string "~%" str) args)
  )

(defmethod fix-algorithm ((dev data) (algo fixation-algorithm)) 
  (with-slots (fix-radius num-bad-data bad-data-threshold fixation-threshold outlier-threshold present-fix potential-fix) algo
    (cond ((good-data-p dev)
           ;;good data
           (dbg "1 good data ~S ~S ~S ~S" (x dev) (y dev) (samples-in present-fix) (samples-in potential-fix))
           (setq num-bad-data 0)
           (cond ((fixation-in-progress-p present-fix)
                  ;;a fixation is in progress
                  (dbg "2 fix-in-progress")
                  (cond ((within-current-fixation-p dev present-fix fix-radius)
                         ;; current point is in circle around current center of gravity
                         (dbg "3 in circle")
                         (restore-out-points potential-fix present-fix) ;;yes, bring back in any outlying points
                         (reset-potential algo)
                         (add-new-point dev present-fix))
                        (t  
                         ;; current point is an outlying point
                         (dbg "3 outlier")
                         (incf (samples-out present-fix))
                         (cond ((and (outlier-threshold-exceeded-p present-fix outlier-threshold) ;;matches LC only if outlier-threshold = fixation-threshold
                                     (minimum-fixation-p present-fix fixation-threshold))  ;;D1
                                (dbg "4 present-done, move potential")
                                (finish-fixation algo) ;save and reset
                                (potential->present algo)
                                (cond ((within-current-fixation-p dev present-fix fix-radius)
                                       (dbg "5 adding to present")
                                       (add-new-point dev present-fix))
                                      (t
                                       (dbg "5 starting potential")
                                       (reset-potential algo)
                                       (add-new-point dev potential-fix))))
                               ((fixation-in-progress-p potential-fix) ;D2 F1
                                (dbg "4 potential in progress ~S ~S ~S ~S" (x dev) (y dev) (x-cog potential-fix) (y-cog potential-fix))
                                (cond ((within-current-fixation-p dev potential-fix fix-radius) ;;G1
                                       (dbg "5 adding to potential")
                                       (add-new-point dev potential-fix)
                                       (when (minimum-fixation-p potential-fix fixation-threshold) ;;H
                                         (dbg "6 potential->present")
                                         (potential->present algo))
                                       (dbg "7 continue potential"))
                                      (t  ;;G2 
                                       (when (potential>present-p present-fix potential-fix) ;;this is new
                                         (dbg "5a moving potential")
                                         (potential->present algo))
                                       (dbg "5 starting potential")
                                       (reset-potential algo)
                                       (add-new-point dev potential-fix))))
                               (t ;F2
                                (dbg "4 starting potential")
                                (reset-potential algo)
                                (add-new-point dev potential-fix))))))
                                
                 (t  
                  ;; no fixation in progress, start one
                  (dbg "2 start-fixation")
                  (add-new-point dev present-fix )
                  (reset-potential algo))))
          (t 
           ;;bad data
           (dbg "1 bad data")
           (incf num-bad-data)
           (cond ((>= num-bad-data bad-data-threshold)
                  ;; checking bad samples and threshold exceeded
                  (dbg "2 bad data threshold exceeded")
                  ;;;;(setq num-bad-data -1) ;;stop count
                  (cond ((minimum-fixation-p present-fix fixation-threshold) 
                         (dbg "3 make fixation")
                         (finish-fixation algo))
                        ((= num-bad-data bad-data-threshold)
                         (dbg "3 reset fixations")
                         (reset-fixations algo))))
                 
                  )))))

;;;
;;;  API
;;;

(defmethod fixation ((obj eye-data))
  (fix-algorithm obj (eye-fix)))

(defmethod fixation ((obj mse-data))
  (fix-algorithm obj (mouse-fix)))

(defun purge-fixations () ;;when finished collecting eye data,
  (aif (eye-fix)
       (if (>= (samples-in (present-fix it)) (fixation-threshold it)) (save-fixation (present-fix it))))
  (aif (mouse-fix)
       (if (>= (samples-in (present-fix it)) (fixation-threshold it)) (save-fixation (present-fix it)))))
       
(defun init-fix-algorithms ()
  (setf (eye-algo (pm)) (make-instance 'eye-fix-algorithm))
  (setf (mse-algo (pm)) (make-instance 'mse-fix-algorithm)))



(defun extract-fixations ()
  "Makes fixations across all the eye data - maintaining file information with each fixations after the entire file has been read in"
(unless (null (get-all-eye-data))
  (set-all-fixations nil)
  (let* ((eye (reverse (get-all-eye-data)))
         (cur-file (if eye (file (first eye)) nil)))
    (dolist (e eye)
      (when (equal (type-of e) 'eye-data) (fixation e)
        (when (not (equal (file e) cur-file)) 
          (purge-fixations)
          (let ((fixes (get-all-fixations)))
            (dolist (f fixes)
              (if (null (file f))
                  (setf (file f) cur-file)))
            (set-all-fixations fixes))
          (setf cur-file (file e)))
        ))
    (purge-fixations)
    (let ((fixes (get-all-fixations)))
      (dolist (f fixes)
        (if (null (file f))
            (setf (file f) cur-file)))
      (set-all-fixations fixes))
    )
  ))

(defun extract-mse-fixations ()
  "Makes fixations across all the mouse data - maintaining file information with each fixations after the entire file has been read in"
(unless (null (get-all-mse-data))
  (set-all-mse-fixations nil)
  (let* ((mse (reverse (get-all-mse-data)))
         (cur-file (if eye (file (first mse)) nil)))
    (dolist (m mse)
      (when (equal (type-of m) 'mse-data) (fixation m)
        (when (not (equal (file m) cur-file)) 
          (purge-fixations)
          (let ((fixes (get-all-mse-fixations)))
            (dolist (f fixes)
              (if (null (file f))
                  (setf (file f) cur-file)))
            (set-all-mse-fixations fixes))
          (setf cur-file (file m)))
        ))
    (purge-fixations)
    (let ((fixes (get-all-mse-fixations)))
      (dolist (f fixes)
        (if (null (file f))
            (setf (file f) cur-file)))
      (set-all-mse-fixations fixes))
    )
  ))

;;;;;;============================ Fixation Assignment Info ====================================;;;;;;
(defun assign-fixation (file this-time x y threshold) (declare (ignorable file))
  "Returns the id of the object that is closest to or within the x-y coordinate specified at time"
  (let ((all-obj-data (get-all-obj-data))
        (obj-times)
        (shortest-dist 10000)
        (dist)
        (ret))
  ;  (setf obj-times (remove NIL (mapcar #'(lambda (x) 
   ;                             (let (last) 
    ;                              (dolist (l (reverse (location x)))
     ;                               (unless (not (null last))
      ;                                (if (>= this-time (data-time l))
       ;                                   (setf last (data-time l)))))
        ;                          (if last (cons (id x) (find last (location x) :test 'equal :key 'data-time))))) all-obj-data))
         ;                         )

    (setf obj-times (remove NIL (mapcar #'(lambda (x) 
                                            (unless (null (find (closest-under (location x) this-time :key 'data-time) (location x) :key 'data-time)) 
                                              (if (null (end-time (find (closest-under (location x) this-time :key 'data-time) (location x) :key 'data-time)))
                                                  (cons (id x) (find (closest-under (location x) this-time :key 'data-time) (location x) :key 'data-time))
                                                (unless (<= (end-time (find (closest-under (location x) this-time :key 'data-time) (location x) :key 'data-time)) this-time) 
                                                  (cons (id x) (find (closest-under (location x) this-time :key 'data-time) (location x) :key 'data-time)))))) all-obj-data)))
    (unless (null obj-times) (dolist (obj obj-times ret)
                               (setf dist 1000)   
  ;   (setf dist (min-dist-to-obj x y (x (rest obj)) (y (rest obj)) (w (rest obj)) (h (rest obj))))
                               (if (within-object? x y (x (rest obj)) (y (rest obj)) (w (rest obj)) (h (rest obj)))
                                   (progn
                                     (setf shortest-dist 0)
                                     (setf ret (first obj)))
                                 (if (and (< dist threshold) (< dist shortest-dist))
                                     (progn
                                       (setf ret (first obj))
                (setf shortest-dist dist))
          (progn
            (setf dist (if (or (null x) (null y) (null (x (rest obj))) (null (y (rest obj)))) 10000 
                         (min-dist-to-obj x y (x (rest obj)) (y (rest obj)) (w (rest obj)) (h (rest obj)))))
            (if (and (< dist threshold) (< dist shortest-dist))
                (progn
                  (setf ret (first obj))
                  (setf shortest-dist dist))))))))))
  
(defun within-object? (x y obj-x obj-y obj-w obj-h)
  "Determines whether the x y falls within an object with the given dimensions"
  (if (or (null obj-h) (null obj-w) (null obj-x) (null obj-y))
      NIL
    (and (<= x (+ obj-x obj-w)) (>= x obj-x) (<= y (+ obj-y obj-h)) (>= y obj-y))
    ))

(defun min-dist-to-obj (x y obj-x obj-y obj-w obj-h)
  "Returns the minimum distance to this rectangular object"
   (let ((dist 1000000))
     (if (or (null x) (null y) (null obj-x) (null obj-y) (null obj-w) (null obj-h))
         dist
       (progn
         (setf dist (min dist (distance x y obj-x obj-y)))
         (dotimes (x2 obj-w)
           (setf dist (min dist (distance x y (+ obj-x x2) obj-y) (distance x y (+ obj-x x2) (+ obj-h obj-y)))))
         (dotimes (y2 obj-h)
           (setf dist (min dist (distance x y obj-x (+ y2 obj-y)) (distance x y (+ obj-w obj-x) (+ y2 obj-y)))))
         ))
     dist))

(defun list-eye-locs ()
  (dolist (eye (get-all-eye-data))
    (print (assign-fixation (file eye) (data-time eye) (x eye) (y eye) 35))
    )
)