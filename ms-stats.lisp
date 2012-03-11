(defconstant M_PI 3.141592653589793)

(defun Norm_p (z)
  (let ((z (abs z)))
    (expt (1+ (* z (+ 0.0498673470
                      (* z (+ 0.0211410061
                              (* z (+ 0.0032776263
                                      (* z (+ 0.0000380036
                                              (* z (+ 0.0000488906
                                                      (* z 0.0000053830))))))))))))
          -16)))

(defun TtoZ (tv df)
  (let* ((a9 (- df 0.5))
         (b9 (* 48 a9 a9))
         (t9 (/ (* tv tv) df))
         z8 p7 b7)
    (if (>= t9 0.04)
        (setf z8 (* a9 (log (1+ t9))))
      (setf z8 (* a9 t9 (1+ (* t9 (- (/ (* t9 (- 1 (* t9 0.75))) 3) 0.5))))))
    (setf p7 (+ 85.5 (* z8 (+ 24 (* z8 (+ 3.3 (* 0.4 z8)))))))
    (setf b7 (+ (* 0.8 z8 z8) 100 b9))
    (* (sqrt z8) (1+ (/ (+ z8 3 (- (/ p7 b7))) b9)))
    ))

(defun TtoP (tv df)
  (let ((tsq (* tv tv))
        (tv (abs tv)))
    (cond ((= df 1)
           (- 1 (/ (* 2 (atan tv)) M_PI))
           )
          ((= df 2)
           (- 1 (/ tv (sqrt (+ tsq 2))))
           )
          ((= df 3)
           (- 1 (/ (* 2 (+ (atan (/ tv (sqrt 3))) (/ (* tv (sqrt 3)) (+ tsq 3)))) M_PI))
           )
          ((= df 4)
           (- 1 (/ (* tv (1+ (/ 2 (+ tsq 4)))) (sqrt (+ tsq 4))))
           )
          (t
           (Norm_p (TtoZ tv df))))))

(defun compute-models (lst1 lst2 out)
  (do ((x (car lst1) (car lst1))
       (y (car lst2) (car lst2)))
      ((null lst1) nil)
    (setf (app-property 'current-controller) (make-instance 'controller :model (make-instance 'model)))
    (let ((model (open-model (app-property 'current-controller) x)) data)
      (setf lst1 (cdr lst1) lst2 (cdr lst2))
      (cond (model
             (setf data nil)
             (with-open-file (durations y :direction :input)
               (do ((line (read durations nil nil) (read durations nil nil)))
                   ((null line) line)
                 (push line data)))
             (run-model (app-property 'current-controller) 30
                        #'(lambda (method &rest args)
                            (case method
                              ('results
                               (let ((times (mapcar #'trial-result-duration (first args))))
                               (multiple-value-bind (tv df) (t-test times data)
                                 (let ((p (TtoP tv df)) (m1 (list-mean times)) (m2 (list-mean data)))
                                   (format out "~A~C~A~C~A~C~A~C~A~C~A~C~A~C~A~%" x
                                           #\tab m1
                                           #\tab (stdev times m1) 
                                           #\tab m2
                                           #\tab (abs (* 100.0 (/ (- m1 m2) m1)))
                                           #\tab tv #\tab df
                                           #\tab p)))))))
                        :wait t :show nil))))))

(defun compute-snt-results (path)
  (initialize)
  (let ((lst (directory (format nil "~A/*.san" path))))
    (setf lst (mapcar #'(lambda (x) (let* ((path (format nil "~A" x))
                                           (newpath (format nil "~A-durations.txt" (subseq path 0 (- (length path) 5)))))
                                      (list x newpath)))
                      lst))
    (let (a b)
      (dolist (x lst)
        (push (first x) a)
        (push (second x) b))
      (compute-models a b *standard-output*))))

(defun compute-dmap5-results (path)
  (initialize)
  (let ((lst (directory (format nil "~A/*.san" path))))
    (setf lst (mapcar #'(lambda (x) (let* ((path (format nil "~A" x))
                                           (newpath (format nil "~A-durations.txt" (subseq path 0 (- (length path) 5)))))
                                      (list x newpath)))
                      lst))
    (let (a b)
      (dolist (x lst)
        (push (first x) a)
        (push (second x) b))
      (compute-models a b *standard-output*))))
