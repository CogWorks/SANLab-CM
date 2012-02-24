#|
Copyright © 2007-2009 Evan W. Patton

This file is part of SANLab-CM.

SANLab-CM is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with SANLab-CM. If not, see <http://www.gnu.org/license/>.
|#

;; we behave slightly differently when importing and displaying information from CogTool,
;; versus a more generic ACT-R trace source
(defparameter *cogtool-trace* nil)
(defparameter *system-events* (make-hash-table :test #'equal))
(defparameter *last-display-event* nil)
(defparameter *vocal-events* nil)
(defparameter *manual-events* nil)

#|
Prints warning messages to stderr
|#
(defun print-warning (&rest args)
  (format *error-output* "*** Warning ***   ")
  (apply #'format *error-output* args))

#|
Enables trace output for debugging purposes
|#
(defparameter *enable-trace* t)

#|
Prints trace information to stream stored in *trace-output*
|#
(defun print-trace (&rest args)
  (cond (*enable-trace*
         (format *trace-output* "*** Trace ***   ")
         (apply #'format *trace-output* args))))

#|
This defines the superclass for all buffer objects. 
|#
(defclass buffer-action ()
  ((label :initform "" :initarg :label :accessor label)
   (start-time :initform nil :initarg :start-time :accessor start-time)
   (end-time :initform nil :initarg :end-time :accessor end-time)
   (dependents :initform nil :initarg :dependents :accessor dependents)
   (prerequisites :initform nil :initarg :prerequisites :accessor prerequisites)
   (depth :initform 0 :initarg :depth :accessor depth)
   (buffer :initform nil :initarg :buffer :accessor buffer)
   (uid :initform nil :initarg :uid :accessor uid)
   (operator-type :initform nil :initarg :operator-type :accessor operator-type)
   (x-pos :initform nil :initarg :x-pos :accessor x-pos)
   (y-pos :initform nil :initarg :y-pos :accessor y-pos)
   (visited :initform nil :accessor visited)
   (event-type :initform 'model :initarg :event-type :accessor event-type)
   (action-type :initform 'standard :initarg :action-type :accessor action-type)
   (created-by-action :initform nil :initarg :created-by-action :accessor created-by-action)
))

#|
Subclass of buffer-action that stores information regarding parallel motor movements
|#
(defclass manual-buffer-action (buffer-action)
  ((state :initform 'free :initarg :state :accessor state)
   (key :initform nil :initarg :key :accessor key)
   (prep-only :initform nil :initarg :prep-only :accessor prep-only)
   (move-id :initform nil :initarg :move-id :accessor move-id))
)

#|
Subclass of buffer-action which stores the next production that will be fired
|#
(defclass production (buffer-action)
  ((next-production :initform nil :initarg :next-production :accessor next-production)))

#|
Subclass of buffer-action which stores information regarding system events
|#
(defclass system-event (buffer-action)
  ((event-key :initform nil :initarg :event-key :accessor event-key)))

(defun balance-parens (str)
  (let ((ct 0) c)
    (do ((i 0 (1+ i)))
        ((= i (length str)))
      (setf c (elt str i))
      (cond ((equal c #\() (incf ct))
            ((equal c #\)) (decf ct))))
    ct)
)

(defun read-line-removing-objects (&rest args)
  (let ((str (apply #'read-line args)))
    (if (not (stringp str)) (return-from read-line-removing-objects str))
    (multiple-value-bind (pos len) (find-regexp-in-string "#<[^>]*>" str)
      (if (and pos len)
          (fill str #\space :start pos :end (+ pos len))))
    (multiple-value-bind (pos len) (find-regexp-in-string "SPEAK TEXT " str)
      (if (and pos len)
          (setf str (format nil "~A~C~A~C" (subseq str 0 (+ pos len)) #\" (subseq str (+ pos len)) #\"))))
    (multiple-value-bind (pos len) (find-regexp-in-string "OUTPUT-SPEECH " str)
      (if (and pos len)
          (setf str (format nil "~A~C~A~C" (subseq str 0 (+ pos len)) #\" (subseq str (+ pos len)) #\"))))
    str
))

#|
Reads a line from the trace file and returns it as a list of symbols.
|#
(defun read-trace-object (file)
  (let ((list (do ((x (read-from-string (format nil "(~A)" (read-line-removing-objects file nil :eof)))
                      (read-from-string (format nil "(~A)" (read-line-removing-objects file nil :eof)))))
                  (x x))))
;    (setf (cdddr list) (cons (cdddr list) nil))
    list))


#|
The following functions are helper functions designed to look for buffer and variable specifications inside of model productions
|#
(defun symbol-starting-with-+-p (sym)
  (and (symbolp sym)
       (char= #\+ (elt (format nil "~A" sym) 0))))

(defun symbol-starting-with-=-p (sym)
  (and (symbolp sym)
       (char= #\= (elt (format nil "~A" sym) 0))))

(defun symbol-starting-with-?-p (sym)
  (and (symbolp sym)
       (char= #\? (elt (format nil "~A" sym) 0))))

(defun symbol-ending-with->-p (sym)
  (and (symbolp sym)
       (let ((str (format nil "~A" sym)))
         (char= #\> (elt str (1- (length str)))))))

(defun buffer-request-symbol (sym)
  (and (symbol-starting-with-+-p sym)
       (symbol-ending-with->-p sym)))

(defun buffer-check-symbol (sym)
  (and (symbol-starting-with-=-p sym)
       (symbol-ending-with->-p sym)))

(defun buffer-query-symbol (sym)
  (and (symbol-starting-with-?-p sym)
       (symbol-ending-with->-p sym)))

(defun buffer-check-minus-> (sym)
  (and (buffer-check-symbol sym)
       (let ((str (format nil "~A" sym)))
         (read-from-string (subseq str 0 (1- (length str)))))))

(defun buffer-without-extra-chars (sym)
  (and (or (buffer-request-symbol sym)
           (buffer-check-symbol sym)
           (buffer-query-symbol sym))
       (let ((str (format nil "~A" sym)))
         (read-from-string (subseq str 1 (1- (length str)))))))

#|
This class is used to define instances of variables which appear on both the LHS and RHS of a production
|#
(defclass production-dependency ()
  ((name :initform nil :initarg :name :accessor name)
   (variable :initform nil :initarg :variable :accessor var)
   (rhs-buffers :initform nil :initarg :rhs-buffers :accessor rhs-buffers)
   (lhs-buffers :initform nil :initarg :lhs-buffers :accessor lhs-buffers)
   (lhs-object :initform nil :initarg :lhs-object :accessor lhs-object)
))

(defclass buffer-query ()
  ((buffer :initform nil :initarg :buffer :accessor buffer)
   (check-on :initform nil :initarg :check-on :accessor check-on)
   (value :initform nil :initarg :value :accessor value)))

#|
This function takes a production and extracts any variable dependencies from it.

THIS DOESN'T WORK WITH !BIND! e.g.:
(P breaking-production
   =goal>
      isa          goal
      count        =count
==>
   !bind!       =new-count      (1+ =count)
   =goal>
      count        =new-count
)
|#
(defparameter *registered-events* (make-hash-table :test #'equal))

(defun process-production (p)
  (let ((table (make-hash-table))
        (queries nil)
        (production (second p))
        (!eval!? nil)
        (lhs t)
        (current-buffer nil)
        (?query nil))
    (print-trace "******    Processing production ~A    ******~%" production)
    (print-trace "On left hand side of ~A~%" production)
    (dolist (sym (if (stringp (third p))
                     (cdddr p)
                   (cddr p))
                 (let ((ret nil))
                     (maphash #'(lambda (k v)
                                  (declare (ignore k))
                                  (if (and (rhs-buffers v) (lhs-buffers v))
                                      (push v ret)))
                              table)
                     (values ret queries)))
      (cond (!eval!?
             (setf !eval!? nil)
             )
            ((equal '==> sym)
             (print-trace "On right hand side of ~A~%" production)
             (setf ?query nil)
             (setf lhs nil))
            ((buffer-query-symbol sym)
             (setf ?query (make-instance 'buffer-query :buffer (buffer-without-extra-chars sym))))
            ((buffer-request-symbol sym)
             (print-trace "Processing buffer request ~A~%" sym)
             (setf current-buffer sym))
            ((buffer-check-symbol sym)
             (setf current-buffer sym)
             (print-trace "Processing buffer ~A ~A~%" (if lhs "check" "modification") sym)
             (let ((var (buffer-check-minus-> sym)))
               (if (not (gethash var table))
                   (if (not lhs)
                       (error "Found buffer modification on RHS not present as a check on LHS")
                     (setf (gethash var table) (make-instance 'production-dependency :name production :variable var :lhs-buffers (list sym)))))))
            ((eq sym '!eval!)
             (setf !eval!? t))
            ((and !eval!? (listp sym) (equal (first sym) 'register-model-event))
             (setf (gethash production *registered-events*) (list (eval (second sym)) (eval (third sym)))))
            ((and ?query (check-on ?query) (value ?query))
             (setf ?query (make-instance 'buffer-query :buffer (buffer ?query) :check-on sym)))
            ((and ?query (check-on ?query))
             (setf (value ?query) sym)
             (push ?query queries))
            (?query
             (setf (check-on ?query) sym))
            ((symbol-starting-with-=-p sym)
             (print-trace "Found variable ~A on ~A hand side~%" sym (if lhs "left" "right"))
             (if (not (gethash sym table))
                 (if (not lhs)
                     (print-warning "Found variable on RHS not bound on LHS")
                   (setf (gethash sym table) (make-instance 'production-dependency :name production :variable sym :lhs-buffers (list current-buffer))))
               (if lhs
                   (and (push current-buffer (lhs-buffers (gethash sym table)))
                        (print-trace "LHS Dependencies: ~A~%" (lhs-buffers (gethash sym table))))
                 (and (push current-buffer (rhs-buffers (gethash sym table)))
                      (print-trace "RHS Dependencies: ~A~%" (rhs-buffers (gethash sym table))))))
             )
            )
)))

#|
Finds the model inside of the target lisp file. This expects the model to be defined at the top-level.
|#
(defun get-model (file)
  (do ((x (read file) (read file)))
      ((and (listp x) (or (equal (first x) 'define-model) (equal (first x) 'define-cogtool-model))) x)))

(defun get-productions-from-model (model)
  (remove-if-not #'(lambda (x) (if (and (listp x) (or (equal (first x) 'p) (equal (first x) 'p*))) x)) model))

(defun get-model-dependency-structure (file)
  (let* ((model (get-model file))
         (productions (get-productions-from-model model))
         (var-table (make-hash-table))
         (query-table (make-hash-table)))
    (dolist (p productions (values var-table query-table))
      ; correct P for cogtool e.g. get-spelling-|?c|-10 ==> get-spelling-?c-10
      (setf (second p) (read-from-string (format nil "~A" (second p))))
      (multiple-value-bind (variables queries) (process-production p)
        (setf (gethash (second p) var-table) variables)
        (setf (gethash (second p) query-table) queries)))))

(defparameter *buffers* (make-hash-table))
(defparameter *history* (make-hash-table))
(defparameter *watches-before* nil)
(defparameter *watches-after* nil)
(defparameter *actions-after* nil)

(defmethod add-buffer-of-interest ((buffer symbol))
  (setf (gethash buffer *buffers*) (make-hash-table))
  (setf (gethash buffer *history*) (make-hash-table))
)

(defmethod buffer-of-interest ((buffer symbol) &key (index 0))
  (multiple-value-bind (obj pres) (gethash buffer *buffers*)
    (if (not pres) (print-warning "Buffer ~A not present in the list of interesting buffers.~%" buffer)
      (multiple-value-bind (obj2 pres2) (gethash index obj)
        (if (not pres2) (print-warning "Index ~S of buffer ~A has no value~%" index buffer))
        obj2))))

(defmethod (setf buffer-of-interest) (val (buffer symbol) &key (index 0))
  (multiple-value-bind (obj pres) (gethash buffer *buffers*)
    (if (not pres) (print-warning "Buffer ~A not present in the list of interesting buffers.~%" buffer)
      (multiple-value-bind (obj2 pres2) (gethash index obj)
        (declare (ignore obj2))
        (if (not pres2) (setf (gethash index obj) (make-hash-table)))
        (and (setf (buffer val) buffer)
             (setf (gethash index (gethash buffer *buffers*)) val))))))

(defmethod (setf buffer-of-interest) :before (val (buffer symbol) &key (index 0))
  (if (and val (listp val)) (break))
  (if (buffer-of-interest buffer :index index)
      (pushnew (buffer-of-interest buffer :index index) (gethash buffer *history*)))
  (dolist (pair *watches-before*)
    (if (and (eql (first pair) buffer) (buffer-of-interest buffer :index index))
        (pushnew (buffer-of-interest (second pair) :index index) (dependents (buffer-of-interest buffer :index index)))))
  (setf *watches-before* (remove buffer *watches-before* :key #'first)))

(defmethod (setf buffer-of-interest) :before (val (buffer (eql 'production)) &key (index 0))
  (if (buffer-of-interest buffer :index index)
      (setf (next-production (buffer-of-interest buffer :index index)) val))
  (if (not val)
      (break))
)

(defmethod (setf buffer-of-interest) :after (val (buffer symbol) &key (index 0))
  (declare (ignore index))
  (if (and val (listp val)) (break))
  (if val
      (progn
        (dolist (pair *watches-after*)
          (if (eql (car (first pair)) buffer)
              (progn
                (print-trace "Firing post-buffer watch for ~A~%" buffer)
                (pushnew val (dependents (second pair))))))
        (setf *watches-after* (remove buffer *watches-after* :key #'(lambda (x) (car (first x)))))))
  (if val
      (progn
        (dolist (act (remove-if-not #'(lambda (x) (equal x buffer)) *actions-after* :key 'car))
          (print-trace "Firing post-buffer action for ~A~%" buffer)
          (funcall (second act) val))
        (setf *actions-after* (remove-if #'(lambda (x) (equal x buffer)) *actions-after* :key 'car))))
)

(defmethod clear-buffer ((buffer symbol) &key (index 0))
  (multiple-value-bind (obj pres) (gethash buffer *buffers*)
    (declare (ignore obj))
    (if (not pres) (print-warning "Buffer ~A not present in list of interesting buffers.~%" buffer)
      (setf (gethash index (gethash buffer *buffers*)) nil))))

(defparameter *buffer-to-activity-map* nil)
(defparameter *y-coord-info* (make-hash-table :test #'equal))

(defun map-buffer (buf act)
  (push (list buf act) *buffer-to-activity-map*))

(defun default-type-for-buffer (buf)
  (let ((obj (find buf *buffer-to-activity-map* :key #'first)))
    (if obj
        (second obj)
      "Basic Operator")))

(add-buffer-of-interest 'production)
(map-buffer 'production "Cognitive Operator")
(setf (gethash "Cognitive Operator" *y-coord-info*) 400)
(setf (gethash "Attend Operator" *y-coord-info*) (gethash "Cognitive Operator" *y-coord-info*))
(setf (gethash "Initiate Operator" *y-coord-info*) (gethash "Cognitive Operator" *y-coord-info*))
(setf (gethash "Verify Operator" *y-coord-info*) (gethash "Cognitive Operator" *y-coord-info*))
(add-buffer-of-interest 'visual-location)
(map-buffer 'visual-location "Perceptual Operator (Visual)")
(setf (gethash "Perceptual Operator (Visual)" *y-coord-info*) 100)
(add-buffer-of-interest 'visual)
(map-buffer 'visual "Perceptual Operator (Visual)")
(add-buffer-of-interest 'vocal)
(map-buffer 'vocal "Speech Operator")
(setf (gethash "Speech Operator" *y-coord-info*) 800)
(add-buffer-of-interest 'aural-location)
(map-buffer 'aural-location "Perceptual Operator (Auditory)")
(setf (gethash "Perceptual Operator (Auditory)" *y-coord-info*) 200)
(add-buffer-of-interest 'aural)
(map-buffer 'aural "Perceptual Operator (Auditory)")
(add-buffer-of-interest 'retrieval)
(map-buffer 'retrieval "Declarative Memory")
(setf (gethash "Declarative Memory" *y-coord-info*) 300)
(add-buffer-of-interest 'manual)
(map-buffer 'manual "Motor Operator")
(setf (Gethash "Motor Operator" *y-coord-info*) 600)
(setf (gethash "Left Hand Operator" *y-coord-info*) 600)
(setf (gethash "Right Hand Operator" *y-coord-info*) 700)
(setf (gethash "Eye Movement Operator" *y-coord-info*) 900)
(add-buffer-of-interest 'imaginal)
(map-buffer 'imaginal "Task-oriented Memory")
(setf (gethash "Task-oriented Memory" *y-coord-info*) 500)
(add-buffer-of-interest 'goal)
(map-buffer 'goal "Task-oriented Memory")
(setf (gethash "System Resource" *y-coord-info*) 0)
(add-buffer-of-interest 'frame)
(map-buffer 'frame "System Resource")
(add-buffer-of-interest 'cogtool)
(map-buffer 'cogtool "System Resource")
(defparameter *start-node* (setf (buffer-of-interest 'production) (make-instance 'production :label "SANLab Start" :start-time 0.0 :end-time 0.0)))
(defparameter *end-node* nil)
(clear-buffer 'visual-location)
(clear-buffer 'visual)
(clear-buffer 'vocal)
(clear-buffer 'aural-location)
(clear-buffer 'aural)
(clear-buffer 'retrieval)
(clear-buffer 'manual)
(clear-buffer 'imaginal)
(clear-buffer 'goal)
(clear-buffer 'cogtool)
#|
(defmethod (setf dependents) :after ((val list) (obj buffer-action))
  (dolist (x val)
    (setf (depth x) (1+ (depth obj)))))

(defmethod (setf depth) :after ((val number) (obj buffer-action))
  (dolist (x (dependents obj))
    (if (depth x)
        (setf (depth x) (max (depth x) (1+ val)))
      (setf (depth x) (1+ val)))))
|#
(defmethod (setf dependents) :before (val (obj buffer-action))
  (if (and val (equal (first val) nil)) (break))
)

(defmethod before-reset-add-dependent ((watch symbol) (dep symbol))
  (push (list watch dep) *watches-before*))

(defmethod after-reset-add-as-dependent ((watch symbol) (parent buffer-action) &key (watch-index 0))
  (push (list (cons watch watch-index) parent) *watches-after*))

(defmethod action-after-set (action (buffer symbol))
  (if (and (symbolp action) (symbol-function action))
      (setf action (symbol-function action)))
  (if (functionp action)
      (push (list buffer action) *actions-after*)
    (error "Action ~A not of type symbol or function" action)))

(defmethod push-if-no-path ((action buffer-action) (target buffer-action) &key (start t) (clear nil))
  ;(if (and start (equal (buffer action) 'frame)) (break "frame"))
  (if (equal action target)
      (progn
        ;(if (equal (label target) "FIND-LOCATION-7") (break))
        t)
    (let ((result nil))
      (cond (clear
             (setf (visited action) nil)
             (dolist (depends (dependents action))
               (cond ((visited depends)
                      (setf (visited depends) nil)
                      (push-if-no-path depends target :start nil :clear t))))
             )
            (t
             (setf (visited action) t)
             (dolist (depends (dependents action))
               (cond ((not (visited depends))
                      (setf (visited depends) t)
                      (setf result (push-if-no-path depends target :start nil))
                      (if result (return result)))))
             (if (and start (not clear))
                 (push-if-no-path action target :start t :clear t))
             (if (and start (not result))
                 (push target (dependents action)))
             result)))))
       

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              ***      PROCEDURAL      ***                 
;;;  These functions handle actions through the procedural module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-motor-buffers ()
  (gethash 'manual *buffers*))

(defun get-prep-action (buffer)
  (cond ((not buffer) nil)
        ((eq (state buffer) 'free) nil)
        ((eq (state buffer) 'prep) buffer)
        (t (get-prep-action (first (prerequisites buffer))))))

(defun check-motor-module-state (sym)
  (format t "Checking motor state for ~A~%" sym)
  (cond ((equal sym 'state)
         (append (check-motor-module-state 'preparation)
                 (check-motor-module-state 'execution)
                 (check-motor-module-state 'processor))
         )
        ((equal sym 'preparation)
         (let ((preps nil))
           (maphash #'(lambda (k v) (declare (ignore k))
                        ;changed from 'prep to 'free
                        (let ((prep (get-prep-action v)))
                          (if prep (push prep preps)))
                        )
                    (get-motor-buffers))
           preps)
         )
        ((equal sym 'execution)
         (let ((execs nil))
           (maphash #'(lambda (k v) (declare (ignore k))
                        ;changed from 'exec to 'free
                        (if (and v (equal 'free (state v)))
                            (push v execs))
                        )
                    (get-motor-buffers))
           execs)
         )
        ((equal sym 'processor)
         (append (check-motor-module-state 'preparation)
                 (check-motor-module-state 'initiation))
         )
        ((equal sym 'initiation)
         (let ((inits nil))
           (maphash #'(lambda (k v) (declare (ignore k))
                        ;changed from 'init to 'free
                        (if (and v (equal 'init (state v)))
                            (push v inits))
                        )
                    (get-motor-buffers))
           inits)
         )
        ((equal sym 'last)
         (let ((result nil))
           (maphash #'(lambda (k v) (declare (ignore k))
                        (if (and v (equal 'free (state v)))
                            (if (and result (> (end-time v) (end-time result)))
                                (setf result v)
                              (if (not result)
                                  (setf result v)))))
                    (get-motor-buffers))
           (if result (list result)))
         )
))

(defmethod processed-trace-object :after ((time number) (module (eql 'procedural)) (action (eql 'production-selected)) &rest extras)
  (if (buffer-of-interest 'production)
      (progn
        (print-trace "Processing production-selected ~A~%" (first extras))
        (setf (next-production (buffer-of-interest 'production)) (make-instance 'production :start-time time :label (format nil "~A" (first extras))))
        (push (next-production (buffer-of-interest 'production)) (dependents (buffer-of-interest 'production)))
        (setf (buffer-of-interest 'production) (next-production (buffer-of-interest 'production)))
        (dolist (x (get-queries (first extras)))
          (cond ((and (equal (buffer x) 'manual) (equal (check-on x) 'state))
                 (let ((guardians (check-motor-module-state (if (equal (value x) 'free) 'last 'state))))
                   (print-trace "Found ~A guardians~%" (length guardians))
                   (dolist (guard guardians)
                     (push-if-no-path guard (buffer-of-interest 'production)))
                   )
                 )
                ((and (equal (buffer x) 'frame) (equal (check-on x) 'name))
                 (print-trace "Testing frame~%")
                 (cond ((and (buffer-of-interest 'frame) (equal (value x) (name (buffer-of-interest 'frame))))
                        (push-if-no-path (buffer-of-interest 'frame) (buffer-of-interest 'production)))))
                ((equal (buffer x) 'visual)
                 (print-trace "Testing visual state~%")
                 (cond ((equal (value x) 'free)
                        (if (buffer-of-interest 'visual-motor)
                            (push (buffer-of-interest 'production)
                                  (dependents (buffer-of-interest 'visual-motor)))))
                       ((equal (value x) 'busy)
                        (push (buffer-of-interest 'production)
                              (dependents (buffer-of-interest 'visual-motor)))
                        (let* ((obj (buffer-of-interest 'visual-motor))
                               (new-obj (make-instance 'buffer-action
                                                       :start-time time
                                                       :end-time (end-time obj)
                                                       :label (format nil "continue ~A" (label obj))
                                                       :operator-type (operator-type obj)
                                                       :action-type (action-type obj))))
                          (setf (end-time obj) time)
                          (push new-obj (dependents obj))
                          (setf (buffer-of-interest 'visual-motor) new-obj)))))
                ((equal (check-on x) 'state)
                 (print-trace "Testing state~%")
                 (cond ((equal (value x) 'free)
                        (if (buffer-of-interest (buffer x))
                            (push (buffer-of-interest 'production) (dependents (buffer-of-interest (buffer x))))))
                       ((equal (value x) 'busy)
                        (push (buffer-of-interest 'production) (dependents (buffer-of-interest (buffer x))))
                        (let* ((obj (buffer-of-interest (buffer x)))
                               (new-obj (make-instance 'buffer-action
                                                       :start-time time
                                                       :end-time (end-time obj)
                                                       :label (format nil "continue ~A" (label obj))
                                                       :operator-type (operator-type obj)
                                                       :action-type (action-type obj))))
                          (setf (end-time obj) time)
                          (push new-obj (dependents obj))
                          (setf (buffer-of-interest (buffer x)) new-obj)))
                       ((equal (value x) 'error)
                        (if (equal 'failure (action-type (buffer-of-interest (buffer x))))
                            (push (buffer-of-interest 'production) (dependents (buffer-of-interest (buffer x))))
                          (let ((event (find 'failure (gethash (buffer x) *history*) :key #'action-type)))
                            (push (buffer-of-interest 'production) (dependents event)))))))
                ((and (equal (buffer x) 'manual) (equal (check-on x) 'preparation))
                 (let ((guardians (check-motor-module-state 'preparation)))
                   (dolist (guard guardians)
                     (push-if-no-path guard (buffer-of-interest 'production)))
                   )
                 )
                ((and (equal (buffer x) 'manual) (equal (check-on x) 'execution))
                 (let ((guardians (check-motor-module-state 'execution)))
                   (dolist (guard guardians)
                     (push-if-no-path guard (buffer-of-interest 'production)))
                   )
                 )
                ((and (equal (buffer x) 'manual) (equal (check-on x) 'processor))
                 (let ((guardians (check-motor-module-state 'processor)))
                   (dolist (guard guardians)
                     (push-if-no-path guard (buffer-of-interest 'production)))
                   )
                 )
))
        (let ((str (string-downcase (label (buffer-of-interest 'production)))))
          (cond ((contains str "attend")
                 (setf (operator-type (buffer-of-interest 'production)) "Attend Operator"))
                ((contains str "verify")
                 (setf (operator-type (buffer-of-interest 'production)) "Verify Operator"))
                ((contains str "initiate")
                 (setf (operator-type (buffer-of-interest 'production)) "Initiate Operator"))
                (t
                 (setf (operator-type (buffer-of-interest 'production)) "Cognitive Operator"))
))))
)

(defmethod processed-trace-object :after ((time number) (module (eql 'procedural)) (action (eql 'production-fired)) &rest extras)
  (if (buffer-of-interest 'production)
      (progn
        (print-trace "Processing production-fired ~A~%" (first extras))
        (setf (end-time (buffer-of-interest 'production)) time)
        (let ((dependents (get-dependencies (first extras))))
          (dolist (dependent dependents)
            (dolist (lhs (lhs-buffers dependent))
              (let ((lhs (buffer-without-extra-chars lhs)))
                (dolist (rhs (rhs-buffers dependent))
                  (let ((rhs (buffer-without-extra-chars rhs))
                        (buf (buffer-of-interest lhs)))
                    (if buf
                        (action-after-set #'(lambda (x) (push x (dependents buf)))
                                          ; Override meaning of visual buffer on RHS to mean visual-motor buffer
                                          (if (eq rhs 'visual) 'visual-motor rhs)))))))))
))
  (if (gethash (first extras) *registered-events*)
      (destructuring-bind (module id) (gethash (first extras) *registered-events*)
        (cond ((equal module 'speech-module)
               (push id *vocal-events*))
              ((equal module 'motor-module)
               (push id *manual-events*)))))
;      (setf (gethash (list 'model (gethash (first extras) *registered-events*)) *system-events*) (buffer-of-interest 'production)))
)

(defmethod processed-trace-object :after ((time number) (module (eql 'procedural)) (action (eql 'buffer-read-action)) &rest extras)
  (if (buffer-of-interest (first extras))
      (progn
;        (if (equal (label (buffer-of-interest 'production)) "VERIFY-0+") (break))
;        (push (buffer-of-interest 'production) (dependents (buffer-of-interest (first extras))))
        (push-if-no-path (buffer-of-interest (first extras)) (buffer-of-interest 'production))
))
)

(defmethod processed-trace-object :after ((time number) (module (eql 'procedural)) (action (eql 'query-buffer-action)) &rest extras)
)

(defmethod processed-trace-object :after ((time number) (module (eql 'procedural)) (action (eql 'clear-buffer)) &rest extras)
)

(defmethod processed-trace-object :after ((time number) (module (eql 'procedural)) (action (eql 'module-request)) &rest extras)
  (cond ((equal (first extras) 'visual-location)
         (after-reset-add-as-dependent 'visual-location (buffer-of-interest 'production)))
        ((equal (first extras) 'vocal)
         (after-reset-add-as-dependent 'vocal (buffer-of-interest 'production)))
        ((equal (first extras) 'visual)
         (after-reset-add-as-dependent 'visual (buffer-of-interest 'production)))
        ((equal (first extras) 'imaginal)
         (setf (buffer-of-interest 'imaginal) (make-instance 'buffer-action :start-time time))
         (push (buffer-of-interest 'imaginal) (dependents (buffer-of-interest 'production))))
        ((equal (first extras) 'aural-location)
         (after-reset-add-as-dependent 'aural-location (buffer-of-interest 'production)))
        ((equal (first extras) 'aural)
         (after-reset-add-as-dependent 'aural (buffer-of-interest 'production)))
        ((equal (first extras) 'manual)
         (after-reset-add-as-dependent 'manual (buffer-of-interest 'production)))
        ((equal (first extras) 'goal)
         (setf (buffer-of-interest 'goal) (make-instance 'buffer-action :start-time time))
         (push (buffer-of-interest 'goal) (dependents (buffer-of-interest 'production)))))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              ***      VISION      ***                 
;;;  These functions handle actions through the vision module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *last-eye-movement* nil)
(defparameter *last-eye-prep* nil)

(defmethod processed-trace-object :after ((time number) (module (eql 'vision)) (action (eql 'set-buffer-chunk)) &rest extras)
  (cond ((equal (first extras) 'visual-location)
         (if (buffer-of-interest 'visual-location)
             (progn
               (print-trace "Ending visual-location chunk~%")
               (cond ((and (stringp (first (last extras))) (> (length (first (last extras))) 0))
                      (setf (label (buffer-of-interest 'visual-location)) (format nil "Find ~A" (first (last extras)))))
                     (t
                      (setf (label (buffer-of-interest 'visual-location)) (format nil "Find ~A" (second extras)))))
               (setf (end-time (buffer-of-interest 'visual-location)) time))
           (progn
             (print-trace "Found visual-location without previous Find-location~%")
             (setf (buffer-of-interest 'visual-location) (make-instance 'buffer-action))
             (setf (start-time (buffer-of-interest 'visual-location)) time)
             (setf (end-time (buffer-of-interest 'visual-location)) time)
             (setf (label (buffer-of-interest 'visual-location)) (format nil "~A" (second extras)))
             (if (= 0 (depth (buffer-of-interest 'production))) (push (buffer-of-interest 'visual-location) (dependents (buffer-of-interest 'production))))
)))))

(defmethod processed-trace-object :after ((time number) (module (eql 'vision)) (action (eql 'find-location)) &rest extras)
  (let ((last-act (buffer-of-interest 'visual-location)))
    (setf (buffer-of-interest 'visual-location) (make-instance 'buffer-action))
    (setf (start-time (buffer-of-interest 'visual-location)) time)
    (if (and (buffer-of-interest 'production) (= 0 (depth (buffer-of-interest 'production))))
        (push (buffer-of-interest 'visual-location) (dependents (buffer-of-interest 'production))))
    (if last-act
        (push (buffer-of-interest 'visual-location) (dependents last-act))))
)

(defmethod processed-trace-object :after ((time number) (module (eql 'vision)) (action (eql 'move-attention)) &rest extras)
  (let ((last-act (buffer-of-interest 'visual-motor)))
    (setf (buffer-of-interest 'visual-motor) (make-instance 'buffer-action :created-by-action action))
    (if last-act (push (buffer-of-interest 'visual-motor) (dependents last-act)))
    ; check for non-production causes
    (let ((system (or (buffer-of-interest 'system) (buffer-of-interest 'frame))))
      (cond ((and system (= (end-time system) time))
             (push system (prerequisites (buffer-of-interest 'visual-motor)))
             (push (buffer-of-interest 'visual-motor) (dependents system)))))
    (setf (operator-type (buffer-of-interest 'visual-motor)) "Eye Movement Operator")
    (setf (label (buffer-of-interest 'visual-motor)) (format nil "~{~A ~}" (remove-if-not 'symbolp extras)))
    (setf (start-time (buffer-of-interest 'visual-motor)) time)
    (if (and (buffer-of-interest 'production) (= 0 (depth (buffer-of-interest 'production))))
        (push (buffer-of-interest 'visual-motor) (dependents (buffer-of-interest 'production)))))
)

(defmethod processed-trace-object :after ((time number) (module (eql 'vision))
                                          (action (eql 'move-attention-attended-loc)) &rest extras)
  (let ((last-act (buffer-of-interest 'visual-motor)))
    (setf (buffer-of-interest 'visual-motor) (make-instance 'buffer-action :created-by-action 'move-attention))
    (if last-act (push (buffer-of-interest 'visual-motor) (dependents last-act)))
    ; check for non-production causes
    (let ((system (or (buffer-of-interest 'system) (buffer-of-interest 'frame))))
      (cond ((and system (= (end-time system) time))
             (push system (prerequisites (buffer-of-interest 'visual-motor)))
             (push (buffer-of-interest 'visual-motor) (dependents system)))))
    (setf (operator-type (buffer-of-interest 'visual-motor)) "Eye Movement Operator")
    (setf (label (buffer-of-interest 'visual-motor)) (format nil "~{~A ~}" (remove-if-not 'symbolp extras)))
    (setf (start-time (buffer-of-interest 'visual-motor)) time)
    (if (and (buffer-of-interest 'production) (= 0 (depth (buffer-of-interest 'production))))
        (push (buffer-of-interest 'visual-motor) (dependents (buffer-of-interest 'production)))))
  )

(defmethod processed-trace-object :after ((time number) (module (eql 'vision)) (action (eql 'prepare-eye-movement)) &rest extras)
  (multiple-value-bind (id extras) (strip-move-id extras)
    (declare (ignore id extras))
    (let ((last-act (buffer-of-interest 'visual-motor)))
      (cond ((and last-act (eq (created-by-action last-act) 'move-attention))
             (setf (label last-act) (format nil "prep - ~A" (label last-act)))
             )
            ((and last-act (eq (created-by-action last-act) 'complete-eye-movement))
             (setf (buffer-of-interest 'visual-motor) (make-instance 'buffer-action :created-by-action action))
             (setf (operator-type (buffer-of-interest 'visual-motor)) "Eye Movement Operator")
             (setf (label (buffer-of-interest 'visual-motor))
                   (format nil "prep~A" (subseq (label last-act) (min 4 (length (label last-act))))))
             (setf (start-time (buffer-of-interest 'visual-motor)) time)
             (push (buffer-of-interest 'visual-motor) (dependents last-act))
             )
            (t
             (error "Unknown condition"))))))

(defmethod processed-trace-object :after ((time number) (module (eql 'vision)) (action (eql 'preparation-complete)) &rest extras)
  (multiple-value-bind (id extras) (strip-move-id extras)
    (declare (ignore id extras))
    (let ((last-act (buffer-of-interest 'visual-motor)))
      (setf (buffer-of-interest 'visual-motor) (make-instance 'buffer-action :start-time time :operator-type "Eye Movement Operator"))
      (setf *last-eye-movement* (buffer-of-interest 'visual-motor))
      (cond (last-act
             (setf (end-time last-act) time)
             (setf (label (buffer-of-interest 'visual-motor)) (format nil "move~A" (subseq (label last-act) (min 4 (length (label last-act))))))
             (setf *last-eye-prep* last-act)
             (push (buffer-of-interest 'visual-motor) (dependents last-act)))))))

(defmethod processed-trace-object :after ((time number) (module (eql 'vision)) (action (eql 'complete-eye-movement)) &rest extras)
  (multiple-value-bind (id extras) (strip-move-id extras)
    (declare (ignore id extras))
    (let ((last-act (buffer-of-interest 'visual-motor)))
      (cond (last-act
             (setf (created-by-action last-act) action)
             (setf *last-eye-movement* last-act)
             (setf (end-time last-act) time)))
      )))

(defmethod processed-trace-object :after ((time number) (module (eql 'vision)) (action (eql 'encoding-complete)) &rest extras)
  (cond (; check for EMMA eye movements
         *last-eye-prep*
         (let ((obj (make-instance 'buffer-action :start-time (end-time *last-eye-prep*) :end-time time :operator-type "Perceptual Operator (Visual)" :label (format nil "Perceive ~{~A ~}" extras))))
           (if (buffer-of-interest 'visual) (push obj (dependents (buffer-of-interest 'visual))))
           (cond ((buffer-of-interest 'frame)
                  (push obj (dependents (buffer-of-interest 'frame)))
                  ))
           (push obj (dependents *last-eye-prep*))
           (push *last-eye-prep* (prerequisites obj))
           (setf (buffer-of-interest 'visual) obj)))
        (; potentially traditional ACT-R vision module
         (buffer-of-interest 'visual-motor)
         (setf (end-time (buffer-of-interest 'visual-motor)) time)
         (let ((last-act (buffer-of-interest 'visual))
               (obj (make-instance 'buffer-action :start-time time :end-time time :operator-type "Perceptual Operator (Visual)"
                                   :label (format nil "Perceive ~A" (first extras)))))
           (if last-act (push obj (dependents last-act)))
           (push obj (dependents (buffer-of-interest 'visual-motor)))
           (if (buffer-of-interest 'frame) (push obj (dependents (buffer-of-interest 'frame))))
           (setf (buffer-of-interest 'visual) obj)))
           
        ))

#|
(defmethod processed-trace-object :after ((time number) (module (eql 'vision)) (action (eql 'encoding-complete)) &rest extras)
  (if (buffer-of-interest 'visual)
      (cond ((equal (action-type (buffer-of-interest 'visual)) 'failure)
             (let ((display (find-last-system-event-of-type 'display-event)))
               (if display
                   (let ((obj (make-instance 'buffer-action :start-time (start-time display) :end-time time :operator-type "Perceptual Operator (Visual)" :label (format nil "Perceive ~A" (first extras))))
                         newobj)
                     (push obj (dependents (buffer-of-interest 'visual)))
                     (push obj (dependents display))
                     (setf new-obj (make-instance 'system-event
                                                  :start-time (start-time display) 
                                                  :end-time (end-time display)
                                                  :operator-type "System Resource"
                                                  :label (format nil "continue ~A" (label display))
                                                  :event-type (event-type display)
                                                  :event-key (event-key display)))
                     (push new-obj (dependents display))
                     (setf (end-time display) (start-time display))
                     (setf (gethash (event-key new-obj) *system-events*) new-obj)
                     (setf (buffer-of-interest 'visual) obj)))))
            (t
             (if (not (end-time (buffer-of-interest 'visual))) (setf (end-time (buffer-of-interest 'visual)) time))
             (setf (label (buffer-of-interest 'visual)) (format nil "~A" (first extras)))
             (let ((obj (find-last-system-event-of-type 'display-event)))
               (cond ((and obj (< (start-time (buffer-of-interest 'visual)) (end-time obj)))
                      (let ((new-obj (make-instance 'system-event
                                                    :start-time (start-time (buffer-of-interest 'visual))
                                                    :end-time (end-time obj)
                                                    :operator-type "System Resource"
                                                    :label (format nil "continue ~A" (label obj))
                                                    :event-type (event-type obj)
                                                    :event-key (event-key obj))))
                        (push new-obj (dependents obj))
                        (setf (end-time obj) (start-time new-obj))
                        (setf (gethash (event-key new-obj) *system-events*) new-obj)
                        (push (buffer-of-interest 'visual) (dependents obj))))
                     (t
                      (if obj (push (buffer-of-interest 'visual) (dependents obj))))))
             (let ((obj (make-instance 'buffer-action)))
               (push obj (dependents (buffer-of-interest 'visual)))
               (setf (buffer-of-interest 'visual) obj)
               (setf (start-time obj) time (end-time obj) time (operator-type obj) "Perceptual Operator (Visual)")
               (setf (label obj) (format nil "~A" (first extras)))))
))
)
|#

(defmethod processed-trace-object :after ((time number) (module (eql 'vision)) (action (eql 'no)) &rest extras)
  (if (buffer-of-interest 'visual)
      (progn
        (if (buffer-of-interest 'frame) 
            (setf (dependents (buffer-of-interest 'frame))
                  (remove (buffer-of-interest 'visual) (dependents (buffer-of-interest 'frame)))))
        (setf (label (buffer-of-interest 'visual)) "Perceive Blank")
        (setf (action-type (buffer-of-interest 'visual)) 'failure))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              ***      AUDIO      ***                 
;;;  These functions handle actions through the audio module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod processed-trace-object :after ((time number) (module (eql 'audio)) (action (eql 'set-buffer-chunk)) &rest extras)
  (cond ((equal (first extras) 'aural-location)
         (if (equal (last extras) '(nil))
             (progn
               (print-trace "Found aural-location without previous Find-sound~%")
               (setf (buffer-of-interest 'aural-location) (make-instance 'buffer-action))
               (let ((obj (find-last-system-event-of-types '(name-event ring-event speech-event tone-event digit-event))))
                 (if obj (push (buffer-of-interest 'aural-location) (dependents obj)))
                 (setf (start-time (buffer-of-interest 'aural-location))
                       (cond ((null obj) time)
                             ((= (- (end-time obj) (start-time obj)) 0) (start-time obj))
                             ((< (end-time obj) time) (end-time obj))
                             (t
                              (let ((new-obj (make-instance 'system-event :start-time time :end-time (end-time obj) :label (label obj) :event-type (event-type obj) :event-key (event-key obj) :operator-type "System Resource")))
                                (setf (end-time obj) time)
                                (setf (label obj) (format nil "start ~A" (label obj)))
                                (push new-obj (dependents obj))
                                (setf (gethash (event-key new-obj) *system-events*) new-obj)
                                time
                                )))))
               (setf (end-time (buffer-of-interest 'aural-location)) time)
               (setf (label (buffer-of-interest 'aural-location)) (format nil "hear ~A" (second extras)))
               (if (= 0 (depth (buffer-of-interest 'production))) (push (buffer-of-interest 'aural-location) (dependents (buffer-of-interest 'production)))))
           (if (buffer-of-interest 'aural-location)
               (progn
                 (print-trace "Ending aural-location chunk~%")
                 (let ((obj (find-last-system-event-of-types '(name-event ring-event speech-event tone-event digit-event))))
                   (if obj (push (buffer-of-interest 'aural-location) (dependents obj))))
                 (setf (end-time (buffer-of-interest 'aural-location)) time))
#|             (progn
               (print-trace "Found aural-location without previous Find-sound~%")
               (setf (buffer-of-interest 'aural-location) (make-instance 'buffer-action))
               (let ((obj (find-last-system-event-of-types '(name-event ring-event speech-event tone-event digit-event) :lookup #'start-time)))
                 (if obj (push (buffer-of-interest 'aural-location) (dependents obj)))
                 (setf (start-time (buffer-of-interest 'aural-location))
                       (let ((obj (find-last-system-event-of-types '(name-event ring-event speech-event tone-event digit-event))))
                         (cond ((null obj) time)
                               ((= (- (end-time obj) (start-time obj)) 0) (start-time obj))
                               ((< (end-time obj) time) (end-time obj))
                               (t
                                (let ((new-obj (make-instance 'system-event :start-time time :end-time (end-time obj) :label (label obj) :event-type (event-type obj) :event-key (event-key obj) :operator-type "System Resource")))
                                  (setf (end-time obj) time)
                                  (setf (label obj) (format nil "start ~A" (label obj)))
                                  (push (buffer-of-interest 'aural-location) (dependents obj))
                                  (push new-obj (dependents obj))
                                  (setf (gethash (event-key new-obj) *system-events*) new-obj)
                                  time
                                  ))))))
               (setf (end-time (buffer-of-interest 'aural-location)) time)
               (setf (label (buffer-of-interest 'aural-location)) (format nil "hear ~A" (second extras)))
               (if (= 0 (depth (buffer-of-interest 'production))) (push (buffer-of-interest 'aural-location) (dependents (buffer-of-interest 'production)))))
|#
))
)
        ((equal (first extras) 'aural))
        (t (print-warning "Unknown buffer ~A encountered in audio module~%" (first extras))))
)

(defmethod processed-trace-object :after ((time number) (module (eql 'audio)) (action (eql 'create-new-buffer-chunk)) &rest extras)
)

(defmethod processed-trace-object :after ((time number) (module (eql 'audio)) (action (eql 'attend-sound)) &rest extras)
  (let ((obj (setf (buffer-of-interest 'aural) (make-instance 'buffer-action))))
    (setf (start-time obj) time)
    (setf (label obj) (format nil "interpret ~A" (first extras))))
)

(defmethod processed-trace-object :after ((time number) (module (eql 'audio)) (action (eql 'audio-encoding-complete)) &rest extras)
  (if (buffer-of-interest 'aural)
      (setf (end-time (buffer-of-interest 'aural)) time))
)

(defmethod processed-trace-object :after ((time number) (module (eql 'audio)) (action (eql 'find-sound)) &rest extras)
  (let ((obj (setf (buffer-of-interest 'aural-location) (make-instance 'buffer-action))))
    (setf (start-time obj) time)
    (setf (label obj) (format nil "Attend Auditory")))
)

(defmethod processed-trace-object :after ((time number) (module (eql 'audio)) (action (eql 'find-sound-failure)) &rest extras)
  (if (buffer-of-interest 'aural-location)
      (progn
        (print-trace "Ending aural-location chunk~%")
        (if (end-time (buffer-of-interest 'aural-location))
            (progn
              (print-trace "Found aural-location without previous Find-sound~%")
              (let ((obj (setf (buffer-of-interest 'aural-location) (make-instance 'buffer-action))))
                (setf (start-time obj) time)
                (setf (end-time obj) time)
                (setf (label obj) "Perceive Silence")
                (if (= 0 (depth (buffer-of-interest 'production))) (push obj (dependents (buffer-of-interest 'production))))))
          (progn
            (setf (end-time (buffer-of-interest 'aural-location)) time)
            (setf (label (buffer-of-interest 'aural-location)) "Perceive Silence"))))
    (progn
      (print-trace "Found aural-location without previous Find-sound~%")
      (let ((obj (setf (buffer-of-interest 'aural-location) (make-instance 'buffer-action))))
        (setf (start-time obj) time)
        (setf (end-time obj) time)
        (setf (label obj) "Perceive Silence")
        (if (= 0 (depth (buffer-of-interest 'production))) (push obj (dependents (buffer-of-interest 'production)))))))
  (if (buffer-of-interest 'aural-location)
      (setf (action-type (buffer-of-interest 'aural-location)) 'failure))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              ***      SPEECH      ***                 
;;;  These functions handle actions through the speech module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod processed-trace-object :after ((time number) (module (eql 'speech)) (action (eql 'subvocalize)) &rest extras)
  (let ((obj (make-instance 'buffer-action :label (format nil "prep - \"~{~A ~}\"" extras) :start-time time)))
    (if (buffer-of-interest 'vocal)
        (push obj (dependents (buffer-of-interest 'vocal))))
    (setf (buffer-of-interest 'vocal) obj)
)
)

(defmethod processed-trace-object :after ((time number) (module (eql 'speech)) (action (eql 'speak)) &rest extras)
  (let ((obj (make-instance 'buffer-action :label (format nil "prep - \"~{~A ~}\"" (cdr extras)) :start-time time)))
    (if (buffer-of-interest 'vocal)
        (push obj (dependents (buffer-of-interest 'vocal))))
    (setf (buffer-of-interest 'vocal) obj)
))

(defmethod processed-trace-object :after ((time number) (module (eql 'speech)) (action (eql 'preparation-complete)) &rest extras)
  (let ((last-act (buffer-of-interest 'vocal)))
    (setf (end-time last-act) time)
    (setf (buffer-of-interest 'vocal) (make-instance 'buffer-action :label (format nil "init~A" (subseq (label last-act) 4)) :start-time time))
    (push (buffer-of-interest 'vocal) (dependents last-act))
))

(defmethod processed-trace-object :after ((time number) (module (eql 'speech)) (action (eql 'initiation-complete)) &rest extras)
  (let ((last-act (buffer-of-interest 'vocal)))
    (setf (end-time last-act) time)
    (setf (buffer-of-interest 'vocal) (make-instance 'buffer-action :label (format nil "exec~A" (subseq (label last-act) 4)) :start-time time))
    (push (buffer-of-interest 'vocal) (dependents last-act))))

(defmethod processed-trace-object :after ((time number) (module (eql 'speech)) (action (eql 'finish-movement)) &rest extras)
  (setf (end-time (buffer-of-interest 'vocal)) time)
  (dolist (event *vocal-events*)
    (setf (gethash (list 'model event) *system-events*) (buffer-of-interest 'vocal)))
  (setf *vocal-events* nil)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              ***      MOTOR      ***                 
;;;  These functions handle actions through the motor module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun remove-at (pos l)
  (cond ((= pos 0)
         (cdr l))
        (t
         (do ((x l (cdr x))
              (y pos (1- y)))
             ((= y 1) (setf (cdr x) (cddr x))))
         l)))

(defun strip-move-id (l)
  (let ((pos (position 'move-id l)))
    (if pos
        (let ((copy (copy-list l))
              val)
          (setf val (nth (1+ pos) copy))
          (setf copy (remove-at pos (remove-at pos copy)))
          (values val copy))
      (values 0 l))))

(defun last-handed-action (action)
  (let (result)
    (do ((x (1- (move-id action)) (1- x)))
        ((<= x 0) result)
;      (print-trace "x = ~A~%" x)
      (let ((buffer (buffer-of-interest 'manual :index x)))
        (cond ((and buffer (equalp (operator-type action) (operator-type buffer)))
               (if (equalp (state buffer) 'free)
                   (and (setf result buffer) (return result))
                 (progn (do ((y (buffer-of-interest 'manual :index x) (if (prerequisites y) (first (prerequisites y)) nil)))
                            ((null y) result)
                          (if (prep-only y) (and (setf result y) (return result)))
                          (if (equalp (state y) (state action))
                              (and (setf result y)
                                   (return result))))
                   (return result))))
              (buffer
;               (print-trace "buffer, but not the same operator type~%")
               )
              (t
;               (print-trace "buffer is nil~%")
               ))))))

(let ((hand nil))
  (defun start-press-key (h) (setf hand h))
  (defun get-press-key-hand () hand)
  (defun reset-press-key () (setf hand nil)))

(defmethod processed-trace-object :after ((time number) (module (eql 'motor)) (action (eql 'start-press-key)) &rest extras)
  (start-press-key (first extras)))

(defmethod processed-trace-object :after ((time number) (module (eql 'motor)) (action (eql 'press-key)) &rest extras)
  (multiple-value-bind (id extras) (strip-move-id extras)
    (set-last-processed-motor action id)
    (let ((obj (setf (buffer-of-interest 'manual :index id) (make-instance 'manual-buffer-action
                                                                           :label (format nil "prep - press ~A" (first extras))
                                                                           :start-time time
                                                                           :state 'prep
                                                                           :operator-type (cond ((equal 'left (get-press-key-hand)) "Left Hand Operator")
                                                                                                ((equal 'right (get-press-key-hand)) "Right Hand Operator")
                                                                                                (t "Motor Operator"))
                                                                           :move-id id)))
          previous)
      (push obj (dependents (buffer-of-interest 'production)))
      (setf previous (last-handed-action obj))
      (if previous (push obj (dependents previous)))
))
)

(defmethod processed-trace-object :after ((time number) (module (eql 'motor)) (action (eql 'punch)) &rest extras)
  (multiple-value-bind (id extras) (strip-move-id extras)
    (set-last-processed-motor action id)
    (let ((obj (setf (buffer-of-interest 'manual :index id) (make-instance 'manual-buffer-action
                                                                 :label (format nil "prep - punch ~A ~A" (second extras) (fourth extras))
                                                                 :start-time time
                                                                 :state 'prep
                                                                 :move-id id
                                                                 :operator-type (cond ((member 'left extras) "Left Hand Operator")
                                                                                      ((member 'right extras) "Right Hand Operator")
                                                                                      (t "Motor Operator")))))
          previous)
      (push obj (dependents (buffer-of-interest 'production)))
      (setf previous (last-handed-action obj))
      (if previous (push obj (dependents previous)))
))
)

(defmethod processed-trace-object :after ((time number) (module (eql 'motor)) (action (eql 'point-hand-at-key)) &rest extras)
  (multiple-value-bind (id extras) (strip-move-id extras)
    (set-last-processed-motor action id)
    (let ((obj (setf (buffer-of-interest 'manual :index id) (make-instance 'manual-buffer-action
                                                                           :label (format nil "prep - point-at ~A" (fourth extras))
                                                                           :start-time time
                                                                           :state 'prep
                                                                           :move-id id
                                                                           :operator-type (cond ((member 'left extras) "Left Hand Operator")
                                                                                      ((member 'right extras) "Right Hand Operator")
                                                                                      (t "Motor Operator")))))
          previous)
      (push obj (dependents (buffer-of-interest 'production)))
      (setf previous (last-handed-action obj))
      (if previous (push obj (dependents previous)))
))
)

(let ((last-mouse-move nil))
  (defmethod set-last-mouse-move ((move manual-buffer-action))
    (setf last-mouse-move move))
  (defun get-last-mouse-move () last-mouse-move))

(defmethod processed-trace-object :after ((time number) (module (eql 'motor)) (action (eql 'move-cursor)) &rest extras)
  (multiple-value-bind (id extras) (strip-move-id extras)
    (set-last-processed-motor action id)
    (let ((obj (setf (buffer-of-interest 'manual :index id) (make-instance 'manual-buffer-action
                                                                 :label (format nil "prep - move cursor to ~A" (first (last extras 2)))
                                                                 :start-time time
                                                                 :state 'prep
                                                                 :move-id id
                                                                 :operator-type "Right Hand Operator")))
          previous)
      (set-last-mouse-move obj)
      (push obj (dependents (buffer-of-interest 'production)))
      (setf previous (last-handed-action obj))
      (if previous (push obj (dependents previous)))
))
)

(let ((last-processed-motor nil) (move-id nil))
  (defun set-last-processed-motor (x y) (setf last-processed-motor x) (setf move-id y))
  (defun get-last-processed-motor () last-processed-motor)
  (defun get-last-processed-move-id () move-id)
  (defun reset-last-processed-motor () (setf last-processed-motor nil) (setf move-id nil)))

#|
(defmethod processed-trace-object :after ((time number) (module (eql 'motor)) action &rest extras)
  (if (not (equal action (get-last-processed-motor)))
      (error "Unhandled motor command ~A" action)))
|#

(defmethod processed-trace-object :after ((time number) (module (eql 'motor)) (action (eql 'click-mouse)) &rest extras)
  (multiple-value-bind (id extras) (strip-move-id extras)
    (declare (ignore extras))
    (set-last-processed-motor action id)
    (let ((obj (setf (buffer-of-interest 'manual :index id) (make-instance 'manual-buffer-action
                                                                           :label (format nil "prep - click mouse")
                                                                           :start-time time
                                                                           :state 'prep
                                                                           :move-id id
                                                                           :operator-type "Right Hand Operator")))
          previous)
      (push obj (dependents (buffer-of-interest 'production)))
      (setf previous (last-handed-action obj))
      (if previous (push obj (dependents previous)))
)))

(defmethod processed-trace-object :after ((time number) (module (eql 'motor)) (action (eql 'hand-to-mouse)) &rest extras)
  (multiple-value-bind (id extras) (strip-move-id extras)
    (declare (ignore extras))
    (set-last-processed-motor action id)
    (let ((obj (setf (buffer-of-interest 'manual :index id) (make-instance 'manual-buffer-action
                                                                 :label (format nil "prep - hand to mouse")
                                                                 :start-time time
                                                                 :state 'prep
                                                                 :move-id id
                                                                 :operator-type "Right Hand Operator")))
          previous)
      (push obj (dependents (buffer-of-interest 'production)))
      (setf previous (last-handed-action obj))
      (if previous (push obj (dependents previous)))
)))

(defmethod processed-trace-object :after ((time number) (module (eql 'motor)) (action (eql 'hand-to-home)) &rest extras)
  (multiple-value-bind (id extras) (strip-move-id extras)
    (set-last-processed-motor action id)
    (let ((obj (setf (buffer-of-interest 'manual :index id) (make-instance 'manual-buffer-action
                                                                           :label (format nil "prep - hand to home")
                                                                           :start-time time
                                                                           :state 'prep
                                                                           :move-id id
                                                                           :operator-type (cond ((member 'right extras) "Right Hand Operator")
                                                                                                ((member 'left extras) "Left Hand Operator")
                                                                                                (t "Motor Operator")))))
          previous)
      (push obj (dependents (buffer-of-interest 'production)))
      (setf previous (last-handed-action obj))
      (if previous (push obj (dependents previous)))
)))

(defmethod processed-trace-object :after ((time number) (module (eql 'motor)) (action (eql 'preparation-complete)) &rest extras)
  (multiple-value-bind (id extras) (strip-move-id extras)
    (declare (ignore extras))
    (set-last-processed-motor action id)
    (let ((last-act (buffer-of-interest 'manual :index id)))
      (setf (end-time last-act) time)
      (cond ((not (prep-only last-act))
             (setf (buffer-of-interest 'manual :index id) (make-instance 'manual-buffer-action
                                                                         :label (format nil "init~A" (subseq (label last-act) 4))
                                                                         :start-time time
                                                                         :operator-type (operator-type (buffer-of-interest 'manual :index id))
                                                                         :state 'init
                                                                         :move-id id))
             (push (buffer-of-interest 'manual :index id) (dependents last-act))
             (push last-act (prerequisites (buffer-of-interest 'manual :index id))))))))

(defmethod processed-trace-object :after ((time number) (module (eql 'motor)) (action (eql 'initiation-complete)) &rest extras)
  (multiple-value-bind (id extras) (strip-move-id extras)
    (declare (ignore extras))
    (set-last-processed-motor action id)
    (let* ((last-act (buffer-of-interest 'manual :index id))
           (test (last-handed-action last-act)))
      (cond (test
             ;(break (format nil "~A" (label last-act)))
             (push last-act (dependents test))
             (setf (start-time last-act) (max (start-time last-act) (end-time test)))))
      (setf (end-time last-act) time)
      (setf (buffer-of-interest 'manual :index id) (make-instance 'manual-buffer-action
                                                                  :label (format nil "exec~A" (subseq (label last-act) 4))
                                                                  :start-time time
                                                                  :operator-type (operator-type (buffer-of-interest 'manual :index id))
                                                                  :state 'exec
                                                                  :move-id id))
      (push (buffer-of-interest 'manual :index id) (dependents last-act))
      (push last-act (prerequisites (buffer-of-interest 'manual :index id))))))

(defmethod processed-trace-object :after ((time number) (module (eql 'motor)) (action (eql 'prepare)) &rest extras)
  (multiple-value-bind (id extras) (strip-move-id extras)
    (set-last-processed-motor action id)
    (let ((obj (setf (buffer-of-interest 'manual :index id) (make-instance 'manual-buffer-action
                                                                           :label (format nil "prep -~{ ~A~}" extras)
                                                                           :start-time time
                                                                           :state 'prep
                                                                           :move-id id
                                                                           :prep-only t
                                                                           :operator-type (cond ((member 'right extras) "Right Hand Operator")
                                                                                                ((member 'left extras) "Left Hand Operator")
                                                                                                (t "Motor Operator")))))
          previous)
      (push obj (dependents (buffer-of-interest 'production)))
      (setf previous (last-handed-action obj))
      (if previous (push obj (dependents previous)))
)))

(defmethod processed-trace-object :after ((time number) (module (eql 'motor)) (action (eql 'move-cursor-absolute)) &rest extras)
  (multiple-value-bind (id extras) (values (move-id (get-last-mouse-move)) extras)
    (set-last-processed-motor action id)
    (let ((last-act (buffer-of-interest 'manual :index id)))
      (setf (end-time last-act) time)
      (setf (buffer-of-interest 'manual :index id) (make-instance 'manual-buffer-action
                                                                  :label (format nil "finish~A" (subseq (label last-act) (position #\Space (label last-act))))
                                                                  :start-time time
                                                                  :state 'exec
                                                                  :move-id id
                                                                  :operator-type "Right Hand Operator"))
      (push (buffer-of-interest 'manual :index id) (dependents last-act))
      (push last-act (prerequisites (buffer-of-interest 'manual :index id))))))

(defmethod processed-trace-object :after ((time number) (module (eql 'motor)) (action (eql 'output-key)) &rest extras)
  (multiple-value-bind (id extras) (strip-move-id extras)
    (set-last-processed-motor action id)
    (let ((last-act (buffer-of-interest 'manual :index id)))
      (setf (end-time last-act) time)
      (setf (buffer-of-interest 'manual :index id) (make-instance 'manual-buffer-action
                                                                  :label (format nil "release~A" (subseq (label last-act) 4))
                                                                  :start-time time
                                                                  :key (first extras)
                                                                  :operator-type (operator-type (buffer-of-interest 'manual :index id))
                                                                  :state 'exec
                                                                  :move-id id))
      (push (buffer-of-interest 'manual :index id) (dependents last-act))
      (push last-act (prerequisites (buffer-of-interest 'manual :index id)))
      (dolist (event *manual-events*)
        (setf (gethash (list 'model event) *system-events*) last-act))
      (setf *manual-events* nil)
)))

(defmethod processed-trace-object :after ((time number) (module (eql 'motor)) (action (eql 'move-a-hand)) &rest extras)
  (multiple-value-bind (id extras) (strip-move-id extras)
    (set-last-processed-motor action id)
    (let ((last-act (buffer-of-interest 'manual :index id)))
      (setf (end-time last-act) time)
      (setf (buffer-of-interest 'manual :index id) (make-instance 'manual-buffer-action
                                                        :label (format nil "move~A" (subseq (label last-act) 4))
                                                        :state 'prep
                                                        :move-id id
                                                        :start-time time
                                                        :operator-type (cond ((member 'left extras) "Left Hand Operator")
                                                                             ((member 'right extras) "Right Hand Operator")
                                                                             (t "Motor Operator"))))
      (push (buffer-of-interest 'manual :index id) (dependents last-act)))))

(defmethod processed-trace-object :after ((time number) (module (eql 'motor)) (action (eql 'finish-movement)) &rest extras)
  (multiple-value-bind (id extras) (strip-move-id extras)
    (declare (ignore extras))
    (set-last-processed-motor action id)
    (setf (end-time (buffer-of-interest 'manual :index id)) time)
    (setf (state (buffer-of-interest 'manual :index id)) 'free)
))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              ***      DECLARATIVE      ***                 
;;;  These functions handle actions through the memory module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod processed-trace-object :after ((time number) (module (eql 'declarative)) (action (eql 'start-retrieval)) &rest extras)
  (setf (buffer-of-interest 'retrieval) (make-instance 'buffer-action :label "Retrieval" :start-time time))
  (push (buffer-of-interest 'retrieval) (dependents (buffer-of-interest 'production))))

(defmethod processed-trace-object :after ((time number) (module (eql 'declarative)) (action (eql 'retrieval-failure)) &rest extras)
  (setf (end-time (buffer-of-interest 'retrieval)) time)
  (setf (label (buffer-of-interest 'retrieval)) "Retrieval failure"))

(defmethod processed-trace-object :after ((time number) (module (eql 'declarative)) (action (eql 'retrieved-chunk)) &rest extras)
  (setf (end-time (buffer-of-interest 'retrieval)) time)
  (setf (label (buffer-of-interest 'retrieval)) (format nil "Retrieved ~A" (first extras))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              ***      IMAGINAL      ***                 
;;;  These functions handle actions through the imaginal module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod processed-trace-object :after ((time number) (module (eql 'imaginal)) (action (eql 'create-new-buffer-chunk)) &rest extras)
  (if (buffer-of-interest 'imaginal)
      (progn
        (setf (end-time (buffer-of-interest 'imaginal)) time)
        (setf (label (buffer-of-interest 'imaginal)) (format nil "~{~A ~}" extras)))
    (progn
      (setf (buffer-of-interest 'imaginal) (make-instance 'buffer-action))
      (push (buffer-of-interest 'imaginal) (dependents (buffer-of-interest 'production)))
      (setf (start-time (buffer-of-interest 'imaginal)) time)
      (setf (end-time (buffer-of-interest 'imaginal)) time)
      (setf (label (buffer-of-interest 'imaginal)) (format nil "~{~A ~}" extras))))
)

(defmethod processed-trace-object :after ((time number) (module (eql 'imaginal)) (action (eql 'set-buffer-chunk)) &rest extras)
  (let ((obj (make-instance 'buffer-action :start-time time :end-time time :label (format nil "~{~A ~}" extras))))
    (if (buffer-of-interest 'imaginal) (push obj (dependents (buffer-of-interest 'imaginal))))
    (setf (buffer-of-interest 'imaginal) obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              ***      GOAL      ***                 
;;;  These functions handle actions through the goal module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod processed-trace-object :after ((time number) (module (eql 'goal)) (action (eql 'create-new-buffer-chunk)) &rest extras)
  (if (buffer-of-interest 'goal)
      (progn
        (setf (end-time (buffer-of-interest 'goal)) time)
        (setf (label (buffer-of-interest 'goal)) (format nil "~{~A ~}" extras)))
    (progn
      (setf (buffer-of-interest 'goal) (make-instance 'buffer-action))
      (push (buffer-of-interest 'goal) (dependents (buffer-of-interest 'production)))
      (setf (start-time (buffer-of-interest 'goal)) time)
      (setf (end-time (buffer-of-interest 'goal)) time)
      (setf (label (buffer-of-interest 'goal)) (format nil "~{~A ~}" extras))))
)

(defmethod processed-trace-object :after ((time number) (module (eql 'goal)) (action (eql 'set-buffer-chunk)) &rest extras)
  (let ((obj (make-instance 'buffer-action :start-time time :end-time time :label (format nil "~{~A ~}" extras))))
    (if (buffer-of-interest 'goal) (push obj (dependents (buffer-of-interest 'goal))))
    (if (null (buffer-of-interest 'goal)) (push obj (dependents *start-node*)))
    (setf (buffer-of-interest 'goal) obj)
))

(defmethod processed-trace-object (time module action &rest extras)
  (format t "~A ~A ~A ~A~%" time module action extras))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              ***      COGTOOL      ***                 
;;;  These functions handle Cogtool events recorded in the trace
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass frame-buffer-action (buffer-action)
  ((name :initform nil :initarg :name :accessor name)))

(defmethod processed-trace-object :after ((time number) (module (eql 'cogtool)) (action (eql 'transition-to)) &rest extras)
  (let ((last-act (buffer-of-interest 'cogtool))
        (last-frame (buffer-of-interest 'frame)))
    (setf (buffer-of-interest 'frame) (make-instance 'frame-buffer-action
                                                     :start-time time
                                                     :end-time time
                                                     :name (first extras)
                                                     :label (format nil "Transition to ~{~A ~}" extras)))
    (if last-act
        (push (buffer-of-interest 'frame) (dependents last-act)))
    (if last-frame
        (push (buffer-of-interest 'frame) (dependents last-frame)))
    (if (and (not last-act) (not last-frame))
        (push (buffer-of-interest 'frame) (dependents *start-node*)))
))

(defmethod processed-trace-object :after ((time number) (module (eql 'cogtool)) (action (eql 'start-system-wait)) &rest extras)
  (let ((last-act (buffer-of-interest 'frame))
        (duration (first extras)))
    (assert (numberp duration) (duration))
    (if last-act
        (setf (end-time last-act) (+ (start-time last-act) duration))
      (setf (buffer-of-interest 'frame)
            (make-instance 'frame-buffer-action
                           :start-time time
                           :end-time (+ time duration)
                           :name (second extras)
                           :label (second extras))))))

(defmethod processed-trace-object :after ((time number) (module (eql 'cogtool)) (action (eql 'device-move-cursor-to)) &rest extras)
  (let ((last-act (buffer-of-interest 'cogtool)))
    (setf (buffer-of-interest 'cogtool) (make-instance 'buffer-action
                                                       :start-time time
                                                       :end-time time
                                                       :label (format nil "Cursor to (~{~A ~})" extras)))
    (if last-act
        (push (buffer-of-interest 'cogtool) (dependents last-act)))
    (let ((manual (buffer-of-interest 'manual :index (get-last-processed-move-id))))
      (if manual (push (buffer-of-interest 'cogtool) (dependents (first (prerequisites manual))))))))

(defmethod processed-trace-object :after ((time number) (module (eql 'cogtool)) (action (eql 'device-click)) &rest extras)
  (let ((last-act (buffer-of-interest 'cogtool)))
    (setf (buffer-of-interest 'cogtool) (make-instance 'buffer-action
                                                       :start-time time
                                                       :end-time time
                                                       :label "Mouse Click"))
    (if last-act
        (push (buffer-of-interest 'cogtool) (dependents last-act)))
    (let ((manual (buffer-of-interest 'manual :index (get-last-processed-move-id))))
      (if manual (push (buffer-of-interest 'cogtool) (dependents (first (prerequisites manual))))))))

(defmethod processed-trace-object :after ((time number) (module (eql 'cogtool)) (action (eql 'device-press-key)) &rest extras)
  (let ((last-act (buffer-of-interest 'cogtool)))
    (setf (buffer-of-interest 'cogtool) (make-instance 'buffer-action
                                                       :start-time time
                                                       :end-time time
                                                       :label (format nil "Pressed key ~A" (subseq (format nil "~W"
                                                                                                           (first extras)
                                                                                                           ) 2))))
    (if last-act
        (push (buffer-of-interest 'cogtool) (dependents last-act)))
    (let ((manual (buffer-of-interest 'manual :index (get-last-processed-move-id))))
      (if manual (push (buffer-of-interest 'cogtool) (dependents (first (prerequisites manual))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              ***      SYSTEM      ***                 
;;;  These functions handle system events recorded in the trace
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod find-last-system-event-of-type (type &key (lookup #'end-time))
  (let (last)
    (maphash #'(lambda (k v)
                 (declare (ignore k))
                 (if (equal (event-type v) type)
                     (if last
                         (if (< (funcall lookup last) (funcall lookup v))
                             (setf last v))
                       (setf last v))))
             *system-events*)
    last))

(defmethod find-last-system-event-of-types (types &key (lookup #'end-time))
  (let ((objs (mapcar #'(lambda (x) (find-last-system-event-of-type x :lookup lookup)) types)))
    (setf objs (remove-if #'null objs))
;    (break)
    (if objs
        (find (apply #'max (mapcar lookup objs)) objs :key lookup)
      nil)))

(defmethod processed-trace-object :after ((time number) (module (eql 'none)) (action (eql 'system-event)) &rest extras)
  (let ((obj (make-instance 'system-event
                            :start-time time
                            :end-time (+ time (first extras))
                            :label (fifth extras)
                            :buffer 'system
                            :operator-type "System Resource"
                            :event-type (third extras)
                            :event-key (list (second extras) (fourth extras)))))
    (cond ((equal (last extras 2) '(nil nil))
           (push obj (dependents *start-node*)))
          (t
           (push obj (dependents (gethash (last extras 2) *system-events*)))))
    (setf (gethash (list (second extras) (fourth extras)) *system-events*) obj))
)

(defmethod processed-trace-object :after ((time number) (module (eql '------)) (action (eql 'stopped)) &rest extras)
  (let ((obj (make-instance 'buffer-action)))
    (setf *end-node* obj)
    (setf (start-time obj) time (end-time obj) time (label obj) "dummy end" (buffer obj) 'production)
    (maphash #'(lambda (k v) (declare (ignore k))
                 (maphash #'(lambda (k v)
                              (declare (ignore k))
                              (if (and v (null (dependents v)))
                                  (push obj (dependents v)))
                              (if (and v (not (end-time v)))
                                  (setf (end-time v) (reduce 'min (mapcar #'start-time (dependents v))))))
                          v)) *buffers*))
)

(defparameter *dependents* nil)
(defparameter *queries* nil)

(defun get-dependencies (p)
  (gethash p *dependents*))

(defun get-queries (p)
  (gethash p *queries*))

(defun test-actr-processor ()
  (reset-actr-trace-system)
  (with-open-file (model-file "/Users/ewpatton/sanlab/test-model.lisp" :direction :input); :external-format '(:latin-1 :eol-style #+win32 :crlf #-win32 :lf))
    (multiple-value-bind (vars queries) (get-model-dependency-structure model-file)
      (setf *dependents* vars)
      (setf *queries* queries)))
  (with-open-file (trace-file "/Users/ewpatton/test-model.txt" :direction :input); :external-format '(:latin-1 :eol-style #+win32 :crlf #-win32 :lf))
    (do ((x (read-trace-object trace-file) (read-trace-object trace-file)))
        ((or (null x) (equal x '(eof))) nil)
      (if (and x (listp x) (not (eql (first x) 'eof)))
          (apply #'processed-trace-object x))))
)

(defmethod assign-uids ((buf buffer-action) ht)
  (let ((uid (random 2000000)))
    (loop (when (not (gethash uid ht)) (return)) (setf uid (random 2000000)))
    (setf (uid buf) uid)
    (setf (gethash uid ht) buf)
    (dolist (dep (dependents buf))
      (if (not (uid dep)) (assign-uids dep ht)))
    )
)

(defmethod assign-types ((buf buffer-action))
  (if (not (operator-type buf))
      (setf (operator-type buf) (default-type-for-buffer (buffer buf))))
  (dolist (dep (dependents buf))
    (assign-types dep))
)

(defmethod assign-types-list ((items list))
  (dolist (x items)
    (if (not (operator-type x))
        (setf (operator-type x) (default-type-for-buffer (buffer x))))))

(defmethod assign-coords ((buf buffer-action))
  (setf (x-pos buf) (* (depth buf) 200)
        (y-pos buf) (gethash (operator-type buf) *y-coord-info*))
  (if (not (y-pos buf)) (break))
  (dolist (dep (dependents buf))
    (assign-coords dep))
)

(defmethod assign-coords-list ((items list))
  (dolist (x items)
    (setf (x-pos x) (* (depth x) 200)
          (y-pos x) (gethash (operator-type x) *y-coord-info*)))
)

(defmethod build-activities ((buf buffer-action) (ht hash-table))
  (if (null (gethash (uid buf) ht))
      (progn
        (if (null (end-time buf))
            (setf (end-time buf) (reduce 'min (mapcar #'start-time (dependents buf)))))
	(let* ((duration (round (* 1000 (- (end-time buf) (start-time buf)))))
	       (use-gaussian (and *cogtool-trace* (not (zerop duration)))))	  
;          (setf use-gaussian nil)
	  (setf (gethash (uid buf) ht)
		(make-instance 'graph-node
			       :uid (uid buf)
			       :activity-type (get-activity-by-typename (operator-type buf))
			       :label (label buf)
			       :stored-x (x-pos buf)
			       :stored-y (y-pos buf)
			       :distribution (get-distribution-by-typename
                                              (cond ((and use-gaussian
                                                          (equal (operator-type buf) "Cognitive Operator")
                                                          (<= 100.0 duration)
                                                          )
                                                     "Multi-Unit Gamma CV")
                                                    (use-gaussian
                                                     "Gamma CV")
                                                    (t "Constant")))
			       :parameters (cons
					    (format nil "~A" duration)
					    (cond ((and use-gaussian
                                                        (equal (operator-type buf) "Cognitive Operator")
                                                        (<= 100.0 duration))
                                                   (list "0.29" (format nil "~A" (floor (/ duration (read-from-string (first (default-params (get-activity-by-typename "Cognitive Operator"))))))))
                                                   )
                                                  ((and use-gaussian (equal (operator-type buf) "Cognitive Operator"))
                                                   '("0.29"))
                                                  (use-gaussian
                                                   '("(random 0.1 1.0 1)"))
                                                  (t nil)))
			       :color (get-color (get-activity-by-typename (operator-type buf))))))
        (dolist (dep (dependents buf))
          (build-activities dep ht)
          (push (gethash (uid dep) ht) (edges-out (gethash (uid buf) ht)))
          (push (gethash (uid buf) ht) (edges-in (gethash (uid dep) ht))))
)))

(defmethod connect-danglers-to-dummy-end ((buf buffer-action))
  (cond ((eql buf *end-node*))
        ((null (dependents buf))
         (push *end-node* (dependents buf)))
        (t
         (dolist (dep (dependents buf))
           (connect-danglers-to-dummy-end dep)))))

(defmethod connect-danglers-to-dummy-end-list ((items list))
  (dolist (x items)
    (if (or (dependents x) (equal x *end-node*))
        nil
      (push *end-node* (dependents x)))))

(defmethod rearrange-coords ((buf buffer-action) (ht hash-table))
  (block rearrange-coords
    (if (member buf (gethash (x-pos buf) ht)) (return-from rearrange-coords t))
    (dolist (obj (gethash (x-pos buf) ht))
      (if (and (= (y-pos buf) (y-pos obj)) (not (eq buf obj)))
          (progn
            (if (< (depth buf) (depth obj))
                (incf (x-pos obj) 200)
              (if (= (depth buf) (depth obj))
                  (incf (y-pos buf) 100)
                (incf (x-pos buf) 200)))
            (return-from rearrange-coords))))
    (push buf (gethash (x-pos buf) ht))
    (dolist (dep (dependents buf))
      (if (not (rearrange-coords dep ht))
          (return-from rearrange-coords)))
    t)
)

(defun flatten-actr-trace-structure (start &key (reader #'dependents))
  (let ((list (copy-list (funcall reader start)))
        (final (list start)))
    (dolist (x list)
      (setf final (append final (flatten-actr-trace-structure x))))))

(defun assign-depths-recursive (buf depth)
  (setf (depth buf) depth)
  (dolist (dep (dependents buf))
    (if (< (depth dep) (1+ depth))
        (assign-depths-recursive dep (1+ depth)))))

(defmethod internal-representation-to-sanlab-model ((start buffer-action) progressbar statuslabel)
  (let ((uids (make-hash-table))
        (items (make-hash-table))
        (model nil)
        (count 0)
        (activities nil))
;    (capi:apply-in-pane-process statuslabel #'(setf capi:title-pane-text) "Flattening graph..." statuslabel)
;    (setf activities (remove-duplicates (flatten-actr-trace-structure start)))
    (capi:apply-in-pane-process progressbar #'(setf capi:range-slug-start) 3 progressbar)
    (capi:apply-in-pane-process statuslabel #'(setf capi:title-pane-text) "Assigning UIDs..." statuslabel)
    (assign-uids start uids)
    (maphash #'(lambda (k v) (declare (ignore k)) (push v activities)) uids)
    (capi:apply-in-pane-process progressbar #'(setf capi:range-slug-start) 4 progressbar)
    (capi:apply-in-pane-process statuslabel #'(setf capi:title-pane-text) "Fixing dangling activities..." statuslabel)
    (connect-danglers-to-dummy-end-list activities)
    (capi:apply-in-pane-process progressbar #'(setf capi:range-slug-start) 5 progressbar)
    (capi:apply-in-pane-process statuslabel #'(setf capi:title-pane-text) "Reassigning depths..." statuslabel)
    (dolist (x activities)
      (setf (depth x) -1))
    (assign-depths-recursive start 0)
    (capi:apply-in-pane-process progressbar #'(setf capi:range-slug-start) 6 progressbar)
    (capi:apply-in-pane-process statuslabel #'(setf capi:title-pane-text) "Assigning operator types..." statuslabel)
    (assign-types-list activities)
    (capi:apply-in-pane-process progressbar #'(setf capi:range-slug-start) 7 progressbar)
    (capi:apply-in-pane-process statuslabel #'(setf capi:title-pane-text) "Assigning initial coordinates..." statuslabel)
    (assign-coords-list activities)
    (capi:apply-in-pane-process progressbar #'(setf capi:range-slug-start) 8 progressbar)
    (loop
     (capi:apply-in-pane-process statuslabel #'(setf capi:title-pane-text) (format nil "Testing coordinate set ~a..." count) statuslabel)
     (when (or (rearrange-coords start (make-hash-table :test #'equal)))
       (return))
     (incf count))
    (capi:apply-in-pane-process progressbar #'(setf capi:range-slug-start) 9 progressbar)
    (capi:apply-in-pane-process statuslabel #'(setf capi:title-pane-text) "Building activity model..." statuslabel)
    (build-activities start items)
    (setf model (make-instance 'model :changed? t :activities (let (list) (maphash #'(lambda (k v) (declare (ignore k)) (push v list)) items) list)))
    (let ((x (stored-x (first (activities model)))))
      (dolist (act (activities model))
        (if (> (stored-x act) x) (setf x (stored-x act))))
      (setf (width model) (+ 200 x)))
    model
))

(defmethod clean-up-connections ((start buffer-action))
  (setf (dependents start) (remove-duplicates (dependents start)))
  (setf (visited start) t)
  (dolist (x (dependents start) start)
    (if (not (visited x)) (clean-up-connections x))))

(defmethod import-actr-model ((files list) progressbar statuslabel &optional *cogtool-trace*)
  (destructuring-bind (trace-file model-file) files
    (capi:apply-in-pane-process progressbar #'(setf capi:range-start) 0 progressbar)
    (capi:apply-in-pane-process progressbar #'(setf capi:range-end) 10 progressbar)
    (capi:apply-in-pane-process progressbar #'(setf capi:range-slug-start) 0 progressbar)
    (capi:apply-in-pane-process statuslabel #'(setf capi:title-pane-text) "Loading Model..." statuslabel)
    (with-open-file (file model-file :direction :input); :external-format '(:latin-1 :eol-style #+win32 :crlf #-win32 :lf))
      (multiple-value-setq (*dependents* *queries*) (get-model-dependency-structure file)))
    (capi:apply-in-pane-process progressbar #'(setf capi:range-slug-start) 1 progressbar)
    (capi:apply-in-pane-process statuslabel #'(setf capi:title-pane-text) "Processing trace file..." statuslabel)
    (let ((last-second -1))
      (with-open-file (file trace-file :direction :input); :external-format '(:latin-1 :eol-style #+win32 :crlf #-win32 :lf))
        (when *cogtool-trace*
          (loop for s = (read-line file)
                while (not (string-equal s ";;; TRACE STARTS HERE"))))
        (do ((x (read-trace-object file) (read-trace-object file)))
            ((or (null x) (equal x '(eof))) nil)
          (if (and x (listp x) (not (eql (first x) 'eof)))
              (progn
                (if (< last-second (round (first x)))
                    (progn
                      (setf last-second (round (first x)))
                      (capi:apply-in-pane-process statuslabel #'(setf capi:title-pane-text) (format nil "Processing entries for second ~A..." last-second) statuslabel)))
                (apply #'processed-trace-object x)))
          )))
    (capi:apply-in-pane-process statuslabel #'(setf capi:title-pane-text) "Searching for duplicate connections..." statuslabel)
    (clean-up-connections *start-node*)
    (capi:apply-in-pane-process progressbar #'(setf capi:range-slug-start) 2 progressbar)
    (capi:apply-in-pane-process statuslabel #'(setf capi:title-pane-text) "Converting to SANLab Model..." statuslabel)
    (internal-representation-to-sanlab-model *start-node* progressbar statuslabel)
))

(defun reset-actr-trace-system ()
  (setf *registered-events* (make-hash-table :test #'equal))
  (setf *buffers* (make-hash-table))
  (setf *history* (make-hash-table))
  (setf *watches-before* nil)
  (setf *watches-after* nil)
  (setf *actions-after* nil)
  (setf *buffer-to-activity-map* nil)
  (setf *y-coord-info* (make-hash-table :test #'equal))
  (add-buffer-of-interest 'production)
  (map-buffer 'production "Cognitive Operator")
  (setf (gethash "Cognitive Operator" *y-coord-info*) 400)
  (setf (gethash "Attend Operator" *y-coord-info*) (gethash "Cognitive Operator" *y-coord-info*))
  (setf (gethash "Initiate Operator" *y-coord-info*) (gethash "Cognitive Operator" *y-coord-info*))
  (setf (gethash "Verify Operator" *y-coord-info*) (gethash "Cognitive Operator" *y-coord-info*))
  (add-buffer-of-interest 'visual-location)
  (map-buffer 'visual-location "Perceptual Operator (Visual)")
  (setf (gethash "Perceptual Operator (Visual)" *y-coord-info*) 100)
  (add-buffer-of-interest 'visual)
  (map-buffer 'visual "Perceptual Operator (Visual)")
  (add-buffer-of-interest 'vocal)
  (map-buffer 'vocal "Speech Operator")
  (setf (gethash "Speech Operator" *y-coord-info*) 800)
  (add-buffer-of-interest 'aural-location)
  (map-buffer 'aural-location "Perceptual Operator (Auditory)")
  (setf (gethash "Perceptual Operator (Auditory)" *y-coord-info*) 200)
  (add-buffer-of-interest 'aural)
  (map-buffer 'aural "Perceptual Operator (Auditory)")
  (add-buffer-of-interest 'retrieval)
  (map-buffer 'retrieval "Declarative Memory")
  (setf (gethash "Declarative Memory" *y-coord-info*) 300)
  (add-buffer-of-interest 'manual)
  (map-buffer 'manual "Motor Operator")
  (setf (Gethash "Motor Operator" *y-coord-info*) 600)
  (setf (gethash "Left Hand Operator" *y-coord-info*) 600)
  (setf (gethash "Right Hand Operator" *y-coord-info*) 700)
  (add-buffer-of-interest 'visual-motor)
  (map-buffer 'visual-motor "Eye Movement Operator")
  (setf (gethash "Eye Movement Operator" *y-coord-info*) 900)
  (add-buffer-of-interest 'imaginal)
  (map-buffer 'imaginal "Task-oriented Memory")
  (setf (gethash "Task-oriented Memory" *y-coord-info*) 500)
  (add-buffer-of-interest 'goal)
  (map-buffer 'goal "Task-oriented Memory")
  (add-buffer-of-interest 'frame)
  (map-buffer 'frame "System Resource")
  (add-buffer-of-interest 'cogtool)
  (map-buffer 'cogtool "System Resource")
  (setf (gethash "System Resource" *y-coord-info*) 0)
  (setf *start-node* (setf (buffer-of-interest 'production) (make-instance 'production :label "SANLab Start" :start-time 0.0 :end-time 0.0)))
  (setf *end-node* nil)
  (clear-buffer 'visual-location)
  (clear-buffer 'visual)
  (clear-buffer 'vocal)
  (clear-buffer 'aural-location)
  (clear-buffer 'aural)
  (clear-buffer 'retrieval)
  (clear-buffer 'manual)
  (clear-buffer 'visual-motor)
  (clear-buffer 'imaginal)
  (clear-buffer 'goal)
  (reset-last-processed-motor)
  (reset-press-key)
  (setf *system-events* (make-hash-table :test #'equal))
  (setf *dependents* nil)
  (setf *queries* nil)
  (setf *vocal-events* nil)
  (setf *manual-events* nil)
  (setf *last-eye-movement* nil)
  (setf *last-eye-prep* nil)
)
