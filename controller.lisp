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

(capi:define-interface progress-window ()
  ()
  (:panes
   (progress
    capi:progress-bar
    :visible-min-width 400
    :accessor progress)
   (status
    capi:title-pane
    :accessor status))
  (:layouts
   (primary-layout
    capi:column-layout
    '(progress status)))
  (:default-initargs
   :title "Progress"))

(defclass graph-node-copy ()
  ((uid :initform 0 :initarg :uid :accessor uid)
   (activity-type :initform nil :initarg :activity-type :accessor activity-type)
   (ir-type :initform nil :initarg :ir-type :accessor ir-type)
   (ir-task :initform nil :initarg :ir-task :accessor ir-task)
   (ir-append :initform nil :initarg :ir-append :accessor ir-append)
   (label :initform nil :initarg :label :accessor label)
   (color :initform nil :initarg :color :accessor color)
   (offset-x :initform 0 :initarg :offset-x :accessor offset-x)
   (offset-y :initform 0 :initarg :offset-y :accessor offset-y)
   (edges-in :initform nil :initarg :edges-in :accessor edges-in)
   (edges-out :initform nil :initarg :edges-out :accessor edges-out)
   (distribution :initform nil :initarg :distribution :accessor distribution)
   (parameters :initform nil :initarg :parameters :accessor parameters)
   (src :initform nil :initarg :src :accessor src)
))

(defmethod make-graph-node-copy ((gn graph-node))
  (let ((x (make-instance 'graph-node-copy)))
    (setf (uid x) (uid gn))
    (setf (activity-type x) (activity-type gn))
    (setf (ir-type x) (ir-type gn))
    (setf (ir-task x) (ir-task gn))
    (setf (ir-append x) (ir-append gn))
    (setf (label x) (label gn))
    (setf (color x) (color gn))
    (setf (offset-x x) (stored-x gn))
    (setf (offset-y x) (stored-y gn))
    (setf (edges-in x) nil)
    (setf (edges-out x) nil)
    (setf (distribution x) (distribution gn))
    (setf (parameters x) (copy-list (parameters gn)))
    (setf (src x) gn)
    x))

(defmethod initialize-instance :after ((self controller) &rest initargs)
  (if (model self)
      (setf (controller (model self)) self)))

(defmethod (setf model) :after (model (self controller))
  (if model
      (setf (controller model) self)))

(defmethod can-undo ((controller controller))
  (undo-history controller))

(defmethod can-redo ((controller controller))
  (redo-history controller))

(defmethod (setf doing-batch-update) :before (val (controller controller))
  (if (not val)
      (progn
        (setf (slot-value controller 'doing-batch-update) nil)
        ;(capi:display-message "Firing batch event")
        (fire-batch-events controller)
        (setf (batch-updates controller) nil))))

(defmethod fire-batch-events ((controller controller))
  (dolist (item (reverse (batch-updates controller)))
    (fire-event-listeners controller (first item) (second item))
    )
)

(defmethod add-event-listener ((event symbol) (callback listener) (controller controller))
  (push callback (gethash event (event-callbacks controller)))
  callback
)

(defmethod remove-event-listener ((controller controller) (event symbol) (callback listener))
  (setf (gethash event (event-callbacks controller)) (remove callback (gethash event (event-callbacks controller))))
  nil
)

(defmethod fire-event-listeners ((controller controller) (event symbol) info)
;  (capi:display-message (format nil "Firing listeners for ~A" event))
;  (capi:display-message (format nil "Doing batch update? ~A" (doing-batch-update controller)))
  (if (not (listp info)) (setf info (list info)))
  (setf info (mapcar #'convert-internal-pointer-to-external-pointer info))
  (if (not (doing-batch-update controller))
      (mapcar #'(lambda (x)
;                  (inspect info)
                  (let ((callback (callback x)))
;                    (inspect x)
                    (cond ((equal (type-of callback) 'symbol)
                           (apply (symbol-function callback) event info))
                          ((or (equal (type-of callback) 'standard-generic-function) (equal (type-of callback) 'function))
                           (apply callback event info)))))
              (gethash event (event-callbacks controller)))))

(defmethod fire-event-listeners (controller (event symbol) info)
  nil
)

(defmacro batch-update (controller &body body)
  (let ((res (gensym)))
    `(let (,res)
       (setf (doing-batch-update ,controller) t)
       (setf ,res (progn
                    ,@body))
       (setf (doing-batch-update ,controller) nil)
       ,res
     )))


(defmethod run-model-with-special-processor ((controller controller) (trials number) (callback function) (seed random-state) (processor symbol) &key (wait nil) (show t))
  (setq *random-state* seed)
  (let ((np (make-instance processor :total-trials trials :callback callback :model (model controller) :show-results show)))
    (execute-processor np :wait wait))
  (clear-redo-history controller)
)

(defmethod run-model ((controller controller) (trials number) (callback function) &key (seed *random-state*) (wait nil) (show t))
  (run-model-with-special-processor controller trials callback seed 'network-processor :wait wait :show show))

(defmethod generate-sample-trace ((controller controller) filepath)
  (generate-actr-style-trace (model controller) filepath)
  (clear-redo-history controller)
)

(defmethod create-activity-in-model ((controller controller) (ap activity-pointer) &rest initargs &key dummy &allow-other-keys)
  (declare (ignore dummy))
  (let ((command (make-instance 'create-activity-in-model-action :initargs (cons (pointer ap) initargs)))
        (response nil))
    (and (perform-action command controller)
         (add-undo-item command)
         (setf response (convert-internal-pointer-to-external-pointer (created-activity command))))
    (clear-redo-history controller)
    response)
)

#|
(defmethod delete-activity-in-model ((controller controller) (act instance-pointer))
  (delete-activity-in-model (model controller) (pointer act))
)
|#

(defmethod delete-activity-in-model ((controller controller) (act instance-pointer))
  (let ((command (make-instance 'delete-activity-from-model-action :activity (pointer act))))
    (and (perform-action command controller)
         (add-undo-item command)
         t))
  (clear-redo-history controller)
)

#|
(defmethod connect-activities-in-model ((controller controller) (act1 instance-pointer) (act2 instance-pointer) &rest initargs)
  (apply #'connect-activities-in-model (model controller) (pointer act1) (pointer act2) initargs))
|#

(defmethod connect-activities-in-model ((controller controller) (act1 instance-pointer) (act2 instance-pointer) &rest initargs &key dummy &allow-other-keys)
  (declare (ignore dummy))
  (let ((command (make-instance 'connect-activities-in-model-action :act1 (pointer act1) :act2 (pointer act2))))
    (and (perform-action command controller)
         (add-undo-item command)
         t))
  (clear-redo-history controller)
)

#|
(defmethod disconnect-activities-in-model ((controller controller) (act1 instance-pointer) (act2 instance-pointer))
  (disconnect-activities-in-model (model controller) (pointer act1) (pointer act2)))
|#

(defmethod disconnect-activities-in-model ((controller controller) (act1 instance-pointer) (act2 instance-pointer))
  (let ((command (make-instance 'disconnect-activities-in-model-action :act1 (pointer act1) :act2 (pointer act2))))
    (and (perform-action command controller)
         (add-undo-item command)
         t))
  (clear-redo-history controller)
)

#|
(defmethod move-activity-in-model ((controller controller) (act instance-pointer) &rest initargs &key dummy &allow-other-keys)
  (apply #'move-activity-in-model (model controller) (pointer act) initargs))

|#

(defmethod move-activity-in-model ((controller controller) (act instance-pointer) &rest initargs &key x y &allow-other-keys)
  (let ((command (make-instance 'move-activity-in-model-action :activity (pointer act) :new-position (list x y))))
    (and (perform-action command controller)
         (add-undo-item command)
         t))
  (clear-redo-history controller)
)

(defmethod move-activities-in-model ((controller controller) (acts list) (positions list))
  (let ((command (make-instance 'move-activities-in-model-action :activities acts :positions positions)))
    (and (perform-action command controller)
         (add-undo-item command)
         t))
  (clear-redo-history controller)
)

(defmethod new-model ((controller controller) &rest args)
  (let ((new-model (make-instance 'model :controller controller)))
;    (capi:display-message (format nil "Model = ~A" new-model))
    (if new-model
        (progn
          (setf (model controller) new-model)
          (clear-undo-history controller)
          (clear-redo-history controller)
          (fire-event-listeners controller 'new-model nil))
      (fire-event-listeners controller 'create-new-model-failed nil))
    new-model)
)

(defmethod open-model ((controller controller) &rest args)
  (let (open-model)
    (multiple-value-bind (success error) (ignore-errors (setq open-model (apply #'open-as-bundle args)))
      (if success
          (progn
            (setf (model controller) open-model)
            (fire-event-listeners controller 'open-model nil)
            (clear-undo-history controller)
            (clear-redo-history controller)
            (setf (changed? (model controller)) nil))
        (fire-event-listeners controller 'open-model-failed error))
      open-model))
)

(defmethod controller-import-macproject-file ((controller controller))
  (let (open-model)
    (multiple-value-bind (success error) (ignore-errors (setq open-model (import-macproject-file)))
      (if success
          (progn
            (setf (model controller) open-model)
            (fire-event-listeners controller 'import-macproject-model nil)
            (clear-undo-history controller)
            (clear-redo-history controller)
            (setf (changed? (model controller)) t))
        (fire-event-listeners controller 'import-macproject-model-failed error))
      open-model))
)

(defmethod import-actr-trace ((controller controller))
  (mp:process-run-function "ACT-R Trace Loader" nil #'(lambda (io)
  (block nil
    (setf *standard-output* io *trace-output* io)
    (reset-actr-trace-system)
    (let (open-model trace model progwindow)
      (setf trace (capi:prompt-for-file "Select a trace" :operation :open :if-does-not-exist :error :filters '("Trace File" "*.txt" "All Files" "*.*")))
      (if (not trace) (return))
      (setf model (capi:prompt-for-file "Select a model" :operation :open :if-does-not-exist :error :filters '("Lisp File" "*.lsp;*.lisp" "All Files" "*.*")))
      (if (not model) (return))
      (setf progwindow (make-instance 'progress-window))
      (capi:display progwindow)
      ; The following seemed like gibberish, as both setq and identity return just a single value; I've edited to what appears to be intended
      ;(multiple-value-bind (success error) (identity (setq open-model (import-actr-model (list trace model) (progress progwindow) (status progwindow))))
      (multiple-value-bind (success error) (import-actr-model (list trace model) (progress progwindow) (status progwindow) nil)
	(setq open-model success)
        (capi:apply-in-pane-process progwindow #'capi:quit-interface progwindow :force t)
        (if success
            (progn
              (setf (model controller) open-model)
              (clear-undo-history controller)
              (clear-redo-history controller)
              (fire-event-listeners controller 'import-actr-trace nil))
          (fire-event-listeners controller 'import-actr-trace-failed error))
        open-model)))
  ) *standard-output*))

(defmethod import-from-cogtool ((controller controller))
  (mp:process-run-function "ACT-R Trace Loader" nil #'(lambda (io)
  (block nil
    (setf *standard-output* io *trace-output* io)
    (reset-actr-trace-system)
    (let (open-model trace progwindow)
      (setf trace (capi:prompt-for-file "Select information exported from CogTool" :operation :open :if-does-not-exist :error :filters '("CogTool Export" "*.txt" "All Files" "*.*")))
      (if (not trace) (return))
      (setf progwindow (make-instance 'progress-window))
      (capi:display progwindow)
      (multiple-value-bind (success error) (import-actr-model (list trace trace) (progress progwindow) (status progwindow) t)
	(setq open-model success)
        (capi:apply-in-pane-process progwindow #'capi:quit-interface progwindow :force t)
        (if success
            (progn
              (setf (model controller) open-model)
              (clear-undo-history controller)
              (clear-redo-history controller)
              (fire-event-listeners controller 'import-from-cogtool nil))
          (fire-event-listeners controller 'import-from-cogtool-failed error))
        open-model)))
  ) *standard-output*))

(defmethod save-model-action ((controller controller) &rest args)
  (multiple-value-bind (success error) (apply #'save-as-bundle (model controller) args)
    (if success
        (progn
          (setf (changed? (model controller)) nil)
          (setf (undo-history controller) nil (redo-history controller) nil)
          (fire-event-listeners controller 'save-model nil))
      (fire-event-listeners controller 'save-model-failed error))
    success)
)

(defmethod save-interactive-routine-action ((controller controller) name &key (overwrite t))
  (block save-interactive-routine-action
    (multiple-value-bind (result success) (capi:prompt-for-color "Interactive Routine Color:")
      (if success
          (setf (color (model controller)) result)
        (return-from save-interactive-routine-action)))
    (multiple-value-bind (success error) (ignore-errors (open name :direction :output :if-exists (if overwrite :supersede :error) :external-format '(:latin-1 :eol-style #+win32 :crlf #-win32 :lf)))
      (cond (success
             (setf (filename (model controller)) name)
             (save-interactive-routine success (model controller))
             (close success)
             (setq success t)
             (setf (changed? (model controller)) nil)
             (setf (undo-history controller) nil (redo-history controller) nil)
             (fire-event-listeners controller 'save-model nil))
            (t
             (fire-event-listeners controller 'save-model-failed error)))
      success))
)

(defmethod layout-graph-action ((controller controller) &rest args)
  (multiple-value-bind (success error) (apply #'layout-model (model controller) args)
    (if success
        (progn
          (setf (changed? (model controller)) t)
          (fire-event-listeners controller 'laid-out-model nil))
      (fire-event-listeners controller 'laid-out-model-error error))
    success)
  (clear-redo-history controller)
)

(defmethod model-changed? ((controller controller))
  (changed? (model controller)))

(defmethod current-textbox ((controller controller))
  (let ((textbox nil))
    (maphash #'(lambda (k v)
                 (declare (ignore k))
                 (capi:map-pane-descendant-children
                  v
                  #'(lambda (p)
                      ;(format t "Visiting ~A~%" p)
                      (if (and (subtypep (type-of p) 'capi:text-input-pane) (capi:pane-has-focus-p p))
                          (setf textbox p)))))
             (views (view controller)))
    textbox
))

(defmethod cut-text-selection ((controller controller))
  (let ((command (make-instance 'cut-text-action :textbox (current-textbox controller))))
    (and (perform-action command controller)
         (add-undo-item command)
         t))
)

(defmethod copy-text-selection ((controller controller))
  (capi:text-input-pane-copy (current-textbox controller))
)

(defmethod paste-text-selection ((controller controller))
  (let ((command (make-instance 'paste-text-action :textbox (current-textbox controller))))
    (and (perform-action command controller)
         (add-undo-item command)
         t))
)

(defmethod cut-selection ((controller controller) &rest args &key dummy &allow-other-keys)
  (declare (ignore dummy))
  (block cut-selection
    (if (current-textbox controller)
        (progn
          (cut-text-selection controller)
          (return-from cut-selection t)))
    (let ((action (apply 'make-instance 'cut-graph-action args)))
      (and (perform-action action controller)
           (add-undo-item action)
           t))
    )
  (clear-redo-history controller)
  t
)

(defmethod copy-selection ((controller controller) &rest args &key dummy &allow-other-keys)
  (declare (ignore dummy))
  (block copy-selection
    (if (current-textbox controller)
        (progn
          (copy-text-selection controller)
          (return-from copy-selection t)))
    (let ((action (apply 'make-instance 'copy-graph-action args)))
      (perform-action action controller))
    )
  t
  )

(defmethod paste-selection ((controller controller) &rest args &key dummy &allow-other-keys)
  (declare (ignore dummy))
  (block paste-selection
    (if (current-textbox controller)
        (progn
          (paste-text-selection controller)
          (return-from paste-selection t)))
    (let ((command (make-instance 'paste-graph-action)))
      (and (perform-action command controller)
           (add-undo-item command)
           t))
    )
  (clear-redo-history controller)
  t
)

(defmethod select-all-text ((controller controller))
  (let ((textbox (current-textbox controller)))
    (capi:set-text-input-pane-selection textbox 0 (length (capi:text-input-pane-text textbox)))))

(defmethod select-all ((controller controller))
  (block select-all
    (if (current-textbox controller)
        (progn
          (select-all-text controller)
          (return-from select-all)))
    (dolist (act (activities (model controller)))
      (setf (selected? act) t))
    (fire-event-listeners controller 'selected-all-activities (activities (model controller)))
    t
    )
  (clear-redo-history controller)
)

(defmethod select-by-type ((controller controller) &rest args)
  (dolist (act (activities (model controller)))
    (if (member (activity-type act) args :key #'pointer)
        (setf (selected? act) t)
      (setf (selected? act) nil)))
  (fire-event-listeners controller 'selected-activities-by-type (remove-if-not #'selected? (activities (model controller))))
  nil
)

(defmethod get-children ((src activity-pointer))
  (mapcar #'convert-internal-pointer-to-external-pointer (get-children (pointer src))))

(defmethod deselect-all ((controller controller))
  (dolist (act (activities (model controller)))
    (setf (selected? act) nil))
  (clear-redo-history controller)
  (fire-event-listeners controller 'deselected-all-activities (activities (model controller)))
  nil
)

(defmethod retrieve-previous-trials ((controller controller) &rest args)
)

(defmethod retrieve-random-state ((controller controller) &rest args)
  (block retrieve-random-state
    (let ((file (pathname controller)))
      (if (equalp nil file) (return-from retrieve-random-state (capi:display-message "You must save your model before retrieving random states")))
      (let ((items (list-random-states-in-bundle file)))
        (if (equalp nil items) (return-from retrieve-random-state (capi:display-message "You must run trials before you can retrieve a previous random state")))
        (multiple-value-bind (chosen successp) (capi:prompt-with-list items "Select a random state:")
          (if (equalp nil successp) (return-from retrieve-random-state))
          (read-random-state-from-bundle file chosen)
          (capi:display-message "Random state loaded successfully")
))))
  (clear-redo-history controller)
)

(defmethod convert-internal-pointer-to-external-pointer ((a graph-node))
  (make-instance 'instance-pointer :pointer a))

(defmethod convert-internal-pointer-to-external-pointer ((a activity-type))
  (make-instance 'activity-pointer :pointer a))

(defmethod convert-internal-pointer-to-external-pointer ((a distribution-type))
  (make-instance 'distribution-pointer :pointer a))

(defmethod convert-internal-pointer-to-external-pointer ((a interactive-routine))
  (make-instance 'interactive-routine-pointer :pointer a))

(defmethod convert-internal-pointer-to-external-pointer ((a ir-task))
  (make-instance 'interactive-routine-task-pointer :pointer a))

(defmethod convert-internal-pointer-to-external-pointer ((a routine-instance))
  (make-instance 'interactive-routine-instance-pointer :pointer a))

(defmethod convert-internal-pointer-to-external-pointer ((a rendered-note))
  (make-instance 'rendered-note-pointer :pointer a))

(defmethod convert-internal-pointer-to-external-pointer (a)
  a)

(defmethod model-activities ((controller controller))
  (mapcar #'convert-internal-pointer-to-external-pointer (activities (model controller))))

(defmethod pathname ((controller controller))
  (filename (model controller))
)

(defun get-all-activity-types ()
  (mapcar #'convert-internal-pointer-to-external-pointer (app-property 'activity-types)))

(pass changed? controller model)
(pass title controller model)
(pass author controller model)
(pass notes controller model)
(pass width controller model)
(pass height controller model)
(pass grid-size controller model)
(pass ir-color instance-pointer graph-node :accessor pointer)
(pass label instance-pointer graph-node :accessor pointer)
(pass-all on-critical-path? instance-pointer graph-node :accessor pointer)
(pass-all selected? instance-pointer graph-node :accessor pointer)

(defmethod set-model-title ((controller controller) val)
  (let ((command (make-instance 'set-model-title-action :title val)))
    (and (perform-action command controller)
         (add-undo-item command)
         t))
  (clear-redo-history controller)
)

(defmethod set-model-author ((controller controller) val)
  (let ((command (make-instance 'set-model-author-action :author val)))
    (and (perform-action command controller)
         (add-undo-item command)
         t))
  (clear-redo-history controller)
)

(defmethod set-model-width ((controller controller) val)
  (let ((command (make-instance 'set-model-width-action :width val)))
    (and (perform-action command controller)
         (add-undo-item command)
         t))
  (clear-redo-history controller)
)

(defmethod set-model-height ((controller controller) val)
  (let ((command (make-instance 'set-model-height-action :height val)))
    (and (perform-action command controller)
         (add-undo-item command)
         t))
  (clear-redo-history controller)
)

(defmethod set-model-notes ((controller controller) val)
  (let ((command (make-instance 'set-model-notes-action :notes val)))
    (and (perform-action command controller)
         (add-undo-item command)
         t))
  (clear-redo-history controller)
)

(defmethod set-selected-activities-type ((controller controller) (activities list) val)
  (let* ((command (make-instance 'set-property-for-activities-in-model-action
                                 :activities activities
                                 :property 'activity-type
                                 :new-value val)))
    (and (perform-action command controller)
         (add-undo-item command)
         t))
  (clear-redo-history controller)
)

(defmethod set-selected-activities-label ((controller controller) (activities list) val)
  (let* ((value (format nil "~A" val))
         (command (make-instance 'set-property-for-activities-in-model-action
                                :activities activities
                                :property 'label
                                :new-value value)))
    (and (perform-action command controller)
         (add-undo-item command)
         t))
  (clear-redo-history controller)
)

(defmethod set-selected-activities-distribution ((controller controller) (activities list) (val distribution-pointer))
  (let ((command (make-instance 'set-property-for-activities-in-model-action
                                :activities activities
                                :property 'distribution
                                :new-value (pointer val))))
    (and (perform-action command controller)
         (add-undo-item command)
         t))
  (clear-redo-history controller)
)

(defmethod set-selected-activities-parameter ((controller controller) (activities list) (param number) val)
  (let ((command (make-instance 'set-parameter-for-activities-in-model-action
                                :activities activities
                                :parameter-value param
                                :new-value val)))
    (and (perform-action command controller)
         (add-undo-item command)
         t))
  (clear-redo-history controller)
)

(defmethod create-model-note ((controller controller) text x y)
  (let ((command (make-instance 'add-model-note-action :text text :position (list x y))))
    (and (perform-action command controller)
         (add-undo-item command)
         t))
  (clear-redo-history controller)
)

(defmethod set-model-note-text ((controller controller) (note rendered-note-pointer) text)
  (cond ((or (null text) (equal text ""))
         (remove-model-note controller note))
        (t
         (let ((command (make-instance 'set-model-note-text-action :text text :note note)))
           (and (perform-action command controller)
                (add-undo-item command)
                t))))
  (clear-redo-history controller)
)

(defmethod move-model-note ((controller controller) (note rendered-note-pointer) x y)
  (let ((command (make-instance 'move-model-note-action :note note :position (list x y))))
    (and (perform-action command controller)
         (add-undo-item command)
         t))
  (clear-redo-history controller)
)

(defmethod remove-model-note ((controller controller) (note rendered-note-pointer))
  (let ((command (make-instance 'remove-model-note-action :note note)))
    (and (perform-action command controller)
         (add-undo-item command)
         t))
  (clear-redo-history controller)
)

(defmethod rendered-notes ((controller controller))
  (mapcar #'convert-internal-pointer-to-external-pointer (rendered-notes (model controller))))

(defmethod (setf label) (val (src instance-pointer))
  (setf (label (pointer src)) (format nil "~A" val)))

(defmethod (setf distribution) ((val distribution-pointer) (src instance-pointer))
  (setf (distribution (pointer src)) (pointer val)))

(defmethod (setf parameters) (val (src instance-pointer))
  (setf (parameters (pointer src)) val))

(defmethod (setf parameter) (val (index number) (src instance-pointer))
  (setf (parameter index (pointer src)) val))

(defmethod (setf activity-type) (val (src instance-pointer))
  (setf (activity-type (pointer src)) val))

(defmethod parameters ((src instance-pointer))
  (parameters (pointer src)))

(defmethod ir-instance ((src instance-pointer))
  (ir-instance (pointer src)))

(defmethod x ((src instance-pointer))
  (stored-x (pointer src)))

(defmethod y ((src instance-pointer))
  (stored-y (pointer src)))

(defmethod activity-type ((src instance-pointer))
  (make-instance 'activity-pointer :pointer (activity-type (pointer src))))

(defmethod highlight-color ((src interactive-routine-pointer))
  (highlight-color (pointer src)))

(defmethod color ((src interactive-routine-pointer))
  (highlight-color (pointer src)))

(defmethod ir-task ((src instance-pointer))
  (convert-internal-pointer-to-external-pointer (ir-task (pointer src))))

(defmethod ir-type ((src instance-pointer))
  (convert-internal-pointer-to-external-pointer (ir-type (pointer src))))

(defmethod task-list ((src interactive-routine-pointer))
  (mapcar #'convert-internal-pointer-to-external-pointer (task-list (pointer src))))

(pass-all offset-x interactive-routine-task-pointer ir-task :accessor pointer)
(pass-all offset-y interactive-routine-task-pointer ir-task :accessor pointer)
(pass-all label interactive-routine-task-pointer ir-task :accessor pointer)
(pass-all distribution interactive-routine-task-pointer ir-task :accessor pointer)
(pass-all parameters interactive-routine-task-pointer ir-task :accessor pointer)

(defmethod operator-type ((src interactive-routine-task-pointer))
  (convert-internal-pointer-to-external-pointer (operator-type (pointer src))))

(defmethod edges-out ((src interactive-routine-task-pointer))
  (mapcar #'convert-internal-pointer-to-external-pointer (edges-out (pointer src))))

(defmethod edges-in ((src interactive-routine-task-pointer))
  (mapcar #'convert-internal-pointer-to-external-pointer (edges-in (pointer src))))

(defmethod get-base-operators ((controller controller))
  (mapcar #'convert-internal-pointer-to-external-pointer (get-root-activities)))

(pass-all name interactive-routine-pointer interactive-routine :accessor pointer)
(pass-all uid interactive-routine-task-pointer ir-task :accessor pointer)
(pass get-color activity-pointer activity-type :accessor pointer)
(pass conditional-mean-start instance-pointer graph-node :accessor pointer)
(pass conditional-mean-end instance-pointer graph-node :accessor pointer)
(pass mean-start instance-pointer graph-node :accessor pointer)
(pass mean-end instance-pointer graph-node :accessor pointer)
