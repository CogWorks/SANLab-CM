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

; The Activity Layout class used for the main editor window.
(defclass activity-layout (capi:pinboard-layout)
  ((activities :initform nil :accessor activities)
   (arrows-parts :initform nil :accessor arrow-parts)
   (arrow-objs :initform nil :accessor arrows)
   (selected-activities :initform nil :accessor selected-activities)
   (changed-selection :initform nil :accessor changed-selection)
   (current-mode :initform 'select :accessor current-mode)
   (dragging :initform nil :reader is-dragging? :writer dragging)
   (arrow-to-be-placed :initform nil :accessor arrow-to-be-placed)
   (current-interactive-routine :initform nil :accessor current-interactive-routine)
   (grid :initform nil :accessor grid)
   (selection-rectangle :initform nil :accessor selection-rectangle)
   (drag-x :initform nil :accessor drag-x)
   (drag-y :initform nil :accessor drag-y)
   (interactive-layer :initform nil :accessor interactive-layer)
   (polling-process :initform nil :accessor polling-process)
   (notes :initform nil :accessor notes)
)
  (:default-initargs
   :horizontal-scroll t
   :vertical-scroll t
   :background #(:RGB 1.0s0 1.0s0 1.0s0)
   :scroll-callback #'send-scroll-event
   :drop-callback #'drop-interactive-routine
   :input-model *pinboard-layout-input-model*)
  (:documentation "The Activity Layout class, a subclass of capi:pinboard-layout, that displays a collection of activity panes, arrows, notes, and the background grid for the main editor of SANLab-CM")
)

; Pass the method criticality-overlay-enabled to its interface
(pass-all criticality-overlay-enabled? activity-layout capi:element-interface)

; When the user selects an activity, we need to set the appropriate flag in
; the underlying model representation
(defmethod (setf selected-activities) :after (acts (self activity-layout))
  "Updates the underlying model representation when the list of selected activities changes"
  (dolist (act acts)
    (if (equalp nil (selected? (source act)))
        (setf (selected? (source act)) t))))

; Some setup stuff that needs to be done. In particular, composite arrows
; that are used for interactive routines result in the creation of two new objects
(defmethod initialize-instance :after ((obj activity-layout) &rest args)
  "Decomposes any compound arrows and configures the layout description"
  (if (and (arrows obj) (not (arrow-parts obj)))
      (setf (arrow-parts obj) (get-arrow-parts (arrows obj))))
  (setf (capi:layout-description obj) (append (list (grid obj)) (notes obj) (arrows obj) (arrow-parts obj) (activities obj))))

; Update the layout if the activities change
(defmethod (setf activities) :after (val (obj activity-layout))
  "Updates the layout when the activities change"
  (setf (capi:layout-description obj) (append (list (grid obj)) (notes obj) (arrows obj) (arrow-parts obj) (activities obj))))

; Update the layout if the arrows change
(defmethod (setf arrows) :after (val (obj activity-layout))
  "Updates the layout when the arrows change"
  (setf (arrow-parts obj) (get-arrow-parts val))
  (setf (capi:layout-description obj) (append (list (grid obj)) (notes obj) (arrows obj) (arrow-parts obj) (activities obj))))

; Update the layout if the notes change
(defmethod (setf notes) :after (val (obj activity-layout))
  "Updates the layout when the notes change"
  (setf (capi:layout-description obj) (append (list (grid obj)) (notes obj) (arrows obj) (arrow-parts obj) (activities obj))))

; Update the layout and redraw the pane
(defmethod update-activity-layout ((obj activity-layout))
  "Updates the description of the layout and redraws the pinboard"
  (setf (capi:layout-description obj) (append (list (grid obj)) (notes obj) (arrows obj) (arrow-parts obj) (activities obj)))
  (capi:with-geometry obj
    (capi:redraw-pinboard-layout obj capi:%x% capi:%y% capi:%width% capi:%height%))
)

; Highlights the activities currently specified by the
; selected-activities reader
(defun highlight-selected-activities (self)
  "Highlights the selected activities of the layout"
  (dolist (act (capi:layout-description self))
    (if (and (activity-pane-p act) (equalp nil (selected? act)))
        (highlight nil act)))
  (dolist (act (selected-activities self))
    (if (activity-pane-p act)
        (highlight t act))))

; Process mouse movement over the activity layout
; conditional on the currently selected tool
(defun activity-layout-mouse-move (self x y)
  "Processes mouse movements across the activity layout"
  (cond ((equal (current-mode self) 'select)
         (dolist (obj (capi:layout-description self))
           (if (over-activity-pane-p obj x y)
               (multiple-value-bind (new-x new-y) (capi:convert-relative-position self obj x y)
                 (handle-highlighting obj new-x new-y)
                 (return-from activity-layout-mouse-move))
             (handle-highlighting obj 0 0))
           (if (and (activity-pane-p obj) (expansion-timer-value obj))
               (setf (expansion-timer-value obj) nil))
           (if (and (activity-pane-p obj) (expanded? obj) (not (collapse-timer-value obj)) (not (is-displaying-input-pane? obj)))
               (collapse-object obj))))
        ((interactive-routine-pointerp (current-mode self))
         (let ((ir (current-interactive-routine self)))
           (multiple-value-bind (new-x new-y) (nearest-grid-point (capi:element-interface self) x y)
             (setq x new-x y new-y))
           (capi:with-geometry ir
             (let* ((min-x 0)
                    (pos-x (- x (/ capi:%width% 2)))
                    (max-x (- (width (controller (capi:element-interface self))) capi:%width%))
                    (min-y 0)
                    (pos-y (- y (/ capi:%height% 2)))
                    (max-y (- (height (controller (capi:element-interface self))) capi:%height%))
                    (best-x pos-x)
                    (best-y pos-y))
               (if (< pos-x min-x) (setf best-x min-x))
               (if (< max-x pos-x) (setf best-x max-x))
               (if (< pos-y min-y) (setf best-y min-y))
               (if (< max-y pos-y) (setf best-y max-y))
               (setf best-x (floor best-x) best-y (floor best-y))
               (setf (capi:pinboard-pane-position ir) (values best-x best-y))
               (gp:invalidate-rectangle self)))))))
               
; Process the start of a drag operation
(defun begin-drag (self mx my)
  "Signals the start of a drag operation"
  (setf (dragging? (capi:element-interface self)) t)
  (multiple-value-bind (mx my) (nearest-grid-point (capi:element-interface self) mx my)
    (dolist (act (selected-activities self))
      (multiple-value-bind (ox oy) (capi:pinboard-pane-position act)
        (setf (stored-x act) (- ox mx))
        (setf (stored-y act) (- oy my))
        (setf (moved-pixels act) 0)
))))

; Process movement during a drag operation
(defun perform-drag (self mx my)
  "Updates the display during the progression of a drag"
  (multiple-value-bind (rounded-x rounded-y)
      (nearest-grid-point (capi:element-interface self) mx my)
    (dolist (act (selected-activities self))
      (if (and (stored-x act) (stored-y act))
          (let* ((px (+ rounded-x (stored-x act)))
                 (py (+ rounded-y (stored-y act)))
                 (pw (+ px (app-property 'editor-default-activity-width)))
                 (ph (+ py (app-property 'editor-default-activity-height))))
            (if (or (< px 0)
                    (< py 0)
                    (> pw (width (controller (capi:element-interface self))))
                    (> ph (height (controller (capi:element-interface self)))))
                (return-from perform-drag nil)))))
    (dolist (act (selected-activities self))
      (if (and (stored-x act) (stored-y act) (not (is-displaying-input-pane? act)))
          (let ((px (+ rounded-x (stored-x act)))
                (py (+ rounded-y (stored-y act))))
            (multiple-value-bind (ox oy) (capi:pinboard-pane-position act)
              (incf (moved-pixels act) (+ (abs (- ox px)) (abs (- oy py))))
              (if (> (moved-pixels act) (app-property 'drag-threshold))
                  (progn
                    (setf (dragging act) t)
                    (if (expanded? act)
                        (collapse-in-background act))
                    (setf (expandable act) nil))))
            (setf (capi:pinboard-pane-position act) (values px py))
            (dolist (arrow (edges-in act))
              (move-arrow arrow))
            (dolist (arrow (edges-out act))
              (move-arrow arrow))
            (gp:invalidate-rectangle self)
)))))

; Process the end of a drag operation
(defun end-drag (self mx my)
  "Processes the end of a drag event in the activity layout"
  (setf (dragging? (capi:element-interface self)) nil)
  (dolist (act (selected-activities self))
    (setf (stored-x act) nil)
    (setf (stored-y act) nil)
    (setf (moved-pixels act) nil)
    (setf (expandable act) t)
    (if (is-dragging? act)
        (let ((positions (mapcar #'(lambda (x) (multiple-value-bind (ox oy) (capi:pinboard-pane-position x) (list ox oy))) (selected-activities self)))
              (activities (mapcar #'source (selected-activities self)))
              )
          (move-activities-in-model (controller (capi:element-interface self)) activities positions)
          (mapcar #'(lambda (x) (setf (dragging x) nil)) (selected-activities self)))
      (if (over-activity-pane-p act mx my)
          (multiple-value-bind (mx my) (capi:convert-relative-position self act mx my)
            (activity-pane-clicked act mx my)
            ))))
  (capi:set-pane-focus self))

; Callback for when the user creates a new activity
(defmethod layout-add-activity ((self activity-layout) (node instance-pointer))
  "Callback function for when the user creates a new activity in the model"
  (let ((pane (make-activity-pane-from-source node)))
    (push pane (activities self)))
)

; Callback for when the user deletes an activity
(defmethod layout-delete-activity ((self activity-layout) (node instance-pointer))
  "Callback function for when the user deletes an activity in the model"
  (let ((x (member node (activities self) :key #'source :test #'pointer=)))
    (if x
        (setf (activities self) (remove (car x) (activities self)))))
)

; Callback for when the user connects two activities
(defmethod layout-connect-activities ((self activity-layout) (act1 instance-pointer) (act2 instance-pointer))
  "Callback function for when the user connects two activities in the model"
  (block nil
    (let ((pane1 (car (member act1 (activities self) :key #'source :test #'pointer=)))
          (pane2 (car (member act2 (activities self) :key #'source :test #'pointer=))))
      (if (not pane1) (return (capi:display-message "Problem with callback: Could not find source instance pointer.")))
      (if (not pane2) (return (capi:display-message "Problem with callback: Could not find destination instance pointer.")))
      (if (member pane2 (edges-out pane1) :key #'end)
          (return)
        (let (a ir-type1 ir-type2)
          (setf ir-type1 (ir-type act1))
          (setf ir-type2 (ir-type act2))
          (if (and ir-type1
                   ir-type2
                   (pointer= ir-type1 ir-type2)
                   (connected-in-iroutine (ir-task act1) (ir-task act2) ir-type1))
              (setf a (make-instance 'arrow :start pane1 :end pane2 :ir-color (color ir-type1)))
            (setf a (make-instance 'arrow :start pane1 :end pane2)))
          (push a (edges-out pane1))
          (push a (edges-in pane2))
          (push a (arrows self))
          a))))
)

; Callback for when the user deletes the connection between two activities
(defmethod layout-disconnect-activities ((self activity-layout) (act1 instance-pointer) (act2 instance-pointer))
  "Callback function for when the user disconnects two activities in the model"
  (let ((x1 (member act1 (activities self) :key #'source :test #'pointer=))
        (x2 (member act2 (activities self) :key #'source :test #'pointer=)))
    (if (and x1 x2)
        (progn
          (dolist (arrow (edges-out (car x1)))
            (if (member arrow (edges-in (car x2)))
                (progn
                  (setf (edges-out (car x1)) (remove arrow (edges-out (car x1))))
                  (setf (edges-in (car x2)) (remove arrow (edges-in (car x2))))
                  (setf (arrows self) (remove arrow (arrows self)))))))))
)

; Callback for when an activity should be displayed on the critical path
(defmethod layout-activity-on-critical-path ((self activity-layout) (act instance-pointer) val)
  "Callback function for when an activity should display the critical path bounding box"
  (let ((x (member act (activities self) :key #'source :test #'pointer=)))
    (if x
        (progn
          (on-critical-path val (car x))
          (gp:invalidate-rectangle (car x)))))
)

; Deselects all of the activities in the model
(defun deselect-all-activities (self x y)
  "Deselects all activities currently selected in this layout"
  (declare (ignore x y))
  (dolist (act (selected-activities self))
    (deselect act)))

; Determines if there is an activity pane under the mouse
; position as specified by the (x,y) coordinate
(defmethod activity-pane-under-cursor ((self activity-layout) x y)
  "Determines whether there is an activity pane at the specified (x, y) coordinate and returns that pane, otherwise nil"
  (dolist (pane (activities self))
    (multiple-value-bind (x y) (capi:convert-relative-position self pane x y)
      (capi:with-geometry pane
        (if (and (<= 0 x)
                 (< x capi:%width%)
                 (<= 0 y)
                 (< y capi:%height%))
            (return pane)))))
)

; Updates the controller with knowledge about the types of
; the currently selected activities
(defmethod change-selected-activity-types ((self activity-layout) (type activity-pointer))
  "Changes the type of the selected activities from their current type to the specified type"
  (set-selected-activities-type (controller (capi:element-interface self)) (mapcar #'source (selected-activities self)) type)
)

; Determines whether an object is a pinboard object or not
(defun pinboard-object-p (x)
  "Determines whether an object is a capi:pinboard-object or not"
  (subtypep (type-of x) (find-class 'capi:pinboard-object)))

; Handles the case where the user presses the mouse button down on the activity layout
(defun activity-layout-mouse-down (self x y &optional (shift nil))
  "Processes mouse down events for the activity layout"
  (if (criticality-overlay-enabled? self) (return-from activity-layout-mouse-down))
  (let ((obj (activity-pane-under-cursor self x y))
        (obj2 (capi:pinboard-object-at-position self x y)))
    (if (equal (type-of obj2) 'capi:arrow-pinboard-object)
        (let ((temp (or (member obj2 (arrows self) :key #'normal-arrow) (member obj2 (arrows self) :key #'ir-arrow))))
          (if temp
              (setf obj2 (car temp))
            (setf obj2 nil)))
      (setf obj2 nil))
    (if (and (equal (current-mode self) 'connect)
             (not obj))
        (setf (interaction-mode (capi:element-interface self)) 'select))
    (cond ((and (equal (current-mode self) 'select) obj)
           (if (equalp nil (selected? obj))
               (select obj shift)
             (if shift
                 (deselect obj)))
           (begin-drag self x y))
          ((and (equal (current-mode self) 'select) obj2)
           (if (not (selected obj2))
               (select obj2 shift)
             (if shift
                 (deselect obj2)))
           (begin-drag self x y))
          ((equal (current-mode self) 'select)
           (if (not shift)
               (progn
                 (dolist (pane (selected-activities self))
                   (deselect pane))
                 (dolist (arrow (arrows self))
                   (deselect arrow))))
           (setf (selection-rectangle self) (make-instance 'capi:drawn-pinboard-object
                                                           :x x :y y
                                                           :width 0
                                                           :height 0
                                                           :display-callback #'draw-selection-rectangle))
           (setf (drag-x self) x)
           (setf (drag-y self) y)
           (capi:manipulate-pinboard self (selection-rectangle self) :add-top)
           (setf (polling-process self) (mp:process-run-function "Polling loop" nil #'poll-position self)))
          ((and (equal (current-mode self) 'connect) obj)
           (setf (dragging? (capi:element-interface self)) t)
           (setf (arrow-to-be-placed self) (make-instance 'arrow :start obj))
           (setf (arrows self) (append (arrows self) (list (arrow-to-be-placed self)))))
))
)

; Handles when the mouse moves over the layout and the button has been depressed
(defun activity-layout-mouse-drag (self x y)
  "Processes mouse drag events passed to this activity layout"
  (if (criticality-overlay-enabled? self) (return-from activity-layout-mouse-drag))
  (let ((obj (activity-pane-under-cursor self x y)))
    (cond ((and (equal (current-mode self) 'select) obj (not (selection-rectangle self)))
           (if (not (and (stored-x obj) (stored-y obj))) (return-from activity-layout-mouse-drag))
           (if (is-displaying-input-pane? obj) (return-from activity-layout-mouse-drag))
           (perform-drag self x y))
          ((equal (current-mode self) 'select)
           (if (selection-rectangle self)
               (cond
                ((and (> x (drag-x self)) (> y (drag-y self)))
                 (setf (capi:pinboard-pane-position (selection-rectangle self))
                       (values (drag-x self) (drag-y self)))
                 (setf (capi:pinboard-pane-size (selection-rectangle self))
                       (values (- x (drag-x self)) (- y (drag-y self)))))
                
                ((and (> x (drag-x self)) (<= y (drag-y self)))
                 (setf (capi:pinboard-pane-position (selection-rectangle self))
                       (values (drag-x self) y))
                 (setf (capi:pinboard-pane-size (selection-rectangle self))
                       (values (- x (drag-x self)) (- (drag-y self) y))))
                
                ((and (<= x (drag-x self)) (> y (drag-y self)))
                 (setf (capi:pinboard-pane-position (selection-rectangle self))
                       (values x (drag-y self)))
                 (setf (capi:pinboard-pane-size (selection-rectangle self))
                       (values (- (drag-x self) x) (- y (drag-y self)))))
                
                ((and (<= x (drag-x self)) (<= y (drag-y self)))
                 (setf (capi:pinboard-pane-position (selection-rectangle self))
                       (values x y))
                 (setf (capi:pinboard-pane-size (selection-rectangle self))
                       (values (- (drag-x self) x) (- (drag-y self) y))))
                )
             (perform-drag self x y)))
          ((and (arrow-to-be-placed self) (equal (current-mode self) 'connect) obj)
           (setf (end (arrow-to-be-placed self)) obj))
          ((and (arrow-to-be-placed self) (equal (current-mode self) 'connect))
           (setf (end (arrow-to-be-placed self)) nil)
           (move-arrow-end (arrow-to-be-placed self) x y))
)))

; Takes any items in the selection rectangle and adds them to the current selection
(defun extend-selection (self x y w h)
  "Extends the current selection by taking all elements within the selection rectangle and adding them to the selected set"
  (dolist (act (activities self))
    (capi:with-geometry act
      (if (intersect-rectangles x y (+ x w) (+ y h) capi:%x% capi:%y% (+ capi:%x% capi:%width%) (+ capi:%y% capi:%height%))
          (if (not (member act (selected-activities self)))
              (progn
                (push act (selected-activities self))
                (highlight t act)
                (setf (selected? act) t))))))
  (dolist (arrow (arrows self))
    (let ((start (start arrow))
          (end (end arrow)))
      (multiple-value-bind (x1 y1) (center start)
        (multiple-value-bind (x2 y2) (center end)
          (if (intersect-lines x y (+ x w) (+ y h) x1 y1 x2 y2)
              (setf (selected arrow) t))))))
  (update-activity-layout self)
)

; Handles when the mouse is released above the activity layout
(defun activity-layout-mouse-up (self x y &optional shift)
  "Processes when the user releases the mouse button within the activity layout"
  (declare (ignore shift))
  (if (criticality-overlay-enabled? self) (return-from activity-layout-mouse-up))
  (let ((obj (activity-pane-under-cursor self x y))
        (obj2 (capi:pinboard-object-at-position self x y)))
    (cond ((equal (current-mode self) 'select)
           (if (selection-rectangle self)
               (progn
                 (if (polling-process self) (mp:process-kill (polling-process self)))
                 (setf (polling-process self) nil)
                 (multiple-value-bind (x y) (capi:pinboard-pane-position (selection-rectangle self))
                   (multiple-value-bind (w h) (capi:pinboard-pane-size (selection-rectangle self))
                     (extend-selection self x y w h)))
                 (capi:manipulate-pinboard self (selection-rectangle self) :delete)
                 (setf (selection-rectangle self) nil)))
           (if (or obj obj2)
               (end-drag self x y)
             (if (not (is-dragging? self))
                 (deselect-all-activities self x y))))
          ((equal (current-mode self) 'note)
           (if (and obj2 (multiline-pinboard-objectp obj2))
               (multiple-value-bind (text answer) (prompt-with-text-area "Note:" :default (multiline-pinboard-object-text obj2))
                 (if answer
                     (set-model-note-text (controller (capi:element-interface self)) (source obj2) text)))
             (multiple-value-bind (text answer) (prompt-with-text-area "Note:" :default "")
               (if answer
                   (create-model-note (controller (capi:element-interface self)) text x y)))))
          ((activity-pointerp (current-mode self))
           (multiple-value-bind (success error)
               (ignore-errors
                 (progn
                   (if (activity-pane-under-cursor self x y) 
                       (progn
                         (setf (interaction-mode (capi:element-interface self)) 'select)
                         (select (activity-pane-under-cursor self x y) nil)
                         (return-from activity-layout-mouse-up)))
                   (multiple-value-bind (x y) (nearest-grid-point (capi:element-interface self) (floor (- x (/ (app-property 'editor-default-activity-width) 2))) (floor (- y (/ (app-property 'editor-default-activity-height) 2))))
                     (create-activity-in-model (controller (capi:element-interface self)) (current-mode self)
                                               :x x
                                               :y y))))
             (declare (ignore success))
             (if error (capi:display-message "Error: ~A" error))))
          ((interactive-routine-pointerp (current-mode self))
           (let* ((object (current-interactive-routine self))
                  (iroutine (current-mode self))
                  (activities nil)
                  (curact nil)
                  (edges-out (make-hash-table))
                  (edges-in (make-hash-table))
                  (extra-text (append-string object)))
             (capi:with-geometry object
               ;;; Create the new activities
               (dolist (task (task-list iroutine))
                 (let ((x (+ capi:%x% (offset-x task)))
                       (y (+ capi:%y% (offset-y task))))
                   (setq curact (create-activity-in-model 
                                 (controller (capi:element-interface self)) (operator-type task)
                                 :x x :y y
                                 :ir-type iroutine :ir-task task :ir-append (copy-seq extra-text)
                                 :distribution (distribution task) :params (copy-list (parameters task))
                                 :label (if extra-text (format nil "~A ~A" (label task) extra-text) (copy-seq (label task)))))
                   (push curact activities)))
               (capi:manipulate-pinboard self object :delete)
               (setq activities (reverse activities))
               (dotimes (i (length activities))
                 (let ((act (nth i activities))
                       (task (nth i (task-list iroutine)))
                       (temp-list nil))
                   (setq temp-list (make-list (length (edges-out task)) :initial-element nil))
                   (dotimes (j (length temp-list))
                     (setf (nth j temp-list) (car (member (nth j (edges-out task)) activities :key #'ir-task :test #'pointer=))))
                   (setf (gethash (pointer act) edges-out) temp-list)
                   (setq temp-list (make-list (length (edges-in task)) :initial-element nil))
                   (dotimes (j (length temp-list))
                     (setf (nth j temp-list) (car (member (nth j (edges-in task)) activities :key #'ir-task :test #'pointer=))))
                   (setf (gethash (pointer act) edges-in) temp-list)
                   ))
               (dolist (source activities)
                 (dolist (target (gethash (pointer source) edges-out))
                   (connect-activities-in-model (controller (capi:element-interface self))
                                                source
                                                target)))))
           (setf (interaction-mode (capi:element-interface self)) 'select))
          ((and (arrow-to-be-placed self) (equal (current-mode self) 'connect))
           (setf (dragging? (capi:element-interface self)) nil)
           (if (or (null (end (arrow-to-be-placed self)))
                   (eq (start (arrow-to-be-placed self)) (end (arrow-to-be-placed self))))
               (progn
                 (setf (arrows self) (remove (arrow-to-be-placed self) (arrows self)))
                 (setf (arrow-to-be-placed self) nil)
                 (if (activity-pane-under-cursor self x y)
                     (progn
                       (setf (interaction-mode (capi:element-interface self)) 'select)
                       (select (activity-pane-under-cursor self x y) nil))
                   )
                 (gp:invalidate-rectangle self))
             (progn
               (push (arrow-to-be-placed self) (edges-out (start (arrow-to-be-placed self))))
               (push (arrow-to-be-placed self) (edges-in (end (arrow-to-be-placed self))))
               (connect-activities-in-model (controller (capi:element-interface self)) (source (start (arrow-to-be-placed self))) (source (end (arrow-to-be-placed self))))
               (setf (arrow-to-be-placed self) nil)
               (gp:invalidate-rectangle self))))
)))

; Handles when the user presses the delete key
(defun activity-layout-delete (self x y char)
  "Deletes the selection if the user presses the delete key"
  (declare (ignore x y char))
  (dolist (act (selected-activities self))
    (dolist (arrow (edges-in act))
      (disconnect-activities-in-model (controller (capi:element-interface self)) (source (start arrow)) (source act)))
    (dolist (arrow (edges-out act))
      (disconnect-activities-in-model (controller (capi:element-interface self)) (source act) (source (end arrow))))
    (delete-activity-in-model (controller (capi:element-interface self)) (source act)))
  (let ((arrows (remove-if-not #'selected (arrows self))))
    (dolist (a arrows)
      (disconnect-activities-in-model (controller (capi:element-interface self)) (source (start a)) (source (end a)))))
)

; Generates a drawn pinboard object which displays the background grid
(defmethod add-grid ((self activity-layout) width height)
  "Creates a grid object for the activity layout"
  (let ((obj (make-instance 'capi:drawn-pinboard-object
                            :x 0
                            :y 0
                            :width width
                            :height height
                            :display-callback #'draw-grid)))
    (capi:manipulate-pinboard self obj :add-bottom)
    (setf (grid self) obj)
    obj)
)

; The method used by the grid pinboard object to draw the grid
(defun draw-grid (pane self x y w h)
  "Draws the grid for the activity layout"
  (declare (ignore self))
  (if (criticality-overlay-enabled? (capi:element-interface pane))
      (let ((background (color:make-rgb 0.2 0.2 0.2)))
        (gp:draw-rectangle pane x y w h :foreground background :filled t)
        )
    (let ((interface (capi:element-interface pane))
          (light-gray (color:make-rgb 0.9 0.9 0.9))
          (dark-gray (color:make-rgb 0.75 0.75 0.75)))
      (dotimes (i (ceiling (/ h (grid-size (controller interface)))))
        (let ((y (+ y (* i (grid-size (controller interface))))))
          (gp:draw-line pane x y (+ x w) y :foreground light-gray)))
      (dotimes (i (ceiling (/ h (* 10 (grid-size (controller interface))))))
        (let ((y (+ y (* i 10 (grid-size (controller interface))))))
          (gp:draw-line pane x y (+ x w) y :foreground dark-gray)))
      
      (dotimes (i (ceiling (/ w (grid-size (controller interface)))))
        (let ((x (+ x (* i (grid-size (controller interface))))))
          (gp:draw-line pane x y x (+ y h) :foreground light-gray)))
      (dotimes (i (ceiling (/ w (* 10 (grid-size (controller interface))))))
        (let ((x (+ x (* i 10 (grid-size (controller interface))))))
          (gp:draw-line pane x y x (+ y h) :foreground dark-gray)))
)))

; Shift clicking behaves differently from normal clicking
(defun activity-layout-mouse-down-shift (self x y)
  "Processes mouse down when the shift key is depressed"
  (activity-layout-mouse-down self x y t))

; Shift clicking behaves differently from normal clicking
(defun activity-layout-mouse-up-shift (self x y)
  "Handles mouse up when the shift key is depressed"
  (activity-layout-mouse-up self x y t))

; Draws the selection rectangle on the pinboard
(defun draw-selection-rectangle (pane obj x y w h)
  "Draws the selection rectangle on the activity layout"
  (declare (ignore x y w h))
  (capi:with-geometry obj
    (gp:draw-rectangle
     pane capi:%x% capi:%y% (1- capi:%width%) (1- capi:%height%)
     :foreground :black
     :dashed t
     :filled nil)))

; If the user presses escape and there is an activity pane displaying an input pane
; then it should be deactivated.
(defun activity-layout-escape (self x y char)
  "Handles when the user presses the Escape key while an activity pane is in edit mode"
  (dolist (act (selected-activities self))
    (if (is-displaying-input-pane? act)
        (activity-pane-escape act x y char)))
)

; Set the current mode to selection
(defun activity-layout-select (self x y char)
  "Changes the current mode of the activity layout to selection"
  (declare (ignore x y char))
  (setf (interaction-mode (capi:element-interface self)) 'select))

; Set the current mode to connection
(defun activity-layout-connect (self x y char)
  "Changes the current mode of the activity layout to connector mode"
  (declare (ignore x y char))
  (setf (interaction-mode (capi:element-interface self)) 'connect))

; Polls the position of the cursor and updates the selection rectangle
(defun poll-position (self)
  "Polls for the position of the mouse every 50 ms to facilitate the drawing of the selection rectangle"
  (do ()
      (nil)
    (sleep 0.05)
    (multiple-value-bind (val err)
        (ignore-errors (multiple-value-bind (x y) (capi:current-pointer-position :relative-to self)
                         (capi:apply-in-pane-process self #'activity-layout-mouse-drag self x y)
                         ))
      (if err (return))))
)

; Respond to a double click; for note editing
(defun activity-layout-double-click (self x y)
  "Processes when the user double-clicks on the layout to perform note editing"
  (let ((pane (capi:pinboard-object-at-position self x y)))
    (if (member pane (notes self))
        (multiple-value-bind (text answer) (prompt-with-text-area "Note:" :default (multiline-pinboard-object-text pane))
          (if answer
              (set-model-note-text (controller (capi:element-interface self)) (source pane) text)))))
)

; Input model for the activity-layout's capi:output-pane superclass
(defparameter *pinboard-layout-input-model*
  '(((:motion) activity-layout-mouse-move)
    ((:button-1 :press :shift) activity-layout-mouse-down-shift)
    ((:button-1 :press) activity-layout-mouse-down)
    ((:button-1 :second-press) activity-layout-double-click)
;    ((:button-1 :motion) activity-layout-mouse-drag)
;    ((:button-1 :motion :shift) activity-layout-mouse-drag)
    ((:button-1 :release) activity-layout-mouse-up)
    ((:button-1 :release :shift) activity-layout-mouse-up-shift)
    ((:gesture-spec "Delete") activity-layout-delete)
    ((:gesture-spec "Backspace") activity-layout-delete)
    ((:gesture-spec #\Escape) activity-layout-escape)
    ((:gesture-spec #\s :control) activity-layout-select)
    ((:gesture-spec #\c :control) activity-layout-connect)
))

; Sets the label of the currently selected activities
(defmethod (setf activity-labels) (val (self activity-layout))
  "Sets the label of the currently selected activities"
  (let ((val (format nil "~A" val)))
    (set-selected-activities-label (controller (capi:element-interface self)) (mapcar #'source (selected-activities self)) val)
))

; Sets the distribution of the currently selected activities
(defmethod (setf activity-distributions) ((val distribution-pointer) (self activity-layout))
  "Sets the distribution of the currently selected activities"
  (set-selected-activities-distribution (controller (capi:element-interface self)) (mapcar #'source (selected-activities self)) val))

; Sets the distribution of the currently selected activities (given the name of a distribution)
(defmethod (setf activity-distributions) ((val string) (self activity-layout))
  "Sets the distribution of the currently selected activities"
  (setf (activity-distributions self) (find-distribution val)))

; Sets the value for the ith parameter for the currently selected activities
(defmethod (setf activity-param-values) (val (i number) (self activity-layout))
  "Sets the value for the ith parameter for the currently selected activities"
  (let ((val (if (equal val "") "0" (format nil "~A" val))))
    (set-selected-activities-parameter (controller (capi:element-interface self)) (mapcar #'source (selected-activities self)) i val))
)

; Gets the current scroll position of the output pane
(defun get-scroll-position (pane dir)
  "Gets the current scroll position of the output pane"
  (let* ((gc (capi::geometry-cache pane))
         (x (slot-value gc 'capi::scroll-x))
         (y (slot-value gc 'capi::scroll-y)))
    (cond ((equal dir :horizontal) x)
          ((equal dir :vertical) y)
          (t 0))))

; Raises an SANLab event when the output pane scrolls
(defmethod send-scroll-event ((self activity-layout) direction operation amount &key interactive)
  "Raises an event when the output pane scrolls"
  (cond (interactive
         (fire-event-listeners (controller (capi:element-interface self)) 'editor-view-scrolled (list (get-scroll-position self :horizontal) (get-scroll-position self :vertical)))))
)
