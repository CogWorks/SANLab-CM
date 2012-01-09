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

; Since the activity pane needs to know about the arrow class
; define an empty class here that will be overwritten by the
; arrow.lisp file
(if (equalp nil (find-class 'arrow nil))
(defclass arrow () ())
)

; A pinboard layout that represents an activity node in the graph
(defclass activity-pane (capi:pinboard-layout)
  ((expansion-timer-value :initform nil :accessor expansion-timer-value)
   (collapse-timer-value :initform nil :accessor collapse-timer-value)
   (expanded :initform nil :reader expanded? :writer expanded)
   (expandable-p :initform t :reader expandable-p :writer (setf expandable))
   (dragging? :initform nil :reader is-dragging? :writer (setf dragging))
   (is-displaying-input-pane :initform nil :reader is-displaying-input-pane? :writer displaying-input-pane)
   (back-color :initform #(:RGB 1.0 0.7 1.0) :initarg :background-color :accessor background-color)
   (ir-color :initform nil :initarg :ir-color :accessor ir-color)
   (is-highlighted :initform nil :reader is-highlighted?); :writer highlight)
   (is-on-cp :initform nil :reader is-on-critical-path? :writer on-critical-path)
   (is-in-ir :initform nil :reader is-in-ir? :writer in-interactive-routine)
   (drawn-background :initform (make-instance 'capi:drawn-pinboard-object :x 0 :y 0 :visible-min-width 200 :visible-min-height 82 :visible-max-height 82) :accessor drawn-background)
   (name-label :initform (make-instance 'capi:item-pinboard-object :x 10 :y 14 :visible-min-width 180 :visible-max-width 180 :text "Activity Description") :accessor name-label)
   (name-textbox :initform (make-instance 'capi:text-input-pane :x 8 :y #+win32 12 #+cocoa 1 :visible-min-width 180 :visible-max-width 180 :visible-min-height #+win32 18 #+cocoa 32 :internal-min-height #+win32 18 #+cocoa 32 :external-min-height #+win32 18 #+cocoa 32 :visible-max-height #+win32 18 #+cocoa t :text "Activity Description" :editing-callback #'standard-text-pane-editing-callback) :accessor name-textbox)
   (dist-label :initform (make-instance 'capi:item-pinboard-object :x 10 :y 34 :visible-min-width 180 :visible-max-width 180 :text "Dist:  Gaussian") :accessor dist-label)
   (dist-combo :initform (make-distribution-option-pane) :accessor dist-combo)
   (param-overview-label :initform (make-instance 'capi:item-pinboard-object :x 10 :y 54 :visible-min-width 180 :visible-max-width 180 :text "0, 0") :accessor param-overview-label)
   (param-labels :initform (build-label-list "Gaussian" (get-param-count "Gaussian") 0) :accessor param-labels)
   (param-textboxes :initform (build-textbox-list "Gaussian" (get-param-count "Gaussian") 0) :accessor param-textboxes)
   (param-values :initform '("0" "0") :initarg :param-values :accessor param-values)
   (visible-params :initform 0 :accessor visible-params)
   (criticality-pane :initform (make-instance 'capi:drawn-pinboard-object :x 0 :y 0 :visible-min-width 200 :visible-min-height 82 :visible-max-height 82 :display-callback #'render-criticality) :accessor criticality-pane)
   (stored-x :initform nil :accessor stored-x)
   (stored-y :initform nil :accessor stored-y)
   (edges-in :initform nil :accessor edges-in)
   (edges-out :initform nil :accessor edges-out)
   (moved-pixels :initform nil :accessor moved-pixels)
   (source :initform nil :initarg :source :accessor source)
   (expansion-process :initform nil :accessor expansion-process)
   (collapse-process :initform nil :accessor collapse-process)
   (distribution :initform nil :initarg :distribution :accessor distribution))
  (:default-initargs
   :visible-min-width 200
   :visible-min-height 82
   :visible-max-height 82
   :input-model *activity-pane-input-model*)
  (:documentation "A visual representation of an activity network node in the editor")
)

; Updates the activity pane from the underlying model
(defmethod update-activity-pane ((self activity-pane))
  "Updates the activity pane from the underlying model"
  ; Update back-color
  (setf (background-color self) (color (source self)))
  ; Update is-in-ir?
  (in-interactive-routine (ir-type (source self)) self)
  ; Update ir-color
  (if (ir-type (source self))
      (setf (ir-color self) (highlight-color (ir-type (source self))))
    (setf (ir-color self) nil))
  ; Update name-label
  (setf (capi:item-text (name-label self)) (format nil "~A: ~A" (abbreviated-name (activity-type (source self))) (label (source self))))
  ; Update name-textbox
  (setf (capi:text-input-pane-text (name-textbox self)) (format nil "~A" (label (source self))))

  (setf (param-values self) (copy-list (parameters (source self))))
  (if (not (equal (distribution self) (distribution (source self))))
      (setf (distribution self) (distribution (source self)))
    (update-param-labels self))
  ; Update dist-label
  (setf (capi:item-text (dist-label self)) (printname (distribution self)))
  ; Update dist-combo
  (setf (capi:choice-selection (dist-combo self)) (position (printname (distribution self)) (capi:collection-items (dist-combo self)) :test #'equal))
  ; Update x
  ; Update y
  (setf (capi:pinboard-pane-position self) (values (x (source self)) (y (source self))))
  ; Update description
  (if (expanded? self)
      (progn
        (setf (capi:layout-description self) (append (list (drawn-background self) (name-label self) (dist-label self)) (param-labels self)))
        (resize-to-fit self)
        (displaying-input-pane nil self)
        (capi:set-pane-focus self))
    (setf (capi:layout-description self) (list (drawn-background self) (name-label self) (dist-label self) (param-overview-label self))))
  ; Force a redraw
  (capi:with-geometry self
    (capi:redraw-pinboard-layout self capi:%x% capi:%y% capi:%width% capi:%height%))
)

; Renders the activity pane when the criticality overlay is enabled
(defun render-criticality (pane self x y width height)
  "Renders the activity pane when the criticality overlay is enabled"
  (declare (ignore self))
  ; Conditional compliation because Windows doesn't deal with alpha transparency correctly
  #+win32
  (let (the-image)
    (capi:with-geometry self
      (gp:with-pixmap-graphics-port (port pane
                                          capi:%width%
                                          capi:%height%
                                          :clear t
                                          :background (color:make-rgb 1.0 1.0 1.0 0.0))
        (gp:draw-rectangle port
                           0 0
                           capi:%width% capi:%height%
                           :foreground #(:RGB 0.0 0.0 0.0)
                           :filled t)
        (setq the-image (gp:make-image-from-port port))
        (gp:draw-image pane
                       the-image
                       capi:%x% capi:%y%
                       :global-alpha (- 1 (criticality (source pane) (last-ct-tracker (capi:element-interface pane)))))
        (gp:free-image port the-image))))
  #+cocoa
  (gp:draw-rectangle pane x y width height :foreground (color:make-rgb 0.0 0.0 0.0 (- 1 (criticality (source pane) (last-ct-tracker (capi:element-interface pane))))) :filled t)
)

; Any attempts to call criticality-overlay-enabled? should be passed to the owner interface
(pass-all criticality-overlay-enabled? activity-pane capi:element-interface)

; Sets whether the activity pane is highlighted or not
(defmethod highlight (val (self activity-pane))
  "Sets whether the activity pane is highlighted or not"
  (setf (slot-value self 'is-highlighted) val)
  (cond (val
         (if (equalp nil (member self (selected-activities (capi:element-parent self))))
             (push self (selected-activities (capi:element-parent self)))))
        (t
         (if (equalp nil (equalp nil (member self (selected-activities (capi:element-parent self)))))
             (setf (selected-activities (capi:element-parent self)) (remove self (selected-activities (capi:element-parent self)))))))
)

; Sets the background color of the activity pane
(defmethod (setf background-color) :after (val (self activity-pane))
  "Invalidates the rectangle after the background color has changed"
  (setf (capi:simple-pane-background self) val)
  (gp:invalidate-rectangle self)
  (gp:invalidate-rectangle (drawn-background self))
)

; Builds the list of parameter labels to display in the pane
(defun build-label-list (dist num ie)
  "Builds the list of parameter labels to display in the pane"
  (let ((res nil)
        (temp nil))
    (dotimes (i num)
      (setf temp (make-instance 'capi:item-pinboard-object :x 10 :y (+ 54 (* i 20)) :visible-min-width 180 :visible-max-width 180 :text (format nil "~A: ~A" (get-param-name dist i) ie)))
      (push temp res))
    (reverse res)))

; Builds the list of textboxes, one per parameter
(defun build-textbox-list (dist num ie)
  "Builds the list of textboxes, one per parameter, for the pane"
  (declare (ignore dist))
  (let ((res nil)
        (temp nil))
    (dotimes (i num)
      (setf temp (make-instance 'capi:text-input-pane :x 8 :y (+ #+win32 52 #+cocoa 41 (* i 20)) :visible-min-width 180 :visible-max-width 180 :visible-min-height #+win32 18 #+cocoa 32 :internal-min-height #+win32 18 #+cocoa 32 :external-min-height #+win32 18 #+cocoa 32 :visible-max-height #+win32 18 #+cocoa t :text (format nil "~A" ie) :editing-callback #'standard-text-pane-editing-callback))
      (push temp res))
    (reverse res)))

; Updates the parameter overview label displayed when the pane is collapsed
(defun update-param-overview-label (obj)
  "Updates the parameter overview label displayed when the pane is collapsed"
  (let ((temp (format nil "~A" (first (param-values obj)))))
    (dolist (x (cdr (param-values obj)))
      (setf temp (format nil "~A, ~A" temp x)))
    (setf (capi:item-text (param-overview-label obj)) temp)
))

; Updates the individual parameter labels
(defun update-param-labels (self)
  "Updates the individual parameter labels in the pane"
  (dotimes (i (length (param-values self)))
    (setf (capi:item-text (nth i (param-labels self))) (format nil "~A: ~A" (get-param-name (distribution self) i) (nth i (param-values self))))
    (setf (capi:text-input-pane-text (nth i (param-textboxes self))) (nth i (param-values self))))
  (update-param-overview-label self)
)

; The background timer routine that expands the activity pane if moused over for a minimum amount of time
(defun expansion-timer (obj)
  "Background timer function that expands an activity pane on completion"
  (ignore-errors
  (loop
   (if (and (expansion-timer-value obj) (> 0 (expansion-timer-value obj))) (return))
   (sleep (app-property 'expand-step))
   (if (expansion-timer-value obj)
       (decf (expansion-timer-value obj) (app-property 'expand-step))
     (progn
       (setf (expansion-process obj) nil)
       (return-from expansion-timer)))))
  (if (expansion-timer-value obj)
      (mp:process-run-function "Expansion Operation" '() #'expansion-function obj))
  (setf (expansion-process obj) nil)
)

; The background timer routine that expands the activity pane if the mouse leaves for a minimum amount of time
(defun collapse-timer (obj)
  "Background timer function that collapses an activity pane on completion"
  (ignore-errors
  (loop
   (if (and (collapse-timer-value obj) (> 0 (collapse-timer-value obj))) (return))
   (sleep (app-property 'expand-step))
   (if (collapse-timer-value obj)
       (decf (collapse-timer-value obj) (app-property 'expand-step))
     (progn
       (setf (collapse-process obj) nil)
       (return-from collapse-timer)))))
  (if (collapse-timer-value obj)
      (mp:process-run-function "Collapse Operation" '() #'collapse-function obj))
  (setf (collapse-process obj) nil)
)

; Calls expand-timer as a separate mp:process
(defun expand-in-background (obj)
  "Calls expand-timer as a separate mp:process"
  (if (not (selected? obj)) (return-from expand-in-background nil))
  (if (collapse-process obj)
      (progn
        (mp:process-kill (collapse-process obj))
        (setf (collapse-process obj) nil)))
  (if (or (not (expansion-process obj)) (mp:process-alive-p (expansion-process obj)))
      (setf (expansion-process obj) (mp:process-run-function "Expansion Timer" '() #'expansion-timer obj)))
)

; Calls collapse-timer as a separate mp:process
(defun collapse-in-background (obj)
  "Calls collapse-timer as a separate mp:process"
  (if (expansion-process obj)
      (progn
        (mp:process-kill (expansion-process obj))
        (setf (expansion-process obj) nil)))
  (if (or (not (collapse-process obj)) (mp:process-alive-p (collapse-process obj)))
      (setf (collapse-process obj) (mp:process-run-function "Collapse Timer" '() #'collapse-timer obj))))

; Expands the activity pane to show all parameters
(defun expansion-function (obj)
  "Expands the activity pane to show all parameters"
  (let ((height (+ 60 (* (length (param-labels obj)) 22)))
        (old-height (capi:with-geometry obj capi:%height%)))
    (expanded t obj)
    (setf (expansion-timer-value obj) nil)
    (ignore-errors
    (do ((i old-height (+ i (/ (- height old-height) 10))))
        ((= i height))
      (capi:apply-in-pane-process obj #'capi:set-geometric-hint obj :visible-min-height i)
      (capi:apply-in-pane-process obj #'capi:set-geometric-hint obj :visible-max-height i)
      (capi:apply-in-pane-process obj #'capi:set-geometric-hint (drawn-background obj) :visible-min-height i)
      (capi:apply-in-pane-process obj #'capi:set-geometric-hint (drawn-background obj) :visible-max-height i)
      (capi:with-geometry obj
        (capi:apply-in-pane-process obj #'capi:redraw-pinboard-layout obj capi:%x% capi:%y% capi:%width% capi:%height%))
      (sleep (/ (app-property 'expand-duration) 10))))
    (capi:apply-in-pane-process obj #'(setf capi:layout-description) (remove (param-overview-label obj) (capi:layout-description obj)) obj)
    (capi:apply-in-pane-process obj #'(setf capi:layout-description) (append (capi:layout-description obj) (copy-list (param-labels obj))) obj)
))

; Resizes the activity pane to fit all of its parameters
(defun resize-to-fit (obj)
  "Resizes the activity pane to fit all of its parameters"
  (let ((height (+ 60 (* (length (param-labels obj)) 22))))
    (ignore-errors
      (capi:apply-in-pane-process obj #'capi:set-geometric-hint obj :visible-min-height height)
      (capi:apply-in-pane-process obj #'capi:set-geometric-hint obj :visible-max-height height)
      (capi:apply-in-pane-process obj #'capi:set-geometric-hint (drawn-background obj) :visible-min-height height)
      (capi:apply-in-pane-process obj #'capi:set-geometric-hint (drawn-background obj) :visible-max-height height))))

; Collapses the activity pane to hide all of its parameters
(defun collapse-function (obj)
  "Collapses the activity pane to hide all of its parameters"
  (let ((height 82)
        (old-height (capi:with-geometry obj capi:%height%)))
    (expanded nil obj)
    (setf (collapse-timer-value obj) nil)
    (let ((temp (capi:layout-description obj)))
      (mapcar #'(lambda (x) (setf temp (remove x temp))) (param-labels obj))
      (setf (capi:layout-description obj) temp))
    (capi:apply-in-pane-process obj #'(setf capi:layout-description) (append (capi:layout-description obj) (list (param-overview-label obj))) obj)
    (ignore-errors
    (do ((i old-height (+ i (/ (- height old-height) 10))))
        ((= i height))
      (capi:apply-in-pane-process obj #'capi:set-geometric-hint obj :visible-min-height i)
      (capi:apply-in-pane-process obj #'capi:set-geometric-hint obj :visible-max-height i)
      (capi:apply-in-pane-process obj #'capi:set-geometric-hint (drawn-background obj) :visible-min-height i)
      (capi:apply-in-pane-process obj #'capi:set-geometric-hint (drawn-background obj) :visible-max-height i)
      (capi:with-geometry obj
        (capi:apply-in-pane-process obj #'capi:redraw-pinboard-layout obj capi:%x% capi:%y% capi:%width% capi:%height%))
      (sleep (/ (app-property 'expand-duration) 10))))
    (let ((x (capi:element-parent obj)))
      (capi:with-geometry x
        (capi:apply-in-pane-process obj #'capi:redraw-pinboard-layout x capi:%x% capi:%y% capi:%width% capi:%height%)))
))

; If selected? is called on the activity pane, pass the call up to the source instance pointer
(pass-all selected? activity-pane instance-pointer :accessor source)

; Creates an activity pane from an instance in the underlying model
(defmethod make-activity-pane-from-source ((src instance-pointer))
  "Creates an activity pane from an instance in the underlying model"
  (let ((act (make-instance 'activity-pane
                            :background-color (color src)
                            :ir-color (ir-color src)
                            :param-values (mapcar #'(lambda (x) (format nil x)) (parameters src))
                            :x (x-position src)
                            :y (y-position src)
                            :source src
                            :distribution (distribution src))))
    (if (ir-color src)
        (in-interactive-routine t act))
    (setf (capi:item-text (name-label act)) (format nil "~A: ~A" (abbreviated-name (activity-type src)) (label src)))
;    (setf (capi:collection-items (dist-combo act)) (list-all-distributions))
    (setf (capi:choice-selection (dist-combo act)) (position (printname (distribution src)) (capi:collection-items (dist-combo act)) :test #'equalp))
    (setf (capi:item-text (dist-label act)) (format nil "Dist: ~A" (printname (distribution src))))
    (setf (distribution act) (distribution src))
    act)
)

; Returns whether the pane represents a selected activity
(defmethod is-selected? ((pane activity-pane))
  "Returns whether the pane represents a selected activity"
  (member pane (selected-activities (capi:element-parent pane))))

; Creates an anonymous callback function for a textbox
(defun text-change-callback-generator (label textbox pane)
  "Creates an anonymous callback function for a textbox"
  #'(lambda (text intf) (set-param-label-text text label textbox pane intf)))

; Sets the distribution of the activity pane
(defmethod (setf distribution) :after (val (obj activity-pane))
  "Sets the distribution of the activity pane"
  (let* ((param-count (get-param-count val))
         (new-param-list (make-list param-count :initial-element "0"))
         (old-param-list (param-values obj))
         (desc (copy-list (capi:layout-description obj))))
    (mapcar #'(lambda (x) (setf desc (remove x desc))) (param-labels obj))
    (dotimes (i (min param-count (length old-param-list)))
      (setf (nth i new-param-list) (nth i old-param-list)))
    (setf (param-values obj) new-param-list)
    (setf (param-labels obj) (build-label-list val param-count 0))
    (setf (param-textboxes obj) (build-textbox-list val param-count 0))
    (do ((i 0 (1+ i))
         (x (param-labels obj) (cdr x))
         (y (param-textboxes obj) (cdr y)))
        ((= i param-count))
      (setf (capi:item-text (car x)) (format nil "~A: ~A" (get-param-name val i) (nth i new-param-list)))
      (setf (capi:text-input-pane-text (car y)) (format nil "~A" (nth i new-param-list)))
      (setf (capi:text-input-pane-callback (car y)) (text-change-callback-generator i (car y) obj)))
    (if (expanded? obj)
        (setf (capi:layout-description obj) (append desc (copy-list (param-labels obj))))
      (setf (capi:layout-description obj) desc))
    (update-param-overview-label obj)
))

; Redraws the pinboard after the highlight value has changed
(defmethod highlight :after (val (obj activity-pane))
  "Redraws the pinboard after the highlight value has changed"
  (capi:apply-in-pane-process obj #'capi:redraw-pinboard-object (drawn-background obj)))

; Redraws the pinboard after the on-critical-path value has changed
(defmethod on-critical-path :after (val (obj activity-pane))
  "Redraws the pinboard after the on-critical-path value has changed"
  (capi:apply-in-pane-process obj #'capi:redraw-pinboard-object (drawn-background obj)))

; Redraws the pinboard after it is added to an interactive routine
(defmethod in-interactive-routine :after (val (obj activity-pane))
  "Redraws the pinboard after it is added to an interactive routine"
  (capi:apply-in-pane-process obj #'capi:redraw-pinboard-object (drawn-background obj)))

; Redraws the pinboard after the interactive routine color changes
(defmethod (setf ir-color) :after (val (obj activity-pane))
  "Redraws the pinboard after the interactive routine color changes"
  (if (is-in-ir? obj)
      (capi:apply-in-pane-process obj #'capi:redraw-pinboard-object (drawn-background obj))))

; Sets the background color for a pinboard object
(defmethod set-background-for-pinboard-object (val (obj capi:pinboard-object))
  "Sets the background color for a pinboard object"
  (let ((pos (position :background (capi:pinboard-object-graphics-args obj))))
    (if (and (not val) (not pos)) (return-from set-background-for-pinboard-object))
    (if (not pos)
        (setf (capi:pinboard-object-graphics-args obj) (cons :background (cons val (capi:pinboard-object-graphics-args obj)))))
    (if pos
        (if val
            (setf (nth (1+ pos) (capi:pinboard-object-graphics-args obj)) val)
          (setf (nth (1+ pos) (capi:pinboard-object-graphics-args obj)) nil)))
    (capi:redraw-pinboard-object obj nil)))

; Checks if an object is a pinboard object
(defmethod is-pinboard-object-p ((obj capi:pinboard-object))
  "Checks if an object is a pinboard object"
  t)
(defmethod is-pinboard-object-p (obj)
  nil)

; Checks if an object is an activity pane
(defmethod activity-pane-p ((obj activity-pane))
  "Checks if an object is an activity pane"
  t)
(defmethod activity-pane-p (obj)
  nil)

; Handles the highlighting of labels when the mouse is over an activity pane
(defun handle-highlighting (self x y)
  "Handles the highlighting of labels when the mouse is over an activity pane"
  (if (not (activity-pane-p self)) (return-from handle-highlighting))
  (if (criticality-overlay-enabled? self) (return-from handle-highlighting))
  (cond ((and (expandable-p self) (not (expanded? self)))
         (if (expansion-timer-value self)
             (setf (expansion-timer-value self) (app-property 'hover-time))
           (cond ((not (and (= x 0) (= y 0)))
                  (setf (expansion-timer-value self) (app-property 'hover-time))
                  (expand-in-background self))))))
  (let ((obj (capi:pinboard-object-at-position self x y))
        (bg-color (background-color self))
        (hover-color #(:RGB 0.0 0.0 0.0)))
    (do ((i 1 (1+ i)))
        ((= i 4))
      (setf (elt hover-color i) (if (< 0.25 (elt bg-color i)) (* 0.75 (elt bg-color i)) (- 1 (elt bg-color i)))))
    (dolist (x (cdr (capi:layout-description self)))
      (if (is-pinboard-object-p x) (set-background-for-pinboard-object bg-color x)))
    (if (and (not (is-displaying-input-pane? self)) (is-pinboard-object-p obj) (not (or (eq obj (drawn-background self)) (eq obj (param-overview-label self)))))
        (set-background-for-pinboard-object hover-color obj))
    (capi:with-geometry self
      (capi:redraw-pinboard-layout self capi:%x% capi:%y% capi:%width% capi:%height%)))
)

; Pass mouse movement to the containing activity layout
(defun activity-pane-mouse-move (self x y)
  "Passes mouse movement to the parent activity layout"
  (multiple-value-bind (x y) (mouse-position-in-parent self x y)
    (activity-layout-mouse-move (capi:element-parent self) x y)))

; Handles a click on the activity pane
(defun activity-pane-clicked (self x y)
  "Handles a click on an activity pane"
  (if (is-displaying-input-pane? self)
      (progn
        (setf (capi:layout-description self) (append (list (drawn-background self) (name-label self) (dist-label self)) (copy-list (param-labels self))))
        (displaying-input-pane nil self)
        (return-from activity-pane-clicked)))
  (let ((obj (capi:pinboard-object-at-position self x y))
        (pos nil))
    (cond ;((eq obj (drawn-background self))
          ; (highlight (not (is-highlighted? self)) self))
          ((eq obj (name-label self))
           (setf (capi:text-input-pane-text (name-textbox self)) (label (source self)))
           (setf pos (position obj (capi:layout-description self)))
           (if (null pos) (return-from activity-pane-clicked))
           (setf (nth pos (capi:layout-description self)) (name-textbox self))
           (setf (capi:layout-description self) (capi:layout-description self))
           (gp:invalidate-rectangle self)
           (capi:set-pane-focus (name-textbox self))
           (displaying-input-pane t self)
           )
          ((eq obj (dist-label self))
           (setf pos (position obj (capi:layout-description self)))
           (if (null pos) (return-from activity-pane-clicked))
           (setf (nth pos (capi:layout-description self)) (dist-combo self))
           (setf (capi:layout-description self) (capi:layout-description self))
           (gp:invalidate-rectangle self)
           (capi:set-pane-focus (dist-combo self))
           (displaying-input-pane t self)
           )
          ((member obj (param-labels self))
           (setf pos (position obj (param-labels self)))
           (if (null pos) (return-from activity-pane-clicked))
           (setf (capi:text-input-pane-text (nth pos (param-textboxes self))) (nth pos (param-values self)))
           (let ((pos2 (position obj (capi:layout-description self))))
             (if (null pos2) (return-from activity-pane-clicked))
             (setf (nth pos2 (capi:layout-description self)) (nth pos (param-textboxes self)))
             (setf (capi:layout-description self) (capi:layout-description self))
             (gp:invalidate-rectangle self)
             (capi:set-pane-focus (nth pos (param-textboxes self)))
             (displaying-input-pane t self))
           )
)))

; Sets the text of the name label
(defun set-name-label-text (text pane interface)
  "Sets the text of the name label"
  (declare (ignore interface))
;  (setf (capi:item-text (name-label pane)) (if (equal text "") "(unnamed)" text))
;  (setf (label (source pane)) (copy-seq text))
  (setf (activity-labels (capi:element-parent pane)) (if (equal text "") "(unnamed)" text))
  (let ((pos (position (name-textbox pane) (capi:layout-description pane))))
    (if (not pos) (return-from set-name-label-text))
    (setf (nth pos (capi:layout-description pane)) (name-label pane))
    (setf (capi:layout-description pane) (capi:layout-description pane))
    (capi:with-geometry pane
      (capi:redraw-pinboard-layout pane capi:%x% capi:%y% capi:%width% capi:%height%))
    (displaying-input-pane nil pane)
    (handle-highlighting pane 0 0)
    (capi:set-pane-focus (capi:element-parent pane))
))

; Sets the text of the distribution label
(defun set-distribution-label-text (item pane intf)
  "Sets the text of the distribution label"
  (declare (ignore intf))
;  (setf (capi:item-text (dist-label pane)) (format nil "Dist: ~A" item))
;  (setf (distribution (source pane)) (find-distribution item))
;  (setf (distribution pane) (find-distribution item))
  (setf (activity-distributions (capi:element-parent pane)) item)
  (let ((pos (position (dist-combo pane) (capi:layout-description pane))))
    (if (not pos) (return-from set-distribution-label-text))
    (setf (nth pos (capi:layout-description pane)) (dist-label pane))
    (setf (capi:layout-description pane) (capi:layout-description pane))
    (capi:with-geometry pane
      (capi:redraw-pinboard-layout pane capi:%x% capi:%y% capi:%width% capi:%height%))
    (displaying-input-pane nil pane)
    (handle-highlighting pane 0 0)
    (capi:set-pane-focus (capi:element-parent pane))
    (update-param-overview-label pane)
    (mp:process-run-function "Expansion Operation" '() #'expansion-function pane)
))

; Draws the background object for an activity pane
(defun background-draw-function (act-pane self pane x y w h)
  "Draws the background object for an activity pane"
  (let* ((hc (is-highlighted? act-pane))
         (cp (is-on-critical-path? act-pane))
         (ir (is-in-ir? act-pane))
         (hl-color (app-property 'editor-highlight-color))
         (bg-color (background-color act-pane))
         (cp-color (app-property 'editor-cp-color))
         (ir-color (if ir (ir-color act-pane) bg-color)))
    (capi:with-geometry pane
      (gp:clear-rectangle self x y w h)
      (gp:draw-rectangle self capi:%x% capi:%y% (1- capi:%width%) (1- capi:%height%))
      (if ir (gp:draw-rectangle self (+ capi:%x% 4) (+ capi:%y% 4) 
                                #+win32
                                (- capi:%width% 8)
                                #-win32
                                (- capi:%width% 9)
                                #+win32
                                (- capi:%height% 8)
                                #-win32
                                (- capi:%height% 9) :foreground ir-color :thickness 8))
      (if cp (gp:draw-rectangle self (+ capi:%x% 2) (+ capi:%y% 2)
                                #+win32
                                (- capi:%width% 4)
                                #-win32
                                (- capi:%width% 5)
                                #+win32
                                (- capi:%height% 4)
                                #-win32
                                (- capi:%height% 5) :foreground cp-color :thickness 4))
      (if hc (gp:draw-rectangle self (+ capi:%x% 2) (+ capi:%y% 2)
                                #+win32
                                (- capi:%width% 4)
                                #-win32
                                (- capi:%width% 5)
                                #+win32
                                (- capi:%height% 4)
                                #-win32
                                (- capi:%height% 5) :foreground hl-color :thickness 4)))
))

; Sets the text of a parameter label
(defun set-param-label-text (text position box pane intf)
  "Sets the text of a parameter label"
  (declare (ignore intf))
;  (setf (capi:item-text label) (format nil "~A: ~A" (get-param-name (distribution pane) (position label (param-labels pane))) (if (equal text "") "0" text)))
;  (setf (nth (position label (param-labels pane)) (param-values pane)) (if (equal text "") "0" text))
;  (setf (parameter (position label (param-labels pane)) (source pane)) (if (equal text "") "0" text))
;  (update-param-overview-label pane)
  (setf (activity-param-values position (capi:element-parent pane)) text)
  (let ((pos (position box (capi:layout-description pane))))
    (if (not pos) (return-from set-param-label-text))
    (setf (nth pos (capi:layout-description pane)) (nth position (param-labels pane)))
    (setf (capi:layout-description pane) (capi:layout-description pane))
    (capi:with-geometry pane
      (capi:redraw-pinboard-layout pane capi:%x% capi:%y% capi:%width% capi:%height%))
    (displaying-input-pane nil pane)
    (handle-highlighting pane 0 0)
    (capi:set-pane-focus (capi:element-parent pane))))

(defmethod initialize-instance :after ((obj activity-pane) &rest args)
  (setf (capi:simple-pane-background obj) (background-color obj))
  (setf (capi:text-input-pane-callback (name-textbox obj)) #'(lambda (text intf) (set-name-label-text text obj intf)))
  (mapcar #'(lambda (label textbox)
              (setf (capi:text-input-pane-callback textbox) #'(lambda (text intf) (set-param-label-text text (position label (param-labels obj)) textbox obj intf))))
          (param-labels obj)
          (param-textboxes obj))
  (setf (capi:callbacks-selection-callback (dist-combo obj)) #'(lambda (item intf) (change-activity-distribution item obj intf)))
  (setf (capi:drawn-pinboard-object-display-callback (drawn-background obj)) #'(lambda (&rest args) (apply #'background-draw-function obj args)))
  (setf (capi:layout-description obj) (list (drawn-background obj) (name-label obj) (dist-label obj) (param-overview-label obj)))
)

; Callback to change the current activity distribution
(defun change-activity-distribution (item pane interface)
  "Callback to change the current activity distribution"
  (set-distribution-label-text item pane interface)
)

; Collapses an activity pane
(defun collapse-object (obj)
  "Collapses an activity pane"
  (setf (collapse-timer-value obj) (app-property 'hover-time))
  (collapse-in-background obj))

; Determines whether the mouse is over the specified activity pane
(defmethod over-activity-pane-p ((pane activity-pane) x y)
  "Determines whether the mouse is over the specified activity pane"
  (multiple-value-bind (obj-x obj-y) (capi:pinboard-pane-position pane)
    (multiple-value-bind (obj-w obj-h) (capi:pinboard-pane-size pane)
      (and (and (<= obj-x x) (< x (+ obj-x obj-w)))
           (and (<= obj-y y) (< y (+ obj-y obj-h)))))))

(defmethod over-activity-pane-p (pane x y)
  nil)

; Selects the activity pane as a function of shift-down
(defmethod select ((pane activity-pane) shift-down)
  "Selects the activity pane as a function of shft-down"
  (if shift-down 
      (progn
        (setf (selected? pane) t)
        (push pane (selected-activities (capi:element-parent pane))))
    (progn
      (dolist (pane (selected-activities (capi:element-parent pane)))
        (setf (selected? pane) nil))
      (dolist (arrow (arrows (capi:element-parent pane)))
        (setf (selected arrow) nil))
      (setf (selected-activities (capi:element-parent pane)) (list pane))))
  (highlight-selected-activities (capi:element-parent pane))
)

; Selects an errow as a function of shift-down
(defmethod select ((arrow arrow) shift-down)
  "Selects an arrow as a function of shift-down"
  (if shift-down (setf (selected arrow) t)
    (progn
      (dolist (pane (selected-activities (capi:pinboard-object-pinboard arrow)))
        (setf (selected? pane) nil))
      (setf (selected-activities (capi:pinboard-object-pinboard arrow)) nil)
      (dolist (arrow (arrows (capi:pinboard-object-pinboard arrow)))
        (setf (selected arrow) nil))
      (setf (selected arrow) t)))
)

; Deselects an activity pane
(defmethod deselect ((pane activity-pane))
  "Deselects an activity pane"
  (setf (selected? pane) nil)
  (setf (selected-activities (capi:element-parent pane)) (remove pane (selected-activities (capi:element-parent pane))))
  (highlight nil pane))

; Deselects an arrow
(defmethod deselect ((arrow arrow))
  "Deselects an arrow"
  (setf (selected arrow) nil))

; Passes mouse down events to the parent activity layout
(defun activity-pane-mouse-down (self x y &optional shift)
  "Passes mouse down events to the parent activity layout"
  (multiple-value-bind (mx my) (mouse-position-in-parent self x y)
    (activity-layout-mouse-down (capi:element-parent self) mx my shift))
)

; Prints the hierarchical distribution menu
(defun special-print (x)
  "Prints the hierarchical distribution menu"
  (if (symbolp x)
      (cond ((equal x 'standard)
             "Standard")
            ((equal x 'cv)
             "Coefficients of Variation"))
    (format nil "   ~A" x)))

; Constructs the distribution option pane for an activity pane
(defun make-distribution-option-pane ()
  "Constructs the distribution option pane for an activity pane"
  (let ((dist-names (list-all-distributions))
        (list1 nil)
        (list2 nil)
        (master nil)
        (enabled-positions nil))
    (dolist (dist dist-names)
      (if (find-regexp-in-string "CV" dist) (push dist list2) (push dist list1)))
    (setf list1 (sort list1 #'string<))
    (setf list2 (sort list2 #'string<))
    (setf master (cons 'standard (append list1 (cons 'cv list2))))
    (setf enabled-positions (remove-if #'null (mapcar #'(lambda (x) (if (symbolp x) nil (position x master))) master)))
    (make-instance 'capi:option-pane
                   :x 10 :y #+win32 30 #+cocoa 34
                   :visible-min-width 180 :visible-max-width 180
                   :test-function #'equal :callback-type :item-interface
                   :items master :print-function #'special-print :enabled-positions enabled-positions :selection 1)))

; Passes mouse up events to the parent activity layout
(defun activity-pane-mouse-up (self x y)
  "Passes mouse up events to the parent activity layout"
  (multiple-value-bind (x y) (mouse-position-in-parent self x y)
    (activity-layout-mouse-up (capi:element-parent self) x y))
)

; Computes the position of the mouse in the parent element
(defun mouse-position-in-parent (self x y)
  "Computes the position of the mouse in the parent element"
  (capi:convert-relative-position self (capi:element-parent self) x y))

; Passes drag events to the parent activity layout
(defun activity-pane-drag (self x y)
  "Passes drag events to the parent activity layout"
  (multiple-value-bind (x y) (mouse-position-in-parent self x y)
    (activity-layout-mouse-drag (capi:element-parent self) x y))
)

; Passes mouse down events to the parent activity layout
(defun activity-pane-mouse-down-with-shift (self x y)
  "Passes mouse down events to the parent activity layout"
  (activity-pane-mouse-down self x y t))

; Passes delete events to the parent activity layout
(defun activity-pane-delete (self x y char)
  "Passes delete events to the parent activity layout"
  (activity-layout-delete (capi:element-parent self) x y char))

; Processes the escape key to stop displaying any input boxes
(defun activity-pane-escape (self x y char)
  "Processes the escape key to stop displaying any input boxes"
  (declare (ignore x y char))
  (let ((object (or (member (name-textbox self) (capi:layout-description self))
                    (member (dist-combo self) (capi:layout-description self))
                    (member-if #'(lambda (x) (member x (capi:layout-description self))) (param-textboxes self)))))
    (if (null object) (return-from activity-pane-escape))
    (setf object (car object))
    (displaying-input-pane nil self)
    (cond ((eq object (name-textbox self))
           (setf (car (member object (capi:layout-description self))) (name-label self)))
          ((eq object (dist-combo self))
           (setf (car (member object (capi:layout-description self))) (dist-label self)))
          (t
           (setf (car (member object (capi:layout-description self)))
                 (nth (position object (param-textboxes self)) (param-labels self))))))
  (setf (capi:layout-description self) (capi:layout-description self))
  (gp:invalidate-rectangle self)
)

; The context menu for activity panes
(capi:define-menu activity-pane-right-click-menu (self) "Right-click menu"
  ((:component
    (("Change Activity Type"
      :callback #'(lambda () (change-activity-pane-type self))
      :callback-type :none)
     ("Delete Activities"
      :callback #'(lambda () (activity-layout-delete (main-layout self) 0 0 #\Backspace))
      :callback-type :none))))
)

; Process when the user right clicks on an activity pane
(defun activity-pane-right-click (self x y)
  (if (criticality-overlay-enabled? self) (return-from activity-pane-right-click))
  (if (not (selected? self))
      (progn
        (deselect-all-activities (capi:element-parent self) x y)
        (setf (selected? self) t)
        (setf (selected-activities (capi:element-parent self)) (list self))
        (highlight t self)))
  (capi:display-popup-menu (activity-pane-right-click-menu (capi:element-interface self)) :owner (capi:element-interface self))
)

; Input model for activity panes
(defparameter *activity-pane-input-model*
  '(((:motion) activity-pane-mouse-move)
    ((:button-1 :press) activity-pane-mouse-down)
    ((:button-1 :press :shift) activity-pane-mouse-down-with-shift)
    ((:button-1 :release) activity-pane-mouse-up)
    ((:button-1 :motion) activity-pane-drag)
    ((:button-3 :release) activity-pane-right-click)
    ((:button-1 :press :control) activity-pane-right-click)
    ((:gesture-spec "Delete") activity-pane-delete)
    ((:gesture-spec "Backspace") activity-pane-delete)
    ((:gesture-spec #\Escape) activity-pane-escape)
))
