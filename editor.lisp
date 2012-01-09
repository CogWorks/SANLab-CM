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


(capi:define-interface editor (interface-view)
  ((last-ct-tracker :initform nil :accessor last-ct-tracker)
   (dragging? :initform nil :accessor dragging?)
   (poller :initform nil :accessor poller)
   (criticality-overlay-enabled? :initform nil :reader criticality-overlay-enabled?))
  (:panes
   (tool-pane
    capi:title-pane
    :accessor tool-pane
    :text "Select Tool"))
  (:layouts
   (primary-layout
    capi:column-layout
    '(main-layout tool-pane))
   (main-layout
    activity-layout '()
    :accessor main-layout
    :visible-min-width 500
    :visible-min-height 500))
  (:menus
   (file-menu
    "File"
    ((:component
      (("New Model"
        :callback #'create-new-model
        :callback-type :interface
        :accelerator "accelerator-n")
       ("Open Model..."
        :callback #'open-sanlab-model
        :callback-type :interface
        :accelerator "accelerator-o")
       ("Import Macproject File..."
        :callback #'import-existing-project
        :callback-type :interface
        :accelerator
        #+cocoa
        "accelerator-shift-o"
        #+win32
        "control-shift-s"
        )
       ("Import ACT-R Trace..."
        :callback #'import-actr-trace
        :callback-type :interface)
       ("Import from CogTool..."
        :callback #'import-from-cogtool
        :callback-type :interface)
       ("Close"
        :callback #'close-this-editor
        :callback-type :interface
        )))
     (:component
      (("Save"
        :callback #'save-sanlab-model
        :callback-type :interface
        :accelerator "accelerator-s")
       ("Save As..."
        :callback #'save-sanlab-model-as
        :callback-type :interface
        :accelerator
        #+cocoa
        "accelerator-shift-s"
        #+win32
        "control-shift-s"
)))
     (:component
      (("Page Setup..."
        :callback #'not-implemented
        :callback-type :interface
        :accelerator
        #+cocoa
        "accelerator-shift-p"
        #+win32
        "control-shift-p"
)
       ("Print..."
        :callback #'not-implemented
        :callback-type :interface
        :accelerator "accelerator-p")))))
   (edit-menu
    "Edit"
    ((:component
      (("Undo"
        :callback #'(lambda (self) (undo-history-item (controller self)))
        :callback-type :interface
        :enabled-function #'able-to-undo
        :accelerator "accelerator-z")
       ("Redo"
        :callback #'(lambda (self) (redo-history-item (controller self)))
        :callback-type :interface
        :enabled-function #'able-to-redo
        :accelerator "accelerator-y")))
     (:component
      (("Cut"
        :callback #'(lambda (self)
                      (cut-selection (controller self)
                                     :selected-activities
                                     (mapcar #'source (selected-activities (main-layout self)))
                                     :selected-connections
                                     (mapcar #'(lambda (x)
                                                 (let ((s (start x))
                                                       (e (end x)))
                                                   (cons (source s) (source e))))
                                             (remove-if-not #'selected (arrows (main-layout self)))))
                      )
        :callback-type :interface
        :accelerator "accelerator-x")
       ("Copy"
        :callback #'(lambda (self)
                      (copy-selection (controller self)
                                      :selected-activities
                                      (mapcar #'source (selected-activities (main-layout self)))
                                      :selected-connections
                                      (mapcar #'(lambda (x)
                                                  (let ((s (start x))
                                                        (e (end x)))
                                                    (cons (source s) (source e))))
                                              (remove-if-not #'selected (arrows (main-layout self))))))
        :callback-type :interface
        :accelerator "accelerator-c")
       ("Paste"
        :callback #'(lambda (self)
                      (paste-selection (controller self)))
        :callback-type :interface
        :accelerator "accelerator-v")))
     (:component
      (("Select All"
        :callback #'(lambda (editor)
                      (select-all (controller editor))
                      (mapcar #'(lambda (x) (setf (selected x) t)) (arrows (main-layout editor))))
        :callback-type :interface
        :accelerator "accelerator-a"
        )
       ("Select By Type"
        :callback #'editor-select-by-type
        :callback-type :interface
        :accelerator
        #+cocoa
        "meta-accelerator-a"
        #+win32
        "control-shift-a"
)
       ("Deselect All"
        :callback #'(lambda (editor) (deselect-all (controller editor)))
        :callback-type :interface)))
     (:component
      (("Edit Activity Types..."
        :callback #'not-implemented
        :callback-type :interface)
       ("Edit Distribution Types..."
        :callback #'show-distribution-editor
        :callback-type :interface)))
     (:component
      (("Layout Graph"
        :callback #'editor-layout-graph-with-width
        :callback-type :interface
        :accelerator "accelerator-g")
       ("Layout Graph Vertically"
        :callback #'editor-layout-graph-without-width
        :callback-type :interface
        :accelerator
        #+cocoa
        "meta-accelerator-g"
        #+win32
        "control-shift-g"
)))))
   (model-menu
    "Model"
    ((:component
      (("Run Model"
        :callback #'editor-run-model
        :callback-type :interface
        :accelerator "accelerator-r")
       ("Generate Sample Trace"
        :callback #'editor-generate-sample-trace
        :callback-type :interface)
       ))
     (:component
      (("View Previous Trials"
        :callback #'editor-view-results
        :callback-type :interface
        :accelerator
        #+cocoa
        "accelerator-shift-t"
        #+win32
        "control-shift-t"
        )
       ("Retrieve Random State"
        :callback #'editor-retrieve-random-state
        :callback-type :interface)
       ))
     (:component
      (("Show Activity Criticalities"
        :callback #'editor-show-criticality-window
        :callback-type :interface
        :enabled-function #'last-ct-tracker
        :selected nil)
       ("Enable Criticality Overlay"
        :callback #'editor-enable-criticality-overlay
        :callback-type :interface
        :enabled-function #'last-ct-tracker
        :selected-function #'criticality-overlay-enabled?)
       )
      :interaction :multiple-selection)
     (:component
      (("Model Properties"
        :callback #'editor-show-model-properties
        :callback-type :interface)))
     ))
   (window-menu
    "Window"
    ((:component
      (("Close Window"
        :callback #'close-this-editor
        :callback-type :interface
        :accelerator "accelerator-w")
       ))
     (:component
      ()
      :selection-callback #'(lambda (data interface)
                              (declare (ignore interface))
                              (capi:execute-with-interface data 'capi:raise-interface data))
      :items-function #'(lambda (interface)
                          (declare (ignore interface))
                          (capi:collect-interfaces 'interface-view))
#|
      :selection-function #'(lambda (interface)
                              (position interface (capi:collect-interfaces 'interface-view))
                              )
|#
      :print-function #'(lambda (interface)
                          (cond ((equal (type-of interface) 'model-editor)
                                 (if (filename (model interface))
                                     (get-bundle-name (filename (model interface)))
                                   "Untitled"))
                                ((equal (type-of interface) 'histogram-window)
                                 (capi:interface-title interface))
                                (t
                                 (capi:interface-title interface)))
                          )
      )
     ))
   )
  (:menu-bar file-menu edit-menu model-menu window-menu)
  (:default-initargs
   :auto-menus nil
   :x 250
   :y 120
   :best-width (- (capi:screen-width *capi-screen*) 250)
   :best-height (- (capi:screen-height *capi-screen*) 170)
   :window-styles '(:motion-events-without-focus)
   :confirm-destroy-function #'confirm-destroy
   :destroy-callback #'editor-destroyed
   :title "SANLab-CM - Untitled"))

(defmethod editor-destroyed ((editor editor))
  (if (poller editor)
      (mp:process-kill (poller editor)))
  (destroy-controller-if-primary editor)
)

(defmethod poll-for-edge-scrolling ((editor editor))
  (setf (poller editor) (mp:get-current-process))
  (let ((layout (main-layout editor)))
    (loop
     (sleep 0.05)
     (if (dragging? editor)
         (multiple-value-bind (x y) (capi:current-pointer-position :relative-to layout)
           (if (< x (+ (capi:get-horizontal-scroll-parameters layout :slug-position) 20))
               (capi:apply-in-pane-process layout #'capi:scroll layout :horizontal :move (- (capi:get-horizontal-scroll-parameters layout :slug-position) 25)))
           (if (< y (+ (capi:get-vertical-scroll-parameters layout :slug-position) 20))
               (capi:apply-in-pane-process layout #'capi:scroll layout :vertical :move (- (capi:get-vertical-scroll-parameters layout :slug-position) 25)))
           (if (> x (- (+ (gp:port-width layout) (capi:get-horizontal-scroll-parameters layout :slug-position)) 20))
               (capi:apply-in-pane-process layout #'capi:scroll layout :horizontal :move (+ 25 (capi:get-horizontal-scroll-parameters layout :slug-position))))
           (if (> y (- (+ (gp:port-height layout) (capi:get-vertical-scroll-parameters layout :slug-position)) 20))
               (capi:apply-in-pane-process layout #'capi:scroll layout :vertical :move (+ 25 (capi:get-vertical-scroll-parameters layout :slug-position)))))))
   )
)

(defmethod editor-show-model-properties ((self editor))
  (let ((window (make-instance 'model-property-window :view-controller (view-controller self))))
    (add-interface-to-view window 'model-property-window (view-controller self))
    (capi:display window))
)

(defmethod editor-show-criticality-window ((self editor))
  (let ((ct-window (make-instance 'criticality-window :criticality-tracker (last-ct-tracker self) :view-controller (view-controller self))))
    (add-interface-to-view ct-window 'criticality-window (view-controller self))
    (capi:display ct-window))
)

(defmethod editor-enable-criticality-overlay ((self editor))
  (setf (criticality-overlay-enabled? self) (not (criticality-overlay-enabled? self)))
)

(defmethod (setf criticality-overlay-enabled?) (val (self editor))
  (setf (slot-value self 'criticality-overlay-enabled?) val)
  (deselect-all-activities (main-layout self) 0 0)
  (capi:redraw-pinboard-layout (main-layout self) 0 0 (width (controller self)) (height (controller self)))
  (dolist (act (activities (main-layout self)))
    (if val
        (setf (capi:layout-description act) (list (drawn-background act) (name-label act) (dist-label act) (param-overview-label act) (criticality-pane act)))
      (setf (capi:layout-description act) (list (drawn-background act) (name-label act) (dist-label act) (param-overview-label act)))))
;    (capi:with-geometry act
;      (capi:redraw-pinboard-layout act 0 0 capi:%width% capi:%height%)))
  (fire-event-listeners (controller self) 'editor-criticality-changed (list self))
)

(defmethod show-distribution-editor ((self editor))
  (let ((window (make-instance 'dist-editor :view-controller (view-controller self))))
    (add-interface-to-view window 'sanlab-distribution-editor (view-controller self))
    (capi:display window)))

(defmethod confirm-destroy ((self editor))
  (if (not (equalp nil (model-changed self)))
      (multiple-value-bind (result success)
              (capi:prompt-for-confirmation (format nil "Save changes to ~a before quitting?"
                                                    (if (not (equalp nil (pathname (controller self))))
                                                        (get-bundle-name (pathname (controller self)))
                                                      "Untitled"))
                                            :cancel-button t)
            (cond
             ((not success) ;; Cancel
              (return-from confirm-destroy))
             (result
              (save-sanlab-model self))
             (t t)))
    t)
)

(defmethod initialize-instance :after ((editor editor) &rest initargs)
  (register-callbacks editor (controller (view-controller editor)))
  (add-grid (main-layout editor) (width (controller editor)) (height (controller editor)))
  (mp:process-run-function "Scroll thread" nil #'poll-for-edge-scrolling editor)
)

(defmethod make-listener ((intf editor) (callback function))
  #'(lambda (&rest args)
      (if args
          (apply #'capi:apply-in-pane-process intf callback intf args)
        (capi:apply-in-pane-process intf callback intf))))

(defmethod register-callbacks ((intf editor) (con controller))
;  (capi:display-message "Registering callbacks")
  (add-event-listener 'new-activity (make-instance 'listener :callback (make-listener intf #'editor-new-activity-listener)) con)
  (add-event-listener 'deleted-activity (make-instance 'listener :callback (make-listener intf #'editor-deleted-activity-listener)) con)
  (add-event-listener 'connected-activities (make-instance 'listener :callback (make-listener intf #'editor-connected-activities-listener)) con)
  (add-event-listener 'disconnected-activities (make-instance 'listener :callback (make-listener intf #'editor-disconnected-activities-listener)) con)

  (add-event-listener 'new-model (make-instance 'listener :callback (make-listener intf #'editor-new-model-listener)) con)
  (add-event-listener 'create-new-model-failed (make-instance 'listener :callback (make-listener intf #'editor-new-model-failed-listener)) con)

  (add-event-listener 'open-model (make-instance 'listener :callback (make-listener intf #'editor-opened-model-listener)) con)
  (add-event-listener 'open-model-failed (make-instance 'listener :callback (make-listener intf #'editor-open-model-failed-listener)) con)

  (add-event-listener 'save-model (make-instance 'listener :callback (make-listener intf #'editor-saved-model-listener)) con)
  (add-event-listener 'save-model-failed (make-instance 'listener :callback (make-listener intf #'editor-save-model-failed-listener)) con)

  (add-event-listener 'import-macproject-model (make-instance 'listener :callback (make-listener intf #'editor-imported-model-listener)) con)
  (add-event-listener 'import-macproject-model-failed (make-instance 'listener :callback (make-listener intf #'editor-imported-model-failed-listener)) con)

  (add-event-listener 'import-actr-trace (make-instance 'listener :callback (make-listener intf #'editor-imported-model-listener)) con)
  (add-event-listener 'import-actr-trace-failed (make-instance 'listener :callback (make-listener intf #'editor-imported-model-failed-listener)) con)

  (add-event-listener 'import-from-cogtool (make-instance 'listener :callback (make-listener intf #'editor-imported-model-listener)) con)
  (add-event-listener 'import-from-cogtool-failed (make-instance 'listener :callback (make-listener intf #'editor-imported-model-failed-listener)) con)

  (add-event-listener 'model-changed (make-instance 'listener :callback (make-listener intf #'editor-model-changed)) con)
  (add-event-listener 'activity-on-critical-path (make-instance 'listener :callback (make-listener intf #'editor-activity-on-cp-listener)) con)

  (add-event-listener 'activity-selected (make-instance 'listener :callback (make-listener intf #'editor-activity-selected)) con)
  (add-event-listener 'activity-deselected (make-instance 'listener :callback (make-listener intf #'editor-activity-deselected)) con)

  (add-event-listener 'moved-activity (make-instance 'listener :callback (make-listener intf #'editor-moved-activity)) con)
  (add-event-listener 'activity-change (make-instance 'listener :callback (make-listener intf #'editor-activity-changed)) con)

  (add-event-listener 'model-width-change (make-instance 'listener :callback (make-listener intf #'editor-model-dimensions-changed)) con)
  (add-event-listener 'model-height-change (make-instance 'listener :callback (make-listener intf #'editor-model-dimensions-changed)) con)

  (add-event-listener 'laid-out-model (make-instance 'listener :callback (make-listener intf #'editor-laid-out-model)) con)
  (add-event-listener 'laid-out-model-error (make-instance 'listener :callback (make-listener intf #'editor-laid-out-model-error)) con)

  (add-event-listener 'created-model-note (make-instance 'listener :callback (make-listener intf #'editor-created-model-note)) con)
  (add-event-listener 'deleted-model-note (make-instance 'listener :callback (make-listener intf #'editor-deleted-model-note)) con)
  (add-event-listener 'model-note-text-changed (make-instance 'listener :callback (make-listener intf #'editor-model-note-text-changed)) con)
  (add-event-listener 'moved-model-note (make-instance 'listener :callback (make-listener intf #'editor-moved-model-note)) con)
)

(defmethod editor-created-model-note ((editor editor) (event symbol) note)
  (let ((pane (make-instance 'multiline-pinboard-object :source note :text (text note) :x (x-position note) :y (y-position note))))
    (push pane (notes (main-layout editor)))
    (update-activity-layout (main-layout editor))))

(defmethod editor-deleted-model-note ((editor editor) (event symbol) note)
  (setf (notes (main-layout editor)) (remove-if #'(lambda (x) (pointer= note (source x))) (notes (main-layout editor))))
  (update-activity-layout (main-layout editor)))

(defmethod editor-model-note-text-changed ((editor editor) (event symbol) note text)
  (setf (multiline-pinboard-object-text (find-if #'(lambda (x) (pointer= note (source x))) (notes (main-layout editor)))) text)
  (update-activity-layout (main-layout editor)))

(defmethod editor-moved-model-note ((editor editor) (event symbol) note x y)
  (setf (capi:pinboard-pane-position (find-if #'(lambda (x) (pointer= note (source x))) (notes (main-layout editor)))) (values x y))
  (update-activity-layout (main-layout editor)))

(defclass copy-node ()
  ((uid :initform 0 :initarg :uid :accessor uid)
   (activity-type :initform nil :initarg :activity-type :accessor activity-type)
   (label :initform nil :initarg :label :accessor label)
   (color :initform nil :initarg :color :accessor color)
   (offset-x :initform nil :initarg :offset-x :accessor offset-x)
   (offset-y :initform nil :initarg :offset-y :accessor offset-y)
   (edges-in :initform nil :initarg :edges-in :accessor edges-in)
   (edges-out :initform nil :initarg :edges-out :accessor edges-out)
   (distribution :initform nil :initarg :distribution :accessor distribution)
   (parameters :initform nil :initarg :parameters :accessor parameters)))

#|
(defmethod copy-graph-node ((p instance-pointer) uid x y)
  (make-instance 'copy-node
                 :uid uid
                 :activity-type (activity-type p)
                 :label (copy-seq (label p))
                 :color (copy-seq (color p))
                 :offset-x (- (x-position p) x)
                 :offset-y (- (y-position p) y)
                 :distribution (distribution p)
                 :parameters (copy-list (parameters p)))
)
|#

#|
(defmethod build-copy-of-selection ((self editor))
  (let ((ht (make-hash-table))
        (min-x 0) (min-y 0)
        (uid 0))
    (setf min-x (x-position (source (first (selected-activity (main-layout self))))))
    (setf min-y (y-position (source (first (selected-activity (main-layout self))))))
    (mapcar #'(lambda (pane)
                (let* ((src (source pane))
                       (x (x-position src))
                       (y (y-position src)))
                  (if (< x min-x) (setf min-x x))
                  (if (< y min-y) (setf min-y y))))
            (selected-activities (main-layout self)))
    (mapcar #'(lambda (pane)
                (let ((cp (copy-graph-node (source pane) uid min-x min-y)))
                  (incf uid)
                  (setf (gethash (source pane) ht) cp)))
            (selected-activities (main-layout self)))
))
|#

(defmethod editor-moved-activity ((self editor) (event symbol) (act instance-pointer) x y)
  (dolist (pane (activities (main-layout self)))
    (if (pointer= (source pane) act)
        (progn
          (update-activity-pane pane)
          (return-from editor-moved-activity))))
)

(defmethod editor-activity-changed ((self editor) (event symbol) (act instance-pointer) (property symbol) &rest info)
  (dolist (pane (activities (main-layout self)))
    (if (pointer= (source pane) act)
        (progn
          (update-activity-pane pane)
          (return-from editor-activity-changed))))
)

(defmethod editor-model-dimensions-changed ((self editor) (event symbol) val)
  (clear-editor-and-update-from-model self)
)

(defmethod editor-laid-out-model ((self editor) (event symbol))
  (clear-editor-and-update-from-model self)
)

(defmethod editor-laid-out-model-error ((self editor) (event symbol) error)
  (capi:display-message "Failed to layout model with error ~A" error)
;  (invoke-debugger error)
)

(defmethod editor-activity-selected ((self editor) (event symbol) (node instance-pointer))
  (dolist (pane (activities (main-layout self)))
    (if (pointer= node (source pane))
        (highlight t pane)))
)

(defmethod editor-activity-deselected ((self editor) (event symbol) (node instance-pointer))
  (dolist (pane (activities (main-layout self)))
    (if (pointer= node (source pane))
        (highlight nil pane)))
)

(defmethod editor-model-changed ((self editor) (event symbol) &rest args)
  (set-title self :changed t))

(defmethod editor-new-activity-listener ((intf editor) (event symbol) (node instance-pointer))
  (layout-add-activity (main-layout intf) node)
)

(defmethod editor-deleted-activity-listener ((self editor) (event symbol) (node instance-pointer))
  (layout-delete-activity (main-layout self) node)
)

(defmethod editor-connected-activities-listener ((intf editor) (event symbol) (act1 instance-pointer) (act2 instance-pointer))
  (layout-connect-activities (main-layout intf) act1 act2)
)

(defmethod editor-disconnected-activities-listener ((self editor) (event symbol) (act1 instance-pointer) (act2 instance-pointer))
  (layout-disconnect-activities (main-layout self) act1 act2)
)

(defmethod editor-activity-on-cp-listener ((self editor) (event symbol) (act instance-pointer) val)
;  (debug "Editor responding to CP change...~%")
  (layout-activity-on-critical-path (main-layout self) act val)
)

(defmethod set-title ((self editor) &key (changed nil))
  (let ((x (pathname (controller self))))
    (capi:apply-in-pane-process self #'(setf capi:interface-title) (format nil (if changed "SANLab-CM - ~a*" "SANLab - ~a") (if (equalp nil x) "Untitled" (get-bundle-name x))) self)
    #+cocoa
    (if (equalp nil x)
        (capi:apply-in-pane-process self #'set-cocoa-pathname self nil)
      (capi:apply-in-pane-process self #'set-cocoa-pathname self (format nil "~A" x)))
))

(defmethod editor-opened-model-listener ((self editor) (event symbol))
  (clear-editor-and-update-from-model self)
  (set-title self)
)

(defmethod editor-open-model-failed-listener ((intf editor) (event symbol) err)
  (capi:display-message (format nil "Failed to open model with error ~A." err))
)

(defmethod editor-saved-model-listener ((self editor) (event symbol))
  (set-title self)
)

(defmethod editor-save-model-failed-listener ((intf editor) (event symbol) err)
  (invoke-debugger err)
  (capi:display-message (format nil "Failed to save model with error ~A." err))
)

(defmethod editor-imported-model-listener ((intf editor) (event symbol))
  (clear-editor-and-update-from-model intf)
  (setf (capi:interface-title intf) "SANLab-CM - Untitled*")
)

(defmethod editor-imported-model-failed-listener ((intf editor) (event symbol) &rest err)
  (if err
      (capi:display-message (format nil "Failed to import model with error ~A." (car err))))
)

(defmethod editor-new-model-listener ((intf editor) (event symbol))
  (clear-editor-and-update-from-model intf)
  (setf (capi:interface-title intf) "SANLab-CM - Untitled*")
  #+cocoa
  (set-cocoa-pathname intf nil)
)

(defmethod editor-new-model-failed-listener ((intf editor) (event symbol) &rest err)
  (if err
      (capi:display-message (format nil "Failed to create a new model with error ~A." (car err))))
)

(defmethod clear-editor-and-update-from-model ((editor editor))
  (setf (activities (main-layout editor)) nil)
  (setf (arrows (main-layout editor)) nil)
  (setf (notes (main-layout editor)) nil)
  (setf (capi:pinboard-pane-size (grid (main-layout editor)))
        (values (width (controller editor))
                (height (controller editor))))
  (setf (capi:pinboard-pane-position (grid (main-layout editor)))
        (values 0 0))
  (let ((panes (make-hash-table :test #'pointer=))
        (acts (model-activities (controller (view-controller editor))))
        (final nil)
        (arrows nil))
    (dolist (act acts)
      (if (null (gethash (pointer act) panes))
          (setf (gethash (pointer act) panes) (make-activity-pane-from-source act)))
      (push (gethash (pointer act) panes) final)
      (dolist (target (dependent-tasks act))
        (if (null (gethash (pointer target) panes))
            (progn
              (format t "Target ~A not found~%" (pointer target))
              (setf (gethash (pointer target) panes) (make-activity-pane-from-source target))))
;        (push (make-instance 'arrow :start (gethash (pointer act) panes) :end (gethash (pointer target) panes)) arrows)
        (let (ir-type1 ir-type2)
          (setf ir-type1 (ir-type act))
          (setf ir-type2 (ir-type target))
          (push (if (and ir-type1 ir-type2 (pointer= ir-type1 ir-type2)
                         (connected-in-iroutine (ir-task act) (ir-task target) ir-type1))
                    (make-instance 'arrow :start (gethash (pointer act) panes) :end (gethash (pointer target) panes) :ir-color (color ir-type1))
                  (make-instance 'arrow :start (gethash (pointer act) panes) :end (gethash (pointer target) panes))) arrows))
        (push (first arrows) (edges-out (gethash (pointer act) panes)))
        (push (first arrows) (edges-in (gethash (pointer target) panes)))))
    (setf (activities (main-layout editor)) final)
    (setf (arrows (main-layout editor)) arrows)
    (dolist (n (rendered-notes (controller editor)))
      (push (make-instance 'multiline-pinboard-object :source n :x (x-position n) :y (y-position n) :text (text n)) (notes (main-layout editor))))
    (mapcar #'move-arrow arrows)
    (update-activity-layout (main-layout editor))
    (mapcar #'move-arrow arrows)
))

(defmethod create-model-editor ((contr controller))
  (let* ((vc (make-instance 'view-controller :controller contr))
         (toolbox (make-instance 'toolbox :view-controller vc))
         (editor (make-instance 'editor :view-controller vc)))
    (setf (view contr) vc)
    (add-interface-to-view editor 'sanlab-model-editor vc)
    (add-interface-to-view toolbox 'sanlab-toolbox vc)
    (capi:display editor)
    (capi:display toolbox)
    (add-interface-to-view (make-instance 'miniviewer :view-controller vc) 'sanlab-miniview vc)
    (setf (primary-view vc) editor)
    vc
))

(defmethod create-new-model ((editor editor))
  (new-model (controller (view-controller editor)))
)

(defmethod open-sanlab-model ((editor editor))
  (let ((filename
         #+win32
         (capi:prompt-for-file "Select model file"
                               :filter "*.mdl"
                               :filters '("SANLab-CM Model" "*.mdl" "All Files" "*.*")
                               :operation :open
                               :pathname (if (equalp nil (pathname (controller (view-controller editor))))
                                             (sys:get-folder-path :documents)
                                           (pop-directory (pathname (controller (view-controller editor))))))
         #-win32
         (capi:prompt-for-file "Select model file"
                               :filter "*.san"
                               :filters '("SANLab-CM Model" "*.san" "Legacy SANLab-CM Files" "*.mdl" "All Files" "*.*")
                               :operation :open
                               :pathname (if (equalp nil (pathname (controller (view-controller editor))))
                                             (sys:get-folder-path :documents)
                                           (pop-directory (pathname (controller (view-controller editor))))))
         ))
    (if filename
        (open-model (controller (view-controller editor)) filename)))
)

(defmethod import-existing-project ((editor editor))
  (controller-import-macproject-file (controller (view-controller editor)))
)

(defmethod import-actr-trace ((editor editor))
  (import-actr-trace (controller editor))
)

(defmethod import-from-cogtool ((editor editor))
  (import-from-cogtool (controller editor))
)

(defmethod close-this-editor ((editor editor))
  (capi:quit-interface editor)
)

(defmethod save-sanlab-model ((editor editor))
  (let ((name (if (equalp nil (pathname (controller (view-controller editor))))
                  (capi:prompt-for-file "Select model file"
                                        :filter "*.san"
                                        :filters '("SANLab Model" "*.san" "SANLab Interactive Routine" "*.sir")
                                        :operation :save
                                        :pathname (system:get-folder-path :documents)))))
;    (capi:prompt-with-message (format nil "~A" name))
    (if name
        (if (string-ends-with (format nil "~A" name) ".sir")
            (save-interactive-routine-action (controller (view-controller editor)) name :overwrite t)
          (save-model-action (controller (view-controller editor)) name :overwrite t))
      (if (not (equalp nil (pathname (controller (view-controller editor)))))
          (save-model-action (controller (view-controller editor)) (pathname (controller (view-controller editor)))))))
)

(defmethod pop-directory ((path pathname))
  (let ((duplicate (merge-pathnames path))
        (dirs nil))
    (setf dirs (subseq (pathname-directory duplicate) 0 (1- (length (pathname-directory duplicate)))))
    (merge-pathnames (make-pathname :directory dirs) duplicate)
))

(defmethod pop-directory ((path string))
  (pop-directory (merge-pathnames path))
)

(defmethod pop-directory (path)
  (system:get-folder-path :documents))

(defmethod save-sanlab-model-as ((editor editor))
  (let ((name (capi:prompt-for-file "Select model file"
                                    :filter "*.san"
                                    :filters '("SANLab Model" "*.san" "SANLab Interactive Routine" "*.sir")
                                    :operation :save
                                    :pathname (let ((path (pathname (controller (view-controller editor)))))
                                                (if (or (equal nil path) (not (pathnamep path)))
                                                    (system:get-folder-path :documents)
                                                  (pop-directory path))))))
    (if name
        (if (string-ends-with (format nil "~A" name) ".sir")
            (save-interactive-routine-action (controller (view-controller editor)) name :overwrite t)
          (save-model-action (controller (view-controller editor)) name :overwrite t))))
)

(defmethod editor-layout-graph-with-width ((editor editor))
  (if (capi:display-dialog (make-instance 'layout-graph-preferences))
      (layout-graph-action (controller editor) :adjust-width t)))

(defmethod editor-layout-graph-without-width ((editor editor))
  (if (capi:display-dialog (make-instance 'layout-graph-preferences))
      (layout-graph-action (controller editor))))

(defmethod able-to-undo ((editor editor))
  (can-undo (controller (view-controller editor)))
)

(defmethod able-to-redo ((editor editor))
  (can-redo (controller (view-controller editor)))
)

(defmethod not-implemented ((editor editor))
  (capi:display-message "Not yet implemented")
)

(defmethod editor-run-model ((editor editor))
  (let ((num-trials (capi:prompt-for-integer "Run this many trials:" :min 1)))
    (if num-trials
        (run-model (controller (view-controller editor)) num-trials (update-model-run-state-generator editor)))))

(defmethod editor-generate-sample-trace ((editor editor))
  (let ((filepath (capi:prompt-for-file "Save Trace" :operation :save :filters '("Text File" ".txt") :if-exists :prompt)))
    (generate-sample-trace (controller editor) filepath)))

(defmethod editor-view-results ((editor editor))
  (block editor-view-results
    (let ((file (pathname (controller editor))))
      (if (equalp nil file) (return-from editor-view-results (capi:display-message "You must save your model before viewing results")))
      (let ((items (list-trials-in-bundle file)))
        (if (equalp nil items) (return-from editor-view-results (capi:display-message "You must run trials before you can load results")))
        (multiple-value-bind (chosen successp) (capi:prompt-with-list items "Select a results file:")
          (if (equalp nil successp) (return-from editor-view-results))
          (let* ((viewer (make-instance 'results-viewer
                                        :source (merge-pathnames (format nil "Results/~A.xls" chosen) file)
                                        :the-results (mapcar #'(lambda (x) (setf (trial-result-model x) (model (controller editor))) x) (read-trials-from-bundle file chosen))
                                        :title (format nil "Results for \"~A\"" chosen)))
                 (res-list (results-viewer-list viewer)))
            (add-interface-to-view viewer 'sanlab-results-viewer (view-controller editor))
            (capi:apply-in-pane-process res-list #'(setf capi:collection-items) (results-viewer-results viewer) res-list)
            (capi:display viewer)))))))

(defmethod editor-retrieve-random-state ((editor editor))
  (retrieve-random-state (controller editor))
)

(defun update-model-run-state-generator (editor)
  #'(lambda (&rest args)
      (cond ((equal (first args) 'stopped)
             (capi:apply-in-pane-process
              editor
              #'(lambda ()
                  (setf (capi:title-pane-text (tool-pane editor)) "Processor stopped."))))
            ((equal (first args) 'update)
             (capi:apply-in-pane-process
              editor
              #'(lambda ()
                  (setf (capi:title-pane-text (tool-pane editor)) (apply #'format nil "Computing trial ~A of ~A... (~A trials/sec)" (rest args))))))
            ((equal (first args) 'finished)
             (capi:apply-in-pane-process
              editor
              #'(lambda ()
                  (setf (capi:title-pane-text (tool-pane editor)) "Done."))))
            ((equal (first args) 'error)
             (capi:apply-in-pane-process
              editor
              #'(lambda ()
                  (capi:raise-interface editor)
                  (setf (capi:title-pane-text (tool-pane editor)) "Error.")
                  (cond ((equal (type-of (second args)) 'network-error)
                         (deselect-all (controller editor))
                         (setf (selected? (first (activities (second args)))) t)
                         (capi:prompt-with-message (format nil "~A~%~%The offending activity should be selected in the Model Editor." (second args)) :owner editor)
                         )
                        (t
                         (capi:prompt-with-message (format nil "Processor failed with error ~A" (second args)) :owner editor)))
                  )))
            ((equal (first args) 'started)
             (capi:apply-in-pane-process
              editor
              #'(lambda ()
                  (setf (capi:title-pane-text (tool-pane editor)) "Starting processor..."))))
            ((equal (first args) 'results)
             (setf (last-ct-tracker editor) (fourth args)))
)))

(defmethod scroll ((editor editor) &rest args)
  (apply #'capi:scroll (main-layout editor) args))

(defmethod editor-select-by-type ((editor editor))
  (multiple-value-bind (choice successp)
      (prompt-with-tree-view (get-base-operators (controller editor)) "Choose activity type:" :expanded-p t)
    (cond (successp
           (deselect-all (controller editor))
           (select-by-type (controller editor) choice)))))

(defmethod prompt-with-tree-view (roots title &key (child-function #'get-children) expanded-p)
  (let ((tree (make-instance 'capi:tree-view
                             :visible-min-height 300
                             :visible-min-width 250
                             :print-function #'get-full-name
                             :roots roots
                             :children-function child-function
                             :expandp-function (if expanded-p #'true #'false)
                             :selection-callback #'(lambda (intf) (capi:redisplay-interface intf))
                             :callback-type :interface
)))
    (capi:popup-confirmer tree title
                          :value-function #'(lambda (pane) (if (capi:choice-selected-item pane) (capi:choice-selected-item pane) (values nil t))))))

(pass controller editor view-controller)
(pass model-changed editor controller :alternate-name model-changed?)
;(pass interaction-mode editor activity-layout :alternate-name current-mode)
;(pass-setf interaction-mode editor activity-layout :alternate-name current-mode)
(pass-all interaction-mode editor activity-layout :accessor main-layout :alternate-name current-mode)
(pass-all current-interactive-routine editor activity-layout :accessor main-layout :alternate-name current-interactive-routine)
(pass grid-size editor controller)

(defmethod (setf interaction-mode) :after (val (self editor))
  (let ((toolbox (viewhash 'sanlab-toolbox (view-controller self))))
    (setf (capi:title-pane-text (tool-pane self)) (cond ((equal val 'select)
                                                         (select-iroutine (ir-pinboard toolbox) nil nil)
                                                         (setf (capi:choice-selection (activity-list toolbox)) nil)
                                                         (setf (capi:button-image (slot-value toolbox 'arrow-button)) (app-property 'image-arrow-selected))
                                                         (setf (capi:button-image (slot-value toolbox 'crosshair-button)) (app-property 'image-crosshair))
                                                         (setf (capi:button-image (slot-value toolbox 'note-button)) (app-property 'image-text))
                                                         "Select Tool")
                                                        ((equal val 'connect)
                                                         (select-iroutine (ir-pinboard toolbox) nil nil)
                                                         (setf (capi:button-image (slot-value toolbox 'arrow-button)) (app-property 'image-arrow))
                                                         (setf (capi:button-image (slot-value toolbox 'crosshair-button)) (app-property 'image-crosshair-selected))
                                                         (setf (capi:button-image (slot-value toolbox 'note-button)) (app-property 'image-text))
                                                         (setf (capi:choice-selection (activity-list toolbox)) nil)
                                                         "Connect Tool")
                                                        ((equal val 'note)
                                                         (select-iroutine (ir-pinboard toolbox) nil nil)
                                                         (setf (capi:choice-selection (activity-list toolbox)) nil)
                                                         (setf (capi:button-image (slot-value toolbox 'arrow-button)) (app-property 'image-arrow))
                                                         (setf (capi:button-image (slot-value toolbox 'crosshair-button)) (app-property 'image-crosshair))
                                                         (setf (capi:button-image (slot-value toolbox 'note-button)) (app-property 'image-text-selected))
                                                         "Note Tool")
                                                        ((activity-pointerp val)
                                                         (select-iroutine (ir-pinboard toolbox) nil nil)
                                                         (format nil "Create ~A" val))
                                                        ((interactive-routine-pointerp val)
                                                         (setf (capi:choice-selection (activity-list toolbox)) nil)
                                                         (format nil "Create ~A" (name val)))))
    (if (not (interactive-routine-pointerp val))
        (if (current-interactive-routine (main-layout self))
            (progn
              (capi:manipulate-pinboard (main-layout self) (current-interactive-routine (main-layout self)) :delete)
              (setf (current-interactive-routine (main-layout self)) nil))))
))

(defmethod change-activity-pane-type ((self editor))
  (multiple-value-bind (choice successp)
      (prompt-with-tree-view (get-base-operators (controller self)) "Pick new type:" :expanded-p t)
    (cond (successp
           (change-selected-activity-types (main-layout self) choice))))
)
#|
  (let ((type (capi:prompt-with-list (mapcar #'get-full-name (get-all-activity-types)) "Pick new type:")))
    (if type
        (change-selected-activity-types (main-layout self) (make-instance 'activity-pointer :pointer (get-activity-by-typename type))))))
|#