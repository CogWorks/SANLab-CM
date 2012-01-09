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

(defclass listener ()
  ((callback :initform nil :initarg :callback :accessor callback))
  (:documentation "The listener class that can be used to register callbacks in the controller")
)

(defclass controller ()
  ((model :initform nil :initarg :model :accessor model)
   (view :initform nil :initarg :view :accessor view)
   (event-callbacks :initform (make-hash-table) :accessor event-callbacks)
   (doing-batch-update :initform nil :accessor doing-batch-update)
   (batch-update :initform nil :accessor batch-updates)
   (undo-history :initform nil :accessor undo-history)
   (redo-history :initform nil :accessor redo-history))
  (:documentation "The controller class mediates interaction between the view controllers and the underlying model representation. It is in charge of keeping track of actions taken by the user and how to properly apply undo/redo operations, among other responsibilities.")
)

(defclass activity-type ()
  ((symname :initform nil :initarg :symname :accessor symname)
   (parent :initform nil :initarg :parent :accessor parent)
   (typename :initform nil :initarg :typename :accessor typename)
   (instname :initform nil :initarg :instname :accessor instname)
   (extra :initform nil :initarg :extra :accessor extra)
   (distribution :initform nil :initarg :distribution :accessor distribution)
   (default-params :initform nil :initarg :default-params :accessor default-params)
   (color :initform nil :initarg :color :accessor color))
  (:documentation "The activity type represents the abstract concept of the operator in CPM-GOMS and contains all of the necessary information for instantiating a new instance of the activity type.")
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

(defclass arrow (capi:pinboard-object)
  ((is-ir-arrow :initform nil :initarg :ir-arrow :reader is-ir-arrow? :writer (setf is-ir-arrow))
   (ir-color :initform nil :initarg :ir-color :accessor ir-color)
   (normal-arrow :initform (make-instance 'capi:arrow-pinboard-object :head :middle :head-breadth 4) :accessor normal-arrow)
   (ir-arrow :initform nil :accessor ir-arrow)
   (start :initform nil :initarg :start :accessor start)
   (end :initform nil :initarg :end :accessor end)
   (selected :initform nil :accessor selected)
)
  (:documentation "Visually represents the dependency of one activity on another.")
)

