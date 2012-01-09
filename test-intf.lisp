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

(compile-file (lw:current-pathname "activity-pane.lisp"))
(compile-file (lw:current-pathname "activity-layout.lisp"))
(load (lw:current-pathname "activity-pane.xfasl"))
(load (lw:current-pathname "activity-layout.lisp"))

(capi:define-interface test-intf (interface-view)
  ()
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
    activity-layout
    '()
    :accessor main-layout
    :visible-min-width 500
    :visible-min-height 500))
  (:default-initargs
   :title "Test sample"
   :best-x 160
   :best-y 110
   :best-width 1275
   :best-height 750
))

(defmethod current-mode ((intf test-intf))
  (current-mode (main-layout intf)))

(defmethod (setf current-mode) (val (intf test-intf))
  (setf (current-mode (main-layout intf)) val))

(defmethod (setf current-mode) :after (val (intf test-intf))
  (cond ((symbolp val)
         (setf (capi:title-pane-text (tool-pane intf))
               (cond ((equal val 'select) "Selection Tool")
                     ((equal val 'connect) "Connection Tool"))))
        ((activity-pointerp val)
         (setf (capi:title-pane-text (tool-pane intf))
               (format nil "Create ~A" (pointer val))))))

(defmethod selected-activities ((intf test-intf))
  (selected-activities (main-layout intf)))

(defmethod (setf selected-activities) (val (intf test-intf))
  (setf (selected-activities (main-layout intf)) val))

(defmethod changed-selection ((intf test-intf))
  (changed-selection (main-layout intf)))

(defmethod (setf changed-selection) (val (intf test-intf))
  (setf (changed-selection (main-layout intf)) val))

;(defparameter *sample-pane* (make-instance 'activity-pane))
;(defparameter *sample-pane-2* (make-instance 'activity-pane :x 300 :y 300 :background-color #(:RGB 0.6 1.0 0.5)))
(defparameter *test-interface* (make-instance 'test-intf))
;(setf (activities (main-layout *test-interface*)) (list *sample-pane* *sample-pane-2*))
(defparameter *test-toolbox* (make-instance 'toolbox))
(defparameter *view-controller* (make-instance 'view-controller))
(add-interface-to-view *test-interface* 'sanlab-model-editor *view-controller*)
(add-interface-to-view *test-toolbox* 'sanlab-toolbox *view-controller*)
(setf (primary-view *view-controller*) *test-interface*)
