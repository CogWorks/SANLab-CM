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

(capi:define-interface toolbox (interface-view)
  ()
  (:panes
   (activity-list
    capi:tree-view
    :visible-min-height 200
    :visible-min-width 120
    :print-function #'get-full-name
    :roots (get-base-operators (app-property 'current-controller))
    :children-function #'get-children
    :expandp-function #'true
    :selection nil
    :horizontal-scroll t
    :selection-callback #'change-tool-activity
    :accessor activity-list)
   (arrow-button
    capi:push-button
    :visible-border nil
    :data 'select
    :callback #'change-tool
    :image (app-property 'image-arrow-selected))
   (crosshair-button
    capi:push-button
    :visible-border nil
    :data 'connect
    :callback #'change-tool
    :image (app-property 'image-crosshair))
   (note-button
    capi:push-button
    :visible-border nil
    :data 'note
    :callback #'change-tool
    :image (app-property 'image-text)))
  (:layouts
   (main-layout
    capi:column-layout
    '(button-layout activity-list :divider ir-pinboard))
   (button-layout
    capi:row-layout
    '(arrow-button crosshair-button note-button))
   (ir-pinboard
    ir-list-layout
    nil
    :items nil
    :background :white
    :key-function #'name
    :accessor ir-pinboard
    :visible-min-width 220
    :visible-min-height 240
    :vertical-scroll t
    :horizontal-scroll t
    :font (gp:make-font-description :family "Verdana" :size 12)
    :input-model '(((:button-1 :press) select-iroutine)
                   ((:button-1 :motion) drag-iroutine-from))))
  (:default-initargs
   :title "Toolbox"
   :best-x 0
   :best-y 120
   :best-height 700
   :enable-tooltips t
   :help-callback #'(lambda (self pane type key)
                      (cond ((eql type :tooltip)
                             (cond ((eql pane (slot-value self 'arrow-button))
                                    "Select Tool - Select one or more activities or constraints to manipulate them.")
                                   ((eql pane (slot-value self 'crosshair-button))
                                    "Connect Tool - Establish a dependency between two activities.")
                                   ((eql pane (slot-value self 'note-button))
                                    "Notes Tool - Places free text onto the model for annotation")))))
   :window-styles '(:toolbox)))

(defmethod initialize-instance :after ((self toolbox) &rest initargs)
  (populate-with-interactive-routines self)
)

(defun change-tool (data intf)
  (if (symbolp data)
      (cond ((equal data 'select)
             (capi:activate-pane (slot-value intf 'arrow-button))
             (setf (capi:button-image (slot-value intf 'arrow-button)) (app-property 'image-arrow-selected))
             (setf (capi:button-image (slot-value intf 'crosshair-button)) (app-property 'image-crosshair))
             (setf (capi:button-image (slot-value intf 'note-button)) (app-property 'image-text)))
            ((equal data 'connect)
             (capi:activate-pane (slot-value intf 'crosshair-button))
             (setf (capi:button-image (slot-value intf 'arrow-button)) (app-property 'image-arrow))
             (setf (capi:button-image (slot-value intf 'crosshair-button)) (app-property 'image-crosshair-selected))
             (setf (capi:button-image (slot-value intf 'note-button)) (app-property 'image-text)))
            ((equal data 'note)
             (capi:activate-pane (slot-value intf 'note-button))
             (setf (capi:button-image (slot-value intf 'arrow-button)) (app-property 'image-arrow))
             (setf (capi:button-image (slot-value intf 'crosshair-button)) (app-property 'image-crosshair))
             (setf (capi:button-image (slot-value intf 'note-button)) (app-property 'image-text-selected)))
))
  (let ((editor (viewhash 'sanlab-model-editor (view-controller intf))))
    (setf (interaction-mode editor) data)))

(defun change-tool-activity (data intf)
  (if (not data) (return-from change-tool-activity))
  (setf (capi:button-image (slot-value intf 'arrow-button)) (app-property 'image-arrow))
  (setf (capi:button-image (slot-value intf 'crosshair-button)) (app-property 'image-crosshair))
  (setf (capi:button-image (slot-value intf 'note-button)) (app-property 'image-text))
  (let ((editor (viewhash 'sanlab-model-editor (view-controller intf))))
    (setf (interaction-mode editor) data)))