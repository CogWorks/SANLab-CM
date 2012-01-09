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

(capi:define-interface layout-graph-preferences ()
  ()
  (:panes
   (sort-list
    draggable-layout
    :items (copy-list (app-property 'layout-graph-order))
    :accessor sort-list)
   (other-list
    capi:list-panel
    :items (let ((items (app-property 'layout-graph-order)))
             (remove-if #'(lambda (x) (member x items :test #'equal)) (mapcar #'get-full-name (get-all-activity-types))))
    :accessor other-list)
   (move-left
    capi:push-button
    :text "<"
    :selection-callback #'move-item-left
    :callback-type :interface)
   (move-right
    capi:push-button
    :text ">"
    :selection-callback #'move-item-right
    :callback-type :interface)
   (ok-button
    capi:push-button
    :default-p t
    :selection-callback #'(lambda (intf) (setf (app-property 'layout-graph-order) (draggable-layout-items (sort-list intf)))
                            (capi:exit-dialog (app-property 'layout-graph-order)))
    :callback-type :interface
    :text "OK")
   (cancel-button
    capi:push-button
    :cancel-p t
    :selection-callback #'capi:abort-dialog
    :text "Cancel"))
  (:layouts
   (main-layout
    capi:column-layout
    '(primary-layout button-layout)
    :adjust :right)
   (button-column
    capi:column-layout
    '(move-left move-right))
   (primary-layout
    capi:row-layout
    '(sort-list button-column other-list))
   (button-layout
    capi:row-layout
    '(cancel-button ok-button)))
  (:default-initargs
   :title "Layout Graph Order"
   :best-width 500
   :best-height 400
))

(defmethod move-item-left ((self layout-graph-preferences))
  (let ((item (capi:choice-selected-item (other-list self))))
    (setf (draggable-layout-items (sort-list self)) (append (draggable-layout-items (sort-list self)) (list item)))
    (setf (capi:collection-items (other-list self))
          (remove-if #'(lambda (x) (member x (draggable-layout-items (sort-list self)) :test #'equal))
                     (mapcar #'get-full-name (get-all-activity-types))))))

(defmethod move-item-right ((self layout-graph-preferences))
  (let ((item (draggable-item-item (draggable-layout-selected (sort-list self)))))
    (setf (draggable-layout-items (sort-list self)) (remove item (draggable-layout-items (sort-list self))))
    (setf (capi:collection-items (other-list self))
          (remove-if #'(lambda (x) (member x (draggable-layout-items (sort-list self)) :test #'equal))
                     (mapcar #'get-full-name (get-all-activity-types))))))