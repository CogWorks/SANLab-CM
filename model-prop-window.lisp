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

(capi:define-interface model-property-window (interface-view)
  ((owner :initform (app-property 'current-controller) :accessor owner))
  (:panes
   (title-pane
    capi:text-input-pane
    :accessor title-pane
    :visible-min-width 300
    :text (title (app-property 'current-controller))
    :editing-callback #'model-property-window-title-edited
    :title "Title")
   (author-pane
    capi:text-input-pane
    :accessor author-pane
    :visible-min-width 300
    :text (author (app-property 'current-controller))
    :editing-callback #'model-property-window-author-edited
    :title "Author")
   (width-pane
    capi:text-input-pane
    :editing-callback #'model-property-window-width-edited
    :accessor width-pane
    :visible-min-width 100
    :text (format nil "~A" (width (app-property 'current-controller)))
    :title "Width")
   (height-pane
    capi:text-input-pane
    :editing-callback #'model-property-window-height-edited
    :accessor height-pane
    :visible-min-width 100
    :text (format nil "~A" (height (app-property 'current-controller)))
    :title "Height")
   (notes-pane
    capi:multi-line-text-input-pane
    :editing-callback #'model-property-window-notes-edited
    :accessor notes-pane
    :visible-min-width 400
    :visible-min-height 300
    :text (notes (app-property 'current-controller))
    :title "Notes"
    :title-position :top))
  (:layouts
   (main-layout
    capi:column-layout
    '(title-pane author-pane dimension-layout notes-pane))
   (dimension-layout
    capi:row-layout
    '(width-pane height-pane)))
  (:default-initargs
   :destroy-callback #'on-close
   :activate-callback #'on-activate
   :title "Model Properties"
   :window-styles '(:toolbox)))

(defun model-property-window-title-edited (pane type)
  (standard-text-pane-editing-callback pane type)
  (let ((window (capi:element-interface pane)))
    (cond ((equal type :end)
           (if (not (equal (title (owner window)) (capi:text-input-pane-text (title-pane window))))
               (set-model-title (owner window) (capi:text-input-pane-text (title-pane window))))))))

(defun model-property-window-author-edited (pane type)
  (standard-text-pane-editing-callback pane type)
  (let ((window (capi:element-interface pane)))
    (cond ((equal type :end)
           (if (not (equal (author (owner window)) (capi:text-input-pane-text (author-pane window))))
               (set-model-author (owner window) (capi:text-input-pane-text (author-pane window))))))))

(defun model-property-window-width-edited (pane type)
  (standard-text-pane-editing-callback pane type)
  (let* ((window (capi:element-interface pane))
         (width (read-from-string (capi:text-input-pane-text (width-pane window)))))
    (cond ((equal type :end)
           (if (not (equal (width (owner window)) width))
               (set-model-width (owner window) width))))))

(defun model-property-window-height-edited (pane type)
  (standard-text-pane-editing-callback pane type)
  (let* ((window (capi:element-interface pane))
         (height (read-from-string (capi:text-input-pane-text (height-pane window)))))
    (cond ((equal type :end)
           (if (not (equal (height (owner window)) height))
               (set-model-height (owner window) height))))))

(defun model-property-window-notes-edited (pane type)
  (standard-text-pane-editing-callback pane type)
  (let ((window (capi:element-interface pane)))
    (cond ((equal type :end)
           (if (not (equal (notes (owner window)) (capi:text-input-pane-text (notes-pane window))))
               (set-model-notes (owner window) (capi:text-input-pane-text (notes-pane window))))))))

(defmethod on-activate ((window model-property-window) activep)
  (capi:display-message "activep = ~A" activep)
  (cond ((not activep) (on-close window)))
)

(defmethod on-close ((window model-property-window))
  (if (not (equal (title (owner window)) (capi:text-input-pane-text (title-pane window))))
      (set-model-title (owner window) (capi:text-input-pane-text (title-pane window))))
  (if (not (equal (author (owner window)) (capi:text-input-pane-text (author-pane window))))
      (set-model-author (owner window) (capi:text-input-pane-text (author-pane window))))
  (let ((width (read-from-string (capi:text-input-pane-text (width-pane window))))
        (height (read-from-string (capi:text-input-pane-text (height-pane window)))))
    (if (not (equal (width (owner window)) width))
        (set-model-width (owner window) width))
    (if (not (equal (height (owner window)) height))
        (set-model-height (owner window) height)))
  (if (not (equal (notes (owner window)) (capi:text-input-pane-text (notes-pane window))))
      (set-model-notes (owner window) (capi:text-input-pane-text (notes-pane window))))
)
