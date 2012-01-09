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

(defclass preference-pane-entry (capi:drawn-pinboard-object)
  ((image :initform nil :initarg :image :accessor image)
   (text :initform nil :initarg :text :accessor text)
   (external-image :initform nil :accessor external-image)
   (internal-image :initform nil :accessor internal-image)
   (will-display-callback :initform nil :initarg :will-display-callback :accessor will-display-callback)
   (save-callback :initform nil :initarg :save-callback :accessor save-callback)
   (selected? :initform nil :accessor selected?))
  (:default-initargs
   :display-callback #'draw-pref-pane-entry))

(defmethod initialize-instance :after ((self preference-pane-entry) &rest initargs)
  (if (image self)
      (setf (external-image self) (gp:read-external-image (image self) :type :bmp)))
)

(defun draw-pref-pane-entry (pane self x y w h)
  (if (and (external-image self) (not (internal-image self)))
      (setf (internal-image self) (gp:convert-external-image pane (external-image self))))
  (gp:clear-rectangle pane x y w h)
  (if (selected? self)
      (gp:draw-rectangle pane x y w h
                         :filled t
                         :foreground #(:RGB 0.6 0.7 0.9 1.0)))
  (multiple-value-bind (tx ty tw th) (gp:get-string-extent pane (text self))
    (let ((iw (gp:image-width (internal-image self)))
          (ih (gp:image-height (internal-image self))))
      (multiple-value-bind (myw myh) (capi:pinboard-pane-size self)
        (if (< myw (max (+ 4 (- tw tx)) (+ 4 iw)))
            (progn
              (setf (capi:pinboard-pane-size self) (values (max (+ 4 (- tw tx)) (+ 4 iw)) (+ 4 ih 4 (- th ty) 4)))
              (gp:invalidate-rectangle pane)
              (update-category-list pane)
              (return-from draw-pref-pane-entry)))
        (capi:with-geometry self
          (gp:draw-image pane (internal-image self)
                         (+ capi:%x% (/ (- capi:%width% iw) 2)) (+ 4 capi:%y%))
          (gp:draw-string pane (text self)
                          (+ capi:%x% (/ (- capi:%width% tw) 2)) (+ 16 capi:%y% ih)))))))

(capi:define-interface preferences-window (interface-view)
  ()
  (:panes
   (button1
    capi:push-button
    :text "Hello")
   (button2
    capi:push-button
    :text "World!"))
  (:layouts
   (primary-layout
    capi:column-layout
    '(category-list switched-layout))
   (category-list
    capi:pinboard-layout
    '()
    :visible-min-height 92
    :visible-max-height 92
    :accessor category-list)
   (switched-layout
    capi:switchable-layout
    '(general-preferences-layout)
    :combine-child-constraints t
    :accessor switched-layout)
   (general-preferences-layout
    capi:column-layout
    '(button1 button2)))
  (:default-initargs
   :title "Preferences"
   :visible-min-width 400)
)

(defmethod update-category-list ((self capi:pinboard-layout))
  (let ((x 0) (y 0))
  (dolist (pane (capi:layout-description self))
    (setf (capi:pinboard-pane-position pane) (values x y))
    (setf x (+ x (capi:pinboard-pane-size pane))))
  )
  (gp:invalidate-rectangle self)
  (capi:with-geometry self
    (capi:redraw-pinboard-layout self 0 0 capi:%width% capi:%height%))
  (dolist (pane (capi:layout-description self))
    (multiple-value-bind (x y) (capi:pinboard-pane-position pane)
      (multiple-value-bind (w h) (capi:pinboard-pane-size pane)
        (draw-pref-pane-entry self pane x y w h))))
)

(defmacro make-preference-pane (name text image will-render-callback save-callback &rest parts)
  `(let ((pref-pane nil)
         (panes (make-hash-table))
         (layouts (make-hash-table)))
     (defclass (quote ,name) (preference-pane-entry)
       ()
       (:default-initargs
        :image ,image
        :text ,text
        :will-render-callback ,will-render-callback
        :save-callback ,save-callback))
     (make-instance 'preference-pane-entry :image image :text text :will-render-callback will-render-callback :save-callback save-callback :pref-pane pref-pane))
)

(defun test-preferences ()
  (let ((pref-pane (make-instance 'preference-pane-entry :x 0 :y 0 :image "~/sanlab/GraphParamsIco.bmp" :text "Graph Layout"))
        (pref-window (make-instance 'preferences-window)))
    (setf (capi:layout-description (category-list pref-window)) (list pref-pane))
    (capi:display pref-window)))
