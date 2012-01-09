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

(defparameter *draggable-layout-item-height* 17)

(defconstant *draggable-layout-line-color* #(:RGB 0.223529S0 0.337254S0 0.839215S0))

(defclass draggable-item (capi:drawn-pinboard-object)
  ((item :initform nil :initarg :item :accessor draggable-item-item))
  (:default-initargs
   :display-callback #'draw-draggable-item)
)

(defmethod draw-draggable-item ((pane capi:output-pane) (self draggable-item) x y width height)
  (declare (ignore x y width height))
  (block draw-draggable-item
    (let ((print-function (draggable-layout-print-function pane)))
      (if (symbolp print-function) (setf print-function (symbol-function print-function)))
      (capi:with-geometry self
        (multiple-value-bind (left top right bottom) (gp:get-string-extent pane (funcall print-function (draggable-item-item self)))
          (declare (ignore left top bottom))
          (if (< (draggable-layout-max-width pane) right)
              (and (setf (draggable-layout-max-width pane) right) (return-from draw-draggable-item))))
        (if (< capi:%width% (draggable-layout-max-width pane))
            (and (setf (capi:pinboard-pane-size self) (values (draggable-layout-max-width pane) *draggable-layout-item-height*))
                 (setf (draggable-layout-max-width pane) (draggable-layout-max-width pane))
                 (return-from draw-draggable-item)))
        (gp:draw-rectangle pane capi:%x% capi:%y% capi:%width% capi:%height%
                           :foreground (if (eq self (draggable-layout-selected pane)) (color:make-rgb 0.337254 0.549019 0.862745) (color:make-rgb 1.0 1.0 1.0))
                           :filled t)
        (gp:draw-string pane (funcall print-function (draggable-item-item self)) capi:%x% (+ capi:%y% (- *draggable-layout-item-height* 3)))
))))

(defclass draggable-layout (capi:pinboard-layout)
  ((items :initform nil :initarg :items :accessor draggable-layout-items)
   (allow-new-items :initform nil :initarg :allow-new-items :accessor draggable-layout-allow-new-items)
   (selected :initform nil :initarg :selected :accessor draggable-layout-selected)
   (print-function :initform #'identity :initarg :print-function :accessor draggable-layout-print-function)
   (max-width :initform 3000 :accessor draggable-layout-max-width)
   (description-items :initform nil :accessor description-items)
   (place-line :initform nil :accessor place-line))
  (:default-initargs
   :fit-size-to-children nil
   :visible-border t
   :internal-border 1
   :font (gp:make-font-description :family "Verdana" :size 12)
   :input-model '(((:button-1 :motion) draggable-layout-drag)
                  ((:button-1 :press) draggable-layout-select))
   :drop-callback #'draggable-layout-drop)
)

(defmethod (setf draggable-layout-items) :before (val (self draggable-layout))
  (if (null val) (break)))

(defmethod draggable-layout-drag ((self draggable-layout) x y)
  (let ((object (capi:pinboard-object-at-position self x y)))
    (if object (capi:drag-pane-object self object :plist (list :capi-item object) :operations '(:move))))
)

(defmethod draggable-layout-select ((self draggable-layout) x y)
  (let ((pane (capi:pinboard-object-at-position self x y))
        (old-pane (draggable-layout-selected self)))
    (setf (draggable-layout-selected self) pane))
  (dolist (i (capi:layout-description self))
    (capi:redraw-pinboard-object i)))

(defmethod (setf draggable-layout-max-width) :after (val (self draggable-layout))
  (dolist (i (capi:layout-description self))
    (setf (capi:pinboard-pane-size i) (values val *draggable-layout-item-height*)))
  (dolist (i (capi:layout-description self))
    (capi:draw-pinboard-object self i))
)

(defmethod initialize-instance :after ((self draggable-layout) &rest initargs)
  (build-item-descriptions self)
)

(defmethod (setf draggable-layout-items) :after (val (self draggable-layout))
  (build-item-descriptions self)
)

(defmethod build-item-descriptions ((self draggable-layout))
  (let ((items (mapcar #'(lambda (x) (make-instance 'draggable-item :item x)) (draggable-layout-items self)))
        (y 0)
        (max-width 0)
        (print-function (draggable-layout-print-function self)))
    (if (symbolp print-function) (setf print-function (symbol-function print-function)))
    (dolist (x items)
      (setf (capi:pinboard-pane-position x) (values 0 y))
      (incf y *draggable-layout-item-height*)
;      (multiple-value-bind (left top right bottom) (gp:get-string-extent self (funcall print-function x) #|(capi:simple-pane-font self)|#)
;        (declare (ignore left top bottom))
;        (setf max-width (max max-width right)))
      )
    (dolist (x items)
      (setf (capi:pinboard-pane-size x) (values max-width *draggable-layout-item-height*)))
    (setf (capi:layout-description self) items)
    (setf (description-items self) items)
))

(defmethod longest-item-width ((self draggable-layout))
  (let ((x 0))
    (dolist (item (description-items self) x)
      (capi:with-geometry item
        (setf x (max x capi:%width%))))))

(defun before (i1 i2 l &key test key)
  (member i2 (member i1 l :test test :key key) :test test :key key))

(defmethod get-pane-at-position ((self draggable-layout) x y)
  (block get-pane-at-position
    (dolist (i (description-items self))
      (capi:with-geometry i
        (if (and (<= capi:%x% x) (<= x (+ capi:%x% capi:%width%))
                 (<= capi:%y% y) (<= y (+ capi:%y% capi:%height%)))
            (return-from get-pane-at-position i)))))
)

(defmethod get-insert-y ((self draggable-layout) my &key item)
  (block get-insert-y
  (let ((list-item (member item (description-items self) :key #'draggable-item-item)))
    (if list-item (setf list-item (car list-item)))
    (if list-item
        (capi:with-geometry list-item
          (if (and (< (- capi:%y% (/ capi:%height% 2)) my)
                   (< my (+ capi:%y% capi:%height% (/ capi:%height% 2))))
              (return-from get-insert-y))))
    (if (not list-item) (setf list-item (get-pane-at-position self 1 my)))
    (if (null list-item)
        (if (null (capi:layout-description self))
            0
          (capi:with-geometry (car (last (description-items self)))
            (+ capi:%y% capi:%height%)))
      (capi:with-geometry list-item
        (if (< my (+ capi:%y% (/ capi:%height% 2)))
            capi:%y%
          (+ capi:%y% capi:%height%))))
)))

(defun insert-after (obj target list)
  (do ((i list (cdr i)))
      ((null i))
    (if (eq (car i) target)
        (and (setf (cdr i) (cons obj (cdr i))) (return-from insert-after list))))
)

(defun insert-before (obj target list)
  (if (eq obj target) (return-from insert-before list))
  (if (eq (first list) target)
      (push obj list)
    (do ((i list (cdr i)))
        ((null i))
      (if (and (cdr i) (eq (cadr i) target))
          (and (push obj (cdr i)) (return-from insert-before list))))))

(defmethod draggable-layout-drop ((self draggable-layout) drop-object stage)
  (block draggable-layout-drop
    (format t "Inside draggable-layout-drop callback.~%")
    (format t "Stage = ~A~%" stage)
  (cond ((equal stage :formats)
         (capi:set-drop-object-supported-formats drop-object (if (draggable-layout-allow-new-items self) '(:capi-item :value) '(:capi-item :value :string))))
        ((equal stage :enter)
         (setf (capi:drop-object-drop-effect drop-object) (if (or (capi:drop-object-provides-format drop-object :value) (capi:drop-object-provides-format drop-object :capi-item)) :move
                                                            (if (capi:drop-object-provides-format drop-object :string)
                                                                :copy
                                                              :none))))
        ((equal stage :drag)
         (setf (capi:drop-object-drop-effect drop-object) (if (or (capi:drop-object-provides-format drop-object :value) (capi:drop-object-provides-format drop-object :capi-item)) :move
                                                            (if (capi:drop-object-provides-format drop-object :string)
                                                                :copy
                                                              :none)))
         (let ((insert-y (get-insert-y self (capi:drop-object-pane-y drop-object) :item (capi:drop-object-get-object drop-object self :value)))
               (width (longest-item-width self)))
           (if (and (null insert-y) (place-line self))
               (setf (capi:layout-description self) (remove (place-line self) (capi:layout-description self))
                     (place-line self) nil))
           (if (null insert-y) (return-from draggable-layout-drop))
           (if (place-line self)
               (capi:move-line (place-line self) 0 insert-y width insert-y)
             (progn
               (setf (place-line self) (make-instance 'capi:line-pinboard-object :start-x 0 :start-y insert-y :end-x width :end-y insert-y :graphics-args (list :foreground *draggable-layout-line-color* :thickness 2)))
               (setf (capi:layout-description self) (append (capi:layout-description self) (list (place-line self))))))))
        ((equal stage :drop)
         (let ((insert-y (get-insert-y self (capi:drop-object-pane-y drop-object) :item (capi:drop-object-get-object drop-object self :value)))
               (item (capi:drop-object-get-object drop-object self :value)))
           (if (place-line self)
               (setf (capi:layout-description self) (remove (place-line self) (capi:layout-description self))
                     (place-line self) nil))
           (if (null insert-y) (return-from draggable-layout-drop))
           (setf item (draggable-item-item item))
           (cond (item
                  (let ((item-before (get-pane-at-position self 0 (1+ insert-y)))
                        (items (copy-list (draggable-layout-items self))))
                    (block escape
                      (if (and item-before (eq item (draggable-item-item item-before))) (return-from escape))
;                      (break)
                      (setf items (remove item items))
                      (if item-before
                          (setf (draggable-layout-items self) (insert-before item (draggable-item-item item-before) items))
                        (setf (draggable-layout-items self) (append items (list item))))
                      ))
                  ))
           )
         (setf (capi:drop-object-drop-effect drop-object) (if (or (capi:drop-object-provides-format drop-object :value) (capi:drop-object-provides-format drop-object :capi-item)) :move
                                                            (if (capi:drop-object-provides-format drop-object :string)
                                                                :copy
                                                              :none)))
         (gp:invalidate-rectangle self)
         (dolist (i (capi:layout-description self))
           (capi:redraw-pinboard-object i t))
))))
