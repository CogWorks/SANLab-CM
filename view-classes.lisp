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

(defclass interface-with-property-list (capi:interface)
  ((property-list :initform nil :accessor property-list)))

(defclass interface-view (interface-with-property-list)
  ((view-controller :initform nil :initarg :view-controller :accessor view-controller))
  (:default-initargs
   :destroy-callback #'destroy-controller-if-primary)
)

(defun destroy-controller-if-primary (intf)
  (if (and (view-controller intf) (eq intf (primary-view (view-controller intf))))
      (destroy-view-controller-primary (view-controller intf)))
)

(defmethod view-property ((sym symbol) (intf interface-with-property-list))
  (do ((l (property-list intf) (cddr l)))
      ((not l) nil)
    (if (equal (car l) sym) (return-from view-property (cadr l)))))

(defmethod (setf view-property) (value (sym symbol) (intf interface-with-property-list))
  (do ((l (property-list intf) (cddr l)))
      ((not l))
    (if (equal (car l) sym) (return (setf (cadr l) value))))
  (push value (property-list intf))
  (push sym (property-list intf))
  value)

(defclass view-controller ()
  ((controller :initform nil :initarg :controller :accessor controller)
   (views :initform (make-hash-table :test #'equal) :initarg :views :accessor views)
   (primary-view :initform nil :initarg :primary :accessor primary-view)))

(defmethod hide-view-controller ((vc view-controller))
  (maphash #'(lambda (key x) (declare (ignore key)) (capi:apply-in-pane-process x #'hide-interface x nil)) (views vc))
)

(defmethod show-view-controller ((vc view-controller))
  (maphash #'(lambda (key x) (declare (ignore key)) (capi:apply-in-pane-process x #'show-interface x)) (views vc))
  (if (primary-view vc)
      (capi:apply-in-pane-process (primary-view vc) #'capi:raise-interface (primary-view vc)))
)

(defmethod destroy-view-controller ((vc view-controller))
  (maphash #'(lambda (key x) (declare (ignore key)) (capi:apply-in-pane-process x #'capi:quit-interface x)) (views vc))
)

(defmethod destroy-view-controller-primary ((vc view-controller))
  (maphash #'(lambda (key x) (declare (ignore key)) (if (not (eq x (primary-view vc)))
                                                        (capi:apply-in-pane-process x #'capi:quit-interface x))) (views vc)))

(defmethod activate-view-controller ((vc view-controller))
  (setf (app-property 'current-controller) (controller vc)))

(defmethod add-interface-to-view ((intf interface-view) (sym symbol) (vc view-controller))
  (setf (gethash sym (views vc)) intf)
  (setf (view-controller intf) vc)
)

(defmethod (setf viewhash) (intf (sym symbol) (vc view-controller))
  (setf (gethash sym (views vc)) intf)
  (setf (view-controller intf) vc)
)

(defmethod viewhash ((sym symbol) (vc view-controller))
  (gethash sym (views vc)))

(defmethod interface-displayed ((intf capi:interface))
  (or (capi:interface-visible-p intf) (capi:interface-iconified-p intf)))

(defmethod interface-displayed (intf)
  nil)

(defmethod display-view-controller ((vc view-controller))
  (maphash #'(lambda (key value) (declare (ignore key)) (capi:display value)) (views vc)))

