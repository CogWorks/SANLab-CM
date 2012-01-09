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

(defclass opaque-pointer ()
  ((pointer :initform nil :initarg :pointer :accessor pointer)))

(defclass activity-pointer (opaque-pointer)
  ())

(defclass distribution-pointer (opaque-pointer)
  ())

(defclass instance-pointer (opaque-pointer)
  ())

(defclass interactive-routine-pointer (opaque-pointer)
  ())

(defclass interactive-routine-task-pointer (opaque-pointer)
  ())

(defclass interactive-routine-instance-pointer (opaque-pointer)
  ())

(defclass rendered-note-pointer (opaque-pointer)
  ())

(defun opaque-pointerp (obj)
  (subtypep (type-of obj) (find-class 'opaque-pointer)))

(defun activity-pointerp (obj)
  (subtypep (type-of obj) (find-class 'activity-pointer)))

(defun distribution-pointerp (obj)
  (subtypep (type-of obj) (find-class 'distribution-pointer)))

(defun instance-pointerp (obj)
  (subtypep (type-of obj) (find-class 'instance-pointer)))

(defun interactive-routine-pointerp (obj)
  (subtypep (type-of obj) (find-class 'interactive-routine-pointer)))

(defun interactive-routine-task-pointerp (obj)
  (subtypep (type-of obj) (find-class 'interactive-routine-task-pointer)))

(defun interactive-routine-instance-pointerp (obj)
  (subtypep (type-of obj) (find-class 'interactive-routine-instance-pointer)))

(defun rendered-note-pointerp (obj)
  (subtypep (type-of obj) (find-class 'rendered-note-pointer)))

(defmethod pointer= ((p1 opaque-pointer) (p2 opaque-pointer))
  (if (and (null (pointer p1)) (null (pointer p2))) (return-from pointer= t))
  (if (not (equal (type-of p1) (type-of p2))) (return-from pointer= nil))
  (equal (pointer p1) (pointer p2)))

(defmethod pointer= (p1 p2)
  (equal p1 p2))

(defmethod get-param-count ((dist simple-base-string))
  (cond ((equal dist "Constant") 1)
        ((equal dist "Gaussian") 2)
        ((equal dist "Beta") 4)
        (t 0)))
#|
(defmethod get-param-count ((dist distribution-pointer))
)
|#
(defmethod get-param-name ((dist simple-base-string) (i fixnum))
  (cond ((equal dist "Constant") (nth i '("Constant")))
        ((equal dist "Gaussian") (nth i '("Mean" "Variance")))
        ((equal dist "Beta") (nth i '("A" "B" "Alpha" "Beta")))
        (t "")))
#|
(defmethod get-param-name ((dist distribution-pointer) (i fixnum))
)
|#
(defmethod print-object ((ap activity-pointer) (s stream))
  (if (pointer ap)
      (print-object (pointer ap) s)
    (format s "~A" nil)))
