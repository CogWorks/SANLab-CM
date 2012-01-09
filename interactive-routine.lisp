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

(defclass ir-task ()
  ((uid :initform nil :initarg :uid :accessor uid)
   (operator-type :initform nil :initarg :operator-type :accessor operator-type)
   (offset-x :initform nil :initarg :offset-x :accessor offset-x)
   (offset-y :initform nil :initarg :offset-y :accessor offset-y)
   (edges-in :initform nil :initarg :edges-in :accessor edges-in)
   (edges-out :initform nil :initarg :edges-out :accessor edges-out)
   (label :initform nil :initarg :label :accessor label)
   (distribution :initform nil :initarg :distribution :accessor distribution)
   (parameters :initform nil :initarg :parameters :accessor parameters)
   (measurable :initform nil :initarg :measurable :accessor measurable)
))

(defclass interactive-routine ()
  ((task-list :initform nil :initarg :task-list :accessor task-list)
   (highlight-color :initform nil :initarg :highlight-color :accessor highlight-color)
   (name :initform "" :initarg :name :accessor name)
))

