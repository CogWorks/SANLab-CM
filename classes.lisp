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
