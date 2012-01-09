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

(defclass controller ()
  ((model :initform nil :initarg :model :accessor model)
   (history :initform nil :initarg :history :accessor history)))

(defmethod undo-action ((ctrl controller)))

(defmethod commit-action ((act action) (ctrl controller)))

(capi:define-interface iroutine-editor ()
  ())