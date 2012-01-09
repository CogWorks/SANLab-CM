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

(defclass multiline-pinboard-object (capi:drawn-pinboard-object)
  ((source :initform nil :initarg :source :accessor source)
   (text :initform "" :initarg :text :accessor multiline-pinboard-object-text)
   (line-height :initform 0 :accessor multiline-pinboard-object-line-height))
  (:default-initargs
   :display-callback #'multiline-pinboard-draw))

(defmethod multiline-pinboard-objectp ((x multiline-pinboard-object))
  t)

(defmethod multiline-pinboard-objectp (x)
  nil)

(defmethod initialize-instance :after ((self multiline-pinboard-object) &rest initargs)
  (let ((items (split-string (multiline-pinboard-object-text self) #\Newline))
        (max-width 0)
        (max-height 0))
    (dolist (i items)
      (multiple-value-bind (left top right bottom) (gp:get-string-extent *invisible-output-pane* i)
        (setf max-width (max max-width (- right left)))
        (setf max-height (max max-height (- bottom top)))))
    (setf (multiline-pinboard-object-line-height self) (1+ max-height))
    (setf (capi:pinboard-pane-size self) (values max-width (* (length items) max-height))))
)

(defmethod (setf text) :after (val (self multiline-pinboard-object))
  (let ((items (split-string (multiline-pinboard-object-text self) #\Newline))
        (max-width 0)
        (max-height 0))
    (dolist (i items)
      (multiple-value-bind (left top right bottom) (gp:get-string-extent *invisible-output-pane* i)
        (setf max-width (max max-width (- right left)))
        (setf max-height (max max-height (- bottom top)))))
    (setf (multiline-pinboard-object-line-height self) (1+ max-height))
    (setf (capi:pinboard-pane-size self) (values max-width (* (length items) max-height))))
)

(defun multiline-pinboard-draw (pane self x y w h)
  (declare (ignore x y w h))
  (let ((items (split-string (multiline-pinboard-object-text self) #\Newline))
        (start 0))
    (capi:with-geometry self
      (setf start (+ capi:%y% (multiline-pinboard-object-line-height self)))
      (dolist (i items)
        (gp:draw-string pane i capi:%x% start)
        (setf start (+ start (multiline-pinboard-object-line-height self)))))))
