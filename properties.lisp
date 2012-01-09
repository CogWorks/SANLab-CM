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

(let ((properties (make-hash-table)))

(defmethod app-property ((sym symbol))
  (gethash sym properties))

(defmethod (setf app-property) (val (sym symbol))
  (setf (gethash sym properties) val))
)

(setf (app-property 'app-name) "SANLab-CM")
(setf (app-property 'app-version) "3.0.0")
(setf (app-property 'allowed-versions) (list "2.0.1" "2.0.2" "2.0.3" "2.0.4" "2.0.5" "2.1.0" "3.0.0"))
(setf (app-property 'current-controller) nil)

(defun load-properties (prop-file)
;  (with-open-file (file prop-file :direction :input)
;    (read file))
  (load prop-file)
)

(defmacro with-app-property (prop sym &body body)
  `(let ((,sym (app-property ,prop)))
     ,@body))

(defmacro with-app-properties (fname &body body)
  (let ((temp (mapcar #'(lambda (x) (setf (cadr x) (list 'app-property (list 'quote (cadr x)))) x) fname)))
    `(let (,@temp)
       ,@body)))
