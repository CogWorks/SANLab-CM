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

(let ((log-base-name "/sanlab-")
      (log-file-name nil)
      (enable-logging t))

(defun init-logging ()
  (let ((dt (multiple-value-bind (s min h d mon y) (get-decoded-time)
              (format nil "~4,'0D~2,'0D~2,'0D~2,'0D~2,'0D" y mon d h min))))
    (setf log-file-name (format nil "~A~A.log" log-base-name dt))
    (with-open-file (file log-file-name :direction :output :if-exists :supersede :if-does-not-exist :create :external-format '(:latin-1 :eol-style #+win32 :crlf #-win32 :lf))
      (format file "SANLAB~CSTART-LOGGING" #\tab))
))

(defun log-event (str)
  (format t "Logged event~%")
  (with-open-file (file log-file-name :direction :output :if-exists :append :if-does-not-exist :create :external-format '(:latin-1 :eol-style #+win32 :crlf #-win32 :lf))
    (ignore-errors (format file str))))

(defun log-push-button-pushed (callbacks)
  (log-event (format nil "PUSHED~C~S~%" #\tab (capi:item-text callbacks))))

(defun log-push-button-panel-selection (callbacks args)
  (log-event (format nil "PUSHED-PANEL-BUTTON~C\"~A\"~%" #\tab (if args (first args) nil))))

(defun log-check-button-checked (callbacks)
  (log-event (format nil "CHECKED~C~S~%" #\tab (capi:item-text callbacks))))

(defun log-check-button-panel-selection (callbacks args)
  (log-event (format nil "CHECKED-PANEL-BUTTON~C\"~A\"~%" #\tab (if args (first args) nil))))

(defun log-check-button-unchecked (callbacks)
  (log-event (format nil "UNCHECKED~C~S~%" #\tab (capi:item-text callbacks))))

)

;(in-package :capi)
;(use-package 'capi)

(let ((*handle-warn-on-redefinition* nil)
      (functions (make-hash-table :test #'equal)))

(defun is-function (x)
  (or (functionp x) (ignore-errors (symbol-function x))))

(defmethod (setf capi:callbacks-selection-callback) :after (value (callbacks capi:callbacks))
  (if (and value (is-function value))
      (if (not (gethash value functions))
          (let ((x #'(lambda (&rest args)
                       (let ((ct (type-of callbacks)))
;                         (format t "(type-of callbacks) ==> ~A~%" ct)
;                         (format t "(type-of ct) ==> ~A~%" (type-of ct))
;                         (format t "(equal ct 'PUSH-BUTTON) ==> ~A~%" (equal ct 'CAPI:PUSH-BUTTON))
                         (cond ((equal ct 'CAPI:PUSH-BUTTON) (log-push-button-pushed callbacks))
                               ((equal ct 'CAPI:CHECK-BUTTON) (log-check-button-checked callbacks))
                               ((equal ct 'CAPI:PUSH-BUTTON-PANEL) (log-push-button-panel-selection callbacks args))
                               ((equal ct 'CAPI:CHECK-BUTTON-PANEL) (log-check-button-panel-selection callbacks args))
                               )
                         (apply value args)))))
            (setf (gethash x functions) x)
            (setf (capi:callbacks-selection-callback callbacks) x))))
)

(defmethod (setf capi:callbacks-retract-callback) :after (value (callbacks capi:callbacks))
  (if (and value (is-function value))
      (if (not (gethash value functions))
          (let ((x #'(lambda (&rest args)
                       (let ((ct (type-of callbacks)))
                         (cond ((equal ct 'CAPI:CHECK-BUTTON)
                                (log-check-button-unchecked callbacks)))
                         (apply value args)))))
            (setf (gethash x functions) x)
            (setf (capi:callbacks-selection-callback callbacks) x))))
)

(defmethod initialize-instance :after ((INSTANCE capi:callbacks) &REST INITARGS &KEY &ALLOW-OTHER-KEYS)
  (setf (capi:callbacks-selection-callback instance) (capi:callbacks-selection-callback instance))
  (setf (capi:callbacks-retract-callback instance) (capi:callbacks-retract-callback instance))
)

(defun get-hash-table ()
  functions)
)

(in-package :cl-user)

(defun push-button-callback (data intf)
  (format t "Got a button push~%"))

(defun check-button-callback (data intf)
  (format t "Got a check marked~%"))

(capi:define-interface test-interface ()
  ()
  (:panes
   (test-push-buttons
    capi:push-button-panel
    :title "Press a button:"
    :items '(a b c)
    :selection-callback 'push-button-callback)
   (test-check-buttons
    capi:check-button-panel
    :title "Check options:"
    :items '(x y z)
    :selection-callback 'check-button-callback))
  (:layouts
   (main-layout
    capi:column-layout
    '(test-push-buttons test-check-buttons))))

;(setf x (make-instance 'test-interface))
;(capi:display x)