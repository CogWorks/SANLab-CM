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

(capi:define-interface application-win (capi:cocoa-default-application-interface)
  ((message-received :initform nil :initarg :message-received :accessor message-received)
   (model :initform nil :initarg :model :accessor model))
  (:menus
   (application-menu
    (app-property 'app-name)
    ((:component
      (((format nil "About ~a..." (app-property 'app-name))
        :callback #'about-sanlab
        :callback-type :interface)))
     (:component
      ()
      :name :application-services)
     (:component
      (((format nil "Hide ~a" (app-property 'app-name))
        :accelerator "accelerator-h"
        :callback-data :hidden)
       ("Hide Others"
        :accelerator "accelerator-meta-h"
        :callback-data :others-hidden)
       ("Show All"
        :callback-data :all-normal))
      :callback #'(setf capi:top-level-interface-display-state)
      :callback-type :data-interface)
     (:component
      (((format nil "Quit ~a" (app-property 'app-name))
        :accelerator "accelerator-q"
        :callback 'confirm-quit
        :callback-type :interface)))))
   (file-menu
    "File"
    ((:component
      (("New Model"
        :accelerator "accelerator-n"
        :callback 'file-new
        :callback-type :interface)
       ("Open..."
        :callback 'file-open
        :callback-type :interface
        :accelerator "accelerator-o")
       ))
     ))
   )
  (:menu-bar application-menu file-menu)
  (:default-initargs
   :title (app-property 'app-name)
   :application-menu 'application-menu
   :message-callback 'message-callback)
  (:documentation "The application window for Mac OS X when the user closes all open application windows. This is not used on Windows.")
  )

; Creates a new SANLab-CM model
(defun file-new (self)
  (declare (ignore self))
  (setf (app-property 'current-controller) (make-instance 'controller :model (make-instance 'model)))
  (display-view-controller (create-model-editor (app-property 'current-controller)))
)

; Opens a new SANLab-CM model
(defun file-open (self)
  (declare (ignore self))
  (let ((path (capi:prompt-for-file "Select model file"
                               :filter "*.san"
                               :filters '("SANLab-CM Model" "*.san" "Legacy SANLab-CM Files" "*.mdl" "All Files" "*.*")
                               :operation :open
                               :pathname (sys:get-folder-path :documents))))
    (cond (path
           (setf (app-property 'current-controller) (make-instance 'controller :model (make-instance 'model)))
           (display-view-controller (create-model-editor (app-property 'current-controller)))
           (open-model (app-property 'current-controller) path))))
)

; Receives messages from Mac OS X when a file is dropped on the dock icon
(defun message-callback (interface message &rest args)
  (setf (message-received interface) t)
  (initialize)
  (cond 
   ((equal message :open-file)
    (cond ((file-directory-p (car args))
           (setf (app-property 'current-controller) (make-instance 'controller :model (make-instance 'model)))
           (display-view-controller (create-model-editor (app-property 'current-controller)))
           (open-model (app-property 'current-controller) (merge-pathnames (format nil "~A/" (car args)))))
          (t
           (setf (app-property 'current-controller) (make-instance 'controller :model (make-instance 'model)))
           (display-view-controller (create-model-editor (app-property 'current-controller)))
           (open-model (app-property 'current-controller) (merge-pathnames (car args)))
           ))
    )
   (t
    (setf (app-property 'current-controller) (make-instance 'controller :model (make-instance 'model)))
    (display-view-controller (create-model-editor (app-property 'current-controller)))))
)

; Confirms that the use wants to quit the application, prompting to save any modified files.
(defun confirm-quit (interface)
  (declare (ignore interface))
  (let ((all-interfaces (capi:collect-interfaces 'editor)))
    (dolist (interface all-interfaces)
      (if (equalp nil (model-changed interface))
          (multiple-value-bind (result success)
              (capi:prompt-for-confirmation (format nil "Save changes to ~a before quitting?"
                                                    (if (not (equalp nil (pathname (controller interface))))
                                                        (get-bundle-name (pathname (controller interface)))
                                                      "Untitled"))
                                            :cancel-button t)
            (cond
             ((not success) ;; Cancel
              (return-from confirm-quit))
             (result
              (save-model-action (controller (view-controller interface))))))))
    (quit)))
