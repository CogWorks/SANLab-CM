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

;;; uncomment to allow falling into Lisp debugger on errors
; (defparameter *enable-sanlab-debugger* nil)

(defun initialize ()
  (declare (special *enable-sanlab-debugger*))
  (cond ((or (not (boundp '*enable-sanlab-debugger*)) *enable-sanlab-debugger*)
         (ensure-directories-exist (get-debug-path))
         (setf *debug-io* (open *debug-path* :direction :output :if-exists :supersede :external-format '(:latin-1 :eol-style #+win32 :crlf #-win32 :lf)))
         (setf *debugger-hook* 'sanlab-debug)))
  (setf *invisible-output-pane* (let ((l (make-instance 'capi:pinboard-layout)))
                                        (capi:display (make-instance 'capi:interface
                                                                     :display-state :hidden
                                                                     :layout l))
                                        l))
  (setf *capi-screen* (capi:convert-to-screen))
  (let ((image-name (lw:lisp-image-name)))
    (if (not (or (contains image-name "LispWorks.app") (contains image-name "LispWorks Personal.app")))
      (setf *app-path* (get-dir (lw:lisp-image-name)) *app-type* #+win32 'windows #+cocoa 'mac)
      (setf *app-type* 'lispworks)))
  (setf *PROGRAM-DIRECTORY* (if (contains *app-path* "SANLab-CM.app") (string-append *app-path* "Contents/") *app-path*))
  (ensure-directories-exist *debug-path*)
  (setf (app-property 'activity-types) nil)
  (setf (app-property 'model-width) 4000)
  (setf (app-property 'model-height) 1000)
  (setf (app-property 'grid-size) 10)
  (setf (app-property 'golden-ratio) (/ (+ 1 (sqrt 5)) 2))
  (setf (app-property 'initial-interface-height) 800)
  (setf (app-property 'initial-interface-width) 600)
  (setf (app-property 'min-pane-width) 300)
  (setf (app-property 'min-pane-height) 388)
  (setf (app-property 'min-obj-width) 110)
  (setf (app-property 'min-obj-height) 60)
  (load-images)
  (with-open-file (revfile (string-append *PROGRAM-DIRECTORY* "CURRENT_REVISION") :direction :input :external-format '(:latin-1 :eol-style #+win32 :crlf #-win32 :lf))
    (setf (app-property 'app-revision) (read revfile)))
  (load-properties (string-append *PROGRAM-DIRECTORY* (if (equal *app-type* 'mac) "../../properties.conf" "properties.conf")))
  (read-activities (directory (string-append *PROGRAM-DIRECTORY* (if (equal *app-type* 'mac) "../../Activities/*.sat" "Activities/*.sat"))))
  (read-distributions (directory (string-append *PROGRAM-DIRECTORY* (if (equal *app-type* 'mac) "../../Distributions/*.spd" "Distributions/*.spd"))))
  (read-interactive-routines (directory (string-append *PROGRAM-DIRECTORY* (if (equal *app-type* 'mac) "../../Interactive Routines/*.sir" "Interactive Routines/*.sir"))))
)

(defun load-images ()
  (setf (app-property 'image-SANLab) (gp:read-external-image (string-append *PROGRAM-DIRECTORY* "Resources/SANLab.bmp")))
  (setf (app-property 'image-splash) (gp:read-external-image (string-append *PROGRAM-DIRECTORY* "Resources/splash.bmp")))
  (setf (app-property 'image-arrow) (gp:read-external-image (string-append *PROGRAM-DIRECTORY* "Resources/Arrow-32x32.bmp") :transparent-color-index 43))
  (setf (app-property 'image-arrow-selected) (gp:read-external-image (string-append *PROGRAM-DIRECTORY* "Resources/Arrow-32x32-selected.bmp")))
  (setf (app-property 'image-crosshair) (gp:read-external-image (string-append *PROGRAM-DIRECTORY* "Resources/connect.bmp") :transparent-color-index 1))
  (setf (app-property 'image-crosshair-selected) (gp:read-external-image (string-append *PROGRAM-DIRECTORY* "Resources/connect-selected.bmp")))
  (setf (app-property 'image-text) (gp:read-external-image (string-append *PROGRAM-DIRECTORY* "Resources/Text-32x32.bmp") :transparent-color-index 50))
  (setf (app-property 'image-text-selected) (gp:read-external-image (string-append *PROGRAM-DIRECTORY* "Resources/Text-32x32-selected.bmp")))
)

(defun sanlab ()
  (let ((app 
         #+cocoa
         (make-instance 'application-win)
         #-cocoa
         nil
))
    #+cocoa
    (capi:set-application-interface app)
    (initialize)
    (splash 2)
    #+cocoa
    (if (not (message-received app))
        (progn
        (setf (app-property 'current-controller) (make-instance 'controller :model (make-instance 'model)))
        (display-view-controller (create-model-editor (app-property 'current-controller)))))
    #-cocoa
    (progn
      (setf (app-property 'current-controller)
            (make-instance 'controller :model (make-instance 'model)))
      (display-view-controller (create-model-editor (app-property 'current-controller))))
    ))
