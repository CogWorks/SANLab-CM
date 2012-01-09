#|
Copyright Â© 2007-2009 Evan W. Patton

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


; Displays the SANLab-CM About dialog
(defun about-sanlab (interface)
  "Displays the SANLab-CM About dialog"
  (declare (ignore interface))
  (capi:display (make-instance 'about-win))
  )

; The about window
(capi:define-interface about-win ()
  ((image :initform nil :accessor image))
  (:panes
   (image-pane
    capi:output-pane
    :display-callback #'(lambda (self x y width height)
                          (declare (ignore x y width height))
                          (cond
                           ((not (image (capi:element-interface self)))
                            (setf (image (capi:element-interface self))
                                  (gp:convert-external-image self (app-property 'image-SANLab)))))
                          (gp:draw-image self (image (capi:element-interface self)) 0 0))
    :visible-min-width 128
    :visible-max-width 128
    :visible-min-height 128
    :visible-max-height 128
    )
   (row-1
    capi:title-pane
    :text (format nil "SANLab-CM v~a" (app-property 'app-version)))
   (row-1a
    capi:title-pane
    :text (format nil "r~A" (app-property 'app-revision)))
   (row-2
    capi:title-pane
    :text "The Stochastic Activity Network Modeling Laboratory for Cognitive Modeling")
   (row-blank
    capi:title-pane
    :text "")
   (row-blank-2
    capi:title-pane
    :text "")
   (row-3
    capi:title-pane
    :text "Copyright © 2009 Evan W. Patton")
   (row-4
    capi:title-pane
    :text "Licensed under the GNU Lesser General Public License")
   (row-5
    capi:title-pane
    :text "See COPYING and COPYING.LESSER for details")
   (row-blank-3
    capi:title-pane
    :text "")
   (row-6
    capi:title-pane
    :text "CogWorks Lab, Rensselaer Polytechnic Institute")
   (row-7
    capi:title-pane
    :text "http://www.cogsci.rpi.edu/cogworks/")

   )
  (:layouts
   (main
    capi:column-layout '(image-pane row-1 row-1a row-2 row-blank row-3 row-4 row-5 row-blank-2 row-6 row-7 row-blank-3)
    :adjust :center)
   )
  (:default-initargs
   :title "About SANLab-CM"
   :window-styles '(:never-iconic))
  (:documentation "The SANLab-CM About dialog class")
  )
