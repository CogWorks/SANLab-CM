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

(capi:define-interface splash-win ()
  ((cogworks-logo :initform nil
                  :accessor cogworks-logo))
  (:panes
   (cogworks-pane capi:output-pane
                  :accessor cogworks-pane
                  :display-callback #'(lambda (pane x y width height)
                                        (declare (ignore x y width height))
                                        (if (not (cogworks-logo (capi:element-interface pane)))
                                            (setf (cogworks-logo (capi:element-interface pane))
                                                  (gp:convert-external-image pane (app-property 'image-splash))))
                                        (gp:draw-image pane (cogworks-logo (capi:element-interface pane)) 0 0))
                  :visible-min-width 500
                  :visible-max-width 322
                  :visible-min-height 322))
  (:layouts
   (main capi:simple-layout '(cogworks-pane)))
  (:default-initargs
   :x (- (/ (capi:screen-width (capi:convert-to-screen)) 2)
         (/ 500 2))
   :y (- (/ (capi:screen-height (capi:convert-to-screen)) 2)
         (/ 322 2))
   :window-styles '(:always-on-top :borderless :internal-borderless))
  )

(defun splash (splash-time)
  (let ((interface (make-instance 'splash-win)))
    (capi:display interface)
    (sleep splash-time)
    (capi:execute-with-interface interface 'capi:destroy interface)))
