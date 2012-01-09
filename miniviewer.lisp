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

(capi:define-interface miniviewer (interface-view)
  ((move-ok :initform nil :accessor move-ok)
   (background :initform nil :accessor miniview-image)
   (viewport :initform nil :accessor viewport)
   (show-criticality :initform nil :accessor show-criticality?)
   (last-ct :initform nil :accessor last-ct))
  (:layouts
   (mini-pane
    capi:pinboard-layout '()
    :background :white
    :cursor :top-left-arrow
    :accessor mini-pane
    :input-model '(((:button-1 :press) mini-pane-mouse-down)
                   ((:button-1 :motion) mini-pane-drag)
                   ((:button-1 :release) mini-pane-release)
                   ((:button-1 :second-press) mini-pane-double-click))))
  (:default-initargs
   :title "Model Overview"
   :window-styles '(:toolbox :internal-borderless)
   :x 0
   :y 0
   :visible-max-height t
   :visible-max-width t))

(defclass mini-viewport (capi:drawn-pinboard-object)
  ((offset-x :initform nil :accessor offset-x)
   (offset-y :initform nil :accessor offset-y)))

(defun obj-center (obj)
  (capi:with-geometry obj
    (let ((x (floor (* (app-property 'miniview-scale) capi:%x%)))
          (y (floor (* (app-property 'miniview-scale) capi:%y%)))
          (w (1- (floor (* (app-property 'miniview-scale) capi:%width%))))
          (h (1- (floor (* (app-property 'miniview-scale) capi:%height%)))))
      (values (floor (+ x (/ w 2)))
              (floor (+ y (/ h 2)))))))

(defun rect-center (x y w h)
  (values (floor (* (app-property 'miniview-scale) (+ x (/ w 2)))) (* (app-property 'miniview-scale) (+ y (/ h 2)))))

(defun draw-miniview (pane self x y width height)
  (declare (ignore self x y))
  (with-app-properties ((scale miniview-scale)
                        (def-width editor-default-activity-width)
                        (def-height editor-default-activity-height)
                        (highlight editor-highlight-color)
                        (cp-color editor-cp-color))
    (let* ((interface (capi:element-interface pane))
           (activities (model-activities (controller interface))))
      (gp:clear-graphics-port pane)
      (cond ((show-criticality? interface)
             (gp:draw-rectangle pane
                               0 0 width height
                               :foreground (color:make-rgb 0.2 0.2 0.2)
                               :filled t)
             (dolist (act activities)
               (dolist (act2 (dependent-tasks act))
                 (multiple-value-bind (x1 y1) (rect-center (x-position act) (y-position act) def-width def-height)
                   (multiple-value-bind (x2 y2) (rect-center (x-position act2) (y-position act2) def-width def-height)
                     (gp:draw-line pane
                                   x1 y1 x2 y2
                                   :foreground (color:make-rgb 0.0 0.0 0.0)
                                   :thickness 1)))))
             (dolist (act activities)
               (gp:draw-rectangle pane
                                  (floor (* scale (x-position act)))
                                  (floor (* scale (y-position act)))
                                  (1- (floor (* scale def-width)))
                                  (1- (floor (* scale def-height)))
                                  :foreground (let ((val (criticality act (last-ct interface))))
                                                (color:make-rgb val val val))
                                  :filled t))
             )
            (t
             (dolist (act activities)
               (dolist (act2 (dependent-tasks act))
                 (multiple-value-bind (x1 y1) (rect-center (x-position act) (y-position act) def-width def-height)
                   (multiple-value-bind (x2 y2) (rect-center (x-position act2) (y-position act2) def-width def-height)
                     (gp:draw-line pane
                                   x1 y1 x2 y2
                                   :foreground (color:make-rgb 0.0 0.0 0.0)
                                   :thickness 1)))))
             (dolist (act activities)
               (gp:draw-rectangle pane
                                  (floor (* scale (x-position act)))
                                  (floor (* scale (y-position act)))
                                  (1- (floor (* scale def-width)))
                                  (1- (floor (* scale def-height)))
                                  :foreground (color act)
                                  :filled t)
               (gp:draw-rectangle pane
                                  (floor (* scale (x-position act)))
                                  (floor (* scale (y-position act)))
                                  (1- (floor (* scale def-width)))
                                  (1- (floor (* scale def-height)))
                                  :foreground (cond ((selected? act) highlight)
                                                    ((on-critical-path? act) cp-color)
                                                    ((ir-type act) (color (ir-type act)))
                                                    (t (color:make-rgb 0.0 0.0 0.0)))
                                  :thickness 1)))))))

#|
(defun draw-miniviewport (pane self x y width height)
  (let ((x (color:make-rgb 0.0S0 0.0S0 0.0S0 0.3S0)))
    ;(inspect x)
    (capi:with-geometry self
      (gp:draw-rectangle
       pane capi:%x% capi:%y% (1- capi:%width%) (1- capi:%height%)
       :foreground x
       :filled t))))
|#

(defun draw-miniviewport (pane self x y width height)
  (declare (ignore x y width height))
  (let ((the-image nil))
    (capi:with-geometry self
      (gp:with-pixmap-graphics-port (port pane
                                          capi:%width%
                                          capi:%height%
                                          :clear t
                                          :background (color:make-rgb 1.0 1.0 1.0 0.0))
        (gp:draw-rectangle port
                           0 0 capi:%width% capi:%height%
                           :foreground #(:RGB 0.0 0.0 0.0)
                           :filled t)
        (setq the-image (gp:make-image-from-port port))
        (gp:draw-image pane
                       the-image
                       capi:%x% capi:%y%
                       :global-alpha 0.3)
        (gp:free-image port the-image))))
)

(defun mini-pane-mouse-down (pane x y)
  (let ((interface (capi:element-interface pane))
        (obj (capi:pinboard-object-at-position pane x y)))
    (setf (move-ok interface) nil)
    (cond ((equal (type-of obj) 'mini-viewport)
           (setf (move-ok interface) t)
           (capi:with-geometry obj
             (setf (offset-x obj) (- capi:%x% x))
             (setf (offset-y obj) (- capi:%y% y)))))))

(defun mini-pane-release (pane x y)
  (declare (ignore x y))
  (let ((interface (capi:element-interface pane)))
    (if (not (move-ok interface)) (return-from mini-pane-release))
    (let ((obj (viewport interface)))
      (setf (move-ok interface) nil)
      (capi:with-geometry obj
        (scroll (viewhash 'sanlab-model-editor (view-controller interface)) :pan :move (mapcar #'(lambda (x) (/ x (app-property 'miniview-scale))) (list capi:%x% capi:%y%)))))))

(defun mini-pane-drag (pane x y)
  (let ((interface (capi:element-interface pane)))
    (if (not (move-ok interface)) (return-from mini-pane-drag))
    (let* ((obj (viewport interface))
           (pos-x (+ (offset-x obj) x))
           (pos-y (+ (offset-y obj) y))
           (max-x 0)
           (max-y 0)
           (x 0)
           (y 0))
      (capi:with-geometry pane
        (setq max-x (1+ capi:%width%) max-y (1+ capi:%height%)))
      (capi:with-geometry obj
        (setq max-x (- max-x capi:%width%) max-y (- max-y capi:%height%))
        (setq x (cond ((> 0 pos-x) 0)
                      ((> pos-x max-x) max-x)
                      (t pos-x)))
        (setq y (cond ((> 0 pos-y) y)
                      ((> pos-y max-y) max-y)
                      (t pos-y)))
        (setf (capi:pinboard-pane-position obj) (values x y))
        (scroll (viewhash 'sanlab-model-editor (view-controller interface)) :pan :move (mapcar #'(lambda (x) (/ x (app-property 'miniview-scale))) (list x y)))))))

(defun mini-pane-double-click (pane x y)
  (let* ((interface (capi:element-interface pane))
         (obj (viewport interface))
         (pos-x 0)
         (pos-y 0)
         (max-x 0)
         (max-y 0)
         (final-x 0)
         (final-y 0))
    (capi:with-geometry pane
      (setq max-x (1+ capi:%width%) max-y (1+ capi:%height%)))
    (capi:with-geometry obj
      (setq pos-x (- x (/ capi:%width% 2)) pos-y (- y (/ capi:%height% 2)))
      (setq max-x (- max-x capi:%width%) max-y (- max-y capi:%height%))
      (setq final-x (cond ((> 0 pos-x) 0)
                          ((> pos-x max-x) max-x)
                          (t pos-x)))
      (setq final-y (cond ((> 0 pos-y) 0)
                          ((> pos-y max-y) max-y)
                          (t pos-y)))
      (setf (capi:pinboard-pane-position obj) (values final-x final-y))
      (scroll (viewhash 'sanlab-model-editor (view-controller interface)) :pan :move (mapcar #'(lambda (x) (/ x (app-property 'miniview-scale))) (list final-x final-y))))))

(defmethod initialize-instance :after ((obj miniviewer) &rest initargs)
  (let* ((pinboard (main-layout (viewhash 'sanlab-model-editor (view-controller obj))))
         (my-pane (mini-pane obj))
         (width (1+ (ceiling (* (app-property 'miniview-scale) (width (controller (view-controller obj)))))))
         (height (1+ (ceiling (* (app-property 'miniview-scale) (height (controller (view-controller obj))))))))
    (setf (miniview-image obj) (make-instance 'capi:drawn-pinboard-object
                                        :x 0
                                        :y 0
                                        :width width
                                        :height height
                                        :display-callback #'draw-miniview))
    (capi:manipulate-pinboard my-pane (miniview-image obj) :add-bottom)
    (capi:with-geometry pinboard
      (setf (viewport obj) (make-instance 'mini-viewport
                                          :x 0
                                          :y 0
                                          :width (ceiling (* (app-property 'miniview-scale) capi:%width%))
                                          :height (ceiling (* (app-property 'miniview-scale) capi:%height%))
                                          :display-callback #'draw-miniviewport)))
    (capi:manipulate-pinboard my-pane (viewport obj) :add-top)
    (register-callbacks obj (controller (view-controller obj)))
))

(defmethod make-listener ((obj miniviewer) (callback function))
  #'(lambda (&rest args)
      (if args
          (apply #'capi:apply-in-pane-process obj callback obj args)
        (capi:apply-in-pane-process obj callback obj))))

(defmethod register-callbacks ((obj miniviewer) (con controller))
  (add-event-listener 'new-activity (make-instance 'listener :callback (make-listener obj #'miniviewer-new-activity-listener)) con)
  (add-event-listener 'deleted-activity (make-instance 'listener :callback (make-listener obj #'miniviewer-deleted-activity-listener)) con)
  (add-event-listener 'connected-activities (make-instance 'listener :callback (make-listener obj #'miniviewer-connected-activities-listener)) con)
  (add-event-listener 'disconnected-activities (make-instance 'listener :callback (make-listener obj #'miniviewer-disconnected-activities-listener)) con)
  (add-event-listener 'moved-activity (make-instance 'listener :callback (make-listener obj #'miniviewer-moved-activity-listener)) con)
  (add-event-listener 'activity-on-critical-path (make-instance 'listener :callback (make-listener obj #'miniviewer-critical-path-listener)) con)

  (add-event-listener 'editor-view-scrolled (make-instance 'listener :callback (make-listener obj #'miniviewer-move-viewport-listener)) con)

  (add-event-listener 'editor-criticality-changed (make-instance 'listener :callback (make-listener obj #'miniviewer-allow-criticality-listener)) con)

  (add-event-listener 'laid-out-model (make-instance 'listener :callback (make-listener obj #'miniviewer-laid-out-model-listener)) con)

  (add-event-listener 'model-width-change (make-instance 'listener :callback (make-listener obj #'miniviewer-model-dimensions-changed-listener)) con)
  (add-event-listener 'model-height-change (make-instance 'listener :callback (make-listener obj #'miniviewer-model-dimensions-changed-listener)) con)

  (add-event-listener 'new-model (make-instance 'listener :callback (make-listener obj #'miniviewer-new-model-listener)) con)
  (add-event-listener 'open-model (make-instance 'listener :callback (make-listener obj #'miniviewer-open-model-listener)) con)
  (add-event-listener 'import-macproject-model (make-instance 'listener :callback (make-listener obj #'miniviewer-import-macproject-model)) con)
  (add-event-listener 'import-actr-trace (make-instance 'listener :callback (make-listener obj #'miniviewer-import-macproject-model)) con)
  (add-event-listener 'import-from-cogtool (make-instance 'listener :callback (make-listener obj #'miniviewer-import-macproject-model)) con)
)

(defmethod miniviewer-allow-criticality-listener ((self miniviewer) (event symbol) editor)
  (setf (show-criticality? self) (criticality-overlay-enabled? editor))
  (setf (last-ct self) (last-ct-tracker editor))
  (capi:redraw-pinboard-object (miniview-image self))
)

(defmethod miniviewer-model-dimensions-changed-listener ((self miniviewer) (event symbol) value)
  (resize-to self (width (controller self)) (height (controller self)))
  (let ((width (1+ (ceiling (* (app-property 'miniview-scale) (width (controller self))))))
        (height (1+ (ceiling (* (app-property 'miniview-scale) (height (controller self)))))))
    (capi:apply-in-pane-process self #'(lambda ()
                                         (setf (capi:pinboard-pane-size (miniview-image self)) (values width height)))))
  (gp:invalidate-rectangle (miniview-image self))
)

(defmethod miniviewer-laid-out-model-listener ((self miniviewer) (event symbol))
  (resize-to self (width (controller self)) (height (controller self)))
  (let ((width (1+ (ceiling (* (app-property 'miniview-scale) (width (controller self))))))
        (height (1+ (ceiling (* (app-property 'miniview-scale) (height (controller self)))))))
    (capi:apply-in-pane-process self #'(lambda ()
                                         (setf (capi:pinboard-pane-size (miniview-image self)) (values width height)))))
  (gp:invalidate-rectangle (miniview-image self)))

(defmethod miniviewer-critical-path-listener ((self miniviewer) (event symbol) (node instance-pointer) val)
  (let ((x (miniview-image self)))
    (gp:invalidate-rectangle x)))

(defmethod miniviewer-new-activity-listener ((self miniviewer) (event symbol) (node instance-pointer))
  (declare (ignore node))
  (gp:invalidate-rectangle (miniview-image self)))

(defmethod miniviewer-deleted-activity-listener ((self miniviewer) (event symbol) (node instance-pointer))
  (gp:invalidate-rectangle (miniview-image self)))

(defmethod miniviewer-connected-activities-listener ((self miniviewer) (event symbol) (act1 instance-pointer) (act2 instance-pointer))
  (declare (ignore act1 act2))
  (gp:invalidate-rectangle (miniview-image self)))

(defmethod miniviewer-disconnected-activities-listener ((self miniviewer) (event symbol) (act1 instance-pointer) (act2 instance-pointer))
  (gp:invalidate-rectangle (miniview-image self)))

(defmethod miniviewer-new-model-listener ((self miniviewer) (event symbol))
  (declare (ignore event other))
  (resize-to self (width (controller (view-controller self))) (height (controller (view-controller self))))
  (let ((width (1+ (ceiling (* (app-property 'miniview-scale) (width (controller self))))))
        (height (1+ (ceiling (* (app-property 'miniview-scale) (height (controller self)))))))
    (capi:apply-in-pane-process self #'(lambda ()
                                         (setf (capi:pinboard-pane-size (miniview-image self)) (values width height)))))
  (gp:invalidate-rectangle (miniview-image self)))

(defmethod miniviewer-open-model-listener ((self miniviewer) (event symbol))
  (resize-to self (width (controller (view-controller self))) (height (controller (view-controller self))))
  (let ((width (1+ (ceiling (* (app-property 'miniview-scale) (width (controller self))))))
        (height (1+ (ceiling (* (app-property 'miniview-scale) (height (controller self)))))))
    (capi:apply-in-pane-process self #'(lambda ()
                                         (setf (capi:pinboard-pane-size (miniview-image self)) (values width height)))))
  (gp:invalidate-rectangle (miniview-image self)))

(defmethod miniviewer-import-macproject-model ((self miniviewer) (event symbol))
  (resize-to self (width (controller (view-controller self))) (height (controller (view-controller self))))
  (let ((width (1+ (ceiling (* (app-property 'miniview-scale) (width (controller self))))))
        (height (1+ (ceiling (* (app-property 'miniview-scale) (height (controller self)))))))
    (capi:apply-in-pane-process self #'(lambda ()
                                         (setf (capi:pinboard-pane-size (miniview-image self)) (values width height)))))
  (gp:invalidate-rectangle (miniview-image self)))

(defmethod miniviewer-moved-activity-listener ((self miniviewer) (event symbol) (act instance-pointer) x y)
  (gp:invalidate-rectangle (miniview-image self)))

(defmethod resize-to ((self miniviewer) (width number) (height number))
  (let ((width (1+ (ceiling (* (app-property 'miniview-scale) width))))
        (height (1+ (ceiling (* (app-property 'miniview-scale) height)))))
    (setf (capi:pinboard-pane-size (miniview-image self)) (values width height))
    (capi:apply-in-pane-process self #'capi:set-geometric-hint (miniview-image self) :visible-min-width width t)
    (capi:apply-in-pane-process self #'capi:set-geometric-hint (miniview-image self) :visible-min-height height t)
))

(defmethod miniviewer-move-viewport-listener ((self miniviewer) (event symbol) x y)
  (setf (capi:pinboard-pane-position (viewport self)) (values (* x (app-property 'miniview-scale))
                                                              (* y (app-property 'miniview-scale))))
)

(pass controller miniviewer view-controller)
