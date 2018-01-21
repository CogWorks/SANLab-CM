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

(defun get-iroutine-by-name (str)
  (if (and str (not (equal str "")) (equal #\" (elt str 0)))
      (setf str (read-from-string str)))
  (dolist (ir (app-property 'interactive-routines))
    (if (equal str (name ir))
        (return-from get-iroutine-by-name ir)))
  nil)

(defun get-ir-task-by-id (ir id)
  (if (or (null ir) (null id)) (return-from get-ir-task-by-id nil))
  (dolist (task (task-list ir))
    (if (= (uid task) id)
        (return-from get-ir-task-by-id task)))
  nil)

(defmacro defroutine (name color &body body)
  `(let ((tasks nil))
     (setq tasks (let ((ir-tasks nil))
                   ,@body
                   ir-tasks))
     (let ((ir (make-instance 'interactive-routine
                              :name ,name
                              :highlight-color ,color
                              :task-list tasks)))
       (dolist (task tasks)
         (setf (edges-in task) (mapcar #'(lambda (x) (find x (task-list ir) :key #'uid)) (edges-in task)))
         (setf (edges-out task) (mapcar #'(lambda (x) (find x (task-list ir) :key #'uid)) (edges-out task))))
       ir)))

(defmacro deftask (uid label op-type dist parameters x y sources targets &key (measurable nil))
  `(push (make-instance 'ir-task
                        :uid ,uid
                        :operator-type (get-activity-by-typename ,op-type)
                        :offset-x ,x :offset-y ,y
                        :edges-in ,sources :edges-out ,targets
                        :label ,label
                        :distribution ,dist
                        :parameters ,parameters
                        :measurable ,measurable)
        ir-tasks))

(defmethod get-ir-boundaries ((ir interactive-routine-pointer))
  (let* ((tasks (task-list ir))
         (max-width (apply #'max (mapcar #'(lambda (task) (+ (app-property 'editor-default-activity-width) (offset-x task))) tasks)))
         (max-height (apply #'max (mapcar #'(lambda (task) (+ (app-property 'editor-default-activity-height) (offset-y task))) tasks))))
    (values max-width max-height)))

(defclass interactive-routine-placer (capi:drawn-pinboard-object)
  ((ir-to-place :initform nil :initarg :ir-to-place :accessor ir-to-place)
   (append-string :initform nil :initarg :append-string :accessor append-string))
  (:default-initargs
   :display-callback #'ir-placer-draw-function)
)

(defmethod initialize-instance :after ((irp interactive-routine-placer) &rest initargs)
;  (setf (capi:drawn-pinboard-object-display-callback irp) #'ir-placer-draw-function)
)

(defun ir-placer-draw-function (pane self x y width height)
  (declare (ignore x y width height))
  (let* ((tasks (task-list (ir-to-place self)))
         (thecolor (highlight-color (ir-to-place self)))
         (the-image nil))
    (capi:with-geometry self
      (multiple-value-bind (width height) (get-ir-boundaries (ir-to-place self))
        (gp:with-pixmap-graphics-port (port pane
                                            width height
                                            :clear t
                                            :background (color:make-rgb 1.0 1.0 1.0 0.0))
          (dolist (source tasks)
            (dolist (target (edges-out source))
              (let ((x1 (+ (offset-x source) (/ (app-property 'editor-default-activity-width) 2)))
                    (y1 (+ (offset-y source) (/ (app-property 'editor-default-activity-height) 2)))
                    (x2 (+ (offset-x target) (/ (app-property 'editor-default-activity-width) 2)))
                    (y2 (+ (offset-y target) (/ (app-property 'editor-default-activity-height) 2))))
                (gp:draw-line port
                              x1 y1 x2 y2
                              :foreground thecolor
                              :thickness 4))))
          (dolist (task tasks)
            (gp:draw-rectangle port
                               (offset-x task) (offset-y task)
                               (app-property 'editor-default-activity-width) (app-property 'editor-default-activity-height)
                               :foreground thecolor
                               :filled t))
          (setq the-image (gp:make-image-from-port port))
          (gp:draw-image pane
            the-image
            capi:%x% capi:%y%
            :global-alpha 0.4)
          (gp:free-image port the-image))))))

(defclass ir-list-item (capi:drawn-pinboard-object)
  ((ir-type :initform nil :initarg :ir-type :accessor ir-type)
))

(defun update-ir-list-item-sizes (pane width)
  (dolist (x (capi:layout-description pane))
    (if (equal (type-of x) 'ir-list-item)
        (progn
          (capi:set-hint-table x (list :visible-min-width width))
          (setf (capi:pinboard-pane-size x) (values width 80))))))

(defun ir-list-item-draw-function (pane self x y width height)
  (declare (ignore x y width height))
  (capi:with-geometry self
    (multiple-value-bind (left top right bottom) (gp:get-string-extent pane (name (ir-type self)))
      (declare (ignore top bottom))
      (if (or (equal capi:%width% :dont-know) (< capi:%width% (+ 88 (- right left))))
          (progn
            (capi:set-hint-table self (list :visible-min-width (+ 88 (- right left))))
            (update-ir-list-item-sizes pane (+ 88 (- right left)))))))
  (cond ((equal (ir-type self) (ir-list-layout-selected-item (capi:pinboard-object-pinboard self)))
         (capi:with-geometry self
           ;(inspect pane)
           (gp:draw-rectangle pane
             capi:%x% capi:%y% capi:%width% capi:%height%
             :foreground (color:make-rgb 0.337254 0.549019 0.862745)
             :filled t)
           (gp:draw-rectangle pane
             (+ 8 capi:%x%) (+ 8 capi:%y%) 64 64
             :foreground (color:make-rgb 1.0 1.0 1.0)
             :filled t)))
        (t
         (capi:with-geometry self
           (gp:draw-rectangle pane
             capi:%x% capi:%y% capi:%width% capi:%height%
             :foreground (color:make-rgb 1.0 1.0 1.0)
             :filled t))))
  (capi:with-geometry self
    (let* ((preview (ir-scaled-image (ir-type self)))
           (width (gp:image-width preview))
           (height (gp:image-height preview)))
      (gp:draw-image
       pane
       preview
       (floor (+ capi:%x% (- 40 (/ width 2)))) (floor (+ capi:%y% (- 40 (/ height 2)))))
      (gp:free-image *invisible-output-pane* preview))
    (multiple-value-bind (left top right bottom) (gp:get-string-extent pane (name (ir-type self)))
      (declare (ignore left right))
      (gp:draw-string pane (name (ir-type self)) (+ 80 capi:%x%) (floor (+ 36 (/ (- bottom top) 2) capi:%y%))))
))

(defun ir-preview-image (ir)
  (if (equal (type-of ir) 'ir-list-layout) (setq ir (ir-list-layout-selected-item ir)))
  (if (not (equal (type-of ir) 'interactive-routine-pointer)) (return-from ir-preview-image))
  (multiple-value-bind (width height) (get-ir-boundaries ir)
    (let* ((scale (min (/ 64 width) (/ 64 height)))
           (half-width (floor (/ (app-property 'editor-default-activity-width) 2)))
           (half-height (floor (/ (app-property 'editor-default-activity-height) 2))))
  (gp:with-pixmap-graphics-port
      (port *invisible-output-pane*
            64 64
            :clear t
            :background nil)
    ;;; Get a preview of the IR
    (let* ((preview (ir-scaled-image ir))
           (width (gp:image-width preview))
           (height (gp:image-height preview)))
      (gp:draw-point
       port
       1 1)
      (gp:externalize-and-write-image port preview "/Users/ewpatton/test.png")
      (ir-scaled-image-internal ir port scale width height half-width half-height)
#|
      (gp:draw-image
       port
       preview
       (floor (- 32 (/ width 2))) (floor (- 32 (/ height 2))))
|#
      (gp:free-image *invisible-output-pane* preview))
    ;;; Make image
    (setq *drag-image* (gp:make-image-from-port port))
))))

(defun ir-scaled-image-internal (ir port scale width height half-width half-height)
  (gp:draw-rectangle port
                     0 0 (round (* scale width)) (round (* scale height))
                     :foreground #(:RGB 1.0 1.0 1.0)
                     :filled t)
  (dolist (source (task-list ir))
    (dolist (target (edges-out source))
      (let ((x1 (+ (offset-x source) half-width))
            (y1 (+ (offset-y source) half-height))
            (x2 (+ (offset-x target) half-width))
            (y2 (+ (offset-y target) half-height)))
        (gp:draw-line port
                      (floor (* scale x1)) (floor (* scale y1))
                      (floor (* scale x2)) (floor (* scale y2))
                      :forecolor #(:RGB 0.0 0.0 0.0)))))
  (dolist (task (task-list ir))
    (gp:draw-rectangle port
                       (floor (* scale (offset-x task))) (floor (* scale (offset-y task)))
                       (1- (floor (* scale (app-property 'editor-default-activity-width)))) (1- (floor (* scale (app-property 'editor-default-activity-height))))
                       :foreground (get-color (operator-type task))
                       :filled t))
)

(defun ir-scaled-image (ir)
  (if (not (equal (type-of ir) 'interactive-routine-pointer)) (error "~a, supplied to ~a, is not of type ~a" ir #'ir-scaled-image 'interactive-routine))
  (multiple-value-bind (width height) (get-ir-boundaries ir)
    (let* ((scale (min (/ 64 width) (/ 64 height)))
           (half-width (floor (/ (app-property 'editor-default-activity-width) 2)))
           (half-height (floor (/ (app-property 'editor-default-activity-height) 2))))
      (gp:with-pixmap-graphics-port
          (port *invisible-output-pane* (round (* scale width)) (round (* scale height)) :clear t)
        (ir-scaled-image-internal ir port scale width height half-width half-height)
        (gp:make-image-from-port port)))))

(defclass ir-list-layout (capi:pinboard-layout)
  ((items :initform nil :initarg :items :accessor ir-list-layout-items)
   (sort-function :initform #'string< :initarg :sort-function :accessor ir-list-layout-sort-function)
   (key-function :initform nil :initarg :key-function :accessor ir-list-layout-key-function)
   (selected :initform nil :initarg :selected :accessor ir-list-layout-selected)
   (selected-item :initform nil :initarg :selectd-item :accessor ir-list-layout-selected-item)
))

(defun make-list-item (items)
  (let ((count -1))
    (mapcar #'(lambda (x)
                (incf count)
                (make-instance 'ir-list-item
                               :ir-type x
                               :x 0
                               :y (* 80 count)
                               :display-callback 'ir-list-item-draw-function
                               :visible-min-width 200
                               :visible-max-width 200
                               :visible-min-height 80
                               :visible-max-height 80))
            items)))

(defmethod (setf ir-list-layout-items) :after (new-items (layout ir-list-layout))
  (cond ((ir-list-layout-sort-function layout)
         (setf (slot-value layout 'items) (sort new-items (ir-list-layout-sort-function layout) :key (ir-list-layout-key-function layout)))))
  (setq new-items (slot-value layout 'items))
  (cond ((and (ir-list-layout-selected-item layout) (member (ir-list-layout-selected-item layout) new-items))
         (setf (ir-list-layout-selected layout) (position (ir-list-layout-selected-item layout) new-items)))
        (t
         (setf (ir-list-layout-selected-item layout) nil)
         (setf (ir-list-layout-selected layout) nil)))
  (let ((items (make-list-item new-items)))
    (capi:apply-in-pane-process layout #'capi:manipulate-pinboard layout items :add-many :position :top))
  (gp:invalidate-rectangle layout)
)

(defun select-iroutine (pane x y)
  (if (and (not x) (not y))
      (progn
        (setf (ir-list-layout-selected-item pane) nil)
        (setf (ir-list-layout-selected pane) nil)
        (gp:invalidate-rectangle pane)
        (return-from select-iroutine)))
  (setf (capi:button-image (slot-value (capi:element-interface pane) 'arrow-button)) (app-property 'image-arrow))
  (setf (capi:button-image (slot-value (capi:element-interface pane) 'crosshair-button)) (app-property 'image-crosshair))
  (let ((object (capi:pinboard-object-at-position pane x y)))
    (cond ((and object (equal (type-of object) 'ir-list-item))
           (setf (ir-list-layout-selected-item pane) (ir-type object))
           (setf (ir-list-layout-selected pane) (position (ir-type object) (ir-list-layout-items pane) :test #'equal)))
          (t
           (setf (ir-list-layout-selected-item pane) nil)
           (setf (ir-list-layout-selected pane) nil)))
    (gp:invalidate-rectangle pane)
))

(defun drag-iroutine-from (pane x y)
  (let ((object (capi:pinboard-object-at-position pane x y)))
    (if (or (null object) (not (equal (type-of object) 'ir-list-item))) (return-from drag-iroutine-from))
    ;(inspect (ir-type object))
    (capi:drag-pane-object pane (ir-type object) :plist (list :interactive-routine (ir-type object))
                           #+cocoa
                           :image-function
                           #+cocoa
                           #'ir-preview-image
                           :operations '(:copy))
    #+cocoa
    (gp:free-image *invisible-output-pane* *drag-image*)
))

(defun drop-interactive-routine (pane object stage)
  (cond ((equal stage :formats)
         (capi:set-drop-object-supported-formats object '(:interactive-routine :value)))
        ((or (equal stage :enter) (equal stage :drag))
         (setf (capi:drop-object-drop-effect object) :copy))
        ((equal stage :drop)
         (let ((ir (capi:drop-object-get-object object pane :interactive-routine)))
           (capi:raise-interface (primary-view (view-controller (capi:element-interface pane))))
           (multiple-value-bind (width height) (get-ir-boundaries ir)
             (let* ((x (capi:drop-object-pane-x object))
                    (y (capi:drop-object-pane-y object))
                    (ir-placer (make-instance 'interactive-routine-placer
                                              :ir-to-place ir
                                              :append-string (capi:prompt-for-string
                                                              "Enter a string to append to the IR components:"
                                                              :text ""
                                                              :popup-args (list :owner (primary-view (view-controller (capi:element-interface pane)))))
                                              :x (floor (- x (/ width 2)))
                                              :y (floor (- y (/ height 2)))
                                              :visible-min-width width
                                              :width width
                                              :visible-min-height height
                                              :height height))
                    (editor (capi:element-interface pane)))
               (setf (capi:drop-object-drop-effect object) :copy)
               (if (null (append-string ir-placer)) (setf (append-string ir-placer) ""))
               (capi:manipulate-pinboard pane ir-placer :add-top)
               (setf (current-interactive-routine editor) ir-placer)
               (setf (interaction-mode editor) ir)
               (gp:invalidate-rectangle pane)
               (capi:raise-interface (capi:element-interface pane))
               (capi:apply-in-pane-process pane #'capi:set-pane-focus pane)
)))))
)

(defmethod connected-in-iroutine ((task1 interactive-routine-task-pointer) (task2 interactive-routine-task-pointer) (type interactive-routine-pointer))
  (declare (ignore type))
  (if (member (pointer task2) (edges-out (pointer task1)))
      t
    nil))

(capi:define-interface ir-list-window ()
  ()
  (:layouts
   (ir-pinboard
    ir-list-layout
    nil
    :items nil
    :background :white
    :key-function #'name
    :accessor ir-pinboard
    :visible-min-width 220
    :visible-min-height 240
    :vertical-scroll t
    :horizontal-scroll t
    :font (gp:make-font-description :family "Verdana" :size 12)
    :input-model '(((:button-1 :press) select-iroutine)
                   ((:button-1 :motion) drag-iroutine-from))))
  (:default-initargs
   :title "Interactive Routines"
   :x (- (capi:screen-width *capi-screen*) 220)
   :y 0
   :window-styles '(:toolbox)))

(defun populate-with-interactive-routines (toolbox)
  (setf (ir-list-layout-items (ir-pinboard toolbox)) (mapcar #'convert-internal-pointer-to-external-pointer (app-property 'interactive-routines)))
)

(defun read-interactive-routines (files)
  (setf (app-property 'interactive-routines) nil)
  (dolist (file files)
    (with-open-file (stream file :direction :input :external-format '(:latin-1 :eol-style #+win32 :crlf #-win32 :lf))
      (push (eval (read stream))
            (app-property 'interactive-routines)))))
