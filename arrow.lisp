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

; Decomposes all arrows if any is part of an interactive routine
(defun get-arrow-parts (list-of-arrows)
  (let ((arrows nil))
    (dolist (arrow list-of-arrows arrows)
      (setq arrows (append arrows (get-subarrows arrow))))))

; Decomposes an arrow into its component parts
(defun get-subarrows (arrow)
  (cond ((and (not (normal-arrow arrow)) (not (ir-arrow arrow)))
         nil)
        ((not (ir-arrow arrow))
         (list (normal-arrow arrow)))
        (t
         (list (ir-arrow arrow) (normal-arrow arrow)))))

; Highlights an arrow and redraws it
(defmethod (setf selected) :after (val (self arrow))
  (if val
      (setf (capi:pinboard-object-graphics-args (normal-arrow self)) '(:thickness 1 :foreground #(:RGB 0.0 0.0 1.0)))
    (setf (capi:pinboard-object-graphics-args (normal-arrow self)) '(:thickness 1 :foreground #(:RGB 0.0 0.0 0.0))))
  (gp:invalidate-rectangle (normal-arrow self)))

; Redraws the arrow if the interactive routine color changes
(defmethod (setf ir-color) :after (val (obj arrow))
  (setf (slot-value obj 'is-ir-arrow) (if val t nil))
  (setf (ir-arrow obj) (if val (make-instance 'capi:arrow-pinboard-object :head :middle :head-breadth 4 :graphics-args (list :thickness 5 :foreground val)) nil)))

; Computes the center of an activity pane to determine the endpoint of an arrow
(defmethod center ((obj activity-pane))
  (multiple-value-bind (x y) (capi:pinboard-pane-position obj)
    (multiple-value-bind (w h) (capi:pinboard-pane-size obj)
      (if (null x) (setq x (x-position (source obj))))
      (if (null y) (setq y (y-position (source obj))))
      (if (null w) (setq w (app-property 'editor-default-activity-width)))
      (if (null h) (setq h (app-property 'editor-default-activity-height)))
      (values (+ x (floor (/ w 2))) (+ y (floor (/ h 2)))))))

; Updates an arrow to reflect movement in one or more of the activities acting as its endpoints
(defmethod move-arrow ((obj arrow))
  (let ((start (start obj))
        (end (end obj)))
    (if (and start end)
        (multiple-value-bind (x1 y1) (center start)
          (multiple-value-bind (x2 y2) (center end)
;            (format t "Moving arrow to (~A, ~A) and (~A, ~A)~%" x1 y1 x2 y2)
            (if (normal-arrow obj) (capi:move-line (normal-arrow obj) x1 y1 x2 y2))
            (if (ir-arrow obj) (capi:move-line (ir-arrow obj) x1 y1 x2 y2)))))))

; Moves the head of the arrow to a particular X,Y coordinate. 
(defmethod move-arrow-end ((obj arrow) x y)
  (let ((start (start obj)))
    (if start
        (multiple-value-bind (x1 y1) (center start)
          (if (normal-arrow obj) (capi:move-line (normal-arrow obj) x1 y1 x y))
          (if (ir-arrow obj) (capi:move-line (ir-arrow obj) x1 y1 x y))))))

(defmethod initialize-instance :after ((self arrow) &rest initargs)
  (if (ir-color self) (setf (ir-color self) (ir-color self)))
  (move-arrow self)
)

; Sets the endpoints of the arrow to the specified coordinates
(defmethod set-endpoints ((obj arrow) start end)
  (setf (slot-value obj 'start) start)
  (setf (slot-value obj 'end) end)
  (move-arrow obj))

; Moves the arrow after the start node has changed
(defmethod (setf start) :after (val (obj arrow))
  (move-arrow obj))

; Moves the arrow after the end node has changed
(defmethod (setf end) :after (val (obj arrow))
  (move-arrow obj))

; The functions defined in this closure handle when the arrow changes from a normal arrow to an interactive routine arrow or vise versa.
; This is to handle the addition or removal of the secondary arrow that displays the interactive routine color behind the actual arrow.
(let ((arrow-to-replace nil))

(defmethod (setf normal-arrow) :before (val (the-arrow arrow))
  (cond ((capi:pinboard-object-pinboard the-arrow)
         (setq arrow-to-replace (position (normal-arrow the-arrow) (arrow-parts (capi:pinboard-object-pinboard the-arrow)))))))

(defmethod (setf normal-arrow) :after (val (the-arrow arrow))
  (cond (arrow-to-replace
         (if (null val)
             (setf (arrow-parts (capi:pinboard-object-pinboard the-arrow))
                   (remove (nth arrow-to-replace (arrow-parts (capi:pinboard-object-pinboard the-arrow)))
                           (arrow-parts (capi:pinboard-object-pinboard the-arrow))))
           (setf (nth arrow-to-replace (arrow-parts (capi:pinboard-object-pinboard the-arrow))) val))
         (update-activity-layout (capi:pinboard-object-pinboard the-arrow)))
        ((and (null arrow-to-replace) val)
         (setf (arrow-parts (capi:pinboard-object-pinboard the-arrow)) (append (get-subarrows the-arrow) (arrow-parts (capi:pinboard-object-pinboard the-arrow))))
         (update-activity-layout (capi:pinboard-object-pinboard the-arrow)))
        )
  (setq arrow-to-replace nil)
)

(defmethod (setf ir-arrow) :before (val (the-arrow arrow))
  (cond ((capi:pinboard-object-pinboard the-arrow)
         (setq arrow-to-replace (position (ir-arrow the-arrow) (arrow-parts (capi:pinboard-object-pinboard the-arrow)))))))

(defmethod (setf ir-arrow) :after (val (the-arrow arrow))
  (cond (arrow-to-replace
         (if (null val)
             (setf (arrow-parts (capi:pinboard-object-pinboard the-arrow))
                   (remove (nth arrow-to-replace (arrow-parts (capi:pinboard-object-pinboard the-arrow)))
                           (arrow-parts (capi:pinboard-object-pinboard the-arrow))))
           (setf (nth arrow-to-replace (arrow-parts (capi:pinboard-object-pinboard the-arrow))) val))
         (update-activity-layout (capi:pinboard-object-pinboard the-arrow)))
        ((and (null arrow-to-replace) val)
         (if (capi:pinboard-object-pinboard the-arrow)
             (progn
             (setf (arrow-parts (capi:pinboard-object-pinboard the-arrow)) (append (list val) (arrow-parts (capi:pinboard-object-pinboard the-arrow))))
             (update-activity-layout (capi:pinboard-object-pinboard the-arrow)))))
        )
  (setq arrow-to-replace nil)
)

)
