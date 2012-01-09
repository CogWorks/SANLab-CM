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

; Code to draw a Histogram in a capi:output-pane

(declaim (optimize (safety 1) (speed 3) (space 0)))

(defun draw-profile (port activities)
  (let* ((max-time nil)
         (min-time nil)
         (interface (capi:element-interface port))
         (path (if (capi:choice-selected-item (path-list interface)) (path-data (capi:choice-selected-item (path-list interface)))))
         (x-indent 40)
         (activity-height 20)
         (activity-spacing 15)
         (combined-height nil)
         (font (gp:find-best-font port (gp:make-font-description :family "Verdana" :size 12)))
         (all-types (reverse (remove-duplicates (mapcar #'(lambda (activity) (activity-type activity)) activities) :test #'pointer=)))
         (activity-count 1)
         (max-label-width nil))

    ;(setf all-types (capi:map-collection-items (activity-order interface)
    ;                                           #'(lambda (item) (capi:item-data item)) t))
    (dolist (activity-type all-types)
      (multiple-value-bind (string-left string-top string-right string-bottom) (gp:get-string-extent port (get-full-name activity-type) font)
        (declare (ignore string-top string-bottom))
        (if (or (not max-label-width) (> (- string-right string-left) max-label-width))
            (setf max-label-width (- string-right string-left)))))

    (setf x-indent (+ max-label-width 20))

    #|
    (dolist (activity activities)
      (let ((mean-end (max (mean-end activity) (if path (gethash path (conditional-mean-end activity)) (mean-end activity)))))
        (if (or (not max-time) (> mean-end max-time))
            (setf max-time mean-end))))
|#

    (setf min-time (min-time interface))
    (setf max-time (max-time interface))

    #|
    (capi:apply-in-pane-process
     (profile-min interface)
     #'(lambda ()
         (setf (capi:text-input-pane-text (profile-min interface)) (format nil "~f" 0.0))))
    (capi:apply-in-pane-process
     (profile-max interface)
     #'(lambda ()
         (setf (capi:text-input-pane-text (profile-max interface)) (format nil "~f" max-time))))
     |#
    
    (gp:clear-graphics-port port)

    (capi:with-geometry port
      
      (setf activity-height (min 20 (- (/ (- capi:%height% 60) (length all-types)) activity-spacing)))
      (setf combined-height (* (length all-types) (+ activity-height activity-spacing)))

      (let ((scale (/ (- capi:%width% x-indent 40) (- max-time min-time)))
            )
        
        ;; Draw the background
        (gp:draw-rectangle
         port
         x-indent 20 (1+ (- capi:%width% x-indent 40)) (- capi:%height% 40) ; (- capi:%height% 25)
         :foreground :white :filled t)

        (setf (min-x interface) x-indent)
        (setf (max-x interface) (+ x-indent (1+ (- capi:%width% x-indent 40))))

        (gp:draw-rectangle
         port
         x-indent 20 (1+ (- capi:%width% x-indent 40)) (- capi:%height% 40) ; (- capi:%height% 25)
         :foreground :black :filled nil)
        
        ;; Draw the X-axis
        (gp:draw-line
         port
         x-indent (- capi:%height% 20) (+ x-indent (- capi:%width% x-indent 40)) (- capi:%height% 20)
         :foreground :black)

        #|
        (gp:draw-line
         port
         x-indent (- capi:%height% 20) x-indent 20 ; (- capi:%height% 25)
         :foreground :black)
        (gp:draw-line
         port
         (+ x-indent (* max-time scale)) (- capi:%height% 20) (+ x-indent (* max-time scale)) (- capi:%height% 25)
         :foreground :black)
|#

        (gp:draw-string
         port (format nil "~2,2f" min-time)
         x-indent (- capi:%height% 6)
         :font font)
        (gp:draw-string
         port (format nil "~2,2f" max-time)
         (- capi:%width% 50) (- capi:%height% 6)
         :font font)

        (dolist (activity-type all-types)

          (setf (gethash (pointer activity-type) (y-pos-hash interface))
                (+ (* 0.75 activity-height) (/ (- capi:%height% 80) 2) (* -0.5 activity-height) (- (/ combined-height 2))
                   (* activity-count (+ activity-height activity-spacing))))

          (gp:draw-string
           port (format nil "~a:" (get-full-name activity-type)) 2
           (+ (* 0.75 activity-height) (/ (- capi:%height% 80) 2) (* -0.5 activity-height) (- (/ combined-height 2))
              (* activity-count (+ activity-height activity-spacing)))
           :font font)

          (let ((instances (remove-if #'(lambda (instance) (not (pointer= (activity-type instance) activity-type))) activities))
                (activity-start nil) (activity-width nil) (activity-end nil)
                )
            (dolist (activity instances)
              (let ((mean-start (if path (gethash path (conditional-mean-start activity)) (mean-start activity)))
                    (mean-end (if path (gethash path (conditional-mean-end activity)) (mean-end activity))))
                (setf activity-start (max x-indent (+ x-indent (* scale (- mean-start min-time)))))
                (setf activity-end (min (- capi:%width% 40) (+ x-indent (* scale (- mean-end min-time)))))
                (setf activity-width (- activity-end activity-start))
                
                ;(if (> (+ activity-start activity-width) (- capi:%width% 40))
                ;    (setf activity-width (- activity-width (- (+ activity-start activity-width) (- capi:%width% 40)))))

                (if (> activity-width 0)
                    (progn
                (gp:draw-rectangle
                 port activity-start
                 (+ (/ (- capi:%height% 80) 2) (* -0.5 activity-height) (- (/ combined-height 2))
                    (* activity-count (+ activity-height activity-spacing)))
                 activity-width
                 activity-height
                 :foreground (copy-seq (color activity))
                 :filled t)
                ;; Draw the frame
                (gp:draw-rectangle
                 port activity-start
                 (+ (/ (- capi:%height% 80) 2) (* -0.5 activity-height) (- (/ combined-height 2))
                    (* activity-count (+ activity-height activity-spacing)))
                 activity-width
                 activity-height
                 :foreground :black
                 :thickness 1
                 :filled nil)
                )))
              )
            )
          (incf activity-count)
          )
        ))
    )
  (gp:make-image-from-port port))

(defun profile-graph-press (port x y)
  (let* ((interface (capi:element-interface port))
         (font (gp:find-best-font port (gp:make-font-description :family "Verdana" :size 12)))
         (time 
          (+ (min-time interface) (* (- (max-time interface) (min-time interface))
                                     (/ (- x (min-x interface)) (- (max-x interface) (min-x interface))))))
         (activities (model-activities (controller (view-controller interface))))
         (candidates nil)
         (path (if (capi:choice-selected-item (path-list interface)) (path-data (capi:choice-selected-item (path-list interface))))))

    

      (setf candidates (remove-if
                        #'(lambda (activity)
                            (let ((mean-start (if path (gethash path (conditional-mean-start activity)) (mean-start activity)))
                                  (mean-end (if path (gethash path (conditional-mean-end activity)) (mean-end activity))))
                              (or (< time mean-start) (> time mean-end))))
                        activities))
      (if candidates
          (let ((min-dist nil)
                (min-activity nil))
            (dolist (candidate candidates)
              (let ((y-pos (gethash (pointer (activity-type candidate)) (y-pos-hash interface))))
                (if (or (not min-dist) (< (abs (- y y-pos)) min-dist))
                    (progn (setf min-dist (abs (- y y-pos)))
                      (setf min-activity candidate)))))
            (if (> min-dist 20) (setf min-activity nil))
            
            (if min-activity
                (multiple-value-bind (string-left string-top string-right string-bottom)
                    (gp:get-string-extent port (label min-activity) font)
                  (let ((start-pos x)
                        (box-width nil))
                    (setf box-width (max (- string-right string-left) 100))
                    (capi:with-geometry port
                      (if (> (+ start-pos 20 box-width)
                             capi:%width%)
                          (setf start-pos (- start-pos (- (+ start-pos 30 box-width) capi:%width%)))))
                    (gp:draw-rectangle
                     port start-pos y
                     (+ 20 box-width) -55
                     :foreground (color:make-rgb 1.0 1.0 1.0 0.8) :filled t)
                    (gp:draw-rectangle
                     port start-pos y
                     (+ 20 box-width) -55
                     :foreground :black :filled nil)
                    (gp:draw-string
                     port (label min-activity) (+ start-pos 10) (- y 40)
                     :font font
                     )
                    (let ((mean-start (if path (gethash path (conditional-mean-start min-activity)) (mean-start min-activity)))
                          (mean-end (if path (gethash path (conditional-mean-end min-activity)) (mean-end min-activity))))
                      (gp:draw-string
                       port (format nil "Start: ~2,2f" mean-start) (+ start-pos 10) (- y 20) :font font)
                      (gp:draw-string
                       port (format nil "End: ~2,2f" mean-end) (+ start-pos 10) (- y 5) :font font))
                    )
                  
                  
                  )
              )
            )
        )
      )
  )

(defun profile-graph-release (port x y)
  (declare (ignore x y))
  (gp:invalidate-rectangle port))

(defun profile-graph-motion (port x y)
  (gp:invalidate-rectangle port)
  (profile-graph-press port x y))

(defun draw-histogram (port data-points intervals &key (font-size 12) (graph-title "Distribution of completion times") (color :red))
  (declare (ignore graph-title))
  (let* ((font (gp:find-best-font port (gp:make-font-description :family "Verdana" :size font-size)))
         (x-min nil) (x-max nil)
         (y-min nil) (y-max nil)
         (n (- (length intervals) 1))
         (bin-counts (make-array n :initial-element 0))
         (min 0) (max 0)
         (skip 0))

    (gp:clear-graphics-port port)

    (capi:with-geometry port
      (let ((bar-width nil)
            (bar-height 0)
            (caption nil)
            (string-width nil))
        
        ;; Determine the boundaries of the graph area
        (let ((max-y-label "100%"))
          (multiple-value-bind (string-left string-top string-right string-bottom) (gp:get-string-extent port max-y-label font)
            (setf x-min (+ (abs (- string-right string-left)) 2))
            (setf x-max capi:%width%)
            (setf y-min (* 2 (abs (- string-top string-bottom))))
            (setf y-max (- capi:%height% (* 3 (abs (- string-top string-bottom)))))
            (setf bar-width (/ (- x-max x-min) n)))
          )

        ;; Draw the Y-axis labels & grid lines
        (dotimes (i 6)
          (setf caption (format nil "~a%" (* i 20)))
          (multiple-value-bind (string-left string-top string-right string-bottom) (gp:get-string-extent port caption font)
            (gp:draw-line
             port
             x-min
             (+ (- y-max (* (/ i 5) (- y-max y-min))))
             x-max
             (+ (- y-max (* (/ i 5) (- y-max y-min))))
             :dashed t)
            (gp:draw-string
             port
             caption
             (- x-min (- string-right string-left) 2)
             (+ (- y-max (* (/ i 5) (- y-max y-min))) (* 0.5 (abs (- string-bottom string-top))))
             :font font)))

        ;; Draw the X-axis
        (gp:draw-line port x-min y-max x-max y-max :foreground :black)

        ;; Draw the chart title
        ;(multiple-value-bind (string-left string-top string-right string-bottom) (gp:get-string-extent port graph-title font)
        ;  (gp:draw-string
        ;   port
        ;   graph-title
        ;   (+ x-min (* 0.5 (- x-max x-min)) (- (* 0.5 (- string-right string-left))))
        ;   (+ 0 (abs (- string-top string-bottom))) :font font))

        ;; Draw the Y-axis
        (gp:draw-line port x-min y-max x-min y-min :foreground :black)
        (gp:draw-line port x-min y-min (+ x-min 3) y-min :foreground :black)

        (cond
         ((> (length data-points) 0)

          ;; Group data points into bins
          (dolist (point data-points)
            (dotimes (i n)
              (setf min (nth i intervals))
              (setf max (nth (+ i 1) intervals))
              (if (and (>= point min)
                       (< point max))
                  (incf (aref bin-counts i)))))))
          
        ;; Draw the data points
        (dotimes (i n)

          (cond
           ((> (length data-points) 0)
            
            (setf bar-height (* (/ (aref bin-counts i) (length data-points)) (- y-max y-min)))
          
            ;; Draw the bar
            (gp:draw-rectangle port (+ x-min (* i bar-width)) y-max
                               (* 0.9 bar-width) (- bar-height) :foreground color :filled t)
            ))
          
          ;; Draw the data point caption
          ;(setf caption (format nil "[~a,~a)" (nth i intervals) (nth (1+ i) intervals)))
          ;(setf caption (format nil "~2,1f" (nth i intervals)))
          (setf caption (format nil "~a" (round (nth i intervals))))
          (gp:draw-line port (+ x-min (* i bar-width)) (+ y-max 4) (+ x-min (* i bar-width)) y-max)
          (multiple-value-bind (string-left string-top string-right string-bottom) (gp:get-string-extent port caption font)
            (setf string-width (abs (- string-right string-left)))
            (decf skip)
            (if (<= skip 0)
                (progn
                  (gp:draw-line port (+ x-min (* i bar-width)) (+ y-max 8) (+ x-min (* i bar-width)) y-max :thickness 1)
                  (gp:draw-string
                   port
                   caption
                   (+ x-min (* i bar-width) (* -0.5 string-width))
                   (+ y-max 3 (* 2 (abs (- string-top string-bottom)))) :font font)
                  (setf skip (ceiling (* 1.2 (/ string-width bar-width))))))
            )
          )
        ))
    (gp:make-image-from-port port)
    )
  )

(defun create-intervals (x-min x-max interval-size)
  (let ((current-x x-min)
        (intervals nil))
    (if (= x-min x-max) (return-from create-intervals (list x-min)))
    (if (= 0 interval-size)
        (return-from create-intervals (dotimes (i (- x-max x-min) (reverse intervals))
                                        (push i intervals))))
    (loop
     (when (> current-x x-max) (return))
     (push current-x intervals)
     (setf current-x (+ current-x interval-size)))
    (setf intervals (reverse intervals))
    intervals))

(defun redraw-histogram-pane (pane x y width height)
  (declare (ignore x y width height))
  (let* ((interface (capi:element-interface pane))
         (image (image interface)))
    (if image
        (gp:draw-image pane image 0 0))))
