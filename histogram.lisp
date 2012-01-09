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

; Histogram Window

(defconstant *golden-ratio* (/ (+ 1 (sqrt 5)) 2))

(defun profile-button-redraw-press (data interface)
  (declare (ignore data))

  (setf (max-time interface) (read-from-string (capi:text-input-pane-text (profile-max interface))))
  (setf (min-time interface) (read-from-string (capi:text-input-pane-text (profile-min interface))))
  (setf (profile-image interface)
        (draw-profile (profile-graph interface) (model-activities (controller (view-controller interface)))))
  (gp:invalidate-rectangle (profile-graph interface))
  

  )

(defun export-results-to-file (data interface)
  (declare (ignore data))
  (let ((src (merge-pathnames (format nil "Results/~A.xls" (first (list-trials-in-bundle (filename (model (controller (view-controller interface))))))) (filename (model (controller (view-controller interface))))))
        (dest (capi:prompt-for-file "Destination: " :filters '("Excel file" "*.xls" "Tab-delimited text file" "*.txt" "All files" "*.*") :operation :save :if-exists :prompt)))
    (if (not dest) (return-from export-results-to-file))
    (copy-file src dest)))

(capi:define-interface histogram-window (interface-view)
  ((image :initform nil :initarg :image :accessor image)
   (model-window :initform nil :initarg :model-window :accessor model-window)
   (intervals :initform nil :initarg :intervals :accessor intervals)
   (profile-image :initform nil :initarg :profile-image :accessor profile-image)
   (min-time :initform 0.0 :initarg :min-time :accessor min-time)
   (max-time :initform 0.0 :initarg :max-time :accessor max-time)
   (min-x :initform 0 :initarg :min-x :accessor min-x)
   (max-x :initform 0 :initarg :max-x :accessor max-x)
   (data :initform 0 :initarg :data :accessor data)
   (activities :initform nil :initarg :activities :accessor activities)
   (y-pos-hash :initform (make-hash-table :test #'equal) :accessor y-pos-hash)
   )
  (:panes
   (title-trials
    capi:title-pane
    :accessor title-trials
    :title "Trials ="
    :text (format nil "~A" 100))
   (run-time
    capi:title-pane
    :accessor run-time
    :title "Execution time ="
    :text (format nil "~A" (get-internal-real-time)))
   (path-list
    capi:multi-column-list-panel
    :interaction :single-selection
    :accessor path-list
    :selection-callback #'select-path
    :retract-callback #'select-path
    :extend-callback #'select-path
    :columns '((:title "%" :visible-min-width 30)
               (:title "Mean" :visible-min-width 50)
               (:title "StDev" :visible-min-width 50)
               (:title "Min" :visible-min-width 50)
               (:title "Max" :visible-min-width 50)
               (:title "CPI" :visible-min-width 20)
               (:title "Path" :visible-min-width 100))
    :visible-min-height '(character 6)
;    :visible-max-height '(character 12)
    :column-function #'print-column-data
    :title "Path statistics:"
    )
   (histogram
    capi:output-pane
    :accessor histogram
    :visible-min-width (* *golden-ratio* 300)
    :visible-max-width nil
    :visible-min-height 200
    :visible-max-height nil
    :resize-callback #'(lambda (pane x y width height)
                         (declare (ignore x y width height))
                         (let ((interface (capi:element-interface pane)))
                           (setf (image interface) nil)
                           (gp:invalidate-rectangle pane)))
    :display-callback #'(lambda (port x y width height)
                          (declare (ignore x y width height))
                          (let ((interface (capi:element-interface port)))
                            (if (image interface)
                                (gp:draw-image port (image interface) 0 0)
                              (setf (image interface)
                                    (draw-histogram port
                                                    (data interface)
                                                    (intervals interface))))))

    )
   (profile-graph
    capi:output-pane
    :accessor profile-graph
    :visible-min-width (* *golden-ratio* 300)
    :visible-max-width nil
    :visible-min-height 200
    :visible-max-height nil
    :input-model '(((:button-1 :press)
                    profile-graph-press)
                   ((:button-1 :release)
                    profile-graph-release))
    :resize-callback #'(lambda (pane x y width height)
                         (declare (ignore x y width height))
                         (let ((interface (capi:element-interface pane)))
                           (setf (profile-image interface) nil)
                           (gp:invalidate-rectangle pane)))
    :display-callback #'(lambda (port x y width height)
                          (declare (ignore x y width height))
                          (let ((interface (capi:element-interface port)))
                            (if (profile-image interface)
                                (gp:draw-image port (profile-image interface) 0 0)
                              (setf (profile-image interface)
                                    (draw-profile port
                                                  (model-activities (controller (view-controller interface)))))
                              )))

    )
   (activity-order
    capi:list-panel
    :accessor activity-order
    :title "Activity legend:"
    :visible-min-height '(character 4)
    :print-function #'capi:item-text
    :items '("1. blassdf" "2. blaasdf" "3. blaasdf" "4. blaasdf"))
   (profile-min
    capi:text-input-pane
    :accepts-focus-p nil
    :visible-max-width '(character 8)
    :accessor profile-min
    :title "X-min ="
    :text "0")
   (profile-max
    capi:text-input-pane
    :accepts-focus-p nil
    :visible-max-width '(character 8)
    :accessor profile-max
    :title "X-max ="
    :text "20")
   (profile-button-redraw
    capi:push-button
    :accessor profile-button-redraw
    :callback #'profile-button-redraw-press
    :text "Redraw")

   (x-min
    capi:text-input-pane
    :accepts-focus-p nil
    :accessor x-min
    :title "X-min ="
    :text "0")
   (x-max
    capi:text-input-pane
    :accepts-focus-p nil
    :accessor x-max
    :title "X-max ="
    :title "20")
   (interval-size
    capi:text-input-pane
    :accepts-focus-p nil
    :accessor interval-size
    :title "Bin size ="
    :text "1")
   (button-redraw
    capi:push-button
    :accessor button-redraw
    :callback #'button-redraw-press
    :text "Redraw")
   )
  (:layouts
   (main
    capi:column-layout '(title-trials run-time graph-layout :divider path-list)
    :adjust :left)
   (graph-layout
    capi:tab-layout '()
    :items (list (list "Histogram" histogram-container)
                 (list "Profile" profile-container))
    :accessor tab-layout
    :print-function 'car
    :visible-child-function #'second)
   (histogram-container
    capi:column-layout '(histogram histogram-settings-row)
    :accessor histogram-container
    :adjust :center)
   (profile-container
    capi:column-layout '(profile-graph profile-settings-row)
    :accessor profile-container
    :adjust :left)
   (order-buttons
    capi:row-layout '(button-move-up button-move-down))
   (histogram-settings-row
    capi:row-layout '(x-min x-max interval-size button-redraw)
    :adjust :left)
   (profile-settings-row
    capi:row-layout '(profile-min profile-max profile-button-redraw)
    :adjust :left)
   )
  (:menus
   (data-menu
    "Data"
    ((:component
      (("Export..."
        :accelerator "accelerator-s"
        :callback #'export-results-to-file)
       ))
     ))
   (window-menu
    "Window"
    ((:component
      (("Close Window"
        :accelerator "accelerator-w"
        :callback #'(lambda (interface)
                      (capi:destroy interface))
        :callback-type :interface)
       ))
     (:component
      ()
      :selection-callback #'(lambda (data interface)
                              (declare (ignore interface))
                              (capi:execute-with-interface data 'capi:raise-interface data))
      :items-function #'(lambda (interface)
                          (declare (ignore interface))
                          (capi:collect-interfaces 'interface-view))
#|
      :selection-function #'(lambda (interface)
                              (position interface (capi:collect-interfaces 'interface-view))
                              )
|#
      :print-function #'(lambda (interface)
                          (cond ((equal (type-of interface) 'model-editor)
                                 (if (filename (model interface))
                                     (get-bundle-name (filename (model interface)))
                                   "Untitled"))
                                ((equal (type-of interface) 'histogram-window)
                                 (capi:interface-title interface))
                                (t
                                 (capi:interface-title interface)))
                          )
      )
     )
    ))
  (:menu-bar data-menu window-menu)
  (:default-initargs
   :title "Simulation results"
   :x 40
   :destroy-callback #'(lambda (self)
                         (dolist (act (model-activities (controller (view-controller self))))
                           (setf (on-critical-path? act) nil)))
   :y 40))

(defun button-redraw-press (data interface)
  (declare (ignore data))
  (let* ((x-min (read-from-string (remove #\\ (capi:text-input-pane-text (x-min interface))) nil))
         (x-max (read-from-string (remove #\\ (capi:text-input-pane-text (x-max interface))) nil))
         (interval-size (read-from-string (remove #\\ (capi:text-input-pane-text (interval-size interface))) nil)))
    
    (setf (intervals interface) (create-intervals x-min x-max interval-size))
    
    (setf (image interface)
          (draw-histogram (histogram interface)
                          (data interface)
                          (intervals interface)))
    (gp:invalidate-rectangle (histogram interface))
    )
  )

(defun select-path (item interface)
  (declare (ignore item))
  (setf (data interface) nil)
  (dolist (act (model-activities (controller (view-controller interface))))
    (setf (on-critical-path? act) nil))
  (cond ((null (capi:choice-selected-item (path-list interface)))
         (return-from select-path)))
;  (break "Test")
  (dolist (item (capi:choice-selected-items (path-list interface)))
    (setf (data interface) (append (data interface) (data item))))
  
  (setf (image interface) nil)
  (if (equal (capi:tab-layout-visible-child (tab-layout interface))
             (histogram-container interface))
      (gp:invalidate-rectangle (histogram interface))
    (progn (setf (profile-image interface) nil)
      (gp:invalidate-rectangle (profile-graph interface))))
  (dolist (item (path-data (capi:choice-selected-item (path-list interface))))
;    (break "Item ~A" item)
    (setf (on-critical-path? item) t)))

(defmethod display-histogram-window ((vc view-controller) max-time trials simulation-min simulation-max simulation-results path-records run-time result-file)
  (let ((window nil)
        (activities (model-activities (controller vc)))
        (simulation-range 0)
        (simulation-middle 0))
    (if (not (equalp nil (viewhash 'sanlab-histogram-window vc)))
        (setf window (viewhash 'sanlab-histogram-window vc))
      (setf (viewhash 'sanlab-histogram-window vc) (setf window (make-instance 'histogram-window))))
    (if (> (mod (ceiling max-time) 10) 0)
        (setf max-time (+ (ceiling max-time) (- 10 (mod (ceiling max-time) 10))))
      (setf max-time (ceiling max-time)))
    (setf (min-time window) 0.0)
    (setf (max-time window) max-time)

;    (break "1")

    (setf (capi:title-pane-text (title-trials window))
          (format nil "~A" trials))
    (setf (capi:text-input-pane-text (profile-min window))
          (format nil "~A" (min-time window)))
    (setf (capi:text-input-pane-text (profile-max window))
          (format nil "~A" (max-time window)))

;    (break "2")

    (if (not (equal nil (pathname (controller vc))))
        (setf (capi:interface-title window)
              (format nil "Simulation results - ~A" (get-bundle-name (pathname (controller vc)))))
      (setf (capi:interface-title window)
            (format nil "Simulation results - Untitled")))

;    (break "3")

    (let ((counter 1)
          (new-list nil)
          (all-types (reverse (remove-duplicates (mapcar #'(lambda (act) (activity-type act)) activities) :test #'pointer=))))

;      (break "4")

      (dolist (activity all-types)
        (push
         (make-instance 'capi:item
                        :text (format nil "~a. ~A" counter activity)
                        :data activity) new-list)
        (incf counter))

;      (break "5")
      
      (setf (capi:collection-items (activity-order window))
            (reverse new-list))

      (setf (activities window) activities)
      
;      (break "6")

      (setf simulation-range (max simulation-range (* 1.1 (- simulation-max simulation-min))))
      (setf simulation-middle (/ (+ simulation-max simulation-min) 2))

;      (break "7")

      (setf (capi:text-input-pane-text (x-min window))
            (format nil "~A" (- (round (- simulation-middle (/ simulation-range 2)))
                                (mod (round (- simulation-middle (/ simulation-range 2))) 10))))
      (setf (capi:text-input-pane-text (x-max window))
            (format nil "~A" (+ (round (+ simulation-middle (/ simulation-range 2)))
                                (- 10 (mod (round (+ simulation-middle (/ simulation-range 2))) 10)))))
      (setf (capi:text-input-pane-text (interval-size window))
            (format nil "~A" (round (float (/ simulation-range 15)))))

;      (break "8")

      (setf (image window) nil)
      (setf (data window) simulation-results)
      (setf (profile-image window) nil)
      (setf (capi:collection-items (path-list window)) (sort (reverse path-records) #'> :key #'(lambda (x) (read-from-string (percent x)))))

;      (break "9")

      (let* ((x-min (read-from-string (remove #\\ (capi:text-input-pane-text (x-min window))) nil))
             (x-max (read-from-string (remove #\\ (capi:text-input-pane-text (x-max window))) nil))
             (interval-size (read-from-string (remove #\\ (capi:text-input-pane-text (interval-size window))) nil)))
        (if (= 0 interval-size) (setf interval-size 10 (capi:text-input-pane-text (interval-size window)) "1"))

;        (break "10  x-min:  ~A    x-max:  ~A    interval-size:  ~A" x-min x-max interval-size)

        (setf (intervals window) (create-intervals x-min x-max interval-size))

;        (break "11")

        (setf (capi:title-pane-text (run-time window))
              (format nil "~1,2f s" (/ run-time 1000)))
        (capi:display window)

;        (break "12")

        (if (equal (capi:tab-layout-visible-child (tab-layout window))
                   (histogram-container window))
            (gp:invalidate-rectangle (histogram window))
          (gp:invalidate-rectangle (profile-graph window))))
      (capi:raise-interface window)

      #+cocoa
      (set-cocoa-pathname window result-file)

;      (break "13")
)))

(defun print-column-data (path-record)
  (list (percent path-record)
        (mean path-record)
        (format nil "~2,2f" (stdev (data path-record) (read-from-string (mean path-record))))
        (format nil "~2,2f" (min-time path-record))
        (format nil "~2,2f" (max-time path-record))
        (cpindex path-record)
        (label path-record)))


