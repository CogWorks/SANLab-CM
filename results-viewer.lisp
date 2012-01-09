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

(capi:define-interface results-viewer (interface-view)
  ((the-results :initform nil :initarg :the-results :accessor results-viewer-results)
   (the-model :initform nil :initarg :the-model :accessor results-viewer-model)
   (source :initform nil :initarg :source :accessor results-viewer-source))
  (:panes
   (results-list
    capi:multi-column-list-panel
    :interaction :single-selection
    :accessor results-viewer-list
    :columns '((:title "Timestamp" :visible-min-width 30)
               (:title "Trial" :visible-min-width 50)
               (:title "Duration" :visible-min-width 50)
               (:title "Critical Path" :visible-min-width 100))
    :visible-min-width '(character 80)
    :visible-min-height '(character 25)
    :column-function #'print-results-data
    :title "Results:"
    ))
  (:menus
   (results-menu
    "Results"
    (("Export..."
      :callback #'results-viewer-export
      :callback-type :interface
      :accelerator "accelerator-s"))))
  (:menu-bar results-menu)
  (:default-initargs
   :title "Results for "))

(defmethod results-viewer-export ((viewer results-viewer))
  (let ((src (results-viewer-source viewer))
        (dest (capi:prompt-for-file "Destination:" :filters '("Excel file" "*.xls" "Tab-delimited text file" "*.txt" "All files" "*.*") :operation :save :if-exists :prompt)))
    (if dest
        (copy-file src dest))))

(defun print-results-data (item)
  (let ((model (trial-result-model item)))
    (list (trial-result-timestamp item)
          (trial-result-trial item)
          (trial-result-duration item)
          (format nil "~A" (mapcar #'(lambda (x)
                                       (let ((act (find-if #'(lambda (y) (= (uid y) x)) (activities model))))
                                         (if act
                                             (format nil "\"~A\"" (label act))
                                           (format nil "\"Missing activity ~A\"" x))))
                                   (trial-result-path item))))))

