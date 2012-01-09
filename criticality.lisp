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


(defclass criticality-tracker ()
  ((table :initform (make-hash-table :test #'equal) :initarg :table :accessor ct-table)
   (trials :initform 0 :initarg :trials :accessor ct-trials)))

(defmethod inc-trials ((ct criticality-tracker))
  (incf (ct-trials ct)))

(defmethod criticality ((key instance-pointer) (ct criticality-tracker))
  (criticality (pointer key) ct)
)

(defmethod criticality (key (ct criticality-tracker))
  (/ (or (gethash key (ct-table ct)) 0) (ct-trials ct)))

(defmethod inc-criticality (key (ct criticality-tracker))
  (if (null (gethash key (ct-table ct)))
      (setf (gethash key (ct-table ct)) 1)
    (incf (gethash key (ct-table ct)))))

(defun display-criticality-window (ct model)
  (make-instance 'criticality-window :criticality-tracker ct :model model))

(defun print-criticality-data (item)
  (list (format nil "~2,2f%" (* 100 (first item)))
        (second item)))

(defun export-criticality-to-file (data interface)
  (declare (ignore data))
  (let ((filename (capi:prompt-for-file "Export to: "  :filters '("Excel file" "*.xls" "Tab-delimited text file" "*.txt" "All files" "*.*") :operation :save :if-exists :prompt)))
    (if (null filename) (return-from export-criticality-to-file))
    (with-open-file (stream filename :direction :output :if-exists :supersede :external-format '(:latin-1 :eol-style #+win32 :crlf #-win32 :lf))
      (format stream "CRITICALITY~aACTIVITY~%" #\tab)
      (dolist (item (capi:collection-items (act-list interface)))
        (format stream "~A~A~A~%" (first item) #\tab (second item))))))

(defun special< (x y)
  (cond ((stringp x) (string< x y))
        ((numberp x) (< x y))))

(defun special> (x y)
  (cond ((stringp x) (string> x y))
        ((numberp x) (> x y))))

(capi:define-interface criticality-window (interface-view)
  ((ct-slot :initform nil :initarg :criticality-tracker :accessor ct-tracker)
   (sort-info :initform (list "CP %" 'ascending) :accessor sort-info))
  (:panes
   (act-list capi:multi-column-list-panel
             :interaction :extended-selection
             :accessor act-list
             :selection-callback nil
             :header-args (list :items '("CP %" "Activity")
                                :callback-type :full
                                :selection-callback #'(lambda (intf item)
                                                        (let* ((ct (ct-tracker intf))
                                                               (info (mapcar #'(lambda (act) (list (criticality act ct) (label act))) (model-activities (controller (view-controller intf)))))
                                                               (key (if (equal "CP %" item) #'first #'second)))
                                                          (if (equal (first (sort-info intf)) item)
                                                              (setf (second (sort-info intf)) (if (equal (second (sort-info intf)) 'ascending) 'descending 'ascending)))
                                                          (setf (capi:collection-items (act-list intf))
                                                                (sort info (if (equal (second (sort-info intf)) 'ascending) #'special< #'special>) :key key))
                                                          (setf (first (sort-info intf)) item))))
             :columns '((:title "CP %" :visible-min-width 50)
                        (:title "Activity" :visible-min-width 250))
             :visible-min-height '(character 12)
             :column-function #'print-criticality-data))
  (:menus
   (data-menu
    "Data"
    ((:component
      (("Export..."
        :accelerator "accelerator-s"
        :callback #'export-criticality-to-file)
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
                          (capi:collect-interfaces 'capi:interface))
      :selection-function #'(lambda (interface)
                              (position interface (capi:collect-interfaces 'capi:interface)))
      :print-function #'(lambda (interface)
                          (cond ((equal (type-of interface) 'model-editor)
                                 (if (filename (model interface))
                                     (get-bundle-name (filename (model interface)))
                                   "Untitled"))
                                (t
                                 (capi:interface-title interface))))
      )
     ))
   )
  (:menu-bar data-menu window-menu) 
  (:default-initargs
   :title "Model Criticality"
   :visible-min-width 350))

(defmethod initialize-instance :after ((window criticality-window) &rest init-args)
  (let ((ct (ct-tracker window))
        (activities (model-activities (controller (view-controller window)))))
    (setf (capi:collection-items (act-list window)) (mapcar #'(lambda (act) (list (criticality act ct) (label act))) activities))))