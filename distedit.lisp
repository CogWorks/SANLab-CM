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


(capi:define-interface dist-editor (interface-view)
  (
   (current-dist :initform nil :initarg :current-dist :accessor current-dist)
   (current-params :initform nil :initarg :current-params :accessor current-params)
   (current-param :initform nil :initarg :current-param :accessor current-param)
   (dist-changed :initform nil :initarg :dist-changed :accessor dist-changed?)
   )
  (:panes
   (dist-name
    capi:text-input-pane
    :text ""
    :title "Name: "
    :accessor dist-name
    :change-callback #'dist-editor-text-changed)
   (dist-doc
    capi:text-input-pane
    :text ""
    :title "Documentation: "
    :accessor dist-doc
    :change-callback #'dist-editor-text-changed)
   (dist-params
    capi:option-pane
    :initial-focus-item "None"
    :enabled nil
    :items '("None")
    :test-function #'equal
    :accessor dist-params
    :title "Parameters: "
    :selection-callback #'dist-editor-param-selected)
   (dist-params-add
    capi:push-button
    :text "Add"
    :accessor dist-params-add
    :callback #'dist-editor-add-param)
   (dist-params-remove
    capi:push-button
    :text "Remove"
    :enabled nil
    :accessor dist-params-remove
    :callback #'dist-editor-remove-param)
   (dist-params-default
    capi:text-input-pane
    :text ""
    :title "Default value: "
    :accessor dist-params-default
    :enabled nil
    :change-callback #'dist-editor-text-changed)
   (dist-editor
    capi:editor-pane
    :text ""
    :buffer-name (new-editor-buffer)
    :buffer-modes '("Lisp")
    :title "Function: "
    :visible-min-width '(character 80)
    :visible-min-height '(character 25)
    :accessor dist-editor
    :change-callback #'dist-editor-body-changed)
   )
  (:layouts
   (main-layout capi:column-layout '(dist-name dist-doc param-layout dist-editor))
   (param-layout capi:column-layout '(param-selector-layout dist-params-default))
   (param-selector-layout capi:row-layout '(dist-params dist-params-add dist-params-remove))
   )
  (:menus
   (file-menu
    "File"
    ((:component
      (("New Distribution"
        :accelerator "accelerator-n"
        :callback 'dist-editor-file-new
        :callback-type :interface)
       ("Edit Distribution..."
        :accelerator "accelerator-o"
        :callback 'dist-editor-file-open
        :callback-type :interface)
       ))
     (:component
      (("Save Distributions"
        :accelerator "accelerator-s"
        :callback 'dist-editor-file-save
        :callback-type :interface)
       ("Close Editor"
        :accelerator "accelerator-w"
        :callback 'dist-editor-file-close
        :callback-type :interface)
       ))
     ))
   (edit-menu
    "Edit"
    ((:component
      (("Reload Distributions"
        :accelerator "accelerator-r"
        :callback 'dist-editor-edit-reload
        :callback-type :interface)
       ))
     ))
   
   )
  (:menu-bar file-menu edit-menu)
  (:default-initargs
   :title "Distribution Editor"
   :confirm-destroy-function 'confirm-destroy-dist-editor
   :destroy-callback 'destroy-dist-editor
   )
)

(let ((counter 0))

(defun new-editor-buffer ()
  (format nil "Distribution Editor Buffer ~A" (incf counter)))

)

(defun dist-type-has-changed (interface)
  (setf (dist-changed? interface) t))

(defun dist-editor-add-param (data interface)
  (declare (ignore data))
  (multiple-value-bind (human-name success-p) (capi:prompt-for-string "Please enter the parameter name:")
    (if (not success-p)
        (return-from dist-editor-add-param))
    (multiple-value-bind (prog-name success-p) (capi:prompt-for-string "Please enter an unbound variable:")
      (if (not success-p)
          (return-from dist-editor-add-param))
      (dist-type-has-changed interface)
      (push (list human-name prog-name 0) (current-params interface))
      (setf (current-param interface) (first (current-params interface)))
      (dist-editor-update-params interface))))

(defun dist-editor-update-current-param (interface)
  (if (null (current-params interface))
      (return-from dist-editor-update-current-param))
  (setf (third (nth (capi:choice-selection (dist-params interface)) (current-params interface))) (read-from-string (get-text (dist-params-default interface)))))

(defun dist-editor-update-params (interface)
  (let* ((params (current-params interface))
         (pane (dist-params interface))
         (choice (ui-selected? pane)))
    (if (null params)
        (progn
          (choice-options pane '("None"))
          (set-option pane "None"))
      (choice-options pane (mapcar #'(lambda (x) (format nil "~A (~A)" (first x) (second x))) params)))
    (enable-pane (dist-params-remove interface) params)
    (enable-pane (dist-params interface) params)
    (if (not (equal choice (ui-selected? pane)))
        (set-text (dist-params-default interface) (format nil "~A" (third (nth (capi:choice-selection pane) params)))))
    (enable-pane (dist-params-default interface) (if params t nil))
))

(defun dist-editor-remove-param (data interface)
  (declare (ignore data))
  (dist-type-has-changed interface)
  (setf (current-params interface) (remove (nth (capi:choice-selection (dist-params interface)) (current-params interface)) (current-params interface)))
  (dist-editor-update-params interface))

(defun dist-editor-param-selected (data interface)
  (declare (ignore data))
  (let ((pane (dist-params interface))
        (value (dist-params-default interface)))
    (setf (third (current-param interface)) (read-from-string (get-text value)))
    (setf (current-param interface) (nth (capi:choice-selection pane) (current-params interface)))
    (set-text value (format nil "~A" (third (current-param interface))))))

(defun dist-editor-text-changed (text pane interface caret)
  (declare (ignore text pane caret))
  (dist-type-has-changed interface))

(defun dist-editor-body-changed (pane point old-length new-length)
  (declare (ignore point old-length new-length))
  (let ((interface (capi:element-interface pane)))
    (dist-type-has-changed interface)))

(defun destroy-dist-editor (interface)
  (editor:buffer-not-modified-command (dist-editor interface) (capi:editor-pane-buffer (dist-editor interface)))
  (editor:kill-buffer-command (capi:editor-pane-buffer (dist-editor interface))))

(defun dist-editor-file-new (interface)
  (if (dist-changed? interface)
      (multiple-value-bind (result success) (capi:prompt-for-confirmation "Would you like to save the current distribution?" :cancel-button t)
        (if (not success)
            (return-from dist-editor-file-new)
          (if result
              (dist-editor-file-save interface)))))
  (setf (current-dist interface) nil)
  (setf (current-params interface) nil)
  (setf (current-param interface) nil)
  (setf (dist-changed? interface) nil)
  (set-text (dist-name interface) "")
  (set-text (dist-doc interface) "")
  (choice-options (dist-params interface) '("None"))
  (set-option (dist-params interface) "None")
  (disable-pane (dist-params interface))
  (disable-pane (dist-params-remove interface))
  (set-text (dist-params-default interface) "")
)

(defun dist-editor-file-open (interface)
  (if (dist-changed? interface)
      (multiple-value-bind (result success) (capi:prompt-for-confirmation "Would you like to save the current distribution?" :cancel-button t)
        (if (not success)
            (return-from dist-editor-file-open)
          (if result
              (dist-editor-file-save interface)))))
  (let* ((name (capi:prompt-with-list (list-all-distributions) "Edit Distribution: "))
         (dist (if (null name) nil (get-distribution-by-typename name))))
    (if (null dist) (return-from dist-editor-file-open))
    (setf (current-dist interface) dist)
    (setf (current-params interface) (mapcar #'(lambda (x y z) (list x y z)) (param-names dist) (param-symbols dist) (param-defaults dist)))
    (setf (current-param interface) (first (current-params interface)))
    (setf (dist-changed? interface) nil)
    (set-text (dist-name interface) (printname dist))
    (set-text (dist-doc interface) (doc-string dist))
    (choice-options (dist-params interface) (if (< 0 (length (current-params interface)))
                                                (mapcar #'(lambda (x) (format nil "~A (~A)" (first x) (second x))) (current-params interface))
                                              '("None")))
    (enable-pane (dist-params interface) (< 0 (length (current-params interface))))
    (enable-pane (dist-params-remove interface) (< 0 (length (current-params interface))))
    (set-text (dist-params-default interface) (if (< 0 (length (current-params interface)))
                                                  (format nil "~A" (third (first (current-params interface))))
                                                ""))
    (enable-pane (dist-params-default interface) (< 0 (length (current-params interface))))
    (set-text (dist-editor interface) (func-string dist))
    )
  (setf (dist-changed? interface) nil)
)

(defun dist-editor-file-save (interface)
  (if (equal "" (get-text (dist-name interface))) (return-from dist-editor-file-save (capi:display-message "You must name the distribution before you can save it.")))
  (if (null (current-dist interface))
      (push (setf (current-dist interface) (make-instance 'distribution-type)) (app-property 'distribution-types)))
  (if (< 0 (length (current-params interface)))
      (setf (third (nth (capi:choice-selection (dist-params interface)) (current-params interface))) (read-from-string (get-text (dist-params-default interface)))))
  (let ((dist (current-dist interface)))
    (setf (printname dist) (get-text (dist-name interface)))
    (setf (symname dist) (dist-name-to-symbol (printname dist)))
    (setf (doc-string dist) (get-text (dist-doc interface)))
    (setf (param-count dist) (length (current-params interface)))
    (setf (param-names dist) (mapcar #'first (current-params interface)))
    (setf (param-symbols dist) (mapcar #'second (current-params interface)))
    (setf (param-defaults dist) (mapcar #'third (current-params interface)))
    (setf (func-string dist) (capi:editor-pane-text (dist-editor interface)))
    (setf (func dist) (read-from-string (format nil "(lambda ; start lambda
~A
~A
) ; end lambda"
                                                (param-symbols dist)
                                                (func-string dist))))
    (setf (symbol-function (symname dist)) (compile nil (func dist)))
    (setf (documentation (symname dist) 'function) (doc-string dist))
    (write-distribution (string-append *PROGRAM-DIRECTORY* "Distributions/~A.spd") dist)
    (setf (dist-changed? interface) nil)
  ))

(defun dist-editor-file-close (interface)
  (if (confirm-destroy-dist-editor interface)
      (capi:destroy interface)
    ))

(defun confirm-destroy-dist-editor (interface)
  (if (dist-changed? interface)
      (multiple-value-bind (result success) (capi:prompt-for-confirmation "Would you like to save the current distribution?" :cancel-button t)
        (if (not success)
            (return-from confirm-destroy-dist-editor nil)
          (if result
              (dist-editor-file-save interface)))))
  t)

(defun dist-editor-edit-reload (interface)
  (if (dist-changed? interface)
      (if (capi:prompt-for-confirmation "Would you like to save the current distribution?")
          (dist-editor-file-save interface)
        (return-from dist-editor-edit-reload)))
  (read-distributions (directory (string-append *PROGRAM-DIRECTORY* "Distributions/*.spd"))))
