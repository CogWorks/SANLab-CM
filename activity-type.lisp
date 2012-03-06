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

; Activities

; This macro wraps the creation of an activity type with set parameters.
; Used by sat files to create activity types
(defmacro define-activity-type (typename extra symname parent instname dist params color)
  `(push (make-instance 'activity-type
                        :symname ',symname
                        :parent ',parent
                        :typename ,typename
                        :instname ,instname
                        :extra ,extra
                        :distribution ,dist
                        :default-params ',params
                        :color ,color)
         (app-property 'activity-types)))

; Fire event listeners when the symbolic name of an activity type changes
(defmethod (setf symname) :after (val (type activity-type))
  (let ((current-controller (app-property 'current-controller)))
    (fire-event-listeners current-controller 'activity-type-changed (list type 'symname val))))

; Fire event listeners when the parent type changes
(defmethod (setf parent) :after (val (type activity-type))
  (let ((current-controller (app-property 'current-controller)))
    (fire-event-listeners current-controller 'activity-type-changed (list type 'parent val))))

; Fire event listeners when the activity type name changes
(defmethod (setf typename) :after (val (type activity-type))
  (let ((current-controller (app-property 'current-controller)))
    (fire-event-listeners current-controller 'activity-type-changed (list type 'typename val))))

; Fire event listeners when the default instance name changes
(defmethod (setf instname) :after (val (type activity-type))
  (let ((current-controller (app-property 'current-controller)))
    (fire-event-listeners current-controller 'activity-type-changed (list type 'instname val))))

; Fire event listeners when the extra name changes
(defmethod (setf extra) :after (val (type activity-type))
  (let ((current-controller (app-property 'current-controller)))
    (fire-event-listeners current-controller 'activity-type-changed (list type 'extra val))))

; Fire event listeners when the default distribution changes
(defmethod (setf distribution) :after (val (type activity-type))
  (let ((current-controller (app-property 'current-controller)))
    (fire-event-listeners current-controller 'activity-type-changed (list type 'distribution val))))

; Fire event listeners when the default parameters change
(defmethod (setf default-params) :after (val (type activity-type))
  (let ((current-controller (app-property 'current-controller)))
    (fire-event-listeners current-controller 'activity-type-changed (list type 'default-params val))))

; Fire event listeners when the activity color changes
(defmethod (setf color) :after (val (type activity-type))
  (let ((current-controller (app-property 'current-controller)))
    (fire-event-listeners current-controller 'activity-type-changed (list type 'color val))))
  
; Gets the full text name of given an activity pointer
(defmethod get-full-name ((at activity-pointer))
  (get-full-name (pointer at)))

; Gets the full name of an activity type
(defmethod get-full-name ((at activity-type))
  (let ((typename (typename at))
        (extra (extra at)))
    (if (equalp "" extra)
        typename
      (format nil "~A (~A)" typename extra))))

; Given a symbol, find the activity represented by that symbol in the list
(defun get-activity-by-symname (symname)
  (dolist (x (app-property 'activity-types) nil)
    (if (equalp symname (symname x))
        (return-from get-activity-by-symname x))))

; Given a string, find the activity represented by that string in the list
(defun get-activity-by-typename (typename)
  (dolist (x (app-property 'activity-types) nil)
    (if (equalp typename (get-full-name x))
        (return-from get-activity-by-typename x))))

; Gets the name of an activity type, protects against null pointers
(defmethod name ((at activity-type))
  (if (null at)
      nil
    (symname at)))

; Gets the children of an activity type, used to build hierarchical displays
(defmethod get-children ((at activity-type))
  (let ((name (name at))
        (children nil))
    (dolist (x (app-property 'activity-types) children)
      (if (equalp name (parent x))
          (push x children)))))

; Gets the names of an activity type's children
(defmethod get-children-names ((at activity-type))
  (mapcar #'get-full-name (get-children at)))

; Given either a string or an activity type, get the children of an activity identified by the parameter
(defun children (name)
  (if (equal (type-of name) 'activity-type)
      (sort (get-children-names name) #'string<)
    (sort (get-children-names (get-activity-by-typename name)) #'string<)))

; Makes an activity type from a set of parts
(defun make-activity-type-descriptor (part)
  (if (= 9 (length part))
      (make-instance 'activity-type
                     :symname (read-from-string (second part))
                     :parent (read-from-string (third part))
                     :typename (read-from-string (fourth part))
                     :instname (read-from-string (fifth part))
                     :extra (read-from-string (sixth part))
                     :distribution (read-from-string (seventh part))
                     :default-params (read-from-string (eighth part))
                     :color (read-from-string (ninth part)))
    nil))

; Gets all activities without parent activities
(defun get-root-activities ()
  (let ((roots nil))
    (dolist (x (app-property 'activity-types) (sort roots #'string<))
      (if (eq t (parent x))
          (push x roots)))))

; Lists all operator types
(defun list-all-operators ()
  (cons "None" (mapcar #'get-full-name (app-property 'activity-types))))

; Gets the default distribution for an operator type
(defmethod get-distribution ((at activity-type))
  (if (equalp :same-as-parent (distribution at))
      (get-distribution (get-activity-by-symname (parent at)))
    (distribution at)))

; Gets the default activity parameters
(defmethod get-default-params ((at activity-type))
  (if (equalp :same-as-parent (default-params at))
      (get-default-params (get-activity-by-symname (parent at)))
    (let ((params (copy-list (default-params at))))
      (do ((i 0 (1+ i)))
          ((= i (length params)) params)
        (if (equal (nth i params) :same-as-parent) (setf (nth i params) (get-default-param at i)))))))

; Gets a specific default parameter
(defmethod get-default-param ((at activity-type) num)
  (let ((param (nth num (default-params at))))
    (cond ((equalp :same-as-parent param)
           (get-default-param (get-activity-by-symname (parent at)) num))
          ((listp param)
           (format nil "random~S" param))
          (t
           (format nil "~A" param)))))

; Gets the background color of the activity type
(defmethod get-color ((at activity-type))
  (if (equalp :same-as-parent (color at))
      (get-color (get-activity-by-symname (parent at)))
    (color at)))

; Generates a unique symbol name for an activity type
(defun generate-unique-symbol (typename extra)
  (let* ((dashed-type (spaced-string-to-dashed-string typename))
         (dashed-extra (spaced-string-to-dashed-string extra))
         (dashed-final (if (equalp "" dashed-extra)
                           dashed-type
                         (format nil "~A-~A" dashed-type dashed-extra)))
         (dashed-unique "")
         (symname (read-from-string dashed-final)))
    (if (null (get-activity-by-symname symname))
        (return-from generate-unique-symbol symname))
    (do ((i 1 (1+ i)))
        (nil)
      (setq dashed-unique (format nil "~A-~A" dashed-final i))
      (setq symname (read-from-string dashed-unique))
      (if (null (get-activity-by-symname symname))
          (return-from generate-unique-symbol symname)))))

; Removes an activity type from the system
(defmethod remove-activity ((at activity-type) &optional (prompt-if-children t))
  (if (null at)
      (return-from remove-activity nil))
  (let ((children (get-children at)))
    (cond ((null children)
           (setf (app-property 'activity-types) (remove at (app-property 'activity-types))))
          (t
           (if prompt-if-children
               (if (not (capi:prompt-for-confirmation (format nil "~A has subtypes. Proceeding will remove these subtypes as well.~%Are you sure you want to continue?" (get-full-name at))))
                   (return-from remove-activity nil)))
           (dolist (x children) (remove-activity x nil))
           (setf (app-property 'activity-types) (remove at (app-property 'activity-types)))))))

; Removes an activity type by name
(defun remove-activity-by-typename (act-name &optional (prompt-if-children t))
  (remove-activity (get-activity-by-typename act-name) prompt-if-children))

; Prints out an activity type
(defmethod print-object ((at activity-type) (s stream))
  (format s (get-full-name at)))

; Gets the type name of an activity in a list
(defmethod typename ((x list))
  (typename (car x)))

; Abberviates the name 
(defmethod abbreviated-name ((at activity-pointer))
  (abbreviated-name (pointer at)))

(let ((operator-regex (precompile-regexp " Operator")))
  (declare (ignore operator-regex))
; Abbreviates the name of an operator into all caps
; E.g. "Cognitive Operator" ==> "CO"
; "Perceptual Operator (Visual)" ==> "PO(V)"
(defmethod abbreviated-name ((at activity-type))
  (let* ((name (copy-seq (typename at)))
         (extra (if (extra at) (copy-seq (extra at))))
         (parts nil)
         (final ""))
    (setf parts (mapcar #'(lambda (x) (subseq x 0 1)) (split-string name #\space)))
    (setf final (format nil "~{~A~}" parts))
    (if (or (not extra) (equal extra "")) (return-from abbreviated-name final))
    (setf parts (split-string extra #\space))
    (setf final (format nil "~A (~{~A~})" final (mapcar #'(lambda (x) (subseq x 0 1)) parts)))))
)

(defmethod is-supertype-of? ((c1 activity-type) (c2 activity-type))
  (cond ((eql c1 c2) t)
        ((eq t (parent c2)) nil)
        (t
         (is-supertype-of? c1 (get-activity-by-symname (parent c2))))))

(defmethod most-specific-superclass ((c1 activity-type) (l list))
  (let ((best (get-activity-by-typename "Operator")))
    (loop for type in l do
          (if (is-supertype-of? type c1)
              (if (or (not best) (is-supertype-of? best type))
                  (setf best type)))
          finally (return best))))
