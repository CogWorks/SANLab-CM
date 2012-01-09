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

; This file contains functions which pertain to loading, saving, and executing distribution calculations for SANLab

(defun dist-name-to-symbol (str)
  (read-from-string (format nil "USER-DEFDIST-~A" (spaced-string-to-dashed-string str))))

(defun call-distribution (name &rest rest)
  (let ((symname (dist-name-to-symbol name)))
    (apply symname rest)))

(defun distribution-documentation (name)
  (let ((symname (dist-name-to-symbol name)))
    (documentation symname 'function)))

(defclass distribution-type ()
  ((symname :initform nil
            :initarg :symname
            :accessor symname)
   (printname :initform nil
              :initarg :printname
              :accessor printname)
   (doc-string :initform nil
               :initarg :docstring
               :accessor doc-string)
   (param-count :initform nil
                :initarg :param-count
                :accessor param-count)
   (param-names :initform nil
                :initarg :param-names
                :accessor param-names)
   (param-symbols :initform nil
                  :initarg :param-symbols
                  :accessor param-symbols)
   (param-defaults :initform nil
                :initarg :param-defaults
                :accessor param-defaults)
   (func :initform nil
         :initarg :func
         :accessor func)
   (func-string :initform nil
                :initarg :func-string
                :accessor func-string)))

(defmacro defdist (name doc params defaults func)
  (let ((symname (dist-name-to-symbol name))
        (count (length params))
        (foo (if (symbolp func) (eval func) func)))
    ;(format t "Processing ~A...~%" name)
    `(let ((dt (make-instance 'distribution-type :symname (quote ,symname) :printname ,name :docstring ,doc :param-count ,count :param-names (quote ,params) :param-defaults (quote ,defaults) :func (quote ,foo))))
       (setf (symbol-function (quote ,symname)) (compile nil (quote ,foo)))
       (setf (documentation (quote ,symname) 'function) ,doc)
       (push dt (app-property 'distribution-types))
       dt)))

(defun list-all-distributions ()
  (mapcar #'printname (app-property 'distribution-types)))

(defmethod get-name ((dist distribution-type))
  (dolist (x (app-property 'distribution-types) "Distribution not found")
    (if (eq dist (symname x))
        (return-from get-name (printname x)))))

(defun get-distribution-by-symname (name)
  (dolist (x (app-property 'distribution-types) nil)
    (if (equal name (symname x))
        (return-from get-distribution-by-symname x))))

(defun get-distribution-by-typename (name)
  (dolist (x (app-property 'distribution-types) nil)
    (if (equal name (printname x))
        (return-from get-distribution-by-typename x))))

(defun find-distribution (name)
  (let ((x (get-distribution-by-typename name)))
    (if x
        (make-instance 'distribution-pointer :pointer x)
      nil)))

(defun read-distributions (files)
  (setf (app-property 'distribution-types) nil)
  ;(capi:display-message file)
  (dolist (file files)
    (with-open-file (prefstream file :direction :input :external-format '(:latin-1 :eol-style #+win32 :crlf #-win32 :lf))
      (let ((state "")
            (dist nil)
            (was-lambda nil)
            (in-body nil)
            (body "")
            (param-list nil))
        (do ((cur-line (read-line prefstream) (read-line prefstream nil nil)))
            ((null cur-line) (close prefstream))
          (setq state (format nil "~A~A~%" state cur-line))
          (cond ((equal cur-line ") ; end defdist")
                 (setq dist (eval (read-from-string state)))
                 (setq state "")
                 (setf (param-symbols dist) param-list)
                 (setf (func-string dist) body)
                 (setq body ""))
                ((equal cur-line "(lambda ; start lambda")
                 (setq was-lambda t))
                (was-lambda
                 (setq was-lambda nil)
                 (setq param-list (read-from-string cur-line))
                 (setq in-body t))
                ((equal cur-line ") ; end lambda")
                 (setq in-body nil))
                (in-body
                 (setq body (format nil "~A~A~%" body cur-line))))))))
  (app-property 'distribution-types))

(defun write-distributions (file)
  (with-open-file (stream file :direction :output :if-exists :supersede :external-format '(:latin-1 :eol-style #+win32 :crlf #-win32 :lf))
    (dolist (dist (app-property 'distribution-types))
      (format stream "(defdist ~S~%" (printname dist))
      (format stream "~S~%" (doc-string dist))
      (format stream "~S~%" (param-names dist))
      (format stream "~A~%" (param-defaults dist))
      (format stream "(lambda ; start lambda~%")
      (format stream "~A~%" (param-symbols dist))
      (format stream "~A~%" (func-string dist))
      (format stream ") ; end lambda~%")
      (format stream ") ; end defdist~%~%"))))

(defun write-distribution (file dist)
  (with-open-file (stream (format nil file (printname dist)) :direction :output :if-exists :supersede :external-format '(:latin-1 :eol-style #+win32 :crlf #-win32 :lf))
    (format stream "(defdist ~S~%" (printname dist))
    (format stream "~S~%" (doc-string dist))
    (format stream "~S~%" (param-names dist))
    (format stream "~A~%" (param-defaults dist))
    (format stream "(lambda ; start lambda~%")
    (format stream "~A~%" (param-symbols dist))
    (format stream "~A~%" (func-string dist))
    (format stream ") ; end lambda~%")
    (format stream ") ; end defdist~%~%")))

(defmethod (setf symname) :after (val (dist distribution-type))
  (if (app-property 'current-controller)
      (fire-event-listeners (app-property 'current-controller) 'distribution-symname-changed (list val)))
)

(defmethod (setf printname) :after (val (dist distribution-type))
  (if (app-property 'current-controller)
      (fire-event-listeners (app-property 'current-controller) 'distribution-printname-changed (list val)))
)

(defmethod (setf doc-string) :after (val (dist distribution-type))
  (if (app-property 'current-controller)
      (fire-event-listeners (app-property 'current-controller) 'distribution-doc-string-changed (list val)))
)

(defmethod (setf param-count) :after (val (dist distribution-type))
  (if (app-property 'current-controller)
      (fire-event-listeners (app-property 'current-controller) 'distribution-param-count-changed (list val)))
)

(defmethod (setf param-names) :after (val (dist distribution-type))
  (if (app-property 'current-controller)
      (fire-event-listeners (app-property 'current-controller) 'distribution-param-names-changed (list val)))
)

(defmethod (setf param-symbols) :after (val (dist distribution-type))
  (if (app-property 'current-controller)
      (fire-event-listeners (app-property 'current-controller) 'distribution-param-symbols-changed (list val)))
)

(defmethod (setf param-defaults) :after (val (dist distribution-type))
  (if (app-property 'current-controller)
      (fire-event-listeners (app-property 'current-controller) 'distribution-param-defaults-changed (list val)))
)

(defmethod (setf func-string) :after (val (dist distribution-type))
  (if (app-property 'current-controller)
      (fire-event-listeners (app-property 'current-controller) 'distribution-func-string-changed (list val)))
)

(pass printname distribution-pointer distribution-type :accessor pointer)
(pass get-param-count distribution-pointer distribution-type :accessor pointer :alternate-name param-count)

(defmethod get-param-name ((ptr distribution-pointer) (i fixnum))
  (nth i (param-names (pointer ptr))))
