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

; Legacy load/save mechanisms

(defmethod load-model ((path string))
  (with-open-file (file path :direction :input :external-format '(:latin-1 :eol-style #+win32 :crlf #-win32 :lf))
    (load-model file)))

(defmethod load-model ((path pathname))
  (with-open-file (file path :direction :input :external-format '(:latin-1 :eol-style #+win32 :crlf #-win32 :lf))
    (load-model file)
))

(defmethod load-model ((stream stream))
  (let ((first-line (explode-tab (read-line stream nil)))
        (constraint-list nil)
        (activity-type-list nil)
        (model nil))

    (cond
     ;; File is not a SANLab model file
     ((or (not first-line) (not (equal (first first-line) "SANLab")))
      (capi:display-message "Error: Invalid model file.")
      (return-from load-model nil))

     ;; Invalid version string
     ((or (< (length first-line) 2) (not (find (second first-line) (app-property 'allowed-versions) :test 'equal)))
      (if
          (not (capi:prompt-for-confirmation
                (format nil "Error: SANLab version ~a is unrecognized. Attempt to continue loading?" (second first-line))))
          (return-from load-model nil))))

    (setf model (make-instance 'model :changed? nil :notes ""))

    (loop
     (let* ((raw-text (read-line stream nil))
            (line-text (explode-tab raw-text)))
       (when (not raw-text) (return))

       (cond
        ((equal (first line-text) "COMMENT")
         ;; Ignore COMMENT lines
         )
        ((equal (first line-text) "MODEL-PROPERTIES")
         (setf (width model) (read-from-string (second line-text)))
         (setf (height model) (read-from-string (third line-text)))
         (setf (grid-size model) (read-from-string (fourth line-text))))
        ((equal (first line-text) "MODEL-TITLE")
         (setf (title model) (second line-text)))
        ((equal (first line-text) "MODEL-AUTHOR")
         (setf (author model) (second line-text)))
        ((equal (first line-text) "BEGIN-MODEL-NOTES")
         (loop
          (let ((notes-line (read-line stream nil)))
            (when (equal (first (explode-tab notes-line)) "END-MODEL-NOTES") (return))
            (setf (notes model) (format nil "~a~a~a" (notes model) notes-line #\Newline))))
         )
        ((equal (first line-text) "BEGIN-NOTE")
          (let ((x (read-from-string (second line-text)))
                (y (read-from-string (third line-text)))
                (value ""))
            (loop
             (let ((notes-line (read-line stream nil)))
               (when (equal (first (explode-tab notes-line)) "END-NOTE")
                 (push (make-instance 'rendered-note :stored-x x :stored-y y :text value) (rendered-notes model))
                 (return)
                 )
               (setf value (format nil "~a~a~a" value notes-line #\Newline)))))
         )
        ((equal (first line-text) "ACTIVITY")
         (let ((new-activity (make-instance 'graph-node)))
           (if (nth 1 line-text) (setf (uid new-activity) (read-from-string (remove #\"  (nth 1 line-text)))))
           (if (nth 2 line-text) (setf (activity-type new-activity) (get-activity-by-typename (nth 2 line-text))))
           (if (nth 3 line-text) (setf (label new-activity) (nth 3 line-text)))
           (if (and (nth 4 line-text)
                    (> (length (nth 4 line-text)) 0)
                    (numberp (read-from-string (nth 4 line-text))))
               (setf (stored-x new-activity) (read-from-string (nth 4 line-text))))
           (if (and (nth 5 line-text)
                    (> (length (nth 5 line-text)) 0)
                    (numberp (read-from-string (nth 5 line-text))))
               (setf (stored-y new-activity) (read-from-string (nth 5 line-text))))
           (if (nth 6 line-text) (setf (color new-activity) (read-from-string (nth 6 line-text))))
           (if (nth 7 line-text) (setf (distribution new-activity) (get-distribution-by-typename (nth 7 line-text))))
           (if (nth 8 line-text) (setf (parameters new-activity) (read-from-string (nth 8 line-text))))
           (if (nth 9 line-text) (setf (ir-type new-activity) (get-iroutine-by-name (read-from-string (nth 9 line-text)))))
           (if (nth 10 line-text) (setf (ir-task new-activity) (get-ir-task-by-id (ir-type new-activity) (read-from-string (nth 10 line-text)))))
           (push new-activity (activities model)))
         )
        ((equal (first line-text) "CONSTRAINT")
         (push (list (second line-text) (third line-text)) constraint-list)
         )

        ((equal (first line-text) "IR-MEMBER")
         (let (act type task append)
           (setf act (find (read-from-string (second line-text)) (activities model) :key #'uid))
           (setf type (get-iroutine-by-name (third line-text)))
           (cond (type
                  (setf task (get-ir-task-by-id type (read-from-string (fourth line-text))))
                  (cond (task
                         (setf append (fifth line-text))
                         (setf (ir-type act) type)
                         (setf (ir-task act) task)
                         (setf (ir-append act) append))
                        (t
                         (capi:display-message "Warning: Invalid IR task identifier ~A for routine ~A." (third line-text) (fourth line-text)))))
                 (t
                  (capi:display-message "Warning: Model refers to non-existent routine ~A." (third line-text)))))
         )
        )
       ))

    (setf (activity-types model) (reverse activity-type-list))
    
    ;; Create the constraints
    (dolist (constraint constraint-list)
      (let ((source (find (first constraint) (activities model)
                          :test #'(lambda (item activity)
                                    (equal (read-from-string item) (uid activity)))))
            (target (find (second constraint) (activities model)
                          :test #'(lambda (item activity)
                                    (equal (read-from-string item) (uid activity))))))
        (if (or (not source)
                (not target))
            (capi:display-message "~a | S: ~a T: ~a" constraint source target))
        (push target (edges-out source))
        (push source (edges-in target))))


    model
    )
  )

(defmethod save-model ((model model) (path string))
  (with-open-file (file path :direction :output :external-format '(:latin-1 :eol-style #+win32 :crlf #-win32 :lf))
    (save-model model file)))

(defmethod save-model ((model model) (path pathname))
  (with-open-file (file path :direction :output :external-format '(:latin-1 :eol-style #+win32 :crlf #-win32 :lf))
    (save-model model file)))

(defmethod save-model ((model model) (stream stream))
  
  (setf (changed? model) nil)

  (format stream "~a~a~a~%~%" "SANLab" #\tab (app-property 'app-version))
  (format stream "COMMENT~aWIDTH~aHEIGHT~aGRID-SIZE~%" #\tab #\tab #\tab)
  (format stream "MODEL-PROPERTIES~a~a~a~a~a~a~%~%"
          #\tab (width model) #\tab (height model) #\tab (grid-size model))
  (format stream "MODEL-TITLE~A~A~%" #\tab (title model))
  (format stream "MODEL-AUTHOR~A~A~%~%" #\tab (author model))
  (format stream "BEGIN-MODEL-NOTES~%~a~%END-MODEL-NOTES~%~%" (notes model))

  (dolist (note (rendered-notes model))
    (format stream "BEGIN-NOTE~a~a~a~a~%~a~%END-NOTE~%~%" #\tab (stored-x note) #\tab (stored-y note) (text note)))

  (format stream "COMMENT~aNAME~aDEFAULT-LABEL~aDISTRIBUTION~aPARAMETERS~aCOLOR~%" #\tab #\tab #\tab #\tab #\tab)

  (format stream "~%COMMENT~aUID~aTYPE~aLABEL~aX~aY~aCOLOR~aDISTRIBUTION~aPARAMETERS~aIR-TYPE~aIR-TASK~%"
          #\tab #\tab #\tab #\tab #\tab #\tab #\tab #\tab #\tab #\tab)

  (dolist (activity (activities model))
    (format stream "ACTIVITY~a~A~a~A~a~A~a~A~a~A~a~S~a~A~a~S~a~S~a~S~%" #\tab
            (uid activity) #\tab
            (get-full-name (let ((x (activity-type activity)))
                        (if (equal 'activity-type (type-of x))
                            x
                          (progn (capi:display-message "~A" (type-of x)) x)))) #\tab
            (label activity) #\tab
            (stored-x activity) #\tab
            (stored-y activity) #\tab
            (color activity) #\tab
            (printname (distribution activity)) #\tab
            (parameters activity) #\tab
            (if (ir-type activity) (name (ir-type activity)) "") #\tab
            (if (ir-task activity) (uid (ir-task activity)) 0)))

  (format stream "~%COMMENT~aSOURCE-ID~aTARGET-ID~%" #\tab #\tab)
  (dolist (activity (activities model))
    (dolist (constraint (edges-out activity))
      (format stream "CONSTRAINT~a~a~a~a~%" #\tab (uid activity) #\tab (uid constraint))))

  (format stream "~%COMMENT~aID~aIROUTINE~aTASK~aAPPEND~%" #\tab #\tab #\tab #\tab)
  (dolist (activity (activities model))
    (if (ir-type activity)
        (format stream "IR-MEMBER~a~a~a~s~a~a~a~s~%" #\tab (uid activity) #\tab (name (ir-type activity)) #\tab (uid (ir-task activity)) #\tab (ir-append activity))))
  
  )

(defmethod irname ((model model))
  (let* ((str (format nil "~A" (filename model)))
         (parts (split-string str #+win32 #\\ #-win32 #\/))
         (temp (first (last parts))))
    (subseq temp 0 (- (length temp) 4))))

(defmethod write-routine-activity ((out stream) (node graph-node) x y)
  (format out "(deftask~%")
  (format out "~S~%" (uid node))
  (format out "~S~%" (label node))
  (format out "~S~%" (get-full-name (activity-type node)))
  (format out "~S~%" (printname (distribution node)))
  (format out "'~S~%" (parameters node))
  (format out "~S ~S~%" (- (stored-x node) x) (- (stored-y node) y))
  (format out "'~S '~S~%" (mapcar #'uid (edges-in node)) (mapcar #'uid (edges-out node)))
  (format out ")~%")
)

(defmethod save-interactive-routine ((out stream) (model model))
  (format out "(defroutine~%")
  (format out "~S~%" (irname model))
  (format out "~S~%" (color model))
  (let ((min-x (reduce 'min (mapcar #'stored-x (activities model))))
        (min-y (reduce 'min (mapcar #'stored-y (activities model)))))
    (dolist (act (activities model))
      (write-routine-activity out act min-x min-y)))
  (format out ")~%")
)