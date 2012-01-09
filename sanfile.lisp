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

(defun read-activities (files)
  (setf (app-property 'activity-types) nil)
  (dolist (file files)
    (with-open-file (stream file :direction :input :external-format '(:latin-1 :eol-style #+win32 :crlf #-win32 :lf))
      (eval (read stream))))
  (setf (app-property 'activity-types) (reverse (app-property 'activity-types)))
)

(defmethod write-to-stream ((act activity-type) stream)
  (format stream "ACTIVITY-TYPE~C~S~C~S~C~S~C~S~C~S~C~S~C~S~C~S~%" #\tab
          (symname act) #\tab
          (parent act) #\tab
          (typename act) #\tab
          (instname act) #\tab
          (extra act) #\tab
          (distribution act) #\tab
          (default-params act) #\tab
          (color act)))

(defun write-activities (file)
  (let ((prefstream (open file :direction :output :if-exists :supersede :external-format '(:latin-1 :eol-style #+win32 :crlf #-win32 :lf)))
        (activities (app-property 'activity-types)))
    (format prefstream "; Activity types~%")
    (format prefstream "HEADER~CTYPE~CPARENT~CTYPENAME~CINSTNAME~CEXTRA~CDURATION~CPARAMETERS~CCOLOR~%"
            #\tab #\tab #\tab #\tab #\tab #\tab #\tab #\tab)
    (dolist (x activities (close prefstream))
      (write-to-stream x prefstream))))

(defun write-random-state-to-bundle (file state run)
  (multiple-value-bind (second minute hour day month year) (get-decoded-time)
    (declare (ignore second))
    (let ((dest-path (merge-pathnames (format nil "RandomStates/~4,'0D--~2,'0D-~2,'0D ~2,'0D-~2,'0D Run ~A RandomState.lisp" year month day hour minute run) file)))
      (with-open-file (stream dest-path :if-exists :supersede :direction :output :external-format '(:latin-1 :eol-style #+win32 :crlf #-win32 :lf))
        (format stream "~S" state))))
)

(defun list-random-states-in-bundle (filename)
  (let* ((dest-path (merge-pathnames "RandomStates/*.lisp" filename))
         (directories (directory dest-path)))
    (mapcar #'pathname-name directories)))

(defun read-random-state-from-bundle (filename run)
  (let ((dest-path (merge-pathnames (format nil "RandomStates/~A.lisp" run) filename)))
    (with-open-file (stream dest-path :direction :input :external-format '(:latin-1 :eol-style #+win32 :crlf #-win32 :lf))
      (setf *random-state* (read stream)))))

(defun copy-file (src dest)
  (with-open-file (in src :element-type '(unsigned-byte 8) :external-format '(:latin-1 :eol-style #+win32 :crlf #-win32 :lf))
    (with-open-file (out dest :element-type '(unsigned-byte 8)
                         :external-format '(:latin-1 :eol-style #+win32 :crlf #-win32 :lf)
                         :if-exists :supersede
                         :direction :output)
      (loop for byte = (read-byte in nil nil)
            while byte
            do
            (write-byte byte out)))))

(defun get-bundle-name (filename)
  (let* ((str (format nil "~A" filename))
         (len (length str))
         #-win32
         (pos (+ 1 (position #\/ str :from-end t :end (- len 1))))
         #+win32
         (pos (+ 1 (position #\\ str :from-end t :end (- len 1))))
         (new-str (subseq str pos (- len 1))))
    (if (ends-with str #+win32 #\\ #-win32 #\/)
        new-str
      (format nil "~Ar" new-str))
    ))

(defun get-bundle-path (filename)
  (let ((p (format nil "~A" filename)))
  (multiple-value-bind (s l) (find-regexp-in-string ".san" p)
    (pathname (subseq p 0 (+ s l))))))

(defun erase-sanlab-bundle (filename)
  #+win32
  (sys:call-system (list "cmd.exe" "/C" (format nil "rmdir /S /Q ~C~A~C" #\" filename #\")))
  #-win32
  (sys:run-shell-command (format nil "rm -rf ~A" filename))
)

(defun make-sanlab-bundle (filename)
  (let* ((template-path (merge-pathnames "Contents/Resources/Template.san/"
                                         *app-path*))
         (template-contents (merge-pathnames "Contents/"
                                             template-path))
         (template-quicklook (merge-pathnames "QuickLook/"
                                              template-path))
         (template-pkginfo (merge-pathnames "PkgInfo"
                                            template-contents))
         (template-thumbnail (merge-pathnames "Thumbnail.jpg"
                                              template-quicklook))
         (dest-path filename)
         (dest-contents (merge-pathnames "Contents/"
                                         dest-path))
         (dest-quicklook (merge-pathnames "Quicklook/"
                                          dest-path))
         (dest-results (merge-pathnames "Results/"
                                        dest-path))
         (dest-stats (merge-pathnames "Statistics/"
                                      dest-path))
         (dest-states (merge-pathnames "RandomStates/"
                                       dest-path))
         (dest-pkginfo (merge-pathnames "PkgInfo"
                                        dest-contents))
         (dest-thumbnail (merge-pathnames "Thumbnail.jpg"
                                          dest-quicklook)))
    (mapcar #'(lambda (x) (ensure-directories-exist x :verbose nil))
            (list dest-path dest-contents dest-quicklook dest-results dest-stats dest-states))
    (copy-file template-pkginfo dest-pkginfo)
    (copy-file template-thumbnail dest-thumbnail)))

(defun write-model-to-bundle (filename model)
  (let ((dest-path (merge-pathnames "Contents/model.mdl"
                                   filename)))
    (with-open-file (stream dest-path :if-exists :supersede :direction :output :external-format '(:latin-1 :eol-style #+win32 :crlf #-win32 :lf))
      (save-model model stream))))

(defun read-model-from-bundle (filename)
  (let ((dest-path
         (if (string-ends-with (format nil "~A" filename) "Contents\\model.mdl")
             filename
           (merge-pathnames "Contents/model.mdl"
                            filename))))
    (with-open-file (stream dest-path :direction :input :external-format '(:latin-1 :eol-style #+win32 :crlf #-win32 :lf))
      (load-model stream))))

(defun read-trials-from-bundle (filename run)
  (let ((dest-path (merge-pathnames (format nil "Results/~A.xls" run) filename))
        (res nil)
        (temp nil)
        (line nil)
        (new nil))
    (with-open-file (stream dest-path :direction :input :external-format '(:latin-1 :eol-style #+win32 :crlf #-win32 :lf))
      (read-line stream)
      (do ()
          ((not (setq line (read-line stream nil nil))) res)
        (setq temp (read-from-string (format nil "(~A)" line)))
        (setq new (make-trial-result :timestamp (first temp)
                                     :trial (second temp)
                                     :duration (third temp)
                                     :ucpid (fourth temp)
                                     :path (fifth temp)))
        (push new res)))))

(defun list-trials-in-bundle (filename)
  (let* ((dest-path (merge-pathnames "Results/*.xls" filename))
         (directories (directory dest-path)))
    (mapcar #'pathname-name directories)))

(defun write-trials-to-bundle (filename trials run)
  (multiple-value-bind (second minute hour day month year) (get-decoded-time)
    (declare (ignore second))
    (let ((dest-path (merge-pathnames (format nil "Results/~4,'0D-~2,'0D-~2,'0D ~2,'0D-~2,'0D Run ~A Trials.xls" year month day hour minute run) filename)))
      (with-open-file (stream dest-path :if-exists :supersede :direction :output :external-format '(:latin-1 :eol-style #+win32 :crlf #-win32 :lf))
        (format stream "TIMESTAMP~aTRIAL~aDURATION~aCPINDEX~aCRITICAL-PATH~%" #\tab #\tab #\tab #\tab)
        (write-trials trials stream))
      dest-path)))

(defstruct trial-result
  timestamp
  trial
  duration
  ucpid
  path
  model)

(defun write-trials (trials stream)
  (dolist (x trials)
    (cond ((eql (type-of x) 'trial-result)
           (format stream "~a~a~a~a~a~a~a~a~a~%"
                   (trial-result-timestamp x) #\tab
                   (trial-result-trial x) #\tab
                   (trial-result-duration x) #\tab
                   (trial-result-ucpid x) #\tab
                   (mapcar #'uid (trial-result-path x)))))))

(defun write-statistics-to-bundle (filename stats run)
  (multiple-value-bind (second minute hour day month year) (get-decoded-time)
    (declare (ignore second))
    (let ((dest-path (merge-pathnames (format nil "Statistics/~4,'0D-~2,'0D-~2,'0D ~2,'0D-~2,'0D Run ~A Stats.xls" year month day hour minute run) filename)))
      (with-open-file (stream dest-path :if-exists :supersede :direction :output :external-format '(:latin-1 :eol-style #+win32 :crlf #-win32 :lf))
        (format stream "PERCENT~aNUM-TRIALS~aMEAN~aSTDDEV~aCRITICAL-PATH~aDATA~%"
                #\tab #\tab #\tab #\tab #\tab)
        (write-statistics stats stream)))))

(defstruct trial-stats
  percent
  number
  mean
  stdev
  path-data
  data)

(defun write-statistics (stats stream)
  (dolist (x stats)
    (print-list-with-tabs stream (list (format nil "~1,2f" (percent x))
                                       (length (data x))
                                       (mean x)
                                       (stdev (data x) (read-from-string (mean x)))
                                       (mapcar #'uid (path-data x))
                                       (data x)))))
