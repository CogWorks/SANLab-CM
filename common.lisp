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


; Common utilities
(defun get-debug-path ()
  (setf *debug-path*
        (cond ((equal *operating-system* 'macosx) "~/Library/SANLab-CM/debug.log")
              ((equal *operating-system* 'windows)
               (merge-pathnames "SANLab-CM/debug.log" (sys:get-folder-path :appdata))))))

(let ((*handle-warn-on-redefinition* nil))
(defmacro debug (text &rest args)
  `(format *debug-io* ,text ,@args))
)

(defmacro ignore-sanlab-errors (&body forms)
  `(handler-case
       (progn
         ,@forms
         )
     (error (condition) (values nil condition))
     (network-error (condition) (values nil condition)))
)

(defun sanlab-debug (condition hook)
  (declare (ignore hook))
  ;(format t "Hello world!")
  ;(capi:display-message "~A" (type-of condition))
  ;(break)
  (cond ((equal (type-of condition) 'network-error)
         ;(capi:display-message "Received an error ~C~A~C when processing the activity network. The problem occurred while processing activity ~A, which has been selected in the main window." #\" (thrown-error condition) #\" (first (activities condition)))
         (capi:display-message "Abandon ship!!!")
         (let ((controller (app-property 'current-controller)))
           (deselect-all controller))
         (setf (selected? (first (activities condition))) t)
         (abort condition)
         )
        (t
         (capi:display-message "Oops! Something's broken. A log of the incident has been generated.~%Please send ~A to pattoe@rpi.edu" *debug-path*)
         (capi:display-message "~A" *debug-io*)
         (format *debug-io* "~&~%Received condition ~A~%" condition)
         (format *debug-io* "#|## BEGIN FULL BACKTRACE ##|#~%")
         (mp:map-all-processes-backtrace #'(lambda (item) (print item *debug-io*)))
         (format *debug-io* "#|## END FULL BACKTRACE ##|#~%~%")
         (finish-output *debug-io*)
         (abort condition))))

(defun ends-with (str char)
  (= (position char str :from-end t) (1- (length str))))

(defun string-ends-with (str1 str2)
  (if (< (length str1) (length str2)) (return-from string-ends-with nil))
  (string= (subseq str1 (- (length str1) (length str2))) str2))

(defun contains (str1 str2)
  (cond ((< (length str1) (length str2)) nil)
        ((= (length str1) (length str2)) (if (string-equal str1 str2) 0))
        (t
         (let ((len1 (length str1))
               (len2 (length str2)))
           (do ((i 0 (1+ i)))
               ((= i (- (1+ len1) len2)) nil)
             (if (string-equal (subseq str1 i (+ i len2)) str2) (return-from contains i)))))))

(defun get-dir (file)
  (let ((mac-os-x (contains file "SANLab-CM.app/"))
        (windows (contains file "SANLab-CM.exe")))
    (if mac-os-x
        (string-append (subseq file 0 mac-os-x) "SANLab-CM.app/")
      (if windows
          (subseq file 0 windows)
        (directory-namestring (current-pathname))))))
(setf *app-path* (get-dir (lw:lisp-image-name)))

(defun starts-with (str1 str2)
  (if (> (length str2) (length str1))
      nil
    (string= (subseq str1 0 (length str2)) str2)))

(defun is-type-of? (obj type)
  (eq (type-of obj) type))

(defun is-one-of? (obj type-list)
  (dolist (x type-list nil)
    (if (is-type-of? obj x) (return-from is-one-of? x))))

(defun enable-pane (obj &optional (en t))
  (cond ((is-one-of? obj '(capi:push-button capi:check-button capi:radio-button))
         (capi:apply-in-pane-process obj #'(setf capi:button-enabled) en obj))
        ((is-type-of? obj 'capi:option-pane) (capi:apply-in-pane-process obj #'(setf capi:option-pane-enabled) en obj))
        ((is-type-of? obj 'capi:text-input-pane) (capi:apply-in-pane-process obj #'(setf capi:text-input-pane-enabled) en obj))))

(defun disable-pane (obj)
  (enable-pane obj nil))

(defun set-option (obj val)
  (cond ((is-type-of? obj 'capi:option-pane) (capi:apply-in-pane-process obj #'(setf capi:choice-selected-item) val obj))))

(defun choice-options (obj options)
  (cond ((is-type-of? obj 'capi:option-pane) (capi:apply-in-pane-process obj #'(setf capi:collection-items) options obj))))

(defun select-button (obj &optional (check t))
  (cond ((is-type-of? obj 'capi:check-button) (capi:apply-in-pane-process obj #'(setf capi:button-selected) check obj))))

(defun unselect-button (obj)
  (select-button obj nil))

(defun ui-selected? (obj)
  (cond ((is-type-of? obj 'capi:check-button) (capi:button-selected obj))
        ((is-type-of? obj 'capi:option-pane) (capi:choice-selected-item obj))
        ((is-type-of? obj 'capi:tree-view) (capi:choice-selected-item obj))))

(defun set-text (obj text)
  (cond ((is-type-of? obj 'capi:text-input-pane) (capi:apply-in-pane-process obj #'(setf capi:text-input-pane-text) text obj))
        ((is-type-of? obj 'capi:editor-pane) (capi:apply-in-pane-process obj #'(setf capi:editor-pane-text) text obj))
))

(defun get-text (obj)
  (cond ((is-type-of? obj 'capi:text-input-pane) (capi:text-input-pane-text obj))))

(defun set-image (obj img)
  (cond ((is-type-of? obj 'capi:push-button) (capi:apply-in-pane-process obj #'(setf capi:button-image) img obj))))

(defun set-roots (tree roots)
  (cond ((is-type-of? tree 'capi:tree-view) (capi:apply-in-pane-process tree #'(setf capi:tree-view-roots) roots tree))))

(defun make-color-bitmap (color)
  (if (or (not (vectorp color)) (not (equalp :RGB (elt color 0))))
      (return-from make-color-bitmap nil))
  (let ((color-red (floor (* 255 (elt color 1))))
        (color-green (floor (* 255 (elt color 2))))
        (color-blue (floor (* 255 (elt color 3)))))
    (make-instance 'gp:external-image :data (create-bitmap 50 13 color-red color-green color-blue))))

(defun any-selected (args)
  (if (null args)
      nil
    (or (capi:button-selected (car args)) (any-selected (cdr args)))))

(defun is-comment (str)
  (and (stringp str) (equalp #\; (elt str 0))))

(defun is-empty (str)
  (and (stringp str) (equalp "" str)))

(defun spaced-string-to-dashed-string (str)
  (let ((strcpy (format nil "~A" str)))
    (do ((i 0 (1+ i)))
        ((= i (length strcpy)))
      (if (equal #\space (elt strcpy i))
          (setf (elt strcpy i) #\-)))
    strcpy))

(defun create-bitmap (width height r g b)
  (let* ((rgb-width (* width 4))
         (size (+ (* (+ rgb-width (mod (- 4 (mod rgb-width 4)) 4)) height) 54))
         (bitmap (make-array size :element-type '(unsigned-byte 8) :initial-element 0))
         (byte-8-0 (byte 8 0))
         (byte-8-8 (byte 8 8))
         (byte-8-16 (byte 8 16))
         (byte-8-24 (byte 8 24)))
    
    (let ((byte-pos 53))
      (declare (fixnum i byte-pos))
      (dotimes (i (* width height))
        (setf (aref bitmap byte-pos) 0)
        (incf byte-pos)
        (setf (aref bitmap byte-pos) b)
        (incf byte-pos)
        (setf (aref bitmap byte-pos) g)
        (incf byte-pos)
        (setf (aref bitmap byte-pos) r)
        (incf byte-pos)
        )
      )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; BITMAPFILEHEADER
    ;; Set header
    (setf (aref bitmap 0) 66)
    (setf (aref bitmap 1) 77)
    ;; Set file size
    (setf (aref bitmap 2) (ldb byte-8-0 size))
    (setf (aref bitmap 3) (ldb byte-8-8 size))
    (setf (aref bitmap 4) (ldb byte-8-16 size))
    (setf (aref bitmap 5) (ldb byte-8-24 size))
    ;; Set bitmap data offset
    (setf (aref bitmap 10) 54)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; BITMAPINFOHEADER
    ;; Set bitmap header size
    (setf (aref bitmap 14) 40)
    ;; Set bitmap width
    (setf (aref bitmap 18) (ldb byte-8-0 width))
    (setf (aref bitmap 19) (ldb byte-8-8 width))
    (setf (aref bitmap 20) (ldb byte-8-16 width))
    (setf (aref bitmap 21) (ldb byte-8-24 width))
    ;; Set bitmap height
    (setf (aref bitmap 22) (ldb byte-8-0 height))
    (setf (aref bitmap 23) (ldb byte-8-8 height))
    (setf (aref bitmap 24) (ldb byte-8-16 height))
    (setf (aref bitmap 25) (ldb byte-8-24 height))
    ;; Set bitmap planes
    (setf (aref bitmap 26) 1)
    ;; Set bits per pixel
    (setf (aref bitmap 28) 32)
    bitmap
    )
  )

(defun date-string ()
  (multiple-value-bind (second minute hour date month year day daylight-p zone) (get-decoded-time)
    (declare (ignore daylight-p zone day second))
    (format nil "~2,'0D-~2,'0D-~2,'0D  ~2,'0D:~2,'0D" year month date hour minute)))

(defun explode-tab (string)
  (let ((items nil)
        (pos nil))
    (loop
     (when (= (length string) 0) (return))
     (setf pos (position #\tab string))
     (if pos
         (progn (push (subseq string 0 pos) items)
           (setf string (subseq string (1+ pos))))
       (progn (push string items)
         (setf string ""))))
    (reverse items)))

(defun split-string (string &optional (c #\tab))
  (let ((items nil)
        (pos nil))
    (loop
     (when (= (length string) 0) (return))
     (setf pos (position c string))
     (if pos
         (progn (push (subseq string 0 pos) items)
           (setf string (subseq string (1+ pos))))
       (progn (push string items)
         (setf string ""))))
    (reverse items)))

(defun log-info (handle field-list)
  "Takes an open file handle and a list of values to log.
   Values are tab-separated and followed by a newline."
  (cond
   (handle
    (write (get-internal-real-time) :stream handle)
    (dolist (field field-list)
      (write-char #\tab handle)
      (write field :stream handle))
    (write-char #\newline handle))))

(defun list-mean (list)
  (let ((sum 0))
    (dolist (item list)
      (incf sum item))
    (float (/ sum (length list)))))

(defun print-path (path)
  (let ((string ""))
    (dolist (activity path)
      (if (> (length string) 0)
          (setf string (format nil "~a  |  ~a" string (label activity)))
        (setf string (label activity))
        ))
    string))

(defun print-path-tab (path)
  (let ((string ""))
    (dolist (activity path)
      (if (> (length string) 0)
          (setf string (format nil "~a~a~a" string #\tab (label activity)))
        (setf string (label activity))
        ))
    string))

(defun get-positions (positions sequence)
  (let ((results nil))
    (dolist (pos positions)
      (push (nth pos sequence) results))
    (reverse results)))

(defun positions (item sequence)
  (let ((position-list nil)
        (position -1))
    (loop
     (setf position (position item sequence :start (1+ position) :test 'equal))
     (if position
         (push position position-list)
       (return position-list)))))

(defun inspect-model (model)
  (dolist (activity (activities model))
    (format t "~a~a~a~a~a~%" (label activity) #\tab (sample-duration activity) #\tab (path-length activity))))

;(defun generate-sample-times (activities)
;  (dolist (activity activities)
;    (apply (func (distribution activity)) (parsed-parameters activity))))

(defun parse-parameter-string (parameter-string)
  (mapcar #'read-from-string parameter-string))

#|
(defun resize-object (pinboard object text params)
  (multiple-value-bind (parts width height) (get-best-string-wrap pinboard text *min-obj-width* *min-obj-height* :parameters params)
    (setf (text-parts object) parts)
    (setf (capi:pinboard-pane-size object)
          (values width height))
    
    (dolist (arrow (arrows-in object))
      (capi:with-geometry object
        (capi:move-line arrow
                        (first (capi::coords arrow))
                        (second (capi::coords arrow))
                        (+ capi:%x% (/ capi:%width% 2))
                        (+ capi:%y% (/ capi:%height% 2)))))
    (dolist (arrow (arrows-out object))
      (capi:with-geometry object
        (capi:move-line arrow
                        (+ capi:%x% (/ capi:%width% 2))
                        (+ capi:%y% (/ capi:%height% 2))
                        (third (capi::coords arrow))
                        (fourth (capi::coords arrow)))))
    (gp:invalidate-rectangle object)))
|#

(defmacro swap (v1 v2)
  (let ((g (gensym)))
    `(let ((,g ,v1))
       (setf ,v1 ,v2)
       (setf ,v2 ,g))))

(defun bounded (y x z)
  (if (< x z)
      (and (<= x y) (<= y z))
    (and (<= z y) (<= y x))))

(defun intersect-lines (rx1 ry1 rx2 ry2 lx1 ly1 lx2 ly2)
  (if (< rx2 rx1) (swap rx1 rx2))
  (if (< ry2 ry1) (swap ry1 ry2))

  ; Check endpoints inside rectangle
  (if (and (<= rx1 lx1) (<= lx1 rx2)
           (<= ry1 ly1) (<= ly1 ry2))
      (return-from intersect-lines t))

  (if (and (<= rx1 lx2) (<= lx2 rx2)
           (<= ry1 ly2) (<= ly2 ry2))
      (return-from intersect-lines t))

  ; Check for vertical line
  (if (= lx1 lx2)
      (if (and (<= rx1 lx1) (<= lx1 rx2)
               (<= ly1 ry1) (<= ry2 ly2))
          (return-from intersect-lines t)
        (return-from intersect-lines nil)))

  ; Check for horizontal line
  (if (= ly1 ly2)
      (if (and (<= ry1 ly1) (<= ly1 ry2)
               (<= lx1 rx1) (<= rx2 lx2))
          (return-from intersect-lines t)
        (return-from intersect-lines nil)))

  ; Compute m + b
  (let* ((m (/ (- ly2 ly1) (- lx2 lx1)))
         (b (- ly1 (* m lx1)))
         (left (+ b (* m rx1)))
         (right (+ b (* m rx2)))
         (top (/ (- ry1 b) m))
         (bottom (/ (- ry2 b) m)))
    (if (and (<= ry1 left) (<= left ry2)
             (bounded left ly1 ly2))
        (return-from intersect-lines t))
    (if (and (<= ry1 right) (<= right ry2)
             (bounded right ly1 ly2))
        (return-from intersect-lines t))
    (if (and (<= rx1 top) (<= top rx2)
             (bounded top lx1 lx2))
        (return-from intersect-lines t))
    (if (and (<= rx1 bottom) (<= top rx2)
             (bounded bottom lx1 lx2))
        (return-from intersect-lines t))
    nil))

(defun intersect-rectangles (r1-x1 r1-y1 r1-x2 r1-y2 r2-x1 r2-y1 r2-x2 r2-y2)
  (or
   
   ;; Horizontally bisected
   (and (< r1-x1 r2-x1) (>= r1-x2 r2-x2) (> r1-y1 r2-y1) (< r1-y2 r2-y2))
   
   ;; Vertically bisected
   (and (< r1-y1 r2-y1) (> r1-x1 r2-x1) (>= r1-y2 r2-y2) (< r1-x2 r2-x2))
   
   ;; One point is contained
   (gp:inside-rectangle (list r2-x1 r2-y1 r2-x2 r2-y2) r1-x1 r1-y1)
   (gp:inside-rectangle (list r2-x1 r2-y1 r2-x2 r2-y2) r1-x1 r1-y2)
   (gp:inside-rectangle (list r2-x1 r2-y1 r2-x2 r2-y2) r1-x2 r1-y1)
   (gp:inside-rectangle (list r2-x1 r2-y1 r2-x2 r2-y2) r1-x2 r1-y2)
   
   (gp:inside-rectangle (list r1-x1 r1-y1 r1-x2 r1-y2) r2-x1 r2-y1)
   (gp:inside-rectangle (list r1-x1 r1-y1 r1-x2 r1-y2) r2-x1 r2-y2)
   (gp:inside-rectangle (list r1-x1 r1-y1 r1-x2 r1-y2) r2-x2 r2-y1)
   (gp:inside-rectangle (list r1-x1 r1-y1 r1-x2 r1-y2) r2-x2 r2-y2)
   
   )
  )

(defun stdev (data-list mean)
  (let ((sum 0.0))
    (dolist (item data-list)
      (incf sum (expt (- item mean) 2)))
    (setf sum (/ sum (1- (length data-list))))
    (sqrt sum)))

(defun nearest-grid-point (interface x y)
  (if (app-property 'snap-to-grid)
      (values
       (* (round (/ x (grid-size interface))) (grid-size interface))
       (* (round (/ y (grid-size interface))) (grid-size interface)))
    (values x y))
  )

(defun print-list-with-tabs (s l)
  (if (null (cdr l))
      (format s "~A~%" (car l))
    (progn
      (format s "~A~A" (car l) #\tab)
      (print-list-with-tabs s (cdr l)))))

(defun center-point (object)
  (capi:with-geometry object
    (if (or (null capi:%x%)
            (null capi:%y%)
            (null capi:%width%)
            (null capi:%height%))
        (let ((hints (slot-value object 'capi::hint-table))
              (x (gensym)))
          (setf (symbol-plist `,x) hints)
          (values (floor (+ (get `,x :x) (/ (get `,x :width) 2)))
                  (floor (+ (get `,x :y) (/ (get `,x :height) 2)))))
      (values (floor (+ capi:%x% (/ capi:%width% 2)))
              (floor (+ capi:%y% (/ capi:%height% 2)))))))

(defun get-best-string-wrap (port name min-width min-height &key parameters)
  (setq name (string-trim '(#\Space) name))
  (let* ((split-candidates (apply #'vector (sort (remove-duplicates (cons 0 (cons (length name) (apply #'append (mapcar #'(lambda (char) (positions char name)) (list #\space #\+ #\- #\, #\- #\; #\:)))))) #'<)))
         (internal-min-width (- min-width 14))
         (internal-min-height (- min-height 14))
         (cur-width internal-min-width)
         (cur-height internal-min-height)
         (line-height (multiple-value-bind (left top right bottom) (gp:get-string-extent port "Ay") (declare (ignore left right)) (+ (- bottom top) 1)))
         (lines 0)
         (parts nil))
    (if parameters
        (dolist (param parameters)
          (multiple-value-bind (left top right bottom) (gp:get-string-extent port param)
            (declare (ignore top bottom))
            (setq cur-width (max (- right left) cur-width)))
          (incf lines)))
    (dotimes (i (length split-candidates))
      (let ((start (svref split-candidates i))
            (end (if (= i (1- (length split-candidates))) (length name) (svref split-candidates (1+ i)))))
        (multiple-value-bind (left top right bottom) (gp:get-string-extent port (subseq name start end))
          (declare (ignore top bottom))
          (setq cur-width (max (- right left) cur-width)))))
    ;;; Use binary search algorithm to find best split position
    (loop
     (when (= (length split-candidates) 1) (return))
      (let* ((start 0)
             (end (length split-candidates))
             (middle 0)
             (str ""))
        (loop
         (when (or (> start end) (>= start (length split-candidates))) (return))
          (setq middle (floor (+ start (/ (- end start) 2))))
          (setq str (subseq name (svref split-candidates 0) (svref split-candidates middle)))
          ;(format t "Considering ~S~%" str)
          (multiple-value-bind (left top right bottom) (gp:get-string-extent port str)
            (declare (ignore top bottom))
            (cond ((= (- right left) cur-width)
                   (setq start (1+ end)))
                  ((< (- right left) cur-width)
                   (setq start (1+ middle)))
                  ((> (- right left) cur-width)
                   (setq end (1- middle))))))
        (multiple-value-bind (left top right bottom) (gp:get-string-extent port str)
          (declare (ignore top bottom))
          (if (> (- right left) cur-width)
              (progn
                (decf middle)
                (setq str (subseq name (svref split-candidates 0) (svref split-candidates middle))))))
        (push str parts)
        (setq split-candidates (subseq split-candidates middle))
        ;(format t "Split-candidates = ~S~%" split-candidates)
        (incf lines)))
    (dolist (text parts)
      (multiple-value-bind (left top right bottom) (gp:get-string-extent port text)
        (declare (ignore top bottom))
        (setq cur-width (max (- right left) cur-width))))
    (setq cur-width (+ cur-width 14))
    (setq cur-height (+ (* lines line-height) 14))
    (setq cur-width (+ cur-width (- 10 (mod cur-width 10))))
    (setq cur-height (+ cur-height (- 10 (mod cur-height 10))))
    (values (reverse parts) (+ cur-width 16) (+ (* lines line-height) 14))))

(defmacro pass (fname symname newsymname &key (accessor nil) (alternate-name nil))
  (if (null alternate-name)
      (setf alternate-name fname))
  (if (null accessor)
      (setf accessor newsymname))
  `(defmethod ,fname ((x ,symname))
     (,alternate-name (,accessor x))))

(defmacro pass-setf (fname symname newsymname &key (accessor nil) (alternate-name nil))
  (if (null alternate-name)
      (setf alternate-name fname))
  (if (null accessor)
      (setf accessor newsymname))
  `(defmethod (setf ,fname) (val (x ,symname))
     (setf (,alternate-name (,accessor x)) val)))

(defmacro pass-all (fname symname newsymname &key (accessor nil) (alternate-name nil))
  `(progn
     (pass ,fname ,symname ,newsymname :accessor ,accessor :alternate-name ,alternate-name)
     (pass-setf ,fname ,symname ,newsymname :accessor ,accessor :alternate-name ,alternate-name)))

(defun non-zero-random (lim)
  (let ((x (random lim)))
    (loop
     (when (/= x 0.0) (return x))
     (setq x (random lim)))))

(defun compute-variance (set mean)
  (* (/ 1 (- (length set) 1))
     (reduce #'+ (mapcar #'(lambda (x) (let ((val (- x mean))) (* val val))) set))))

(defun t-test (set1 set2)
  (let* ((len1 (length set1))
         (len2 (length set2))
         (mean1 (/ (reduce #'+ set1) len1))
         (mean2 (/ (reduce #'+ set2) len2))
         (variance1 (compute-variance set1 mean1))
         (variance2 (compute-variance set2 mean2)))
    (values (float (abs (/ (- mean1 mean2)
                           (* (sqrt (+ (/ 1 len1) (/ 1 len2)))
                              (sqrt (/ (+ (* (- len1 1) variance1)
                                          (* (- len2 1) variance2))
                                       (+ len1 len2 -2)))))))
            (+ len1 len2 -2))))

(defconstant *t-critical-table*
  '((1 6.314)
    (2 2.920)
    (3 2.353)
    (4 2.132)
    (5 2.015)
    (6 1.943)
    (7 1.895)
    (8 1.860)
    (9 1.833)
    (10 1.812)
    (11 1.796)
    (12 1.782)
    (13 1.771)
    (14 1.761)
    (15 1.753)
    (16 1.746)
    (17 1.740)
    (18 1.734)
    (19 1.729)
    (20 1.725)
    (21 1.721)
    (22 1.717)
    (23 1.714)
    (24 1.711)
    (25 1.708)
    (26 1.706)
    (27 1.703)
    (28 1.701)
    (29 1.699)
    (30 1.697)
    (40 1.684)
    (50 1.676)
    (60 1.671)
    (80 1.664)
    (100 1.660)
    (120 1.658)
    (inf 1.645)))

(defun t-critical (v)
  (dolist (x *t-critical-table*)
    (if (eql 'inf (first x)) (return-from t-critical (second x)))
    (if (<= v (first x)) (return-from t-critical (second x))))
  1.645
)

(defun different-sets? (set1 set2)
  (multiple-value-bind (t-val df) (t-test set1 set2)
    (> t-val (t-critical df))))


#+cocoa
(defmethod set-cocoa-pathname ((self capi:interface) (path string))
  (objc:invoke (slot-value (capi-internals:representation self) 'capi-cocoa-library::window)
               "setRepresentedFilename:"
               path))

#+cocoa
(defmethod set-cocoa-pathname ((self capi:interface) path)
  (objc:invoke (slot-value (capi-internals:representation self) 'capi-cocoa-library::window)
               "setRepresentedFilename:"
               ""))

#+cocoa
(defmethod set-cocoa-pathname ((self capi:interface) (path pathname))
  (objc:invoke (slot-value (capi-internals:representation self) 'capi-cocoa-library::window)
               "setRepresentedFilename:"
               (format nil "~A" path)))

(defun prompt-with-text-area (prompt &key (default ""))
  (capi:popup-confirmer (make-instance 'capi:multi-line-text-input-pane :text default :visible-min-width 300) prompt :value-function #'capi:text-input-pane-text))

(defun standard-text-pane-editing-callback (pane type)
  (return-from standard-text-pane-editing-callback)
  (case type
    (:start
     (setf (current-textbox (app-property 'current-controller)) pane)
     )
    (:end
     (setf (current-textbox (app-property 'current-controller)) nil)
     )))

(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos)))
