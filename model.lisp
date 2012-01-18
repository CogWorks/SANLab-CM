#|
Copyright Â© 2007-2009 Evan W. Patton

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

(defclass model ()
  ((filename :initform nil :initarg :filename :accessor filename)
   (author :initform nil :initarg :author :accessor author)
   (title :initform nil :initarg :title :accessor title)
   (changed? :initform nil :initarg :changed? :accessor changed?)
   (name :initform nil :initarg :name :accessor name)
   (activity-types :initform (app-property 'builtin-activities) :initarg :activity-types :accessor activity-types)
   (rendered-notes :initform nil :initarg :rendered-notes :accessor rendered-notes)
   (notes :initform (format nil "
Model created on ~A

***************************************
SANLab v~A
***************************************
© 2007-2009 Evan W. Patton (pattoe@rpi.edu)
© 2006, 2007 Chris R. Sims (simsc@rpi.edu)
CogWorks Lab, Rensselaer Polytechnic Institute
http://www.cogsci.rpi.edu/cogworks/
***************************************
" (date-string) (app-property 'app-version))
          :initarg :notes :accessor notes)
   (width :initform (app-property 'default-model-width) :initarg :width :reader width)
   (height :initform (app-property 'default-model-height) :initarg :height :reader height)
   (grid-size :initform (app-property 'default-grid-size) :initarg :grid-size :accessor grid-size)
   (activities :initform nil :initarg :activities :accessor activities)
   (routines :initform nil :initarg :routines :accessor routines)
   (controller :initform nil :initarg :controller :accessor controller)
   (run-number :initform 0 :initarg :run-number :accessor run-number)
   (color :initform #(:RGB 1.0s0 1.0s0 1.0s0) :initarg :color :accessor color)
))

(defclass rendered-note ()
  ((stored-x :initform 0 :initarg :stored-x :accessor stored-x)
   (stored-y :initform 0 :initarg :stored-y :accessor stored-y)
   (text :initform 0 :initarg :text :accessor text)))

(defmethod x-position ((note rendered-note-pointer))
  (stored-x (pointer note)))

(defmethod y-position ((note rendered-note-pointer))
  (stored-y (pointer note)))

(defmethod text ((note rendered-note-pointer))
  (text (pointer note)))

(defclass routine-instance ()
  ((uid :initform nil :initarg :uid :accessor uid)
   (model :initform nil :initarg :model :accessor model)
   (activities :initform nil :initarg :activities :accessor activities)
   (ir-type :initform nil :initarg :ir-type :accessor ir-type)))

(defmethod initialize-instance :after ((self routine-instance) &rest initargs)
)

(defclass graph-node ()
  ((uid :initform (random 100000) :initarg :uid :accessor uid)
   (activity-type :initform nil :initarg :activity-type :accessor activity-type)
   (ir-type :initform nil :initarg :ir-type :accessor ir-type)
   (ir-task :initform nil :initarg :ir-task :accessor ir-task)
   (ir-append :initform nil :initarg :ir-append :accessor ir-append)
   (ir-instance :initform nil :initarg :ir-instance :accessor ir-instance)
   (label :initform nil :initarg :label :accessor label)
   (color :initform nil :initarg :color :accessor color)

   (stored-x :initform 0 :initarg :stored-x :accessor stored-x)
   (stored-y :initform 0 :initarg :stored-y :accessor stored-y)

   (edges-in :initform nil :initarg :edges-in :accessor edges-in)
   (edges-out :initform nil :initarg :edges-out :accessor edges-out)

   (distribution :initform nil :initarg :distribution :accessor distribution)
   (parameters :initform nil :initarg :parameters :accessor parameters)
   (parsed-parameters :initform nil :initarg :parsed-parameters :accessor parsed-parameters)

   (selected? :initform nil :accessor selected?)

   (start-time :initform nil :accessor start-time)
   (end-time :initform nil :accessor end-time)

   (slowest-parent :initform nil :accessor slowest-parent)
   (path-length :initform most-positive-single-float :accessor path-length)
   (sample-duration :initform 0.0 :accessor sample-duration)

   (conditional-mean-start :initform nil :accessor conditional-mean-start)
   (conditional-mean-end :initform nil :accessor conditional-mean-end)

   (mean-start :initform nil :accessor mean-start)
   (mean-end :initform nil :accessor mean-end)

   (on-critical-path? :initform nil :accessor on-critical-path?)
   (depth :initform nil :accessor depth)
))

(defmethod (setf title) :after (val (model model))
  (setf (changed? model) t))

(defmethod (setf author) :after (val (model model))
  (setf (changed? model) t))

(defmethod (setf notes) :after (val (model model))
  (setf (changed? model) t))

(defmethod (setf rendered-notes) :after (val (model model))
  (setf (changed? model) t))

(defmethod (setf selected?) :after (val (node graph-node))
  (fire-event-listeners (app-property 'current-controller) (if val 'activity-selected 'activity-deselected) node))

(defmethod (setf changed?) :after (val (model model))
  (if val (fire-event-listeners (controller model) 'model-changed nil)))

(defmethod x-position ((src instance-pointer))
  (stored-x (pointer src)))

(defmethod y-position ((src instance-pointer))
  (stored-y (pointer src)))

#|
(defmethod on-critical-path? ((src instance-pointer))
  (on-critical-path? (pointer src)))
|#

(defmethod (setf on-critical-path?) :after (val (src graph-node))
;  (debug "Firing event listener 'activity-on-critical-path for ~A...~%" src)
;  (break "Firing event listener 'activity-on-critical-path for ~A...~%" src)
  (fire-event-listeners (app-property 'current-controller) 'activity-on-critical-path (list src val))
)

(defmethod distribution ((src instance-pointer))
  (if (distribution (pointer src))
      (make-instance 'distribution-pointer :pointer (distribution (pointer src)))
    nil
))

(defmethod color ((src instance-pointer))
  (if (equal :same-as-parent (color (pointer src)))
      (get-color (activity-type (pointer src)))
    (color (pointer src))))

(defmethod ir-color ((src graph-node))
  (if (ir-type src)
      (highlight-color (ir-type src))
    nil))

(defmethod (setf activity-types) :after (new-value (model model))
  (fire-event-listeners (controller model) 'activity-type-change (list new-value))
  (setf (changed? model) t)
)

(defmethod (setf filename) :after (new-value (model model))
  (fire-event-listeners (controller model) 'file-change (list new-value))
  (setf (changed? model) t)
)

(defmethod (setf name) :after (new-value (model model))
  (fire-event-listeners (controller model) 'name-change (list new-value))
  (setf (changed? model) t)
)

(defmethod (setf width) (new-value (model model))
  (setf (slot-value model 'width) (max (app-property 'default-model-width) (if (activities model) (+ 200 (reduce #'max (mapcar #'stored-x (activities model)))) 0) new-value))
)

(defmethod (setf height) (new-value (model model))
  (setf (slot-value model 'height) (max (app-property 'default-model-height) (if (activities model) (+ 100 (reduce #'max (mapcar #'stored-y (activities model)))) 0) new-value))
)

(defmethod (setf width) :after (new-value (model model))
  (fire-event-listeners (controller model) 'model-width-change (list new-value))
  (setf (changed? model) t)
)

(defmethod (setf height) :after (new-value (model model))
  (fire-event-listeners (controller model) 'model-height-change (list new-value))
  (setf (changed? model) t)
)

(defmethod (setf activities) :after (new-value (model model))
  (fire-event-listeners (controller model) 'activities-change (list new-value))
  (setf (changed? model) t)
)

(defmethod dependent-tasks ((node instance-pointer))
  (setf node (pointer node))
  (mapcar #'convert-internal-pointer-to-external-pointer (edges-out node))
)

(defmethod (setf activity-type) :after ((type activity-type) (node graph-node))
  (setf (color node) (get-color type))
  (let ((current-controller (app-property 'current-controller)))
    (fire-event-listeners current-controller 'activity-change (list node 'activity-type type))
    (setf (changed? (model current-controller)) t)
))

(defmethod (setf activity-type) ((type activity-pointer) (node graph-node))
  (setf (activity-type node) (pointer type)))

(defmethod (setf ir-type) :after ((type interactive-routine) (node graph-node))
  (let ((current-controller (app-property 'current-controller)))
    (fire-event-listeners current-controller 'activity-change (list node 'ir-type type))
    (setf (changed? (model current-controller)) t)
))

(defmethod (setf ir-task) :after ((task ir-task) (node graph-node))
  (let ((current-controller (app-property 'current-controller)))
    (fire-event-listeners current-controller 'activity-change (list node 'ir-task task))
    (setf (changed? (model current-controller)) t)
))

(defmethod (setf ir-append) :after (str (node graph-node))
  (let ((current-controller (app-property 'current-controller)))
    (fire-event-listeners current-controller 'activity-change (list node 'ir-append str))
    (setf (changed? (model current-controller)) t)
))

(defmethod (setf label) :after (str (node graph-node))
;  (break "Changed activity label")
  (let ((current-controller (app-property 'current-controller)))
    (fire-event-listeners current-controller 'activity-change (list node 'label str))
    (setf (changed? (model current-controller)) t)
))

(defmethod (setf color) :after (color (node graph-node))
;  (break "Changed activity color")
  (let ((current-controller (app-property 'current-controller)))
    (fire-event-listeners current-controller 'activity-change (list node 'color color))
    (setf (changed? (model current-controller)) t)
))

(defmethod (setf stored-x) :after (x (node graph-node))
  (let ((current-controller (app-property 'current-controller)))
    (fire-event-listeners current-controller 'activity-change (list node 'x x))
    (setf (changed? (model current-controller)) t)
))

(defmethod (setf stored-y) :after (y (node graph-node))
  (let ((current-controller (app-property 'current-controller)))
    (fire-event-listeners current-controller 'activity-change (list node 'y y))
    (setf (changed? (model current-controller)) t)
))

(defmethod (setf edges-in) :after (edges (node graph-node))
  (let ((current-controller (app-property 'current-controller)))
    (fire-event-listeners current-controller 'activity-change (list node 'edges-in edges))
    (setf (changed? (model current-controller)) t)
))

(defmethod (setf edges-out) :after (edges (node graph-node))
  (let ((current-controller (app-property 'current-controller)))
    (fire-event-listeners current-controller 'activity-change (list node 'edges-out edges))
    (setf (changed? (model current-controller)) t)
))

(defun adapt-list-to-size (l n)
  (cond ((= (length l) n)
         l)
        ((< (length l) n)
         (let ((l2 (make-list n :initial-element "0")))
           (do ((i 0 (1+ i))
                (x l (cdr x)))
               ((null x) l2)
             (setf (nth i l2) (car x)))))
        ((> (length l) n)
         (let ((l2 l))
           (dotimes (i (1- n))
             (setq l2 (cdr l2)))
           (setf (cdr l2) nil)
           l2))))

(defmethod (setf distribution) :after ((dist distribution-type) (node graph-node))
;  (break "Changed activity distribution")
  (let ((current-controller (app-property 'current-controller)))
    (setf (parameters node) (adapt-list-to-size (parameters node) (param-count dist)))
    (fire-event-listeners current-controller 'activity-change (list node 'distribution dist))
    (setf (changed? (model current-controller)) t)
))

(defmethod (setf parameters) :after (params (node graph-node))
;  (break "Changed activity parameters")
  (let ((current-controller (app-property 'current-controller)))
    (fire-event-listeners current-controller 'activity-change (list node 'parameters params))
    (setf (changed? (model current-controller)) t)
))

(defmethod (setf parameter) (param (index number) (node graph-node))
;  (break "Changed activity parameter")
  (block nil
    (if (or (< index 0) (>= index (length (parameters node)))) (return))
    (setf (nth index (parameters node)) param)
    (fire-event-listeners (app-property 'current-controller) 'activity-change (list node 'parameter index param))
    (setf (changed? (model (app-property 'current-controller))) t)))

(defmethod push-activity ((act graph-node) (model model))
  (dolist (x (activities model))
    (cond ((= (uid x) (uid act))
           (setf (uid act) (random 100000))
           (return-from push-activity (push-activity act model)))))
  (push act (activities model)))

(defmethod process-parameters ((act graph-node) &optional (callback nil))
  (setf (parsed-parameters act) (parse-parameter-string (parameters act)))
  (dolist (param (parsed-parameters act) t)
    (cond ((and (listp param) (= 4 (length param)))
           (setf (cddddr param) (cons 0 nil)))
          ((listp param)
           (if callback (apply callback (list 'error :random-param-mismatch)))
           (return-from process-parameters))
          ((not (numberp param))
           (if callback (apply callback (list 'error :invalid-param-value)))
           (return-from process-parameters)))))
#|
(defmethod get-full-name ((act activity-type))
  (let ((typename (typename act))
        (extra (extra act)))
    (if (equal extra "")
        typename
      (format nil "~A (~A)" typename extra))))

(defun get-activity-by-symname (symname)
  (let ((activities (app-property 'activity-types)))
    (dolist (x activities nil)
      (if (equal symname (symname x))
          (return-from get-activity-by-symname x)))))

(defun get-activity-by-fullname (fullname)
  (let ((activities (app-property 'activity-types)))
    (dolist (x activities nil)
      (if (equal fullname (get-full-name x))
          (return-from get-activity-by-fullname x)))))

(defmethod name ((act activity-type))
  (if (null act)
      nil
    (symname act)))

(defmethod get-children ((act activity-type))
  (let ((name (name act))
        (children nil)
        (activities (app-property 'activity-types)))
    (dolist (x activities (reverse children))
      (if (equal name (parent x))
          (push x children)))))

(defmethod get-children-names ((act activity-type))
  (mapcar #'get-full-name (get-children act)))

(defun children (name)
  (cond ((equal (type-of name) 'activity-type)
         (sort (get-children-names name) #'string<))
        ((equal (type-of name) 'symbol)
         (sort (get-children-names (get-activity-by-symname name)) #'string<))
        (t
         (sort (get-children-names (get-activity-by-fullname name)) #'string<))))

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

(defun get-root-activities ()
  (let ((activities (app-property 'activity-types))
        (roots nil))
    (dolist (x activities (sort roots #'string< :key #'get-full-name))
      (if (eq t (parent x))
          (push x roots)))))

(defun list-all-operators ()
  (cons "None" (mapcar #'get-full-name (app-property 'activity-types))))

(defmethod get-distribution ((act activity-type))
  (if (equal :same-as-parent (distribution act))
      (get-distribution (get-activity-by-symname (parent act)))
    (distribution act)))

(defmethod get-default-params ((act activity-type))
  (if (equal :same-as-parent (default-params act))
      (get-default-params (get-activity-by-symname (parent act)))
    (let ((params (copy-list (default-params act))))
      (do ((i 0 (1+ i)))
          ((= i (length params)) params)
        (if (equal (nth i params) :same-as-parent)
            (setf (nth i params) (get-default-param (get-activity-by-symname (parent act)) i)))))))

(defmethod get-default-param ((act activity-type) num)
  (let ((param (nth num (default-params act))))
    (cond ((equal :same-as-parent param)
           (get-default-param (get-activity-by-symname (parent act)) num))
          (t
           (format nil "~A" param)))))

(defmethod get-color ((act activity-type))
  (if (equal :same-as-parent (color act))
      (get-color (get-activity-by-symname (parent act)))
    (color act)))

(defun generate-unique-symbol (typename extra)
  (let* ((dashed-type (spaced-string-to-dashed-string typename))
         (dashed-extra (spaced-string-to-dashed-string extra))
         (dashed-final (if (equal "" dashed-extra)
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

(defmethod remove-activity-type ((act activity-type))
  (if (null act)
      (return-from remove-activity-type))
  (let ((children (get-children act)))
    (dolist (x children) (remove-activity x))
    (setf (app-property 'activity-type) (remove act (app-property 'activity-type)))))

(defun remove-activity-by-fullname (name)
  (remove-activity (get-activity-by-fullname name)))
|#
(defmethod create-activity-in-model ((model model) (act activity-type) &key (x 0) (y 0) (ir-type nil) (ir-task nil) (ir-append "") (label nil) (color nil) (distribution nil) (params nil))
  (if (null act)
      (error "Model cannot create instance of null activity"))
  (if (null label)
      (setf label (instname act)))
  (if (null color)
      (setf color (get-color act)))
  (if (null distribution)
      (setf distribution (get-distribution act)))
  (if (null params)
      (setf params (get-dist-params act)))
  (let ((node (make-instance 'graph-node :activity-type act :ir-type ir-type :ir-task ir-task :ir-append ir-append :stored-x x :stored-y y :label label :color color :distribution (get-distribution-by-typename distribution) :parameters params)))
    (push-activity node model)
    (fire-event-listeners (controller model) 'new-activity (list node))
;    (inspect node)
    (setf (changed? model) t)
    node)
)

(defmethod delete-activity-in-model ((model model) (node graph-node))
  (dolist (src (edges-in node))
    (setf (edges-out src) (remove node (edges-out src))))
  (dolist (dest (edges-out node))
    (setf (edges-in dest) (remove node (edges-in dest))))
  (setf (activities model) (remove node (activities model)))
  (fire-event-listeners (controller model) 'deleted-activity (list node))
  (setf (changed? model) t)
  t
)

(defmethod move-activity-in-model ((model model) (act graph-node) &key (x 0) (y 0))
  (if (null act)
      (error "Model cannot move null activity"))
  (setf (stored-x act) x)
  (setf (stored-y act) y)
  (fire-event-listeners (controller model) 'moved-activity (list act x y))
  (setf (changed? model) t)
  t
)

(defmethod connect-activities-in-model ((model model) (act1 graph-node) (act2 graph-node) &rest initargs)
  (if (null act1)
      (error "Cannot connect lambda node to activity."))
  (if (null act2)
      (error "Cannot connect activity to lambda node."))
  (push act2 (edges-out act1))
  (push act1 (edges-in act2))
  (fire-event-listeners (controller model) 'connected-activities (list act1 act2))
  (setf (changed? model) t)
  t)

(defmethod disconnect-activities-in-model ((model model) (act1 graph-node) (act2 graph-node))
  (if (or (null act1) (null act2))
      (error "Cannot disconnect null activity."))
  (setf (edges-out act1) (remove act2 (edges-out act1)))
  (setf (edges-in act2) (remove act1 (edges-in act2)))
  (fire-event-listeners (controller model) 'disconnected-activities (list act1 act2))
  (setf (changed? model) t)
  t)

(defmethod open-as-bundle ((path pathname))
  (let ((mdl (read-model-from-bundle path))
        #+win32
        (filename (let ((str (format nil "~A" path)))
                    (if (string-ends-with str "\\Contents\\model.mdl")
                      (merge-pathnames (subseq str 0 (1+ (- (length str) (length "\\Contents\\model.mdl"))))))))
        #-win32
        (filename path)
)
    (if mdl
        (setf (filename mdl) filename))
    mdl))

(defmethod save-as-bundle ((model model) (path pathname) &key (overwrite nil))
  (let ((temp-name (if (ends-with (format nil "~A" path)
                                  #-win32
                                  #\/
                                  #+win32
                                  #\\
                                  ) (format nil "~A" path) #-win32 (format nil "~A/" path) #+win32 (format nil "~A\\" path))))
;    (capi:prompt-with-message temp-name)
    (cond ((find-regexp-in-string #+win32 ".san\\\\" #-win32 ".san/" temp-name)
           (if overwrite (erase-sanlab-bundle temp-name))
           (make-sanlab-bundle temp-name)
           (write-model-to-bundle temp-name model)))
    (setf (filename model) temp-name)))

(defmethod save-as-bundle ((model model) (path string) &key (overwrite nil))
  (save-as-bundle model (pathname path) :overwrite overwrite))

(defclass layout-node ()
  ((graph-node :initform nil :initarg :graph-node :accessor graph-node)
   (new-x :initform nil :initarg :new-x :accessor new-x)
   (new-y :initform nil :initarg :new-y :accessor new-y)
   (activity-type :initform nil :initarg :type :accessor activity-type)
   (edges-in :initform nil :accessor edges-in)
   (edges-out :initform nil :accessor edges-out)
   (depth :initform nil :initarg :depth :accessor depth)))

(defun assign-depths (node &optional (start 0))
  (cond ((depth node)
         (cond ((> start (depth node))
                (setf (depth node) start)
                (dolist (n (edges-out node))
                  (assign-depths n (1+ start))))))
        (t
         (setf (depth node) start)
         (dolist (n (edges-out node))
           (assign-depths n (1+ start))))))

(defun obj-list-min (objs &key (key #'identity))
  (if (null objs) (return-from obj-list-min 0))
  (if (symbolp key) (setf key (symbol-function key)))
  (let ((min (first objs)))
    (dolist (obj objs min)
      (if (< (funcall key obj) (funcall key min))
          (setf min obj)))))

(defun obj-list-max (objs &key (key #'identity))
  (if (null objs) (return-from obj-list-max 0))
  (if (symbolp key) (setf key (symbol-function key)))
  (let ((min (first objs)))
    (dolist (obj objs min)
      (if (> (funcall key obj) (funcall key min))
          (setf min obj)))))

(defmethod best-supertype ((type activity-type) (list list))
  (let ((parent-type (get-activity-by-symname (parent type))))
    (if (member type list)
        type
      (best-supertype parent-type list))))

(defun list-up-til-now (list node)
  (let ((list (copy-list list)))
    (if (eq (first list) node) (return-from list-up-til-now nil))
    (do ((i (cdr list) (cdr i)))
        ((null i) list)
      (if (and (cdr i) (eq (car (cdr i)) node))
          (progn
            (setf (cdr i) nil)
            (return-from list-up-til-now list))))))

(defmethod layout-model ((model model) &key (adjust-width nil))
  (let* ((types (mapcar #'get-activity-by-typename (let ((typelist (app-property 'layout-graph-order)))
                                                     (if (position "Operator" typelist :test #'equal)
                                                         typelist
                                                       (cons "Operator" typelist)))))
         (depth-table (make-hash-table))
         (friend-table (make-hash-table))
         (nodes nil))
    (dotimes (i (length types))
      (setf (gethash (nth i types) depth-table) (* i (app-property 'layout-graph-delta-y))))
    (setf nodes (mapcar #'(lambda (node)
                            (make-instance 'layout-node
                                           :graph-node node
                                           :new-y (cond ((and (app-property 'layout-graph-group-with-supertype) (best-supertype (activity-type node) types))
                                                         (gethash (best-supertype (activity-type node) types) depth-table))
                                                        ((and (not (app-property 'layout-graph-group-with-supertype)) (gethash (activity-type node) depth-table))
                                                         (gethash (activity-type node) depth-table))
                                                        (t
                                                         (gethash (get-activity-by-typename "Operator") depth-table)))
                                           :type (if (app-property 'layout-graph-group-with-supertype)
                                                     (best-supertype (activity-type node) types)
                                                   (if (gethash (activity-type node) depth-table)
                                                       (activity-type node)
                                                     (get-activity-by-typename "Operator")))))
                        (activities  model)))
    (cond (adjust-width
           (dolist (node nodes)
             (setf (gethash (graph-node node) friend-table) node))
           (dolist (node nodes)
             (dolist (edge (edges-in (graph-node node)))
               (push (gethash edge friend-table) (edges-in node)))
             (dolist (edge (edges-out (graph-node node)))
               (push (gethash edge friend-table) (edges-out node))))
           (assign-depths (find-if #'(lambda (x) (null (edges-in x))) nodes))
           (if (find-if #'(lambda (x) (null (depth x))) nodes)
               (error "Multiple starting nodes in graph"))
           (setf nodes (sort nodes #'< :key #'depth))
           (setf (new-x (first nodes)) 0)
           (dolist (node (cdr nodes))
             (let ((max-pos (obj-list-max (edges-in node) :key #'(lambda (x) (+ (new-x x) (app-property 'editor-default-activity-width))))))
               (if (eq (activity-type max-pos) (activity-type node))
                   (setf (new-x node) (+ (new-x max-pos) (app-property 'layout-graph-same-delta-x)))
                 (setf (new-x node) (+ (new-x max-pos) (app-property 'layout-graph-diff-delta-x))))))
           (dolist (type types)
             (let ((items (remove-if-not #'(lambda (x) (eq type (activity-type x))) nodes)))
               (setf items (sort items #'< :key #'(lambda (x)
                                                    (if (member-if #'(lambda (y)
                                                                       (and (< (new-x x) (new-x y))
                                                                            (< (new-x y) (+ (new-x x) (app-property 'layout-graph-same-delta-x)))))
                                                                   items)
                                                        (setf (new-x x) (+ (new-x x) (- (app-property 'layout-graph-same-delta-x) (app-property 'layout-graph-diff-delta-x))))
                                                      (new-x x))))))))
          (t
           (dolist (node nodes)
             (setf (new-x node) (stored-x (graph-node node))))))
    (dolist (node nodes)
      (setf (stored-x (graph-node node)) (new-x node))
      (setf (stored-y (graph-node node)) (new-y node))))
  (let ((max-x (stored-x (first (activities model))))
        (max-y (stored-y (first (activities model)))))
    (dolist (act (activities model))
      (if (< max-x (stored-x act)) (setf max-x (stored-x act)))
      (if (< max-y (stored-y act)) (setf max-y (stored-y act))))
    (setf (width model) (+ max-x 200))
    (setf (height model) (+ max-y 100)))
  t
)
