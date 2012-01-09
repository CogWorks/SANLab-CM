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

(defclass sanlab-action ()
  ((type :initform nil :initarg :type :accessor item-type)
   (can-undo :initform t :initarg :can-undo :reader can-undo?)
   (confirm-action :initform nil :initarg :confirm-action :reader confirm-action?)
)
  (:documentation "The sanlab-action class defines the superclass of all actions the user can take to interact with the model. These are recorded by the controller to provide undo/redo support.")
)

; Adds an undo item to the list of undo items
(defmethod add-undo-item ((action sanlab-action))
  (let ((current-controller (app-property 'current-controller)))
    (push action (undo-history current-controller))))

; Adds a redo item to the list of redo items
(defmethod add-redo-item ((action sanlab-action))
  (let ((current-controller (app-property 'current-controller)))
    (push action (redo-history current-controller))))

; Undoes a history item and moves it to the redo stack
(defmethod undo-history-item ((controller controller))
  (let ((item (pop (undo-history controller))))
    (undo-action item controller)
    (add-redo-item item)))

; Redoes a history item and moves it to the undo stack
(defmethod redo-history-item ((controller controller))
  (ignore-errors
  (let ((item (pop (redo-history controller))))
    (perform-action item controller)
    (add-undo-item item))))

; Clears all items in the redo history (when saving, opening, or creating a model)
(defmethod clear-redo-history ((controller controller))
  (setf (redo-history controller) nil))

; Clears all items in the undo history (when saving, opening, or creating a model)
(defmethod clear-undo-history ((controller controller))
  (setf (undo-history controller) nil))

; Executes a set of sanlab action items in one go
(defmethod execute-sanlab-actions (controller actions)
  (block execute-sanlab-actions
    (dolist (act actions t)
      (if (not (perform-action act controller))
          (return-from execute-sanlab-actions nil)))))

; Undoes a set of actions in one go
(defmethod unexecute-sanlab-actions (controller actions)
  (block unexecute-sanlab-actions
    (dolist (act (reverse actions) t)
      (if (not (undo-action act controller))
          (return-from unexecute-sanlab-actions nil)))))

(defclass create-activity-in-model-action (sanlab-action)
  ((initargs :initform nil :initarg :initargs :accessor initargs)
   (created-activity :initform nil :accessor created-activity))
  (:default-initargs
   :type 'create-activity-in-model-action)
  (:documentation "This action handles the creation of a new activity node within the model.")
)

; Create the activity node if it does not already exist and add it to the model
(defmethod perform-action ((action create-activity-in-model-action) (controller controller))
  (if (created-activity action)
      (and (push (created-activity action) (activities (model controller)))
           (fire-event-listeners controller 'new-activity (list (created-activity action))))
    (setf (created-activity action) (apply #'create-activity-in-model (model controller) (initargs action))))
)

; Undoes the creation of an activity node by removing it from the graph
(defmethod undo-action ((action create-activity-in-model-action) (controller controller))
  (setf (activities (model controller)) (remove (created-activity action) (activities (model controller))))
  (fire-event-listeners controller 'deleted-activity (list (created-activity action)))
)

(defclass connect-activities-in-model-action (sanlab-action)
  ((act1 :initform nil :initarg :act1 :accessor act1)
   (act2 :initform nil :initarg :act2 :accessor act2))
  (:default-initargs
   :type 'connect-activities-in-model-action)
  (:documentation "This action establishes a connection between two activity nodes in the model.")
)

(defmethod initialize-instance :after ((action connect-activities-in-model-action) &rest initargs)
  (if (instance-pointerp (act1 action))
      (setf (act1 action) (pointer action)))
  (if (instance-pointerp (act2 action))
      (setf (act2 action) (pointer action)))
)

(defmethod perform-action ((action connect-activities-in-model-action) (controller controller))
  (if (null (act1 action))
      (error "Cannot connect lambda node to activity."))
  (if (null (act2 action))
      (error "Cannot connect activity to lambda node."))
  (push (act1 action) (edges-in (act2 action)))
  (push (act2 action) (edges-out (act1 action)))
  (fire-event-listeners controller 'connected-activities (list (act1 action) (act2 action)))
  ;(setf (changed? model) t)
)

(defmethod undo-action ((action connect-activities-in-model-action) (controller controller))
  (setf (edges-in (act2 action)) (remove (act1 action) (edges-in (act2 action))))
  (setf (edges-out (act1 action)) (remove (act2 action) (edges-out (act1 action))))
  (fire-event-listeners controller 'disconnected-activities (list (act1 action) (act2 action)))
  ;(setf (changed? model) t)
)

(defclass disconnect-activities-in-model-action (sanlab-action)
  ((act1 :initform nil :initarg :act1 :accessor act1)
   (act2 :initform nil :initarg :act2 :accessor act2))
  (:default-initargs
   :type 'disconnect-activities-in-model-action)
)

(defmethod initialize-instance :after ((action disconnect-activities-in-model-action) &rest initargs)
  (if (instance-pointerp (act1 action))
      (setf (act1 action) (pointer action)))
  (if (instance-pointerp (act2 action))
      (setf (act2 action) (pointer action)))
)

(defmethod perform-action ((action disconnect-activities-in-model-action) (controller controller))
  (setf (edges-in (act2 action)) (remove (act1 action) (edges-in (act2 action))))
  (setf (edges-out (act1 action)) (remove (act2 action) (edges-out (act1 action))))
  (fire-event-listeners controller 'disconnected-activities (list (act1 action) (act2 action)))
  ;(setf (changed? model) t)
)

(defmethod undo-action ((action disconnect-activities-in-model-action) (controller controller))
  (push (act1 action) (edges-in (act2 action)))
  (push (act2 action) (edges-out (act1 action)))
  (fire-event-listeners controller 'connected-activities (list (act1 action) (act2 action)))
  ;(setf (changed? model) t)
)

(defclass delete-activity-from-model-action (sanlab-action)
  ((extra-actions :initform nil :accessor extra-actions)
   (deleted-activity :initform nil :initarg :activity :accessor deleted-activity))
  (:default-initargs
   :type 'delete-activity-from-model-action)
)

(defmethod initialize-instance :after ((action delete-activity-from-model-action) &rest initargs)
  (if (instance-pointerp (deleted-activity action))
      (setf (deleted-activity action) (pointer (deleted-activity action)))))

(defmethod perform-action ((action delete-activity-from-model-action) (controller controller))
  (cond ((not (extra-actions action))
         (cond ((ir-instance (deleted-activity action))
                (dolist (act (activities (ir-instance (deleted-activity action))))
                  (if (not (equal (deleted-activity action) act))
                      (push (make-instance 'remove-interactive-routine-information-from-activity
                                           :initargs (list :activity act))
                            (extra-actions action))))))
         (dolist (act (edges-out (deleted-activity action)))
           (push (make-instance 'disconnect-activities-in-model-action
                                :act1 (deleted-activity action) :act2 act)
                 (extra-actions action)))
         (dolist (act (edges-in (deleted-activity action)))
           (push (make-instance 'disconnect-activities-in-model-action
                                :act1 act :act2 (deleted-activity action))
                 (extra-actions action)))))
  (execute-sanlab-actions controller (extra-actions action))
  (setf (activities (model controller)) (remove (deleted-activity action) (activities (model controller))))
  (fire-event-listeners controller 'deleted-activity (list (deleted-activity action)))
)

(defmethod undo-action ((action delete-activity-from-model-action) (controller controller))
  (push (deleted-activity action) (activities (model controller)))
  (unexecute-sanlab-actions controller (extra-actions action))
  (fire-event-listeners controller 'new-activity (list (deleted-activity action)))
)

(defclass instantiate-interactive-routine-in-model-action (sanlab-action)
  ()
  (:default-initargs
   :type 'instantiate-interactive-routine-in-model-action)
)

(defmethod perform-action ((action instantiate-interactive-routine-in-model-action) (controller controller))
)

(defmethod undo-action ((action instantiate-interactive-routine-in-model-action) (controller controller))
)

(defclass remove-instantiated-interactive-routine-from-model-action (sanlab-action)
  ((routine-instance :initform nil :initarg :routine-instance :accessor routine-instance)
   (extra-actions :initform nil :accessor extra-actions))
  (:default-initargs
   :type 'remove-instantiated-interactive-routine-from-model-action)
)

(defmethod initialize-instance :after ((action remove-instantiated-interactive-routine-from-model-action) &rest initargs)
  (if (interactive-routine-instance-pointerp (routine-instance action))
      (setf (routine-instance action) (pointer (routine-instance action))))
)

(defmethod perform-action ((action remove-instantiated-interactive-routine-from-model-action) (controller controller))
  (cond ((null (extra-actions action))
         (dolist (act (activities (routine-instance action)))
           (dolist (edge (edges-in act))
             (push (make-instance 'disconnect-activities :act1 edge :act2 act)
                   (extra-actions action)))
           (dolist (edge (edges-out act))
             (push (make-instance 'disconnect-activities :act1 act :act2 edge)
                   (extra-actions action))))
         (dolist (act (activities (routine-instance action)))
           (push (make-instance 'delete-activity-from-model-action :activity act)
                 (extra-actions action)))
         (setf (extra-actions action) (reverse (extra-actions action)))))
  (execute-sanlab-actions controller (extra-actions action))
)

(defmethod undo-action ((action remove-instantiated-interactive-routine-from-model-action) (controller controller))
  (unexecute-sanlab-actions controller (extra-actions action))
)

(defclass remove-interactive-routine-information-for-activity-action (sanlab-action)
  ((activity :initform nil :initarg :activity :accessor activity)
   (ir-type :initform nil :accessor ir-type)
   (ir-task :initform nil :accessor ir-task)
   (ir-append :initform nil :accessor ir-append)
   (ir-instance :initform nil :accessor ir-instance))
  (:default-initargs
   :type 'remove-interactive-routine-information-for-activity-action)
)

(defmethod initialize-instance :after ((action remove-interactive-routine-information-for-activity-action) &rest initargs)
  (if (instance-pointerp (activity action))
      (setf (activity action) (pointer action)))
)

(defmethod perform-action ((action remove-interactive-routine-information-for-activity-action) (controller controller))
  (cond ((not (ir-type action))
         (setf (ir-type action) (ir-type (activity action)))
         (setf (ir-task action) (ir-task (activity action)))
         (setf (ir-append action) (ir-append (activity action)))
         (setf (ir-instance action) (ir-instance (activity action)))))
  (setf (ir-type (activity action)) nil)
  (setf (ir-task (activity action)) nil)
  (setf (ir-append (activity action)) nil)
  (setf (ir-instance (activity action)) nil)
  t
)

(defmethod undo-action ((action remove-interactive-routine-information-for-activity-action) (controller controller))
  (setf (ir-type (activity action)) (ir-type action))
  (setf (ir-task (activity action)) (ir-task action))
  (setf (ir-append (activity action)) (ir-append action))
  (setf (ir-instance (activity action)) (ir-instance action))
  t
)

(defclass set-activity-property-in-model-action (sanlab-action)
  ((activity :initform nil :initarg :activity :accessor activity)
   (property :initform nil :initarg :property :accessor property)
   (new-value :initform nil :initarg :new-value :accessor new-value)
   (old-value :initform nil :accessor old-value))
  (:default-initargs
   :type 'set-activity-property-in-model-action)
)

(defmethod initialize-instance :after ((action set-activity-property-in-model-action) &rest initargs)
  (if (instance-pointerp (activity action))
      (setf (activity action) (pointer (activity action))))
  (if (opaque-pointerp (new-value action))
      (setf (new-value action) (pointer (new-value action))))
)

(defmethod perform-action ((action set-activity-property-in-model-action) (controller controller))
  (cond ((not (old-value action))
         (let ((p (property action))
               (act (activity action)))
           (setf (old-value action) (eval `(,p ,act))))))
  (let ((p (property action))
        (act (activity action))
        (val (new-value action)))
    (eval `(setf (,p ,act) ,val)))
  t
)

(defmethod undo-action ((action set-activity-property-in-model-action) (controller controller))
  (let ((p (property action))
        (act (activity action))
        (val (old-value action)))
    (eval `(setf (,p ,act) ,val)))
  t
)

(defclass set-property-for-activities-in-model-action (sanlab-action)
  ((activities :initform nil :initarg :activities :accessor activities)
   (property :initform nil :initarg :property :accessor property)
   (new-value :initform nil :initarg :new-value :accessor new-value)
   (operations :initform nil :accessor operations))
  (:default-initargs
   :type 'set-property-for-activities-in-model-action)
)

(defmethod initialize-instance :after ((action set-property-for-activities-in-model-action) &rest initargs)
  (setf (operations action) (mapcar #'(lambda (x) (make-instance 'set-activity-property-in-model-action :activity x :property (property action) :new-value (new-value action))) (activities action)))
)

(defmethod perform-action ((action set-property-for-activities-in-model-action) (controller controller))
  (dolist (op (operations action))
    (perform-action op controller))
  t
)

(defmethod undo-action ((action set-property-for-activities-in-model-action) (controller controller))
  (dolist (op (reverse (operations action)))
    (undo-action op controller))
  t
)

(defclass set-parameter-for-activities-in-model-action (sanlab-action)
  ((activities :initform nil :initarg :activities :accessor activities)
   (parameter :initform nil :initarg :parameter-value :accessor parameter#)
   (new-value :initform nil :initarg :new-value :accessor new-value)
   (old-values :initform nil :accessor old-values))
  (:default-initargs
   :type 'set-parameter-for-activities-in-model-action))

(defmethod initialize-instance :after ((action set-parameter-for-activities-in-model-action) &rest initargs)
  (setf (old-values action) (mapcar #'(lambda (x)
                                        (if (>= (parameter# action) (length (parameters x)))
                                            nil
                                          (nth (parameter# action) (parameters x))))
                                    (activities action)))
)

(defmethod perform-action ((action set-parameter-for-activities-in-model-action) (controller controller))
  (mapcar #'(lambda (x)
              (setf (parameter (parameter# action) x) (new-value action)))
          (activities action))
  t)

(defmethod undo-action ((action set-parameter-for-activities-in-model-action) (controller controller))
  (mapcar #'(lambda (x y)
              (setf (parameter (parameter# action) x) y))
          (activities action)
          (old-values action))
  t)

(defclass move-activity-in-model-action (sanlab-action)
  ((old-pos :initform nil :accessor old-pos)
   (activity :initform nil :initarg :activity :accessor activity)
   (new-pos :initform nil :initarg :new-position :accessor new-pos))
  (:default-initargs
   :type 'move-activity-in-model-action)
)

(defmethod initialize-instance :after ((action move-activity-in-model-action) &rest initargs)
  (if (instance-pointerp (activity action))
      (setf (activity action) (pointer (activity action)))))

(defmethod perform-action ((action move-activity-in-model-action) (controller controller))
  (cond ((not (old-pos action))
         (setf (old-pos action) (list (stored-x (activity action)) (stored-y (activity action))))))
  (setf (stored-x (activity action)) (first (new-pos action)))
  (setf (stored-y (activity action)) (second (new-pos action)))
  (fire-event-listeners controller 'moved-activity (list (activity action) (first (new-pos action)) (second (new-pos action))))
  ;(setf (changed? model) t)
  t
)

(defmethod undo-action ((action move-activity-in-model-action) (controller controller))
  (setf (stored-x (activity action)) (first (old-pos action)))
  (setf (stored-y (activity action)) (second (old-pos action)))
  (fire-event-listeners controller 'moved-activity (list (activity action) (first (old-pos action)) (second (old-pos action))))
  ;(setf (changed? model) t)
  t
)

(defclass move-activities-in-model-action (sanlab-action)
  ((extra-commands :initform nil :accessor extra-commands)
   (activities :initform nil :initarg :activities :accessor activities)
   (positions :initform nil :initarg :positions :accessor new-positions))
  (:default-initargs
   :type 'move-activities-in-model-action))

(defmethod initialize-instance :after ((action move-activities-in-model-action) &rest initargs)
  (setf (extra-commands action) (mapcar #'(lambda (x y) (make-instance 'move-activity-in-model-action :activity x :new-position y))
                                        (activities action)
                                        (new-positions action)))
)

(defmethod perform-action ((action move-activities-in-model-action) (controller controller))
  (dolist (c (extra-commands action))
    (perform-action c controller))
  (fire-event-listeners controller 'moved-activities (activities action))
  t)

(defmethod undo-action ((action move-activities-in-model-action) (controller controller))
  (dolist (c (extra-commands action))
    (undo-action c controller))
  (fire-event-listeners controller 'moved-activities (activities action))
  t)

(defclass layout-graph-action (sanlab-action)
  ((old-positions :initform nil :accessor old-positions)
   (activity-order :initform nil :initarg :activity-order :accessor activity-order)
   (only-vertical :initform nil :initarg :only-vertical :accessor only-vertical))
  (:default-initargs
   :type 'layout-graph-action))

(defmethod perform-action ((action layout-graph-action) (controller controller))
  (cond ((not (old-positions action))
         (dolist (act (activities (model controller)))
           (push (list act (stored-x act) (stored-y act)) (old-positions action)))))
  (layout-model (model controller) (not (only-vertical action)))
  (fire-event-listeners controller 'laid-out-model nil)
)

(defmethod undo-action ((action layout-graph-action) (controller controller))
  (dolist (pos (old-positions action))
    (setf (stored-x (first pos)) (second pos))
    (setf (stored-y (first pos)) (third pos)))
  (fire-event-listeners controller 'laid-out-model nil)
)

(defclass set-note-text-action (sanlab-action)
  ((old-text :initform nil :accessor old-text)
   (note :initform nil :initarg :note :accessor note)
   (new-text :initform nil :initarg :text :accessor new-text))
  (:default-initargs
   :type 'set-note-text-action))

(defmethod initialize-instance :after ((action set-note-text-action) &rest initargs)
  (if (opaque-pointerp (note action))
      (setf (note action) (pointer (note action))))
)

(defmethod perform-action ((action set-note-text-action) (controller controller))
  (cond ((not (old-text action))
         (setf (old-text action) (text (note action)))))
  (setf (text (note action)) (new-text action)))

(defmethod undo-action ((action set-note-text-action) (controller controller))
  (setf (text (note action)) (old-text action))
)

(defclass set-model-title-action (sanlab-action)
  ((old-title :initform nil :accessor old-title)
   (new-title :initform nil :initarg :title :accessor new-title))
  (:default-initargs
   :type 'set-model-title-action))

(defmethod perform-action ((action set-model-title-action) (controller controller))
  (cond ((not (old-title action))
         (setf (old-title action) (title (model controller)))))
  (setf (title (model controller)) (new-title action)))

(defmethod undo-action ((action set-model-title-action) (controller controller))
  (setf (title (model controller)) (old-title action)))

(defclass set-model-author-action (sanlab-action)
  ((old-author :initform nil :accessor old-author)
   (new-author :initform nil :initarg :author :accessor new-author))
  (:default-initargs
   :type 'set-model-author-action))

(defmethod perform-action ((action set-model-author-action) (controller controller))
  (cond ((not (old-author action))
         (setf (old-author action) (author (model controller)))))
  (setf (author (model controller)) (new-author action)))

(defmethod undo-action ((action set-model-author-action) (controller controller))
  (setf (author (model controller)) (old-author action)))

(defclass set-model-width-action (sanlab-action)
  ((old-width :initform nil :accessor old-width)
   (new-width :initform nil :initarg :width :accessor new-width))
  (:default-initargs
   :type 'set-model-width-action))

(defmethod perform-action ((action set-model-width-action) (controller controller))
  (cond ((not (old-width action))
         (setf (old-width action) (width (model controller)))))
  (setf (width (model controller)) (new-width action)))

(defmethod undo-action ((action set-model-width-action) (controller controller))
  (setf (width (model controller)) (old-width action)))

(defclass set-model-height-action (sanlab-action)
  ((old-height :initform nil :accessor old-height)
   (new-height :initform nil :initarg :height :accessor new-height))
  (:default-initargs
   :type 'set-model-height-action))

(defmethod perform-action ((action set-model-height-action) (controller controller))
  (cond ((not (old-height action))
         (setf (old-height action) (height (model controller)))))
  (setf (height (model controller)) (new-height action)))

(defmethod undo-action ((action set-model-height-action) (controller controller))
  (setf (height (model controller)) (old-height action)))

(defclass add-model-note-action (sanlab-action)
  ((text :initform nil :initarg :text :accessor text)
   (position :initform nil :initarg :position :accessor new-position)
   (note :initform nil :accessor note))
  (:default-initargs
   :type 'add-model-note-action))

(defmethod initialize-instance :after ((action add-model-note-action) &rest initargs)
  (setf (note action) (make-instance 'rendered-note :text (text action) :stored-x (first (new-position action)) :stored-y (second (new-position action)))))

(defmethod perform-action ((action add-model-note-action) (controller controller))
  (push (note action) (rendered-notes (model controller)))
  (fire-event-listeners controller 'created-model-note (list (note action)))
  (setf (changed? (model controller)) t)
  t)

(defmethod undo-action ((action add-model-note-action) (controller controller))
  (setf (rendered-notes (model controller)) (remove (note action) (rendered-notes (model controller))))
  (fire-event-listeners controller 'deleted-model-note (list (note action)))
  (setf (changed? (model controller)) t)
  t)

(defclass set-model-note-text-action (sanlab-action)
  ((new-text :initform nil :initarg :text :accessor new-text)
   (note :initform nil :initarg :note :accessor note)
   (old-text :initform nil :accessor old-text))
  (:default-initargs
   :type 'set-model-note-text-action))

(defmethod initialize-instance :after ((action set-model-note-text-action) &rest initargs)
  (if (rendered-note-pointerp (note action))
      (setf (note action) (pointer (note action)))))

(defmethod perform-action ((action set-model-note-text-action) (controller controller))
  (cond ((not (old-text action))
         (setf (old-text action) (text (note action)))))
  (setf (text (note action)) (new-text action))
  (fire-event-listeners controller 'model-note-text-changed (list (note action) (new-text action)))
  (setf (changed? (model controller)) t)
  t
)

(defmethod undo-action ((action set-model-note-text-action) (controller controller))
  (setf (text (note action)) (old-text action))
  (fire-event-listeners controller 'model-note-text-changed (list (note action) (old-text action)))
  (setf (changed? (model controller)) t)
  t
)

(defclass move-model-note-action (sanlab-action)
  ((new-position :initform nil :initarg :position :accessor new-position)
   (note :initform nil :initarg :note :accessor note)
   (old-position :initform nil :accessor old-position))
  (:default-initargs
   :type 'move-model-note-action))

(defmethod initialize-instance :after ((action move-model-note-action) &rest initargs)
  (if (rendered-note-pointerp (note action))
      (setf (note action) (pointer (note action)))))

(defmethod perform-action ((action move-model-note-action) (controller controller))
  (cond ((not (old-position action))
         (setf (old-position action) (list (stored-x (note action)) (stored-y (note action))))))
  (setf (stored-x (note action)) (first (new-position action)))
  (setf (stored-y (note action)) (second (new-position action)))
  (fire-event-listeners controller 'moved-model-note (list (note action) (stored-x (note action)) (stored-y (note action))))
  (setf (changed? (model controller)) t)
  t
)

(defmethod undo-action ((action move-model-note-action) (controller controller))
  (setf (stored-x (note action)) (first (old-position action)))
  (setf (stored-y (note action)) (second (old-position action)))
  (fire-event-listeners controller 'moved-model-note (list (note action) (stored-x (note action)) (stored-y (note action))))
  (setf (changed? (model controller)) t)
  t)

(defclass remove-model-note-action (sanlab-action)
  ((note :initform nil :initarg :note :accessor note))
  (:default-initargs
   :type 'remove-model-note-action))

(defmethod initialize-instance :after ((action remove-model-note-action) &rest initargs)
  (if (rendered-note-pointerp (note action))
      (setf (note action) (pointer (note action)))))

(defmethod perform-action ((action remove-model-note-action) (controller controller))
  (setf (rendered-notes (model controller)) (remove (note action) (rendered-notes (model controller))))
  (fire-event-listeners controller 'deleted-model-note (list (note action)))
  (setf (changed? (model controller)) t)
  t
)

(defmethod undo-action ((action remove-model-note-action) (controller controller))
  (push (note action) (rendered-notes (model controller)))
  (fire-event-listeners controller 'created-model-note (list (note action)))
  (setf (changed? (model controller)) t)
  t
)

(defclass set-model-notes-action (sanlab-action)
  ((old-notes :initform nil :accessor old-notes)
   (new-notes :initform nil :initarg :notes :accessor new-notes))
  (:default-initargs
   :type 'set-model-notes-action))

(defmethod perform-action ((action set-model-notes-action) (controller controller))
  (cond ((not (old-notes action))
         (setf (old-notes action) (notes (model controller)))))
  (setf (notes (model controller)) (new-notes action)))

(defmethod undo-action ((action set-model-notes-action) (controller controller))
  (setf (notes (model controller)) (old-notes action)))

(defclass cut-text-action (sanlab-action)
  ((textbox :initform nil :initarg :textbox :accessor textbox)
   (text :initform nil :accessor text)
   (bounds :initform nil :accessor bounds))
  (:default-initargs
   :type 'cut-text-action))

(defmethod perform-action ((action cut-text-action) (controller controller))
  (cond ((not (text action))
         (setf (text action) (capi:text-input-pane-selected-text (textbox action)))
         (setf (bounds action) (multiple-value-bind (start end) (capi:text-input-pane-selection (textbox action))
                                 (list start end)))))
  (if (eq nil (text action)) (setf (text action) ""))
  (capi:text-input-pane-cut (textbox action)))

(defmethod undo-action ((action cut-text-action) (controller controller))
  (let* ((caret (first (bounds action)))
         (textbox (textbox action))
         (part1 (subseq (capi:text-input-pane-text textbox) 0 caret))
         (part2 (subseq (capi:text-input-pane-text textbox) caret)))
    (if (null part1) (setf part1 ""))
    (if (null part2) (setf part2 ""))
    (setf (capi:text-input-pane-text textbox) (format nil "~A~A~A" part1 (text action) part2))
    (capi:set-text-input-pane-selection textbox (first (bounds action)) (second (bounds action)))))

(defclass paste-text-action (sanlab-action)
  ((textbox :initform nil :initarg :textbox :accessor textbox)
   (text :initform nil :accessor text)
   (bounds :initform nil :accessor bounds))
  (:default-initargs
   :type 'paste-text-action))

(defmethod perform-action ((action paste-text-action) (controller controller))
  (cond ((not (text action))
         (setf (text action) (capi:text-input-pane-text (textbox action)))
         (setf (bounds action) (multiple-value-bind (start end) (capi:text-input-pane-selection (textbox action))
                                 (list start end)))))
  (if (eq nil (text action)) (setf (text action) ""))
  (capi:text-input-pane-paste (textbox action))
  (let ((x (+ (second (bounds action)) (- (length (capi:text-input-pane-text (textbox action))) (length (text action))))))
    (capi:set-text-input-pane-selection (textbox action) x x))
)

(defmethod undo-action ((action paste-text-action) (controller controller))
  (let* ((textbox (textbox action))
         (text (text action)))
    (setf (capi:text-input-pane-text textbox) text)
    (capi:set-text-input-pane-selection textbox (first (bounds action)) (second (bounds action)))))
    
(defclass copy-graph-action (sanlab-action)
  ((copied-graph-hash :initform (make-hash-table) :accessor copied-graph-hash)
   (selected-activities :initform nil :initarg :selected-activities :accessor selected-activities)
   (selected-connections :initform nil :initarg :selected-connections :accessor selected-connections))
  (:default-initargs
   :type 'copy-graph-action))

(defmethod copy-graph-node ((x graph-node))
  (let ((copy (make-instance 'graph-node)))
    (setf (activity-type copy) (activity-type x)
	  (ir-type copy) (ir-type x)
	  (ir-task copy) (ir-task x)
	  (ir-append copy) (ir-append x)
	  (ir-instance copy) (ir-instance x)
	  (label copy) (label x)
	  (color copy) (color x)
	  (stored-x copy) (stored-x x)
	  (stored-y copy) (stored-y x)
	  (distribution copy) (distribution x)
	  (parameters copy) (copy-list (parameters x)))
    copy))

(defmethod initialize-instance :after ((self copy-graph-action) &rest initargs)
  (setf (selected-activities self) (mapcar #'(lambda (x) (if (opaque-pointerp x) (pointer x) x)) (selected-activities self)))
  (setf (selected-connections self) (mapcar #'(lambda (x) (cons (if (opaque-pointerp (car x)) (pointer (car x)) (car x))
                                                                (if (opaque-pointerp (cdr x)) (pointer (cdr x)) (cdr x))))
                                            (selected-connections self)))
)

(defmethod perform-action ((action copy-graph-action) (controller controller))
  (dolist (activity (selected-activities action))
    (setf (gethash (uid activity) (copied-graph-hash action)) (copy-graph-node activity)))
  (dolist (connection (selected-connections action))
    (let ((s (car connection))
	  (e (cdr connection)))
      (cond ((and s e (selected? s) (selected? e))
	     (push (gethash (uid e) (copied-graph-hash action)) (edges-out (gethash (uid s) (copied-graph-hash action))))
	     (push (gethash (uid s) (copied-graph-hash action)) (edges-in (gethash (uid e) (copied-graph-hash action)))))
            )))
  (setf (gethash 'graph (copied-graph-hash action)) t)
  (capi:set-clipboard (primary-view (view controller)) (copied-graph-hash action))
)

(defmethod undo-action ((action copy-graph-action) (controller controller))
)

(defclass paste-graph-action (sanlab-action)
  ((actions :initform nil :accessor actions))
  (:default-initargs
   :type 'paste-graph-action))

(defmethod instantiate-copy ((x graph-node))
  (let ((new (make-instance 'graph-node)))
    (setf (activity-type new) (activity-type x)
	  (ir-type new) (ir-type x)
	  (ir-task new) (ir-task x)
	  (ir-append new) (ir-append x)
	  (ir-instance new) (ir-instance x)
	  (label new) (label x)
	  (color new) (color x)
	  (stored-x new) (+ 10 (stored-x x))
	  (stored-y new) (+ 10 (stored-y x))
	  (distribution new) (distribution x)
	  (parameters new) (copy-list (parameters x)))
    new
    )
)

(defmethod perform-action ((action paste-graph-action) (controller controller))
  (let ((ht (capi:clipboard (primary-view (view controller)) :value))
	(ht2 (make-hash-table))
	(actions nil))
    (cond ((not (actions action))
	   (maphash
	    #'(lambda (k v)
                (block nil
                (if (eql k 'graph) (return))
		(if (not (gethash (uid v) ht2))
		    (let ((act (instantiate-copy v))
			  (action (make-instance 'create-activity-in-model-action)))
		      (setf (gethash (uid v) ht2) act)
		      (setf (created-activity action) act)
		      (push action actions))
		  )
		
		(dolist (edge (edges-out v))
		  (if (not (gethash (uid edge) ht2))
		      (let ((act (instantiate-copy edge))
			    (action (make-instance 'create-activity-in-model-action)))
			(setf (gethash (uid edge) ht2) act)
			(setf (created-activity action) act)
			(push action actions)
			))
		  (push (make-instance 'connect-activities-in-model-action
				       :act1 (gethash (uid v) ht2)
				       :act2 (gethash (uid edge) ht2))
			actions)
		  )
		))
	    ht)
	   (setf (actions action) actions)))
    (deselect-all controller)
    (mapcar #'(lambda (x) (perform-action x controller)) (reverse actions))
    (maphash #'(lambda (k v) (declare (ignore k)) (setf (selected? v) t)) ht2)
))

(defmethod undo-action ((action paste-graph-action) (controller controller))
  (mapcar #'undo-action (reverse (actions action)))
)

(defclass cut-graph-action (sanlab-action)
  ((actions :initform nil :accessor actions)
   (selected-activities :initform nil :initarg :selected-activities :accessor selected-activities)
   (selected-connections :initform nil :initarg :selected-connections :accessor selected-connections)
   )
  (:default-initargs
   :type 'cut-graph-action))

(defmethod initialize-instance :after ((self cut-graph-action) &rest initargs)
  (setf (selected-activities self) (mapcar #'(lambda (x) (if (opaque-pointerp x) (pointer x) x)) (selected-activities self)))
  (setf (selected-connections self) (mapcar #'(lambda (x) (cons (if (opaque-pointerp (car x)) (pointer (car x)) (car x))
                                                                (if (opaque-pointerp (cdr x)) (pointer (cdr x)) (cdr x))))
                                            (selected-connections self)))
)

(defmethod perform-action ((action cut-graph-action) (controller controller))
  (cond ((null (actions action))
         (perform-action (make-instance 'copy-graph-action :selected-activities (selected-activities action) :selected-connections (selected-connections action)) controller)
	 (dolist (connection (selected-connections action))
	   (push (make-instance 'disconnect-activities-in-model-action :act1 (car connection) :act2 (cdr connection)) (actions action))
	   )
	 (dolist (activity (selected-activities action))
	   (push (make-instance 'delete-activity-from-model-action :activity activity)
		 (actions action))
	   )))
  (mapcar #'(lambda (x) (perform-action x controller)) (actions action))
)

(defmethod undo-action ((action cut-graph-action) (controller controller))
  (mapcar #'undo-action (reverse (actions action)))
)
