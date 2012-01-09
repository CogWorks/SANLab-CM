(let ((id 0)) (defun next-id () (incf id)))

(fmakunbound 'prepare-movement)

(defgeneric prepare-movement (module movement)
  (:documentation "Tell <module> to prepare <movement>."))

(defmethod move-id (arg)
  0)

(defmethod prepare-movement ((module pm-module) (mvmt movement-style))
  (change-state module :prep 'BUSY :proc 'BUSY)
  (setf (fprep-time mvmt) 
        (rand-time (compute-prep-time module mvmt)))
  (setf (last-prep module) mvmt)
  (queue-command :command 'preparation-complete :where (my-name module)
                 :params (list :move-id (move-id mvmt))
                 :time (fprep-time mvmt) :randomize nil)
  (when (and (waiting-for-proc-p module) (null (exec-queue module))
             (exec-immediate-p mvmt))
    (setf (set-proc-p mvmt) t)
    (queue-command :time (+ (fprep-time mvmt) (init-time module))
                   :where (my-name module) :command 'change-state 
                   :params '(:proc free) :randomize nil)))

(defclass cogtool-peck-recoil (peck-recoil)
  ((move-id :accessor move-id :initarg :move-id :initform nil)))


(defmethod peck-recoil ((mtr-mod motor-module) &key hand finger r theta move-id)
  (unless (or (check-jam mtr-mod) (check-specs hand finger r theta))
    (when (symbolp theta)
      (setf theta (symbol-value theta)))
    (prepare-movement mtr-mod
                      (make-instance 'cogtool-peck-recoil :hand hand :finger finger
                                     :r r :theta theta :move-id move-id))))

(fmakunbound 'hand-to-home)

(defgeneric hand-to-home (mtr-mod &key move-id)
  (:documentation  "Moves the right hand to the home row position from the mouse loc"))

(defmethod hand-to-home ((mtr-mod motor-module) &key move-id)
  (unless (equal (loc (right-hand mtr-mod)) #(7 4))
    (let ((polar (xy-to-polar (loc (right-hand mtr-mod)) #(7 4))))
      (point-hand mtr-mod :hand 'right :r (vr polar) 
                  :theta (vtheta polar) :twidth 4.0
                  :move-id move-id))))

(fmakunbound 'preparation-complete)

(defgeneric preparation-complete (module &key move-id)
  (:documentation "Method to be called when movement preparation is complete."))

(defmethod preparation-complete ((module pm-module) &key move-id)
  (declare (ignore move-id))
  (change-state module :prep 'free)
  (when (last-prep module)
    (if (exec-immediate-p (last-prep module))
      (setf (exec-queue module)
            (append (exec-queue module) (mklist (last-prep module))))
      (when (and (plusp (init-stamp module))
                 (>= (mp-time) (+ (init-stamp module) (init-time module))))
        (change-state module :proc 'FREE))))
  (maybe-execute-movement module))

(fmakunbound 'perform-movement)

(defgeneric perform-movement (module movement)
  (:documentation "Have <module> perform <movement>."))

(defmethod perform-movement ((module pm-module) (mvmt movement-style))
  (queue-command :time (init-time module) :where (my-name module) 
                 :params (list :move-id (move-id mvmt))
                 :command 'INITIATION-COMPLETE)
  (change-state module :proc 'BUSY :exec 'BUSY)
  
  ;;; DAN
  ;(setf (init-stamp module) (mp-time *mp*))
  
  (setf (init-stamp module) (mp-time))
  
  (setf (exec-time mvmt) (compute-exec-time module mvmt))
  (setf (finish-time mvmt) (compute-finish-time module mvmt))
  (queue-output-events module mvmt)
  (queue-finish-event module mvmt))

(fmakunbound 'queue-finish-event)

(defgeneric queue-finish-event (module movement)
  (:documentation "Queue the FINISH-MOVEMENT associated with <movement>."))


(defmethod queue-finish-event ((module pm-module) (mvmt movement-style))
  (queue-command :time (finish-time mvmt) :command 'finish-movement
                 :params (list :move-id (move-id mvmt))
                 :where (my-name module)))

(fmakunbound 'finish-movement)
(defgeneric finish-movement (module &key move-id)
  (:documentation "Method called when a movement finishes completely."))

(defmethod finish-movement ((module pm-module) &key move-id)
  (declare (ignore move-id))
  (change-state module :exec 'free)
  (maybe-execute-movement module))

(defclass cogtool-hand-ply (hand-ply)
  ((move-id :accessor move-id :initarg :move-id :initform nil)))

(defmethod queue-output-events ((mtr-mod motor-module) (self cogtool-hand-ply))
  (queue-command
   :where :MOTOR :command 'MOVE-A-HAND :time (exec-time self)
   :params (list (hand self) (r self) (theta self) :move-id (move-id self))))

(defmethod queue-output-events ((mtr-mod motor-module) (self punch))
  (queue-command 
   :where :DEVICE :command 'OUTPUT-KEY :time (exec-time self)
   :params
   (list (move-a-finger mtr-mod (hand self) (finger self) 0 0) :move-id (move-id self))
   ;;; DAN
   :from :motor
   :output 'medium))

(defmethod queue-output-events ((mtr-mod motor-module) (self cogtool-peck-recoil))
  (queue-command
   :where :DEVICE :command 'OUTPUT-KEY :time (exec-time self)
   :params
   (list (polar-move-xy (finger-loc-m mtr-mod (hand self) (finger self))
                        (vector (r self) (theta self)))
         :move-id (move-id self))
   ;;; DAN
   :from :motor
   :output 'medium))

(fmakunbound 'press-key)
(defgeneric press-key (mtr-mod key &key move-id)
  (:documentation  "High-level interface to press a key: Look up the command and execute it."))

(defmethod press-key ((mtr-mod motor-module) key &key move-id)
  (when (stringp key)
    (setf key (read-from-string key)))
  (let ((command (key->cmd 
                  ;;; DAN
                  ;(device-interface *mp*) 
                  
                  (current-device-interface )
                                    
                  key)))
    (if (null (first command))
        (print-warning "No press-key mapping available for key ~s." key)
      (apply (first command) mtr-mod (append (rest command) (list :move-id move-id))))))


(fmakunbound 'output-key)

(defgeneric output-key (devin keyloc &key move-id)
  (:documentation  "Request that the device register a key output for the key at a given location."))

(defmethod output-key ((devin device-interface) (keyloc vector) &key move-id)
  (declare (ignore move-id))
  (let* ((invalid (or (< (svref keyloc 1) 0)
                      (> (svref keyloc 1) 6)
                      (< (svref keyloc 0) 0)
                      (and (> (svref keyloc 0) 22)
                           (not (= (svref keyloc 0) 28)))))
         (the-key (if invalid nil (loc->key (keyboard devin) keyloc))))
    (if (eq the-key 'mouse)
        (device-handle-click (device devin))
      (progn 
        (when (null the-key)
          (print-warning "Invalid key location pressed ~s" keyloc))
        (device-handle-keypress (device devin) the-key)))))

(fmakunbound 'move-a-hand)
(defgeneric move-a-hand (mtr-mod hand r theta &key move-id)
  (:documentation  "Moves a hand, returning the new XY location"))

(defmethod move-a-hand ((mtr-mod motor-module) hand r theta &key move-id)
  (ecase hand
    (right (move-hand (right-hand mtr-mod) r theta :move-id move-id))
    (left (move-hand (left-hand mtr-mod) r theta :move-id move-id))))

(fmakunbound 'move-hand)
(defgeneric move-hand (the-hand r theta &key move-id)
  (:documentation  "Moves the hand to a new location"))

(defmethod move-hand ((the-hand hand) r theta &key move-id)
  (declare (ignore move-id))
  (setf (loc the-hand) (polar-move-xy (loc the-hand) (vector r theta))))

(fmakunbound 'initiation-complete)

(defmethod initiation-complete ((module pm-module) &key move-id)
  (declare (ignore move-id))
  (change-state module :proc 'FREE))

#|
(fmakunbound 'prepare)

(defgeneric preparation-complete (module &key move-id)
  (:documentation "Method to be called when movement preparation is complete."))

(defmethod preparation-complete ((module pm-module) &key move-id)
  (change-state module :prep 'free)
  (when (last-prep module)
    (if (exec-immediate-p (last-prep module))
      (setf (exec-queue module)
            (append (exec-queue module) (mklist (last-prep module))))
      (when (and (plusp (init-stamp module))
                 (>= (mp-time) (+ (init-stamp module) (init-time module))))
        (change-state module :proc 'FREE))))
  (maybe-execute-movement module))
|#

(fmakunbound 'hand-to-mouse)

(defgeneric hand-to-mouse (mtr-mod &key move-id)
  (:documentation  "Moves the right hand to the mouse"))

(defmethod hand-to-mouse ((mtr-mod motor-module) &key move-id)
  (unless (vpt= (loc (right-hand mtr-mod)) #(28 2))
    (let ((polar (xy-to-polar (loc (right-hand mtr-mod)) #(28 2))))
      (point-hand mtr-mod :hand 'right :r (vr polar) 
                  :theta (vtheta polar) :twidth 4.0 :move-id move-id))))

(fmakunbound 'point-hand)

(defmethod point-hand ((mtr-mod motor-module) &key hand r theta twidth move-id)
  (unless (or (check-jam mtr-mod) (check-specs hand r theta))
    (prepare-movement mtr-mod
                      (make-instance 'cogtool-hand-ply :hand hand :r r :theta theta
                                     :target-width 
                                     (aif twidth it 
                                          (default-target-width mtr-mod))
                                     :move-id move-id
))))


(fmakunbound 'click-mouse)

(defgeneric click-mouse (mtr-mod &key move-id)
  (:documentation  "Execute a mouse click operation (a punch)"))

(defmethod click-mouse ((mtr-mod motor-module) &key move-id)
  (if (vpt= (loc (right-hand mtr-mod)) #(28 2))
    (punch mtr-mod :hand 'right :finger 'index :move-id move-id)
    (pm-warning "CLICK-MOUSE requested when hand not at mouse!")))

(fmakunbound 'punch)
(defStyle punch () hand finger move-id)

(fmakunbound 'pm-module-request)

(defmethod pm-module-request ((aud-mod audio-module) buffer-name chunk-spec)
  ;(declare (ignore aud-mod))
  (case buffer-name
    (aural
     (case (chunk-spec-chunk-type chunk-spec)
       (clear
        (schedule-event-relative 0 'clear :module :audio :destination :audio
                                 :output 'medium))
       (sound
        (let ((event (when (slot-in-chunk-spec-p chunk-spec 'event) 
                        (verify-single-explicit-value 
                         (chunk-spec-slot-spec chunk-spec 'event) 
                         :audio 'sound 'event))))
          (when event
            (schedule-event-relative 0 'attend-sound
                                     :params (list :event event)
                                     :module :audio  :destination :audio
                                     :details (mkstr 'attend-sound " " event)
                                     :output 'medium))))
       ;; should we support LISTEN-FOR anymore?  Hmm...
       (t
        (print-warning "Invalid command ~a sent to the aural buffer" 
                       (chunk-spec-chunk-type chunk-spec)))))
    (aural-location
     (case (chunk-spec-chunk-type chunk-spec)
       (;; DAN 
        ;;aural-location
        audio-event
        (let ((attended (if (slot-in-chunk-spec-p chunk-spec :attended) 
                            (verify-single-explicit-value 
                             (chunk-spec-slot-spec chunk-spec :attended) 
                             :audio 'aural-location :attended)
                            :IGNORE))
              (kind (if (slot-in-chunk-spec-p chunk-spec 'kind) 
                       (verify-single-explicit-value 
                        (chunk-spec-slot-spec chunk-spec 'kind) 
                        :audio 'aural-location 'kind)
                       :IGNORE))
              (location (if (slot-in-chunk-spec-p chunk-spec 'location) 
                            (verify-single-explicit-value 
                             (chunk-spec-slot-spec chunk-spec 'location) 
                             :audio 'aural-location 'location)
                            :IGNORE))
              (onset (when (slot-in-chunk-spec-p chunk-spec 'onset) 
                        (verify-single-explicit-value 
                         (chunk-spec-slot-spec chunk-spec 'onset) 
                         :audio 'aural-location 'onset))))
          ;(setf (stuffed aud-mod) nil)
          (schedule-event-relative 0 'find-sound :module :audio 
                                   :output 'medium
                                   :destination :audio 
                                   :details ;(format nil "~s" 'find-sound)
                                   (mkstr 'find-sound)
                                   :params (list :kind kind :attended attended 
                                                 :location location
                                                 :onset onset
                                                 ;; Dan 
                                                 ;; this isn't a valid
                                                 ;; keyword for find-sound
                                                 ;:offset offset
                                                 ))))
       (t
        (print-warning "Invalid command ~a sent to the aural-location buffer" 
                       (chunk-spec-chunk-type chunk-spec)))))))

(defmethod pm-module-request ((motor motor-module) buffer-name chunk-spec)
  (case (chunk-spec-chunk-type chunk-spec)
    (clear ;; replaces the implicit clear from -manual
     (schedule-event-relative 0 'clear :module :motor :destination :motor
                              :output 'medium)
     )
    (execute
     (schedule-event-relative 0 'execute :module :motor :destination :motor
                              :params (list :move-id (next-id))
                              :output 'medium)
     )
    (click-mouse
     (schedule-event-relative 0 'click-mouse :module :motor 
                              :params (list :move-id (next-id))
                              :destination :motor
                              :output 'low)
     )
    (hand-to-mouse
     (schedule-event-relative 0 'hand-to-mouse :module :motor 
                              :params (list :move-id (next-id))
                              :destination :motor
                              :output 'low)
     )
    (hand-to-home
     (schedule-event-relative 0 'hand-to-home :module :motor 
                              :params (list :move-id (next-id))
                              :destination :motor
                              :output 'low)
     )
    
    (move-cursor
     (let ((object (if (slot-in-chunk-spec-p chunk-spec 'object) 
                      (verify-single-explicit-value 
                       (chunk-spec-slot-spec chunk-spec 'object) 
                       :motor 'move-cursor 'object)
                      nil))
           (location (if (slot-in-chunk-spec-p chunk-spec 'loc)
                         (verify-single-explicit-value 
                          (chunk-spec-slot-spec chunk-spec 'loc)
                          :motor 'move-cursor 'loc)
                         nil))
           (device (if (slot-in-chunk-spec-p chunk-spec 'device)
                      (verify-single-explicit-value 
                       (chunk-spec-slot-spec chunk-spec 'device)
                       :motor 'move-cursor 'device)
                      nil))
           )
       
       
       (when (or object location)
         (if device
           (schedule-event-relative 
            0 
            'move-cursor 
            :params (list motor :object object 
                          :loc location
                          :device device
                          :move-id (next-id)
                          )
            :module :motor
            :output 'low)
           (schedule-event-relative 
            0 
            'move-cursor 
            :destination :motor
            
            :params (list :object object 
                          :loc location
                          :move-id (next-id))
            :module :motor
            :output 'low)))))
    
    ((peck peck-recoil) 
     (let* ((cmd (chunk-spec-chunk-type chunk-spec))
            (hand (if (slot-in-chunk-spec-p chunk-spec 'hand) 
                     (verify-single-explicit-value 
                      (chunk-spec-slot-spec chunk-spec 'hand) 
                      :motor cmd 'hand)
                     nil))
            (finger (if (slot-in-chunk-spec-p chunk-spec 'finger)
                       (verify-single-explicit-value 
                        (chunk-spec-slot-spec chunk-spec 'finger)
                        :motor cmd 'finger)
                       nil))
            (r (if (slot-in-chunk-spec-p chunk-spec 'r)
                  (verify-single-explicit-value 
                   (chunk-spec-slot-spec chunk-spec 'r)
                   :motor cmd 'r)
                  nil))
            (theta (if (slot-in-chunk-spec-p chunk-spec 'theta)
                      (verify-single-explicit-value 
                       (chunk-spec-slot-spec chunk-spec 'theta)
                       :motor cmd 'theta)
                      nil)))
       
       (when (and hand finger r theta)
         (schedule-event-relative 
          0 
          cmd
          :destination :motor
          :params (list :hand hand :finger finger :r r :theta theta :move-id (next-id))
          :module :motor
          :output 'low))))
    
    (point-hand-at-key 
     (let ((hand (if (slot-in-chunk-spec-p chunk-spec 'hand) 
                    (verify-single-explicit-value 
                     (chunk-spec-slot-spec chunk-spec 'hand) 
                     :motor 'point-hand-at-key 'hand)
                    nil))
           (to-key (if (slot-in-chunk-spec-p chunk-spec 'to-key)
                      (verify-single-explicit-value 
                       (chunk-spec-slot-spec chunk-spec 'to-key)
                       :motor 'point-hand-at-key 'to-key)
                      nil)))
       
       (when (and hand to-key)
         (schedule-event-relative 
          0 
          'point-hand-at-key
          :destination :motor
          :params (list :hand hand :to-key to-key :move-id (next-id))
          :module :motor
          :output 'low))))
    (punch 
     (let* ((hand (if (slot-in-chunk-spec-p chunk-spec 'hand) 
                     (verify-single-explicit-value 
                      (chunk-spec-slot-spec chunk-spec 'hand) 
                      :motor 'punch 'hand)
                     nil))
            (finger (if (slot-in-chunk-spec-p chunk-spec 'finger)
                       (verify-single-explicit-value 
                        (chunk-spec-slot-spec chunk-spec 'finger)
                        :motor 'punch 'finger)
                       nil)))
       
       (when (and hand finger)
         (schedule-event-relative 
          0 
          'punch
          :destination :motor
          :params (list :hand hand :finger finger :move-id (next-id))
          :module :motor
          :output 'low))))
    (press-key 
     (let* ((key (verify-single-explicit-value 
                   (chunk-spec-slot-spec chunk-spec 'key) 
                   :motor 'press-key 'key)))
       
       (when key
         (schedule-event-relative 
          0 
          'press-key
          :destination :motor
          :params (list key :move-id (next-id))
          :module :motor
          :output 'low))))
    
    
    (prepare
     (let* ((style (verify-single-explicit-value 
                   (chunk-spec-slot-spec chunk-spec 'style) 
                    :motor 'prepare 'style))
            (hand (if (slot-in-chunk-spec-p chunk-spec 'hand) 
                     (verify-single-explicit-value 
                      (chunk-spec-slot-spec chunk-spec 'hand) 
                      :motor 'prepare 'hand)
                     nil))
            (finger (if (slot-in-chunk-spec-p chunk-spec 'finger)
                       (verify-single-explicit-value 
                        (chunk-spec-slot-spec chunk-spec 'finger)
                        :motor 'prepare 'finger)
                       nil))
            (r (if (slot-in-chunk-spec-p chunk-spec 'r)
                  (verify-single-explicit-value 
                   (chunk-spec-slot-spec chunk-spec 'r)
                   :motor 'prepare 'r)
                  nil))
            (theta (if (slot-in-chunk-spec-p chunk-spec 'theta)
                      (verify-single-explicit-value 
                       (chunk-spec-slot-spec chunk-spec 'theta)
                       :motor 'prepare 'theta)
                      nil)))
       
       (if style
         (schedule-event-relative 
          0 
          'prepare
          :destination :motor
          :params (append (list style) (mapcan (lambda (name value) (when value (list name value)))
                                         '(:hand :finger :r :theta) (list hand finger r theta)) (list :move-id (next-id)))
          :module :motor
          :output 'low)
         (print-warning "Style required for a prepare request to the manual module"))))
    
    (t
     (aif (gethash (chunk-spec-chunk-type chunk-spec) (new-requests-table motor))
          (funcall (cdr it) motor chunk-spec)
          (print-warning "Invalid command ~a sent to the ~s buffer" 
                         (chunk-spec-chunk-type chunk-spec)
                         buffer-name)))))

(defmethod pm-module-request ((speech speech-module) buffer-name 
                                  chunk-spec)
  (declare (ignore speech))
  (case (chunk-spec-chunk-type chunk-spec)
    (clear 
     (schedule-event-relative 0 'clear :module :speech :destination :speech :output 'low))
    (speak 
     (let ((string (if (slot-in-chunk-spec-p chunk-spec 'string) 
                       (verify-single-explicit-value 
                        (chunk-spec-slot-spec chunk-spec 'string) 
                        :speech 'speak 'string)
                     nil)))
       
       (if (stringp string)
             (schedule-event-relative 
              0 
              'speak 
              :destination :speech
              :params (list :text string)
              :module :speech
              :output 'low)
         (model-warning "String slot in a speak request must be a Lisp string."))))
    (subvocalize 
     (let ((string (if (slot-in-chunk-spec-p chunk-spec 'string) 
                       (verify-single-explicit-value 
                        (chunk-spec-slot-spec chunk-spec 'string) 
                        :speech 'speak 'string)
                     nil)))
       
       (if (stringp string)
         (schedule-event-relative 
          0 
          'subvocalize 
          :destination :speech
          :params (list :text string)
          :module :speech
          :output 'low)
         (model-warning "String slot in a subvocalize request must be a Lisp string."))))
    (t
     (print-warning "Invalid command ~a sent to the ~s buffer" 
                    (chunk-spec-chunk-type chunk-spec)
                    buffer-name))))


(defmethod pm-module-request ((vis-mod vision-module) buffer-name chunk-spec)
  (case buffer-name
    (visual
     (when (visual-lock vis-mod)
       (setf (visual-lock vis-mod) nil)
       (schedule-event-relative 0 'unlock-device 
                                :module :vision
                                :destination :device
                                :priority :min
                                :output nil
                                :maintenance t))
       
     (case (chunk-spec-chunk-type chunk-spec)
       (clear ;; replaces the implicit clear from -visual
        (schedule-event-relative 0 'clear :module :vision :destination :vision :output 'low))
       (clear-scene-change
        (schedule-event-relative 0 'clear-scene-change :module :vision :destination :vision :output 'low))
       (start-tracking
        (schedule-event-relative 0 'start-tracking 
                                 :destination :vision
                                 :module :vision
                                 :output 'medium)
        )
       (assign-finst
        (let ((object (if (slot-in-chunk-spec-p chunk-spec 'object) 
                         (verify-single-explicit-value 
                          (chunk-spec-slot-spec chunk-spec 'object) 
                          :vision 'assign-finst 'object)
                         nil))
              (location (if (slot-in-chunk-spec-p chunk-spec 'location)
                            (verify-single-explicit-value 
                             (chunk-spec-slot-spec chunk-spec 'location) 
                             :vision 'assign-finst 'location)
                            nil)))
          
          (if (or object location)
            (schedule-event-relative 0 'assign-finst 
                                     :params (list vis-mod :object object 
                                                   :location location)
                                     :module :vision
                                     :output 'medium)
            (print-warning "An object or location is required for an assign-finst request"))))
       
       
       (visual-object
        (print-warning "Move attention requests are now done with an isa move-attention"))
       
       
       (move-attention
        (let ((sp (if (slot-in-chunk-spec-p chunk-spec 'screen-pos) 
                     (verify-single-explicit-value 
                      (chunk-spec-slot-spec chunk-spec 'screen-pos) 
                      :vision 'visual-object 'screen-pos)
                     nil))
              (scale (if (slot-in-chunk-spec-p chunk-spec 'scale)
                        (verify-single-explicit-value 
                         (chunk-spec-slot-spec chunk-spec 'scale) 
                         :vision 'visual-object 'scale)
                        nil)))
         ; (when scale
         ;     (print-warning "Scale values are not yet handled by the new vision module - ignoring it."))
          (when sp
            (if (chunk-p-fct sp)
                (if (chunk-type-subtype-p-fct (chunk-chunk-type-fct sp) 'visual-location)
                    (schedule-event-relative 0 'move-attention 
                                     :params (list vis-mod :scale scale :location sp)
                                     :details 
                                     (concatenate 'string "Move-attention " (symbol-name sp)
                                                  " " (symbol-name scale))                                     
                                             :module :vision)
                  (print-warning "screen-pos value ~s in a move-attention request was not a visual-location chunk" sp))
              (print-warning "screen-pos value ~s in a move-attention request was not a chunk" sp)))))
       (t
        (print-warning "Invalid command ~a sent to the visual buffer" 
                       (chunk-spec-chunk-type chunk-spec)))))
    
    (visual-location
     (cond ((chunk-type-subtype-p-fct (chunk-spec-chunk-type chunk-spec) 'visual-location)
            (let ((nearest (if (slot-in-chunk-spec-p chunk-spec :nearest) 
                               (verify-single-explicit-value 
                                (chunk-spec-slot-spec chunk-spec :nearest) 
                                :vision 'visual-location :nearest)
                             :none))              
                  (attended (if (slot-in-chunk-spec-p chunk-spec :attended) 
                                (multiple-value-bind (val valid)
                                    (verify-single-explicit-value 
                                     (chunk-spec-slot-spec chunk-spec :attended) 
                                     :vision 'visual-location :attended)
                                  valid)
                              :none)))           
              
              (if (or (null nearest) (null attended))
                  (print-warning "Invalid value in a request to the visual-location buffer")
                
                (schedule-event-relative 0 'find-location :module :vision 
                                         :destination :vision 
                                         :details "Find-location" 
                                         :output 'medium
                                         :params (list chunk-spec)))))
           ((chunk-type-subtype-p-fct (chunk-spec-chunk-type chunk-spec) 'set-visloc-default)
            (let ((nearest (if (slot-in-chunk-spec-p chunk-spec :nearest) 
                               (verify-single-explicit-value 
                                (chunk-spec-slot-spec chunk-spec :nearest) 
                                :vision 'set-visloc-default :nearest)
                             :none))              
                  (attended (if (slot-in-chunk-spec-p chunk-spec :attended) 
                                (multiple-value-bind (val valid)
                                    (verify-single-explicit-value 
                                     (chunk-spec-slot-spec chunk-spec :attended) 
                                     :vision 'set-visloc-default :attended)
                                  valid)
                              :none))
                  (type (if (slot-in-chunk-spec-p chunk-spec 'type) 
                                (multiple-value-bind (val valid)
                                    (verify-single-explicit-value 
                                     (chunk-spec-slot-spec chunk-spec 'type) 
                                     :vision 'set-visloc-default 'type)
                                  val)
                              :none)))           
              
              (if (or (null nearest) (null attended))
                  (print-warning "Invalid value in a request to the visual-location buffer")
                (if (and type (not (eq type :none)) (not (chunk-type-p-fct type)))
                    (print-warning "Invalid type specified in set-visloc-default request.")
                  (schedule-event-relative 0 'set-visloc-default-request :module :vision 
                                           :destination :vision 
                                           :details "Set-visloc-default" 
                                           :output 'medium
                                           :priority 9 ; just below the buffer clearing by the production
                                           :params (list chunk-spec))))))
           (t (print-warning "Invalid command ~a sent to the visual-location buffer" 
                             (chunk-spec-chunk-type chunk-spec)))))))


(fmakunbound 'reset-motor-module)

(defun reset-motor-module (instance)
   
  (chunk-type motor-command move-id)
  (chunk-type (click-mouse (:include motor-command)))
  (chunk-type (hand-to-mouse (:include motor-command)))
  (chunk-type (hand-to-home (:include motor-command)))
  (chunk-type (move-cursor (:include motor-command)) object loc device)
  (chunk-type (peck (:include motor-command)) hand finger r theta)
  (chunk-type (peck-recoil (:include motor-command)) hand finger r theta)
  (chunk-type (point-hand-at-key (:include motor-command)) hand to-key)
  (chunk-type (press-key (:include motor-command)) key)
  (chunk-type (punch (:include motor-command)) hand finger)
  
  (chunk-type (prepare (:include motor-command)) style hand finger r theta)
  (chunk-type (execute (:include motor-command)))
  
  ;; Moved so that extensions which define new motor
  ;; commands can specialize the reset method to add the 
  ;; required chunk-types and include motor-command.
  
  (reset-pm-module instance)
  
  ; Not sure yet what chunks it needs...
  ;(define-chunks 
  ;    )
  
  
  ;; Define the chunk-types for the user specified extensions
  
  (maphash (lambda (key value)
             (declare (ignore key))
             (let ((chunk-type (car value)))
               (unless (chunk-type-fct chunk-type)
                 (print-warning "Failed to extend motor capabilities with chunk-type: ~s" chunk-type))))
           (new-requests-table instance))
                   
  
  )
