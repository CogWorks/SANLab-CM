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

(defparameter *operator-keywords*
  '(("attend" "Cognitive Operator")
    ("initiate" "Cognitive Operator")
    ("verify" "Cognitive Operator")
    ("eye movement" "Motor Operator")
    ("eyemvmt" "Motor Operator")
    ("get" "Cognitive Operator")
    ("unpack" "Cognitive Operator")
    ("mouseDn" "Motor Operator")
    ("mouseUp" "Motor Operator")
    ("move" "Motor Operator")
    ("speech" "Motor Operator")
    ("left-hand" "Motor Operator")
    ("right-hand" "Motor Operator")
    ("perceive" "Perceptual Operator")
    ("home" "Left Hand Operator")
))

;(defun expected-operator-type (str1)
;  (let ((res "Basic Operator"))
;    (mapcar #'(lambda (x) (if (contains str1 (first x)) (setf res (second x)))) *operator-keywords*)
;    res))

(defun expected-operator-type (str1)
  (let* ((res "Basic Operator")
         (possible (remove-if #'null (mapcar #'(lambda (x) (contains str1 (first x))) *operator-keywords*)))
         (index (if possible (apply #'min possible) -1)))
    (dolist (op *operator-keywords* res)
      (let ((pos (contains str1 (first op))))
        (if (and pos (= index pos))
          (return-from expected-operator-type (second op)))))))

(defstruct (macproj-component (:conc-name component-))
  (name "") (critical nil) (duration 0) (task-id 0) (target-names '()) (target-ids '()) (dependent-names '()) (dependent-ids '()))

(defstruct (task-graph (:conc-name graph-))
  (start-nodes '()) (node-count 0))

(defstruct (task-graph-node (:conc-name graph-node-))
  (name "") (id 0) (type "Basic Operator") (critical nil) (duration 0) (number 0) (depth 0) (pos-x 0) (pos-y 0) (prereqs nil) (targets nil) (visited nil))

(defun pathname-to-string (path)
  (if (not (pathnamep path))
      NIL
    (let ((dir (pathname-directory path))
          (name (pathname-name path))
          (ext (pathname-type path))
          (final-dt "")
          (final-pt ""))
      (if (equal :ABSOLUTE (car dir))
          (setq final-dt "/"))
      (do ((i 1 (1+ i)))
          ((= i (length dir)))
        (setq final-dt (string-append final-dt (nth i dir) "/")))
      (setq final-pt (string-append final-dt name))
      (setq final-dt (string-append final-dt name))
      (setf (elt final-pt (- (length final-pt) 2)) #\P)
      (cond ((not (equal ext ':unspecific))
             (setq final-dt (string-append final-dt "." ext))
             (setq final-pt (string-append final-pt "." ext))))
      (list final-pt final-dt))))

(defun string-to-int (str)
  (if (stringp str)
      (string-to-int (read-from-string str))
    str))

(defun string-to-string (str)
  (if (= 0 (length str)) (return-from string-to-string str))
  (if (and (stringp str) (equal #\" (elt str 0)))
      (read-from-string str)
    str))

(defun read-macproject-file (file-name)
  ;(format t "Reached read-macproject-file~%")
  (if (not (pathnamep file-name))
      NIL
    (let* ((file-names (pathname-to-string file-name))
           (pt-file-stream (open (first file-names) :direction :input)) ;:external-format '(:latin-1 :eol-style #+win32 :crlf #-win32 :lf)))
           (dt-file-stream (open (second file-names) :direction :input)) ;:external-format '(:latin-1 :eol-style #+win32 :crlf #-win32 :lf)))
           (table (make-hash-table :test #'equal)))
      (if (or (null dt-file-stream) (null pt-file-stream))
          NIL
        (let (dt-parts pt-parts line)
          (loop
           (setq line (read-line dt-file-stream nil nil))
           (when (null line) (return))
           (format t "Read DT line: ~A~%" line)
           (setq dt-parts (split-string line #\tab))
           (if (= 1 (length dt-parts))
               (setq dt-parts (split-string (first dt-parts) #\comma)))
           (if (null (gethash (first dt-parts) table))
               (setf (gethash (first dt-parts) table)
                     (make-task-graph-node :name (first dt-parts) :id (string-to-int (second dt-parts)))))
           (if (null (gethash (third dt-parts) table))
               (setf (gethash (third dt-parts) table)
                     (make-task-graph-node :name (third dt-parts) :id (string-to-int (fourth dt-parts)))))
           (push (gethash (first dt-parts) table) (graph-node-prereqs (gethash (third dt-parts) table)))
           (push (gethash (third dt-parts) table) (graph-node-targets (gethash (first dt-parts) table))))
          (loop
           (setq line (read-line pt-file-stream nil nil))
           (when (null line) (return))
           (format t "Read PT line: ~A~%" line)
           (setq pt-parts (split-string line #\tab))
           (if (= 1 (length pt-parts))
               (setq pt-parts (split-string (first pt-parts) #\comma)))
           (if (< 3 (length pt-parts))
               (setf pt-parts (list (first pt-parts) (second pt-parts) (car (last pt-parts)))))
           (let ((node (gethash (first pt-parts) table)))
             (setf (graph-node-critical node) (if (string-equal "\"True\"" (second pt-parts)) t nil))
             (setf (graph-node-type node) (expected-operator-type (first pt-parts)))
             (setf (graph-node-duration node) (string-to-int (third pt-parts)))))
           
           ))
      (close dt-file-stream)
      (close pt-file-stream)
      table)))

(defun hash-table-to-list (table)
  (let ((all-entries '()))
    (maphash #'(lambda (key value) (push value all-entries)) table)
    all-entries))

(defun find-component-in-list (id lst)
  (let ((final NIL)
        (cur-comp NIL))
    (do ((i 0 (1+ i)))
        ((or final (= i (length lst))))
      (setq cur-comp (nth i lst))
      (if (= id (component-task-id cur-comp))
          (setq final cur-comp)))
    final))

(defun does-node-exist (id nodelist)
  (dolist (curnode nodelist nil)
    (if (= id (graph-node-id curnode)) (return-from does-node-exist curnode)))
  )

(defun build-subgraph (src-component all-components created-nodes)
; Check if any created-nodes have src-component id
  (if (null src-component) (return-from build-subgraph created-nodes))
  (if (string-equal (component-name src-component) "\"home to \"5\" (2)\"") (format t "Targets = ~A~%" (component-target-names src-component)))
  (let ((node-exists (does-node-exist (component-task-id src-component) created-nodes))
        (target-ids nil)
        (my-node nil))
    (if node-exists (return-from build-subgraph (cons node-exists (remove node-exists created-nodes))))
    (let ((new-node (make-task-graph-node)))
      (setf (graph-node-name new-node) (component-name src-component))
      (setf (graph-node-id new-node) (component-task-id src-component))
      (setf (graph-node-duration new-node) (component-duration src-component))
; Prepend graph-node to created-nodes
      (push new-node created-nodes))
    (setq my-node (first created-nodes))
    (format t "~A~%" my-node)
; Get src-component target-ids
    (setq target-ids (component-target-ids src-component))
; Do-loop through target-ids
    (do ((i 0 (1+ i)))
        ((= i (length target-ids)) created-nodes)
;     If does-node-exist id created-nodes Do nothing
;     else set created-nodes (build-subgragh child-component all-components created-nodes)
      (let* ((temp (build-subgraph (find-component-in-list (nth i target-ids) all-components) all-components created-nodes))
             (child (first temp)))
        (setq created-nodes temp)
        (if (and (not (eql child my-node)) (not (member child (graph-node-targets my-node))))
            (push child (graph-node-targets my-node)))))
; move my-node to front of list and return created-nodes
    (setf created-nodes (cons my-node (remove my-node created-nodes)))))

(defun component-to-graph-node (comp)
  (let ((my-node (make-task-graph-node)))
    (setf (graph-node-name my-node) (component-name comp))
    (setf (graph-node-id my-node) (component-task-id comp))
    (setf (graph-node-duration my-node) (component-duration comp))
    my-node))

(defun convert-components-to-nodes (components)
  (let* ((ht (make-hash-table :test #'equal))
         (nodes (mapcar #'(lambda (c) (setf (gethash (component-task-id c) ht) (component-to-graph-node c))) components)))
    (dolist (c components)
      (format t "Finding targets for ~S~%" (component-task-id c))
      (format t "Targets = ~S~%" (component-target-ids c))
      (let ((node (gethash (component-name c) ht)))
        (setf (graph-node-targets node) (mapcar #'(lambda (x) (gethash x ht)) (component-target-ids c)))))
    nodes))

(defun process-graph-parenthood (nodes)
  (dolist (gn nodes nodes)
    (dolist (child (graph-node-targets gn))
      (push gn (graph-node-prereqs child)))))

(defun assign-numbers (nodes)
  (let ((counter 0))
    (dolist (gn nodes nodes)
      (setf (graph-node-number gn) counter)
      (incf counter))))

(defun graph-sorted? (nodes)
  (dolist (gn nodes t)
    (dolist (parent (graph-node-prereqs gn))
      (if (> (graph-node-number parent) (graph-node-number gn)) (return-from graph-sorted? nil)))
    (dolist (child (graph-node-targets gn))
      (if (< (graph-node-number child) (graph-node-number gn)) (return-from graph-sorted? nil)))))

(defun sort-graph (nodes)
  (do ()
      ((graph-sorted? nodes) nodes)
    (dolist (gn nodes)
      (dolist (parent (graph-node-prereqs gn))
        (if (> (graph-node-number parent) (graph-node-number gn))
            (let ((swap (graph-node-number parent)))
              (setf (graph-node-number parent) (graph-node-number gn))
              (setf (graph-node-number gn) swap)))))))

(defun order-graph (nodes)
  (sort nodes #'< :key #'graph-node-number))

(defun depth-ordered? (nodes)
  (dolist (node nodes t)
    (dolist (parent (graph-node-prereqs node))
      (if (>= (graph-node-depth parent) (graph-node-depth node)) (return-from depth-ordered? nil)))))

(defun assign-depth (nodes)
  (let ((unvisited (list (first nodes)))
        (temp nil)
        (final nil))
    ;(do ((gn (first unvisited) (first (setq unvisited (rest unvisited)))))
    ;    ((null unvisited) nodes)
    ;  (if (= -1 (graph-node-depth gn))
    ;      (progn
    ;        (let ((choices (cons 0 (mapcar #'(lambda (x) (1+ (graph-node-depth x))) (graph-node-prereqs gn)))))
    ;          (format t "~A~%" choices)
    ;          (setf (graph-node-depth gn) (apply #'max choices))
    ;          (setq unvisited (append unvisited (graph-node-targets gn)))))))
    (do ()
        ((depth-ordered? nodes) nodes)
      (dolist (node nodes)
        (dolist (parent (graph-node-prereqs node))
          (if (>= (graph-node-depth parent) (graph-node-depth node))
              (setf (graph-node-depth node) (1+ (graph-node-depth parent)))))))
))

(defun order-graph-depth (nodes)
  (sort nodes #'< :key #'graph-node-depth))

(defun depth-nodes (nodes)
  (let* ((final-depth (graph-node-depth (first (last nodes))))
         (counters (make-array (list (1+ final-depth)) :initial-element 0)))
    (dolist (gn nodes counters)
      (incf (elt counters (graph-node-depth gn))))))

(defun max-depth (depths)
  (let ((max (elt depths 0)))
    (dotimes (i (length depths) max)
      (if (> (elt depths i) max) (setq max (elt depths i))))))

;(defun assign-positions (nodes)
;  (let* ((nodes-at-depth (depth-nodes nodes))
;         (seen-nodes (make-array (list (length nodes-at-depth)) :initial-element 0))
;         (max-y (* 200 (max-depth nodes-at-depth))))
;    (dolist (gn nodes nodes)
;      (let ((depth (graph-node-depth gn)))
;        (setf (graph-node-pos-x gn) (* 150 depth))
;        (setf (graph-node-pos-y gn) (floor (* (/ (+ (elt seen-nodes depth) 1) (+ (elt nodes-at-depth depth) 1)) max-y)))
;        (incf (elt seen-nodes depth))))))

(defun get-activity-index (act-name)
  (- (length (app-property 'activity-types)) (length (find (get-activity-by-typename act-name) (app-property 'activity-types)))))

(defun vector-to-list (vec)
  (let ((final nil))
    (dotimes (i (length vec) (reverse final))
      (push (elt vec i) final))))

(defun depth-nodes2 (nodes)
  (let* ((max-depth (graph-node-depth (car (last nodes))))
         (depth-array (make-array (list (1+ max-depth)) :initial-element nil)))
    (dolist (node nodes (vector-to-list depth-array))
      (push node (elt depth-array (graph-node-depth node))))))

(defun assign-positions (nodes)
  (let* ((nodes-at-depth (depth-nodes nodes))
         (depth-hash (make-hash-table :test #'equal))
         (operator-depth (make-array (list (length (app-property 'activity-types))) :initial-element 0)))
    (dolist (act (app-property 'activity-types))
      (setf (gethash (get-full-name act) depth-hash) (make-array (list (length nodes-at-depth)) :initial-element 0)))
    (dolist (gn nodes)
      (incf (elt (gethash (graph-node-type gn) depth-hash) (graph-node-depth gn))))
    (dotimes (i (length (app-property 'activity-types)))
      (let ((act (nth i (app-property 'activity-types))))
        (dotimes (j (length nodes-at-depth))
          (setf (elt operator-depth i) (max (elt operator-depth i) (elt (gethash (get-full-name act) depth-hash) j))))))
    (let ((sum 0)
          (act nil)
          (nodes2 (depth-nodes2 nodes)))
      (dotimes (i (length (app-property 'activity-types)))
        (setq act (nth i (app-property 'activity-types)))
        (dolist (dn nodes2)
          (let ((nodes-seen (make-array (list (length (app-property 'activity-types))) :initial-element 0)))
            (dolist (node dn)
              (if (equal (graph-node-type node) (get-full-name act))
                  (progn
                    (setf (graph-node-pos-x node) (* (app-property 'macproject-x-multiplier) (graph-node-depth node)))
                    (setf (graph-node-pos-y node) (* (app-property 'macproject-y-multiplier) (+ sum (elt nodes-seen i))))
                    (incf (elt nodes-seen i)))))))
        (incf sum (elt operator-depth i))))
    nodes
    ))

(defun get-start-components (lst)
  (let ((start-tasks NIL)
        (cur-comp NIL))
    (do ((i 0 (1+ i)))
        ((= i (length lst)) start-tasks)
      (setq cur-comp (nth i lst))
      (if (null (graph-node-prereqs cur-comp))
          (push cur-comp start-tasks)))))

(defun remove-outer-quotes (str)
  (let ((len (length str)))
    (if (and (char= #\" (elt str 0)) (char= #\" (elt str (1- len))))
        (subseq str 1 (1- len))
      str)))

(defun graph-node-to-activity (node)
  (let ((act-type (get-activity-by-typename (expected-operator-type (graph-node-name node)))))
    (make-instance 'graph-node
                   :uid (graph-node-id node)
                   :activity-type act-type
                   :label (graph-node-name node)
                   :color (get-color act-type)
                   :stored-x (graph-node-pos-x node)
                   :stored-y (graph-node-pos-y node)
                   :distribution (get-distribution-by-typename "Constant")
                   :parameters (list (format nil "~A" (graph-node-duration node))))))

(defun connect-activities (node activities)
  (let ((target-ids (graph-node-targets node))
        (src-act (find (graph-node-id node) activities :test #'(lambda (id act) (= id (uid act))))))
    (dolist (target target-ids)
      (dolist (act activities)
        (if (= (graph-node-id target) (uid act))
            (progn
              (push src-act (edges-in act))
              (push act (edges-out src-act))))))))

(defun check-final-model (model nodes)
  (let* ((activities (activities model))
         (source (remove-if #'(lambda (activity) (edges-in activity)) activities))
         (target (remove-if #'(lambda (activity) (edges-out activity)) activities)))
    (dolist (activity activities)
      (setf (mean-start activity) 0.0)
      (setf (mean-end activity) 0.0)
      (setf (parsed-parameters activity)
            (parse-parameter-string (parameters activity)))
      )
    (generate-sample-times activities)
    (simulate-network activities (first source) (first target))
    (let ((path (get-longest-path (first source) (first target)))
          (errors nil)
          (short-list (remove-if #'null (copy-list nodes) :key #'graph-node-critical)))
      (dolist (node short-list)
        (if (not (find (graph-node-name node) path :test #'equal :key #'label)) (push (list (graph-node-name node) 'node) errors)))
      (dolist (act path)
        (if (not (find (label act) short-list :test #'equal :key #'graph-node-name)) (push (list (label act) 'model) errors)))
      errors
      )
    )
)

(defun check-for-cycle (component visited-components)
  (if (member component visited-components)
      (error "Cycle found starting with component ~S, following ~{ ~S~}"
             (graph-node-id component)
             (mapcar #'graph-node-id (reverse (subseq visited-components 0 (1+ (position component visited-components)))))))
  (setf (graph-node-visited component) t)
  (let ((new-visited (cons component visited-components)))
    (dolist (edge (graph-node-targets component) nil)
      (if (not (graph-node-visited edge))
          (check-for-cycle edge new-visited))))
)

(defun import-macproject-file ()
  (let ((file-name (capi:prompt-for-file "Select MacProject file to import:" :pathname (user-homedir-pathname) :filter "*-DT"))
        (components nil)
        (start-components nil)
        (nodes nil)
        (activities nil))
    (if (not (pathnamep file-name)) (return-from import-macproject-file))
    (let ((table (read-macproject-file file-name)))
;      (inspect table)
;      (return-from import-macproject-file)
      (setf components (hash-table-to-list table)))
    ;(return-from import-macproject-file components)
    ;(inspect components)
    (setf start-components (get-start-components components))
    (cond ((= 0 (length start-components))
           (capi:display-message "Could not locate any starting nodes in the MacProject file. Aborting import.")
           (return-from import-macproject-file))
          ((< 1 (length start-components))
           (push (make-task-graph-node :name "SanLab temporary start" :id 0 :targets start-components) components)
           (dolist (comp start-components)
             (setf (graph-node-prereqs comp) (list (first components))))
           (setq start-components (list (first components)))))
    (format t "Checking graph for cycles~%")
    (check-for-cycle (first components) nil)
    ;(return-from import-macproject-file components)
    ;(setq nodes (build-subgraph (first start-components) components nil))
    ;(setq nodes (convert-components-to-nodes components))
    ;(inspect nodes)
    ;(return-from import-macproject-file nodes)
    ;(setq nodes (process-graph-parenthood nodes))
    ;(return-from import-macproject-file nodes)
    (setq nodes components)
    (mapcar #'(lambda (node) (setf (graph-node-name node) (remove-outer-quotes (graph-node-name node)))) nodes)
    (format t "Assigning node numbers~%")
    (setq nodes (assign-numbers nodes))
    (format t "Sorting graph~%")
    (setq nodes (sort-graph nodes))
    (format t "Ordering graph~%")
    (setq nodes (order-graph nodes))
    (format t "Assigning node depth~%")
    (setq nodes (assign-depth nodes))
    (format t "Ordering graph depths~%")
    (setq nodes (order-graph-depth nodes))
    ;(inspect nodes)
    ;(return-from import-macproject-file nil)
    (format t "Assigning positions to nodes~%")
    (setq nodes (assign-positions nodes))
    (format t "Creating graph nodes~%")
    (setq activities (mapcar #'graph-node-to-activity nodes))
    (format t "Connecting activities together~%")
    (mapcar #'(lambda (node) (connect-activities node activities)) nodes)
    (format t "Creating final model object~%")
    (let* ((model (make-instance 'model
                                 :activities activities
                                 :width (max (app-property 'default-model-width) (+ 200 (graph-node-pos-x (first (last nodes)))))
                                 :height (max (app-property 'default-model-height) (+ 100 (apply #'max (mapcar #'graph-node-pos-y nodes))))))
           (errors (check-final-model model nodes)))
      (setf (notes model) (string-append (notes model) (format nil "~%~%Imported Macproject File:~%~A~%~%" file-name)))
      (if errors
          (values model (capi:display (make-instance 'macproject-errors :errors (reverse errors))))
        (values model nil)))
))

(capi:define-interface macproject-errors ()
  ((errors :initform nil :initarg :errors :accessor model-errors))
  (:panes
   (error-list capi:list-panel :items nil :accessor model-error-list :visible-min-width 220 :visible-min-height 200)
   (close-btn capi:push-button :text "Close" :selection-callback #'(lambda (data intf) (capi:destroy intf))))
  (:layouts
   (main-layout capi:column-layout '(error-list close-btn) :adjust :center))
  (:default-initargs
   :title "Macproject conflicts"
   :x (- (capi:screen-width *capi-screen*) 220)
   :y (- (capi:screen-height *capi-screen*) 200)
   :best-width 220
   :best-height 200
   :window-styles '(:toolbox :always-on-top)))

(defmethod initialize-instance :after ((obj macproject-errors) &rest initargs)
  (declare (ignore initargs))
  (capi:apply-in-pane-process (model-error-list obj)
                              #'(setf capi:collection-items)
                              (mapcar #'(lambda (x)
                                          (format nil "Model CP ~A: \"~A\""
                                                  (if (equal (second x) 'model) "contains" "lacks")
                                                  (first x)))
                                      (model-errors obj))
                              (model-error-list obj)))































