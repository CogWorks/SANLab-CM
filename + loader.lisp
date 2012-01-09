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

; Loader file for SANLab v3.0

(declaim (optimize (safety 1) (speed 3) (space 0)))
(defconstant *SANLab-version* "3.0.0")

(defsystem "SANLab-Files"
  (:package CL-USER)
  :members ("globals.lisp"
            "properties.lisp"
            "pointers.lisp"
            "common.lisp"
            "classes.lisp"
            "interactive-routine.lisp"
            "distribution.lisp"
            "activity-type.lisp"
            "model.lisp"
            "command.lisp"
            "actr-trace.lisp"
            "view-classes.lisp"
            "controller.lisp"
            "loadsave.lisp"
            "sanfile.lisp"
            "iroutines.lisp"
            "criticality.lisp"
            "network-processor.lisp"
            "results-viewer.lisp"
            "histpane.lisp"
            "histogram.lisp"
            "miniviewer.lisp"
            "activity-pane.lisp"
            "arrow.lisp"
            "multiline-pinboard-object.lisp"
            "activity-layout.lisp"
            "toolbox.lisp"
            "draggable-list.lisp"
            "graph-pane.lisp"
            "model-prop-window.lisp"
            "editor.lisp"
; The activity editor is disabled until it can be rewritten to support SANLab-CM 3.0
;            "actedit.lisp"
            "distedit.lisp"
            "macproject.lisp"
            "splash.lisp"
            "about.lisp"
            "app-window.lisp"
            "sanlab.lisp")
  :rules ((:in-order-to :compile :all
           (:requires (:load :previous)))
          (:in-order-to :load :all
           (:requires (:load :previous)))
          )
  )

(compile-system "SANLab-Files")
(load-system "SANLab-Files")
