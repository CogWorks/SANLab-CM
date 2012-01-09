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

(defparameter *invisible-output-pane* nil)
(defparameter *capi-screen* nil)
(defparameter *drag-image* nil)
(defparameter *compile-dir* (directory-namestring (current-pathname)))
(defparameter *app-path* nil)
(defparameter *PROGRAM-DIRECTORY* "")
(defparameter *compile-directory* (directory-namestring (current-pathname)))
(defparameter *app-type* nil)
(defparameter *operating-system*
#+cocoa
'macosx
#-cocoa
'windows
)
(defparameter *debug-path* (cond ((equal *operating-system* 'macosx) "~/Library/SANLab-CM/debug.log")
                                 ((equal *operating-system* 'windows) (merge-pathnames "SANLab-CM/debug.log" (sys:get-folder-path :appdata)))))