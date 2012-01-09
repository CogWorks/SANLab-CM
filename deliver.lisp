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

(load-all-patches)
(push ':DELIVERED *features*)

(setf *enable-sanlab-debugger* t)

#+win32
(require "ole")

(load (current-pathname "+ loader.lisp"))

#+:cocoa
(compile-file-if-needed
 (sys:example-file "configuration/macos-application-bundle")
 :load t)

(deliver 'sanlab
         #+win32
         "C:\\Program Files\\SANLab-CM\\SANLab-CM.exe"
         #+cocoa
         (CL-USER::write-macos-application-bundle
          "/Applications/SANLab-CM/SANLab-CM.app"
          :application-icns "~/Research/SANLab/v3.0/Resources/app2.icns"
          :document-types (list (list "SANLab-CM Model File" '("san") "~/Research/SANLab/v3.0/Resources/app2.icns")
                                (list "SANLab-CM Interactive Routine" '("sir") "~/Research/SANLab/v3.0/Resources/app2.icns")
                                (list "Legacy SANLab-CM Model File" '("mdl") "~/Research/SANLab/v3.0/Resources/app2.icns"))
          :version *SANLab-version*
          )
         0
         :interface :capi
         #+win32
         :icon-file
         #+win32
         "Resources\\SANLab.ico"
         :multiprocessing t
         :quit-when-no-windows nil
	 :keep-pretty-printer t
         )
(quit)

