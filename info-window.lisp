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

(capi:define-interface model-properties-window (interface-view)
  ()
  (:panes
   (author-pane
    capi:text-input-pane
    :accessor author-pane)
   (title-pane
    capi:text-input-pane
    :accessor title-pane)
   (width-pane
    capi:text-input-pane
    :accessor width-pane)
   (height-pane
    capi:text-input-pane
    :accessor height-pane)
   (notes-pane
    capi:text-input-pane
    :accessor notes-pane))
  (:layouts
   (main-layout
    capi:column-layout
    
)