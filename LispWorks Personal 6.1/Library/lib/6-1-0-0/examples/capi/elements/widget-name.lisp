;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/elements:widget-name.lisp,v 1.4.2.1 2011/08/24 13:26:21 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package "CL-USER")

#|

Example of using :widget-name to access Motif resources.

1. Load this file and then call this:

  (CAPI:DISPLAY (MAKE-INSTANCE 'AN-INTERFACE))


With the default resource file, the first two tiles have the default
background which is light beige (#fdf8f3) by default. The third one
has background LightGray, which is the default for collector-pane.



2. Now add these lines to the resource file:

! light pink
Lispworks*sobamorov.background:                                 #fdf8f3

! light brown
Lispworks*an-interface*collector-pane.background:               #fdf3d9



3. Restart your LispWorks image, and repeat step 1.


Now the second tile has background light pink, and the third one has
background light brown. The color of other collector-panes (e.g.  the
Output tab of the editor tool) is unchanged, because they are not
inside an 'an-interface'.
|#


(capi:define-interface an-interface ()
 ()
 (:panes
  (title1 
   capi:title-pane
   :text "No Widget name")
  (title2
   capi:title-pane
   :text "Widget name: sobamorov"
   :widget-name "sobamorov")
   (title3
   capi:title-pane
   :text "Widget name: collector-pane"
   :widget-name "collector-pane")))


