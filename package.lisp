(defpackage #:sc-vst
  (:use #:cl)
  (:export #:vst-plugin.ar
	   #:vst-controller
	   #:node
	   #:parameter-list
	   #:parameter
	   #:editor
	   #:note-on
	   #:note-off
	   #:midi-cc))
