(defpackage #:sc-vst
  (:use #:cl)
  (:export #:vst-search
	   #:vst-plugin.ar
	   #:vst-controller
	   #:node
	   #:*debug-paramger-changed*
	   #:parameter-list
	   #:parameter
	   #:parameter-map
	   #:parameter-unmap
	   #:editor
	   #:note-on
	   #:note-off
	   #:play-note
	   #:midi-cc
	   #:write-program
	   #:read-program))
