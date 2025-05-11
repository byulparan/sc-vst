# sc-vst

Additional library collection for [cl-collider](https://github.com/byulparan/cl-collider).  
it's binding for [vstplugin](https://github.com/Spacechild1/vstplugin).

## Usage:
```cl
;;; load on cl-collider and sc-extensions
(ql:quickload '(:cl-collider :sc-vst))

(in-package :sc-user)
(named-readtables:in-readtable :sc)

;;; bootup server
(setf *s* (make-external-server "Lisp-collider" :port 57880))
(server-boot *s*)



;; use ValhallaShimmer
(defsynth shimmer ()
  (out.ar 0 (sc-vst:vst-plugin.ar (in.ar 100 2) 2 :reverb)))

(defparameter *reverb* (sc-vst:vst-controller (synth 'shimmer) :reverb "ValhallaShimmer"))

(sc-vst:editor *reverb*)
(sc-vst:parameter-list *reverb*)

(sc-vst:parameter *reverb* 0)
(setf (sc-vst:parameter *reverb* 0) 1.0)


;; use Pianoteq7
(defsynth piano ()
  (out.ar 0 (sc-vst:vst-plugin.ar nil 2 :piano)))

(defparameter *piano* (sc-vst:vst-controller (synth 'piano) :piano "Pianoteq 7"))
(sc-vst:editor *piano*)

(sc-vst::note-on *piano* 1 50 100)
(sc-vst::note-off *piano* 1 50 100)
```

