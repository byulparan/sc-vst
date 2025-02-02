(in-package :sc-vst)

(defvar *vst-parameter-table* (make-hash-table :test #'equalp))
(defvar *vst-parameter-action* (make-hash-table))

(defvar *debug-paramger-changed* nil
  "Determines whether to output changes when VST parameter values are modified by user gestures.")

(defvar *initialize-vst-responder* nil)


(defun install-vst-responder ()
  (unless *initialize-vst-responder*
    (sc:add-reply-responder
     "/vst_set"
     (lambda (node synth-index index value)
       (declare (ignorable  synth-index index))
       (funcall (gethash node *vst-parameter-action*) value)))
    (sc:add-reply-responder
     "/vst_param"
     (lambda (node-id reply-id param-id value &rest rest)
       (declare (ignore node-id reply-id rest))
       (when *debug-paramger-changed*
	 (format t "~&parameter changed :id ~d :value ~f~%" param-id value))))
    (sc:add-reply-responder
     "/vst_auto"
     (lambda (node synth-index index value)
       (declare (ignorable node synth-index index value))))
    (sc:add-reply-responder
     "/vst_midi"
     (lambda (&rest param)
       (declare (ignore param))))
    (sc:add-reply-responder
     "/vst_update"
     (lambda (node val)
       (declare (ignore node val))))
    (sc:add-reply-responder
     "/vst_program"
     (lambda (&rest param)
       (declare (ignore param))))
    (sc:add-reply-responder
     "/vst_program_read"
     (lambda (node synth-index success)
       (declare (ignore synth-index success))))
    (sc:add-reply-responder
     "/vst_program_write"
     (lambda (node synth-index success)
       (declare (ignore synth-index success))))
    (setf *initialize-vst-responder* t)))

(flet ((uninitalize-vst ()
	 (setf *initialize-vst-responder* nil)))
  (push #'uninitalize-vst sc:*server-quit-hooks*))


(defun vst-search ()
  "Explores information about VST plugins installed on the system and caches the results."
  (sc:send-message sc:*s* "/cmd" "/vst_search" 7 -1 0 0))

(defclass vst-plugin (sc::multiout-ugen)
  ((id :initarg :id :accessor id)))

(defmethod sc::optimize-graph ((ugen vst-plugin))
  (setf (getf (sc::synthdef-metadata (sc::name sc::*synthdef*) :vst) (id ugen)) (sc::synth-index ugen)))



(defun vst-plugin.ar (in num-output id &key params)
  "This is a UGen that creates a VST. An appropriate id should be defined so that it can be controlled later by the vst-controller.

Examples:

(defsynth vst-synth ()
  ;; :your-vst-id is an identifier that can be arbitrarily defined to locate the VST to be controlled in the Synth.
  (let* ((sig (sc-vst:vst-plugin.ar nil 2 :your-vst-id)))
    (out.ar 0 sig)))
"
  (assert (evenp (length params)) nil "params should be list of key/value pair")
  (let* ((in (alexandria:ensure-list in))
	 (inputs (append (list 0 0 0 (if in 1 0)) (if in (cons (length in) in)) (list 1 num-output) (list (/ (length params) 2))
			 params)))
    (apply #'sc::new1 (make-instance 'vst-plugin
			:name "VSTPlugin"
			:id id
			:synthdef sc::*synthdef*
			:rate :audio
			:check-fn (lambda (ugen) ugen)
			:signal-range :bipolar)
	   num-output inputs)))




(defclass vst-controller ()
  ((name :initarg :name :reader name)
   (node :initarg :node :reader node)
   (index :initarg :index :reader index)
   (window :initarg :window :reader window)
   (latency :initarg :latency :reader latency)))


(defun update-parameter (name)
  (let* ((tmp-path (merge-pathnames (temporary-file::get-default-temporary-directory)
				    (temporary-file::generate-random-string))))
    (sc:send-message sc:*s* "/cmd" "/vst_query" name (namestring tmp-path))
    (sc:sync)
    (unwind-protect (with-open-file (stream tmp-path)
		      (loop until (string= "[parameters]" (read-line stream)))
		      (let* ((n (parse-integer (subseq (read-line stream) 2)))
			     (result nil))
			(dotimes (i n)
			  (let* ((param (read-line stream)))
			    (push (subseq param 0 (position #\, param)) result)))
			(setf (gethash name *vst-parameter-table*) (nreverse result))))
	 (delete-file tmp-path))))


(defun parameter-list (vst-controller)
  "Outputs the list of parameters for the specified VST from the cached data. As it cannot handle dynamically generated parameters, in such cases, the *debug-parameter-changed* variable should be utilized."
  (let* ((name (name vst-controller))
	 (result (gethash name *vst-parameter-table*)))
    (unless result
      (setf result (update-parameter name)))
    (loop for param in result
	  for i from 0
	  do (format t "[~d] ~a~%" i param))))


(defun parameter-map (vst-controller &rest pair-of-index-and-bus)
  "The parameters of the VST can be controlled using the values of bus objects. This can be utilized when you want to control parameters with UGens.

Examples:

(defvar *bus0* (sc:bus-control))
(defvar *bus1* (sc:bus-control))

(sc-vst:parameter-map *vst-controller* 0 *bus0* 10 *bus1*)
"
  (assert (evenp (length pair-of-index-and-bus)) nil "parameter map sholud be pair of index/bus")
  (let* ((node (node vst-controller))
	 (kr-vals nil)
	 (ar-vals nil))
    (loop for (index bus) on pair-of-index-and-bus by #'cddr
	  do (if (eql (slot-value bus 'sc::type) :control)
		 (alexandria:appendf kr-vals (list index (sc::busnum bus) (slot-value bus 'sc::chanls)))
	       (alexandria:appendf ar-vals (list index (sc::busnum bus) (slot-value bus 'sc::chanls)))))
    (when (> (length kr-vals) 0)
      (apply #'sc:send-message (sc::server node)  "/u_cmd" (sc::id node) (index vst-controller) "/map" kr-vals))
    (when (> (length ar-vals) 0)
      (apply #'sc:send-message (sc::server node)  "/u_cmd" (sc::id node) (index vst-controller) "/mapa" ar-vals)))
  vst-controller)


(defun parameter-unmap (vst-controller index &rest indices)
  "Disconnect the bus object from the parameter.

Examples:

(sc-vst:parameter-unmap *vst-controller* 0 10)
"
  (let* ((node (node vst-controller)))
    (apply #'sc:send-message (sc::server node)  "/u_cmd" (sc::id node) (index vst-controller) "/unmap" index indices)))




(defun vst-controller (synth id name)
  "Creates an object to control the VST included in the currently playing synth node.

Examples:

(defsynth vst-synth ()
  ;; :your-vst-id is an identifier that can be arbitrarily defined to locate the VST to be controlled in the Synth.
  (let* ((sig (sc-vst:vst-plugin.ar nil 2 :your-vst-id)))
    (out.ar 0 sig)))

(defparameter *vst* (sc-vst:vst-controller (synth 'vst-synth) :your-vst-id \"/PATH/TO/YOUR/VST.vst\"))

"
  (sc:sync)
  (assert (sc:is-playing-p synth) nil "Not running ~a" synth)
  (install-vst-responder)
  (let* ((synth-index (getf (sc::synthdef-metadata (sc::name synth) :vst) id))
	 (loaded nil)
	 (window nil)
	 (latency nil))
    (sc:add-reply-responder "/vst_open"
			 (lambda (&rest msg)
			   (setf loaded (if (zerop (floor (nth 2 msg))) nil t)
				 window (if (zerop (floor (nth 3 msg))) nil t)
				 latency (nth 4 msg))))
    (sc:send-message sc:*s* "/u_cmd" (sc::id synth) synth-index "/open" name 1 0 0)
    (sc:sync)
    (assert loaded nil "Failed loaded VSTPlugin ~s" name)
    (update-parameter name)
    (make-instance 'vst-controller
      :name name
      :node synth
      :index synth-index
      :window window
      :latency latency)))



(defun editor (vst-controller)
  "Display window of VST."
  (assert (sc:is-playing-p (node vst-controller)) nil "Not running ~a" (node vst-controller))
  (assert (window vst-controller) nil "VSTPlugin ~s don't have window" (name vst-controller))
  (sc:send-message sc:*s* "/u_cmd" (sc::id (node vst-controller)) (index vst-controller) "/vis" 1))



(defun parameter (vst-controller index &optional action)
  "Read / Write the VST parameter value. If no action argument is specified, it returns the value.

Examples:

(sc-vst:parameter *vst-controller* 0)

(sc-vst:parameter *vst-controller* 0 (lambda (v) (uiop:println v)))

(setf (sc-vst:parameter *vst-controller* 0) .7)
"
  (assert (sc:is-playing-p (node vst-controller)) nil "Not running ~a" (node vst-controller))
  (let* ((result nil)
	 (handle (or action (lambda (value) (setf result value)))))
    (setf (gethash (sc::id (node vst-controller)) *vst-parameter-action*) handle)
    (sc::message-distribute (node vst-controller) 
			  (list "/u_cmd" (sc::id (node vst-controller)) (index vst-controller) "/get" index)
			  sc:*s*)
    (unless action
      (sc:sync)
      result)))


(defun (setf parameter) (value vst-controller index)
  (assert (sc:is-playing-p (node vst-controller)) nil "Not running ~a" (node vst-controller))
  (sc::message-distribute (node vst-controller) 
			  (list "/u_cmd" (sc::id (node vst-controller)) (index vst-controller) "/set" index
				(min 1.0 (max .0 value) ))
			  sc:*s*))




(defun note-on (vst-controller chan note vel)
  "send NoteOn message to VST."
  (assert (sc:is-playing-p (node vst-controller)) nil "Not running ~a" (node vst-controller))
  (setf chan (alexandria:clamp (- chan 1) 0 15))
  (sc::message-distribute (node vst-controller) 
			  (list "/u_cmd" (sc::id (node vst-controller)) (index vst-controller)
				"/midi_msg" (vector (logior #x90 chan) note vel) 0.0)
			  sc:*s*))

(defun note-off (vst-controller chan note vel)
  "send NoteOff message to VST."
  (assert (sc:is-playing-p (node vst-controller)) nil "Not running ~a" (node vst-controller))
  (setf chan (alexandria:clamp (- chan 1) 0 15))
  (sc::message-distribute (node vst-controller) 
			  (list "/u_cmd" (sc::id (node vst-controller)) (index vst-controller)
				"/midi_msg" (vector (logior #x80 chan) note vel) 0.0)
			  sc:*s*))


(defun play-note (vst-controller beat chan note vel duration)
  "Sends a NoteOn message to the specified beat and automatically sends a NoteOff message after the specified duration.

Examples:

(sc-vst:play-note *vst-controller* (sc:clock-quant 1) 1 60 100 1)

"
  (sc:at-beat beat (note-on vst-controller chan note vel))
  (sc:at-beat (+ beat (* duration .9)) (note-off vst-controller chan note 0)))


(defun midi-cc (vst-controller chan control val)
  "send Control Change message to VST."
  (assert (sc:is-playing-p (node vst-controller)) nil "Not running ~a" (node vst-controller))
  (setf chan (alexandria:clamp (- chan 1) 0 15))
  (sc::message-distribute (node vst-controller) 
			  (list "/u_cmd" (sc::id (node vst-controller)) (index vst-controller)
				"/midi_msg" (vector (logior #xb0 chan) control val) 0.0)
			  sc:*s*))



(defun write-program (vst-controller path)
  "Store the current state of the VST as a file at the specified path."
  (assert (sc:is-playing-p (node vst-controller)) nil "Not running ~a" (node vst-controller))
  (sc:send-message sc:*s* "/u_cmd" (sc::id (node vst-controller)) (index vst-controller)
		   "/program_write" (sc::full-pathname path) 1))


(defun read-program (vst-controller path)
  "Restores the state of the VST saved at the specified path."
  (assert (sc:is-playing-p (node vst-controller)) nil "Not running ~a" (node vst-controller))
  (sc:send-message sc:*s* "/u_cmd" (sc::id (node vst-controller)) (index vst-controller)
		   "/program_read" (sc::full-pathname path) 1))



(defmethod sc::free ((object vst-controller))
  (sc:free (node object)))

(defmethod sc::floatfy ((object vst-controller))
  (node object))




