(in-package :sc-vst)

(defvar *vst-parameter-table* (make-hash-table :test #'equalp))
(defvar *vst-parameter-action* (make-hash-table))

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
     (lambda (&rest rest)
       (declare (ignore rest))))
    (sc:add-reply-responder
     "/vst_auto"
     (lambda (node synth-index index value)
       (declare (ignorable node synth-index index value))))
    (setf *initialize-vst-responder* t)))

(flet ((uninitalize-vst ()
	 (setf *initialize-vst-responder* nil)))
  (push #'uninitalize-vst sc:*server-quit-hooks*))


(defclass vst-plugin (sc::multiout-ugen)
  ((id :initarg :id :accessor id)))

(defmethod sc::optimize-graph ((ugen vst-plugin))
  (setf (getf (sc::synthdef-metadata (sc::name sc::*synthdef*) :vst) (id ugen)) (sc::synth-index ugen)))

(defun vst-plugin.ar (in num-output id)
  (let* ((in (alexandria:ensure-list in))
	 (inputs (append (list 0 0 0 (if in 1 0)) (if in (cons (length in) in)) (list 1 num-output) (list 0))))
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
  (let* ((name (name vst-controller))
	 (result (gethash name *vst-parameter-table*)))
    (unless result
      (setf result (update-parameter name)))
    (loop for param in result
	  for i from 0
	  do (format t "[~d] ~a~%" i param))))

(defun vst-controller (synth id name)
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
  (assert (sc:is-playing-p (node vst-controller)) nil "Not running ~a" (node vst-controller))
  (assert (window vst-controller) nil "VSTPlugin ~s don't have window" (name vst-controller))
  (sc:send-message sc:*s* "/u_cmd" (sc::id (node vst-controller)) (index vst-controller) "/vis" 1))


(defun parameter (vst-controller index &optional action)
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
  (sc::message-distribute (node vst-controller) 
			  (list "/u_cmd" (sc::id (node vst-controller)) (index vst-controller) "/set" index
				(min 1.0 (max .0 value) ))
			  sc:*s*))


(defun note-in (vst-controller chan note vel)
  (sc::message-distribute (node vst-controller) 
			  (list "/u_cmd" (sc::id (node vst-controller)) (index vst-controller)
				"/midi_msg" (vector (logior #x90 chan) note vel) 0.0)
			  sc:*s*))

(defun note-off (vst-controller chan note vel)
  (sc::message-distribute (node vst-controller) 
			  (list "/u_cmd" (sc::id (node vst-controller)) (index vst-controller)
				"/midi_msg" (vector (logior #x80 chan) note vel) 0.0)
			  sc:*s*))

(defun midi-cc (vst-controller chan control val)
  (sc::message-distribute (node vst-controller) 
			  (list "/u_cmd" (sc::id (node vst-controller)) (index vst-controller)
				"/midi_msg" (vector (logior #xb0 chan) control val) 0.0)
			  sc:*s*))

