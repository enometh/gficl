(in-package :gficl)

(defparameter *state* nil
   "store internal state of render")

(defclass render-state ()
  ((width :initarg :width :accessor win-width :type integer)
   (height :initarg :height :accessor win-height :type integer)
   (prev-x :initform 0 :accessor prev-x :type integer)
   (prev-y :initform 0 :accessor prev-y :type integer)
   (prev-width :initform 0 :accessor prev-width :type integer)
   (prev-height :initform 0 :accessor prev-height :type integer)
   (resize-fn :initarg :resize-fn :accessor resize-fn :type (function (integer integer)))
   (frame-time :initform (get-internal-real-time) :accessor frame-time :type integer)
   (prev-frame-time :initform (get-internal-real-time) :accessor prev-frame-time :type integer)
   (fullscreen :initform nil :accessor fullscreen :type boolean)
   (input :initform (make-instance 'input-state) :accessor render-input)
   (prev-input :initform (make-instance 'input-state) :accessor render-prev-input))
  (:documentation "stores interal state of the renderer"))

(defun update-render-state ()
  (update-input-state))

(defun update-frame-time ()
  (let ((dt 0))
    (setf (frame-time *state*) (get-internal-real-time))
    (setf dt (/ (- (frame-time *state*) (prev-frame-time *state*))
		internal-time-units-per-second))
    (setf (prev-frame-time *state*) (frame-time *state*))
    dt))

;; --- input state ---

(defun update-input-state ()
  (setf (slot-value (render-prev-input *state*) 'key-state)
	(alexandria:copy-hash-table (slot-value (render-input *state*) 'key-state)))
  (setf (slot-value (render-prev-input *state*) 'mouse-state)
	(alexandria:copy-hash-table (slot-value (render-input *state*) 'mouse-state)))
  ;;(setf (slot-value (render-input *state*) 'modifier-state) nil)
  (setf (slot-value (render-input *state*) 'scroll-state) nil))

(defclass input-state ()
  ((key-state :initform (make-hash-table) :type hash-table)
   (mouse-state :initform (make-hash-table) :type hash-table)
   (modifier-state :initform nil)
   (scroll-state :initform nil))
  (:documentation "store previous frame's pressed keys."))

(defun %update-modifier-state (input-state key-or-button action mod-keys)
  "Actually we don't use mod-keys at all. We only track press and release
events of a set of keys. def-key-callback and
def-mouse-button-callback only give us mod-keys with the release
event."
  (flet ((key-to-mod (k)
	   (case k
	     ((:left-shift :right-shift) :shift)
	     ((:left-alt :right-alt) :alt)
	     ((:left-control :right-control) :control)
	     ((:left-super :right-super) :super))))
    (let ((k (key-to-mod key-or-button)))
      (when k
	(ecase action
	  (:press (pushnew k (slot-value input-state 'modifier-state)))
	  (:release (setf (slot-value input-state 'modifier-state)
			  (delete k (slot-value input-state 'modifier-state)))))))))


(defgeneric update-key-state (state key action mod-keys)
  (:documentation "handle a key input state change."))

(defmethod update-key-state ((state input-state) key action mod-keys)
  (case action
	(:press (setf (gethash key (slot-value state 'key-state)) t))
	(:release (remhash key (slot-value state 'key-state))))
  (%update-modifier-state state key action mod-keys))

(defgeneric update-mouse-pos (state x y)
  (:documentation "handle a mouse position change"))

(defmethod update-mouse-pos ((state input-state) x y)
  (with-slots (mouse-state) state
    (setf (gethash :x mouse-state) x)
    (setf (gethash :y mouse-state) y)))

(defgeneric update-mouse-buttons (state button action mod-keys)
  (:documentation "handle a mouse button state change."))

(defmethod update-mouse-buttons ((state input-state) button action mod-keys)
  (with-slots (mouse-state) state
    (case action
	  (:press (setf (gethash button mouse-state) t))
	  (:release (remhash button mouse-state))))
  (%update-modifier-state state button action mod-keys))

(defgeneric update-scroll (state x y))

(defmethod update-scroll ((state input-state) x y)
  (with-slots (scroll-state) state
    (setq scroll-state
	  (cond ((zerop x)
		 (cond ((zerop y) (warn "impossible!"))
		       ((plusp y) :scroll-up)
		       (t :scroll-down)))
		((plusp x)
		 (cond ((zerop y) :scroll-left)
		       (t (warn "impossible!"))))
		(t (cond ((zerop y) :scroll-right)
			 (t (warn "impossible!"))))))))

;; --- hardware state ---

(defparameter *max-msaa-samples* 0)

(declaim (ftype (function (&optional integer) integer) msaa-samples))
(defun msaa-samples (&optional (samples 0))
  "Return the minimum of SAMPLES and maximum supported samples.
Returns maximum supported samples if SAMPLES is 0, or samples isn't passed"
  (if (= *max-msaa-samples* 0)
      (setf *max-msaa-samples* (gl:get-integer :max-samples)))
  (if (> samples 0) (min samples *max-msaa-samples*) *max-msaa-samples*))
