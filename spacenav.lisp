;; spacenav.lisp
;;
;; Copyright (c) 2023 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package #:spacenav)

(cffi:define-foreign-library spacenav-lib
  (:darwin (:or "libspnav.dylib" "libspnav"))
  (:unix (:or "libspnav.so" "libspnav" "spnav"))
  (t (:default "libspnav")))
(cffi:use-foreign-library spacenav-lib)

(cffi:defcenum event-type :any :motion :button)

(declaim (optimize (speed 1) (space 3) (safety 1) (debug 1)))
(declaim (inline button-press-p button-release-p decode event poll-event wait-event))
(declaim (inline sn-open sn-close fd sensitivity spnav-poll-event spnav-wait-event))

(cffi:defcstruct sn-motion-event
  "Motion event from libspnav.  x, y, z, rx, ry, and rz \
range between -500 and 500.  Period is the duration since the previous event."
  (type :int)
  (x :int)
  (y :int)
  (z :int)
  (rx :int)
  (ry :int)
  (rz :int)
  (period :unsigned-int)
  (data :pointer))

(cffi:defcstruct sn-button-event
  "Button event from libspnav.  press is 1 if the button was pressed, 0 if released \
bnum is the button number."
  (type :int)
  (press :int)
  (bnum :int))

(cffi:defcunion spacenav-event
  (type :int)
  (motion (:struct sn-motion-event))
  (button (:struct sn-button-event)))


(cffi:defcfun ("spnav_open" sn-open) :int
  "Open the connection to spacenavd.")
(cffi:defcfun ("spnav_close" sn-close) :int
  "Close the connection to spacenavd.")

(cffi:defcfun ("spnav_fd" fd) :int
  "Return the underlying file descriptor used to communicate with spacenavd.")

(cffi:defcfun ("spnav_sensitivity" sensitivity) :int
  "Set the sensitivity of the space mouse."
  (sens :double))

(cffi:defcfun ("spnav_remove_events" remove-events) :int
  "Remove event o the specified type: :any :motion :button"
  (type event-type))



(cffi:defcfun "spnav_wait_event" :int
  (event (:pointer (:union spacenav-event))))

(cffi:defcfun "spnav_poll_event" :int
  (event (:pointer (:union spacenav-event))))

(cffi:defcfun ("spnav_protocol" protocol-version) :int
  "Return protocol version number.")

(cffi:defcfun ("spnav_client_name" set-client-name) :int
  (name :string))


(defun evmask (&optional (options '(:motion :button :dev :cfg :raw-axis :raw-button :input :default :all)))
  (apply #'logior
         (mapcar (lambda (opt)
                   (alexandria:assoc-value `((:motion .    #16R01)
                                             (:button .    #16R02)
                                             (:dev    .    #16R04)
                                             (:cfg    .    #16R08)
                                             (:raw-axis .  #16R10)
                                             (:raw-button . #16R20)
                                             (:input   .   ,(logior #16R01 #16R02))
                                             (:default .   ,(logior #16R01 #16R02 #16R04))
                                             (:all    .    #16Rffff))
                                           opt))
                 options)))

(cffi:defcfun ("spnav_evmask" set-evmask) :int
  (mask :unsigned-int))

(cffi:defcfun ("spnav_dev_name" dev-name) :int
  (buf :string) (bufsz :int))

(defun device-name ()
  (let* ((size (dev-name (cffi:null-pointer) 0))
         (dname (cffi:foreign-alloc :char :count (1+ size)))
         (size2 (dev-name dname size)))
    (when (not (= size size2))
      (error "Inconsistent size: ~a ~a" size size2))
    (let ((rval (cffi:foreign-string-to-lisp dname)))
      (cffi:foreign-free dname)
      (values rval size2))))

(cffi:defcfun ("spnav_dev_path" dev-path) :int
  (buf :string) (bufsz :int))

(defun device-path ()
  (let* ((size (dev-path (cffi:null-pointer) 0))
         (dpath (cffi:foreign-alloc :char :count (1+ size)))
         (size2 (dev-path dpath size)))
    (when (not (= size size2))
      (error "Inconsistent size: ~a ~a" size size2))
    (let ((rval (cffi:foreign-string-to-lisp dpath)))
      (cffi:foreign-free dpath)
      (values rval size2))))

(cffi:defcfun ("spnav_dev_buttons" device-buttons) :int)
(cffi:defcfun ("spnav_dev_axes" device-axes) :int)
(cffi:defcenum dev-type
  :unknown 
  ;; serial devices */
  (:Spaceball-1003/2003/2003C #16R100)
  :Spaceball-3003/3003C
  :Spaceball-4000FLX/5000FLX
  :Magellan-SpaceMouse
  :Spaceball-5000-spacemouse-protocol
  :3Dconnexion-CadMan-spacemouse-protocol
  ;; USB devices */
  (:SpaceMouse-Plus-XT #16R200)
  :3Dconnexion-CadMan-USB-version
   :SpaceMouse-Classic
  :Spaceball-5000-USB-version
  :Space-Traveller
  :Space-Pilot
  :Space-Navigator
  :Space-Explorer
  :Space-Navigator-for-Notebooks
  :Space-Pilot-pro
  :SpaceMouse-Pro
  :NuLOOQ
  :SpaceMouse-Wireless
  :SpaceMouse-Pro-Wireless
  :SpaceMouse-Enterprise
  :SpaceMouse-Compact
  :SpaceMouse-Module
  )
(cffi:defcfun ("spnav_dev_type" device-type) dev-type)

(cffi:defcenum led-state
  (:error -1)
  :off
  :on
  :auto)
(cffi:defcfun ("spnav_cfg_set_led" set-led) led-state
  (state led-state))
(cffi:defcfun ("spnav_cfg_get_led" get-led) led-state)

(cffi:defcenum grab-state
  (:error -1)
  (:no-grab 0)
  (:grab 1))
(cffi:defcfun ("spnav_cfg_set_grab" set-grab) grab-state
  (state grab-state))
(cffi:defcfun ("spnav_cfg_get_grab" get-grab) grab-state)

(cffi:defcfun ("spnav_dev_usbid" dev-usbid) :int
  (vendor (:pointer :unsigned-int))
  (prod (:pointer :unsigned-int)))

(defun device-usbid ()
  (cffi:with-foreign-objects ((vendor :unsigned-int 2)
                              (prod :unsigned-int 2))
    (dev-usbid vendor prod)
    (format nil
            "~4,'0x:~4,'0x"
            (cffi:mem-aref vendor :unsigned-int)
            (cffi:mem-aref prod :unsigned-int))))

(defclass motion-event ()
  ((x :initarg :x :type fixnum :initform 0)
   (y :initarg :y :type fixnum :initform 0)
   (z :initarg :z :type fixnum :initform 0)
   (rx :initarg :rx :type fixnum :initform 0)
   (ry :initarg :ry :type fixnum :initform 0)
   (rz :initarg :rz :type fixnum :initform 0)
   (period :initarg :period :type fixnum :initform 1))
  (:documentation "A 3D mouse motion event."))

(defmethod print-object ((object motion-event) stream)
  "Print a motion-event."
  (declare (type stream stream))
  (with-slots (x y z rx ry rz period) object
    (format stream
            "(:x ~a :y ~a :z ~a :rx ~a :ry ~a :rz ~a :period ~a)"
            x y z rx ry rz period)))

(defclass button-event ()
  ((press :initarg :press :type fixnum)
   (button :initarg :button :type fixnum))
  (:documentation "A 3D mouse button event."))

(defmethod print-object ((object button-event) stream)
  "Print a button-event."
  (declare (type stream stream))
  (with-slots (press button) object
    (format stream
            "(:press ~a :button ~a)"
            press button)))

(defgeneric button-press-p (event num)
  (:documentation "Return t if an event is a button press for button num."))


(defmethod button-press-p ((event button-event) (num integer))
  (and event
       (with-slots (press button) event
         (declare (type fixnum press button))
         (and (= button num)
              (= press 1)))))

(defmethod button-press-p ((event button-event) (bname symbol))
  (and event
       (with-slots (press button) event
         (declare (type fixnum press button))
         (let ((bnum (alexandria:assoc-value '((:left . 0)
                                               (:right . 1)
                                               (:menu . 0)
                                               (:fit . 1)
                                               (:alt . 23)
                                               (:ctrl . 25)
                                               (:shift . 24)
                                               (:esc . 22)
                                               (:one . 12)
                                               (:two . 13)
                                               (:three . 14)
                                               (:four . 15)
                                               (:rotate . 8)
                                               (:joystick . 26 )
                                               (:t . 2)
                                               (:f . 5)
                                               (:r . 4))
                                             bname)))
           (and bnum
                (= button
                   bnum)
                (= press 1))))))

(defgeneric button-release-p (event num)
  (:documentation "Return t if an event is a button press for button num."))

(defmethod button-release-p ((event t) num)
  (declare (ignore num))
  nil)

(defmethod button-release-p ((event button-event) (num integer))
  "Return t if event is a button release for button num."
  (and event
       (with-slots (press button) event
         (declare (type fixnum press button))
         (and (= button num)
              (= press 0)))))


(defun decode-event (event)
  "Convert a low level event into an object."
  (let ((type (cffi:foreign-slot-value event '(:union spacenav-event) 'type)))
    (cond ((= type 1)
           (cffi:with-foreign-slots ((x y z rx ry rz period) event (:struct sn-motion-event))
             (make-instance 'motion-event
                            :x x
                            :y y
                            :z z
                            :rx rx
                            :ry ry
                            :rz rz
                            :period period)))
          ((= type 2)
           (cffi:with-foreign-slots ((press bnum) event (:struct sn-button-event))
             (make-instance 'button-event
              :press press
              :button bnum)))
          (t nil))))

(defun poll-event ()
  "Return an event immediately.  If none are queued then return nil."
  (cffi:with-foreign-object (event '(:union spacenav-event))
    (let ((res (spnav-poll-event event)))
      (cond ((= res 0) nil)
            (t
             (decode-event event))))))

(defun wait-event ()
  "Return an event, and wait for one if none are available.  Return nil on error."
  (cffi:with-foreign-object (event '(:union spacenav-event))
    (let ((res (spnav-wait-event event)))
      (cond ((= res 0) nil)
            (t
             (decode-event event))))))

(defun debug-events ()
  "Loop until button one is released."
  (unwind-protect
       (let ((res (sn-open)))
         (when res
           (loop
             for event = (spacenav:wait-event)
             do (print event)
             while (and event
                        (not (button-release-p event 1)))
             do
                (print event))))
    (print "Closing ...")
    (sn-close)))

(push :spacenav *features*)
