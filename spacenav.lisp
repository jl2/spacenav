;; spacenav.lisp
;;
;; Copyright (c) 2021 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

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

(declaim (optimize (speed 3) (space 3)))
(declaim (inline button-press-p button-release-p decode event poll-event wait-event))
(declaim (inline sp-open sp-close fd sensitivity spnav-poll-event spnav-wait-event))

(cffi:defcstruct sp-motion-event
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

(cffi:defcstruct sp-button-event
  "Button event from libspnav.  press is 1 if the button was pressed, 0 if released \
bnum is the button number."
  (type :int)
  (press :int)
  (bnum :int))

(cffi:defcunion spacenav-event
  (type :int)
  (motion (:struct sp-motion-event))
  (button (:struct sp-button-event)))


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

(defclass motion-event ()
  ((x :initarg :x :type fixnum)
   (y :initarg :y :type fixnum)
   (z :initarg :z :type fixnum)
   (rx :initarg :rx :type fixnum)
   (ry :initarg :ry :type fixnum)
   (rz :initarg :rz :type fixnum)
   (period :initarg :period :type fixnum))
  (:documentation "A 3D mouse motion event."))

(defmethod print-object ((object motion-event) stream)
  "Print a motion-event."
  (declare (type stream stream))
  (with-slots (x y z rx ry rz period) object
    (format stream
            "(:x ~a :y ~a :z ~a :rx ~a :ry ~a :rz ~a :period ~a )"
            x y z rx ry rz period)))

(defclass button-event ()
  ((press :initarg :press)
   (button :initarg :button))
  (:documentation "A 3D mouse button event."))

(defmethod print-object ((object button-event) stream)
  "Print a button-event."
  (declare (type stream stream))
  (with-slots (press button) object
    (format stream
            "(:press ~a :button ~a)"
            press button)))



(defun button-press-p (event num)
  "Return t if an event is a button press for button num."
  (declare (type fixnum num)
           (type (or null motion-event button-event) event))
  (and event
       (eq 'button-event (type-of event))
       (with-slots (press button) event
         (declare (type fixnum button press))
         (and (= button num)
              (= press 1)))))

(defun button-release-p (event num)
  "Return t if event is a button release for button num."
  (declare (type fixnum num)
           (type (or null motion-event button-event) event))
  (and event
       (eq 'button-event (type-of event))
       (with-slots (press button) event
         (declare (type fixnum button press))
         (and (= button num)
              (= press 0)))))

(defun decode-event (event)
  "Convert a low level event into an object."
  (let ((type (cffi:foreign-slot-value event '(:union spacenav-event) 'type)))
    (cond ((= type 1)
           (cffi:with-foreign-slots ((x y z rx ry rz period) event (:struct motion-event))
             (make-instance 'motion-event
                            :x x
                            :y y
                            :z z
                            :rx rx
                            :ry ry
                            :rz rz
                            :period period)))
          ((= type 2)
           (cffi:with-foreign-slots ((press bnum) event (:struct button-event))
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
    (format  t "Closing ...~%")
    (sn-close)))