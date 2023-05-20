;; package.lisp
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


(in-package :cl-user)
(defpackage :spacenav.test
  (:use :cl :fiveam :spacenav))

(in-package :spacenav.test)

(def-suite :spacenav)
(in-suite :spacenav)

(test button-press-predicate
  (let ((events
          (list
           (list (make-instance 'sn:button-event :press 0 :button 0) 0 nil)
           (list (make-instance 'sn:button-event :press 1 :button 0) 0 t)
           (list (make-instance 'sn:button-event :press 1 :button 1) 1 t)
           (list (make-instance 'sn:button-event :press 0 :button 1) 1 nil))))
    (dolist (test-data events)
      (is (eq (sn:button-press-p (car test-data)
                                 (cadr test-data))
              (caddr test-data))))))

(test button-release-predicates
  (let ((events
          (list
           (list (make-instance 'sn:button-event :press 0 :button 0) 0 t)
           (list (make-instance 'sn:button-event :press 1 :button 0) 0 nil)
           (list (make-instance 'sn:button-event :press 1 :button 1) 1 nil)
           (list (make-instance 'sn:button-event :press 0 :button 1) 1 t))))
    (dolist (test-data events)
      (is (eq (sn:button-release-p (car test-data)
                                   (cadr test-data))
              (caddr test-data))))))

(test motion-decode
  (flet ((make-and-decode-motion-event (&key (x 0) (y 0) (z 0) (rx 0) (ry 0) (rz 0) (period 1))
           (cffi:with-foreign-object (me '(:union sn::spacenav-event))
             (setf (cffi:foreign-slot-value me '(:struct sn::sn-motion-event) 'sn::type) 1)
             (setf (cffi:foreign-slot-value me '(:struct sn::sn-motion-event) 'sn::x) x)
             (setf (cffi:foreign-slot-value me '(:struct sn::sn-motion-event) 'sn::y) y)
             (setf (cffi:foreign-slot-value me '(:struct sn::sn-motion-event) 'sn::z) z)
             (setf (cffi:foreign-slot-value me '(:struct sn::sn-motion-event) 'sn::rx) rx)
             (setf (cffi:foreign-slot-value me '(:struct sn::sn-motion-event) 'sn::ry) ry)
             (setf (cffi:foreign-slot-value me '(:struct sn::sn-motion-event) 'sn::rz) rz)
             (setf (cffi:foreign-slot-value me '(:struct sn::sn-motion-event) 'sn::period) period)
             (sn::decode-event me)))
         (motion-eq (a b)
           (every (lambda (sv)
                    (eq (slot-value a sv) (slot-value b sv)))
                  '(sn::x sn::y sn::z sn::rx sn::ry sn::rz sn::period))))
    (is (motion-eq (make-and-decode-motion-event)
                   (make-instance 'sn:motion-event)))

    (is (motion-eq (make-and-decode-motion-event :x 23)
                   (make-instance 'sn:motion-event :x 23)))
    (is (motion-eq (make-and-decode-motion-event :y 34)
                        (make-instance 'sn:motion-event :y 34)))
    (is (motion-eq (make-and-decode-motion-event :z 34)
                        (make-instance 'sn:motion-event :z 34)))
    (is (motion-eq (make-and-decode-motion-event :rx 23)
                   (make-instance 'sn:motion-event :rx 23)))
    (is (motion-eq (make-and-decode-motion-event :ry 34)
                        (make-instance 'sn:motion-event :ry 34)))
    (is (motion-eq (make-and-decode-motion-event :rz 34)
                        (make-instance 'sn:motion-event :rz 34)))

    (is (not (motion-eq (make-and-decode-motion-event :y 34)
                        (make-instance 'sn:motion-event))))))

(test button-decode
  (flet ((make-and-decode-button-event (button press)
           (cffi:with-foreign-object (be '(:union sn::spacenav-event))
             (setf (cffi:foreign-slot-value be '(:struct sn::sn-button-event) 'sn::type) 2)
             (setf (cffi:foreign-slot-value be '(:struct sn::sn-button-event) 'sn::press) press)
             (setf (cffi:foreign-slot-value be '(:struct sn::sn-button-event) 'sn::bnum) button)
             (sn::decode-event be))))
    (is-true (sn:button-press-p (make-and-decode-button-event 0 1) 0))
    (is-false (sn:button-press-p (make-and-decode-button-event 0 0) 0))
    (is-true (sn:button-release-p (make-and-decode-button-event 0 0) 0))
    (is-true (sn:button-release-p (make-and-decode-button-event 8 0) 8))
    (is-true (sn:button-press-p (make-and-decode-button-event 8 1) 8))))
