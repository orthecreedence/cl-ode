(defpackage #:cl-ode
  (:use :cl :cffi))

(defpackage #:cl-ode.bindings
  (:use :cl :cffi)
  (:nicknames :ode))

(in-package :cl-ode)

(define-foreign-library ode
  (:unix (:or "libode.so" "libode.dylib"))
  (:windows "ode.dll")
  (t (:default "ode")))

(use-foreign-library ode)


