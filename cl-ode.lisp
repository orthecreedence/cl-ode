(defpackage #:cl-ode
  (:use :cl :cffi)
  (:nicknames :ode))

(in-package :cl-ode)

(define-foreign-library ode
  (:unix (:or "libode.so" "/usr/local/lib/libode.so" "libode.dylib"))
  (:windows "ode.dll")
  (t (:default "ode")))

(use-foreign-library ode)


