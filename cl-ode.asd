(asdf:defsystem cl-ode
  :depends-on (#:cffi)
  :components
  ((:file "cl-ode")
   (:file "bindings" :depends-on ("cl-ode"))
   (:file "exports" :depends-on ("cl-ode" "bindings"))
   (:file "accessors" :depends-on ("cl-ode" "bindings" "exports"))))
