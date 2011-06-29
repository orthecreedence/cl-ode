(asdf:defsystem cl-ode
  :depends-on (#:cffi)
  :components
  ((:file "ode")
   (:file "bindings" :depends-on ("ode"))
   (:file "exports" :depends-on ("ode" "bindings"))
   (:file "accessors" :depends-on ("ode" "bindings" "exports"))))
