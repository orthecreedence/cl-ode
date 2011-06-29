(in-package :cl-ode)

(defmacro make-accessors (c-struct)
  `(progn
     ,@(loop for slot-name in (foreign-slot-names c-struct)
	  for accessor-name = (intern (concatenate 'string (symbol-name c-struct)
						   -
						   (symbol-name slot-name)))
	  append (list `(defmacro ,accessor-name (ptr)
			  (list 'foreign-slot-value ptr '',c-struct '',slot-name))
		       `(export ',accessor-name)))))


(make-accessors '#.(swig-lispify-noprefix "dJointFeedback"))
(make-accessors '#.(swig-lispify-noprefix "dSurfaceParameters"))
(make-accessors '#.(swig-lispify-noprefix "dContactGeom"))
(make-accessors '#.(swig-lispify-noprefix "dContact"))
(make-accessors '#.(swig-lispify-noprefix "dStopwatch"))
(make-accessors '#.(swig-lispify-noprefix "dMass"))
(make-accessors '#.(swig-lispify-noprefix "dGeomClass"))

