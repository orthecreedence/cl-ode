(in-package :cl-ode.bindings)

(defmacro make-accessors (c-struct)
  `(progn
     ,@(loop for slot-name in (foreign-slot-names c-struct)
	  for accessor-name = (intern (concatenate 'string (symbol-name c-struct)
						   -
						   (symbol-name slot-name)))
	  append (list `(defmacro ,accessor-name (ptr)
			  (list 'foreign-slot-value ptr '',c-struct '',slot-name))
		       `(export ',accessor-name)))))


(make-accessors '#.(chipmunk-lispify dJointFeedback 'classname)
(make-accessors '#.(chipmunk-lispify dSurfaceParameters 'classname)
(make-accessors '#.(chipmunk-lispify dContactGeom 'classname)
(make-accessors '#.(chipmunk-lispify dContact 'classname)
(make-accessors '#.(chipmunk-lispify dStopwatch 'classname)
(make-accessors '#.(chipmunk-lispify dMass 'classname)
(make-accessors '#.(chipmunk-lispify dGeomClass 'classname)
