(in-package :cl-ode)

(defmacro make-accessors (c-struct)
  `(progn
     ,@(loop for slot-name in (foreign-slot-names c-struct)
	  for accessor-name = (intern (concatenate 'string (symbol-name c-struct)
						   "-"
						   (symbol-name slot-name)))
	  append (list `(defmacro ,accessor-name (ptr)
			  (list 'foreign-slot-value ptr '',c-struct '',slot-name))
		       `(export ',accessor-name)))))


(make-accessors #.(swig-lispify-noprefix "dJointFeedback" 'classname))
(make-accessors #.(swig-lispify-noprefix "dSurfaceParameters" 'classname))
(make-accessors #.(swig-lispify-noprefix "dContactGeom" 'classname))
(make-accessors #.(swig-lispify-noprefix "dContact" 'classname))
(make-accessors #.(swig-lispify-noprefix "dStopwatch" 'classname))
(make-accessors #.(swig-lispify-noprefix "dMass" 'classname))
(make-accessors #.(swig-lispify-noprefix "dGeomClass" 'classname))
