#!/bin/bash

echo -n "Creating ODE bindings in bindings.lisp..."
swig -cffi cl-ode.i
echo "done."

# done, now generate the exports.lisp file
echo -n "Now creating exported struct/function list in exports.lisp..."

echo "(in-package :cl-ode.bindings)" > exports.lisp
echo "(in-package :cl-ode.bindings)

(defmacro make-accessors (c-struct)
  \`(progn
     ,@(loop for slot-name in (foreign-slot-names c-struct)
	  for accessor-name = (intern (concatenate 'string (symbol-name c-struct)
						   "-"
						   (symbol-name slot-name)))
	  append (list \`(defmacro ,accessor-name (ptr)
			  (list 'foreign-slot-value ptr '',c-struct '',slot-name))
		       \`(export ',accessor-name)))))

" > accessors.lisp
echo >> exports.lisp
for STRUCT in `cat bindings.lisp | grep "defcstruct" | sed "s|.*\"\([^\"]\+\)\".*|\1|g"`; do
    echo "(make-accessors '#.(chipmunk-lispify "$STRUCT" 'classname)" >> accessors.lisp
    echo "(cl:export '#.(swig-lispify-noprefix \"$STRUCT\" 'classname))" >> exports.lisp
done
echo >> exports.lisp
for FUNC in `cat bindings.lisp | grep "defcfun" | sed "s|.*\"\([^\"]\+\)\".*|\1|g"`; do
    echo "(cl:export '#.(swig-lispify-noprefix \"$FUNC\" 'function))" >> exports.lisp
done

echo "done."

