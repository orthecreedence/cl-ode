#!/bin/bash
# 
# This is a script to automate the generation of bindings to the Open Dynamics
# Engine (ODE) c-api for Common Lisp. It uses swig for the binding generation
# and some helpful scripting to create the exports and accessors. A lot of the
# ideas (namely accessors and lispifying of names) comes from the clipmunk
# project which provides bindings for the Chipmunk Physics Engine (thanks!).
# 
# Some of the code from ODE doesn't play nicely with swig, so we do some small
# source-code tweaks in odemath.h to remove some __inline directives. It does 
# this by copying all ODE header files to a local temporary directory and doing
# any replacements on those as opposed to modifying installed headers.
# 
# The hope with this script is that you have one command to generate bindings
# for ANY ODE version (not just the one I used it with, 0.11.1) without ANY
# modifications whatsoever.
# 
# Andrew Lyon <orthecreedence@gmail.com>


# init the script and make sure the paths are correct
# -------------------------------------

PREFIX=$1

if [ "`dirname $0`" == "." ]; then
    echo "Please run this script from the main directory (ie ./generate/generate.sh) instead of in the "generate" folder."
    exit 1
fi

GENDIR=`dirname $0`

# get default prefix if not specified
if [ "$PREFIX" == "" ]; then
	echo
	echo "NOTE: Prefix not passed as first argument, assuming /usr/local as ODE install prefix."
	echo
	PREFIX="/usr/local"
fi

# verify the prefix
if [ ! -d $PREFIX/include/ode ]; then
	echo
	echo "ODE installation not found under $PREFIX. Please specify the correct install prefix as the first argument:"
	echo
	echo "   $0 /path/to/ode"
	echo
    exit
fi

# necessary source code refinements
# -------------------------------------

# copying includes to local tmp directory so we can do text replacements on them (namely the
# odemath.h file has an __inline directive in two functions that swig doesn't take too kindly
# to.
echo -n "Creating local version of ODE headers to do some necessary text replacements..."
mkdir -p $GENDIR/tmp/include
cp -R $PREFIX/include/ode/* $GENDIR/tmp/include
sed -i 's|__inline\s*||g' $GENDIR/tmp/include/odemath.h
sed -i 's|dAllocateMaskAll\s*=\s*~0U|dAllocateMaskAll = -1|' $GENDIR/tmp/include/odeinit.h
echo "done."

# binding generation
# -------------------------------------

# create the actual swig bindings.
echo -n "Creating ODE bindings in bindings.lisp..."

# fix enums
patch -p0 $GENDIR/tmp/include/collision.h < $GENDIR/collision.h.patch > /dev/null

swig -cffi $GENDIR/cl-ode.i > /dev/null
mv $GENDIR/bindings.lisp .

# fix some swig problems in bindings
sed -i 's|dFirstSpaceCass|dFirstSpaceClass|' bindings.lisp
sed -i 's|dQadTreeSpaceCass|dQuadTreeSpaceClass|' bindings.lisp
sed -i 's|dMaxserCasses|dMaxUserClasses|' bindings.lisp
sed -i 's|dFirstserCass|dFirstUserClass|' bindings.lisp
sed -i "s|#.\(d[a-z]\+\)|#.(swig-lispify-noprefix \"\1\" 'enumvalue)|i" bindings.lisp
sed -i "s|\s\(d[A-Z][A-Za-z]\+\)| #.(swig-lispify-noprefix \"\1\" 'enumvalue)|g" bindings.lisp

echo "done."

# done, now generate the exports.lisp file
echo -n "Creating exported struct/function list in exports.lisp..."

# create accessors and bindings
# -------------------------------------

# start by creating new versions of the exports.lisp and accessors.lisp files with
# their headers
echo "(in-package :cl-ode)" > exports.lisp
echo >> exports.lisp
echo "(in-package :cl-ode)

(defmacro make-accessors (c-struct)
  \`(progn
     ,@(loop for slot-name in (foreign-slot-names c-struct)
	  for accessor-name = (intern (concatenate 'string (symbol-name c-struct)
						   \"-\"
						   (symbol-name slot-name)))
	  append (list \`(defmacro ,accessor-name (ptr)
			  (list 'foreign-slot-value ptr '',c-struct '',slot-name))
		       \`(export ',accessor-name)))))

" > accessors.lisp

# create structure (or classname) exports/accessors
for STRUCT in `cat bindings.lisp | grep "defcstruct" | sed "s|.*\"\([^\"]\+\)\".*|\1|g"`; do
    echo "(make-accessors #.(swig-lispify-noprefix \"$STRUCT\" 'classname))" >> accessors.lisp
    echo "(cl:export '#.(swig-lispify-noprefix \"$STRUCT\" 'classname))" >> exports.lisp
done

# create function binding exports
echo >> exports.lisp
for FUNC in `cat bindings.lisp | grep "defcfun" | sed "s|.*\"\([^\"]\+\)\".*|\1|g"`; do
    echo "(cl:export '#.(swig-lispify-noprefix \"$FUNC\" 'function))" >> exports.lisp
done
echo "done."

# cleanup
# -------------------------------------

echo "Removing temp files."
rm -rf $GENDIR/tmp/

echo "Finished."

