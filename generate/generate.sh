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
INFINITY=$2

if [ "`dirname $0`" == "." ]; then
    echo "Please run this script from the main directory (ie ./generate/generate.sh) instead of in the "generate" folder."
    exit 1
fi

GENDIR=`dirname $0`

# get default prefix if not specified
if [ "$PREFIX" == "" ]; then
	echo "NOTE: Prefix not passed as first argument, assuming /usr/local/ode as ODE install prefix."
	PREFIX="/usr/local/ode"
fi

if [ "$INFINITY" == "" ]; then
	echo "NOTE: Common Lisp infinity value not passed as second value. Defaulting to 1E++0"
	INFINITY=1E++0
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
patch -p0 $GENDIR/tmp/include/collision.h < $GENDIR/collision.h.patch > /dev/null
patch -p0 $GENDIR/tmp/include/odeinit.h < $GENDIR/odeinit.h.patch > /dev/null
patch -p0 $GENDIR/tmp/include/common.h < $GENDIR/common.h.patch > /dev/null
echo "done."

# binding generation
# -------------------------------------

# create the actual swig bindings.
echo -n "Creating ODE bindings in bindings.lisp..."

# fix enums
#patch -p0 $GENDIR/tmp/include/collision.h < $GENDIR/collision.h.patch > /dev/null

swig -cffi $GENDIR/cl-ode.i > /dev/null
mv $GENDIR/bindings.lisp .

# fix some swig problems in bindings
sed -i 's|Firstser|FirstUser|' bindings.lisp
sed -i 's|Maxser|MaxUser|' bindings.lisp
sed -i 's|Cass|Class|' bindings.lisp
sed -i "s|#.\(d[a-z]\+\)|#.(swig-lispify-noprefix \"\1\" 'enumvalue)|i" bindings.lisp
sed -i "s|\s\(d[A-Z][A-Za-z]\+\)| #.(swig-lispify-noprefix \"\1\" 'enumvalue)|g" bindings.lisp
sed -i "s|\#\.\~0|#.-1|" bindings.lisp					# fixed this in odeinit.h.patch, but for some reason swig ignores it.
sed -i "s|(cl:/ 1.0d0 0.0d0)|$INFINITY|" bindings.lisp	# not portable, please fix
#sed -i "s|(cffi:defcenum.*\"dJointType\" 'enumname.*|(defanonenum|" bindings.lisp

echo "done."

# done, now generate the exports.lisp file
echo -n "Creating exported struct/function list in exports.lisp..."

# create accessors and bindings
# -------------------------------------

# start by creating new versions of the exports.lisp and accessors.lisp files with
# their headers
echo "(in-package :cl-ode)

(cl:export 'swig-lispify-noprefix)
" > exports.lisp
echo >> exports.lisp
echo "(in-package :cl-ode.accessors)

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

# export useful enums
echo >> exports.lisp
TMPIFS=$IFS
IFS=$'\n'		# loop over lines, not words
#for ENUM in `cat bindings.lisp | egrep 'dJointType[A-Z]' | sed "s|.*\(#\.([^)]\+)\).*|'\1|" | sed "s|\s*:keyword||"`; do
#	echo "(cl:export $ENUM)" >> exports.lisp
#done
echo "(cl:export '#.(swig-lispify-noprefix \"dJointType\" 'enumname))" >> exports.lisp
echo >> exports.lisp
for ENUM in `cat bindings.lisp | egrep 'dParam.*enumvalue\)' | sed "s|.*\(#\.([^)]\+)\).*|'\1|"`; do
	echo "(cl:export $ENUM)" >> exports.lisp
done
echo >> exports.lisp
for ENUM in `cat bindings.lisp | egrep 'dContact[A-Z].*enumval' | sed "s|.*\(#\.([^)]\+)\).*|'\1|"`; do
	echo "(cl:export $ENUM)" >> exports.lisp
done
IFS=$TMPIFS

# create function binding exports
echo >> exports.lisp
for FUNC in `cat bindings.lisp | grep "defcfun" | sed "s|.*\"\([^\"]\+\)\".*|\1|g"`; do
    echo "(cl:export '#.(swig-lispify-noprefix \"$FUNC\" 'function))" >> exports.lisp
done
echo "done."

# cleanup
# -------------------------------------

echo "Removing temp files."
#rm -rf $GENDIR/tmp/

echo "Finished."

