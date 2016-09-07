%module bindings

%feature("intern_function", "swig-lispify-noprefix");

%insert("lisphead") %{
(in-package :cl-ode)

(cl:eval-when (:compile-toplevel :load-toplevel)
  (cl:unless (cl:fboundp 'swig-lispify-noprefix)
    (cl:defun swig-lispify-noprefix (name flag cl:&optional (package cl:*package*))
      (cl:labels ((helper (lst last rest cl:&aux (c (cl:car lst)))
                    (cl:cond
                      ((cl:null lst)
                       rest)
                      ((cl:upper-case-p c)
                       (helper (cl:cdr lst) 'upper
                               (cl:case last
                                 (lower (cl:list* c #\- rest))
                                 (cl:t (cl:cons c rest)))))
                      ((cl:lower-case-p c)
                       (helper (cl:cdr lst) 'lower (cl:cons (cl:char-upcase c) rest)))
                      ((cl:digit-char-p c)
                       (helper (cl:cdr lst) 'digit 
                               (cl:case last
                                 ((upper lower) (cl:list* c #\- rest))
                                 (cl:t (cl:cons c rest)))))
                      ((cl:char-equal c #\_)
                       (helper (cl:cdr lst) '_ (cl:cons #\- rest)))
                      (cl:t
                       (cl:error "Invalid character: ~A" c))))
                  (strip-prefix (prf str)
                    (let ((l (length prf)))
                      (if (and (> (length str) l) (string= prf (subseq str 0 l)))
                        (subseq str l)
                        str))))
        (cl:let ((fix (cl:case flag
                        ((constant enumvalue) "+")
                        (variable "*")
                        (cl:t ""))))
          (cl:intern
           (cl:concatenate
            'cl:string
            fix
            (cl:nreverse (helper (cl:concatenate 'cl:list (strip-prefix "d" name)) cl:nil cl:nil))
            fix)
           package))))))

(defun infinity (&optional (precision :single))
  (if (eql precision :single)
      (progn
        #+sbcl sb-ext:single-float-positive-infinity
        #+clozure 1S++0
        #+abcl ext:single-float-positive-infinity
        #+allegro excl::*infinity-single*
        #+cmu ext:single-float-positive-infinity
        #+(and ecl (not infinity-not-available)) si:single-float-positive-infinity
        #+lispworks (coerce infinity$$ 'single-float)
        #+scl ext:single-float-positive-infinity
        #+t most-positive-single-float)
      (progn
        #+sbcl sb-ext:double-float-positive-infinity
        #+clozure 1D++0
        #+abcl ext:double-float-positive-infinity
        #+allegro excl::*infinity-double*
        #+cmu ext:double-float-positive-infinity
        #+(and ecl (not infinity-not-available)) si:double-float-positive-infinity
        #+lispworks #.(read-from-string "10E999")
        #+scl ext:double-float-positive-infinity
        #+t most-positive-double-float)))
%}

%rename(dOdeError) dError;
extern void dError(int, const char *);

%rename(dOdeDebug) dDebug;
extern void dDebug(int, const char *);

%rename(dOdeMessage) dMessage;
extern void dMessage(int, const char *);

%include "tmp/include/odeconfig.h"
%include "tmp/include/compatibility.h"
%include "tmp/include/common.h"
%include "tmp/include/odeinit.h"
%include "tmp/include/contact.h"
%include "tmp/include/error.h"
%include "tmp/include/memory.h"
%include "tmp/include/odemath.h"
%include "tmp/include/matrix.h"
%include "tmp/include/timer.h"
%include "tmp/include/rotation.h"
%include "tmp/include/mass.h"
%include "tmp/include/misc.h"
%include "tmp/include/objects.h"
%include "tmp/include/collision_space.h"
%include "tmp/include/collision_trimesh.h"
%include "tmp/include/collision.h"
%include "tmp/include/export-dif.h"
%include "tmp/include/objects.h"

