%module bindings

%feature("intern_function", "swig-lispify-noprefix");

%insert("lisphead") %{
(in-package :cl-ode)

(cl:eval-when (:compile-toplevel :load-toplevel)
  (cl:unless (cl:fboundp 'swig-lispify-noprefix)
    (cl:defun chipmunk-lispify (name flag cl:&optional (package cl:*package*))
      (cl:labels ((helper (lst last rest cl:&aux (c (cl:car lst)))
                    (cl:cond
                      ((cl:null lst)
                       rest)
                      ((cl:upper-case-p c)
                       (helper (cl:cdr lst) 'upper
                               (cl:case last
                                 ((lower digit) (cl:list* c #\- rest))
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
           package))))))%}

%include "include/odeconfig.h"
%include "include/compatibility.h"
%include "include/common.h"
%include "include/odeinit.h"
%include "include/contact.h"
%include "include/error.h"
%include "include/memory.h"
%include "include/odemath.h"
%include "include/matrix.h"
%include "include/timer.h"
%include "include/rotation.h"
%include "include/mass.h"
%include "include/misc.h"
%include "include/objects.h"
#%include "include/odecpp.h"
%include "include/collision_space.h"
%include "include/collision.h"
#%include "include/odecpp_collision.h"
%include "include/export-dif.h"



