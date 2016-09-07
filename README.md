cl-ode
======

This is a set of bindings for the Open Dynamics Engine. It also includes the scripts 
I used to generate the bindings. The scripts/conventions are heavily "borrowed" from
[clipmunk, CFFI binding to the Chipmunk physics engine](https://github.com/fred-o/clipmunk)
(thanks, fred-o =]).

There are different branches in this project representing which release of ODE it's
wrapping. The current and most recent is 0.12.

## Generating the bindings
I made it easy for people with POSIX-type systems (unix, linux, cygwin, etc) to regenerate
the bindings for themselves:

    cd /path/to/cl-ode
    ./generate/generate [/path/to/ode-install [infinity precision]]

The ODE install path defaults to /usr/local/ode, and the infinity precision defaults to
:single (other possible value is :double). See below for infinity notes.

## Cross implementation notes
There is one caveat. Common Lisp does not have a standard value for "infinity" which is
part of ODE. What this means is it's nearly impossible to come up with a cross-
implementation way of representing the infinity value while mapping it to the ODE internals
portably.

What's more is ODE can be compiled with single or double precision, making the infinity value
yet even more complicated.

So while the bindings let you specify whether you use :single or :double precision, it doesn't
mean your implementation will play nicely with ODE via CFFI. For instance, the Clozure CL
single float infinity value 1E++0 maps *exactly* to (float)(1.0 / 0.0) in C. In SBCL, sending
sb-ext:single-float-positive-infinity to a C function results in a divide by zero. Not sure
if there is a way around this, I don't use SBCL much.

The best solution, if you even *need* infinity,  is probably for you to compile ODE with 
-DINFINITY=[your val] and update the "infinity" function in bindings.lisp to return this
value.
