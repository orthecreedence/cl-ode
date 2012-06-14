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
    ./generate/generate [/path/to/ode-install [infinity-value]]

The ODE install path defaults to /usr/local/ode, and the infinity value defaults to 1E++0
(I mainly use Clozure CL).

## Cross implementation notes
There is one horrible caveat. Common Lisp does not have a standard value for "infinity"
which is part of ODE. What this means is it's nearly impossible to come up with a cross-
implementation way of representing the infinity value while mapping it to the ODE internals
portably. 

What's more is ODE can be compiled with single or double precision, making the infinity value
yet even more complicated.

If you need to use the infinity value in ODE, I suggest you compile with -DINFINITY=[value]
and then regenerate the bindings as noted above, using the same infinity value for your 
bindings as compiled with ODE.
