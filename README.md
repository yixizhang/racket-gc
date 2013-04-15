racket-gc
=========

This research is to compare the efficiency and number of heap operations between traditional mark-sweep and incremental mark-sweep strategy for the 2nd generation of Racket generational garbage collector.
The repo contains implementation of incremental, copying, generational garbage collector, and related mutator programs written in Racket's plai/gc2.

The speciality of this project is no intepreter or compiler is written or used, as all code is in Racket or subset of Racket itself.
The project also includes patches to plai/gc2 too.
