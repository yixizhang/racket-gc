Garbage Collector Evaluation
=============================

Goal
----
This project is for my master's thesis at EECS Northwestern University, which is to evaluate the efficiency and heap layout when running different garbage collectors with different memory allocation patterns, so that we can give suggestions on how to choose the right garbage collector with cogent reasoning and benchmarks.

Resources
---------
To build garbage collector that's easy to benchmark and not have to touch interpreter or compiler, I referred to:

* [Racket](https://github.com/plt/racket) :: `plai/gc2`
* [Teaching Garbage Collection without Implementing Compilers or Interpreters](http://www.eecs.northwestern.edu/~robby/pubs/papers/cooper-sigcse2013.pdf)
* [Uniprocessor Garbage Collection Techniques](https://ritdml.rit.edu/bitstream/handle/1850/5112/PWilsonProceedings1992.pdf)
* [On-the-Fly Garbage Collection](http://pdf.aminer.org/000/017/456/on_the_fly_garbage_collection_an_exercise_in_cooperation.pdf)

Collectors
----------
* Generational + Mark-Sweep
* Generational + Incremental Mark-Sweep

Data Tags
----------
* flat
* pair
* closure
* vector
* structure type
* structure instance

Heap values
-----------
* number
* symbol
* char
* string
* bytes
* eof-object
* regexp
* boolean
* void
* empty
* closure-code