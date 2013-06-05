To-Do
-----
#### benchmark

* Output metrics in a racket readable format.
* Measure allocated spaces for the whole heap, not only on the big generation.
* Benchmark with different ratio of small to big generation.
* Benchmark different caching models.
* More tests on `vector` and `structure`.

#### `plai/gc2`
* Make `plai/gc2/mutator` more extensible.
* Fix `mutator-cond` to support `(begin e …)` for each case.
