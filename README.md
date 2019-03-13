This package contains a dependently typed [idris](https://www.idris-lang.org/) implementation of Dantzig's [Simplex](https://en.wikipedia.org/wiki/Simplex_algorithm) algorithm for solving linear programs. This implementation expects that the programs are in the following form:
```
Maximize:
  c . x
Given:
  A . x <= b
  x >= 0
```

To install DependentLinearProgramming (this assumes you have Idris-dev/libs/contrib/contrib.ipkg installed already):
```
idris --install DependentLinearProgramming.ipkg
```

To run tests:
```
idris --testpkg DependentLinearProgramming.ipkg
```

To run the examples:
```
idris -p dependent_linear_programming -p contrib Examples/BasicExample.idr -o BasicExample; ./BasicExample
```
