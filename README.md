This package contains a dependently typed implementation of the Simplex algorithm for solving linear programs. The algorithm can solve linear programs in the following form:
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
