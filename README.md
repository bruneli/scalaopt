scalaopt
========

Scala Numerical Optimization library.

Currently this package is implementing classical methods used to solve unconstrained continuous 
optimization problems. All algorithms are implementing a 'minimize' method that take as input an
objective function f that we want to minimize. f is a scalar function of x a vector of _continuous_
variables represented by any collection deriving from 'Seq\[Double\]'.

The following lines show how to minimize a quadratic function with the BFGS method:

    scala> import org.scalaopt.algos._
    scala> import org.scalaopt.algos.gradient.BFGS._
    scala> minimize((x: Variables) => x dot x, Vector(2.0, 4.0)) // Approximate derivatives
    scala> minimize(((x: Variables) => x dot x, (x: Variables) => x * 2.0), Vector(2.0, 4.0)) // Exact derivatives

'Variables' is an alias for 'Seq\[Double\]'. The two classes 'RichVector' and 'RichMatrix' provide
some small DSL used internally to easy the understanding of vector and matrix algebra.

The 'algos' project has only one dependency on apache math common3 for the matrix algebra.

Below is a list of algorithms implemented:

* Quasi-Newton methods: 
    * BFGS
* Conjugate gradient methods: 
    * classical Conjugate Gradient
    * Newton Conjugate Gradient (line search method for large scale optimization)
    * Steihaug Conjugate Gradient (trust region method for large scale optimization)
* Least-squares problems:
    * Levenberg-Marquardt
* Derivative free methods:
    * Nelder-Mead
    * Powell