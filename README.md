scalaopt
========

Scala Numerical Optimization library.

Currently this package is implementing classical methods used to solve unconstrained continuous 
optimization problems. All algorithms are implementing a `minimize` method that take as input an
objective function f that we want to minimize. f is a scalar function of x a vector of _continuous_
variables represented by any collection deriving from `Seq[Double]`.

The following lines show how to minimize a quadratic function with the BFGS method:

    scala> import com.github.bruneli.scalaopt.core._
    scala> import gradient.BFGS._
    scala> minimize((x: Variables) => x dot x, Vector(2.0, 4.0)) // Approximate derivatives
    scala> minimize(((x: Variables) => x dot x, (x: Variables) => x * 2.0), Vector(2.0, 4.0)) // Exact derivatives

`Variables` is an alias for `Seq[Double]`. The two classes `RichVector` and `RichMatrix` provide
some small DSL used internally to ease the understanding of vector and matrix algebra.

The `core` project has only one dependency on apache math common3 for the matrix algebra.

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
    
To get further details on the package you can look at the [Reference Manual](http://bruneli.github.io/scalaopt/#com.github.bruneli.scalaopt.core.package)

scalaopt Usage
--------------

### SBT

Add the following dependency to your `build.sbt`

    libraryDependencies += "com.github.bruneli.scalaopt" % "scalaopt-core_2.10" % "0.1"

### Maven

Add the following dependency to your `pom` file

    <dependency>
        <groupId>com.github.bruneli.scalaopt</groupId>
        <artifactId>scalaopt-core_2.10</artifactId>
        <version>0.1</version>
    </dependency>