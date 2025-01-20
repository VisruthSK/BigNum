# BigNum: A showcase and reference for S7

Written for `S7 0.2.0.`

```r         
pak::pkg_install("VisruthSK/BigNum")
```

## Overview

***This project is a work in progress, and needs reviews and revisions to check for consistency and such. Some best practices for S7 development (e.g. [documentation](https://github.com/RConsortium/S7/issues/315)) haven't been established yet though.***

What is S7? What is BigNum? Why can't I subtract two `big_num`s?

Let's start with the most interesting part: S7.

### S7

["The S7 package is a new OOP system designed to be a successor to S3 and S4." - R Consortium Object-Oriented Programming Working Group.](https://rconsortium.github.io/S7/)

S7 is a new way to do OOP in R, and I think it is fantastic. The quote above links to the official S7 website, which contains a great deal of information regarding S7--its origins, features, compatibility, specifications, etc. I think any developer who wishes to use S7 should certainly study that website. It is quite cogent and detailed, and I use it as a reference all the time–its practically an extra chapter of [Advanced R](https://adv-r.had.co.nz/). This package is meant to be used in conjunction with the S7 website, hopefully serving as a (slightly non-contrived) implementation which can be used to understand how S7 can work in practice.

Some goals of the project, with regards to S7:

Since S7 is so new, and is still technically experimental, major changes to the API [aren't off the table](https://rconsortium.github.io/S7/index.html?q=experimental#s7). As such, for this project to be effective, it will be updated with S7 as the OOP system matures. The package's major version will follow S7, which is why this (will) release into 2.0.0. Best practices, as and when they are developed, will be implemented to serve as an effective, worked out example for S7 package development.

### Why S7?

This package was created for a number of reasons. Chief among those is to provide a worked example of S7 in a package context, but it was also written to purvey the benefits of S7, and posit it as the next R OOP paradigm you should reach for. I highly recommend listening to [Hadley's talk on S7 (then called R7) at posit::conf(2022)](https://www.youtube.com/watch?v=P3FxCvSueag) (thank you [Dr Bodwin](https://www.kelly-bodwin.com/) for showing this to me!), and reading the S7 webpage to get arguments for S7 from very bright people. This section can be skipped--or at least skimmed--if you (like me) are convinced that S7 is the way to go.

Here's an unordered list of some nice things about S7. I won't directly address these ideas, but you can observe them throughout the code.

1.  Just the right amount of formality
2.  Just the right amount of flexibility
3.  Cogent API
4.  Easy to extend
5.  Classes, properties, validators, etc. give nice guarantees
6.  Simple to swap to from S3

S7 marries (most of) the flexibility of S3 with (most of) the formality of S4. I find it strikes a perfect middle ground which makes it easier to dictate and understand what objects of type `<T>` can and can't do. S7 is elegantly formal. I first encountered S3 when speaking to Dr Bodwin; the topic came up somehow and she showed me how simple and flexible S3 is with an example, something like:

```r
x <- 1:10
class(x) <- "test"
mean.test <- function(x){
    "HELLO WORLD"
}

mean(x)
#> [1] "HELLO WORLD"
```

I was shocked at how easy it was to defined a class and write its method for a generic, especially since my knowledge of OOP was mostly in Java (which is very verbose). The flexibility offers a lot of obvious benefits, but it does come with some drawbacks. Firstly, I think it can be a bit hard to understand; I still find S3 object construction a bit strange. For example, if I wanted to skip `x`'s declaration, I could've written instead:

```r
mean.test <- function(x){
    "HELLO WORLD"
}

mean(structure(1:10, class = "test"))
```

but I think this is harder to reason about. The idea of just giving a garden variety vector `1:10` a whole class without really defining what that class is, is strange to me. I find it hard sometimes to understand what S3 objects are and how they interact with methods. S7 makes it very clear what things are; I find this easier to reason about. This is something I noted firsthand when swapping to S7 in some [tidyverse](https://www.tidyverse.org/) packages. Even with well documented code, it can be hard to understand how things work.

S7 may be formal, but it is also elegant–there are nice design patterns that can be implemented effortlessly. Take, for example, this implementation of some `Shape`s.

```r
library(S7)

Shape <- new_class("Shape", abstract = TRUE)
Circle <- new_class("Circle",
  Shape,
  properties = list(
    radius = class_numeric
  )
)
Rect <- new_class("Rect",
  Shape,
  properties = list(
    width = class_numeric,
    height = class_numeric
  )
)

Area <- new_generic("Area", "x")
method(Area, Circle) <- \(x) pi * x@radius^2
method(Area, Rect) <- \(x) x@width * x@height

circ <- Circle(4 / (sqrt(pi)))
rect <- Rect(10, 4)

Area(circ)
Area(rect)
```

This was a quick and dirty implementation I wrote in a few minutes.

It can easily be extended to admit a `Square` class:

```r
Square <- new_class("Square",
  Rect,
  constructor = function(side) {
    new_object(S7_object(), width = side, height = side)
  }
)
square <- Square(5)

Area(square)
```

And, more usefully, our classes can be (minimally) changed to guarantee that the provided values are positive–being numeric is already ensured by using `class_numeric`, even in the original definitions.

```r
positive_numeric <- new_property(class_numeric,
  validator = function(value) {
    if (value <= 0) "must be greater than 0"
  }
)

Circle <- new_class("Circle",
  Shape,
  properties = list(
    radius = positive_numeric
  )
)
Rect <- new_class("Rect",
  Shape,
  properties = list
  (
    width = positive_numeric,
    height = positive_numeric
  )
)

Rect(0, 4)
#> ! <Rect> object properties are invalid:
#> - @width must be greater than 0

Circle(-10)
#> ! <Circle> object properties are invalid:
#> - @radius must be greater than 0

# and of course this still holds for Squares
Square(-1)
#> ! <Square> object properties are invalid:
#> - @width must be greater than 0
#> - @height must be greater than 0
```

We could also compute area in a property instead of a method:

```r
positive_numeric <- new_property(class_numeric,
  validator = function(value) {
    if (value <= 0) "must be greater than 0"
  }
)

Shape <- new_class("Shape", abstract = TRUE)
Circle <- new_class(
  "Circle",
  Shape,
  properties = list(
    radius = positive_numeric,
    area = new_property(class_numeric, getter = function(self) pi * self@radius^2, setter = NULL)
  ),
  constructor = function(radius) {
    new_object(S7_object(), radius = radius)
  }
)
Rect <- new_class(
  "Rect",
  Shape,
  properties = list(
    width = positive_numeric,
    height = positive_numeric,
    area = new_property(class_numeric, getter = function(self) self@width * self@height, setter = NULL)
  ),
  constructor = function(width, height) {
    new_object(S7_object(), width = width, height = height)
  }
)
Square <- new_class("Square",
  Rect,
  constructor = function(side) {
    new_object(S7_object(), width = side, height = side)
  }
)
circ <- Circle(10)
circ
#> <Circle>
#>  @ radius: num 10
#>  @ area  : num 314    
                    
circ@area <- 10
#> ! Can't set read-only property <Circle>@area
                    
Rect(10, 3)
#> <Rect>
#>  @ width : num 10
#>  @ height: num 3
#>  @ area  : num 30
                    
Square(5)
#> <Square>
#>  @ width : num 5
#>  @ height: num 5
#>  @ area  : num 25
```

`Shapes` are hardly the most interesting example (hence the package!), and these aren't the most groundbreaking principles, but having these OOP ideas in R is very nice. They're presented in a cogent API and allow you to spell out what you need to do: `new_class()`, `new_generic()`, `new_external_generic()`, `method()`, etc. This allows formality without being overly stuffy or verbose. The structure that S7 provides manifests as interpretable, clean code. Most importantly, it is very readable code.

### Some Random Thoughts/Interesting Points

##### Read Only Properties

You can set read only properties with S7, which you can observe in the package (properties in ALLCAPS are constants or "final".) That is a very nice way to expose values which you wish users to be aware of but don't want to mess with, like the value of a linked list node. 

```r
# https://rconsortium.github.io/S7/articles/classes-objects.html#frozen-properties
eg <- new_class("eg",
  properties = list(
    VALUE = new_property(
      class_any,
      setter = function(self, value) {
        if (!is.null(self@VALUE)) stop("@VALUE is read only.")
        self@VALUE <- value
        self
      }
    )
  )
)

tmp <- eg(10)
tmp
#> <eg>
#> @ VALUE: num 10

# tmp@VALUE  <- 1
#> ! @VALUE is read only.
```

You can, of course, bypass this:

```r
attr(tmp, "VALUE") <- 1
tmp
#> <eg>
#> @ VALUE: num 1
```

but you clearly have to go out of your way to. Normal usage of `eg` objects, with the exposed API, would ensure that you don't accidentally change constants.

You can also do cool stuff with [computed/dynamic properties](https://rconsortium.github.io/S7/articles/classes-objects.html#properties). The last implementation of `Shape`s above uses this to compute the area dynamically–thus allowing the area to change as you adjust the measurements of a shape.

##### Var Args

I don't think you can't easily do anything like this in S7 ([yet](https://github.com/RConsortium/S7/issues/515)):

```r
function(...){
    structure(..., class = "test")
}
```

##### State

You have to use environments to get state in S7 (again, see package)--but I have it on strong authority that a stateful S7 is being thought of. Don't quote me on that.

### BigNum

So finally what is BigNum? And why this project?

Firstly, it is important to note that this is wholly a *toy* package. Whilst the package will build, and expose an API one can use to create `big_num` objects, I do not think one *should* use these as arbitrary precision numbers (look towards packages like [Rmpfr](https://cran.r-project.org/web/packages/Rmpfr/index.html).) This package's implementation is very rough and many properties one would expect from "numbers" are missing–negation, subtraction, division, modulus, etc. are all not implemented. The point of this package is not to veritably implement infinite precision numbers, but rather to show how S7 can be used in a package to make some basic data structures and work with some generics/functions. The package itself is very simple and has a relatively concise implementation. Since this package's purpose is pedagogical, there isn't much point in implementing the aforementioned features.

I wrote the S7 implementation first, and have since expanded to a S3 and S4 implementation (and might add a rough R6 one too?). These three versions (S3, S4, S7) have rough feature parity, but part of this project is to highlight the benefits of S7 so there are things you can't do (or at least, can't do as easily) in the other OOP structures.

So why BigNum specifically? Well, mostly because it's easy to implement. The BigNum project is taken from my CSC 203 class, a OOP course at Cal Poly. I already had all the methods implemented (in Java) and a clear idea of what I needed to do and how, with the main work being in porting design to R as opposed to novel thought. This greatly simplified dev time since I had a reference implementation to use. **Importantly,** **this (along with my lack of experience) could lead to unidiomatic R/S7 code and design patterns.** **If you notice anything strange, please open an issue/PR!**

I haven't developed an R package before, and so that provided additional motivation for me to create this project. That also means that this package is certainly written sub-optimally. Additionally, my experience with S7 is extremely limited–I would be extremely grateful for any and all R sourcerers who can rain issues and pull requests down from the heavens fixing all my mistakes :)

## Acknowledgements

Thank you Dr T and Dr Bodwin for helping with this project!

## References

Vaughan D, Hester J, Kalinowski T, Landau W, Lawrence M, Maechler M, Tierney L, Wickham H (2024). *S7: An Object Oriented System Meant to Become a Successor to S3 and S4*. R package version 0.2.0.9000, <https://github.com/RConsortium/S7>, <https://rconsortium.github.io/S7/>.

Henry L, Wickham H (2024). *rlang: Functions for Base Types and Core R and 'Tidyverse' Features*. R package version 1.1.4, <https://CRAN.R-project.org/package=rlang>.

Maechler M (2024). *Rmpfr: Interface R to MPFR - Multiple Precision Floating-Point Reliable*. R package version 1.0-0, <https://CRAN.R-project.org/package=Rmpfr>.

Wickham H (2019). *Advanced R* (2nd ed.). CRC Press.

Wickham H (2022). *An Introduction to R7*. <https://www.youtube.com/watch?v=P3FxCvSueag>

Wickham H, Bryan J (2023). *R Packages* (2nd ed.). O'Reilly Media.

## TODOs:

-   Write documentation

    -   Comment code

-   Write tests

-   Write a vignette?
