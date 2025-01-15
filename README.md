# BigNum: A showcase and reference for S7

Written for `S7 0.2.0.`

```         
pak::pkg_install("VisruthSK/BigNum")
```

## Why S7?

This package was created for a number of reasons. Chief among those is to provide a worked example of S7 in a package context, but it was also written to purvey the benefits of S7. So here I wish to address some reasons why S7 is best, and why you should seriously consider using S7 the next time you need a class for something. This is almost entirely subjective, and as an op-ed, the points I make are personal. There are certainly many contexts where S7 is not appropriate (one could argue that this package is best suited to R6), but there are also many places where you *could* use S7; I'm more focused on the latter.

S7 is formal. I first encountered S3 when speaking to Dr Bodwin; the topic came up somehow and she showed me how flexible S3 is with an example, something like:

```{r}
x <- 1:10
class(x) <- "test"
mean.test <- function(x){
    "HELLO WORLD"
}

mean(x)
#> [1] "HELLO WORLD"
```

I was shocked at how easy it was to defined a class and write its method for a generic, especially since my knowledge of OOP was mostly in Java (which is very verbose.) The flexibility offers a lot of obvious benefits, but it does come with some drawbacks. Firstly, I think it can be a bit hard to understand; I still find S3 object construction a bit strange. For example, if I wanted to skip `x`'s declaration, I could've written instead:

```{r}
mean.test <- function(x){
    "HELLO WORLD"
}

mean(structure(1:10, class = "test"))
```

but I think this is harder to reason about. The idea of just giving `1:10` a class without really defining what that class is, is strange to me. S7 makes it very clear what things are; I find this easier to reason about.

## Overview

***This project is a work in progress, needs reviews and revisions to check for consistency and such. Some best practices for S7 development (e.g. [documentation](https://github.com/RConsortium/S7/issues/315)) haven't been established yet though.***

What is S7? What is BigNum? Why can't I subtract two `big_num`s?

Let's start with the most interesting part: S7.

### S7

["The S7 package is a new OOP system designed to be a successor to S3 and S4." - R Consortium Object-Oriented Programming Working Group.](https://rconsortium.github.io/S7/)

S7 is a new way to do OOP in R, and I think it is fantastic. The quote above links to the official S7 website, which contains a great deal of information regarding S7--its origins, features, compatibility, specifications, etc. I think any developer who wishes to use S7 should certainly study that website. It is quite cogent and detailed, and I use it as a reference all the time–its practically an extra chapter of Advanced R. This package is meant to be used in conjunction with the S7 website, hopefully serving as a (slightly non-contrived) implementation which can be used to understand how S7 can work in practice.

Some goals of the project, viz. with regards to S7:

Since S7 is so new, and is still technically experimental, major changes to the API [aren't off the table](https://rconsortium.github.io/S7/index.html?q=experimental#s7). As such, for this project to be effective, it will be updated with S7 as the OOP system matures. The package's major version will follow S7, which is why this (will) release into 2.0.0. Best practices, as and when they are developed, will be implemented to serve as an effective, worked out example for S7 package development.

### BigNum

So what is BigNum? And why this project?

Firstly, it is important to note that this is wholly a *toy* package. Whilst the package will build, and expose an API one can use to create `big_num` objects, I do not think one *should* use these as arbitrary precision numbers (look towards packages like [Rmpfr](https://cran.r-project.org/web/packages/Rmpfr/index.html).) This package's implementation is very rough and many properties one would expect from "numbers" are missing–negation, subtraction, division, modulus, etc. are all not implemented. The point of this package is not to veritably implement infinite precision numbers, but rather to show how S7 can be used in a package to make some basic data structures and work with some generics/functions. The package itself is very simple and has a relatively concise implementation. Since this package's purpose is pedagogical, there isn't much point in implementing the aforementioned features.

I wrote the S7 implementation first, and have since expanded to a S3 and S4 implementation (and might add a rough R6 one too?). These three versions (S3, S4, S7) have rough feature parity, but part of this project is to highlight the benefits of S7 so there are things you can't do (or at least, can't do as easily) in the other OOP structures.

So why BigNum specifically? Well, mostly because it's easy to implement. The BigNum project is taken from my CSC 203 class, a OOP course at Cal Poly. I already had all the methods implemented (in Java) and a clear idea of what I needed to do and how, with the main work being in porting design to R as opposed to novel thought. This greatly simplified dev time since I had a reference implementation to use. **Importantly,** **this (along with my lack of experience) could lead to unidiomatic R/S7 code and design patterns.** **If you notice anything strange, please open an issue/PR!**

I haven't developed an R package before, and so that provided additional motivation for me to create this project. That also means that this package is certainly written sub-optimally. Additionally, my experience with S7 is extremely limited–I would be extremely grateful for any and all R sourcerers who can rain issues and pull requests down from the heavens fixing all my mistakes :)

## References

Vaughan D, Hester J, Kalinowski T, Landau W, Lawrence M, Maechler M, Tierney L, Wickham H (2024). *S7: An Object Oriented System Meant to Become a Successor to S3 and S4*. R package version 0.2.0.9000, <https://github.com/RConsortium/S7>, <https://rconsortium.github.io/S7/>.

Henry L, Wickham H (2024). *rlang: Functions for Base Types and Core R and 'Tidyverse' Features*. R package version 1.1.4, <https://CRAN.R-project.org/package=rlang>.

Maechler M (2024). *Rmpfr: Interface R to MPFR - Multiple Precision Floating-Point Reliable*. R package version 1.0-0, <https://CRAN.R-project.org/package=Rmpfr>.

Wickham H (2019). *Advanced R* (2nd ed.). CRC Press.

Wickham H, Bryan J (2023). *R Packages* (2nd ed.). O'Reilly Media.

## TODOs:

-   Write documentation

    -   Comment code

-   Write tests

-   Write a vignette?