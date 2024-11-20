# BigNum: A showcase and reference for S7

Written for `S7 0.2.0.`

## Overview

***This project is a work in progress, needs reviews and revisions to check for consistency and such. Some best practices for S7 development (e.g. [documentation](https://github.com/RConsortium/S7/issues/315)) haven't been established yet though.***

What is S7? What is BigNum? Why can't I subtract two `big_num`s?

Let's start with the most interesting part: S7.

### S7

["The S7 package is a new OOP system designed to be a successor to S3 and S4." - R Consortium Object-Oriented Programming Working Group.](https://rconsortium.github.io/S7/)

S7 is a new way to do OOP in R, and it is fantastic. That quote links to the official S7 website, which contains a great deal of information regarding S7--its origins, features, compatibility, specifications, etc. Any developer who wishes to use S7 should certainly study that website. It is quite cogent and detailed, and I use it as a reference all the time–its practically an extra chapter of Advanced R. This package is meant to be used in conjunction with the S7 website, hopefully serving as a (slightly non-contrived) total which can be used to understand how S7 can work in practice. Since S7 is so new, and is still technically experimental, major changes to the API aren't unusual. As such, for this project to be effective, it must be kept in time with S7 as the OOP system matures. Best practice, as and when they are developed, should be implemented to serve as an effective, worked out example for S7 package development.

S7 is in use by a number of people already–some are writing about their experiences as well. One I happened upon is this [blog post](https://blog.djnavarro.net/posts/2024-02-25_s7/) by Dr. Navarro detailing how they used S7 to make some of her latest artwork. Like all of Dr Navarro's posts, it is well worth a read.

### BigNum

So what is BigNum? And why this project?

Firstly, it is important to note that this is a *toy* package. Whilst the package will build, and expose an API one can use to create `big_num` objects, I do not think one *should* use these as infinite precision numbers (for that purpose, look towards packages like [Rmpfr](https://cran.r-project.org/web/packages/Rmpfr/index.html).) This package's implementation is very rough and many properties one would expect from "numbers" are missing–negative numbers, subtraction, division, modulus, etc. are all not implemented. The point of this package is not to veritably implement infinite precision numbers, but rather to show how S7 can be used in a package to make some basic data structures and work with some generics/functions. The package itself is very simple and has a relatively concise and simple implementation. Since this package's purpose is pedagogical, there isn't much point in implementing all the aforementioned features.

So why BigNum specifically? Well, mostly because it's easy to implement. The BigNum project is taken from my CSC 203 class, a course in OOP at Cal Poly. As such, I already had all the methods implemented and a clear idea of what I needed to do and how, with the main work being in porting logic to R as opposed to devising the methods and classes needed. This greatly simplified dev time since I had a reference implementation to use. **Importantly,** **this (along with my lack of experience) could lead to unidiomatic R/S7 code and design patterns.** **If you notice anything strange, please open an issue/PR!**

I haven't developed an R package before, and so that provided additional motivation for me to create this project. That also means that this package is certainly written sub-optimally. Additionally, my experience with S7 is extremely limited–I would be extremely grateful for any and all R sourcerers who can rain issues and pull requests down from the heavens, fixing all my mistakes :)

## References

Vaughan D, Hester J, Kalinowski T, Landau W, Lawrence M, Maechler M, Tierney L, Wickham H (2024). *S7: An Object Oriented System Meant to Become a Successor to S3 and S4*. R package version 0.2.0.9000, <https://github.com/RConsortium/S7>, <https://rconsortium.github.io/S7/>.

Navarro, Danielle. 2024. “Creating New Generative Art Tools in R with Grid, Ambient, and S7.” February 25, 2024. <https://blog.djnavarro.net/posts/2024-02-25_s7/>.

Henry L, Wickham H (2024). *rlang: Functions for Base Types and Core R and 'Tidyverse' Features*. R package version 1.1.4, <https://CRAN.R-project.org/package=rlang>.

Maechler M (2024). *Rmpfr: Interface R to MPFR - Multiple Precision Floating-Point Reliable*. R package version 1.0-0, <https://CRAN.R-project.org/package=Rmpfr>.

Wickham H (2019). *Advanced R* (2nd ed.). CRC Press.

Wickham H, Bryan J (2023). *R Packages* (2nd ed.). O'Reilly Media.

## TODOs:

-   Finish readme

    -   add more citations

-   Write documentation

    -   Comment code

    -   Some basic comments for actual use of package

-   Write tests

-   Write a vignette?

-   Website?