# Yet Another RStudio Application

It all starts with a story... Below is the live tale behind the code in this repository.

## Getting ready

First some enthusiasism in our first commit.

![](Images/getting_ready.png)

I have a few ideas, I hope you like them!

* I love the *bootstrap* technique;  I used to teach computational statistics at ITAM, and now I'm a PhD student at UT Austin with interest statistical methods for complex data. I want to do something simple but elegant and illustrative:
* Let's follow Davidson and Hinkley and design a function for (parametric) bootstrap robust simple linear regression (beginning of ch. 6).
* I will use `Rcpp` for speed. In fact, I'll use `RcppArmadillo` because it's very easy to implement.
* Let's make this a package! It should be easy with `devtools` and `roxygen2`. I discovered unit testing not long ago, but ever since my life is different; for this, I will use `testthat`.

More ideas:

* Everything should work smoothly within the `tidyverse` workflow.
* I will visualize the results with `ggplot2`! There is nothing as great as ggplot out there, every other library becomes tedious when it comes to plotting statistical data (especially the Python ones...).
* Creating git branches is kind of boring when you are the only user.. but let's do this anyway. I will add features progressively and keep track using continuous integration with `Travis CI` and `Codecov`.
* Oh, by the way, this README is an `rmarkdown`.

Let's get started!

## Continuous integration and unit testing

![](Images/creating_package.png)

If you want to install this package and test it in your computer just run

```r
devtools::install_github('mauriciogtec/YetAnotherRStudioApplication')
```

*Note*: All docuemntation will be created with roxygen directives and the function `devtools::document()`. 

## Configuration of Travis CI and Codecov with `covr` and testthat.

To make sure all releases work just fine (especially since Rcpp needs to be built). I will register the repository in Travis CI and Codecov. I am excited, `travis_integration` and `tests` will be the frist branches. And...

Great news! Our first merge! From now on I won't talk about git branches We'll just believe I'll continue working the right way. 

![](Images/continuous_integration.png)

Here are my precious banners:

---

[![Build Status](https://travis-ci.org/mauriciogtec/YetAnotherRStudioApplication.svg?branch=master)](https://travis-ci.org/mauriciogtec/YetAnotherRStudioApplication)
[![codecov](https://codecov.io/gh/mauriciogtec/YetAnotherRStudioApplication/branch/master/graph/badge.svg)](https://codecov.io/gh/mauriciogtec/YetAnotherRStudioApplication)

---

![](Images/banners.png)



  

