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


##  A light bootstrap library

I don't pretend to do anything as powerful as `rsample`. In fact, I wanna show my elementary approach to bootstrapping with a very simple example. Thanks to the `tidyverse` machinery we can hove something up and running in no time.

I'll use the following libraries



```r
library(YetAnotherRStudioApplication) # my functions defined below that come with this package
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(knitr)
```


The light-weight example library will have three basic R functions
- *constructor*: creates a bootstrap resample by resampling indices with replacement
- *indexing*: defines a bracket notation to simplify indexing
- *mapping*: applies a function to each resample. It is exploiting the power of `purrr` to make it versatile. Here they are


```r
#' @title light bootstrap constructor
#' @description creates resampling indices but does not make entire copies of the dataset
#' @param d a data.frame, tibble or matrix 
#' @export
bootstrap <- function(data, times = 50L) {
  # Validate input
  !inherits(data, c("data.frame", "matrix")) && stop("d must be a data frame or numeric matrix")
  # Create samples for the most basic bootstrap scheme
  idx <- replicate(times, sample(nrow(data), replace = TRUE), simplify = FALSE)
  names(idx) <- paste0("sample", 1:times)
  # Output boot_light object
  # new("bootstrap_light", data = data, indices = idx, times = as.integer(times))
  x <- list(data = data, idx = idx, times = as.integer(times))
  class(x) <- c("bootstrap")
  x
}

#' @title boot light indexing
#' @description simplifies accessing a bootstrap resample
#' @details use `x[i]` to access the i-th resample
#' @rdname bootstrap
#' @export
`[.bootstrap` <- function(x, i) x$data[x$idx[[i]], ]


#' @title Apply a function to each entrie of bootstrap
#' @description creates resampling indices but does not make entire copies of the dataset
#' @param x a bootstrap_light object
#' @param .f function to apply to x, can be formula, see the map in the purrr package for details
#' @export
bootstrap_map <- function(x, .f, times = 50L) {
  # Validate input
  !inherits(x, c("bootstrap")) && stop("x must be of class bootstrap")
  !inherits(.f, c("function", "formula")) && stop(".f must be a function or formula")
  # Create samples for the most basic bootstrap scheme
  .f <- purrr:::as_mapper(.f)
  purrr::map(1:x$times, function(i) .f(x[i]))
}
```

Now let's take a look at the example data I will use. Our task is to predict the admittance of a student into a UCLA program, based on their application features. We will use a logistic regression to exemplify the bootstrap approach, which in this case will be naive and resample the cases.


```r
data <- read_csv("https://stats.idre.ucla.edu/stat/data/binary.csv") %>% 
  mutate(admit = factor(admit)) %>% 
  mutate(rank = factor(rank))
head(data)
```

```
## # A tibble: 6 x 4
##    admit   gre   gpa   rank
##   <fctr> <int> <dbl> <fctr>
## 1      0   380  3.61      3
## 2      1   660  3.67      3
## 3      1   800  4.00      1
## 4      1   640  3.19      4
## 5      0   520  2.93      4
## 6      1   760  3.00      2
```

The following scatter diagram shows that there seems to be an association between higher gre's and acceptance. The effect of the GPA is less clear to me (we notice how `ggplot` makes it easy to compare by fixing the plotting limits accross subplots: `ggplot` is great!).


```r
ggplot(data, aes(x = gre, y = gpa)) +
  geom_point() +
  facet_grid(admit ~ rank, labeller = labeller(rank = function(i) paste0("rank", i)))
```

![](README_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Now what I'm gonna do is ***cheat and create a bad mess*** by introducing an outlier. We want to see how the bootstrap will be stand superior to the simple model.

```r
data$gpa[1] <- 40.0
```


Of course we want to use bootstrap, but let us quickly first show the results of a logistic regression (measured in terms of prediction).


```r
mod <- glm(admit ~ ., data = data, family = "binomial")
original_coeffs <- coefficients(mod)
original_summary <- summary(mod)
original_coeffs
```

```
##  (Intercept)          gre          gpa        rank2        rank3 
## -1.848905784  0.003222303  0.013771710 -0.720677098 -1.294947112 
##        rank4 
## -1.600576455
```

Prediction accuracy

```r
pred <- round(fitted.values(mod, data))
sprintf("Original model accuracy is %0.2f%%", 100*sum(pred == data$admit) / nrow(data))
```

```
## [1] "Original model accuracy is 69.50%"
```

We'll now *create our bootstrap samples*!!


```r
set.seed(999)
times <- 1000L
system.time({
  b_resamples <- bootstrap(data, times)
})
```

```
##    user  system elapsed 
##       0       0       0
```

Evidently, the new object doesn't grow in size proportionally to the number of resamples, since only a indices are being created. For 1000 resamples, the growth factor in this dataset is


```r
as.numeric(object.size(b_resamples) / object.size(data))
```

```
## [1] 169.5811
```

Let's now run a regression model for each resample:


```r
system.time({
  b_models <- bootstrap_map(b_resamples, 
    ~glm(admit ~ ., data = .x, family = "binomial")
  )
})
```

```
##    user  system elapsed 
##    4.31    0.05    4.36
```
 
Let's start by comparing the coefficients of the model. We can use `purrr::map` and `purrr::reduce` to collect the bootstraps.


```r
b_coefs <- b_models %>% 
  map(~coefficients(.x)) %>% 
  reduce(rbind) 
```


```r
b_coefs %>% 
  colMeans()
```

```
##  (Intercept)          gre          gpa        rank2        rank3 
## -2.707795980  0.002936329  0.312005616 -0.703583790 -1.322967905 
##        rank4 
## -1.616685610
```

The following plot shows the value of the coefficients for each bootstrap resample; the red vertical lines are the original estimates.


```r
plots <- b_coefs %>% 
  as.data.frame() %>% 
  gather("coef", "value") %>% 
  split(.$coef) %>% 
  map(~ ggplot(., aes(x = value)) +
    geom_histogram(bins = 30) +
    geom_vline(xintercept = original_coeffs[.$coef[1]], colour = "red") +
    ggtitle(.$coef[1])) 
marrangeGrob(plots, 3, 2, top = NULL)
```

![](README_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

We can obtained improved model estimates and standard errors, comparing the original estimates (note: the original estimates given by the summary are in fact an approximation based on the model assumptions--the bootstrap version is not)


```r
new_coef <- b_coefs %>% 
  colMeans()
new_sd <- b_coefs %>% 
  apply(2, sd)
```

Compare result

```r
data_frame(
  `Original coefs` = original_coeffs,
  `bootstrap coefs` = new_coef,
  `Original sd` = original_summary$coefficients[ ,2],
  `Bootstrap sd` = new_sd
) %>% 
  kable()
```



 Original coefs   bootstrap coefs   Original sd   Bootstrap sd
---------------  ----------------  ------------  -------------
     -1.8489058        -2.7077960     0.7119777      1.3630155
      0.0032223         0.0029363     0.0010188      0.0012052
      0.0137717         0.3120056     0.0666459      0.4553481
     -0.7206771        -0.7035838     0.3131267      0.3272693
     -1.2949471        -1.3229679     0.3414103      0.3522296
     -1.6005765        -1.6166856     0.4150435      0.4444470

If we compare with the original estimates for the mean and standard deviation of the coefficient, they are completely different, especially in `gpa`, where we had our sneaky outlier. This is the main contribution of bootstrap! We have an entire distribution of the estimators, and much better sense of their uncertainty!

Let's now compare predictions by selecting the prediction for each individual in each bootstrapped model. The final final decision is taken by averaging.


```r
b_pred <- b_models %>% 
  map(~ predict(., data, type = "response")) %>% 
  reduce(rbind) %>% 
  colMeans() %>% 
  map_dbl(round)
b_acc <- sum(b_pred == data$admit) / nrow(data)
sprintf("The new prediction is %0.2f%%", 100*b_acc)
```

```
## [1] "The new prediction is 70.50%"
```

It's a very modest yet positive improvement over the original model.

## Bootstrapping residuals for robust regression with Rcpp

Just to finish this presentation. I will show an approach for bootstrapping regression by resampling from the errors. This time I will create a single `rcpp` function to do the entire job. There's of course many rooms of improvement, including the resampling schme and need to integrate it the previous framework. But it is a nice ilustration of the power of Rcpp and the bootstrapping method.


```cpp
#define ARMA_64BIT_WORD 1
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;
using namespace arma;
using namespace std;

//' @title parametric bootstrap regression
//' @export
// [[Rcpp::export]]
Rcpp::List bootstrap_rlm(
      arma::mat &X,
      arma::vec &y,
      int times
    ){
  uword n = X.n_rows;
  uword p = X.n_cols;
  uword B = times;
  
  // Step 1: Obtain the projection matrix and leverages
  mat pseudo_inv = pinv(X);
  mat proj_mat = X * pseudo_inv;
  vec leverage = proj_mat.diag();
  
  // Step 2: Find coefficients and robust residuals
  vec coeffs = pseudo_inv * y;
  vec fitted = X * coeffs;
  vec err = y - fitted;
  
  // Step 3: Modified residuals
  vec rres = err / sqrt(1 - leverage);
  vec centered_rres = rres - mean(rres);
  
  // Step 4: Resample obtained bootstrapped coefficients
  mat b_coefs(B, p);
  vec b_se(B);
  for (uword i = 0; i < B; i++) {
    // Sample indices
    uvec idx(n); 
    for (uword i = 0; i < n; i++) {
      idx(i) = rand() % n;  // main area of opportunity for improvement
    }
    vec y_i = fitted + centered_rres.elem(idx);
    // Fit regression
    vec coefs_i =  pseudo_inv * y_i;
    b_coefs.row(i) =  coefs_i.t();
    vec err_i = y_i - X * coefs_i;
    b_se(i) = sqrt(sum(square(err_i)) / (n - p)) ;
  }

  return Rcpp::List::create(Named("coefficients") = wrap(b_coefs),
                            Named("se") = wrap(b_se),
                            Named("residuals") = wrap(err),
                            Named("leverages") = wrap(leverage),
                            Named("modified residuals") = wrap(rres));
}
```

Let's see an example with a commin R dataset. We will compare with `rlm` from the `MASS` package.


```r
library(MASS)
library(tidyverse)
```
We want to predict the varible `stackloss` in the following data set

```r
data("stackloss")
X <- stackloss %>% 
  dplyr::select(-stack.loss) %>% 
  cbind(1, .) %>% 
  data.matrix()
y <- stackloss %>% 
  pull(stack.loss)
head(stackloss)
```

```
##   Air.Flow Water.Temp Acid.Conc. stack.loss
## 1       80         27         89         42
## 2       80         27         88         37
## 3       75         25         90         37
## 4       62         24         87         28
## 5       62         22         87         18
## 6       62         23         87         18
```


```r
mod <- rlm(stack.loss ~ ., data = stackloss)
summary(mod)
```

```
## 
## Call: rlm(formula = stack.loss ~ ., data = stackloss)
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -8.91753 -1.73127  0.06187  1.54306  6.50163 
## 
## Coefficients:
##             Value    Std. Error t value 
## (Intercept) -41.0265   9.8073    -4.1832
## Air.Flow      0.8294   0.1112     7.4597
## Water.Temp    0.9261   0.3034     3.0524
## Acid.Conc.   -0.1278   0.1289    -0.9922
## 
## Residual standard error: 2.441 on 17 degrees of freedom
```

Now with our bootstrap fast function


```r
n_bootstraps <- 500
system.time({
  bmod <- bootstrap_rlm(X, y, n_bootstraps)
})
```

```
##    user  system elapsed 
##       0       0       0
```


```r
b_coefs <- bmod$coefficient %>% 
  as.data.frame()
names(b_coefs) <- names(mod$coefficients)
head(b_coefs)
```

```
##   (Intercept)  Air.Flow Water.Temp  Acid.Conc.
## 1   -37.50070 0.5968135  1.2624080 -0.07971442
## 2   -31.09271 0.6124507  1.5157649 -0.23778420
## 3   -49.57264 0.8586416  0.9760800 -0.05443508
## 4   -37.83825 0.7074117  1.0662287 -0.12108659
## 5   -40.04078 0.7637483  0.8980218 -0.07024551
## 6   -14.18202 0.7968815  1.6244003 -0.58311938
```

We can now compare the distribution we get from our model with the `rlm` function in `MASS`, which is shown in red.


```r
plots <- b_coefs %>% 
  as.data.frame() %>% 
  gather("coef", "value") %>% 
  split(.$coef) %>% 
  map(~ ggplot(., aes(x = value)) +
    geom_histogram(bins = 30) +
    geom_vline(xintercept = mod$coefficients[.$coef[1]], colour = "red") +
    ggtitle(.$coef[1])) 
marrangeGrob(plots, 2, 2, top = NULL)
```

![](README_files/figure-html/unnamed-chunk-23-1.png)<!-- -->

Let's compare mean and errors of the models


```r
b_estimates <- b_coefs %>%
  colMeans()
b_sd <- b_coefs %>% 
  apply(2, sd)
data_frame(
  `MASS rlm coefs` = mod$coefficients,
  `bootstrap coefs` = b_estimates,
  `MASS rlm sd.` = summary(mod)$coefficients[ ,2],
  `bootstrap sd` = b_sd
) %>% 
  kable()
```



 MASS rlm coefs   bootstrap coefs   MASS rlm sd.   bootstrap sd
---------------  ----------------  -------------  -------------
    -41.0265311       -40.8145182      9.8073472     12.2931804
      0.8293739         0.7032308      0.1111803      0.1399549
      0.9261082         1.3288288      0.3034081      0.3691765
     -0.1278492        -0.1414518      0.1288526      0.1615458

