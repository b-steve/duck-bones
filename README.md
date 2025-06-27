# Data analysis for pull-out strength in duck bone

This repository contains data and code for the statistical analysis found in the following manuscript:

* Cohen, O., Baron, H. R., Stevenson, B. C., Wills, D. J., Wang, T., Walsh, W. R., Cowan, M. L., and Fearnside, S. M. (in submission) *Ex-vivo* pull-out strength of locking and cortex screws in the femur and tibiotarsus of the Pekin Duck (*Anas platyrhynchos domesticus*).

The analysis requires the free software environment [R](https://www.r-project.org/). The code to run the analysis can be found in the file `duck-analysis.R`. The R packages `car`, `lme4`, and `RLRsim` must be installed prior to running the code. The following line of code will do the trick:

```r
install.packages(c("car", "lme4", "RLRsim"))
```

