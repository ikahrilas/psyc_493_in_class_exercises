In-Class Exercise \#1
================
Ian Kahrilas
2/4/2021

``` r
dat <- read_sav("LOT12.SAV")
glimpse(dat)
```

    ## Rows: 549
    ## Columns: 12
    ## $ XpctBest <dbl> 3, 4, 1, 2, 3, 3, 3, 1, 3, 2, 1, 1, 3, 3, 1, 2, 3, 4, 0, 2, …
    ## $ EzRelax  <dbl> 2, 4, 3, 3, 2, 1, 3, 1, 2, 2, 2, 3, 3, 2, 2, 3, 3, 1, 2, 3, …
    ## $ GoWrong  <dbl> 3, 1, 3, 2, 1, 3, 1, 2, 3, 2, 2, 4, 1, 2, 1, 3, 2, 2, 2, 1, …
    ## $ BritSide <dbl> 3, 4, 1, 3, 2, 4, 3, 1, 3, 1, 2, 1, 2, 2, 1, 3, 3, 3, 3, 3, …
    ## $ OptFutur <dbl> 2, 4, 3, 3, 4, 4, 2, 2, 3, 2, 2, 2, 2, 3, 2, 3, 3, 2, 4, 3, …
    ## $ EnjoyFrn <dbl> 3, 3, 3, 3, 4, 4, 4, 3, 3, 3, 3, 3, 4, 3, 3, 4, 4, 4, 3, 4, …
    ## $ KeepBusy <dbl> 3, 4, 3, 2, 4, 3, 1, 4, 3, 2, 3, 2, 2, 3, 1, 1, 4, 4, 2, 3, …
    ## $ NevrXpct <dbl> 2, 1, 2, 2, 1, 1, 0, 2, 2, 3, 2, 4, 3, 2, 1, 2, 1, 1, 1, 1, …
    ## $ NevrWork <dbl> 2, 2, 1, 1, 2, 1, 0, 2, 2, 3, 2, 4, 1, 2, 3, 1, 1, 0, 4, 0, …
    ## $ NoUpset  <dbl> 1, 3, 3, 4, 2, 3, 3, 1, 2, 2, 2, 2, 3, 1, 1, 1, 2, 1, 0, 3, …
    ## $ SilvrLin <dbl> 2, 4, 3, 3, 2, 2, 4, 0, 3, 1, 2, 2, 4, 2, 3, 3, 2, 2, 0, 3, …
    ## $ NoCount  <dbl> 3, 4, 1, 1, 1, 2, 0, 1, 2, 3, 2, 3, 1, 2, 1, 0, 2, 1, 4, 0, …

``` r
summary(dat)
```

    ##     XpctBest       EzRelax         GoWrong         BritSide        OptFutur    
    ##  Min.   :0.00   Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :0.000  
    ##  1st Qu.:1.00   1st Qu.:1.000   1st Qu.:1.000   1st Qu.:2.000   1st Qu.:2.000  
    ##  Median :2.00   Median :2.000   Median :2.000   Median :2.000   Median :3.000  
    ##  Mean   :2.25   Mean   :2.166   Mean   :1.891   Mean   :2.443   Mean   :2.738  
    ##  3rd Qu.:3.00   3rd Qu.:3.000   3rd Qu.:3.000   3rd Qu.:3.000   3rd Qu.:3.000  
    ##  Max.   :4.00   Max.   :4.000   Max.   :4.000   Max.   :4.000   Max.   :4.000  
    ##     EnjoyFrn       KeepBusy        NevrXpct       NevrWork        NoUpset     
    ##  Min.   :0.00   Min.   :0.000   Min.   :0.00   Min.   :0.000   Min.   :0.000  
    ##  1st Qu.:3.00   1st Qu.:2.000   1st Qu.:1.00   1st Qu.:1.000   1st Qu.:1.000  
    ##  Median :4.00   Median :3.000   Median :1.00   Median :1.000   Median :2.000  
    ##  Mean   :3.41   Mean   :2.801   Mean   :1.55   Mean   :1.503   Mean   :2.093  
    ##  3rd Qu.:4.00   3rd Qu.:4.000   3rd Qu.:2.00   3rd Qu.:2.000   3rd Qu.:3.000  
    ##  Max.   :4.00   Max.   :4.000   Max.   :4.00   Max.   :4.000   Max.   :4.000  
    ##     SilvrLin        NoCount     
    ##  Min.   :0.000   Min.   :0.000  
    ##  1st Qu.:2.000   1st Qu.:1.000  
    ##  Median :2.000   Median :1.000  
    ##  Mean   :2.443   Mean   :1.494  
    ##  3rd Qu.:3.000   3rd Qu.:2.000  
    ##  Max.   :4.000   Max.   :4.000

``` r
one_fac_cfa <- 'global_opt =~ XpctBest + BritSide + OptFutur + SilvrLin + GoWrong + NevrXpct + NevrWork + NoCount'
```

``` r
fit <- cfa(one_fac_cfa, data = dat, std.lv = TRUE, estimator = "ML")
summary(fit, fit.measure = TRUE, standardized = TRUE)
```

    ## lavaan 0.6-7 ended normally after 14 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         16
    ##                                                       
    ##   Number of observations                           549
    ##                                                       
    ## Model Test User Model:
    ##                                                       
    ##   Test statistic                               301.458
    ##   Degrees of freedom                                20
    ##   P-value (Chi-square)                           0.000
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                              1797.157
    ##   Degrees of freedom                                28
    ##   P-value                                        0.000
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.841
    ##   Tucker-Lewis Index (TLI)                       0.777
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)              -5634.607
    ##   Loglikelihood unrestricted model (H1)      -5483.878
    ##                                                       
    ##   Akaike (AIC)                               11301.214
    ##   Bayesian (BIC)                             11370.144
    ##   Sample-size adjusted Bayesian (BIC)        11319.353
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.160
    ##   90 Percent confidence interval - lower         0.144
    ##   90 Percent confidence interval - upper         0.176
    ##   P-value RMSEA <= 0.05                          0.000
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.078
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                             Standard
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   global_opt =~                                                         
    ##     XpctBest          0.450    0.048    9.411    0.000    0.450    0.409
    ##     BritSide          0.655    0.042   15.645    0.000    0.655    0.632
    ##     OptFutur          0.642    0.040   15.939    0.000    0.642    0.642
    ##     SilvrLin          0.571    0.041   14.008    0.000    0.571    0.578
    ##     GoWrong          -0.663    0.041  -16.087    0.000   -0.663   -0.646
    ##     NevrXpct         -0.874    0.039  -22.618    0.000   -0.874   -0.828
    ##     NevrWork         -0.790    0.038  -20.689    0.000   -0.790   -0.779
    ##     NoCount          -0.704    0.042  -16.629    0.000   -0.704   -0.663
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .XpctBest          1.009    0.063   16.110    0.000    1.009    0.833
    ##    .BritSide          0.644    0.043   15.034    0.000    0.644    0.600
    ##    .OptFutur          0.590    0.039   14.956    0.000    0.590    0.588
    ##    .SilvrLin          0.649    0.042   15.415    0.000    0.649    0.666
    ##    .GoWrong           0.612    0.041   14.915    0.000    0.612    0.582
    ##    .NevrXpct          0.351    0.031   11.497    0.000    0.351    0.315
    ##    .NevrWork          0.405    0.031   12.964    0.000    0.405    0.394
    ##    .NoCount           0.632    0.043   14.756    0.000    0.632    0.560
    ##     global_opt        1.000                               1.000    1.000

``` r
semPaths(fit, whatLabels = "est", rotation = 4, nCharNodes = 0, sizeMan2 = 6, sizeMan = 8, sizeLat = 16)
```

![](In-Class-Exercise-One_files/figure-gfm/draw%20diagram-1.png)<!-- -->

``` r
two_fac_cfa <- 'Optimism =~ XpctBest + BritSide + OptFutur + SilvrLin
                Pessimism =~ GoWrong + NevrXpct + NevrWork + NoCount'
```

``` r
fit <- cfa(two_fac_cfa, data = dat, std.lv = TRUE, estimator = "ML")
summary(fit, fit.measure = TRUE, standardized = TRUE)
```

    ## lavaan 0.6-7 ended normally after 17 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         17
    ##                                                       
    ##   Number of observations                           549
    ##                                                       
    ## Model Test User Model:
    ##                                                       
    ##   Test statistic                                55.575
    ##   Degrees of freedom                                19
    ##   P-value (Chi-square)                           0.000
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                              1797.157
    ##   Degrees of freedom                                28
    ##   P-value                                        0.000
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.979
    ##   Tucker-Lewis Index (TLI)                       0.970
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)              -5511.666
    ##   Loglikelihood unrestricted model (H1)      -5483.878
    ##                                                       
    ##   Akaike (AIC)                               11057.332
    ##   Bayesian (BIC)                             11130.569
    ##   Sample-size adjusted Bayesian (BIC)        11076.604
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.059
    ##   90 Percent confidence interval - lower         0.041
    ##   90 Percent confidence interval - upper         0.078
    ##   P-value RMSEA <= 0.05                          0.184
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.035
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                             Standard
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   Optimism =~                                                           
    ##     XpctBest          0.552    0.048   11.570    0.000    0.552    0.501
    ##     BritSide          0.865    0.040   21.804    0.000    0.865    0.835
    ##     OptFutur          0.766    0.039   19.441    0.000    0.766    0.765
    ##     SilvrLin          0.635    0.041   15.579    0.000    0.635    0.643
    ##   Pessimism =~                                                          
    ##     GoWrong           0.674    0.041   16.432    0.000    0.674    0.657
    ##     NevrXpct          0.922    0.038   24.269    0.000    0.922    0.873
    ##     NevrWork          0.830    0.038   22.080    0.000    0.830    0.818
    ##     NoCount           0.703    0.042   16.582    0.000    0.703    0.662
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   Optimism ~~                                                           
    ##     Pessimism        -0.668    0.032  -20.874    0.000   -0.668   -0.668
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .XpctBest          0.907    0.058   15.517    0.000    0.907    0.749
    ##    .BritSide          0.325    0.036    9.058    0.000    0.325    0.303
    ##    .OptFutur          0.415    0.035   11.786    0.000    0.415    0.415
    ##    .SilvrLin          0.572    0.040   14.307    0.000    0.572    0.587
    ##    .GoWrong           0.597    0.040   14.794    0.000    0.597    0.568
    ##    .NevrXpct          0.264    0.030    8.849    0.000    0.264    0.237
    ##    .NevrWork          0.340    0.030   11.478    0.000    0.340    0.331
    ##    .NoCount           0.634    0.043   14.747    0.000    0.634    0.562
    ##     Optimism          1.000                               1.000    1.000
    ##     Pessimism         1.000                               1.000    1.000

``` r
semPaths(fit, whatLabels = "est", rotation = 4, nCharNodes = 0, sizeMan2 = 6, sizeMan = 8, sizeLat = 16, sizeLat2 = 14)
```

![](In-Class-Exercise-One_files/figure-gfm/draw%20diagram%20for%20two%20factor%20model-1.png)<!-- -->

``` r
three_fac_cfa <- 'Optimism =~ v1*XpctBest + v1*BritSide + OptFutur + SilvrLin
                  Pessimism =~ GoWrong + NevrXpct + NevrWork + NoCount'
```

``` r
fit <- cfa(three_fac_cfa, data = dat, std.lv = TRUE, estimator = "ML")
summary(fit, fit.measure = TRUE, standardized = TRUE)
```

    ## lavaan 0.6-7 ended normally after 17 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         17
    ##   Number of equality constraints                     1
    ##                                                       
    ##   Number of observations                           549
    ##                                                       
    ## Model Test User Model:
    ##                                                       
    ##   Test statistic                                88.833
    ##   Degrees of freedom                                20
    ##   P-value (Chi-square)                           0.000
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                              1797.157
    ##   Degrees of freedom                                28
    ##   P-value                                        0.000
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.961
    ##   Tucker-Lewis Index (TLI)                       0.946
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)              -5528.295
    ##   Loglikelihood unrestricted model (H1)      -5483.878
    ##                                                       
    ##   Akaike (AIC)                               11088.589
    ##   Bayesian (BIC)                             11157.519
    ##   Sample-size adjusted Bayesian (BIC)        11106.728
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.079
    ##   90 Percent confidence interval - lower         0.063
    ##   90 Percent confidence interval - upper         0.096
    ##   P-value RMSEA <= 0.05                          0.002
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.075
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                             Standard
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   Optimism =~                                                           
    ##     XpctBest  (v1)    0.755    0.034   22.134    0.000    0.755    0.623
    ##     BritSide  (v1)    0.755    0.034   22.134    0.000    0.755    0.770
    ##     OptFutur          0.765    0.040   19.301    0.000    0.765    0.765
    ##     SilvrLin          0.640    0.041   15.597    0.000    0.640    0.648
    ##   Pessimism =~                                                          
    ##     GoWrong           0.674    0.041   16.420    0.000    0.674    0.657
    ##     NevrXpct          0.924    0.038   24.348    0.000    0.924    0.875
    ##     NevrWork          0.829    0.038   22.044    0.000    0.829    0.817
    ##     NoCount           0.703    0.042   16.577    0.000    0.703    0.662
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   Optimism ~~                                                           
    ##     Pessimism        -0.682    0.032  -21.484    0.000   -0.682   -0.682
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .XpctBest          0.897    0.061   14.678    0.000    0.897    0.612
    ##    .BritSide          0.392    0.033   11.800    0.000    0.392    0.407
    ##    .OptFutur          0.416    0.036   11.597    0.000    0.416    0.415
    ##    .SilvrLin          0.566    0.040   14.062    0.000    0.566    0.581
    ##    .GoWrong           0.598    0.040   14.809    0.000    0.598    0.569
    ##    .NevrXpct          0.261    0.030    8.789    0.000    0.261    0.234
    ##    .NevrWork          0.342    0.030   11.552    0.000    0.342    0.333
    ##    .NoCount           0.634    0.043   14.761    0.000    0.634    0.562
    ##     Optimism          1.000                               1.000    1.000
    ##     Pessimism         1.000                               1.000    1.000

``` r
semPaths(fit, whatLabels = "est", rotation = 4, nCharNodes = 0, sizeMan2 = 6, sizeMan = 8, sizeLat = 16, sizeLat2 = 14)
```

![](In-Class-Exercise-One_files/figure-gfm/draw%20diagram%20for%20two%20factor%20model%20with%20constrained%20loadings-1.png)<!-- -->
