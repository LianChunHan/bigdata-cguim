Pima Indians Diabetes Database
================

???ƫe?B?z
---------

???B?z?ʭ?,?çR???L?????Ѽ?,?̫??N1/3???H?\]???V?m?ջP?t?~2/3?????ղմ??? ???L?V?m???G

?w???ҫ??إ?
----------

?ϥΰj?k?��R,?}???f?O?\_?|?o ?Ndiabetes?\]??y,[???L?U?ܶ????@x](mailto:???L?U?ܶ????@x)

``` r
fit<-glm(diabetes~.,PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==F,],family="binomial")
library(MASS)
finalFit<-stepAIC(fit,direction = "both",trace = F)
summary(finalFit)$coefficients
```

    ##                 Estimate   Std. Error   z value     Pr(>|z|)
    ## (Intercept)  7.938950622 0.8654282909  9.173436 4.581790e-20
    ## pregnant    -0.102532611 0.0381468407 -2.687840 7.191581e-03
    ## glucose     -0.034322107 0.0044170167 -7.770427 7.822179e-15
    ## pressure     0.013362751 0.0065433212  2.042197 4.113199e-02
    ## insulin      0.001630088 0.0009005087  1.810185 7.026705e-02
    ## mass        -0.084615633 0.0165147963 -5.123626 2.997158e-07
    ## pedigree    -0.659597537 0.3490904357 -1.889475 5.882825e-02
    ## age         -0.018709419 0.0112192356 -1.667620 9.539122e-02

install.packages("caret") install.packages("lattice") install.packages("ggplot2") \#\#?w???ҫ????ҡA???? ?N??????pos?令?Ĥ@,sesitvity?w???ȹF0.73???k

    ## Warning: package 'caret' was built under R version 3.2.5

    ## Loading required package: lattice

    ## Warning: package 'lattice' was built under R version 3.2.5

    ## Loading required package: ggplot2

    ## Warning: package 'ggplot2' was built under R version 3.2.5

    ## [1] 0.7674419

    ## [1] 0.7529412

    ## [1] 0.6111111

    ## [1] 0.8648649
