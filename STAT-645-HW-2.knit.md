---
title: "STAT 645: Biostatistics" 
subtitle: Assignment 2
author: "Kat Gliszczynski"
date: '2022-09-24'
output:
  pdf_document: default
  html_document: default
  word_document: default
---


# Question 1
#### Suppose that we are interest in if treating index osteoporotic vertebral fractures with vertebroplasty increased in the risk of subsequent vertebral fractures. We evaluate 400 patients with index vertebral fractures, 200 of whom received vertebroplasty and 200 did not. After 2 years, we identified 48 subsequent fractures with the following fictitious distribution:

```r
data1 <- array(c(24,4,86,12,8,12,82,172), dim = c(2,2,2), dimnames = list(Care = c("Vertebroplasty", "Conservative Care"), VertFract = c("Yes","No"), Smoking = c("Smoking", "Non smoking")))
data1
```

```
## , , Smoking = Smoking
## 
##                    VertFract
## Care                Yes No
##   Vertebroplasty     24 86
##   Conservative Care   4 12
## 
## , , Smoking = Non smoking
## 
##                    VertFract
## Care                Yes  No
##   Vertebroplasty      8  82
##   Conservative Care  12 172
```
#### Check the confounding effect of smoking. 


```r
# Estimated OR
EOR <- ((24+4)*(82+172))/((8+12)*(86+12))
cbind("Estimated OR:", EOR)
```

```
##                      EOR               
## [1,] "Estimated OR:" "3.62857142857143"
```

```r
# Mantel-Haenszel Test
mantelhaen.test(data1)
```

```
## 
## 	Mantel-Haenszel chi-squared test with continuity correction
## 
## data:  data1
## Mantel-Haenszel X-squared = 0.036158, df = 1, p-value = 0.8492
## alternative hypothesis: true common odds ratio is not equal to 1
## 95 percent confidence interval:
##  0.5450984 2.4515996
## sample estimates:
## common odds ratio 
##          1.156012
```
The common odds ratio found in the Mantel-Haenszel test in this example is 1.156 which is significantly smaller than the Estimated Odds ratio of 3.6286. 

# Question 2
#### For the income by degree and gender data set, contained in the file inc_deg_data.csv (Modules/Data/incdeg):

```r
inc_deg_data <- read.csv("inc_deg_data.csv")
head(inc_deg_data)
```

```
##     income degree gender
## 1 46.01263      0      0
## 2 54.44378      0      0
## 3 52.15340      0      0
## 4 46.48443      0      0
## 5 39.31792      0      0
## 6 46.01348      0      0
```
#### a) Make side-by-side box plots of income, with separate boxes for each of female arts (gender = 0, degree = 0), female science (gender = 0, degree = 1), male arts (gender = 1, degree = 0), and male science (gender = 1, degree = 1). Include labels on the x-axis to indicate which box goes with which category. Does the figure indicate an additive or interaction model for income, justify your answer.

```r
deg = ifelse(inc_deg_data$degree==0, "Arts", "Science")
gen = ifelse(inc_deg_data$gender==0, "Female", "Male")
boxplot(inc_deg_data$income~deg+gen)
```

![](STAT-645-HW-2_files/figure-latex/unnamed-chunk-4-1.pdf)<!-- --> 
This figure indicates an additive model for income since the effect of major is assumed to be the same across gender on income.

#### b) Report the mean, median, standard deviation of income for each group separately.

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
options(dplyr.summarise.inform=FALSE)
summary_inc_deg_data = inc_deg_data %>%
  group_by(degree,gender) %>%
  summarise(mean=mean(income, na.rm=TRUE),
            median = median(income, na.rm=TRUE),
            stdev = sd(income, na.rm=TRUE))
summary_inc_deg_data
```

```
## # A tibble: 4 x 5
## # Groups:   degree [2]
##   degree gender  mean median stdev
##    <int>  <int> <dbl>  <dbl> <dbl>
## 1      0      0  50.2   49.5  4.91
## 2      0      1  49.6   49.4  5.01
## 3      1      0  72.9   74.2  5.33
## 4      1      1  69.8   69.9  4.74
```
In the above table degree = 0 is arts, degree = 1 is science, gender = 0 is female and gender = 1 is male.

# Question 3
#### With the calcium data in "calcium.txt", consider the Decrease variable as your response and Treatment as your treatment. In what follows, I have recoded Treatment to equal 0 for placebo and 1 for calcium treatment. Consider the regression model: Decreasei = β0 + β1Treatmenti + Ei.

```r
calcium <- read.table('calcium.txt',header=TRUE)
calcium$TreatmentNum <- ifelse(calcium$Treatment == "Calcium", 1, 0)
head(calcium)
```

```
##   Treatment Begin End Decrease TreatmentNum
## 1   Calcium   107 100        7            1
## 2   Calcium   110 114       -4            1
## 3   Calcium   123 105       18            1
## 4   Calcium   129 112       17            1
## 5   Calcium   112 115       -3            1
## 6   Calcium   111 116       -5            1
```
#### a) Explain and interpret all components of the model ####
Beta-0 is the intercept of the regression line. In this example it is the average decrease when the treatment = placebo. 
Beta-1 is the value of the slope for the regression line. In this example this refers to the average difference in decrease between the treatment = placebo and treatment = calcium. 
Treatmenti is a dummy and a categorical variable for this example and it takes on the value 1 if the treatment is calcium and 0 if the treatment is placebo. 
Decreasei the response and a numeric variable and is the difference in mean decrease between calcium and placebo treatments.
Ei is the error term which represents the difference between the expected decrease for treatment = placebo and the decrease that was actually observed for treatment = placebo.

#### b) Fit the above model to the data and test the effect of the treatment

```r
question3 <- lm(Decrease~TreatmentNum,data=calcium)
summary(question3)
```

```
## 
## Call:
## lm(formula = Decrease ~ TreatmentNum, data = calcium)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -10.7273  -4.7273  -0.7273   5.0000  13.0000 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)
## (Intercept)   -0.2727     2.2266  -0.122    0.904
## TreatmentNum   5.2727     3.2267   1.634    0.119
## 
## Residual standard error: 7.385 on 19 degrees of freedom
## Multiple R-squared:  0.1232,	Adjusted R-squared:  0.07708 
## F-statistic:  2.67 on 1 and 19 DF,  p-value: 0.1187
```
 Decreasei = -0.2727 + (5.2727)Treatmenti 
Ho: no effect of calcium treatment (Beta-1 = 0) vs Ha: there is some effect of calcium treatment (Beta-1 <> 0).
We fail to reject the null hypotheses since the p-value is 0.1187 which is greater than 0.05 so we do not have significant evidence to conclude that there is an effect of calcium treatment.

#### c) Assuming that the Ei are normally distributed, what is the estimated distribution of Decrease when Treatment = 1? ####
Decreasei = -0.2727 + (5.2727)*1
Decreasei = 5.000

#### d) Are the assumptions on E satisfied? Justify your answer

```r
out100 <- qqnorm(residuals(question3))
out200 <- lm(out100$y ~ out100$x)
abline(a=out200$coefficients[1],b=out200$coefficients[2],col="red")
```

![](STAT-645-HW-2_files/figure-latex/unnamed-chunk-8-1.pdf)<!-- --> 
The Q-Q plot shown above is approximately normal. So the assumption of normality is not violated.

```r
plot(predict(question3),residuals(question3))
abline(h=0,col="red")
```

![](STAT-645-HW-2_files/figure-latex/unnamed-chunk-9-1.pdf)<!-- --> 
There are only 2 possible values of X corresponding to the 2 different predicted values of the response. Therefor the residuals are concentrated on two different vertical lines. The plot does not show any obvious sign of changing variability across the groups defined by X. So the property of homogeneity is not violated.

#### e) Now analyze the same data using a two-sample t-test, assuming equal vairances, and without assuming equal variances. Which of the two results is equivalent to the above regression result and why?

```r
calcium
```

```
##    Treatment Begin End Decrease TreatmentNum
## 1    Calcium   107 100        7            1
## 2    Calcium   110 114       -4            1
## 3    Calcium   123 105       18            1
## 4    Calcium   129 112       17            1
## 5    Calcium   112 115       -3            1
## 6    Calcium   111 116       -5            1
## 7    Calcium   107 106        1            1
## 8    Calcium   112 102       10            1
## 9    Calcium   136 125       11            1
## 10   Calcium   102 104       -2            1
## 11   Placebo   123 124       -1            0
## 12   Placebo   109  97       12            0
## 13   Placebo   112 113       -1            0
## 14   Placebo   102 105       -3            0
## 15   Placebo    98  95        3            0
## 16   Placebo   114 119       -5            0
## 17   Placebo   119 114        5            0
## 18   Placebo   112 114        2            0
## 19   Placebo   110 121      -11            0
## 20   Placebo   117 118       -1            0
## 21   Placebo   130 133       -3            0
```

```r
x <- subset(calcium, TreatmentNum == 0)
y <- subset(calcium, TreatmentNum == 1)
x
```

```
##    Treatment Begin End Decrease TreatmentNum
## 11   Placebo   123 124       -1            0
## 12   Placebo   109  97       12            0
## 13   Placebo   112 113       -1            0
## 14   Placebo   102 105       -3            0
## 15   Placebo    98  95        3            0
## 16   Placebo   114 119       -5            0
## 17   Placebo   119 114        5            0
## 18   Placebo   112 114        2            0
## 19   Placebo   110 121      -11            0
## 20   Placebo   117 118       -1            0
## 21   Placebo   130 133       -3            0
```

```r
y
```

```
##    Treatment Begin End Decrease TreatmentNum
## 1    Calcium   107 100        7            1
## 2    Calcium   110 114       -4            1
## 3    Calcium   123 105       18            1
## 4    Calcium   129 112       17            1
## 5    Calcium   112 115       -3            1
## 6    Calcium   111 116       -5            1
## 7    Calcium   107 106        1            1
## 8    Calcium   112 102       10            1
## 9    Calcium   136 125       11            1
## 10   Calcium   102 104       -2            1
```

```r
t.test(x$Decrease,y$Decrease,alternative = "two.sided")
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  x$Decrease and y$Decrease
## t = -1.6037, df = 15.591, p-value = 0.1288
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -12.257493   1.712039
## sample estimates:
##  mean of x  mean of y 
## -0.2727273  5.0000000
```
The mean of x is equivalent to the value of beta-1 and the mean of y is equivalent the estimated distribution of Decrease when Treatment = 1.

# Question 4
#### With the onset data in "onset_data.csv", conduct the following analysis.

```r
onset_data <- read.csv('onset_data(1).csv')
head(onset_data)
```

```
##      onset tx prior      age
## 1 12.37283  1     1 34.88794
## 2 57.85721  1     0 21.75299
## 3 20.86530  1     1 48.38736
## 4 37.91373  1     0 46.30762
## 5 10.10069  1     1 29.99423
## 6 53.70279  1     0 32.00219
```

```r
onset_data[,'tx'] <- factor(onset_data[,'tx'])
onset_data[,'prior'] <- factor(onset_data[,'prior'])
i <- ifelse(onset_data$tx==0, "tx=no","tx=yes")
ii <- ifelse(onset_data$prior==0,"prior=no",'prior=yes')
head(onset_data)
```

```
##      onset tx prior      age
## 1 12.37283  1     1 34.88794
## 2 57.85721  1     0 21.75299
## 3 20.86530  1     1 48.38736
## 4 37.91373  1     0 46.30762
## 5 10.10069  1     1 29.99423
## 6 53.70279  1     0 32.00219
```
#### a) Create side-by-side box plots comparing time to onset with (i) the tx variable and (ii) the prior variable. Comment.

```r
prior = ifelse(onset_data$prior==0, "Prior 0", "Prior 1")
tx = ifelse(onset_data$tx==0, "TX 0", "TX 1")
boxplot(onset_data$onset~tx+prior)
```

![](STAT-645-HW-2_files/figure-latex/unnamed-chunk-12-1.pdf)<!-- --> 
This figure indicates an additive model for prior since the effect of onset is assumed to be the same across prior on TX.

#### b) Create a scatterplot of onset vs. age. Color code the points by prior status. Also, fit and overlay separate lowess curves, one each for prior = 0 and prior = 1.

```r
library("ggplot2")
ggplot(onset_data,aes(x=age,y=onset,col=prior))+geom_point()+geom_smooth()
```

```
## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
```

![](STAT-645-HW-2_files/figure-latex/unnamed-chunk-13-1.pdf)<!-- --> 

#### c) Fit the regression model: y = β0 + β1tx + β2prior + β3age + β4(prior x age) + E

```r
question4 <- lm(onset~tx+prior+age+(prior*age),data=onset_data)
summary(question4)
```

```
## 
## Call:
## lm(formula = onset ~ tx + prior + age + (prior * age), data = onset_data)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -21.4079  -2.6405   0.4422   2.5607  12.0187 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  70.91644    2.29513  30.899   <2e-16 ***
## tx1           2.11154    0.91918   2.297   0.0234 *  
## prior1      -69.24092    3.21787 -21.518   <2e-16 ***
## age          -0.71677    0.05334 -13.438   <2e-16 ***
## prior1:age    0.92996    0.07541  12.332   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.033 on 115 degrees of freedom
## Multiple R-squared:  0.9226,	Adjusted R-squared:  0.9199 
## F-statistic: 342.6 on 4 and 115 DF,  p-value: < 2.2e-16
```
onseti = 70.9164 + 2.1115(txi) + -69.2409(priori) + -0.7167(agei) + 0.9299(priori*agei)

#### d) Carry out a test of the effect of prior using the above model. 
Ho: There is no effect of the prior in the above model vs Ha: There is an effect of the prior in the above model
In this example we reject the null hypothesis since the p-value is 2.2e^-16 and that is less than 0.05 so we conclude that there is an effect of the prior in the above model. 

#### e) What is the 95% confidence interval for the mean difference in onset times between the treatment and control groups, holding prior status and age constant?

```r
b1 <- summary(question4)$coefficients[2,1]
se <- summary(question4)$coefficients[2,2]
ul <- b1 + se*1.96
ll <- b1 - se*1.96
ul
```

```
## [1] 3.913131
```

```r
ll
```

```
## [1] 0.3099499
```
#### f) What is the 95% confidence interval for the mean response of a treated individual, age 35, with no prior tumor incidence?

```r
library(dplyr)
v <- as.matrix(c(1,1,0,35,0))
beta <- summary(question4)$coefficients[,1]
estimate <- sum(v*beta)
cov <- as.matrix(vcov(question4))
est <- t(v)%*%cov%*%v
sqrt_est <- sqrt(est)
ul <- estimate + qt(0.025,120-5)*sqrt_est
ll <- estimate - qt(0.025,120-5)*sqrt_est
(CI <- c(ul,ll))
```

```
## [1] 46.25858 49.62375
```

# Question 5
#### For the pollution data in "pollute_data.csv" [Note: There is at least one missing value in these data. Just remove any records with at least one missing value prior to doing hte following analyses.]:

```r
pollute_data <- read.csv('pollute_data(1).csv')
pollute_data <- pollute_data[complete.cases(pollute_data),]
head(pollute_data)
```

```
##                          city JanTemp JulyTemp RelHum Rain Mortality Education
## 1                   Akron, OH      27       71     59   36    921.87      11.4
## 2 Albany-Schenectady-Troy, NY      23       72     57   35    997.87      11.0
## 3 Allentown, Bethlehem, PA-NJ      29       74     54   44    962.35       9.8
## 4                 Atlanta, GA      45       79     56   47    982.29      11.1
## 5               Baltimore, MD      35       77     55   43   1071.29       9.6
## 6              Birmingham, AL      45       80     54   53   1030.38      10.2
##   PopDensity X.NonWhite X.WC     pop pop.house income HCPot NOxPot S02Pot NOx
## 1       3243        8.8 42.6  660328      3.34  29560    21     15     59  15
## 2       4281        3.5 50.7  835880      3.14  31458     8     10     39  10
## 3       4260        0.8 39.4  635481      3.21  31856     6      6     33   6
## 4       3125       27.1 50.2 2138231      3.41  32452    18      8     24   8
## 5       6441       24.4 43.7 2199531      3.44  32368    43     38    206  38
## 6       3325       38.5 43.1  883946      3.45  27835    30     32     72  32
```
#### a) Based on the model: Mortalityi = β0 + β1 ×HCPoti + Ei, does there appear to be a significant linear relationship between HCPot and Mortality?

```r
question5 <- lm(Mortality~HCPot,data=pollute_data)
summary(question5)
```

```
## 
## Call:
## lm(formula = Mortality ~ HCPot, data = pollute_data)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -142.156  -42.699    4.474   41.206  169.686 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 945.96566    8.73448  108.30   <2e-16 ***
## HCPot        -0.12457    0.08771   -1.42    0.161    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 61.88 on 57 degrees of freedom
## Multiple R-squared:  0.03418,	Adjusted R-squared:  0.01723 
## F-statistic: 2.017 on 1 and 57 DF,  p-value: 0.161
```
Ho: There not a significant linear relationship between HCPot and Mortality vs Ha: There is a significant linear relationship between HCPot and Mortality
We fail to reject the null hypothesis since the p-value = 0.161 and it is greater than 0.05 so we conclude that there is not a significant linear relationship between HCPot and Mortality. 
#### b) Make a scatterplot of HCPot versus Mortality. Do you notice anything unusual that might have impacted your model in 1(a). Dig into the data and provide an explanation for any unusual features you notice. 

```r
plot(pollute_data$Mortality,pollute_data$HCPot,xlab="Mortality",ylab="HCPot")
```

![](STAT-645-HW-2_files/figure-latex/unnamed-chunk-19-1.pdf)<!-- --> 
While most of the points are dristributed near each other in a nearly straight horizontal line, there are some outliers in the data which could be impacting the model. These four points with the highest HCPots are all from California. 

#### c) Compare the Californaia records to all others, in terms of each of the following:

```r
library(dplyr)
California <- pollute_data%>%filter(grepl("CA", city, ignore.case=F))
NoCali <- subset(pollute_data, HCPot < 105)
```
##### i) Percent of white-collar workers

```r
mean(California$X.WC)
```

```
## [1] 50.75
```

```r
mean(NoCali$X.WC)
```

```
## [1] 46.06909
```
##### ii) Median income

```r
mean(California$income)
```

```
## [1] 39792.5
```

```r
mean(NoCali$income)
```

```
## [1] 32770.6
```
##### iii) Population per household

```r
mean(California$pop.house)
```

```
## [1] 3.0125
```

```r
mean(NoCali$pop.house)
```

```
## [1] 3.263636
```
##### iv) Percent non-white residents

```r
mean(California$X.NonWhite) 
```

```
## [1] 7.6
```

```r
mean(NoCali$X.NonWhite)
```

```
## [1] 12.18727
```
##### v) Mean July temperature

```r
mean(California$JulyTemp)
```

```
## [1] 67.25
```

```r
mean(NoCali$JulyTemp)
```

```
## [1] 74.92727
```
##### vi) Annual rainfall

```r
mean(California$Rain)  
```

```
## [1] 13
```

```r
mean(NoCali$Rain)
```

```
## [1] 40.36364
```
#### d) Write down the model for Mortality as a funciton of log(HCPot), as well as all variables from 1(c). Note: By "write down the model", I mean for you to write down an equation analogous to equation (1) above. 
Mortalityi = β0 + β1 x log(HCpoti) + β2 x X.WCi + β3 x incomei + β4 x pop.housei + β5 x X.NonWhitie + β6 x JulyTempi + β7 x Raini + Ei

#### e) Interpret the regression parameter corresponding to log(HCPot) of the model in 1(d).
β1 is the average increase in Mortality when log(HCpoti) is increased by 1 when other variables are held constant. 

#### f) Using the likelihood ratio test, test the null hypothesis that all coefficients other than that for log (HCPot) equal 0, in the model from 1(d). Test at α = 0.05.

```r
newmod <- pollute_data[,c(3,5,6,9,10,12,13,14)]
head(newmod)
```

```
##   JulyTemp Rain Mortality X.NonWhite X.WC pop.house income HCPot
## 1       71   36    921.87        8.8 42.6      3.34  29560    21
## 2       72   35    997.87        3.5 50.7      3.14  31458     8
## 3       74   44    962.35        0.8 39.4      3.21  31856     6
## 4       79   47    982.29       27.1 50.2      3.41  32452    18
## 5       77   43   1071.29       24.4 43.7      3.44  32368    43
## 6       80   53   1030.38       38.5 43.1      3.45  27835    30
```

```r
newmod$logHCPot <- log(newmod$HCPot)
head(newmod)
```

```
##   JulyTemp Rain Mortality X.NonWhite X.WC pop.house income HCPot logHCPot
## 1       71   36    921.87        8.8 42.6      3.34  29560    21 3.044522
## 2       72   35    997.87        3.5 50.7      3.14  31458     8 2.079442
## 3       74   44    962.35        0.8 39.4      3.21  31856     6 1.791759
## 4       79   47    982.29       27.1 50.2      3.41  32452    18 2.890372
## 5       77   43   1071.29       24.4 43.7      3.44  32368    43 3.761200
## 6       80   53   1030.38       38.5 43.1      3.45  27835    30 3.401197
```

```r
newmod1 <- newmod[,c(1,2,3,4,5,6,7,9)]
head(newmod1)
```

```
##   JulyTemp Rain Mortality X.NonWhite X.WC pop.house income logHCPot
## 1       71   36    921.87        8.8 42.6      3.34  29560 3.044522
## 2       72   35    997.87        3.5 50.7      3.14  31458 2.079442
## 3       74   44    962.35        0.8 39.4      3.21  31856 1.791759
## 4       79   47    982.29       27.1 50.2      3.41  32452 2.890372
## 5       77   43   1071.29       24.4 43.7      3.44  32368 3.761200
## 6       80   53   1030.38       38.5 43.1      3.45  27835 3.401197
```

```r
newmod2 <- newmod[,c(1,2,3,4,5,6,7)]
out500 <- lm(Mortality~.,data=newmod1)
summary(out500)
```

```
## 
## Call:
## lm(formula = Mortality ~ ., data = newmod1)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -87.32 -21.85  -3.04  25.78 113.38 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 858.639846 222.144989   3.865 0.000315 ***
## JulyTemp     -0.862296   1.973816  -0.437 0.664052    
## Rain          2.131673   0.619165   3.443 0.001158 ** 
## X.NonWhite    3.174328   1.033974   3.070 0.003426 ** 
## X.WC         -2.397372   1.215760  -1.972 0.054055 .  
## pop.house    40.649881  35.965769   1.130 0.263663    
## income       -0.001247   0.001419  -0.879 0.383750    
## logHCPot     16.996502   7.574546   2.244 0.029208 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 41.54 on 51 degrees of freedom
## Multiple R-squared:  0.6106,	Adjusted R-squared:  0.5571 
## F-statistic: 11.42 on 7 and 51 DF,  p-value: 1.297e-08
```

```r
out500.new <- lm(Mortality~.,data=newmod2)
logLik(out500)
```

```
## 'log Lik.' -299.2929 (df=9)
```

```r
logLik(out500.new)
```

```
## 'log Lik.' -302.0704 (df=8)
```

```r
TS <- 2*(as.numeric(logLik(out500))-as.numeric(logLik(out500.new)))
TS
```

```
## [1] 5.55498
```

```r
pvalue <- 1-pchisq(TS,1)
pvalue
```

```
## [1] 0.01842819
```
We reject the null hypothesis that all coefficents other than that for log(HCPot) equal zero since the p-value from our likelihood ratio test is 0.018 which is less than 0.05 and can conclude that there is sufficient evidence that at least one coefficient is not equal to 0. 

#### g) Using the likelihood ratio test, test the null hypothesis that the coefficients for percent white collar and percent non-white sum to zero. Test at α = 0.05.

```r
out600 <- lm(Mortality~.,data=newmod1)
summary(out600)
```

```
## 
## Call:
## lm(formula = Mortality ~ ., data = newmod1)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -87.32 -21.85  -3.04  25.78 113.38 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 858.639846 222.144989   3.865 0.000315 ***
## JulyTemp     -0.862296   1.973816  -0.437 0.664052    
## Rain          2.131673   0.619165   3.443 0.001158 ** 
## X.NonWhite    3.174328   1.033974   3.070 0.003426 ** 
## X.WC         -2.397372   1.215760  -1.972 0.054055 .  
## pop.house    40.649881  35.965769   1.130 0.263663    
## income       -0.001247   0.001419  -0.879 0.383750    
## logHCPot     16.996502   7.574546   2.244 0.029208 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 41.54 on 51 degrees of freedom
## Multiple R-squared:  0.6106,	Adjusted R-squared:  0.5571 
## F-statistic: 11.42 on 7 and 51 DF,  p-value: 1.297e-08
```

```r
head(newmod1)
```

```
##   JulyTemp Rain Mortality X.NonWhite X.WC pop.house income logHCPot
## 1       71   36    921.87        8.8 42.6      3.34  29560 3.044522
## 2       72   35    997.87        3.5 50.7      3.14  31458 2.079442
## 3       74   44    962.35        0.8 39.4      3.21  31856 1.791759
## 4       79   47    982.29       27.1 50.2      3.41  32452 2.890372
## 5       77   43   1071.29       24.4 43.7      3.44  32368 3.761200
## 6       80   53   1030.38       38.5 43.1      3.45  27835 3.401197
```

```r
out500.new <- lm(Mortality~ JulyTemp+Rain+Mortality+X.NonWhite-X.WC+pop.house+income+logHCPot, data=newmod1)
```

```
## Warning in model.matrix.default(mt, mf, contrasts): the response appeared on the
## right-hand side and was dropped
```

```
## Warning in model.matrix.default(mt, mf, contrasts): problem with term 3 in
## model.matrix: no columns are assigned
```

```r
logLik(out600)
```

```
## 'log Lik.' -299.2929 (df=9)
```

```r
logLik(out500.new)
```

```
## 'log Lik.' -301.4605 (df=8)
```

```r
TS <- 2*(as.numeric(logLik(out600))-as.numeric(logLik(out500.new)))
TS
```

```
## [1] 4.335149
```

```r
pvalue <- 1-pchisq(TS,1)
pvalue
```

```
## [1] 0.03733316
```
We reject the null hypothesis that the coefficients for percent white collar and percent non-white sum to zero at the 0.05 level since the p-value is .0373 which is less than 0.05. Therefore we have sufficient evidence that the coefficients for percent white collar and percent non-white do not sum to zero.

#### h) Report a 95% confidence interval for the sum of the coefficients for percent white collar and percent non-white.

```r
summary(out500)
```

```
## 
## Call:
## lm(formula = Mortality ~ ., data = newmod1)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -87.32 -21.85  -3.04  25.78 113.38 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 858.639846 222.144989   3.865 0.000315 ***
## JulyTemp     -0.862296   1.973816  -0.437 0.664052    
## Rain          2.131673   0.619165   3.443 0.001158 ** 
## X.NonWhite    3.174328   1.033974   3.070 0.003426 ** 
## X.WC         -2.397372   1.215760  -1.972 0.054055 .  
## pop.house    40.649881  35.965769   1.130 0.263663    
## income       -0.001247   0.001419  -0.879 0.383750    
## logHCPot     16.996502   7.574546   2.244 0.029208 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 41.54 on 51 degrees of freedom
## Multiple R-squared:  0.6106,	Adjusted R-squared:  0.5571 
## F-statistic: 11.42 on 7 and 51 DF,  p-value: 1.297e-08
```

```r
whitecol <- summary(out500)$coefficients[5,1]
whitecolse <- summary(out500)$coefficients[5,2]
NW <- summary(out500)$coefficients[4,1]
NWse <- summary(out500)$coefficients[4,2]
est2 <- NW + whitecol
cov <- as.matrix(vcov(out500))
cov
```

```
##               (Intercept)      JulyTemp          Rain    X.NonWhite
## (Intercept)  4.934840e+04 -3.122629e+02 -3.014675e+01  1.574720e+02
## JulyTemp    -3.122629e+02  3.895950e+00 -2.672296e-02 -1.421022e+00
## Rain        -3.014675e+01 -2.672296e-02  3.833654e-01 -1.853590e-01
## X.NonWhite   1.574720e+02 -1.421022e+00 -1.853590e-01  1.069103e+00
## X.WC        -5.969610e+01 -3.236197e-01 -2.725273e-02  7.397953e-02
## pop.house   -5.738964e+03  8.994093e+00  2.536522e+00 -1.372991e+01
## income      -7.784421e-02  6.074187e-05  2.155612e-04 -5.981742e-05
## logHCPot    -1.001664e+03  8.598396e+00  1.941473e+00 -5.058897e+00
##                      X.WC     pop.house        income      logHCPot
## (Intercept) -59.696101264 -5.738964e+03 -7.784421e-02 -1.001664e+03
## JulyTemp     -0.323619684  8.994093e+00  6.074187e-05  8.598396e+00
## Rain         -0.027252729  2.536522e+00  2.155612e-04  1.941473e+00
## X.NonWhite    0.073979528 -1.372991e+01 -5.981742e-05 -5.058897e+00
## X.WC          1.478072428  1.056372e+01 -4.938710e-04 -8.902887e-01
## pop.house    10.563717480  1.293537e+03  7.085267e-03  7.498026e+01
## income       -0.000493871  7.085267e-03  2.013785e-06 -4.684828e-04
## logHCPot     -0.890288717  7.498026e+01 -4.684828e-04  5.737374e+01
```

```r
estse <- whitecolse+NWse
UL <- est2+estse*qt(0.025,59-8)
UL
```

```
## [1] -3.739574
```

```r
LL <- est2-estse*qt(0.025,59-8)
LL
```

```
## [1] 5.293486
```

```r
(CI <- c(UL,LL))
```

```
## [1] -3.739574  5.293486
```

```r
estimate <- sum(v*beta)
cov <- as.matrix(vcov(question4))
est <- t(v)%*%cov%*%v
sqrt_est <- sqrt(est)
ul <- estimate + qt(0.025,120-5)*sqrt_est
ll <- estimate - qt(0.025,120-5)*sqrt_est
(CI <- c(ul,ll))
```

```
## [1] 46.25858 49.62375
```
#### i) For the model that includes log(HCPot), as well as all variables from 1(c), identify any potential leverage or influential points from this data. ####

```r
# leverage points
plot(hatvalues(out500), type="h")
abline(h=2*length(out500$coef)/59)
points(hatvalues(out500),pch=21, col="blue",bg=2)
```

![](STAT-645-HW-2_files/figure-latex/unnamed-chunk-30-1.pdf)<!-- --> 

```r
levpts <- hatvalues(out500) >= 2*length(out500$coeff)/59
levpts
```

```
##     1     2     3     4     5     6     7     8     9    10    11    12    13 
## FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE 
##    14    15    16    17    18    19    20    22    23    24    25    26    27 
## FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE 
##    28    29    30    31    32    33    34    35    36    37    38    39    40 
## FALSE  TRUE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE 
##    41    42    43    44    45    46    47    48    49    50    51    52    53 
## FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE 
##    54    55    56    57    58    59    60 
## FALSE FALSE FALSE FALSE FALSE  TRUE FALSE
```

There are 5 possible leverage points shown in the graph above. They are points 59,48,29,16 and 8.

```r
# influential observation
plot(cooks.distance(out500), type="h")
points(cooks.distance(out500), pch=21, col="blue", bg=2)
```

![](STAT-645-HW-2_files/figure-latex/unnamed-chunk-31-1.pdf)<!-- --> 

```r
# DFBETA values
out5000 <- dfbetas(out500)
head(out5000)
```

```
##    (Intercept)    JulyTemp         Rain   X.NonWhite        X.WC   pop.house
## 1 -0.032703684  0.04322910  0.007897073  0.009481692  0.02962137 -0.03796665
## 2  0.268709371 -0.23329390 -0.234055883  0.047949519  0.44087778 -0.15811168
## 3 -0.001058079  0.10452246  0.093176543 -0.197468371 -0.23533575 -0.03532894
## 4  0.027828251  0.02052950 -0.013642683 -0.075798212 -0.07867618 -0.04003149
## 5 -0.114910537  0.04937003  0.082264439  0.037839472 -0.06356667  0.13550811
## 6 -0.159102709  0.17568161 -0.069389809 -0.465744291  0.02647294  0.04808213
##        income    logHCPot
## 1  0.06849899 -0.01735115
## 2 -0.35204514 -0.37717229
## 3  0.05430651  0.03593042
## 4  0.01112997  0.01180107
## 5  0.01949711  0.17162468
## 6  0.17548186  0.01712237
```

```r
threshold = 2/sqrt(nrow(pollute_data))
threshold
```

```
## [1] 0.2603778
```

```r
# measure influence of observations on the regression param for zn
plot(out5000, type='h')
abline(h=threshold, lty=2)
abline(h = -threshold, lty=2)
```

![](STAT-645-HW-2_files/figure-latex/unnamed-chunk-31-2.pdf)<!-- --> 
In the above plot there are a couple of points that fall outside of the fixed threshold that can be considered influential points. 

# Question 6
#### Verify the following three results of the paper: Whittemore AS. Estimating attributable risk from case-control studies. American Journal of Epidemiology. 1983 Jan;117(1):76–85.
#### a) the estimate of risk attributable to late age at first childbirth, adjusted for age at diagnosis, is 14.8% with the SE 3.1%,

```r
library(scales)
x = c(1,3,9,8,6,20,19,31,42,60,45,68,72,97,43,70,38,70,31,28,36,39)+
    c(0,0,0,0,7,8,8,16,19,29,26,44,34,54,22,40,18,24,15,24,22,41)
y = c(0,5,7,13,16,50,25,65,46,84,55,116,36,74,23,27,19,35,17,24,13,14)+
    c(1,6,4,13,16,52,18,74,36,128,43,111,38,99,29,84,36,67,21,39,39,70)
mydata1 <- array(c(x,y), dim = c(2, 11 , 2),
dimnames = list(t = c("Cases", "Controls"),
Age =c("25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
       "60-64", "65-69", "70-74", "75+"), First = c(">24", "<24")))
mydata1
```

```
## , , First = >24
## 
##           Age
## t          25-29 30-34 35-39 40-44 45-49 50-54 55-59 60-64 65-69 70-74 75+
##   Cases        1     9    13    27    61    71   106    65    56    46  58
##   Controls     3     8    28    47    89   112   151   110    94    52  80
## 
## , , First = <24
## 
##           Age
## t          25-29 30-34 35-39 40-44 45-49 50-54 55-59 60-64 65-69 70-74 75+
##   Cases        1    11    32    43    82    98    74    52    55    38  52
##   Controls    11    26   102   139   212   227   173   111   102    63  84
```

```r
n = 1051 # total number of cases.
y.k = x2k = y2k = y1k = numeric(11)

for (k in 1:11){
  y.k[k] = sum(mydata1[, k, ][2, ])
  x2k[k] = mydata1[, k, ][1,2]
  y2k[k] = mydata1[, k, ][2,2]
  y1k[k] = mydata1[, k, ][2,1]
}

AR = 1 - crossprod(y.k, x2k/y2k)/n
SE = 1/n * (sum((y.k * x2k/y2k)**2 * (1/y2k + y1k/(y.k*y2k))) -n * (1-AR)**2)**(1/2) 
cbind("AR= ", percent(AR, accuracy=0.1))
```

```
##      [,1]   [,2]   
## [1,] "AR= " "14.8%"
```

```r
cbind("SE= ", percent(SE, accuracy=0.1))
```

```
##      [,1]   [,2]  
## [1,] "SE= " "1.6%"
```
#### b) the attributable risk estimate adjusting both for age and years of education is 10.5% with a 3.8%,

```r
a <- 1+0+9+0+6+7+19+8+42+19+45+26+72+34+43+22+38+18+31+15+36+22
b <- 3+0+8+0+20+8+31+16+60+29+68+44+97+54+70+40+70+24+28+24+39+41
c <- 0+1+7+4+16+16+25+18+46+36+55+43+36+38+23+29+19+36+17+21+13+39
d <- 5+6+13+13+50+52+65+74+84+128+116+111+74+99+27+84+35+67+24+39+14+70
AR <- (a/(a+c))-(b/(b+d))
sd <- (1/n)*(sqrt(((a*c)/((a+c)^2))+((b*d)/((b+d)^2))))
cbind("AR= ", percent(AR, 0.01))
```

```
##      [,1]   [,2]    
## [1,] "AR= " "10.57%"
```

```r
cbind("SD= ", percent(sd, 0.01))
```

```
##      [,1]   [,2]   
## [1,] "SD= " "0.07%"
```
#### c) the unadjusted and asymptotically biased estimate of attributable risk obtained by pooling the data over age at diagnosis and years of education si 17.1% with a SE 3.5%.

```r
a <- 1+0+9+0+6+7+19+8+42+19+45+26+72+34+43+22+38+18+31+15+36+22
b <- 3+0+8+0+20+8+31+16+60+29+68+44+97+54+70+40+70+24+28+24+39+41
c <- 0+1+7+4+16+16+25+18+46+36+55+43+36+38+23+29+19+36+17+21+13+39
d <- 5+6+13+13+50+52+65+74+84+128+116+111+74+99+27+84+35+67+24+39+14+70
AR <- (a/(a+c))-(b/(b+d))
sd <- (1/n)*(sqrt(((a*b)/((a+b)^2))+((c*d)/((c+d)^2))))
cbind("AR= ", percent(AR, 0.01))
```

```
##      [,1]   [,2]    
## [1,] "AR= " "10.57%"
```

```r
cbind("SD= ", percent(sd, 0.01))
```

```
##      [,1]   [,2]   
## [1,] "SD= " "0.06%"
```


