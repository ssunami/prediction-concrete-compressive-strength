Compressive Strength Project - R code
================

``` r
library(readxl)
library(olsrr)
data <- read_excel("Concrete_Data.xls")
names(data) <- c("cement", "blast_furnace_slag","fly_ash","water","superplasticizer","coarse_aggregate","fine_aggregate","age","concrete_compressive_strength")
```

``` r
# Load necessary libraries
library(ggplot2)
library(ggpubr)

#Plot the response variable vs each explanatory variable 
p1 <- ggplot(data, aes(x = cement, y = concrete_compressive_strength)) + 
  geom_point(size = 1, color = "blue") +
  theme_minimal() + 
  labs(x = "Cement", y = "Concrete Compressive Strength ")

p2 <- ggplot(data, aes(x = blast_furnace_slag, y = concrete_compressive_strength)) + 
  geom_point(size = 1, color = "green") +
  theme_minimal() +
  labs(x = "Blast Furnace Slag", y = "Concrete Compressive Strength ")

p3 <- ggplot(data, aes(x = fly_ash, y = concrete_compressive_strength)) + 
  geom_point(size = 1, color = "red") +
  theme_minimal() + 
  labs(x = "Fly Ash", y = "Concrete Compressive Strength ")

p4 <- ggplot(data, aes(x = water, y = concrete_compressive_strength)) + 
  geom_point(size = 1, color = "purple") +
  theme_minimal() + 
  labs(x = "Water", y = "Concrete Compressive Strength ")

p5 <- ggplot(data, aes(x = superplasticizer, y = concrete_compressive_strength)) + 
  geom_point(size = 1, color = "orange") +
  theme_minimal() + 
  labs(x = "Superplasticizer", y = "Concrete Compressive Strength ")

p6 <- ggplot(data, aes(x = coarse_aggregate, y = concrete_compressive_strength)) + 
  geom_point(size = 1, color = "yellow") +
  theme_minimal() +
  labs(x = "Coarse Aggregate", y = "Concrete Compressive Strength ")

p7 <- ggplot(data, aes(x = fine_aggregate, y = concrete_compressive_strength)) + 
  geom_point(size = 1, color = "lightblue") +
  theme_minimal() + 
  labs(x = "Fine Aggregate", y = "Concrete Compressive Strength ")

p8 <- ggplot(data, aes(x = age, y = concrete_compressive_strength)) + 
  geom_point(size = 1, color = "black") +
  theme_minimal() + 
  labs(x = "Age", y = "Concrete Compressive Strength ")

figure <- ggarrange(p1, p2, p3, p4, p5,p6, p7, p8,
                    labels = c("A", "B", "C","D","E","F","G","H"),
                    ncol = 3, nrow = 3)

#ggsave("plots.png", figure, width = 12, height = 12, units = "in", dpi = 300)
figure
```

![](compressive_strength_project_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
#Find the correlation between the response variable vs each explanatory variable 
cor_cement <- cor(data$concrete_compressive_strength, data$cement)
cor_bfs <- cor(data$concrete_compressive_strength, data$blast_furnace_slag)
cor_fa <- cor(data$concrete_compressive_strength, data$fly_ash)
cor_water <- cor(data$concrete_compressive_strength, data$water)
cor_sp <- cor(data$concrete_compressive_strength, data$superplasticizer)
cor_ca <- cor(data$concrete_compressive_strength, data$coarse_aggregate)
cor_fa <- cor(data$concrete_compressive_strength, data$fine_aggregate)
cor_age <- cor(data$concrete_compressive_strength, data$age)

print(paste("Correlation between drinks and cement:", cor_cement))
```

    ## [1] "Correlation between drinks and cement: 0.497832722274844"

``` r
print(paste("Correlation between drinks and bfs:", cor_bfs))
```

    ## [1] "Correlation between drinks and bfs: 0.134824445143343"

``` r
print(paste("Correlation between drinks and fa:", cor_fa))
```

    ## [1] "Correlation between drinks and fa: -0.167248961981689"

``` r
print(paste("Correlation between drinks and water:", cor_water))
```

    ## [1] "Correlation between drinks and water: -0.289613475690416"

``` r
print(paste("Correlation between drinks and sp:", cor_sp))
```

    ## [1] "Correlation between drinks and sp: 0.366102297683224"

``` r
print(paste("Correlation between drinks and ca:", cor_ca))
```

    ## [1] "Correlation between drinks and ca: -0.164927821057335"

``` r
print(paste("Correlation between drinks and fa:", cor_fa))
```

    ## [1] "Correlation between drinks and fa: -0.167248961981689"

``` r
print(paste("Correlation between drinks and age:", cor_age))
```

    ## [1] "Correlation between drinks and age: 0.328876975510285"

``` r
#Full Linear Model
model_full_linear <- lm(concrete_compressive_strength~.,data=data)
print(BIC(model_full_linear))
```

    ## [1] 7807.437

``` r
summary(model_full_linear)
```

    ## 
    ## Call:
    ## lm(formula = concrete_compressive_strength ~ ., data = data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -28.653  -6.303   0.704   6.562  34.446 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)        -23.163756  26.588421  -0.871 0.383851    
    ## cement               0.119785   0.008489  14.110  < 2e-16 ***
    ## blast_furnace_slag   0.103847   0.010136  10.245  < 2e-16 ***
    ## fly_ash              0.087943   0.012585   6.988 5.03e-12 ***
    ## water               -0.150298   0.040179  -3.741 0.000194 ***
    ## superplasticizer     0.290687   0.093460   3.110 0.001921 ** 
    ## coarse_aggregate     0.018030   0.009394   1.919 0.055227 .  
    ## fine_aggregate       0.020154   0.010703   1.883 0.059968 .  
    ## age                  0.114226   0.005427  21.046  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 10.4 on 1021 degrees of freedom
    ## Multiple R-squared:  0.6155, Adjusted R-squared:  0.6125 
    ## F-statistic: 204.3 on 8 and 1021 DF,  p-value: < 2.2e-16

``` r
#Residual plot of full linear model
residuals_data <- data.frame(
  Residuals = residuals(model_full_linear),
  Fitted = fitted(model_full_linear)
)

residual_plot_full_linear <- ggplot(residuals_data, aes(x=Fitted, y=Residuals)) +
  geom_point(colour="blue") +
  geom_hline(yintercept=0, colour="red") +
  labs(
    x="Fitted Values",
    y="Residuals"
  ) +
  theme_minimal()

# Print the plot
print(residual_plot_full_linear)
```

![](compressive_strength_project_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
#ggsave("residual_plot_full_linear.png", plot=residual_plot_full_linear, width=10, height=8, dpi=300)
```

The preview shows you a rendered HTML copy of the contents of the
editor. Consequently, unlike *Knit*, *Preview* does not run any R code
chunks. Instead, the output of the chunk when it was last run in the
editor is displayed.

``` r
#Best subset selection
library(leaps)
subset_model <- regsubsets(concrete_compressive_strength~., data = data)
subset_summary <- summary(subset_model)
subset_summary
```

    ## Subset selection object
    ## Call: regsubsets.formula(concrete_compressive_strength ~ ., data = data)
    ## 8 Variables  (and intercept)
    ##                    Forced in Forced out
    ## cement                 FALSE      FALSE
    ## blast_furnace_slag     FALSE      FALSE
    ## fly_ash                FALSE      FALSE
    ## water                  FALSE      FALSE
    ## superplasticizer       FALSE      FALSE
    ## coarse_aggregate       FALSE      FALSE
    ## fine_aggregate         FALSE      FALSE
    ## age                    FALSE      FALSE
    ## 1 subsets of each size up to 8
    ## Selection Algorithm: exhaustive
    ##          cement blast_furnace_slag fly_ash water superplasticizer coarse_aggregate fine_aggregate age
    ## 1  ( 1 ) "*"    " "                " "     " "   " "              " "              " "            " "
    ## 2  ( 1 ) "*"    " "                " "     " "   "*"              " "              " "            " "
    ## 3  ( 1 ) "*"    " "                " "     " "   "*"              " "              " "            "*"
    ## 4  ( 1 ) "*"    "*"                " "     "*"   " "              " "              " "            "*"
    ## 5  ( 1 ) "*"    "*"                "*"     "*"   " "              " "              " "            "*"
    ## 6  ( 1 ) "*"    "*"                "*"     "*"   "*"              " "              " "            "*"
    ## 7  ( 1 ) "*"    "*"                "*"     "*"   "*"              "*"              " "            "*"
    ## 8  ( 1 ) "*"    "*"                "*"     "*"   "*"              "*"              "*"            "*"

``` r
#Find adjusted R^2, Cp, and BIC of each model
BIC_1 <- BIC(lm(concrete_compressive_strength~cement, data = data))
BIC_2 <- BIC(lm(concrete_compressive_strength~cement+superplasticizer, data = data))
BIC_3 <- BIC(lm(concrete_compressive_strength~cement+superplasticizer+age, data = data))
BIC_4 <- BIC(lm(concrete_compressive_strength~cement+blast_furnace_slag+water+age, data = data))
BIC_5 <- BIC(lm(concrete_compressive_strength~cement+blast_furnace_slag+fly_ash+water+age, data = data))
BIC_6 <- BIC(lm(concrete_compressive_strength~cement+blast_furnace_slag+fly_ash+water+superplasticizer+age, data = data))
BIC_7 <- BIC(lm(concrete_compressive_strength~cement+blast_furnace_slag+fly_ash+water+superplasticizer+fine_aggregate+age, data = data))
BIC_8 <- BIC(model_full_linear)
data.frame(
  Adj.R2 = subset_summary$adjr2,
  CP = subset_summary$cp,
  BIC = c(BIC_1,BIC_2,BIC_3,BIC_4,BIC_5,BIC_6,BIC_7,BIC_8)
)
```

    ##      Adj.R2         CP      BIC
    ## 1 0.2471057 971.106802 8449.920
    ## 2 0.3498095 698.999892 8304.796
    ## 3 0.4801330 354.302127 8080.327
    ## 4 0.5560232 154.244016 7923.725
    ## 5 0.6090627  14.954539 7798.615
    ## 6 0.6117109   8.955467 7797.545
    ## 7 0.6114862  10.546145 7804.209
    ## 8 0.6124517   9.000000 7807.437

``` r
#Model with polynomial terms
model_poly <- lm(concrete_compressive_strength ~ cement + blast_furnace_slag + fly_ash + water + poly(superplasticizer,2) + coarse_aggregate + fine_aggregate + poly(age,2), data = data)
print(BIC(model_poly))
```

    ## [1] 7327.477

``` r
print(ols_mallows_cp(model_poly,model_full_linear))
```

    ## [1] -379.8752

``` r
summary(model_poly)
```

    ## 
    ## Call:
    ## lm(formula = concrete_compressive_strength ~ cement + blast_furnace_slag + 
    ##     fly_ash + water + poly(superplasticizer, 2) + coarse_aggregate + 
    ##     fine_aggregate + poly(age, 2), data = data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -26.673  -4.671  -0.014   4.867  32.871 
    ## 
    ## Coefficients:
    ##                              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                 1.971e+01  2.085e+01   0.946    0.345    
    ## cement                      1.085e-01  6.715e-03  16.164  < 2e-16 ***
    ## blast_furnace_slag          8.467e-02  8.084e-03  10.474  < 2e-16 ***
    ## fly_ash                     4.238e-02  1.055e-02   4.018 6.29e-05 ***
    ## water                      -1.730e-01  3.166e-02  -5.464 5.85e-08 ***
    ## poly(superplasticizer, 2)1  6.678e+01  1.449e+01   4.608 4.58e-06 ***
    ## poly(superplasticizer, 2)2 -8.649e+01  9.711e+00  -8.906  < 2e-16 ***
    ## coarse_aggregate            4.171e-03  7.424e-03   0.562    0.574    
    ## fine_aggregate              5.667e-03  8.455e-03   0.670    0.503    
    ## poly(age, 2)1               2.289e+02  8.680e+00  26.371  < 2e-16 ***
    ## poly(age, 2)2              -1.908e+02  8.266e+00 -23.078  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.191 on 1019 degrees of freedom
    ## Multiple R-squared:  0.7619, Adjusted R-squared:  0.7596 
    ## F-statistic: 326.1 on 10 and 1019 DF,  p-value: < 2.2e-16

``` r
anova(model_poly)
```

    ## Analysis of Variance Table
    ## 
    ## Response: concrete_compressive_strength
    ##                             Df Sum Sq Mean Sq   F value  Pr(>F)    
    ## cement                       1  71172   71172 1060.7860 < 2e-16 ***
    ## blast_furnace_slag           1  22957   22957  342.1690 < 2e-16 ***
    ## fly_ash                      1  21636   21636  322.4795 < 2e-16 ***
    ## water                        1  11459   11459  170.7920 < 2e-16 ***
    ## poly(superplasticizer, 2)    2   6001    3000   44.7181 < 2e-16 ***
    ## coarse_aggregate             1    211     211    3.1510 0.07618 .  
    ## fine_aggregate               1     13      13    0.1886 0.66420    
    ## poly(age, 2)                 2  85355   42677  636.0833 < 2e-16 ***
    ## Residuals                 1019  68369      67                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#Residual plot of polynomial model
residuals_data <- data.frame(
  Residuals = residuals(model_poly),
  Fitted = fitted(model_poly)
)


residual_plot_poly <- ggplot(residuals_data, aes(x=Fitted, y=Residuals)) +
  geom_point(colour="purple") +
  geom_hline(yintercept=0, colour="red") +
  labs(
    x="Fitted Values",
    y="Residuals"
  ) +
  theme_minimal()

# Print the plot
print(residual_plot_poly)
```

![](compressive_strength_project_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
ggsave("residual_plot_poly.png", plot=residual_plot_poly, width=10, height=8, dpi=300)
```

``` r
#Model with all the possible two-way interaction terms
model_full_interaction <- lm(concrete_compressive_strength ~ (cement+blast_furnace_slag+ fly_ash+water+superplasticizer+coarse_aggregate+fine_aggregate+age)^2, data = data)
summary(model_full_interaction)
```

    ## 
    ## Call:
    ## lm(formula = concrete_compressive_strength ~ (cement + blast_furnace_slag + 
    ##     fly_ash + water + superplasticizer + coarse_aggregate + fine_aggregate + 
    ##     age)^2, data = data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -27.0224  -5.5781   0.0675   5.9385  30.8873 
    ## 
    ## Coefficients:
    ##                                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                         -1.797e+02  1.417e+02  -1.269 0.204909    
    ## cement                               3.206e-01  1.506e-01   2.129 0.033462 *  
    ## blast_furnace_slag                   7.031e-02  2.063e-01   0.341 0.733271    
    ## fly_ash                             -1.850e-01  3.016e-01  -0.613 0.539793    
    ## water                                1.932e+00  4.640e-01   4.164 3.39e-05 ***
    ## superplasticizer                     6.923e-02  4.876e+00   0.014 0.988673    
    ## coarse_aggregate                     5.997e-02  9.894e-02   0.606 0.544573    
    ## fine_aggregate                      -1.287e-01  9.953e-02  -1.293 0.196392    
    ## age                                 -4.444e-01  4.848e-01  -0.917 0.359514    
    ## cement:blast_furnace_slag            1.318e-04  5.742e-05   2.295 0.021962 *  
    ## cement:fly_ash                       2.944e-04  8.092e-05   3.638 0.000289 ***
    ## cement:water                        -1.803e-03  3.790e-04  -4.759 2.24e-06 ***
    ## cement:superplasticizer             -4.124e-03  1.887e-03  -2.186 0.029077 *  
    ## cement:coarse_aggregate              3.016e-05  6.669e-05   0.452 0.651179    
    ## cement:fine_aggregate                1.021e-04  5.921e-05   1.724 0.085032 .  
    ## cement:age                           5.476e-04  1.788e-04   3.063 0.002248 ** 
    ## blast_furnace_slag:fly_ash           4.718e-04  1.165e-04   4.050 5.52e-05 ***
    ## blast_furnace_slag:water            -1.305e-03  5.266e-04  -2.478 0.013359 *  
    ## blast_furnace_slag:superplasticizer -2.364e-03  2.264e-03  -1.044 0.296641    
    ## blast_furnace_slag:coarse_aggregate -4.132e-05  8.869e-05  -0.466 0.641385    
    ## blast_furnace_slag:fine_aggregate    3.060e-04  7.319e-05   4.180 3.17e-05 ***
    ## blast_furnace_slag:age               8.265e-04  1.806e-04   4.576 5.35e-06 ***
    ## fly_ash:water                       -1.997e-03  6.505e-04  -3.070 0.002202 ** 
    ## fly_ash:superplasticizer            -7.403e-03  2.935e-03  -2.522 0.011810 *  
    ## fly_ash:coarse_aggregate             1.166e-04  1.255e-04   0.929 0.353373    
    ## fly_ash:fine_aggregate               4.921e-04  1.338e-04   3.679 0.000247 ***
    ## fly_ash:age                          1.714e-03  2.980e-04   5.752 1.17e-08 ***
    ## water:superplasticizer               7.807e-03  6.169e-03   1.266 0.205953    
    ## water:coarse_aggregate              -1.018e-03  2.731e-04  -3.727 0.000205 ***
    ## water:fine_aggregate                -4.764e-04  2.731e-04  -1.744 0.081423 .  
    ## water:age                           -4.595e-04  8.274e-04  -0.555 0.578784    
    ## superplasticizer:coarse_aggregate    1.385e-03  1.827e-03   0.758 0.448664    
    ## superplasticizer:fine_aggregate     -9.696e-04  2.169e-03  -0.447 0.654922    
    ## superplasticizer:age                 5.897e-03  2.327e-03   2.535 0.011410 *  
    ## coarse_aggregate:fine_aggregate      1.439e-04  6.960e-05   2.068 0.038894 *  
    ## coarse_aggregate:age                 2.342e-05  1.495e-04   0.157 0.875578    
    ## fine_aggregate:age                   5.076e-04  2.072e-04   2.450 0.014470 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.388 on 993 degrees of freedom
    ## Multiple R-squared:  0.7567, Adjusted R-squared:  0.7479 
    ## F-statistic:  85.8 on 36 and 993 DF,  p-value: < 2.2e-16

``` r
#Model with selected interaction terms 
selected_model_with_interactions <- lm(concrete_compressive_strength ~ cement+blast_furnace_slag+fly_ash+water+superplasticizer+                                                                    coarse_aggregate+fine_aggregate+age+
                                         blast_furnace_slag:fine_aggregate+
                                         blast_furnace_slag:fly_ash+
                                         water:coarse_aggregate+
                                         blast_furnace_slag:age+
                                         fly_ash:fine_aggregate+
                                         fly_ash:age
                                         ,data=data)
print(BIC(selected_model_with_interactions))
```

    ## [1] 7608.457

``` r
print(ols_mallows_cp(selected_model_with_interactions,model_full_linear))
```

    ## [1] -191.6932

``` r
summary(selected_model_with_interactions)
```

    ## 
    ## Call:
    ## lm(formula = concrete_compressive_strength ~ cement + blast_furnace_slag + 
    ##     fly_ash + water + superplasticizer + coarse_aggregate + fine_aggregate + 
    ##     age + blast_furnace_slag:fine_aggregate + blast_furnace_slag:fly_ash + 
    ##     water:coarse_aggregate + blast_furnace_slag:age + fly_ash:fine_aggregate + 
    ##     fly_ash:age, data = data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -34.769  -5.605   0.485   5.810  28.615 
    ## 
    ## Coefficients:
    ##                                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                       -2.718e+02  5.070e+01  -5.362 1.02e-07 ***
    ## cement                             1.283e-01  7.890e-03  16.268  < 2e-16 ***
    ## blast_furnace_slag                -6.029e-02  3.550e-02  -1.698  0.08973 .  
    ## fly_ash                           -1.485e-01  6.199e-02  -2.396  0.01674 *  
    ## water                              1.261e+00  2.223e-01   5.674 1.82e-08 ***
    ## superplasticizer                   4.147e-01  8.574e-02   4.837 1.52e-06 ***
    ## coarse_aggregate                   2.749e-01  4.257e-02   6.458 1.64e-10 ***
    ## fine_aggregate                     9.424e-03  1.132e-02   0.833  0.40524    
    ## age                                6.835e-02  6.517e-03  10.488  < 2e-16 ***
    ## blast_furnace_slag:fine_aggregate  2.000e-04  4.548e-05   4.398 1.21e-05 ***
    ## blast_furnace_slag:fly_ash         2.248e-04  7.413e-05   3.032  0.00249 ** 
    ## water:coarse_aggregate            -1.417e-03  2.213e-04  -6.401 2.36e-10 ***
    ## blast_furnace_slag:age             3.216e-04  5.899e-05   5.452 6.25e-08 ***
    ## fly_ash:fine_aggregate             1.972e-04  7.574e-05   2.604  0.00934 ** 
    ## fly_ash:age                        1.896e-03  1.365e-04  13.890  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 9.281 on 1015 degrees of freedom
    ## Multiple R-squared:  0.6956, Adjusted R-squared:  0.6914 
    ## F-statistic: 165.7 on 14 and 1015 DF,  p-value: < 2.2e-16

``` r
#Residual plot of interaction model
residuals_data <- data.frame(
  Residuals = residuals(selected_model_with_interactions),
  Fitted = fitted(selected_model_with_interactions)
)

library(ggplot2)
residual_plot_interaction <- ggplot(residuals_data, aes(x=Fitted, y=Residuals)) +
  geom_point(colour="green") +
  geom_hline(yintercept=0, colour="red") +
  labs(
    x="Fitted Values",
    y="Residuals"
  ) +
  theme_minimal()

# Print the plot
print(residual_plot_interaction)
```

![](compressive_strength_project_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
ggsave("residual_plot_interaction.png", plot=residual_plot_interaction, width=10, height=8, dpi=300)
```

``` r
#cross validation for each model 
library(caret)
```

    ## Loading required package: lattice

``` r
set.seed(123)
train.control <- trainControl(method = "cv", 
                              number = 5)
model_cv_full_linear <- train(concrete_compressive_strength~., 
data = data, method = "lm", trControl = train.control)
rmse_full_linear <- model_cv_full_linear$results$RMSE

model_cv_poly <- train(concrete_compressive_strength ~ cement + blast_furnace_slag + fly_ash + water + poly(superplasticizer,2) + coarse_aggregate + fine_aggregate + poly(age,2), 
data = data, method = "lm", trControl = train.control)
rmse_poly <- model_cv_poly$results$RMSE

model_cv_interaction <- train(concrete_compressive_strength ~ cement+blast_furnace_slag+fly_ash+water+superplasticizer+                                                                    coarse_aggregate+fine_aggregate+age+
                                         blast_furnace_slag:fine_aggregate+
                                         blast_furnace_slag:fly_ash+
                                         water:coarse_aggregate+
                                         blast_furnace_slag:age+
                                         fly_ash:fine_aggregate+
                                         fly_ash:age, 
data = data, method = "lm", trControl = train.control)
rmse_interaction <- model_cv_interaction$results$RMSE

#RMSE values for each model
print(rmse_full_linear)
```

    ## [1] 10.46027

``` r
print(rmse_poly)
```

    ## [1] 8.237803

``` r
print(rmse_interaction)
```

    ## [1] 9.396861
