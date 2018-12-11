Defense Wins…Games? Principal Component Analysis on NFL Offensive/Defensive Stats Through Week 13
================
Caitlin Pinkard

It's a seemingly ubiquitous truth in the NFL, and football as a whole, that "defense wins championships." However, from working in professional football analytics on and off for 3 years, I know that challenging these traditional cliches is one way to get ahead in a world of status quo. I've gathered offensive and defensive statistics for all NFL teams for the 2018 season through week 13. While this leads to a fairly small sample size (n = 32), league regulations and club rosters change so quickly that including more than one season may skew results. Expanding the sample size could also be an area for future work on the topic.

I hypothesize that performing principal components analysis will streamline the metrics I've gathered into something like "offensive talent" and "defensive talent." However, even if this is not the case, we should still be able to derive a decent model for predicting the number of wins a team has accrued through week 13 from the metrics collected.

``` r
setwd("~/Desktop/Data Mining/Blog2")

stats <- read.csv("stats.csv")
stats$winningSeas <- ifelse(stats$Wins>7, 1, 0)

X <- stats[,c(2:18)] 
n <- nrow(X)
p <- ncol(X)
e <- eigen(cov(X))
Y <- (scale(X,center=T,scale=F)) %*% e$vectors 

par(mfrow=c(2,2))

plot(Y[,1], Y[,2], xlab = "PC1", ylab = "PC2", main = "First vs. Second PC", 
     col = c("red","green")[stats$winningSeas+1], pch = 19)

plot(Y[,2], Y[,3], xlab = "PC2", ylab = "PC3", main = "Second vs. Third PC", 
     col = c("red","green")[stats$winningSeas+1], pch = 19)

plot(Y[,3], Y[,4], xlab = "PC3", ylab = "PC4", main = "Third vs. Fourth PC", 
     col = c("red","green")[stats$winningSeas+1], pch = 19)

plot(Y[,4], Y[,5], xlab = "PC4", ylab = "PC5", main = "Fourth vs. Fifth PC", 
     col = c("red","green")[stats$winningSeas+1], pch = 19)
```

![](ComponentsOfFootball_files/figure-markdown_github/data%20prep-1.png)

``` r
par(mfrow=c(1,1))

plot(Y[,5], Y[,6], xlab = "PC5", ylab = "PC6", main = "Fifth vs. Six PC", 
     col = c("red","green")[stats$winningSeas+1], pch = 19)
```

![](ComponentsOfFootball_files/figure-markdown_github/data%20prep-2.png)

### Principal Component Creation

In order to visualize the principal components, I first had to generate a binary variable. I chose to designate winning teams as those that have won more than 7 games so far this season.

Using matrix algebra concepts like eigenvalues, eigenvectors, and covariance matrices, I generated 6 principal components for this data.

Scatterplots of a few of these components are shown above. Red points represent teams with 7 wins or fewer, while green points represent teams with 8 wins or greater. Unfortunately, these plots do not paint a clear picture of the divide between winning teams and losing teams, and what may set them apart.

Next, we will look to see how many of these principal compenents we should use in a predictive model that aims to pinpoint win totals through week 13.

``` r
par(mfrow=c(1,1))
acc.sums <- rep(NA,p)
for(i in 1:p){
    acc.sums[i] <- sum(e$values[1:i])
}
plot(acc.sums/sum(e$values),type="o",xlab="Index",ylab="Cumulative Variation Explained")
```

![](ComponentsOfFootball_files/figure-markdown_github/evaluating%20the%20principal%20components-1.png)

### Principal Component Selection

According to the Cumulative Variation plot above, we should include the first 3 principal components in our predictive model. These 3 components will help explain about 93% of the variation in the original data. If we were to use fewer components, there is explainable variation left on the table. On the other hand, if we were to include more principal components, our predictive model may perform slightly better, but will be much more complex and harder to interpret. 3 finds that perfect balance.

``` r
toModel <- data.frame(cbind(Y[,1:3],stats$Wins))
names(toModel) <- c("PC1","PC2","PC3","Wins")

RegMod <- lm(Wins~.-TEAM-winningSeas,data= stats)
summary(RegMod)
```

    ## 
    ## Call:
    ## lm(formula = Wins ~ . - TEAM - winningSeas, data = stats)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.37954 -0.63563  0.07516  0.59561  1.19527 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)        1.152811  12.163395   0.095   0.9258  
    ## AvgYardsGained    -4.756840   6.206751  -0.766   0.4562  
    ## AvgPts             0.455731   0.231113   1.972   0.0687 .
    ## AvgRushYrdGained   4.761953   6.210286   0.767   0.4559  
    ## AvgPassYrdsGained  4.746053   6.207341   0.765   0.4572  
    ## FirstDowns        -0.005277   0.022769  -0.232   0.8201  
    ## TOP                0.036281   0.374696   0.097   0.9242  
    ## ScoreEff           0.001851   0.135088   0.014   0.9893  
    ## TO                 0.024463   0.104949   0.233   0.8191  
    ## AvgYrdsAllowed     0.586080   5.899733   0.099   0.9223  
    ## AvgPtsAllowed     -0.314102   0.191646  -1.639   0.1235  
    ## AvgRushAllowed    -0.570816   5.902347  -0.097   0.9243  
    ## AvgPassAllowed    -0.579362   5.905527  -0.098   0.9232  
    ## SACK               0.016788   0.064268   0.261   0.7977  
    ## INT                0.051090   0.101622   0.503   0.6230  
    ## FF                -0.021330   0.084154  -0.253   0.8036  
    ## S                 -0.369667   0.855446  -0.432   0.6722  
    ## TDS               -0.156195   0.221953  -0.704   0.4931  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.15 on 14 degrees of freedom
    ## Multiple R-squared:  0.8934, Adjusted R-squared:  0.7641 
    ## F-statistic: 6.905 on 17 and 14 DF,  p-value: 0.0003559

``` r
PCAMod <- lm(Wins~.,data = toModel)
summary(PCAMod)
```

    ## 
    ## Call:
    ## lm(formula = Wins ~ ., data = toModel)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.3789 -1.0973  0.1162  1.2681  2.7561 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  6.437500   0.302334  21.293  < 2e-16 ***
    ## PC1          0.017386   0.004553   3.818 0.000683 ***
    ## PC2         -0.014379   0.007458  -1.928 0.064063 .  
    ## PC3         -0.049219   0.013573  -3.626 0.001134 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.71 on 28 degrees of freedom
    ## Multiple R-squared:  0.529,  Adjusted R-squared:  0.4785 
    ## F-statistic: 10.48 on 3 and 28 DF,  p-value: 8.582e-05

### Model Testing

I created a dataset that only contains the first 3 principal components and the number of wins each team has earned through week 13. The goal is to use just these principal components to accurately pinpoint these win totals. By using principal components instead of all the individuals variables in the dataset, we are cutting down on both computation time (not a huge issue here with a pretty limited number of features) and complexity, essentially boiling team talent down to 3 parameters.

Please note that, because the goal of this analysis is not to show the prowess of one modeling algorithm over another, I am using a simple linear regression model to show the differences in the two data approaches.

The simple linear regression model is able to explain about 89% of the wins through week 13. While the linear regression using the principal components is only able to explain about 53% of the variation in wins through week 13, it does so using only (technically) 3 predictors, which is about 1/7 the complexity of the regular linear regression.

These findings lend credence to the idea that principal compenents analysis performs extensive dimension reduction while not sacrificing too much information.

``` r
par(mfrow=c(3,1))

barplot(e$vectors[,1],names.arg=paste("X",1:17,sep=""),
    main="Coefficients for PC1",xlab="",ylab="Eigenvector 1")

barplot(e$vectors[,2],names.arg=paste("X",1:17,sep=""),
    main="Coefficients for PC2",xlab="",ylab="Eigenvector 1")

barplot(e$vectors[,3],names.arg=paste("X",1:17,sep=""),
    main="Coefficients for PC3",xlab="",ylab="Eigenvector 1")
```

![](ComponentsOfFootball_files/figure-markdown_github/interpretting%20the%20components-1.png)

\*I initially attempted to produce these coefficient plots with the correct variable name labels on the x-axis. However, the variable names are simply too long for them all to be displayed.

### Principal Component Interpretation

Interpretting PC1: We see that the most influential predictors to PC1 values are X1, X4, and X5, which are AvgYardsGained, AvgPassYrdsGained, and FirstDowns, respectively. All three of these variables measure offensive efficiency. Therefore, this component could essentially be labeled "Offensive Talent."

Interpretting PC2: We see that the most influential predictors to PC2 values are X9, X11, and X12, which are AvgYrdsAllowed, AvgRushAllowed, and AvgPassAllowed, respectively. All three of thsee variables measure defensive efficiency. Therefore, this component could essentially be labeled "Defensive Talent." We should be careful, though: while we use the label "defensive TALENT," positive values of this PC actually indicate more yards allowed, which is a negative characteristic. We will go into this more below.

Interpretting PC3: We see that the most influential predictors to PC3 values are X3, X4, and X12, which are AvgRushYrdsGained, AvgPassYrdsGained, and AvgPassAllowed, respectively. This component is an interesting mix of offensive and defensive metrics.

``` r
PCAMod$coefficients
```

    ## (Intercept)         PC1         PC2         PC3 
    ##  6.43750000  0.01738552 -0.01437852 -0.04921914

I've output the coefficients for the principal components again here so that we can interpret their size and magnitude.

We see that increases in offensive talent correspond to more wins through week 13 (as indicated by the positive PC1 coefficient). On the other hand, the PC2 coefficient is negative. Here, we have to remember that higher values of "Defensive Talent" actually indicate more yards allowed. Therefore, a negative coefficient for PC2 tells us that poorer defensive performance is associated with fewer wins as a whole.

### Takeaways

Both of these interpretations make sense - better offense = more wins, better defense = more wins. But, the absolute value of the PC1 coefficient (0.017) is slightly higher than that of PC2 (0.014), indicating that an offense's performance MAY have a slightly higher impact on a team's win totals.

However, this difference is so slight that it likely isn't significant. What we have shown, though, is that the principal components of a team's ability to win games in the NFL can ultimately be broken down into offensive and defense skill.
