###### R packages ###################
library(shiny)
library(ggplot2)
library(dplyr)
library(xtable)
library(pROC)
library(epiR)
library(shinythemes)
library(knitr)
library(rmarkdown)
library(readr)
library(dplyr)
library(epiR)
############  epi.test DIAGNOSTIC TEST ACCURACY####################
epi.tests<-function (dat, conf.level = 0.95,decimal=3) 
{
  elements <- list()
  elements <- within(elements, {
    N. <- 1 - ((1 - conf.level)/2)
    z <- qnorm(N., mean = 0, sd = 1)
    .funincrisk <- function(cdat, conf.level) {
      N. <- 1 - ((1 - conf.level)/2)
      a <- cdat[, 1]
      n <- cdat[, 2]
      b <- n - a
      p <- a/n
      a. <- ifelse(a == 0, a + 1, a)
      b. <- ifelse(b == 0, b + 1, b)
      low <- a./(a. + (b. + 1) * (1/qf(1 - N., 2 * a., 
                                       2 * b. + 2)))
      up <- (a. + 1)/(a. + 1 + b./(1/qf(1 - N., 2 * b., 
                                        2 * a. + 2)))
      low <- ifelse(a == 0, 0, low)
      up <- ifelse(a == n, 1, up)
      rval <- data.frame(est = p, lower = low, upper = up)
      rval
    }
    a <- dat[1]
    b <- dat[3]
    c <- dat[2]
    d <- dat[4]
    M1 <- a + c
    M0 <- b + d
    N1 <- a + b
    N0 <- c + d
    total <- a + b + c + d
    tdat <- as.matrix(cbind(M1, total))
    trval <- .funincrisk(tdat, conf.level)
    tp <- trval$est
    tp.low <- trval$lower
    tp.up <- trval$upper
    tprev <- data.frame(est = tp, lower = tp.low, upper = tp.up)
    tdat <- as.matrix(cbind(N1, total))
    trval <- .funincrisk(tdat, conf.level)
    ap <- trval$est
    ap.low <- trval$lower
    ap.up <- trval$upper
    aprev <- data.frame(est = ap, lower = ap.low, upper = ap.up)
    tdat <- as.matrix(cbind(a, M1))
    trval <- .funincrisk(tdat, conf.level)
    se <- round(trval$est,3)
    se.low <-round(trval$lower,3)
    se.up <- round(trval$upper,3)
    sensitivity <- data.frame(est = se, lower = se.low, upper = se.up)
    tdat <- as.matrix(cbind(d, M0))
    trval <- .funincrisk(tdat, conf.level)
    sp <- trval$est
    sp.low <- trval$lower
    sp.up <- trval$upper
    specificity <- data.frame(est = sp, lower = sp.low, upper = sp.up)
    tdat <- as.matrix(cbind(a, N1))
    trval <- .funincrisk(tdat, conf.level)
    ppv <- trval$est
    ppv.low <- trval$lower
    ppv.up <- trval$upper
    pv.positive <- data.frame(est = ppv, lower = ppv.low, 
                              upper = ppv.up)
    tdat <- as.matrix(cbind(d, N0))
    trval <- .funincrisk(tdat, conf.level)
    npv <- trval$est
    npv.low <- trval$lower
    npv.up <- trval$upper
    pv.negative <- data.frame(est = npv, lower = npv.low, 
                              upper = npv.up)
    lrpos <- (a/M1)/(1 - (d/M0))
    lrpos.low <- exp(log(lrpos) - z * sqrt((1 - se)/(M1 *se) + (sp)/(M0 * (1 - sp))))
    lrpos.up <- exp(log(lrpos) + z * sqrt((1 - se)/(M1 *se) + (sp)/(M0 * (1 - sp))))
    lr.positive <- data.frame(est = lrpos, lower = lrpos.low,upper = lrpos.up)
    lrneg <- (1 - (a/M1))/(d/M0)
    lrneg.low <- exp(log(lrneg) - z * sqrt((se)/(M1 * (1 -se)) + (1 - sp)/(M0 * (sp))))
    lrneg.up <- exp(log(lrneg) + z * sqrt((se)/(M1 * (1 -se)) + (1 - sp)/(M0 * (sp))))
    lr.negative <- data.frame(est = lrneg, lower = lrneg.low,upper = lrneg.up)
    tdat <- as.matrix(cbind((a + d), total))
    trval <- .funincrisk(tdat, conf.level)
    da <- trval$est
    da.low <- trval$lower
    da.up <- trval$upper
    diag.acc <- data.frame(est = da, lower = da.low, upper = da.up)
    dOR.p <- (a * d)/(b * c)
    lndOR <- log(dOR.p)
    lndOR.var <- 1/a + 1/b + 1/c + 1/d
    lndOR.se <- sqrt(1/a + 1/b + 1/c + 1/d)
    lndOR.l <- lndOR - (z * lndOR.se)
    lndOR.u <- lndOR + (z * lndOR.se)
    dOR.se <- exp(lndOR.se)
    dOR.low <- exp(lndOR.l)
    dOR.up <- exp(lndOR.u)
    diag.or <- data.frame(est = dOR.p, lower = dOR.low, upper = dOR.up)
    ndx <- 1/(se - (1 - sp))
    ndx.1 <- 1/(se.low - (1 - sp.low))
    ndx.2 <- 1/(se.up - (1 - sp.up))
    ndx.low <- min(ndx.1, ndx.2)
    ndx.up <- max(ndx.1, ndx.2)
    nnd <- data.frame(est = ndx, lower = ndx.low, upper = ndx.up)
    c.p <- se - (1 - sp)
    c.1 <- se.low - (1 - sp.low)
    c.2 <- se.up - (1 - sp.up)
    c.low <- min(c.1, c.2)
    c.up <- max(c.1, c.2)
    youden <- data.frame(est = c.p, lower = c.low, upper = c.up)
    rval <- list(aprev = elements$aprev, tprev = elements$tprev, 
                 se = elements$sensitivity, sp = elements$specificity, 
                 diag.acc = elements$diag.acc, diag.or = elements$diag.or, 
                 nnd = elements$nnd, youden = elements$youden, ppv = elements$pv.positive, 
                 npv = elements$pv.negative, plr = elements$lr.positive, 
                 nlr = elements$lr.negative)
    
  })
}
#### DEMOS ##################

Elderly<-read_csv("www/Elderly.csv")
#colnames(Elderly)<-c("","Leukocytes","Bacteria","Culture")
Elderly$Culture<-as.factor(Elderly$Culture)
Elderly$Leukocytes<-as.integer(Elderly$Leukocytes)
Elderly$Bacteria<-as.integer(Elderly$Bacteria)
#dplyr::select(Elderly,Leukocytes,Bacteria,Culture)

#___________________________________________Neonates

Neonates<-data.frame (Culture_result=c("Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Positive","Positive","Positive","Positive","Positive","Positive","Positive","Positive","Positive","Positive","Positive","Positive","Positive","Positive","Positive","Positive","Positive","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Positive","Positive","Positive","Positive","Positive","Positive","Positive","Positive","Positive","Positive","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Positive","Positive","Positive","Positive","Positive","Positive","Positive","Positive","Positive","Positive","Positive","Positive","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Positive","Positive","Positive","Positive","Positive","Positive","Positive","Positive","Positive","Positive","Positive","Positive","Positive","Positive","Negative","Negative","Negative","Positive","Positive","Negative","Negative","Negative","Negative","Negative","Positive","Negative","Negative","Positive","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative","Negative"),
                      
                      Bacteria=c(1764.6,316.8,268.4,188.9,184.3,137.7,129,106.9,93.9,89.4,70.9,66.2,61.7,50.1,49.7,49.1,46.5,40.5,39.6,39.3,34.9,32.2,28.6,24.8,22.3,19.6,16,16,15.2,14.3,13.3,12.8,8.9,8,7.1,6.2,5.3,4.6,2.6,2.6,1.8,1.7,0.8,24.8,3.6,20.2,1080.5,30.4,32.2,13000.6,3073,2295.4,1840.1,1726.3,916.2,352.6,237.8,231.2,156.5,110,827.7,99999.9,616.7,92.1,88.5,118.8,66.2,35.7,30.4,30.4,29.5,29.4,26.7,22.1,20.5,16.1,5.3,0.9,32.2,19.6,7051.1,4599.8,606.6,184.3,140.4,99999.9,6011.3,481.4,434.2,32183.1,23,30.4,59,77.4,75.1,51.6,22.1,19.1,17.4,13.8,9.1,7.9,112.4,11.9,11,11.9,33.1,4.7,3.1,18.4,25.7,8.2,95.8,4.6,33.5,38.3,163.1,5.5,14.7,110.3,19.3,21.1,106.9,34,146.5,19.3,87.5,34,12.7,43.2,49.5,122.6,4330.9,135.1,8647.7,202.8,197.5,249.8,21495.6,35,139.9,1318.3,46.3,12.8,19.3,29.4,33.1,79.2,46.3,46,47.9,11.1,19.1,11.9,28.5,83.8,13.5,9.5,14.3,15.1,15812.5,18602.8,15812.5,17477.3,11942.3,50.3,30457.8,19918.5,19784.9,8950.2,8832.3,775.9,624.1,467.4,443.4,376.1,327.2,324.5,269.4,228.6,212,189.5,126.2,113.3,93.5,77.4,75.1,74.3,60.7,47.9,43.9,40.5,39.9,24.7,22,17.5,15.6,12.8,11,10.3,7.1,6.4,5.5,4.7),
                      
                      Leukocytes=c(48.6,98.8,87.8,39.5,56.9,652.1,29.3,25.5,9,286,649.2,53.3,12.3,9,16.8,10.5,31.5,5.1,43.6,11.3,1,26.5,0.9,8,15.7,16,5.8,7.4,35,4.2,4.1,0.3,6.4,2.4,1.7,6.3,17.1,1.9,0.4,3.7,1.4,2.9,2.8,63.3,3.2,49.8,18.5,10.2,12.8,28,63.3,11.5,125.3,1959.7,15.1,869.8,88.3,18.9,9.7,542.9,160.6,431.9,24,39.8,3.7,1.9,18.9,21.8,30.6,6.3,29.3,20.7,7.5,8.1,53,7.4,2.9,1.5,26.7,14.5,4214,180.8,6.7,193.9,36,10563.6,780.2,503,1889.2,28.2,4.9,12.8,28.7,1273.1,48.2,97.7,6.2,5.1,17.2,5.7,5.5,2.9,227.5,4.6,3.3,1.9,12.6,8.5,2.9,11.2,11.2,5.7,65.3,2.7,36.9,33.2,468.9,5.1,7.2,6.2,2,1.7,323.6,7.7,99.3,11.4,50.4,7.7,7.8,3.6,13.7,250.2,83.9,49.5,8054.6,78.9,738.5,20.3,85.5,56.7,2.5,227.5,19.8,0.9,5,19.4,12.7,56.9,14.8,43.7,42.7,2.4,4.3,8.6,10.6,15.7,13,7.8,8.4,5.4,578.8,578.3,578.8,3684.8,498.4,3,428,507.7,35.3,8281,288.8,22,78.1,38.4,5.9,78.2,51.3,918.2,16.9,12,54.5,37.7,68.7,217.3,80.8,11.5,69.5,29.2,42.6,29,7.8,10.9,1.8,166.9,3.6,13.3,20.2,4,15.8,5.3,10.4,3.4,1,0.9))
Neonates$Culture_result<-as.factor(Neonates$Culture_result)