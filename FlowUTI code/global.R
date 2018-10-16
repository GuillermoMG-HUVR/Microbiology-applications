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
#### DEMO ##################

Demo<-data.frame(Bacteria=c(24.7 ,   4.6 ,    244.7 ,  11.1 ,   11.9 ,   12015.8, 63153.5, 6.3 ,    2.3 ,   
                            9530.6 , 2.7,     19.9 ,   153.5,   7619.8,  8.2,     4.7,     5291.1,  7.1 ,   
                            12288.3, 23 ,     1.5 ,    32685.1, 12608.6, 4.5 ,    50.3,    0 ,      1.8,    
                            5.5,     1.5 ,    14456.9 ,10553.9, 311.6 ,  87828,   3900.7,  4.6 ,    1382 ,  
                            8888,    7279.9 , 27.9,    8.2 ,    7.1,     5.5,     765.2,   8705,    10.1,   
                            6.4,     32.2,    21660.5, 9827.1,  2220.1,  2.7,     16.5,    15.1,    185.5 , 
                            9.5 ,    5.5 ,    9691.4,  17663.5, 4.6 ,    97.5,    73.4,    4.7 ,    15.9,   
                            8898.9,  21.5 ,   12699.6, 10689.3, 153.9,   188.7,   10.3,    96447.4, 20.7,   
                            0.9 ,    60.8 ,   349.4,   91.9,    18.3,    131.1,   115.2,   3173.4,  2634.9, 
                            25.5 ,   72.8 ,   20791.5, 9.2 ,    73517.1, 6560.9,  13075.8, 6.4,     3.6 ,   
                            3.9 ,    5.5 ,    43997,   8.2 ,    14.7 ,   7493.9,  1210.4,  9241,    36.6,   
                            45157.8, 5319.9 , 14.3,    60693.2, 2.7 ,    82.2,    19.3,    331.9,   1.8,    
                            3838.8,  12076.5, 16.7,    15.6,    1.8 ,    8.2,     93.5,    6.3,    11.9,   
                            356.7,   1.8,     4857.8,  4076.4,  0 ,      172.7,   852.7 ,  103.2 ,  178.7 , 
                            7.9,     6793.1,  18.4,    22.3 ,   19.1 ,   70.9,    175.9,   3145.6,  8.2 ,   
                            22837.6, 117.5,   34 ,     135,    0.9,     71.1,    12.7 ,   2053.2,  124.7,  
                            3548.6, 2469.5,  4.7 ,    6151.1,  14.3,    12.9,    10.3,    33.5,    808.4,  
                            8.2 ,    9.5,     330,     6.3,     12.7,    28.5 ,   6.4,     85.7,    48.7,   
                            15.1 ,   51.6,    7.9,     11,      99443.2, 42215.6, 10.1,    2708.6,  6.3,    
                            24.8 ,   6.3,     541.5,   69.1,    7598.3,  4.6,     4014.3,  1468.7,  7.9,    
                            20.7 ,   2356.5,  11406.8, 14.7 ,   10728.5, 9.2,     20.2,    97.7,    0.7,    
                            443.8,   55.2,    38.3,    10.3 ,   200.8,   6.3,     15691.2, 44.2,    15.2,   
                            3169.7,  6.3 ,    259 ,    14686.8, 25155.7, 16.7,    43.9,    17.5,    2.7,    
                            7.3,     57097.8, 3.9 ,    702.5,   17.5,    2.7,     7.9,     0 ,      279.9,  
                            2412.7,  33002.4, 5171.8,  13993.9, 5431.2 , 9416.1,  5084.6,  7591.8,  19.3,   
                            839.3,   192.7,   82.3,    10829.4, 616.7,   5094.9,  13602.8, 4.6,     705.2,  
                            140.1,   175.1,   7523.8 , 21.1,    23388.9, 79.1,    10.3,    5.5,     30791.6,
                            379.7,   3715.8,  499.6,   85.5,    32.7,    295.8 ,  11  ),  
                 
                 Leukocytes=c(3.4,    5.5,    67.6,   48.4,   23 ,    271.9,  1506.2, 1.4 ,   4.3,    2929.7, 1.4,    0.7,    2873.1, 313.7,  7.8,    4.1,   
                              94.3,   16.5,  705.5,  15.2 ,  3.4,    2696,   27.4,   3.8,    3.4,    0.3,    8.6,    5.8,    0.7,    258.5,  548.4,  59.7,  
                              1183.6, 2483.7, 6.4,    2383.5, 3254.2, 149.2,  134.1,  31.6,   39.6,   1.6,    9.9,    3016.1, 1.3,    7.3,    23.4,   499.4, 
                              3084.6, 546.2,  2.5,    2.2,    1.9,    41.1,   0.5,    1.6,    1381.7, 4020.7, 0.9,    6.1,    0.9 ,   1.2,    0.9,    35.7,  
                              2.1,    43.6,   217.8,  23.9,   6.2,    0.3,    519,    556.4,  1,      2.7,    6.6,    0.6 ,   3.1,    58.1,   110.7,  588.7, 
                              94.2,   0.9,    867.9,  157.8,  9.1,    170.2,  1682.1, 0.5,    4.3,    3.2,    0.5,    15.5,   3229.9, 15.2,   10,     153.5, 
                              1.8,    374.3,  9.4,    2210,   10.5,   8.2,    627.6,  0.8,    5.2,    11.5,   37.4,   1,      3.7,    7.9,    62.8,   116.7, 
                              0.8,   0.9 ,   53.7,   3.1,    6.1 ,   963.1,  0.4,    33,     110.8,  0.3,    299.4,  30,     981.3,  11.3,   45.7,   46.3,  
                              16.2,   5.1,    3.4,    10.1,   1741.5, 372.4,  15.1,   59.7,   208.2,  0.8,    7.7,    0.3,    60.2,   2.6 ,   371.1,  36.3,  
                              6 ,     18876,  4.3,    245.5,  96.1,   3.8,    24.5,   9.9,    223.5,  0.6,    1,      88,     18.2,   7.4 ,   36.1,   2.1,   
                              1069.3, 14.9,   37.4,   380.8,  1.1,    101.4,  6493,   1672.7, 59.4,   24.1,   6.8,    7.2,    4.9,    54.9,   3.1,    18.9,  
                              2.4,    1545.5, 755.9,  0.8,    3.4,    53.8,   152.2,  3,      178.9,  11.7,   82.5,   70,     3.2,    43.1,   5.8,    25.8,  
                              5,      16 ,    3.3,    586.1,  232.8,  26.5,   446.3,  1.7,    51.6,   18.3,   58.4,   1.4,    48.6,   4.1,    1.8,    3,     
                              5143.7, 0.5,    213.9,  9,      1.5,    14.5,   1.8,    16.2,   16.8,   15.1,   53,     3.4,    14.3,   32,     209.3,  3.2,   
                              20 ,    1.5,    3.5,    2.2,    31.8,   214.5,  827.3,  707.5,  4.3 ,   10,     445.2,  93.1,   1938.7, 4.2,    2508.2, 2,
                              7.2 ,   6.2 ,   1897.7, 10,    27.3,   83.9,   7.9,    3,      186.2,  2.4 ),
                 
                 Culture_result=c("Negative", "Negative", "Negative", "Negative", "Negative", "Positive", "Positive", "Negative", "Negative", "Positive", "Negative", "Negative",
                                  "Negative", "Positive", "Negative", "Negative", "Positive", "Negative", "Positive", "Negative", "Negative", "Positive", "Positive", "Negative",
                                  "Negative", "Negative", "Negative", "Negative", "Negative", "Positive", "Positive", "Negative", "Positive", "Positive", "Negative", "Positive",
                                  "Positive", "Positive", "Negative", "Negative", "Negative", "Negative", "Positive", "Positive", "Negative", "Negative", "Negative", "Positive",
                                  "Positive", "Positive", "Negative", "Negative", "Negative", "Negative", "Negative", "Negative", "Positive", "Positive", "Negative", "Negative",
                                  "Negative", "Negative", "Negative", "Positive", "Negative", "Positive", "Positive", "Negative", "Negative", "Negative", "Positive", "Negative",
                                  "Negative", "Negative", "Positive", "Negative", "Negative", "Negative", "Negative", "Positive", "Positive", "Negative", "Negative", "Positive",
                                  "Negative", "Positive", "Positive", "Positive", "Negative", "Negative", "Negative", "Negative", "Positive", "Negative", "Negative", "Positive",
                                  "Positive", "Positive", "Negative", "Positive", "Positive", "Negative", "Positive", "Negative", "Negative", "Negative", "Negative", "Negative",
                                  "Positive", "Positive", "Negative", "Negative", "Negative", "Negative", "Negative", "Negative", "Negative", "Positive", "Negative", "Negative",
                                  "Positive", "Negative", "Negative", "Negative", "Positive", "Negative", "Negative", "Positive", "Negative", "Negative", "Negative", "Negative",
                                  "Negative", "Positive", "Negative", "Positive", "Negative", "Negative", "Negative", "Negative", "Negative", "Negative", "Negative", "Negative",
                                  "Positive", "Negative", "Negative", "Positive", "Negative", "Negative", "Negative", "Negative", "Positive", "Negative", "Negative", "Negative",
                                  "Negative", "Negative", "Negative", "Negative", "Negative", "Negative", "Negative", "Negative", "Negative", "Negative", "Positive", "Positive",
                                  "Negative", "Positive", "Negative", "Negative", "Negative", "Positive", "Negative", "Positive", "Negative", "Negative", "Positive", "Negative",
                                  "Negative", "Positive", "Positive", "Negative", "Positive", "Negative", "Negative", "Negative", "Negative", "Negative", "Negative", "Negative",
                                  "Negative", "Positive", "Negative", "Positive", "Negative", "Negative", "Positive", "Negative", "Positive", "Positive", "Positive", "Negative",
                                  "Negative", "Negative", "Negative", "Negative", "Negative", "Negative", "Positive", "Negative", "Negative", "Negative", "Negative", "Positive",
                                  "Positive", "Positive", "Positive", "Positive", "Positive", "Positive", "Positive", "Positive", "Negative", "Positive", "Negative", "Negative",
                                  "Positive", "Negative", "Positive", "Positive", "Negative", "Positive", "Negative", "Negative", "Positive", "Negative", "Positive", "Negative",
                                  "Negative", "Negative", "Positive", "Negative", "Positive", "Negative", "Negative", "Negative", "Positive", "Negative"))
                 
