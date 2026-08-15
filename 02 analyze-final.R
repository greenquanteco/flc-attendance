# packages
library(ordinal)
library(MASS)
library(emmeans)

# get data
dx <- read.csv("cleaned-data.csv")

# collapse classes into categories
dx$sub <- sapply(strsplit(dx$class, " "), "[", 1)
dx$lev <- as.numeric(sapply(strsplit(dx$class, " "), "[", 2))
dx$lev <- ifelse(dx$lev >= 3000, "upper", "lower")

# set up factors
# semester is ordered
dx$sem <- factor(dx$sem, levels=
                   c("Spring24", "Spring25", "Sum25", "Fall25","Spring26"),
                 ordered=TRUE)

# as are letter grades
letts <- c("F", "D", "C", "B", "A")
dx$fe.letter <- factor(dx$fe.letter, levels=letts, ordered=TRUE)
dx$co.letter <- factor(dx$co.letter, levels=letts, ordered=TRUE)

# and levels
dx$lev <- factor(dx$lev, levels=c("lower", "upper"), ordered=TRUE)

# do we need to analyze both?
cor.test(dx$finalexam, dx$coursepct, use="pairwise.complete.obs")
plot(dx$finalexam, dx$coursepct)

ftable(fe.letter~co.letter, data=dx)

# analyze final exam data
dz <- dx[which(!is.na(dx$fe.letter)),]
dc <- dx[which(!is.na(dx$co.letter)),]

nrow(dz)
nrow(dc)

table(dz$class)
#biol 1107 chem 1211 math 1190 math 2202 
#      134        66        37       100 

table(dc$class)
#biol 3300 chem 1211 math 1190 math 2202 
#      105       228        37       100 

table(dz$sub)
#biol chem math 
# 134   66  137 

table(dc$sub)
#biol chem math 
# 105  228  137 

table(dz$lev)
#lower upper 
#  337     0 

table(dc$lev)
#lower upper 
#  365   105 

# dz has no upper division, so drop 6 and 7
mod01 <- polr(fe.letter~attend, data=dz, Hess=TRUE)
mod02 <- polr(fe.letter~attend+class, data=dz, Hess=TRUE)
mod03 <- polr(fe.letter~attend*class, data=dz, Hess=TRUE)
mod04 <- polr(fe.letter~attend+sub, data=dz, Hess=TRUE)
mod05 <- polr(fe.letter~attend*sub, data=dz, Hess=TRUE)
#mod06 <- polr(fe.letter~attend+lev, data=dz, Hess=TRUE)
#mod07 <- polr(fe.letter~attend*lev, data=dz, Hess=TRUE)

aic.df <- AIC(mod01, mod02, mod03, mod04, mod05)
aic.df$delta <- aic.df$AIC - min(aic.df$AIC)
aic.df$wt <- exp(-0.5*aic.df$delta)
aic.df$wt <- aic.df$wt/sum(aic.df$wt)

# order by descending AIC weight
aic.df <- aic.df[order(-aic.df$wt),]
aic.df
#     df       AIC      delta           wt
#mod03 11  897.1192   0.000000 7.544725e-01
#mod05  9  899.3644   2.245221 2.455273e-01
#mod04  7  928.0079  30.888690 1.479959e-07
#mod02  8  929.9717  32.852546 5.543750e-08
#mod01  5 1040.8625 143.743369 4.614955e-32

confint(mod3)
#                              2.5 %      97.5 %
#attend                 5.625774e-05  0.02735832
#classchem 1211        -8.418225e+00 -1.87008353
#classmath 1190        -1.171438e+01 -4.75703864
#classmath 2202        -1.844919e+01 -8.02789478
#attend:classchem 1211 -1.544274e-02  0.05593232
#attend:classmath 1190  4.078171e-02  0.12297726
#attend:classmath 2202  7.291770e-02  0.18245609

summary(mod3)
#Coefficients:
#                          Value Std. Error t value
#attend                  0.01364   0.006953  1.9620
#classchem 1211         -4.66731   1.624699 -2.8727
#classmath 1190         -8.09014   1.763081 -4.5886
#classmath 2202        -12.84988   2.656835 -4.8365
#attend:classchem 1211   0.01563   0.017758  0.8801
#attend:classmath 1190   0.08048   0.020851  3.8597
#attend:classmath 2202   0.12386   0.027930  4.4348

#Intercepts:
#      Value    Std. Error t value 
#F|D  -1.9535   0.5780    -3.3799
#D|C  -1.0035   0.5654    -1.7749
#C|B  -0.1754   0.5554    -0.3157
#B|A   1.4595   0.5590     2.6108

emtrends(mod03, ~class, var="attend")
#    class attend.trend      SE  df asymp.LCL asymp.UCL
#biol 1107       0.0136 0.00695 Inf  1.39e-05    0.0273
#chem 1211       0.0293 0.01640 Inf -2.78e-03    0.0613
#math 1190       0.0941 0.01970 Inf  5.54e-02    0.1328
#math 2202       0.1375 0.02720 Inf  8.43e-02    0.1907 
  
emtrends(mod05, ~sub, var="attend")
#sub  attend.trend      SE  df asymp.LCL asymp.UCL
#biol       0.0136 0.00693 Inf -3.46e-05    0.0271
#chem       0.0291 0.01630 Inf -2.91e-03    0.0611
#math       0.0973 0.01450 Inf  6.89e-02    0.1257

# conclusion: use mod03 for final exam score

tr1 <- emtrends(mod03, ~ class, var = "attend")
pairs(tr1)
#contrast              estimate     SE  df z.ratio p.value
#biol 1107 - chem 1211  -0.0156 0.0178 Inf  -0.880  0.8152
#biol 1107 - math 1190  -0.0805 0.0209 Inf  -3.860  0.0007
#biol 1107 - math 2202  -0.1239 0.0279 Inf  -4.435 <0.0001
#chem 1211 - math 1190  -0.0648 0.0255 Inf  -2.541  0.0539
#chem 1211 - math 2202  -0.1082 0.0315 Inf  -3.433  0.0033
#math 1190 - math 2202  -0.0434 0.0326 Inf  -1.332  0.5423



# The association between participation and final-exam grade varied substantially across the courses in the dataset. Model selection moderately favored course-specific relationships over subject-specific relationships. Across both formulations, the clearest positive association occurred in mathematics; evidence was weaker in biology and absent or inconclusive in chemistry.

# analyze course grade

mod11 <- polr(co.letter~attend, data=dc, Hess=TRUE)
mod12 <- polr(co.letter~attend+class, data=dc, Hess=TRUE)
mod13 <- polr(co.letter~attend*class, data=dc, Hess=TRUE)
mod14 <- polr(co.letter~attend+sub, data=dc, Hess=TRUE)
mod15 <- polr(co.letter~attend*sub, data=dc, Hess=TRUE)
mod16 <- polr(co.letter~attend+lev, data=dc, Hess=TRUE)
mod17 <- polr(co.letter~attend*lev, data=dc, Hess=TRUE)

aic.df2 <- AIC(mod11, mod12, mod13, mod14, mod15, mod16, mod17)
aic.df2$delta <- aic.df2$AIC - min(aic.df2$AIC)
aic.df2$wt <- exp(-0.5*aic.df2$delta)
aic.df2$wt <- aic.df2$wt/sum(aic.df2$wt)

# order by descending AIC weight
aic.df2 <- aic.df2[order(-aic.df2$wt),]
aic.df2
#      df      AIC    delta           wt
#mod13 11 1363.619  0.00000 9.996932e-01
#mod15  9 1379.809 16.18927 3.050787e-04
#mod17  7 1390.281 26.66144 1.623335e-06
#mod12  8 1397.766 34.14647 3.846407e-08
#mod16  6 1398.359 34.73932 2.859693e-08
#mod14  7 1400.356 36.73630 1.053612e-08
#mod11  5 1401.881 38.26131 4.915054e-09

emtrends(mod13, ~class, var="attend")
#    class     attend.trend      SE  df asymp.LCL asymp.UCL
#biol 3300       0.0292 0.00678 Inf    0.0159    0.0425
#chem 1211       0.0454 0.00674 Inf    0.0322    0.0586
#math 1190       0.1153 0.02020 Inf    0.0758    0.1548
#math 2202       0.1433 0.02510 Inf    0.0941    0.1925

tr2 <- emtrends(mod13, ~ class, var = "attend")
pairs(tr2)
#contrast              estimate      SE  df z.ratio p.value
#biol 3300 - chem 1211  -0.0162 0.00932 Inf  -1.742  0.3020
#biol 3300 - math 1190  -0.0861 0.02100 Inf  -4.093  0.0002
#biol 3300 - math 2202  -0.1141 0.02570 Inf  -4.442 <0.0001
#chem 1211 - math 1190  -0.0698 0.02090 Inf  -3.335  0.0047
#chem 1211 - math 2202  -0.0979 0.02560 Inf  -3.827  0.0007
#math 1190 - math 2202  -0.0280 0.03150 Inf  -0.891  0.8094

summary(mod13)
#Call:
#  polr(formula = co.letter ~ attend * class, data = dc, Hess = TRUE)
#
#Coefficients:
#                          Value Std. Error t value
#attend                  0.02919    0.00678   4.306
#classchem 1211         -1.61830    0.74740  -2.165
#classmath 1190         -6.84087    1.73368  -3.946
#classmath 2202        -11.08943    2.42558  -4.572
#attend:classchem 1211   0.01623    0.00932   1.742
#attend:classmath 1190   0.08607    0.02103   4.093
#attend:classmath 2202   0.11411    0.02569   4.442

#Intercepts:
#       Value    Std. Error t value 
#F|D  -0.0500   0.4913    -0.1019
#D|C   1.3067   0.4928     2.6517
#C|B   2.5246   0.5046     5.0029
#B|A   3.9779   0.5195     7.6568

# all of that is cool, but let's focus on percentages and just
# label them

# percentage grades are proportions in disguise
# logit transform and store as y
dz$y <- qlogis(pmax(0.1, pmin(99.9, dz$finalexam))/100)
dc$y <- qlogis(pmax(0.1, pmin(99.9, dc$coursepct))/100)

# dz has no upper division, so drop 6 and 7
mod01 <- lm(y~attend, data=dz)
mod02 <- lm(y~attend+class, data=dz)
mod03 <- lm(y~attend*class, data=dz)
mod04 <- lm(y~attend+sub, data=dz)
mod05 <- lm(y~attend*sub, data=dz)

aic.df1 <- AIC(mod01, mod02, mod03, mod04, mod05)
aic.df1$delta <- aic.df1$AIC - min(aic.df1$AIC)
aic.df1$wt <- exp(-0.5*aic.df1$delta)
aic.df1$wt <- aic.df1$wt/sum(aic.df1$wt)

# order by descending AIC weight
aic.df1 <- aic.df1[order(-aic.df1$wt),]
aic.df1
#      df      AIC     delta           wt
#mod03  9 1479.238  0.000000 9.832330e-01
#mod05  7 1487.381  8.142944 1.676635e-02
#mod04  5 1508.784 29.546159 3.773893e-07
#mod02  6 1509.661 30.423304 2.433996e-07
#mod01  3 1557.386 78.148008 1.054465e-17

# dc has some upper division, so include 6 and 7
mod11 <- lm(y~attend, data=dc)
mod12 <- lm(y~attend+class, data=dc)
mod13 <- lm(y~attend*class, data=dc)
mod14 <- lm(y~attend+sub, data=dc)
mod15 <- lm(y~attend*sub, data=dc)
mod16 <- lm(y~attend+lev, data=dc)
mod17 <- lm(y~attend*lev, data=dc)

aic.df2 <- AIC(mod11, mod12, mod13, mod14, mod15, mod16, mod17)
aic.df2$delta <- aic.df2$AIC - min(aic.df2$AIC)
aic.df2$wt <- exp(-0.5*aic.df2$delta)
aic.df2$wt <- aic.df2$wt/sum(aic.df2$wt)

# order by descending AIC weight
aic.df2 <- aic.df2[order(-aic.df2$wt),]
aic.df2
#      df      AIC    delta           wt
#mod13  9 1623.555  0.00000 9.996977e-01
#mod15  7 1639.936 16.38168 2.770971e-04
#mod17  5 1645.366 21.81107 1.835079e-05
#mod12  6 1647.811 24.25580 5.404916e-06
#mod16  4 1652.144 28.58928 6.191366e-07
#mod11  3 1652.385 28.83045 5.488014e-07
#mod14  5 1654.108 30.55363 2.318634e-07

emtrends(mod03, ~class, var="attend")
#class     attend.trend      SE  df lower.CL upper.CL
#biol 1107       0.0323 0.00782 329   0.0169   0.0477
#chem 1211       0.0588 0.01250 329   0.0343   0.0834
#math 1190       0.0934 0.02090 329   0.0523   0.1344
#math 2202       0.1536 0.01950 329   0.1154   0.1919

emtrends(mod13, ~class, var="attend")
#class     attend.trend      SE  df lower.CL upper.CL
#biol 3300       0.0173 0.00509 462   0.0073   0.0273
#chem 1211       0.0306 0.00427 462   0.0222   0.0390
#math 1190       0.0813 0.01310 462   0.0555   0.1072
#math 2202       0.0673 0.01220 462   0.0432   0.0914

tr1 <- emtrends(mod03, ~ class, var = "attend")
pairs(tr1)

tr2 <- emtrends(mod13, ~ class, var = "attend")
#pairs(tr2)
#contrast              estimate      SE  df t.ratio p.value
#biol 3300 - chem 1211  -0.0133 0.00664 462  -2.005  0.1874
#biol 3300 - math 1190  -0.0640 0.01410 462  -4.546 <0.0001
#biol 3300 - math 2202  -0.0500 0.01330 462  -3.770  0.0011
#chem 1211 - math 1190  -0.0507 0.01380 462  -3.672  0.0015
#chem 1211 - math 2202  -0.0367 0.01300 462  -2.828  0.0251
#math 1190 - math 2202   0.0140 0.01800 462   0.782  0.8628

# make some figures
ag <- aggregate(attend~class, data=dx, range, na.rm=TRUE)
#      class  attend.1  attend.2
#1 biol 1107   4.00000 100.00000
#2 biol 3300   0.00000 100.00000
#3 chem 1211   7.00000 100.00000
#4 chem 3361   0.00000 100.00000
#5 math 1190  46.36364 100.00000
#6 math 2202  50.00000 100.00000

n <- 200
pxmat <- matrix(NA, nrow=n, ncol=6)
colnames(pxmat) <- ag$class
for(i in 1:nrow(ag)){
  pxmat[,i] <- seq(ag$attend[i,1], ag$attend[i,2], length=n)
}

# final exam
px1 <- pxmat[,"biol 1107"]
px2 <- pxmat[,"chem 1211"]
px3 <- pxmat[,"math 1190"]
px4 <- pxmat[,"math 2202"]

prx <- data.frame(attend=c(px1, px2, px3, px4),
                  class=rep(sort(unique(dz$class)),each=n))
pred <- predict(mod03, newdata=prx, interval="confidence", se.fit=TRUE)
prx$lo <- 100*plogis(pred$fit[,2])
prx$md <- 100*plogis(pred$fit[,1])
prx$up <- 100*plogis(pred$fit[,3])

# symbols

fig.name <- "finalexam.jpg"
jpeg(fig.name, width=1.5*7.5, height=1.5*5, units="in", res=500)
par(mfrow=c(2,2), mar=c(4.1, 4.1, 1.1, 1.1), 
    lend=1, las=1, xpd=NA, bty="n",
    cex.axis=1.5, cex.lab=1.5, cex.main=1.5,
    oma=c(0, 0.2, 0, 0))
plot(dz$attend, dz$finalexam, type="n",
     xlab="",
     ylab="Final exam grade (%)",
     xlim=c(0, 100), ylim=c(0, 100),
     main="BIOL 1107: Principles of Biol. I")
title(xlab="Participation or attendance (%)", line=2.4)
flag1 <- which(prx$class == "biol 1107")
flag2 <- which(dz$class == "biol 1107")
polygon(x=c(prx$attend[flag1], rev(prx$attend[flag1])),
        y=c(prx$lo[flag1], rev(prx$up[flag1])),
        border=NA, col="grey80")
points(prx$attend[flag1], prx$md[flag1], type="l", lwd=3)
points(dz$attend[flag2], dz$finalexam[flag2], cex=1.2)

plot(dz$attend, dz$finalexam, type="n",
     xlab="",
     ylab="Final exam grade (%)",
     xlim=c(0, 100), ylim=c(0, 100),
     main="CHEM 1211: Principles of Chem. I")
title(xlab="Participation or attendance (%)", line=2.4)
flag1 <- which(prx$class == "chem 1211")
flag2 <- which(dz$class == "chem 1211")
polygon(x=c(prx$attend[flag1], rev(prx$attend[flag1])),
        y=c(prx$lo[flag1], rev(prx$up[flag1])),
        border=NA, col="grey80")
points(prx$attend[flag1], prx$md[flag1], type="l", lwd=3)
points(dz$attend[flag2], dz$finalexam[flag2], cex=1.2)


plot(dz$attend, dz$finalexam, type="n",
     xlab="",
     ylab="Final exam grade (%)",
     xlim=c(0, 100), ylim=c(0, 100),
     main="MATH 1190: Calculus I")
title(xlab="Participation or attendance (%)", line=2.4)
flag1 <- which(prx$class == "math 1190")
flag2 <- which(dz$class == "math 1190")
polygon(x=c(prx$attend[flag1], rev(prx$attend[flag1])),
        y=c(prx$lo[flag1], rev(prx$up[flag1])),
        border=NA, col="grey80")
points(prx$attend[flag1], prx$md[flag1], type="l", lwd=3)
points(dz$attend[flag2], dz$finalexam[flag2], cex=1.2)


plot(dz$attend, dz$finalexam, type="n",
     xlab="",
     ylab="Final exam grade (%)",
     xlim=c(0, 100), ylim=c(0, 100),
     main="Math 2202: Calculus II")
title(xlab="Participation or attendance (%)", line=2.4)
flag1 <- which(prx$class == "math 2202")
flag2 <- which(dz$class == "math 2202")
polygon(x=c(prx$attend[flag1], rev(prx$attend[flag1])),
        y=c(prx$lo[flag1], rev(prx$up[flag1])),
        border=NA, col="grey80")
points(prx$attend[flag1], prx$md[flag1], type="l", lwd=3)
points(dz$attend[flag2], dz$finalexam[flag2], cex=1.2)
dev.off()


############################################################
px1 <- pxmat[,"biol 3300"]

prx <- data.frame(attend=c(px1, px2, px3, px4),
                  class=rep(sort(unique(dc$class)),each=n))
pred <- predict(mod13, newdata=prx, interval="confidence", se.fit=TRUE)
prx$lo <- 100*plogis(pred$fit[,2])
prx$md <- 100*plogis(pred$fit[,1])
prx$up <- 100*plogis(pred$fit[,3])


fig.name <- "coursegrade.jpg"
jpeg(fig.name, width=1.5*7.5, height=1.5*5, units="in", res=500)
par(mfrow=c(2,2), mar=c(4.1, 4.1, 1.1, 1.1), 
    lend=1, las=1, xpd=NA, bty="n",
    cex.axis=1.5, cex.lab=1.5, cex.main=1.5,
    oma=c(0, 0.2, 0, 0))
plot(dc$attend, dc$coursepct, type="n",
     xlab="",
     ylab="Course grade (%)",
     xlim=c(0, 100), ylim=c(0, 100),
     main="BIOL 3300: Genetics")
title(xlab="Participation or attendance (%)", line=2.4)
flag1 <- which(prx$class == "biol 3300")
flag2 <- which(dc$class == "biol 3300")
polygon(x=c(prx$attend[flag1], rev(prx$attend[flag1])),
        y=c(prx$lo[flag1], rev(prx$up[flag1])),
        border=NA, col="grey80")
points(prx$attend[flag1], prx$md[flag1], type="l", lwd=3)
points(dc$attend[flag2], dc$coursepct[flag2], cex=1.2)

plot(dc$attend, dc$coursepct, type="n",
     xlab="",
     ylab="Course grade (%)",
     xlim=c(0, 100), ylim=c(0, 100),
     main="CHEM 1211: Principles of Chem. I")
title(xlab="Participation or attendance (%)", line=2.4)
flag1 <- which(prx$class == "chem 1211")
flag2 <- which(dc$class == "chem 1211")
polygon(x=c(prx$attend[flag1], rev(prx$attend[flag1])),
        y=c(prx$lo[flag1], rev(prx$up[flag1])),
        border=NA, col="grey80")
points(prx$attend[flag1], prx$md[flag1], type="l", lwd=3)
points(dc$attend[flag2], dc$coursepct[flag2], cex=1.2)


plot(dc$attend, dc$coursepct, type="n",
     xlab="",
     ylab="Course grade (%)",
     xlim=c(0, 100), ylim=c(0, 100),
     main="MATH 1190: Calculus I")
title(xlab="Participation or attendance (%)", line=2.4)
flag1 <- which(prx$class == "math 1190")
flag2 <- which(dc$class == "math 1190")
polygon(x=c(prx$attend[flag1], rev(prx$attend[flag1])),
        y=c(prx$lo[flag1], rev(prx$up[flag1])),
        border=NA, col="grey80")
points(prx$attend[flag1], prx$md[flag1], type="l", lwd=3)
points(dc$attend[flag2], dc$coursepct[flag2], cex=1.2)


plot(dc$attend, dc$coursepct, type="n",
     xlab="",
     ylab="Course grade (%)",
     xlim=c(0, 100), ylim=c(0, 100),
     main="Math 2202: Calculus II")
title(xlab="Participation or attendance (%)", line=2.4)
flag1 <- which(prx$class == "math 2202")
flag2 <- which(dc$class == "math 2202")
polygon(x=c(prx$attend[flag1], rev(prx$attend[flag1])),
        y=c(prx$lo[flag1], rev(prx$up[flag1])),
        border=NA, col="grey80")
points(prx$attend[flag1], prx$md[flag1], type="l", lwd=3)
points(dc$attend[flag2], dc$coursepct[flag2], cex=1.2)
dev.off()


# make some figures

# what about C or better in the course
dc$cc <- ifelse(dc$co.letter %in% c("A", "B", "C"), 1, 0)

mod21 <- glm(cc~attend, data=dc, family=binomial)
mod22 <- glm(cc~attend+class, data=dc, family=binomial)
mod23 <- glm(cc~attend*class, data=dc, family=binomial)
mod24 <- glm(cc~attend+sub, data=dc, family=binomial)
mod25 <- glm(cc~attend*sub, data=dc, family=binomial)
mod26 <- glm(cc~attend+lev, data=dc, family=binomial)
mod27 <- glm(cc~attend*lev, data=dc, family=binomial)

aic.df3 <- AIC(mod21, mod22, mod23, mod24, mod25, mod26, mod27)
aic.df3$delta <- aic.df3$AIC - min(aic.df3$AIC)
aic.df3$wt <- exp(-0.5*aic.df3$delta)
aic.df3$wt <- aic.df3$wt/sum(aic.df3$wt)

# order by descending AIC weight
aic.df3 <- aic.df3[order(-aic.df3$wt),]
aic.df3

tr3 <- emtrends(mod23, ~ class, var = "attend")
tr3
#class     attend.trend      SE  df asymp.LCL asymp.UCL
#biol 3300       0.0350 0.00919 Inf    0.0170    0.0530
#chem 1211       0.0356 0.00760 Inf    0.0207    0.0505
#math 1190       0.1226 0.03910 Inf    0.0460    0.1992
#math 2202       0.1060 0.02770 Inf    0.0517    0.1602

pairs(tr3)
#contrast              estimate     SE  df z.ratio p.value
#biol 3300 - chem 1211 -0.00061 0.0119 Inf  -0.051  1.0000
#biol 3300 - math 1190 -0.08757 0.0402 Inf  -2.181  0.1286
#biol 3300 - math 2202 -0.07093 0.0292 Inf  -2.431  0.0713
#chem 1211 - math 1190 -0.08696 0.0398 Inf  -2.184  0.1277
#chem 1211 - math 2202 -0.07032 0.0287 Inf  -2.449  0.0682
#math 1190 - math 2202  0.01664 0.0479 Inf   0.347  0.9856

tr4 <- emtrends(mod25, ~ sub, var = "attend")
tr4
#sub  attend.trend      SE  df asymp.LCL asymp.UCL
#biol       0.0350 0.00919 Inf    0.0170    0.0530
#chem       0.0356 0.00760 Inf    0.0207    0.0505
#math       0.0830 0.01670 Inf    0.0503    0.1157

# multipliers for different amounts of 
# additional participations
parts <- c(2.4, 3.6, 10)
names(parts) <- c("3x50", "2x75", "10%")

exp(parts*0.035)
exp(parts*0.0356)
exp(parts*0.083)



# for biology, every additional participation
# results in:
## 10%
exp(10*0.035)

## day in 3 x 50 minute class
exp(2.4*0.035)

## day in 2 x 75 minute class
exp(3.6*0.035)



pairs(tr4)
#contrast    estimate     SE  df z.ratio p.value
#biol - chem -0.00061 0.0119 Inf  -0.051  0.9986
#biol - math -0.04801 0.0191 Inf  -2.520  0.0315
#chem - math -0.04740 0.0183 Inf  -2.585  0.0263


# what about D or worse in the course
dc$ff <- ifelse(dc$co.letter %in% c("D", "F"), 1, 0)
dc$absent <- 100 - dc$attend

mod31 <- glm(ff~absent, data=dc, family=binomial)
mod32 <- glm(ff~absent+class, data=dc, family=binomial)
mod33 <- glm(ff~absent*class, data=dc, family=binomial)
mod34 <- glm(ff~absent+sub, data=dc, family=binomial)
mod35 <- glm(ff~absent*sub, data=dc, family=binomial)
mod36 <- glm(ff~absent+lev, data=dc, family=binomial)
mod37 <- glm(ff~absent*lev, data=dc, family=binomial)

aic.df4 <- AIC(mod31, mod32, mod33, mod34, mod35, mod36, mod37)
aic.df4$delta <- aic.df4$AIC - min(aic.df4$AIC)
aic.df4$wt <- exp(-0.5*aic.df4$delta)
aic.df4$wt <- aic.df4$wt/sum(aic.df4$wt)

# order by descending AIC weight
aic.df4 <- aic.df4[order(-aic.df4$wt),]
aic.df4

tr4 <- emtrends(mod33, ~ class, var = "absent")
tr4
pairs(tr4)

tr5 <- emtrends(mod35, ~ sub, var = "absent")
tr5
pairs(tr5)


# make a figure out of model 35
pchs <- c(0, 1, 2)
names(pchs) <- c("biol", "chem", "math")

cols <- c("green4", "blue3", "red4")
cols2 <- adjustcolor(cols, alpha.f=0.3)
names(cols) <- names(pchs)

dc$pch <- pchs[dc$sub]
dc$color <- cols[dc$sub]

ag <- aggregate(absent~sub, data=dc, range, na.rm=TRUE)
n <- 200
pxmat <- matrix(NA, nrow=n, ncol=3)
colnames(pxmat) <- ag$sub
for(i in 1:nrow(ag)){
  pxmat[,i] <- seq(ag$absent[i,1], ag$absent[i,2], length=n)
}

px1 <- pxmat[,"biol"]
px2 <- pxmat[,"chem"]
px3 <- pxmat[,"math"]

prx <- data.frame(absent=c(px1, px2, px3),
                  sub=rep(sort(unique(dc$sub)),each=n))
pred <- predict(mod35, newdata=prx, type="link", se.fit=TRUE)
prx$lo <- plogis(qnorm(0.025, pred$fit, pred$se.fit))
prx$md <- plogis(qnorm(0.5, pred$fit, pred$se.fit))
prx$up <- plogis(qnorm(0.975, pred$fit, pred$se.fit))



fig.name <- "d-or-f.jpg"
jpeg(fig.name, width=1.5*7.5, height=1.5*5, units="in", res=500)
par(mfrow=c(1,1), mar=c(4.1, 4.1, 1.1, 1.1), 
    lend=1, las=1, xpd=NA, bty="n",
    cex.axis=1.5, cex.lab=1.5, cex.main=1.5,
    oma=c(0, 0.2, 0, 0))
plot(dc$absent, jitter(dc$ff, amount=0.02), #type="n",
     pch=dc$pch, col=dc$color,
     xlab="", cex=1.4,
     ylab="Probability of D or F (%)",
     xlim=c(0, 100), ylim=c(0, 1),
     yaxt="n")
title(xlab="Participation or attendance (%)", line=2.4)
flag1 <- which(prx$sub == "biol")
flag2 <- which(prx$sub == "chem")
flag3 <- which(prx$sub == "math")
polygon(x=c(prx$absent[flag1], rev(prx$absent[flag1])),
        y=c(prx$lo[flag1], rev(prx$up[flag1])),
        border=NA, col=cols2[1])
polygon(x=c(prx$absent[flag2], rev(prx$absent[flag2])),
        y=c(prx$lo[flag2], rev(prx$up[flag2])),
        border=NA, col=cols2[2])
polygon(x=c(prx$absent[flag3], rev(prx$absent[flag3])),
        y=c(prx$lo[flag3], rev(prx$up[flag3])),
        border=NA, col=cols2[3])
points(prx$absent[flag1], prx$md[flag1], type="l", lwd=3, col=cols[1], lty=1)
points(prx$absent[flag2], prx$md[flag2], type="l", lwd=3, col=cols[2], lty=2)
points(prx$absent[flag3], prx$md[flag3], type="l", lwd=3, col=cols[3], lty=3)
legend(78, 0.4, legend=c("Biology", "Chemistry", "Math"),
       col=cols, pch=pchs, cex=1.8, lwd=3, bty="n",
       lty=1:3)
axis(side=2, at=0:5*0.2, labels=0:5*20)
dev.off()


