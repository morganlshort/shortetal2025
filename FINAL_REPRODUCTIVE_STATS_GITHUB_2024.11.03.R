#Reproductive Statistics for Coral Reefs Publication
#Short et al., "A reproductive and trophic analysis of corals in a degraded environment"


#FERTILITY ##########################################
#analyses included hermaphrodite but do not include colony 11 or 103 due to insufficient data

#Fertility ~ Site Contingency Tables  ###############
data.xtab <-xtabs (Frequency ~ Site + Fertility, Fertility_Site_SSID_2020_2019_2024_09_22)
chisq.test(data.xtab,corr=F)$exp
# Fertility
# Site     No  Yes
# ex situ 2.0  8.0
# Port    5.6 22.4
# Reef 1  2.4  9.6
# Reef 2  3.0 12.0
#>20% of expected frequencies are <5

#Fisher.exact test
fisher.test(data.xtab)
# p-value = 0.01413


library(vcd) # install package vcd before
strucplot(data.xtab,shade=T)
chisq.test (data.xtab, corr=F)$res
# Fertility
# Site              No        Yes
# ex situ  0.7071068 -0.3535534
# Port    -1.9438548  0.9719274
# Reef 1   1.0327956 -0.5163978
# Reef 2   1.1547005 -0.5773503
#Port is "not fertile" less than expected


#Fertility ~ Year ############
#does NOT include samples collected ex situ since were not sampled in 2019
NOEXSITU <- subset(Aug2020_2019_Reproductive_Final_09_22_24, !(Site== "ex situ"))
freq_table <- table(NOEXSITU$Year, NOEXSITU$Fertility)
freq_table
 
data.xtab <-xtabs (Frequency ~ Year + Fertility, Aug2020_2019_Fertility_Frequency_Year_NOEXSITU_11_3_2024)
chisq.test(data.xtab,corr=F)$exp
# Fertility
# Year          N        Y
# 2019 4.814815 21.18519
# 2020 5.185185 22.81481
#>20% of expected frequencies are <5; assumption is not met
fisher.test(data.xtab)
# p-value = 0.2975
# alternative hypothesis: true odds ratio is not equal to 1
# 95 percent confidence interval:
#   0.05874227 2.02962892
# sample estimates:
#   odds ratio 
# 0.3980113 


#Fertility ~ Size Class Contingency table ######################
data.xtab <-xtabs (Frequency ~ Size_Class + Fertility, Aug2020_2019_Fertility_Frequency_SizeClass_9_28_24)
chisq.test(data.xtab,corr=F)$exp # expected frequencies
#>20% are less than 5
#assumption is not met
fisher.test(data.xtab)
#p-value = 0.060
#There is no association between size and fertility status
library(vcd)
strucplot(data.xtab,shade=T)


#FECUNDITY ########################################
#Fecundity ~ Site:Size GAM ######################

library('mgcv')

    #MALES ############################
#NOTE: Fecundity is NOT square rooted, like it is for the fecundity~year
SSIDm <- subset(Aug2020_2019_Reproductive_Final_09_22_24, `M/F`=='M')
#Fit 2 models to determine best smoother
gam1<-gam(Cross_Mes~s(Surface_Area)+factor(Site),data = SSIDm) #same smoother across all factors
gam2 <- gam (Cross_Mes~s(Surface_Area)+ s(Surface_Area, by=factor(Site)) +
               factor(Site), data=SSIDm) 
# Error in gam(Cross_Mes ~ s(Surface_Area) + s(Surface_Area, by = factor(Site)) +  : 
#                Model has more coefficients than data
#use gam1

summary(gam1)
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)          -576.3     1423.3  -0.405   0.6909  
# factor(Site)Port     1854.4     1612.7   1.150   0.2671  
# factor(Site)Reef 1   2782.7     1682.0   1.654   0.1175  
# factor(Site)Reef 2   3692.5     1593.4   2.317   0.0341 *
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df     F p-value
# s(Surface_Area)   1      1 0.978   0.337
# 
# R-sq.(adj) =  0.148   Deviance explained = 31.8%
# GCV = 4.0996e+06  Scale est. = 3.1235e+06  n = 21

anova(gam1)
# Parametric Terms:
#   df     F p-value
# factor(Site)  3 2.246   0.122
# 
# Approximate significance of smooth terms:
#                 edf Ref.df     F p-value
# s(Surface_Area)   1      1 0.978   0.337
gam.check(gam1)
# Smoothing parameter selection converged after 14 iterations.
# The RMS GCV score gradient at convergence was 0.5764069 .
# The Hessian was positive definite.
# Model rank =  13 / 13 
# 
# Basis dimension (k) checking results. Low p-value (k-index<1) may
# indicate that k is too low, especially if edf is close to k'.
# 
#                 k' edf k-index p-value
# s(Surface_Area)  9   1    1.42    0.97
plot(gam1)


#Summary Plot

# Create the new data frame
new.data.IV1 <- rep(seq(min(SSIDm$Surface_Area),max(SSIDm$Surface_Area)),
                    length(unique(factor(SSIDm$Site)))[1]) 
new.data.IV2 <- c(rep("ex situ", length(new.data.IV1)/4), rep("Port", length(new.data.IV1)/4),
                  rep("Reef 1", length(new.data.IV1)/4), rep("Reef 2", length(new.data.IV1)/4))
new.data <- as.data.frame(cbind(new.data.IV1, new.data.IV2))
names(new.data) <- c("Surface_Area", "Site") 
str(new.data)
class(new.data$Surface_Area)
#character 
new.data$Surface_Area <- as.numeric(as.character(new.data$Surface_Area))
class(new.data$Surface_Area)
model.predict <- predict(gam1, newdata = new.data,
                         type="response", se.fit=TRUE)
preds <- cbind(model.predict, new.data)
names(preds)

pred.1 <- subset(preds, Site=="ex situ")
pred.2 <- subset(preds, Site=="Port")
pred.3 <- subset(preds, Site=="Reef 1")
pred.4 <- subset(preds, Site=="Reef 2")

dataset.1 <- subset(SSIDm, SSIDm$Site=="ex situ")
dataset.2 <- subset(SSIDm, SSIDm$Site=="Port")
dataset.3 <- subset(SSIDm, SSIDm$Site=="Reef 1")
dataset.4 <- subset(SSIDm, SSIDm$Site=="Reef 2")

x <- c(pred.1$Surface_Area, rev(pred.1$Surface_Area))
y1 <- c(pred.1$fit+ pred.1$se.fit, rev(c(pred.1$fit-pred.1$se.fit)))
y2 <- c(pred.2$fit+ pred.2$se.fit, rev(c(pred.2$fit-pred.2$se.fit)))
y3 <- c(pred.3$fit+ pred.3$se.fit, rev(c(pred.3$fit-pred.3$se.fit)))
y4 <- c(pred.4$fit+ pred.4$se.fit, rev(c(pred.4$fit-pred.4$se.fit)))

windows(8,8)
par(mar = c(5, 5, 4, 3))
par(mgp = c(3.3,0.75, 0))

plot(fit~Surface_Area, col=c("#999999"), data=pred.1, type="l", lwd=5,
     ylim=c(0, max(SSIDm$Cross_Mes)),xlab=expression("Surface Area (cm"^2*")"), ylab=expression('Ave. No. Spermaries per Polyp'), 
    cex.axis = 1.2, main = "Males", cex.main = "2" , cex.lab = 1.4, las = 1)
points(fit~ Surface_Area, col=c('#08589E'), data=pred.2, type="l", lwd=5)
points(fit~ Surface_Area, col="#4EB3D3", data=pred.3, type="l", lwd=5)
points(fit~ Surface_Area, col="#A8DDB5", data=pred.4, type="l", lwd=5)


#Adding Standard Error bars
#Ex situ
x <- c(pred.1$Surface_Area, rev(pred.1$Surface_Area))
y1 <- c(pred.1$fit + pred.1$se.fit, rev(pred.1$fit - pred.1$se.fit))
polygon(x, y1, col = rgb(153/255, 153/255, 153/255, 0.1), border = NA)
#Port
x2 <- c(pred.2$Surface_Area, rev(pred.2$Surface_Area))
y2 <- c(pred.2$fit + pred.2$se.fit, rev(pred.2$fit - pred.2$se.fit))
polygon(x2, y2, col = rgb(8/255, 88/255, 158/255, 0.1), border = NA)
#Reef 1
x3 <- c(pred.3$Surface_Area, rev(pred.3$Surface_Area))
y3 <- c(pred.3$fit + pred.3$se.fit, rev(pred.3$fit - pred.3$se.fit))
polygon(x3, y3, col = rgb(78/255, 179/255, 211/255, 0.1), border = NA)
#Reef 2
x4 <- c(pred.4$Surface_Area, rev(pred.3$Surface_Area))
y4 <- c(pred.4$fit + pred.4$se.fit, rev(pred.4$fit - pred.4$se.fit))
polygon(x4, y4, col = rgb(168/255, 221/255, 181/255, 0.1), border = NA)

#Adding points
points(Cross_Mes ~ Surface_Area , col=c("#333333"), data= dataset.1, pch=8, cex=2.3) 
points(Cross_Mes ~ Surface_Area, col=c('#08589E'), data= dataset.2, pch=19, cex=2.1)  
points(Cross_Mes ~ Surface_Area, col="#4EB3D3", data= dataset.3, pch=17, cex=1.6) 
points(Cross_Mes ~ Surface_Area, col="#85C497", data= dataset.4, pch=15, cex=1.6) 


#Legend 
windows(8,8)
par(new = TRUE, xpd = TRUE) 
plot(NULL, yaxt='n',bty='n',ylab='',xlab='') #empty plot for legend
legend("top",inset = c( 0,-0.17), legend =c(expression(italic("ex situ")), 'Port Everglades','Reef 1','Reef 2'), pch=c(8, 19,17,15), pt.cex=2, cex=2, bty='n', 
       col = c("#333333",'#08589E', '#4EB3D3','#85C497'))
par(xpd = FALSE)

    #FEMALES ##################
SSIDf <- subset(Aug2020_2019_Reproductive_Final_09_22_24, `M/F`=='F')
#NOTE: Fecundity is NOT square rooted, like it is for the fecundity~year
#Fit 2 models to determine best smoother
gam1<-gam(Cross_Mes~s(Surface_Area)+factor(Site),data = SSIDf) #same smoother across all factors
gam2 <- gam (Cross_Mes~s(Surface_Area)+ s(Surface_Area, by=factor(Site)) +
                 factor(Site), data=SSIDf)     #different smoother for each level of factor
    #Error in gam(Cross_Mes ~ s(Surface_Area) + s(Surface_Area, by = factor(Site)) +  : 
    #Model has more coefficients than data
    #AIC(gam1,gam2)

#USE gam1

summary(gam1)
  # Parametric coefficients:
  #   Estimate Std. Error t value Pr(>|t|)
  # (Intercept)          19.457     20.516   0.948    0.352
  # factor(Site)Port     21.626     23.490   0.921    0.366
  # factor(Site)Reef 1    4.624     31.144   0.148    0.883
  # factor(Site)Reef 2    3.765     27.890   0.135    0.894
  # 
  # Approximate significance of smooth terms:
  #   edf Ref.df    F p-value
  # s(Surface_Area)   1      1 0.83   0.371
  # 
  # R-sq.(adj) =  -0.00758   Deviance explained = 13.1%
  # GCV = 1838.3  Scale est. = 1531.9    n = 30

anova(gam1)
  # Parametric Terms:
  #   df    F p-value
  # factor(Site)  3 0.56   0.647
  # 
  # Approximate significance of smooth terms:
  #   edf Ref.df    F p-value
  # s(Surface_Area)   1      1 0.83   0.371


gam.check(gam1)
plot(gam1)

#Summary Plot

# Create the new data frame
new.data.IV1 <- rep(seq(min(SSIDf$Surface_Area),max(SSIDf$Surface_Area)),
                    length(unique(factor(SSIDf$Site)))[1]) 
new.data.IV2 <- c(rep("ex situ", length(new.data.IV1)/4), rep("Port", length(new.data.IV1)/4),
                  rep("Reef 1", length(new.data.IV1)/4), rep("Reef 2", length(new.data.IV1)/4))
new.data <- as.data.frame(cbind(new.data.IV1, new.data.IV2))
names(new.data) <- c("Surface_Area", "Site") 
str(new.data)
class(new.data$Surface_Area)
#character 
new.data$Surface_Area <- as.numeric(as.character(new.data$Surface_Area))
class(new.data$Surface_Area)
model.predict <- predict(gam1, newdata = new.data,
                         type="response", se.fit=TRUE)
preds <- cbind(model.predict, new.data)
names(preds)

pred.1 <- subset(preds, Site=="ex situ")
pred.2 <- subset(preds, Site=="Port")
pred.3 <- subset(preds, Site=="Reef 1")
pred.4 <- subset(preds, Site=="Reef 2")

dataset.1 <- subset(SSIDf, SSIDf$Site=="ex situ")
dataset.2 <- subset(SSIDf, SSIDf$Site=="Port")
dataset.3 <- subset(SSIDf, SSIDf$Site=="Reef 1")
dataset.4 <- subset(SSIDf, SSIDf$Site=="Reef 2")

x <- c(pred.1$Surface_Area, rev(pred.1$Surface_Area))
y1 <- c(pred.1$fit+ pred.1$se.fit, rev(c(pred.1$fit-pred.1$se.fit)))
y2 <- c(pred.2$fit+ pred.2$se.fit, rev(c(pred.2$fit-pred.2$se.fit)))
y3 <- c(pred.3$fit+ pred.3$se.fit, rev(c(pred.3$fit-pred.3$se.fit)))
y4 <- c(pred.4$fit+ pred.4$se.fit, rev(c(pred.4$fit-pred.4$se.fit)))

windows(8,8)
par(mar = c(5, 5, 4, 3))
par(mgp = c(3.3,0.75, 0)) 

plot(fit~Surface_Area, col=c("#999999"), data=pred.1, type="l", lwd=5,
     ylim=c(0, max(SSIDf$Cross_Mes)),xlab=expression("Surface Area (cm"^2*")"), ylab=expression('Ave. No. Eggs per Polyp'), 
     cex.axis = 1.2, main = "Female", cex.main = "2" , cex.lab = 1.4, las = 1)
points(fit~ Surface_Area, col=c('#08589E'), data=pred.2, type="l", lwd=5)
points(fit~ Surface_Area, col="#4EB3D3", data=pred.3, type="l", lwd=5)
points(fit~ Surface_Area, col="#A8DDB5", data=pred.4, type="l", lwd=5)

#Adding Standard Error bars
#Ex situ
x <- c(pred.1$Surface_Area, rev(pred.1$Surface_Area))
y1 <- c(pred.1$fit + pred.1$se.fit, rev(pred.1$fit - pred.1$se.fit))
polygon(x, y1, col = rgb(153/255, 153/255, 153/255, 0.1), border = NA)
#Port
x2 <- c(pred.2$Surface_Area, rev(pred.2$Surface_Area))
y2 <- c(pred.2$fit + pred.2$se.fit, rev(pred.2$fit - pred.2$se.fit))
polygon(x2, y2, col = rgb(8/255, 88/255, 158/255, 0.1), border = NA)
#Reef 1
x3 <- c(pred.3$Surface_Area, rev(pred.3$Surface_Area))
y3 <- c(pred.3$fit + pred.3$se.fit, rev(pred.3$fit - pred.3$se.fit))
polygon(x3, y3, col = rgb(78/255, 179/255, 211/255, 0.1), border = NA)
#Reef 2
x4 <- c(pred.4$Surface_Area, rev(pred.3$Surface_Area))
y4 <- c(pred.4$fit + pred.4$se.fit, rev(pred.4$fit - pred.4$se.fit))
polygon(x4, y4, col = rgb(168/255, 221/255, 181/255, 0.1), border = NA)

#Adding points
points(Cross_Mes ~ Surface_Area , col=c("#333333"), data= dataset.1, pch=8, cex=2.5) 
points(Cross_Mes ~ Surface_Area, col=c('#08589E'), data= dataset.2, pch=19, cex=2.1)   
points(Cross_Mes ~ Surface_Area, col="#4EB3D3", data= dataset.3, pch=17, cex=1.6) 
points(Cross_Mes ~ Surface_Area, col="#85c497", data= dataset.4, pch=15, cex=1.6)


# Fecundity ~ Year t-test (ex situ excluded) ##################################################
    #Create Subsets ######################
class(Aug2020_2019_Reproductive_Final_09_22_24$Cross_Mes)
class(Aug2020_2019_Reproductive_Final_09_22_24$Year)
Aug2020_2019_Reproductive_Final_09_22_24$Year <- factor(Aug2020_2019_Reproductive_Final_09_22_24$Year)

SSIDmIN <- subset(Aug2020_2019_Reproductive_Final_09_22_24, 
                `M/F` == 'M' & !(Site== "ex situ"))
SSIDmIN #all in situ males
SSIDfIN <- subset(Aug2020_2019_Reproductive_Final_09_22_24, 
                `M/F` == 'F' & !(Site== "ex situ"))
SSIDfIN #all in situ females
    #MALES  ####################################
attach(SSIDmIN)

boxplot(Cross_Mes~Year)
shapiro.test(subset(SSIDmIN,Year=='2019')$Cross_Mes)
#W = 0.81382, p-value = 0.02933
#data is not normal
shapiro.test(sqrt(subset(SSIDmIN,Year=='2019')$Cross_Mes))
#W = 0.90656, p-value = 0.2926
#data is normal with sqrt
shapiro.test(sqrt(subset(SSIDmIN,Year=='2020')$Cross_Mes))
#W = 0.88164, p-value = 0.1363
#data is NORMAL with sqrt

bartlett.test (sqrt(Cross_Mes)~Year)
#Bartlett's K-squared = 0.00030717, df = 1, p-value = 0.986

#transformed data MEETS PARAMETRIC assumptions

#t-test
t.test(sqrt(Cross_Mes)~Year)
# t = -0.19398, df = 16.813, p-value = 0.8485
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -27.91318  23.21622
# sample estimates:
#   mean in group 2019 mean in group 2020 
# 33.62764           35.97612 

detach(SSIDmIN)

    #FEMALES ####################################
attach(SSIDfIN)

boxplot(Cross_Mes~Year)
shapiro.test(subset(SSIDfIN,Year=='2019')$Cross_Mes)
#W = 0.66579, p-value = 0.000167
#data is not normal
shapiro.test(sqrt(subset(SSIDfIN,Year=='2019')$Cross_Mes))
#W = 0.89681, p-value = 0.1014
#data is normal with sqrt
shapiro.test(sqrt(subset(SSIDfIN,Year=='2020')$Cross_Mes))
#W = 0.94998, p-value = 0.6437
#data is normal with sqrt

bartlett.test (sqrt(Cross_Mes)~Year)
#Bartlett's K-squared = 1.2362, df = 1, p-value = 0.2662

#transformed data does meet parametric assumptions

t.test (sqrt(Cross_Mes)~Year)
# data:  sqrt(Cross_Mes) by Year
# t = 0.037616, df = 22.806, p-value = 0.9703
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -2.401130  2.490028
# sample estimates:
#   mean in group 2019 mean in group 2020 
# 5.320984           5.276535

detach(SSIDfIN)

