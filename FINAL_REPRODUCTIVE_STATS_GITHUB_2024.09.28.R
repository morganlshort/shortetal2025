#New Stats for Coral Reefs Publication Revisions
#Updated 10/6/2024


#FERTILITY ##########################################
#Fertility ~ Site Contingency Tables  ###############
# Includes hermaphrodite but does not include colony 11 and 103 due to insufficient data
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
#p<0.05, Fertility has a strong association with Site


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


#Fertility ~ Size Class Contingency table ######################
#includes hermaphrodite but does not include colony 11 or 103 due to insufficient data
data.xtab <-xtabs (Frequency ~ Size_Class + Fertility, Aug2020_2019_Fertility_Frequency_SizeClass_9_28_24)
chisq.test(data.xtab,corr=F)$exp # expected frequencies
#>20% are less than 5
#assumption is not met
fisher.test(data.xtab)
#p-value = 0.060
#There is no association between size and fertility status
library(vcd)
strucplot(data.xtab,shade=T)


# Fertility ~ Year ############
freq_table <- table(Aug2020_2019_Reproductive_Final_09_22_24$Year, Aug2020_2019_Reproductive_Final_09_22_24$Fertility)
freq_table
# Fecundity ~ Year 
data.xtab <-xtabs (Frequency ~ Year + Fertility, Aug2020_2019_Fertility_Frequency_Year_9_28_24)
chisq.test(data.xtab,corr=F)$exp
# Fertility
# Year         N        Y
# 2019 5.28125 20.71875
# 2020 7.71875 30.28125
#0% of expected frequencies are <5; assumption is met
chisq.test(data.xtab, corr=F)
#X-squared = 2.0826, df = 1, p-value = 0.149


#FECUNDITY ########################################
#Fecundity ~ Site:Year GAM ###### ADD SUMMARY PLOT IF USING ######################

library('mgcv')

    #MALES ############################
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
#   Estimate Std. Error t value
# (Intercept)          -576.3     1423.3  -0.405
# factor(Site)Port     1854.4     1612.7   1.150
# factor(Site)Reef 1   2782.7     1682.0   1.654
# factor(Site)Reef 2   3692.5     1593.4   2.317
# Pr(>|t|)  
# (Intercept)          0.6909  
# factor(Site)Port     0.2671  
# factor(Site)Reef 1   0.1175  
# factor(Site)Reef 2   0.0341 *
#   ---
#   Signif. codes:  
#   0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
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
#   edf Ref.df     F p-value
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


    #FEMALES ##################
SSIDf <- subset(Aug2020_2019_Reproductive_Final_09_22_24, `M/F`=='F')
#NOTE: Fecundity is NOT square rooted, like it is for the
#fecundity~year or surface area~year
#Fit 2 models ot determine best smoother
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
#p=0.647

gam.check(gam1)
plot(gam1)

#Summary plot

# CREATE SUBSETS FOR FECUNDITY ANALYSES BELOW ############################################
class(Aug2020_2019_Reproductive_Final_09_22_24$Site)
Aug2020_2019_Reproductive_Final_09_22_24$Site <- factor(Aug2020_2019_Reproductive_Final_09_22_24$Site)
class(Aug2020_2019_Reproductive_Final_09_22_24$Cross_Mes)
class(Aug2020_2019_Reproductive_Final_09_22_24$Surface_Area)
class(Aug2020_2019_Reproductive_Final_09_22_24$Diameter)
Aug2020_2019_Reproductive_Final_09_22_24$Habitat <- factor(Aug2020_2019_Reproductive_Final_09_22_24$Habitat)
class(Aug2020_2019_Reproductive_Final_09_22_24$Year)
Aug2020_2019_Reproductive_Final_09_22_24$Year <- factor(Aug2020_2019_Reproductive_Final_09_22_24$Year)

SSIDm <- subset(Aug2020_2019_Reproductive_Final_09_22_24, `M/F`=='M')
SSIDm #male SSIDs, 21
SSIDf <- subset(Aug2020_2019_Reproductive_Final_09_22_24, `M/F`=='F')
SSIDf #female SSIDS, 30 females
# Fecundity ~ Surface Area Correlation Analysis ######################
    #MALES ####################
attach(SSIDm)
library(lme4)
library(car)
scatterplot(Cross_Mes~Surface_Area)
#appears to be a slight upward trend, but there are peaks and valleys...

shapiro.test(Cross_Mes)
#W = 0.79136, p-value = 0.0004855
#p<0.05, try transformations
shapiro.test(sqrt(Cross_Mes))
#W = 0.88528, p-value = 0.01832
shapiro.test(log10(Cross_Mes))
#W = 0.81286, p-value = 0.001039
shapiro.test(log(Cross_Mes+1))
#W = 0.86295, p-value = 0.00716
#DATA IS NOT NORMAL, EVEN AFTER TRANSFORMATIONS!

#Non-parametric correlation
#Spearman's rank (sample size of 21)
cor.test(Surface_Area,Cross_Mes,method='spearman')
# Spearman's rank correlation rho
# 
# data:  Surface_Area and Cross_Mes
# S = 1346, p-value = 0.5851
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#      rho 
# 0.125974 
#p>0.05, they are not correlated 

detach(SSIDm)

    #FEMALES ##############################

attach(SSIDf)
library(lme4)
library(car)
scatterplot(Cross_Mes~Surface_Area)
#even distribution, slight negative trend with increasing surface area; largely
#driven by a single high outlier 


#Check normality
shapiro.test(Cross_Mes)
#W = 0.69147, p-value = 1.175e-06
#not normal, try transformations
shapiro.test(sqrt(Cross_Mes))
#W = 0.92866, p-value = 0.04523
shapiro.test(log10(Cross_Mes))
#W = 0.87714, p-value = 0.002425  
shapiro.test(log(Cross_Mes+1))
#W = 0.90553, p-value = 0.0115
#DATA IS NOT NORMAL

#Non-parametric correlation anlysis
#spearman's rank --> 30 female samples (just barely makes it)
cor.test(sqrt(Surface_Area),sqrt(Cross_Mes),method='spearman')
#Spearman's rank correlation rho
# S = 6150, p-value = 0.04605               
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#   rho 
# -0.3681869 
#the output does not change if surface area or cross_mes are not sqrt
#not significant, but a trend of decreasing fecundity with increasing size

detach(SSIDf)

# Fecundity ~ Year T-test ##################################################
    #MALES  ####################################
#Variables: year - categorical predictor (2 levels)
#polyp fecundity - continuous response
#Make sure using SSIDm from Aug2020_2019_Reproductive_Final_09_22_24
class(SSIDm$Year)
#factor
attach(SSIDm)
levels(Year)
#2019 and 2020

boxplot(Cross_Mes~Year)
shapiro.test(subset(SSIDm,Year=='2019')$Cross_Mes)
#W = 0.81382, p-value = 0.02933
#data is not normal
shapiro.test(sqrt(subset(SSIDm,Year=='2019')$Cross_Mes))
#W = 0.90656, p-value = 0.2926
#data is normal with sqrt
shapiro.test(sqrt(subset(SSIDm,Year=='2020')$Cross_Mes))
#W = 0.86303, p-value = 0.05336
#data is NORMAL with sqrt

bartlett.test (sqrt(Cross_Mes)~Year)
#Bartlett's K-squared = 0.0012681, df = 1, p-value = 0.9716

#transformed data MEETS PARAMETRIC assumptions

#t-test
t.test(sqrt(Cross_Mes)~Year)
# t = 0.21205, df = 17.501, p-value = 0.8345
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -22.04617  26.98496
# sample estimates:
#   mean in group 2019 mean in group 2020 
# 33.62764           31.15824 
#p>0.05, year does not have a significant effect on fecundity
detach(SSIDm)

    #FEMALES ####################################
#Variables: year - categorical predictor (2 levels)
#polyp fecundity - continuous response
#make sure using SSIDf based on Aug2020_2019_Reproductive_Final_09_22_24
class(SSIDf$Year)
#factor
attach(SSIDf)
levels(Year)
#2019 and 2020

boxplot(Cross_Mes~Year)
shapiro.test(subset(SSIDf,Year=='2019')$Cross_Mes)
#W = 0.66579, p-value = 0.000167
#data is not normal
shapiro.test(sqrt(subset(SSIDf,Year=='2019')$Cross_Mes))
#W = 0.89681, p-value = 0.1014
#data is normal with sqrt
shapiro.test(sqrt(subset(SSIDf,Year=='2020')$Cross_Mes))
#W = 0.94621, p-value = 0.4321
#data is normal with sqrt

bartlett.test (sqrt(Cross_Mes)~Year)
#Bartlett's K-squared = 0.94826, df = 1, p-value = 0.3302

#transformed data does meets parametric assumptions

t.test (sqrt(Cross_Mes)~Year)
# t = 0.81377, df = 24.255, p-value = 0.4237
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -1.419682  3.269658
# sample estimates:
#   mean in group 2019 mean in group 2020 
# 5.320984           4.395996

detach(SSIDf)

#Fecundity ~ Site ANOVA ################################################
    #MALES (IN SITU ONLY) #######################################################
#Remove the ex situ colonies because there are only 2 males
SSIDmIS <- subset(SSIDm, !(`Site`)=='ex situ')
SSIDmIS
#19

attach(SSIDmIS)

#CHECK PARAMETRIC ASSUMPTIONS
boxplot(Cross_Mes~Site) #data may be normal

shapiro.test (subset(SSIDmIS, Site=='Port')$Cross_Mes) 
#W = 0.76063, p-value = 0.004809
#p<0.05, therefore data is not normal
shapiro.test (log10(subset(SSIDmIS, Site=='Port')$Cross_Mes))
#W = 0.92269, p-value = 0.38
shapiro.test (sqrt(subset(SSIDmIS, Site=='Port')$Cross_Mes))
#W = 0.89913, p-value = 0.2143
#Parametric assumption of normality met with square root transformation
    #Selected sqrt to be consistent with females

shapiro.test (sqrt(subset(SSIDmIS, Site=='Reef 2')$Cross_Mes))
#W = 0.85203, p-value = 0.2328
shapiro.test (sqrt(subset(SSIDmIS, Site=='Reef 1')$Cross_Mes))
#W = 0.87365, p-value = 0.2815
#p>0.05 with transformations, will proceed to test homogeneity of variance

plot (tapply (sqrt(Cross_Mes), Site, mean), tapply (sqrt(Cross_Mes), Site,
                                                     var))
bartlett.test(sqrt(Cross_Mes)~Site)
#Bartlett's K-squared = 1.5939, df = 2, p-value = 0.4507
#p>0.05, variances are homogeneous

#RUN MODEL EXCLUDING LAND SITES
mod.SSIDmIS<- aov (sqrt(Cross_Mes) ~ Site)
anova(mod.SSIDmIS)
#           Df Sum Sq Mean Sq F value Pr(>F)
# Site2      2 2434.6 1217.31  2.0714 0.1585
# Residuals 16 9402.7  587.67   
#p>0.05, there is not a significant difference in male SSID fecundity between ocean sites

windows(8,8)
par(mar = c(3, 5.5, 4, 3) + 0.5) 
boxplot(sqrt(Cross_Mes)~Site,main='Males',cex.main = 1.8, xlab='',
        ylab=expression(sqrt('Ave. No. spermaries per polyp')), 
        col=c('#08589E', '#4EB3D3',  '#A8DDB5'), cex.lab = 2.0, cex.axis=1.7, las=1)
text(0.4,68.5,cex = 1.2, expression(italic(p)== 0.16), pos = 4)
text(0.4,71.9,cex = 1.2, expression(italic(F)[list(2,16)]== 2.1), pos = 4)

detach(SSIDmIS)
    #FEMALE (ALL SITES) ##################################################
attach(SSIDf)

#CHECK PARAMETRIC ASSUMPTIONS
boxplot(Cross_Mes~Site) #data may be normal??

shapiro.test (subset(SSIDf, Site=='Port')$Cross_Mes)
#W = 0.71817, p-value = 0.0002695
#p<0.05, will try transformations
shapiro.test (sqrt(subset(SSIDf, Site=='Port')$Cross_Mes))
#W = 0.92386, p-value = 0.1946
shapiro.test (log10(subset(SSIDf, Site=='Port')$Cross_Mes))
#W = 0.85683, p-value = 0.0172
#Parametric assumption of normality met with square root transformation


shapiro.test(sqrt(subset(SSIDf, Site=='Reef 1')$Cross_Mes))
#W = 0.99695, p-value = 0.8946
shapiro.test(sqrt(subset(SSIDf, Site=='Reef 2')$Cross_Mes))
#W = 0.95779, p-value = 0.8026
shapiro.test(sqrt(subset(SSIDf, Site=='ex situ')$Cross_Mes))
#W = 0.89414, p-value = 0.3784
#p>0.05 FOR ALL SiteS; DATA IS NORMAL

plot (tapply (sqrt(Cross_Mes), Site, mean), tapply (sqrt(Cross_Mes), Site,
                                                    var))
bartlett.test(sqrt(Cross_Mes)~Site)
#Bartlett's K-squared = 1.5602, df = 3, p-value = 0.6684

#parametric assumptions are met 

#RUN PARAMETRIC TEST
mod.SSIDf <- aov (sqrt(Cross_Mes) ~ Site)
anova(mod.SSIDf)
# Df  Sum Sq Mean Sq F value Pr(>F)
# Site       3  39.837 13.2791  1.5199 0.2328
# Residuals 26 227.159  8.7369  
#p>0.05, there is no significant difference in female SSID fecundity between sites


windows(8,8)
par(mar = c(3, 5.5, 4, 3) + 0.5) 
boxplot(sqrt(Cross_Mes)~Site,main='Females',cex.main = 1.8, xlab='',
        ylab=expression(sqrt('Ave. No. eggs per polyp')), xaxt = 'n', 
        col=c('#999999','#08589E', '#4EB3D3',  '#A8DDB5'), cex.lab = 2.0, cex.axis=1.7,las=1)
axis(1, at=c(1,2,3,4), labels=c(expression(italic("ex situ")), 'Port','Reef 1','Reef 2'),cex.axis=1.7, las=1)
text(4.6,13.5,cex = 1.2, expression(italic(p)== 0.23), pos = 2)
text(4.6,14.2,cex = 1.2, expression(italic(F)[list(3,26)]== 1.5), pos = 2)

detach(SSIDf)

