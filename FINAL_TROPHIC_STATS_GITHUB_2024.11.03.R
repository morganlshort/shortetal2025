#Trophic Analyses for Coral Reefs Publication (Short et al., 2024)

#Datasets used in this script:
    #Trophic_Position_10.6.24
    #d15N_AllAAs_10.6.24
    #PercentHetero_Theoretical_10.6.24

###### TROPHIC POSITION ONLY ANALYSES & SUMMARIES ##############################
# 1. Create New dataset for Coral Tissues only #############
FECTP <- subset(Trophic_Position_10_6_24, Tissue_type=='coralt')
#Fecundity & Trophic position
FECTP
# Summary Statistics for CORALS at each site ############
#### Trophic Position and Phe & Glu dn15 at All Sites
attach(FECTP)
#Phe
max(Phe_d15N)
#6.416564
min(Phe_d15N)
#-1.617872
mean(Phe_d15N)
#2.888362
sd(Phe_d15N)
#2.447321

#Glu
max(Glu_d15N)
#13.9
min(Glu_d15N)
#5.7
mean(Glu_d15N)
#10.004
sd(Glu_d15N)
#1.996973

#Trophic Position
max(TP_Glu_Phe)
#2.035557
min(TP_Glu_Phe)
#1.068965
mean(TP_Glu_Phe)
#1.488596
sd(TP_Glu_Phe)
#0.266248
detach(FECTP)

# %heterotrophy at all sites
attach(PercentHetero_Theoretical_10_6_24)
max(HOSTavg_Percent)
# 0.7766677
min(HOSTavg_Percent)
#0.05172345
mean(HOSTavg_Percent)
#0.3664469
sd(HOSTavg_Percent)
#0.199686
detach(PercentHetero_Theoretical_10_6_24)

#### Corals in Port Everglades
Port <- subset(Trophic_Position_10_6_24, Tissue_type == 'coralt' & Site == 'Port')
Port

attach(Port)
max(TP_Glu_Phe)
#1.537655
min(TP_Glu_Phe)
#1.068965
max(TP_Glu_Phe) - min(TP_Glu_Phe)
#0.4686907
mean(TP_Glu_Phe)
#1.332351
sd(TP_Glu_Phe)
#0.2119602
detach(Port)


##### Corals in Reef1
Reef1 <- subset(Trophic_Position_10_6_24, Tissue_type == 'coralt' & Site == 'Reef 1')
Reef1

attach(Reef1)
max(TP_Glu_Phe)
# 1.61406
min(TP_Glu_Phe)
#1.208111
max(TP_Glu_Phe) - min(TP_Glu_Phe)
#0.4059489
mean(TP_Glu_Phe)
#1.437699
sd(TP_Glu_Phe)
#0.1864628
detach(Reef1)

####Corals in Reef2
Reef2 <- subset(Trophic_Position_10_6_24, Tissue_type == 'coralt' & Site == 'Reef 2')
Reef2

attach(Reef2)
max(TP_Glu_Phe)
#1.770186
min(TP_Glu_Phe)
#1.083245
max(TP_Glu_Phe) - min(TP_Glu_Phe)
#0.6869418
mean(TP_Glu_Phe)
# 1.357053
sd(TP_Glu_Phe)
#0.2314727
detach(Reef2)

#### Corals Ex Situ
coralsLAB <- subset(Trophic_Position_10_6_24, Tissue_type == 'coralt' & Site=='ex situ')
coralsLAB

attach(coralsLAB)
max(TP_Glu_Phe)
#2.035557
min(TP_Glu_Phe)
#1.472734
mean(TP_Glu_Phe)
#1.736248
sd(TP_Glu_Phe)
#0.2124023
detach(coralsLAB)

#### Corals in situ (Port, Reef 1, and Reef 2 combined)
coralsNOLAB <- subset(Trophic_Position_10_6_24, Tissue_type == 'coralt' & !(Site=='ex situ'))
coralsNOLAB

attach(coralsNOLAB)
max(TP_Glu_Phe)
#1.770186
min(TP_Glu_Phe)
#1.068965
mean(TP_Glu_Phe)
#1.372054
sd(TP_Glu_Phe)
#0.2037097

detach(coralsNOLAB)

# Summary Statistics for ENDMEMBERS at each site ############################

#Zooplankton
Zoop <- subset(Trophic_Position_10_6_24, Tissue_type=='Zooplankton')
Zoop
#All sites combined: units are cm^2
attach(Zoop)
#Max
max(TP_Glu_Phe)
#1.900077
#min
min(TP_Glu_Phe)
#1.775615
#Mean:
mean(TP_Glu_Phe) 
# 1.847093
#STDEV
sd(TP_Glu_Phe)
#0.05848586
hist(TP_Glu_Phe)
detach(Zoop)

#In situ Zoop:
ZoopINSITU <- subset(Trophic_Position_10_6_24, Tissue_type=='Zooplankton' & !(Site =='ex situ'))
ZoopINSITU

attach(ZoopINSITU)
#Max
max(TP_Glu_Phe)
#1.900077
#min
min(TP_Glu_Phe)
#1.775615
#Mean:
mean(TP_Glu_Phe) 
# 1.855001
#STDEV
sd(TP_Glu_Phe)
#0.06896106
hist(TP_Glu_Phe)
detach(ZoopINSITU)


#POM
POM <- subset(Trophic_Position_10_6_24, Tissue_type=='POM')
POM
#All sites combined: units are cm^2
attach(POM)
#Max
max(TP_Glu_Phe)
#2.039677
#min
min(TP_Glu_Phe)
#0.8924428
#Mean:
mean(TP_Glu_Phe) 
# 1.318021
#STDEV
sd(TP_Glu_Phe)
#0.5042966
hist(TP_Glu_Phe)
detach(POM)

#In situ POM:
POMINSITU <- subset(Trophic_Position_10_6_24, Tissue_type=='POM' & !(Site =='ex situ'))
POMINSITU

attach(POMINSITU)
#Max
max(TP_Glu_Phe)
#1.262748
#min
min(TP_Glu_Phe)
#0.8924428
#Mean:
mean(TP_Glu_Phe) 
# 1.07747
#STDEV
sd(TP_Glu_Phe)
#0.1851525
hist(TP_Glu_Phe)
detach(POMINSITU)


#Endosymbiont
symbiont <- subset(Trophic_Position_10_6_24, Tissue_type=='symbiont')
symbiont
#All sites combined: units are cm^2
attach(symbiont)
#Max
max(TP_Glu_Phe)
#1.212773
#min
min(TP_Glu_Phe)
#0.6418002
#Mean:
mean(TP_Glu_Phe) 
# 0.9738862
#STDEV
sd(TP_Glu_Phe)
#0.172448
hist(TP_Glu_Phe)
detach(symbiont)

#Endosymbiont
symbiontINSITU <- subset(Trophic_Position_10_6_24, Tissue_type=='symbiont' & !(Site =='ex situ'))
symbiontINSITU
#units are cm^2
attach(symbiontINSITU)
#Max
max(TP_Glu_Phe)
#1.212773
#min
min(TP_Glu_Phe)
#0.6418002
#Mean:
mean(TP_Glu_Phe) 
# 0.9441853
#STDEV
sd(TP_Glu_Phe)
#0.1754295
hist(TP_Glu_Phe)
detach(symbiontINSITU)


# Coral Trophic Position ~ Site One-Way ANOVA (Glu-Phe) ######### ###################
class(FECTP$Site)
FECTP$Site <- factor(FECTP$Site)
attach(FECTP)

boxplot(TP_Glu_Phe ~ Site)
shapiro.test(subset(FECTP,Site=='Reef 1')$TP_Glu_Phe)
shapiro.test(subset(FECTP,Site=='Port')$TP_Glu_Phe)
shapiro.test(subset(FECTP,Site=='ex situ')$TP_Glu_Phe) 
shapiro.test(subset(FECTP,Site=='Reef 2')$TP_Glu_Phe) 
#data is normal


plot (tapply (TP_Glu_Phe, Site, mean), tapply (TP_Glu_Phe, Site,
                                               var))

bartlett.test(TP_Glu_Phe ~Site)
#Bartlett's K-squared = 0.18753, df = 3, p-value = 0.9796

#Parametric assumptions are met

#One-way ANOVA
model <- aov (TP_Glu_Phe ~ Site)
anova(model)
# Analysis of Variance Table
# 
# Response: TP_Glu_Phe
#           Df  Sum Sq  Mean Sq F value   Pr(>F)   
# Site       3 0.75390 0.251300  5.5702 0.005674 **
#   Residuals 21 0.94741 0.045115                      
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#p<0.05, reject the null

TukeyHSD(model)
# $Site
# diff        lwr         upr     p adj
# Port-Lab      -0.40389721 -0.7236328 -0.08416160 0.0101030
# Reef 1-Lab    -0.29854831 -0.6360604  0.03896374 0.0951938
# Reef 2-Lab    -0.37919522 -0.6989308 -0.05945961 0.0163838
# Reef 1-Port    0.10534890 -0.2531463  0.46384408 0.8447857
# Reef 2-Port    0.02470199 -0.3171098  0.36651373 0.9970132
# Reef 2-Reef 1 -0.08064691 -0.4391421  0.27784827 0.9222065
library(multcompView)# install package multcompView before
multcompLetters(extract_p(TukeyHSD(model)$Site))
# Port Reef 1 Reef 2    Lab 
# "a"   "ab"    "a"    "b" 

windows(8,6.5)
par(mar = c(3, 5.5, 2.5, 2.5) + 0.5) 
boxplot(TP_Glu_Phe~Site,xlab='',ylab=expression("TP"[list(Glu-Phe)]),
        ylim = c(1.0, 2.2), xaxt = 'n',
        col=c('#999999', '#08589E', '#4EB3D3','#A8DDB5'),
        cex.lab = 2.0, cex.axis=1.5, las=1)
axis(1, at=c(1,2,3,4), labels=c(expression(italic("ex situ")), 'Port','Reef 1','Reef 2'),cex.axis=1.7, las=1)
text(4.7,2.15,cex = 1.2, expression(italic(p)== 0.006), pos = 2)
text(4.7,2.2,cex = 1.2, expression(italic('F ')[list(3,21)]== 5.57), pos = 2)
text(1,2.15,cex = 1.2, expression(b), pos = 1) #lab
text(2,1.66,cex = 1.2, expression(a), pos = 1) #port
text(3,1.76,cex = 1.2, expression(ab), pos = 1) #reef 1
text(4,1.9,cex = 1.2, expression(a), pos = 1) #reef 2



# All Tissues' Trophic Position ~ Tissue & Site Boxplot #############################
class(Trophic_Position_10_6_24$Tissue_type)
Trophic_Position_10_6_24$Tissue_type <- factor (Trophic_Position_10_6_24$Tissue_type)
Trophic_Position_10_6_24$Site <- factor (Trophic_Position_10_6_24$Site)
attach(Trophic_Position_10_6_24)

#Using colors from the brewer pallet "Paired"
#brewer.pal(n = 12, name = 'Paired')

noPlankton <- subset(Trophic_Position_10_6_24, !(Tissue_type == "POM") & !(Tissue_type == "Zooplankton"))
noPlankton.dat <- as.data.frame(noPlankton)
noPlankton.dat


windows(8,6)
par(mar = c(3, 5, 4, 9) + 0.5) 
boxplot(TP_Glu_Phe ~ Tissue_type + Site, noPlankton.dat, main='',xlab = "",
        ylab=expression('TP'[Glu-Phe]), ylim = c(0.5,2.3),
        col=c("#6A3D9A", "#30c026","#30c026","#6A3D9A"), cex.lab = 2.0, cex.axis=1.5, xaxt='n', 
        at = c(1,2,2,3,  5,6,6,7, 9,10,10,11,  13,14,14,15), las=1) #had to enter 16 numbers even 
          #though 8 of the columns have nothing; the 3rd number in each set is for the symbionts
abline(v=c(4,8,12), col=c("black"), lty=c(2), lwd=c(1))
axis(1, at=c(2,6,10,14),
     labels=c(expression(italic("ex situ")), "Port","Reef 1","Reef 2"),cex.axis=1.75, las=1)


#Add POM instrument uncertainty
arrows(x0=3, y0=2.0396768, x1=3, y1=2.0396768 - 0.2, code=3, angle=90, length=0.05)
arrows(x0=3, y0=2.0396768, x1=3, y1=2.0396768 + 0.2, code=3, angle=90, length=0.05)
points(3,2.0396768, pch=18, cex=3.5, col = "#7bd6f4",lwd=3)  #port

arrows(x0=7, y0=1.0772183, x1=7, y1=1.0772183 - 0.1, code=3, angle=90, length=0.05)
arrows(x0=7, y0=1.0772183, x1=7, y1=1.0772183 + 0.1, code=3, angle=90, length=0.05)
points(7,1.0772183, pch=18, cex=3.5, col = "#7bd6f4",lwd=3)  #port

arrows(x0=11, y0=1.2627475, x1=11, y1=1.2627475 - 0.2, code=3, angle=90, length=0.05)
arrows(x0=11, y0=1.2627475, x1=11, y1=1.2627475 + 0.2, code=3, angle=90, length=0.05)
points(11,1.2627475, pch=18, cex=3.5, col = "#7bd6f4",lwd=3)  #reef 1

arrows(x0=15, y0=0.8924428, x1=15, y1=0.8924428 - 0.2, code=3, angle=90, length=0.05)
arrows(x0=15, y0=0.8924428, x1=15, y1=0.8924428 + 0.2, code=3, angle=90, length=0.05)
points(15,0.8924428, pch=18, cex=3.5, col = "#7bd6f4",lwd=3)  #reef 2

#Add zooplankton instrument uncertainty
arrows(x0=2, y0=1.8233673, x1=2, y1=1.8233673 - 0.2, code=3, angle=90, length=0.05)
arrows(x0=2, y0=1.8233673, x1=2, y1=1.8233673 + 0.2, code=3, angle=90, length=0.05)
points(2,1.8233673, pch=8, cex=2.5, col = "#e41b43",lwd=2)  #port

arrows(x0=6, y0=1.7756148, x1=6, y1=1.7756148 - 0.2, code=3, angle=90, length=0.05)
arrows(x0=6, y0=1.7756148, x1=6, y1=1.7756148 + 0.2, code=3, angle=90, length=0.05)
points(6,1.7756148, pch=8, cex=2.5, col = "#e41b43",lwd=2)  #port

arrows(x0=10, y0=1.8893118, x1=10, y1=1.8893118 - 0.2, code=3, angle=90, length=0.05)
arrows(x0=10, y0=1.8893118, x1=10, y1=1.8893118 + 0.2, code=3, angle=90, length=0.05)
points(10,1.8893118, pch=8, cex=2.5, col = "#e41b43",lwd=2)  #reef 1

arrows(x0=14, y0=1.9000771, x1=14, y1=1.9000771 - 0.2, code=3, angle=90, length=0.05)
arrows(x0=14, y0=1.9000771, x1=14, y1=1.9000771 + 0.2, code=3, angle=90, length=0.05)
points(14,1.9000771, pch=8, cex=2.5, col = "#e41b43",lwd=2)  #reef 2

#Legend
par(new=TRUE, xpd = TRUE) 
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1) 
legend("topright", inset = c( -0.35, 0), legend =c('Coral Host','Symbiont','POM','Zooplankton'), 
       pch=c(15,15,18,8),  
       pt.cex=c(2.3,2.3,3.0,2.3), pt.lwd = c(3,3,3.5,2), cex=1.3, bty='n', 
       col = c("#6A3D9A", "#30c026","#7bd6f4","#e41b43"))
par(xpd = FALSE)
detach(Trophic_Position_10_6_24)


# All Tissues' Source (phe & lys) ~ Tissue & Site Boxplot ###############################
#produce a boxplot that shows each tissue type's average Phe separated by site (will just be average of host and symbiont + 1 point for heterotrophic sources)
#NOTE: SOURCE AVERAGE ASSUMES ALL POSITIVE AND NEGATIVE VALUES ARE TRUE (i.e., I did NOT take the absolute value)
class(d15N_AllAAs_10_6_24$Tissue_type)
d15N_AllAAs_10_6_24$Tissue_type <- factor (d15N_AllAAs_10_6_24$Tissue_type)
d15N_AllAAs_10_6_24$Site <- factor (d15N_AllAAs_10_6_24$Site)
d15N_AllAAs_10_6_24$Lys <- as.numeric (d15N_AllAAs_10_6_24$Lys)
d15N_AllAAs_10_6_24$Phe <- as.numeric (d15N_AllAAs_10_6_24$Phe)


#make a new column containing the average of PHE and LYS
d15N_AllAAs_10_6_24$PheLysAVE <- (d15N_AllAAs_10_6_24$Lys +  d15N_AllAAs_10_6_24$Phe)/2


attach(d15N_AllAAs_10_6_24)

#Remove plankton from the dataset since there are no replicates of those endmembers
noPlankton <- subset(d15N_AllAAs_10_6_24, !(Tissue_type == "POM") & !(Tissue_type == "Zooplankton") & !(PheLysAVE == "NA"))
noPlankton.dat <- as.data.frame(noPlankton)
noPlankton.dat

windows(8,6)
par(mar = c(3, 5, 4, 9) + 0.5) 
boxplot(PheLysAVE ~ Tissue_type + Site , noPlankton.dat, main='',xlab = "", 
        ylab=expression("Mean Source  "*delta^15*'N'*"(\u2030)"), ylim = c(-8,8),
        col=c("#6A3D9A", "#30c026","#30c026","#6A3D9A"), cex.lab = 2.0, cex.axis=1.5, xaxt='n', 
        at = c(1,2,2,3,  5,6,6,7, 9,10,10,11,  13,14,14,15), las=1) #had to enter 16 numbers even 
          #though 8 of the columns are empty; the 3rd number in each set is for the symbionts
abline(v=c(4,8,12), col=c("black"), lty=c(2), lwd=c(1))
axis(1, at=c(2,6,10,14),
     labels=c(expression(italic("ex situ")), "Port","Reef 1","Reef 2"),cex.axis=1.75, las=1)


#POM
points(3,3.53476031, pch=18, cex=3.5, col = "#7bd6f4",lwd=3)  #ex situ
points(7,5.82509839, pch=18, cex=3.5, col = "#7bd6f4",lwd=3)  #port
points(11,3.12653780, pch=18, cex=3.5, col = "#7bd6f4",lwd=3)  #reef 1
points(15,4.81722857, pch=18, cex=3.5, col = "#7bd6f4",lwd=3)  #reef 2

#Zooplankton
points(3,3.01784731, pch=8, cex=2.5, col = "#e41b43",lwd=2)  #ex situ
points(7,6.17260720, pch=8, cex=2.5, col = "#e41b43",lwd=2)  #port
points(11,3.35094313, pch=8, cex=2.5, col = "#e41b43",lwd=2)  #reef 1
points(15,3.76198719, pch=8, cex=2.5, col = "#e41b43",lwd=2)  #reef 2


#Legend
par(new=TRUE, xpd = TRUE) 
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1) #empty plot for legend
legend("topright", inset = c( -0.4, 0), legend =c('Coral Host','Endosymbiont','POM','Zooplankton'), 
       pch=c(15,15,18,8),  
       pt.cex=c(2.3,2.3,3.0,2.3), pt.lwd = c(3,3,3.5,2), cex=1.4, bty='n', 
       col = c("#6A3D9A", "#30c026","#7bd6f4","#e41b43"))
par(xpd = FALSE)
detach(d15N_AllAAs_10_6_24)


###### TROPHIC & REPRODUCTIVE ANALYSES #########################################
# 1. Create New datasets for Coral Tissues split by Male & Female ###################
FECTPM <- subset(FECTP, `M_F`=='M')
FECTPM#male SSIDs
#9 total
FECTPF <- subset(FECTP, `M_F`=='F')
FECTPF#female SSIDs
#9 total
# Summary Stats for Trophic Position ~ Fertility or M/F ####

#FERTILE CORALS
coralsF <- subset(Trophic_Position_10_6_24, Tissue_type == 'coralt' & Fertility=='Y')
coralsF
attach(coralsF)
max(TP_Glu_Phe)
#2.035557
min(TP_Glu_Phe)
#1.068965
mean(TP_Glu_Phe)
#1.465563
sd(TP_Glu_Phe)
#0.2447042
detach(coralsF)

#NONFERTILE CORALS
coralsN <- subset(Trophic_Position_10_6_24, Tissue_type == 'coralt' & Fertility=='N')
coralsN
attach(coralsN)
max(TP_Glu_Phe)
#1.997118
min(TP_Glu_Phe)
#1.083245
mean(TP_Glu_Phe)
#1.547825
sd(TP_Glu_Phe)
#0.3289486
detach(coralsN)

#MALES
attach(FECTPM)
max(TP_Glu_Phe)
#1.577604
min(TP_Glu_Phe)
#1.068965
mean(TP_Glu_Phe)
#1.382022
sd(TP_Glu_Phe)
#0.2124568
detach(FECTPM)

#FEMALES
attach(FECTPF)
max(TP_Glu_Phe)
#2.035557
min(TP_Glu_Phe)
#1.208111
mean(TP_Glu_Phe)
#1.549103
sd(TP_Glu_Phe)
#0.2576914
detach(FECTPF)



# Trophic Position ~ Fertility t-TEST #####################
class(Trophic_Position_10_6_24$Fertility)
Trophic_Position_10_6_24$Fertility <- factor(Trophic_Position_10_6_24$Fertility)
attach(Trophic_Position_10_6_24)
boxplot(TP_Glu_Phe ~ Fertility)
shapiro.test(subset(Trophic_Position_10_6_24,Fertility=='N')$TP_Glu_Phe)
shapiro.test(subset(Trophic_Position_10_6_24,Fertility=='Y')$TP_Glu_Phe)
#both normal

bartlett.test(TP_Glu_Phe ~ Fertility)
#Bartlett's K-squared = 0.79598, df = 1, p-value = 0.3723

#parametric assumptions are met

t.test (TP_Glu_Phe ~ Fertility)
# t = 0.6002, df = 8.7178, p-value = 0.5636
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.2293207  0.3938448
# sample estimates:
#   mean in group N mean in group Y 
# 1.547825        1.465563 
detach(Trophic_Position_10_6_24)

# Trophic Position ~ Sex Wilcoxon-Rank-Sum Test ##########################
class(Trophic_Position_10_6_24$M_F)
Trophic_Position_10_6_24$M_F <- factor(Trophic_Position_10_6_24$M_F)
attach(Trophic_Position_10_6_24)
boxplot(TP_Glu_Phe ~ M_F)
shapiro.test(subset(Trophic_Position_10_6_24,M_F=='M')$TP_Glu_Phe)
#data is not normal; try transformations
shapiro.test(log10(subset(Trophic_Position_10_6_24,M_F=='M')$TP_Glu_Phe))
shapiro.test(sqrt(subset(Trophic_Position_10_6_24,M_F=='M')$TP_Glu_Phe))
shapiro.test(subset(Trophic_Position_10_6_24,M_F=='F')$TP_Glu_Phe)

#data is NOT NORMAL, even after transformations 

bartlett.test(TP_Glu_Phe ~ M_F)
#Bartlett's K-squared = 0.27881, df = 1, p-value = 0.5975
#Variances are homogeneous

#Parametric assumptions are NOT met

#Non-parametric Wilcoxon test
wilcox.test (TP_Glu_Phe ~ M_F)
#W = 50, p-value = 0.4363
detach(Trophic_Position_10_6_24)

# Female Fecundity ~ Trophic Position Correlation Analysis ###########################
attach(FECTPF)
library(lme4)
library(car)
scatterplot(Cross_Mes,TP_Glu_Phe)
#assumption of linearity is not met, will try transformations
scatterplot(log10(Cross_Mes),log10(TP_Glu_Phe))
#not improved
scatterplot(sqrt(Cross_Mes),sqrt(TP_Glu_Phe))
#parametric assumptions are not met, even after transformations

#non-parametric correlation analysis
cor.test(TP_Glu_Phe,Cross_Mes,method='spearman')
# S = 82, p-value = 0.4101
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#   rho 
# 0.3166667 


detach(FECTPF)

# Male Fecundity ~ Trophic Position Correlation Analysis  ###########################
attach(FECTPM)
library(lme4)
library(car)
scatterplot(Cross_Mes,TP_Glu_Phe)
#assumption of linearity is not met, will try transformations
scatterplot(log10(Cross_Mes),log10(TP_Glu_Phe))
#not improved
scatterplot(sqrt(Cross_Mes),sqrt(TP_Glu_Phe))
#not improved

#parametric assumptions are not met, even after transformations

#non-parametric correlation analysis
#Sample size between 7 and 30, use Spearman rank correlation
cor.test(TP_Glu_Phe,Cross_Mes,method='spearman')
# S = 148.12, p-value = 0.544
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#   rho 
# -0.2343117 
detach(FECTPM)

  #FECUNDITY ~ TROPHIC POSITION GRAPHS (M/F)  ###########################
    #MALES (ALL SITES)  #################
windows(8,8)
par(mar = c(5, 6, 4, 3) + 0.5)             

# Assign site shapes
pch_values <- c(8, 19, 17, 15)  # star, circle, triangle, square

# Reorder the 'Site' column to match the legend order
FECTPM$Site <- factor(FECTPM$Site, levels = c("ex situ", "Port", "Reef 1", "Reef 2"))

site_labels <- c(expression(italic("ex situ")), "Port", "Reef 1", "Reef 2")
site_colors <- c("#333333",'#08589E', '#4EB3D3','#85C497')
site_shapes <- as.numeric(FECTPM$Site)
site_colors_mapped <- site_colors[site_shapes]

plot(FECTPM$TP_Glu_Phe, FECTPM$Cross_Mes, pch = pch_values[site_shapes],
     cex = 2,  
     xlab = "", 
     ylab = "",
     main = "Males", cex.main = 2.0,
     cex.lab = 2.1, cex.axis = 1.3, xlim = c(0.8, 2.0),
     col = site_colors_mapped,
     las =1
      )
reg<-lm(Cross_Mes ~ TP_Glu_Phe, data = FECTPM)   
abline(reg, lwd = 3)     
mtext("Ave. No. Spermaries per Polyp", side = 2, line = 4.5, cex = 2.0)
mtext(expression('TP'[Glu-Phe]), side = 1, line = 3, col='Black', cex = 2.0)
attach(FECTPM)
arrows(x0=TP_Glu_Phe-TP_prop_uncertainty, y0=Cross_Mes, x1=TP_Glu_Phe+TP_prop_uncertainty, y1=Cross_Mes, 
       code=3, angle=90, col = site_colors_mapped, length=0.05)
detach(FECTPM)
text(2.04,4275,cex = 1.2, expression(italic('S')== 148.12), pos = 2)
text(2.04,4075,cex = 1.2, expression(italic(p)== 0.54), pos = 2)
text(2.04,3875,cex = 1.2, expression(italic(rho)== -0.23), pos = 2)       


    #FEMALES (ALL SITES)  ###############################
par(mar = c(5, 6, 4, 3) + 0.5) 

# Assign site shapes
pch_values <- c(8, 19, 17, 15)  

# Reorder the 'Site' column to match the legend order
FECTPF$Site <- factor(FECTPF$Site, levels = c("ex situ", "Port", "Reef 1", "Reef 2"))

site_labels <- c(expression(italic("ex situ")), "Port", "Reef 1", "Reef 2")
site_colors <- c("#333333",'#08589E', '#4EB3D3','#85C497')
site_shapes <- as.numeric(FECTPF$Site)
site_colors_mapped <- site_colors[site_shapes]

plot(FECTPF$TP_Glu_Phe, FECTPF$Cross_Mes, pch = pch_values[site_shapes],
     cex = 2,     
     col = site_colors_mapped,
     axes = TRUE, xlab = "", ylab = "", main = "Females", cex.main = 2, cex.lab = 2.1, cex.axis = 1.3, xlim = c(1.0, 2.4),
     las =1)
mtext("Ave. No. Eggs per Polyp", side = 2, line = 4.5, cex = 2.0)
reg<-lm(Cross_Mes ~ TP_Glu_Phe, data = FECTPF) 
abline(reg, lwd = 3)
mtext(expression('TP'[Glu-Phe]), side = 1, line = 3, col='Black', cex = 2.0)
attach(FECTPF)
arrows(x0=TP_Glu_Phe-TP_prop_uncertainty, col = site_colors_mapped, y0=Cross_Mes, x1=TP_Glu_Phe+TP_prop_uncertainty, y1=Cross_Mes, code=3, angle=90, length=0.05)
detach(FECTPF)
text(2.45,54.55,cex = 1.2, expression(italic('S')== 82), pos = 2)
text(2.45,52,cex = 1.2, expression(italic(p)== 0.41), pos = 2)
text(2.45,49.5,cex = 1.2, expression(italic(rho)== 0.32), pos = 2)

#Legend for both graphs
windows(8,8)
par(new=TRUE, xpd = TRUE) 
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1) 
legend("topright", legend = site_labels, pch = pch_values, cex = 2)
par(xpd = FALSE)



###### PERECENT HETEROTROPHY ###################################################
# %Heterotrophy ~ Site One-Way ANOVA  ################
class(PercentHetero_Theoretical_10_6_24$Tissue_type)
PercentHetero_Theoretical_10_6_24$Tissue_type <- factor (PercentHetero_Theoretical_10_6_24$Tissue_type)
PercentHetero_Theoretical_10_6_24$Site <- factor (PercentHetero_Theoretical_10_6_24$Site)
PercentHetero_Theoretical_10_6_24$HOSTavg_Percent <- numeric(PercentHetero_Theoretical_10_6_24$HOSTavg_Percent)


attach(PercentHetero_Theoretical_10_6_24)
boxplot(HOSTavg_Percent ~Site)
shapiro.test (subset(PercentHetero_Theoretical_10_6_24, Site=='ex situ')$HOSTavg_Percent)
shapiro.test (subset(PercentHetero_Theoretical_10_6_24, Site=='Port')$HOSTavg_Percent)
shapiro.test (subset(PercentHetero_Theoretical_10_6_24, Site=='Reef 2')$HOSTavg_Percent)
shapiro.test (subset(PercentHetero_Theoretical_10_6_24, Site=='Reef 1')$HOSTavg_Percent)
#all data found to be normal

plot (tapply (HOSTavg_Percent, Site, mean), tapply (HOSTavg_Percent, Site,var))
bartlett.test(HOSTavg_Percent~Site)
#Bartlett's K-squared = 0.18753, df = 3, p-value = 0.9796
#parametric assumptions are met

#ANOVA
model <- aov (HOSTavg_Percent~Site)
anova(model)
#           Df  Sum Sq  Mean Sq F value   Pr(>F)   
# Site       3 0.42407 0.141357  5.5702 0.005674 **
# Residuals 21 0.53292 0.025377  


#TUKEY
TukeyHSD(model)
#                       diff        lwr         upr     p adj
# Port-ex situ   -0.30292291 -0.5427246 -0.06312120 0.0101030
# Reef 1-ex situ -0.22391123 -0.4770453  0.02922281 0.0951938
# Reef 2-ex situ -0.28439642 -0.5241981 -0.04459471 0.0163838
# Reef 1-Port     0.07901167 -0.1898597  0.34788306 0.8447857
# Reef 2-Port     0.01852649 -0.2378323  0.27488530 0.9970132
# Reef 2-Reef 1  -0.06048518 -0.3293566  0.20838621 0.9222065

library(multcompView)# install package multcompView before
multcompLetters(extract_p(TukeyHSD(model)$Site))
# Port  Reef 1  Reef 2 ex situ 
# "a"    "ab"     "a"     "b" 

detach(PercentHetero_Theoretical_10_6_24)

