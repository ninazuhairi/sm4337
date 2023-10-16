#Q1
#Principal Component Analysis
install.packages("psych")
library(psych)
data(bfi)
head(bfi)

summary(bfi)
is.na(bfi)
sum(is.na(bfi))
#[1] 731

bfi2 <- na.omit(bfi[1:25])
bfi.pca<-prcomp(bfi2[1:25],center=TRUE,scale=TRUE) 
#centering and scaling to standardize and avoid skewness

summary(bfi.pca)
#Importance of components:
  
#                         PC1    PC2     PC3     PC4     PC5     PC6     PC7     PC8
#Standard deviation     2.2659 1.6589 1.46380 1.36100 1.24425 1.03614 0.91626 0.89398
#Proportion of Variance 0.2054 0.1101 0.08571 0.07409 0.06193 0.04294 0.03358 0.03197
#Cumulative Proportion  0.2054 0.3155 0.40116 0.47525 0.53718 0.58012 0.61370 0.64567

#                         PC9    PC10    PC11    PC12    PC13    PC14    PC15    PC16
#Standard deviation     0.84793 0.82951 0.82242 0.80734 0.78946 0.77237 0.75039 0.73709
#Proportion of Variance 0.02876 0.02752 0.02705 0.02607 0.02493 0.02386 0.02252 0.02173
#Cumulative Proportion  0.67443 0.70195 0.72901 0.75508 0.78001 0.80387 0.82640 0.84813

#                         PC17    PC18    PC19    PC20    PC21    PC22    PC23    PC24
#Standard deviation     0.71730 0.70321 0.69472 0.67002 0.65067 0.63299 0.62274 0.61795
#Proportion of Variance 0.02058 0.01978 0.01931 0.01796 0.01693 0.01603 0.01551 0.01527
#Cumulative Proportion  0.86871 0.88849 0.90779 0.92575 0.94269 0.95871 0.97422 0.98950

#                         PC25
#Standard deviation     0.5124
#Proportion of Variance 0.0105
#Cumulative Proportion  1.0000

#the s.d. are decreasing

plot(bfi.pca,type="l")
#between components no. 5 and 6 has the biggest drop before the line going near horizontal
#so retain 5 components
#the first five PCs accounts for 53.72% (cumulative proportion of PC5) of the variance of the data

print(bfi.pca)
#check the groupings of the loadings/ highest loadings

PC1:
E1  0.19552510 
E2  0.28337131 
E3 -0.24841978 
E4 -0.27470650 
E5 -0.24401690
#PC1 corresponds to extroversion (high components in Es)

PC2:
N2  0.39341608 
N3  0.40533943 
N4  0.29018347 
N5  0.30149001
#PC2 corresponds to neuroticism (high loadings in Ns)

PC3:
C1 -0.357012772 
C2 -0.345690611 
C3 -0.269473465 
C4  0.326097064  
C5  0.201094118

PC4:
O1  0.30870336  
O2 -0.33710067  
O3  0.34769402 
O4  0.21048383
O5 -0.37272166 

PC5:
A1  0.490281326
A2 -0.333324199
A3 -0.255207890
A4 -0.189163026
A5 -0.150969480 

#Q2
#Factor Analysis
library(faraway)
data(iris)
head(iris)

summary(iris)
is.na(iris)
sum(is.na(iris))
#[1] 0

cortest.bartlett(iris[1:4])
#$chisq
#[1] 706.9592

#$p.value
#[1] 1.92268e-149

#$df
#[1] 6

#since the p-value is <0.05, we can proceed with factor analysis

KMO(iris[1:4])

#Overall MSA =  0.54
#MSA for each item = 
#Sepal.Length  Sepal.Width Petal.Length  Petal.Width 
#0.58         0.27         0.53         0.63 

#0.54 is marginal to the suggested value 0.6 hence, we can proceed with FA

scree(iris[1:4])
#between 2 and 3

fa.parallel(iris[1:4])
#parallel analysis suggests that the no. of factors = 3 and 
#no. of components = 1 

install.packages("GPArotation")
library(GPArotation)
iris.fa <- fa(iris[1:4],nfactors=3)
fa.diagram(iris.fa)
