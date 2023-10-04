
library(lattice)
library(faraway)
data("ratdrink")

head(ratdrink)
attach(ratdrink)
xyplot(wt~weeks|subject,type="l")
xyplot(wt~weeks|treat,type="l",groups=subject)

library(lme4)
ratmod<-lmer(wt~weeks*treat+weeks+treat+(weeks|subject))
summary(ratmod)

#Linear mixed model fit by REML ['lmerMod']
#Formula: wt ~ weeks * treat + weeks + treat + (weeks | subject)

#REML criterion at convergence: 878.7

#Scaled residuals: 
#  Min       1Q   Median       3Q      Max 
#-1.83136 -0.54991  0.04003  0.58230  2.03660 

#Random effects:
#  Groups   Name        Variance Std.Dev. Corr 
#subject  (Intercept) 32.49    5.700         
#weeks       14.14    3.760    -0.13
#Residual             18.90    4.348         
#Number of obs: 135, groups:  subject, 27

#Fixed effects:
#                       Estimate Std. Error t value
#(Intercept)            52.8800     2.0937  25.256
#weeks                  26.4800     1.2661  20.915
#treatthiouracil         4.7800     2.9610   1.614
#treatthyroxine         -0.7943     3.2628  -0.243
#weeks:treatthiouracil  -9.3700     1.7905  -5.233
#weeks:treatthyroxine    0.6629     1.9730   0.336

#Correlation of Fixed Effects:
#  (Intr) weeks  trtthr trtthy wks:trtthr
#weeks       -0.250                                
#treatthircl -0.707  0.177                         
#treatthyrxn -0.642  0.160  0.454                  
#wks:trtthrc  0.177 -0.707 -0.250 -0.113           
#wks:trtthyr  0.160 -0.642 -0.113 -0.250  0.454    

confint(ratmod)

#                          2.5 %     97.5 %
#.sig01                  3.4506344  7.6654748
#.sig02                 -0.5261030  0.3794203
#.sig03                  2.6064121  4.8687653
#.sigma                  3.7555591  5.1142342
#(Intercept)            48.8692859 56.8907140
#weeks                  24.0547424 28.9052555
#treatthiouracil        -0.8920062 10.4520061
#treatthyroxine         -7.0445321  5.4559606
#weeks:treatthiouracil -12.7998322 -5.9401707
#weeks:treatthyroxine   -3.1166339  4.4423449

#The weights of rats on thiouracil are 4.78 times higher compared to control
#The weights of rats on thyroxine are 0.79 times less than the control

shapiro.test(residuals(ratmod))

#Shapiro-Wilk normality test

#data:  residuals(ratmod)
#W = 0.99276, p-value = 0.7224

#we do not reject H0, the residuals are normal

plot(fitted(ratmod),residuals(ratmod),xlab = "Fitted", ylab = "Residuals")

qqmath(~resid(ratmod)|treat)

xyplot(resid(ratmod)~fitted(ratmod)|treat,layout=c(3,1),xlab = "Fitted",ylab = "Residuals")
