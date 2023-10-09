#Q1
library(faraway)
data(pima)
summary(pima)

   #pregnant         glucose        diastolic         triceps         insulin     
#Min.   : 0.000   Min.   :  0.0   Min.   :  0.00   Min.   : 0.00   Min.   :  0.0  
#1st Qu.: 1.000   1st Qu.: 99.0   1st Qu.: 62.00   1st Qu.: 0.00   1st Qu.:  0.0  
#Median : 3.000   Median :117.0   Median : 72.00   Median :23.00   Median : 30.5  
#Mean   : 3.845   Mean   :120.9   Mean   : 69.11   Mean   :20.54   Mean   : 79.8  
#3rd Qu.: 6.000   3rd Qu.:140.2   3rd Qu.: 80.00   3rd Qu.:32.00   3rd Qu.:127.2  
#Max.   :17.000   Max.   :199.0   Max.   :122.00   Max.   :99.00   Max.   :846.0  

    #bmi           diabetes           age                  
#Min.   : 0.00   Min.   :0.0780   Min.   :21.00   
#1st Qu.:27.30   1st Qu.:0.2437   1st Qu.:24.00   
#Median :32.00   Median :0.3725   Median :29.00   
#Mean   :31.99   Mean   :0.4719   Mean   :33.24    
#3rd Qu.:36.60   3rd Qu.:0.6262   3rd Qu.:41.00   
#Max.   :67.10   Max.   :2.4200   Max.   :81.00    

#other variables except pregnancy are not suppose to have minimum value of 0

pairs(pima)

attach(pima)
#changing all zeros into NA
glucose[glucose == 0] <- NA
diastolic[diastolic == 0] <-NA
bmi[bmi == 0] <-NA
triceps[triceps == 0] <-NA
insulin[insulin == 0] <-NA

summary(pima2)
attach(pima2)
plot(pima2)

mod<-glm(cbind(test,1-test)~.,family=binomial,data=pima2)
summary(mod)

install.packages("ResourceSelection")
library(ResourceSelection)

hoslem.test(mod$y,fitted(mod))
#Ho: there is no difference between predicted and observed values (model is well specified)
#H1: there is a difference between predicted and observed values (model is not well specified)

#since p-value is 0.9335 > 0.05, we do not reject Ho and conclude at 5% level that there is 
#no significant evidence that the model is well specified

x0<-c(1,1,99,64,22,76,27,0.25,25)
#the first '1' is for the intercepts cause the intercepts will not change

eta0<-sum(x0*coef(mod))
ilogit(eta0)
#p=0.0457, predicted value = 4.57%
#this implies that the predicted probability that the women is positive for diabetes with 
#the predictor variables specified is about 4.57%

pred<-predict(mod,newdata = data.frame(pregnant = 1, glucose = 99, diastolic = 64, triceps = 22,
                                       insulin = 76, bmi = 27, diabetes = 0.25, age = 25),se=T)
pred
ilogit(c(pred$fit-1.96*pred$se.fit, pred$fit+1.96*pred$se.fit))
#95% C.I. = (2.50%, 8.21%)

#Q2
data(wbca)
summary(wbca)

#     Class            Adhes            BNucl            Chrom            Epith       
#Min.   :0.0000   Min.   : 1.000   Min.   : 1.000   Min.   : 1.000   Min.   : 1.000  
#1st Qu.:0.0000   1st Qu.: 1.000   1st Qu.: 1.000   1st Qu.: 2.000   1st Qu.: 2.000  
#Median :1.0000   Median : 1.000   Median : 1.000   Median : 3.000   Median : 2.000  
#Mean   :0.6505   Mean   : 2.816   Mean   : 3.542   Mean   : 3.433   Mean   : 3.231  
#3rd Qu.:1.0000   3rd Qu.: 4.000   3rd Qu.: 6.000   3rd Qu.: 5.000   3rd Qu.: 4.000  
#Max.   :1.0000   Max.   :10.000   Max.   :10.000   Max.   :10.000   Max.   :10.000  
#     Mitos            NNucl            Thick            UShap            USize      
#Min.   : 1.000   Min.   : 1.000   Min.   : 1.000   Min.   : 1.000   Min.   : 1.00  
#1st Qu.: 1.000   1st Qu.: 1.000   1st Qu.: 2.000   1st Qu.: 1.000   1st Qu.: 1.00  
#Median : 1.000   Median : 1.000   Median : 4.000   Median : 1.000   Median : 1.00  
#Mean   : 1.604   Mean   : 2.859   Mean   : 4.436   Mean   : 3.204   Mean   : 3.14  
#3rd Qu.: 1.000   3rd Qu.: 4.000   3rd Qu.: 6.000   3rd Qu.: 5.000   3rd Qu.: 5.00  
#Max.   :10.000   Max.   :10.000   Max.   :10.000   Max.   :10.000   Max.   :10.00  

mod2<-glm(cbind(Class,1-Class)~.,family=binomial,data=wbca)
summary(mod2)

#Deviance Residuals: 
#   Min        1Q    Median        3Q       Max  
#-2.48282  -0.01179   0.04739   0.09678   3.06425  

#Coefficients:
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept) 11.16678    1.41491   7.892 2.97e-15 ***
#Adhes       -0.39681    0.13384  -2.965  0.00303 ** 
#BNucl       -0.41478    0.10230  -4.055 5.02e-05 ***
#Chrom       -0.56456    0.18728  -3.014  0.00257 ** 
#Epith       -0.06440    0.16595  -0.388  0.69795    
#Mitos       -0.65713    0.36764  -1.787  0.07387 .  
#NNucl       -0.28659    0.12620  -2.271  0.02315 *  
#Thick       -0.62675    0.15890  -3.944 8.01e-05 ***
#UShap       -0.28011    0.25235  -1.110  0.26699    
#USize        0.05718    0.23271   0.246  0.80589    

#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 881.388  on 680  degrees of freedom
#Residual deviance:  89.464  on 671  degrees of freedom
#AIC: 109.46

#Number of Fisher Scoring iterations: 8

mod3<-glm(cbind(Class,1-Class)~Adhes+BNucl+Chrom+Mitos+NNucl+Thick+UShap,binomial,wbca)
mod3

#Coefficients:
#(Intercept)    Adhes        BNucl        Chrom        Mitos        NNucl  
#11.0333        -0.3984      -0.4192      -0.5679      -0.6456      -0.2915  
#Thick          UShap  
#-0.6216        -0.2541  

#Degrees of Freedom: 680 Total (i.e. Null);  673 Residual
#Null Deviance:	    881.4 
#Residual Deviance: 89.66 	AIC: 105.7

x2<-c(1,1,1,3,1,1,4,1)
eta0<-sum(x2*coef(mod3))
ilogit(eta0)
#[1] 0.9921115

pred<-predict(mod3,newdata = data.frame(Adhes = 1, BNucl = 1, Chrom = 3, Mitos = 1, NNucl = 1,
                                        Thick = 4, UShap = 1),se=T)
ilogit(c(pred$fit-1.96*pred$se.fit, pred$fit+1.96*pred$se.fit))
#95% C.I. = (0.9757467, 0.9974629)
