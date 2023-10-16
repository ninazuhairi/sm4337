#Q1
bright <- c(59.88,60.12,60.88,60.98,59.9,
            59.87,60.32,60.42,59.99,60.12,
            60.83,60.87,60.56,61,60.5,
            61.01,60.87,60.69,60.53,60.63)
operator <- c("A","A","A","A","A",
              "B","B","B","B","B",
              "C","C","C","C","C",
              "D","D","D","D","D")
model <- lm (bright ~ operator)
summary(model)

#Residuals:
#  Min     1Q Median     3Q    Max 
#-0.472 -0.220 -0.040  0.194  0.628 

#Coefficients:
#             Estimat Std. Error t value   Pr(>|t|)    
#(Intercept)  60.3520     0.1454 415.136   <2e-16 ***
#operatorB    -0.2080     0.2056  -1.012   0.3267    
#operatorC     0.4000     0.2056   1.946   0.0695 .  
#operatorD     0.3940     0.2056   1.916   0.0734 .  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.3251 on 16 degrees of freedom
#Multiple R-squared:  0.4464,	Adjusted R-squared:  0.3426 
#F-statistic:   4.3 on 3 and 16 DF,  p-value: 0.02097

#   Ho:μA=μB=μC=μD
#   Ho:μA=/=μB=/=μC=/=μD
#      μA=/=μB=μC=μD     since p-value is 0.02097 < 0.05, we reject Ho

anova (model)

#Analysis of Variance Table

#Response: bright
#Df Sum Sq Mean Sq F value  Pr(>F)  
#operator   3 1.3633 0.45442  4.3001 0.02097 *
#  Residuals 16 1.6908 0.10567                  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

TukeyHSD(aov(bright~operator))

#Tukey multiple comparisons of means
#95% family-wise confidence level

#Fit: aov(formula = bright ~ operator)

#$operator
#diff         lwr       upr     p adj
#B-A -0.208 -0.79621621 0.3802162 0.7450627
#C-A  0.400 -0.18821621 0.9882162 0.2489941
#D-A  0.394 -0.19421621 0.9822162 0.2603189
#C-B  0.608  0.01978379 1.1962162 0.0415316
#D-B  0.602  0.01378379 1.1902162 0.0439458
#D-C -0.006 -0.59421621 0.5822162 0.9999907

#Q2
breaks <- c(26,30,54,25,70,52,51,26,67,
            27,14,29,19,29,31,41,20,44,
            18,21,29,17,12,18,35,30,36,
            42,26,19,16,39,28,21,39,29,
            36,21,24,18,10,43,28,15,26,
            20,21,24,17,13,15,15,16,28)
wool <- c("A","A","A","A","A","A","A","A","A",
          "B","B","B","B","B","B","B","B","B",
          "A","A","A","A","A","A","A","A","A",
          "B","B","B","B","B","B","B","B","B",
          "A","A","A","A","A","A","A","A","A",
          "B","B","B","B","B","B","B","B","B")
tension <- c("L","L","L","L","L","L","L","L","L",
             "M","M","M","M","M","M","M","M","M",
             "H","H","H","H","H","H","H","H","H",
             "L","L","L","L","L","L","L","L","L",
             "M","M","M","M","M","M","M","M","M",
             "H","H","H","H","H","H","H","H","H")
wool <- as.factor(wool)
is.factor(wool)

data_1 <- data.frame(wool,tension,breaks)

interaction.plot(wool,tension,breaks)
interaction.plot(tension,wool,breaks)

model <- lm(breaks~wool+tension+wool*tension)
summary(model)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-30.889  -9.750  -2.222   6.139 145.111 

#Coefficients:
#                Estimate Std. Error t value Pr(>|t|)    
#(Intercept)      42.889      8.248   5.200 4.07e-06 ***
#woolB           -24.111     11.664  -2.067   0.0441 *  
#tensionL          1.667     11.664   0.143   0.8870    
#tensionM        -18.222     11.664  -1.562   0.1248    
#woolB:tensionL    8.333     16.496   0.505   0.6157    
#woolB:tensionM   27.667     16.496   1.677   0.1000    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 24.74 on 48 degrees of freedom
#Multiple R-squared:  0.1389,	Adjusted R-squared:  0.04919 
#F-statistic: 1.548 on 5 and 48 DF,  p-value: 0.1928
 
#even if the parent model (in this case, wool) is insignificant, we do not eliminate it

anova(model)

#Analysis of Variance Table

#Response: breaks
#              Df  Sum Sq Mean Sq F value Pr(>F)  
#wool          1  1980.2 1980.17  3.2343 0.0784 .
#tension       2   946.7  473.35  0.7731 0.4672  
#wool:tension  2  1813.0  906.50  1.4806 0.2377  
#Residuals    48 29387.8  612.25                 
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

model_2 <- lm(breaks^{10}~wool+tension+wool*tension)
anova(model_2)

model_3 <- lm(1/breaks~wool+tension)
anova(model_3)

model_4 <- lm(1/breaks~tension)
anova(model_4)

TukeyHSD(aov(breaks~wool+tension+wool*tension))

#Q3
length <- c(4.2,11.5,7.3,5.8,6.4,10,11.2,11.2,5.2,7,
        15.2,21.5,17.6,9.7,14.5,10,8.2,9.4,16.5,9.7,
        16.5,16.5,15.2,17.3,22.5,17.3,13.6,14.5,18.8,15.5,
        19.7,23.3,23.6,26.4,20,25.2,25.8,21.2,14.5,27.3,
        23.6,18.5,33.9,25.5,26.4,32.5,26.7,21.5,23.3,29.5,
        25.5,26.4,22.4,24.5,24.8,30.9,26.4,27.3,29.4,23)

suppl <- c("vc","vc","vc","vc","vc","vc","vc","vc","vc","vc",
           "oj","oj","oj","oj","oj","oj","oj","oj","oj","oj",
           "vc","vc","vc","vc","vc","vc","vc","vc","vc","vc",
           "oj","oj","oj","oj","oj","oj","oj","oj","oj","oj",
           "vc","vc","vc","vc","vc","vc","vc","vc","vc","vc",
           "oj","oj","oj","oj","oj","oj","oj","oj","oj","oj")

dose <- c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,
          1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
          2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,
          0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,
          1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
          2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0)

suppl <- as.factor(suppl)
data_2 <- data.frame(suppl,dose,length)

mod_2 <- lm(length~suppl+dose+suppl*dose)
plot(mod_2)

plot(length~suppl+dose+suppl*dose)
