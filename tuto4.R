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

    #bmi           diabetes           age             test      
#Min.   : 0.00   Min.   :0.0780   Min.   :21.00   Min.   :0.000  
#1st Qu.:27.30   1st Qu.:0.2437   1st Qu.:24.00   1st Qu.:0.000  
#Median :32.00   Median :0.3725   Median :29.00   Median :0.000  
#Mean   :31.99   Mean   :0.4719   Mean   :33.24   Mean   :0.349  
#3rd Qu.:36.60   3rd Qu.:0.6262   3rd Qu.:41.00   3rd Qu.:1.000  
#Max.   :67.10   Max.   :2.4200   Max.   :81.00   Max.   :1.000  

pairs(pima)

attach(pima)
#changing all zeros into NA
glucose[glucose == 0] <- NA
diastolic[diastolic == 0] <-NA
bmi[bmi == 0] <-NA
triceps[triceps == 0] <-NA
insulin[insulin == 0] <-NA

pima2 <- pima
summary(pima2)
attach(pima2)
plot(pima2)


