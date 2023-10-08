library(faraway)
data(pima)
summary(pima)
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
