# Train a Multiple Linear Regression model given a dataset (input CSV file)
#
# background on MLR: https://www.investopedia.com/terms/m/mlr.asp
# R example: https://www.tutorialspoint.com/r/r_multiple_regression.htm

# read data in
raw <- read.csv("data/moldescs_scaled.csv", header = T, sep = ",")
# only keep interesting columns
train <- raw[,c("score","MolW.scaled","cLogP.scaled","RotB.scaled")]

molw <- train[,c("MolW.scaled")]
molw_mean <- mean(molw)
molw_sd <- sd(molw)
print(c("MolW", molw_mean, molw_sd))

clogp <- train[,c("cLogP.scaled")]
clogp_mean <- mean(clogp)
clogp_sd <- sd(clogp)
print(c("cLogP", clogp_mean, clogp_sd))

rotb <- train[,c("RotB.scaled")]
rotb_mean <- mean(rotb)
rotb_sd <- sd(rotb)
print(c("RotB", rotb_mean, rotb_sd))

# train model
# !!! ALL DEPENDANT VARIABLES MUST HAVE BEEN SCALED BEFORE !!!
model <- lm(score ~ MolW.scaled + cLogP.scaled + RotB.scaled, data = train)

# Show the model.
a <- coef(model)[1]
b <- coef(model)[2]
c <- coef(model)[3]
d <- coef(model)[4]
print(c(a, b, c, d))
