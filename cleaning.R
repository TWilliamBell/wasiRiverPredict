setwd("C:/Users/twill/Desktop/Folder/R Projects/Wasi Prediction")

# ## Read in the Chippewa creek data
# dat <- read.csv("Data/chippewaFinalBig.csv", skip = 9)
# dat$Date..EST. <- as.POSIXct(dat$Date..EST., "%Y-%m-%d %H:%M:%S", tz = "EST")
# 
# chippewaClean <- data.frame("Time" = dat$Date..EST.,
#                             "CurrentLevel" = dat$`Value..m³.s.`)
# 
# pointsAvail <- 570    
# chippewaClean <- cbind(chippewaClean, 
#                        matrix(NA, ncol = pointsAvail, nrow = nrow(chippewaClean)))
# 
# colnames(chippewaClean)[3:ncol(chippewaClean)] <- seq(5, pointsAvail*5, by = 5)
# 
# ## Include a bunch of columns that are Chippewa Creek certain amounts of time in the past
# for (i in seq_len(nrow(dat))) {
#   for (j in 1:pointsAvail) {
#     if (i-j < 1) next
#     if (difftime(dat$Date..EST.[i-j],
#                  dat$Date..EST.[i], units = "mins") < -pointsAvail*5) {
#       next
#     }
#     chippewaClean[i, as.character(-difftime(dat$Date..EST.[i-j],
#                                  dat$Date..EST.[i], units = "mins"))] <-
#       dat$`Value..m³.s.`[i-j]
#   }
# }
# 
# chippewaClean <- na.omit(chippewaClean)
# 
# ## Write to file
# write.csv(chippewaClean, "Data/chippewaDelay.csv", row.names = F)
# 
# ## Read in the Wasi River data
# wasiDat <- read.csv("Data/wasiFinalBig.csv", skip = 9)
# 
# wasiDat <- wasiDat[c(1, 3)]
# wasiDat$OldLevelsWasi <- NA
# wasiDat$OlderLevelsWasi <- NA
# 
# ## Add in columns for the Wasi River level 2 and 3 hours in the past
# colnames(wasiDat) <- c("Time", "CurrentLevelsWasi", "OldLevelsWasi", "OlderLevelsWasi")
# wasiDat$Time <- as.POSIXct(wasiDat$Time, "%Y-%m-%d %H:%M:%S", tz = "EST")
# 
# for (i in seq_len(nrow(wasiDat))) {
#   for (j in 1:100) {
#     if (i-j < 1) next
#     if (as.double(difftime(wasiDat$Time[i-j], wasiDat$Time[i], units = "mins")) == -120) {
#       wasiDat$OldLevelsWasi[i] <- wasiDat$CurrentLevelsWasi[i-j]
#     }
#     if (as.double(difftime(wasiDat$Time[i-j], wasiDat$Time[i], units = "mins")) == -180) {
#       wasiDat$OlderLevelsWasi[i] <- wasiDat$CurrentLevelsWasi[i-j]
#       break
#     }
#   }
# }
# 
# ## Save that data to file
# write.csv(wasiDat, "Data/wasiDatDelay.csv", row.names = F)
# 
# ## Join the Wasi and Chippewa data so we can use both for prediction
# dat <- dplyr::inner_join(wasiDat, chippewaClean)
# 
# ## Save the Chippewa data separately for plots
# chipDatFull <- dat$CurrentLevel
# 
# ## To avoid problems from multicollinearity, use sparse Chippewa data
# dat <- dat[c(1:4, seq(29, 10*floor(pointsAvail/10), by = 30))]
# dat <- na.omit(dat)
# 
# write.csv(dat, "Data/cleanedForModellingData.csv", row.names = F)

## If you've already done the above steps, just read in the data
dat <- read.csv("Data/cleanedForModellingData.csv")

## Test/train split point
splitPoint <- 100000

## Rename columns something formula-friendly
colnames(dat)[5:ncol(dat)] <- paste0("min", colnames(dat)[5:ncol(dat)])

## A model using all of the remaining features and a robust linear regression fitting
model <- MASS::rlm(as.formula(paste0("CurrentLevelsWasi ~ OldLevelsWasi + OlderLevelsWasi +", paste(
  colnames(dat)[5:ncol(dat)], collapse = " + "))), dat[1:splitPoint, ])

## A model using just Wasi data, used as a 'base case'
modelBasic <- MASS::rlm(as.formula("CurrentLevelsWasi ~ OldLevelsWasi + OlderLevelsWasi"),
                        dat[1:splitPoint, ])

## For our non-linear model, we incorporate the difference between the 2 and 3 hours ago Wasi
## data as a distinct feature that can be used
dat$Der <- dat$OldLevelsWasi-dat$OlderLevelsWasi

## A model using MARS regression and all features, the current state of the art
modelDeluxe <- earth::earth(as.formula(paste0("CurrentLevelsWasi ~ OldLevelsWasi + OlderLevelsWasi +", paste(
  colnames(dat)[5:ncol(dat)], collapse = " + "))), na.omit(dat[1:splitPoint, ]), thresh = 0)

## Predict the test data using the MARS model
predictions <- predict(modelDeluxe, dat[(splitPoint+1):nrow(dat), ])

## Make the test data into time series objects for ease of plotting
trueDat <- as.ts(dat$CurrentLevelsWasi[(splitPoint+1):nrow(dat)])
predDat <- as.ts(predictions)
chipDat <- as.ts(chipDatFull[(splitPoint+1):nrow(dat)])

## Predict the train data using the MARS model
trainPredictions <- predict(modelDeluxe, dat[1:splitPoint, ])

## Make the training data into time series objects for ease of plotting
trueTrainDat <- as.ts(dat$CurrentLevelsWasi[1:splitPoint])
predTrainDat <- as.ts(trainPredictions)
chipTrainDat <- as.ts(chipDatFull[1:splitPoint])

## Plot training data and training predictions
ts.plot(trueTrainDat, predTrainDat, chipTrainDat, gpars = list(col = c("red", "blue", "green")), 
        ylim = c(0, 30))

## Plot test data and test predictions
ts.plot(trueDat, predDat, chipDat, as.ts(c(predDat[c(24:length(predDat), 1:23)])),
        gpars = list(col = c("red", "blue", "green", "orange")), ylim = c(0, 15))

## Plot specifically an interesting spike in the test data
ts.plot(as.ts(trueDat[4000:4500]), as.ts(predDat[4000:4500]), as.ts(chipDat[4000:4500]), 
        #as.ts(c(predDat[c(24:length(predDat), 1:23)])[4000:4500]),
        gpars = list(col = c("red", "blue", "green"
                             #, "orange"
                             )), ylim = c(0, 6))

## Plot the true test data against the predictions with a 1:1 reference line
plot(trueDat, predDat, xlab = "True Values", ylab = "Predicted Values", xlim = c(0, 15), 
     ylim = c(0, 15))
abline(0, 1, col = "red")

## Plot the true train data against the predictions with a 1:1 reference line
plot(trueTrainDat, predTrainDat, xlab = "True Training Values", ylab = "True Predicted Values")
abline(0, 1, col = "red")

## Summarize the MARS model
summary(modelDeluxe)

## Test and train adjusted R squared for the MARS model
print(1-var(trueDat-predDat)/var(trueDat))
print(1-var(modelDeluxe$residuals)/var(trueTrainDat))

## Test and train adjusted R squared for the base case (Wasi data only) model
print(1-var(modelBasic$residuals)/var(trueTrainDat))
print(1-var(trueDat-predict(modelBasic, dat[(splitPoint+1):nrow(dat), ]))/var(trueDat))

## Test and train adjusted R squared for the robust linear regression model
print(1-var(model$residuals)/var(trueTrainDat))
print(1-var(trueDat-predict(model, dat[(splitPoint+1):nrow(dat), ])))
#plot(chipTrainDat, trueTrainDat, xlab = "Chippewa Flow Rates", ylab = "Wasi Flow Rates")
#abline(0, 4.77, col = "red")

## Save the models
saveRDS(model, "Analysis/model.rds")
saveRDS(modelBasic, "Analysis/modelBasic.rds")
saveRDS(modelDeluxe, "Analysis/modelDeluxe.rds")
