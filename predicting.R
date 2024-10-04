setwd("C:/Users/twill/Desktop/Folder/R Projects/Wasi Prediction")

dat <- read.csv("Data/newChippewaDat.csv", skip = 9)
dat$Date..EST. <- as.POSIXct(dat$Date..EST., "%Y-%m-%d %H:%M:%S", tz = "EST")

chippewaClean <- data.frame("Time" = dat$Date..EST.,
                            "CurrentLevel" = dat$`Value..m³.s.`)

pointsAvail <- 570    
chippewaClean <- cbind(chippewaClean, 
                       matrix(NA, ncol = pointsAvail, nrow = nrow(chippewaClean)))

colnames(chippewaClean)[3:ncol(chippewaClean)] <- seq(5, pointsAvail*5, by = 5)

for (i in seq_len(nrow(dat))) {
  for (j in 1:pointsAvail) {
    if (i-j < 1) next
    if (difftime(dat$Date..EST.[i-j],
                 dat$Date..EST.[i], units = "mins") < -pointsAvail*5) {
      next
    }
    chippewaClean[i, as.character(-difftime(dat$Date..EST.[i-j],
                                            dat$Date..EST.[i], units = "mins"))] <-
      dat$`Value..m³.s.`[i-j]
  }
}

chippewaClean <- na.omit(chippewaClean)

#write.csv(chippewaClean, "Data/chippewaDelay.csv", row.names = F)

wasiDat <- read.csv("Data/newWasiDat.csv", skip = 9)

wasiDat <- wasiDat[c(1, 3)]
wasiDat$OldLevelsWasi <- NA
wasiDat$OlderLevelsWasi <- NA

colnames(wasiDat) <- c("Time", "CurrentLevelsWasi", "OldLevelsWasi", "OlderLevelsWasi")
wasiDat$Time <- as.POSIXct(wasiDat$Time, "%Y-%m-%d %H:%M:%S", tz = "EST")

for (i in seq_len(nrow(wasiDat))) {
  for (j in 1:100) {
    if (i-j < 1) next
    if (as.double(difftime(wasiDat$Time[i-j], wasiDat$Time[i], units = "mins")) == -120) {
      wasiDat$OldLevelsWasi[i] <- wasiDat$CurrentLevelsWasi[i-j]
    }
    if (as.double(difftime(wasiDat$Time[i-j], wasiDat$Time[i], units = "mins")) == -180) {
      wasiDat$OlderLevelsWasi[i] <- wasiDat$CurrentLevelsWasi[i-j]
      break
    }
  }
}

#write.csv(wasiDat, "Data/wasiDatDelay.csv", row.names = F)

dat <- dplyr::inner_join(wasiDat, chippewaClean)

model <- readRDS("Analysis/model.rds")

chipDatFull <- dat$CurrentLevel

dat <- dat[c(1:4, seq(17, 10*floor(pointsAvail/10), by = 30))]
dat <- na.omit(dat)

colnames(dat)[5:ncol(dat)] <- paste0("min", colnames(dat)[5:ncol(dat)])

colnames(dat)[2:4] <- c("OldLevelsWasi", "OlderLevelsWasi", 
                        "AncientLevelsWasi")

colnames(dat)[5:ncol(dat)] <- paste0("min", as.integer(gsub("min", "", 
                                            colnames(dat)[5:ncol(dat)]))+60)
predictions <- as.ts(predict(model, dat))

ts.plot(predictions, as.ts(dat$OldLevelsWasi), 
          as.ts(chipDatFull),
        gpars = list(col = c("blue", "red", "green")))

ts.plot(as.ts(tail(predictions, 100)), as.ts(tail(dat$OldLevelsWasi, 100)), 
            as.ts(tail(chipDatFull, 100)),
        gpars = list(col = c("blue", "red", "green")))

testRSq <- 1-var(predictions[1:(length(predictions)-23)]-dat$OldLevelsWasi[24:nrow(dat)])/
                var(dat$OldLevelsWasi)
