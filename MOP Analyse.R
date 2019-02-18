#read in data and drop every second row to only get the driven distance
MOP <- read.csv("C:/Users/Martin/Desktop/Masterthesis/DrivingProfiles_JHO_6466_Vollzeit.csv", header = FALSE, sep = ",", dec = ".", stringsAsFactors = FALSE)[c(TRUE, FALSE), ]

#rename the rows
rownames(MOP) <- 1:nrow(MOP)

#convert factors to numeric values
MOP[, 1:ncol(MOP)] <- sapply(MOP[, 1:ncol(MOP)], as.numeric)

#split data into weekdays, one day has 96 oberservations
MOP_day1 <- MOP[, 1:96]
MOP_day2 <- MOP[, 97:192]
MOP_day3 <- MOP[, 193:288]
MOP_day4 <- MOP[, 289:384]
MOP_day5 <- MOP[, 385:480]
MOP_day6 <- MOP[, 481:576]
MOP_day7 <- MOP[, 577:672]

#if the vehicle was driven it is indicated by 1, 0 otherwise
MOP_day1[MOP_day1 > 0] <- 1
MOP_day2[MOP_day2 > 0] <- 1
MOP_day3[MOP_day3 > 0] <- 1
MOP_day4[MOP_day4 > 0] <- 1
MOP_day5[MOP_day5 > 0] <- 1
MOP_day6[MOP_day6 > 0] <- 1
MOP_day7[MOP_day7 > 0] <- 1

#new dataframes for the hourly aggregation
MOP_day1_hourly <- as.data.frame(matrix(0, ncol = 24, nrow = nrow(MOP)))
MOP_day2_hourly <- as.data.frame(matrix(0, ncol = 24, nrow = nrow(MOP)))
MOP_day3_hourly <- as.data.frame(matrix(0, ncol = 24, nrow = nrow(MOP)))
MOP_day4_hourly <- as.data.frame(matrix(0, ncol = 24, nrow = nrow(MOP)))
MOP_day5_hourly <- as.data.frame(matrix(0, ncol = 24, nrow = nrow(MOP)))
MOP_day6_hourly <- as.data.frame(matrix(0, ncol = 24, nrow = nrow(MOP)))
MOP_day7_hourly <- as.data.frame(matrix(0, ncol = 24, nrow = nrow(MOP)))

#from 15 minutes to hourly
i = 1
start = 1
end = 4
while (i < 25) {
  if (i != 1) {
    start <- start + 4
    end <- end + 4
  }
  MOP_day1_hourly[, i] <- rowSums(MOP_day1[, start:end])
  MOP_day2_hourly[, i] <- rowSums(MOP_day2[, start:end])
  MOP_day3_hourly[, i] <- rowSums(MOP_day3[, start:end])
  MOP_day4_hourly[, i] <- rowSums(MOP_day4[, start:end])
  MOP_day5_hourly[, i] <- rowSums(MOP_day5[, start:end])
  MOP_day6_hourly[, i] <- rowSums(MOP_day6[, start:end])
  MOP_day7_hourly[, i] <- rowSums(MOP_day7[, start:end])
  i <- i+1
}

#if the vehicle was driven in one hour it is indicated by 1, 0 otherwise
MOP_day1_hourly[MOP_day1_hourly > 0] <- 1
MOP_day2_hourly[MOP_day2_hourly > 0] <- 1
MOP_day3_hourly[MOP_day3_hourly > 0] <- 1
MOP_day4_hourly[MOP_day4_hourly > 0] <- 1
MOP_day5_hourly[MOP_day5_hourly > 0] <- 1
MOP_day6_hourly[MOP_day6_hourly > 0] <- 1
MOP_day7_hourly[MOP_day7_hourly > 0] <- 1

#caculate the probability that one person (one row) drove at the specified time (x-axis)
#aggregate these probabilities into one data frame, every row is one person the value is the probability that the person was using the car at the time (e.g. row 2 column 7 value 1 means person 2 used the car every day of the 5 day week between 6 and 7 o'clock)
MOP_weekly_hourly_colsum_all <- as.data.frame(matrix(0, ncol = 24, nrow = nrow(MOP)))
i = 1
while (i < nrow(MOP)) {
  MOP_weekly_hourly <- rbind(MOP_day1_hourly[i,],MOP_day2_hourly[i,], MOP_day3_hourly[i,], MOP_day4_hourly[i,], MOP_day5_hourly[i, ])
  MOP_weekly_hourly_colsum <- colSums(MOP_weekly_hourly)/nrow(MOP_weekly_hourly)
  MOP_weekly_hourly_colsum_all[i, ] <- MOP_weekly_hourly_colsum
  i = i+1
}

#average probability that a person used the car 
total_probability <- colSums(MOP_weekly_hourly_colsum_all)/nrow(MOP_weekly_hourly_colsum_all)

barplot(total_probability)

count = 1
while(count < 21) {
  barplot(data.matrix(MOP_weekly_hourly_colsum_all[count, ]), ylim=c(0,1))
  count = count + 1
}
