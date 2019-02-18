data <- read.csv("C:/Users/Martin/Desktop/Masterthesis/DrivingProfiles_JHO_6466_Vollzeit.csv", header = FALSE, sep = ",", dec = ".", stringsAsFactors = FALSE)[c(TRUE, FALSE), ]
weekdays <- data[, 1:480]

#rename the rows
rownames(weekdays) <- 1:nrow(weekdays)

#convert factors to numeric values
weekdays[, 1:ncol(weekdays)] <- sapply(weekdays[, 1:ncol(weekdays)], as.numeric)

monday <- weekdays[, 1:96]
tuesday <- weekdays[, 97:192]
wednesday <- weekdays[, 193:288]
thursday <- weekdays[, 289:384]
friday <- weekdays[, 385:480]

monday[, 1:ncol(monday)] <- sapply(monday[, 1:ncol(monday)], as.numeric)
tuesday[, 1:ncol(tuesday)] <- sapply(tuesday[, 1:ncol(tuesday)], as.numeric)
wednesday[, 1:ncol(wednesday)] <- sapply(wednesday[, 1:ncol(wednesday)], as.numeric)
thursday[, 1:ncol(thursday)] <- sapply(thursday[, 1:ncol(thursday)], as.numeric)
friday[, 1:ncol(friday)] <- sapply(friday[, 1:ncol(friday)], as.numeric)

# calculate daily maximum pause length per person
departure.time.per.person <- data.frame(Monday=double(),
                                              Tuesday=double(),
                                              Wednesday=double(),
                                              Thursday=double(),
                                              Friday=double())
o = 1
while (o < 2967) {
  counter = 0
  next.Person = FALSE
  i = 1
  while (i < 97 & next.Person == FALSE) {
    if (monday[o, i] == 0) {
      counter = counter + 1
      i = i + 1
    } else if (monday[o, i] != 0) {
      next.Person = TRUE
      departure.time.per.person[o, 1] = counter
      counter = 0
      o = o + 1
    } 
  }
  if (counter == 96) {
    #departure.time.per.person[o, 1] = counter
    o = o + 1
  }
}

o = 1
while (o < 2967) {
  counter = 0
  next.Person = FALSE
  i = 1
  while (i < 97 & next.Person == FALSE) {
    if (tuesday[o, i] == 0) {
      counter = counter + 1
      i = i + 1
    } else if (tuesday[o, i] != 0) {
      next.Person = TRUE
      departure.time.per.person[o, 2] = counter
      counter = 0
      o = o + 1
    } 
  }
  if (counter == 96) {
    #departure.time.per.person[o, 2] = counter
    o = o + 1
  }
}

o = 1
while (o < 2967) {
  counter = 0
  next.Person = FALSE
  i = 1
  while (i < 97 & next.Person == FALSE) {
    if (wednesday[o, i] == 0) {
      counter = counter + 1
      i = i + 1
    } else if (wednesday[o, i] != 0) {
      next.Person = TRUE
      departure.time.per.person[o, 3] = counter
      counter = 0
      o = o + 1
    } 
  }
  if (counter == 96) {
    #departure.time.per.person[o, 3] = counter
    o = o + 1
  }
}

o = 1
while (o < 2967) {
  counter = 0
  next.Person = FALSE
  i = 1
  while (i < 97 & next.Person == FALSE) {
    if (thursday[o, i] == 0) {
      counter = counter + 1
      i = i + 1
    } else if (thursday[o, i] != 0) {
      next.Person = TRUE
      departure.time.per.person[o, 4] = counter
      counter = 0
      o = o + 1
    } 
  }
  if (counter == 96) {
    #departure.time.per.person[o, 4] = counter
    o = o + 1
  }
}


o = 1
while (o < 2967) {
  counter = 0
  next.Person = FALSE
  i = 1
  while (i < 97 & next.Person == FALSE) {
    if (friday[o, i] == 0) {
      counter = counter + 1
      i = i + 1
    } else if (friday[o, i] != 0) {
      next.Person = TRUE
      departure.time.per.person[o, 5] = counter
      counter = 0
      o = o + 1
    } 
  }
  if (counter == 96) {
    #departure.time.per.person[o, 5] = counter
    o = o + 1
  }
}


average.departure.time.per.person = rowSums(departure.time.per.person)/5
o = 1
while (o < 2967) {
  if (average.departure.time.per.person != "NA") {
    average.departure.time.per.person[o] = average.departure.time.per.person[o]/4  
  }
  o = o+1
}

# prop.table(table(unlist(average.departure.time.per.person)))
# png('abfahrtszeit.png')
# plot(prop.table(table(unlist(average.departure.time.per.person))), xlab="Abfahrtszeiten", ylab="Wahrscheinlichkeit")
# dev.off()
# getwd()
#+ abline(v=mean(average.departure.time.per.person)) + abline(v=median(average.departure.time.per.person)) + abline(v=28)
mean(average.departure.time.per.person, na.rm = TRUE)
median(average.departure.time.per.person, na.rm = TRUE)
sd(average.departure.time.per.person, na.rm = TRUE)

quantile(average.departure.time.per.person, na.rm = TRUE)

clean.departure.time <- na.omit(average.departure.time.per.person)

write.csv(clean.departure.time, file ="C:/Users/Martin/Desktop/departure.csv", row.names=FALSE)


x <- table(unlist(departure.time.per.person))
summary(x)
      
prop.table(table(unlist(departure.time.per.person)))
