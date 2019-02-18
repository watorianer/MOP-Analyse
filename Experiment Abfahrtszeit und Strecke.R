library(ggplot2)
library(gtable)
library(grid)

data <- read.csv("C:/Users/Martin Wolff/Desktop/Masterthesis/DrivingProfiles_JHO_6466_Vollzeit.csv", header = FALSE, sep = ",", dec = ".", stringsAsFactors = FALSE)[c(TRUE, FALSE), ]
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

# calculate average daily driven distance for each user
average.daily.driven.km.per.person <- rowSums(weekdays)/5


# calculate departure time per person
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
    departure.time.per.person[o, 1] = counter
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
    departure.time.per.person[o, 2] = counter
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
    departure.time.per.person[o, 3] = counter
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
    departure.time.per.person[o, 4] = counter
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
    departure.time.per.person[o, 5] = counter
    o = o + 1
  }
}

#convert to hours
average.departure.time.per.person = rowSums(departure.time.per.person)/5
average.departure.time.per.person <- sapply(average.departure.time.per.person, function(i) (i*15)/60)


#fill data frame with the data
experiment.data <- data.frame(to_drive=average.daily.driven.km.per.person,
                              departure=average.departure.time.per.person,
                              stringsAsFactors = FALSE)

#write the csv
write.csv(experiment.data, file = "C:\\Users\\Martin Wolff\\Downloads\\Experiment.csv",row.names=FALSE)

ggplot(data=experiment.data, aes(x=to_drive)) + geom_histogram(bins=30, fill='skyblue', color='white') + scale_x_log10()
ggplot(data=experiment.data, aes(x=departure)) + geom_histogram(bins=30, fill='skyblue', color='white')

ggplot(data=experiment.data, aes(x=to_drive, y=departure)) + geom_jitter(alpha=0.1, colour="blue", size=1) + xlab("Zu fahrende Kilometer") + ylab("Abfahrtszeit") + coord_cartesian(ylim=c(0,24))

ggplot(data=experiment.data, aes(x=to_drive, y=departure)) + stat_density_2d(aes(fill = ..level..), geom = "polygon") + theme_bw() + scale_fill_viridis()

ggplot(data = experiment.data, aes(x=to_drive, y=departure)) + geom_bin2d(bins=40) + theme_bw() + xlab("Zu fahrende Kilometer") + ylab("Abfahrtszeit") + scale_fill_viridis()

p <- ggplot(data = experiment.data, aes(x=to_drive, y=departure)) + geom_hex(bins=80) + theme_bw()+ theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + xlab("Zu fahrende Kilometer") + ylab("Abfahrtszeit") + scale_fill_viridis() + ggtitle("Anzahl vergangener Fahrten mit gefahrenen Kilometern und dazugehörigen Abfahrtszeiten") + labs(fill=" Anzahl\nFahrten") + theme(legend.title.align = 0.5)

ggplot(data = experiment.data, aes(x=to_drive, y=departure)) + geom_hex(bins=80) + theme_bw()+ theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + xlab("Gefahrene Kilometer") + ylab("Abfahrtszeit") + scale_fill_viridis() + ggtitle("Anzahl vergangener Fahrten") + labs(fill=" Anzahl\nFahrten") + theme(legend.title.align = 0.5) + scale_y_continuous(breaks=c(0,4,8,12,16,20,24)) + scale_x_continuous(breaks=c(0,25,50,75,100,125,150,200,250,300,400,500,600)) + theme(plot.title = element_text(hjust=0.5))

p <- ggplot(data = experiment.data, aes(x=to_drive, y=departure)) + geom_hex(bins=80) + theme_bw()+ theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + xlab("Gefahrene Kilometer") + ylab("Abfahrtszeit") + scale_fill_viridis() + ggtitle("Häufigkeit vergangener Fahrten") + labs(fill="Häufigkeit \nvon Fahrten") + theme(legend.title.align = 0.5) + scale_y_continuous(breaks=c(0,4,8,12,16,20,24), labels=c("0 Uhr", "4 Uhr", "8 Uhr", "12 Uhr", "16 Uhr", "20 Uhr", "24 Uhr")) + scale_x_continuous(breaks=c(0,25,50,75,100,125,150,200,250,300,400,500,600)) + theme(plot.title = element_text(hjust=0.5))

# labels=c("0 km", "25 km", "50 km", "75 km", "100 km", "125 km", "150 km", "200 km", "250 km", "300 km", "400 km", "500 km", "600 km")
# extract legend
g <- ggplotGrob(p)
grobs <- g$grobs
legend_index <- which(sapply(grobs, function(x) x$name) == "guide-box")
legend <- grobs[[legend_index]]

# extract guides table
guides_index <- which(sapply(legend$grobs, function(x) x$name) == "layout")
guides <- legend$grobs[[guides_index]]

# add extra column for spacing
# guides$width[5] is the extra spacing from the end of the legend text
# to the end of the legend title. If we instead distribute it 50:50 on
# both sides, we get a centered legend
guides <- gtable_add_cols(guides, 0.5*guides$width[5], 1)
guides$widths[6] <- guides$widths[2]
title_index <- guides$layout$name == "title"
guides$layout$l[title_index] <- 2

# reconstruct legend and write back
legend$grobs[[guides_index]] <- guides
g$grobs[[legend_index]] <- legend

grid.newpage()
grid.draw(g)

experiment <- ggplot(data = experiment.data, aes(x=to_drive, y=departure)) + geom_hex(bins=80) + theme_bw()+ theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + xlab("Zu fahrende Kilometer") + ylab("Abfahrtszeit") + scale_fill_viridis()
ggsave(p, file="C:\\Users\\Martin Wolff\\Downloads\\ExperimentFinal.svg")
