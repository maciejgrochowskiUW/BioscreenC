library(lubridate)
library(gridExtra)

setwd("C:/Users/TEAM_2018/Desktop/Maciek/Bioscreen/2023.05.31")

#First U have to create vector that will carry all strain names and conditions that were used in experiment
#If you're using two plates and you want to leave some additional blank wells on your first plate then it's very important to assign conditions tested on plate1 and plate2 to c1 and c2 respectively
c1 <- c("YES_WT", "YES_Cid1", "YES_Dis32", "YES_Lsm1", "YES_Ski2", "YES_Cid1Dis32", "YES_Cid1Lsm1", "YES_Cid1Ski2", "YES_Dis32Lsm1", "YES_Dis32Ski2", "YES_Lsm1Ski2", "YES_Cid1Dis32Lsm1", "YES_Cid1Dis32Ski2", "YES_Cid1Lsm1Ski2", "YES_Dis32Lsm1Ski2")
c2 <- c("CYCLO_WT", "CYCLO_Cid1", "CYCLO_Dis32", "CYCLO_Lsm1", "CYCLO_Ski2", "CYCLO_Cid1Dis32", "CYCLO_Cid1Lsm1", "CYCLO_Cid1Ski2", "CYCLO_Dis32Lsm1", "CYCLO_Dis32Ski2", "CYCLO_Lsm1Ski2", "CYCLO_Cid1Dis32Lsm1", "CYCLO_Cid1Dis32Ski2", "CYCLO_Cid1Lsm1Ski2", "CYCLO_Dis32Lsm1Ski2")
c3 <- c(c1, c2)

#use function below to modify vector that u created so now it also carries info about number of clones and technical repetitions used. 
getcolnames <- function(strainnames, nclones, nrepetitions, nplates, separated = F){

  c4 <- c()
for (a in c(1:length(strainnames))) {
  temp <- strainnames[a]
  for (b in 1:nclones) {
    clone <- paste(temp, b, sep = ".")
    for (d in 1:nrepetitions) {
      repetition <- paste(clone, d, sep = "_")
      c4 <-  append(c4, repetition)
    }
  }
}
if (separated == F){
          if (nplates == 1) {
                       if (length(c4) < 96) {
                       c4[(length(c4)+1):96] <- "blank"
                       } 
          c4 <- append(c4, "blank", after = 0)
          c4 <- append(c4, "blank", after = 9)
          c4 <- append(c4, "blank", after = 90)
          c4 <- append(c4, "blank", after = 99)
          }
          if (nplates == 2) {
                       if (length(c4) < 192) {
                       c4[(length(c4)+1):192] <- "blank"
                       } 
          c4 <- append(c4, "blank", after = 0)
          c4 <- append(c4, "blank", after = 9)
          c4 <- append(c4, "blank", after = 90)
          c4 <- append(c4, "blank", after = 99)
          c4 <- append(c4, "blank", after = 100)
          c4 <- append(c4, "blank", after = 109)
          c4 <- append(c4, "blank", after = 190)
          c4 <- append(c4, "blank", after = 199)
          }
} else {
  blank_plate1 <- length(c1)*nclones*nrepetitions
  blank_plate1 <- blank_plate1:95
  for (blanks in blank_plate1){
    c4 <- append(c4, "blank", after = blanks)
  } 
  c4 <- append(c4, "blank", after = 0)
  c4 <- append(c4, "blank", after = 9)
  c4 <- append(c4, "blank", after = 90)
  c4 <- append(c4, "blank", after = 99)
  blank_plate2 <- 100+length(c2)*nclones*nrepetitions
  blank_plate2 <- blank_plate2:195
  for (blanks in blank_plate2){
    c4 <- append(c4, "blank", after = blanks)
  }
  c4 <- append(c4, "blank", after = 100)
  c4 <- append(c4, "blank", after = 109)
  c4 <- append(c4, "blank", after = 190)
  c4 <- append(c4, "blank", after = 199)
}
  
c4 <- append(c4, "Time", after = 0)
  return(c4)

}
c5 <- getcolnames(strainnames = c3, nclones = 2, nrepetitions = 3, nplates = 2, separated = T)
#c5 represents the order of samples on your plate(s) 

#create layout.pdf file with layout of your plates, you can print it and use it as a template while pipetting
createlayout <- function(orderofsamples){
if (length(orderofsamples) == 101) {
  pdf("layout.pdf", 20,3)
  grid.table(matrix(c5[2:101], nrow = 10, ncol = 10))
  dev.off()
}
if (length(orderofsamples) == 201) {
  pdf("layout1.pdf", 20, 3)
  grid.table(matrix(c5[2:101], nrow = 10, ncol = 10))
  dev.off()
  pdf("layout2.pdf", 20, 3)
  grid.table(matrix(c5[102:201], nrow = 10, ncol = 10))
  dev.off()
}
}
createlayout(c5)

#upload .csv file genetrated by bioscreen 
df <- read.csv("23.05.31.csv")

#use your vector to name columns of your .csv file genetrated by bioscreen
#fix time format so it can be plotted
#generate results.csv 
gen_csv <- function(rawdata, orderofsamples, filename = "results.csv"){
colnames(rawdata) <- orderofsamples
for (a in 1:length(rawdata$Time)) {
 rawdata$Time[a] <- as.numeric(seconds(hms(rawdata[a,1])))/3600
}
write.csv(rawdata, file = filename, row.names = FALSE)
}
gen_csv(df,c5)