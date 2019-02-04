#----- Below Code is used to manipulate given data set ----------------------#
#----------------------Installing Packages-----------------------------------#
install.packages("lubridate", dependencies = T)
install.packages("dplyr", dependencies = T)
install.packages("eeptools", dependencies = T)

#-----------------------Loading Packages-------------------------------------#
library(lubridate)
library(dplyr)
library(eeptools)

#-----------------------Reading CSV------------------------------------------#
Pain <- read.csv("Pain_Full.csv")

Pain <- Pain %>% 
  select (ID,	Centre,	Assignment,	Nosessions,	Gender,	DOB,	Abdomen,	painsites,	otherillness,	illnessdetails,	painmgt,	Education,	Marital,	T1_presentpain,	T1_worstpain,	T1_averagepain,	T1Painintensity,	bestdescribes,	T1McGillTot,	appetitie,	T1totRolandMorris,	T1Employment_final,	T2McGillTot,	T2totRM,	Medicalcard,	Treatment)

#-----------------------Data Cleaninig---------------------------------------#
#verify the structure of data and see if it looks good
str(Pain)

# We noticed column DOB is in character format but it should be date, converting DOB to date
Pain$DOB <- dmy(Pain$DOB)


summary(Pain)
#Filter all records where Pain score is not recorded after the treatment.
Pain <- Pain %>%
  filter(!is.na(T2McGillTot))

#Fill in average pain and pain intensity of NA rows with average values
Pain$T1_averagepain[which(is.na(Pain$T1_averagepain))] <- mean(Pain$T1_averagepain, na.rm = TRUE)
Pain$T1Painintensity[which(is.na(Pain$T1Painintensity))] <- mean(Pain$T1Painintensity, na.rm = TRUE)

#Fill in 0 for MedicalCard where valur is NA
Pain$Medicalcard[which(is.na(Pain$Medicalcard))] <- 0

#verify the above processing
summary(Pain)

#------------------------Derived COluns-------------------------------------#
#Calculate age from the DOB column
Pain$Age <- floor(age_calc(Pain$DOB, units = "years"))

#Calculate T1AvgScore And T2AvgScore
Pain$T1AverageScore <- (Pain$T1McGillTot + Pain$T1totRolandMorris)/2
Pain$T2AverageScore <- (Pain$T2McGillTot + Pain$T2totRM)/2

#Calculate Average Reduction Pain Score reduction
Pain$AvgReduction <- Pain$T1AverageScore - Pain$T2AverageScore

#------------------------Write CSV-------------------------------------#
write.csv(Pain, file="Pain_Clean.csv")
