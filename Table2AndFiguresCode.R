#Reading dataset, removing rows with NAs in columns mentioned in paper
nhanes = read.csv("C:\\Users\\ryang\\OneDrive\\Documents\\UConn\\STAT 4625\\Group Project Data\\Final_Dataset.csv")
nhanes = subset(nhanes, !is.na(MeanNA))
nhanes = subset(nhanes, !is.na(BMI))
nhanes = subset(nhanes, !is.na(Diabetes))
nhanes = subset(nhanes, !is.na(Education))
nhanes = subset(nhanes, !is.na(MeanSystolicBP))
nhanes = subset(nhanes, !is.na(MeanDiastolicBP))
nhanes = subset(nhanes, !is.na(MeanK))
nhanes = subset(nhanes, !is.na(Creatinine.mg.dL.))

#for quantile confidence intervals
library(jmuOutlier)

#for percentage intervals
propint <- function(p, n) {
  lb = p - 1.96*sqrt(p*(1-p)/n)
  ub = p + 1.96*sqrt(p*(1-p)/n)
  paste(lb, " ", ub)
}

#removing pregnant individuals
library(foreign)
pregnancy34 = read.xport("C:\\Users\\ryang\\OneDrive\\Documents\\UConn\\STAT 4625\\Group Project Data\\2003_2004RHQ_C.xpt")
pregnancy34 = subset(pregnancy34, select = c(SEQN, RHD143))

pregnancy56 = read.xport("C:\\Users\\ryang\\OneDrive\\Documents\\UConn\\STAT 4625\\Group Project Data\\2005_2006RHQ_D.xpt")
pregnancy56 = subset(pregnancy56, select = c(SEQN, RHD143))


pregnancy78 = read.xport("C:\\Users\\ryang\\OneDrive\\Documents\\UConn\\STAT 4625\\Group Project Data\\2007_2008RHQ_E.xpt")
pregnancy78 = subset(pregnancy78, select = c(SEQN, RHD143))

pregnancy = rbind(pregnancy34, pregnancy56, pregnancy78)
pregnant = subset(pregnancy, RHD143 == 1)
nhanes = subset(nhanes, !(ID %in% pregnant$SEQN))


#All observations ----
fivenum(nhanes$MeanNA)

quantileCI(nhanes$MeanNA, probs = c(.25, .5, .75))

sum(nhanes$MeanNA>1500)/nrow(nhanes)
propint(sum(nhanesO20$MeanNA>1500)/nrow(nhanesO20), nrow(nhanesO20))

sum(nhanes$MeanNA>2300)/nrow(nhanes)
propint(sum(nhanesO20$MeanNA>2300)/nrow(nhanesO20), nrow(nhanesO20))


#Age ----

#Age 20-30
nhanes20_30 = subset(nhanes, Age >= 20 & Age <= 30)
fivenum(nhanes20_30$MeanNA)

quantileCI(nhanes20_30$MeanNA, probs = c(.25, .5, .75))

sum(nhanes20_30$MeanNA>1500)/nrow(nhanes20_30)
propint(sum(nhanes20_30$MeanNA>1500)/nrow(nhanes20_30), nrow(nhanes20_30))

sum(nhanes20_30$MeanNA>2300)/nrow(nhanes20_30)
propint(sum(nhanes20_30$MeanNA>2300)/nrow(nhanes20_30), nrow(nhanes20_30))

#Age 31-50
nhanes31_50 = subset(nhanes, Age >= 31 & Age <= 50)
fivenum(nhanes31_50$MeanNA)

quantileCI(nhanes31_50$MeanNA, probs = c(.25, .5, .75))


count = sum(nhanes31_50$MeanNA>1500)
n = nrow(nhanes31_50)
count/n
propint(count/n, n)

count = sum(nhanes31_50$MeanNA>2300)
n = nrow(nhanes31_50)
count/n
propint(count/n, n)



#Age 51-70
nhanes51_70 = subset(nhanes, Age >= 51 & Age <= 70)
fivenum(nhanes51_70$MeanNA)

quantileCI(nhanes51_70$MeanNA, probs = c(.25, .5, .75))


count = sum(nhanes51_70$MeanNA>1500)
n = nrow(nhanes51_70)
count/n
propint(count/n, n)

count = sum(nhanes51_70$MeanNA>2300)
n = nrow(nhanes51_70)
count/n
propint(count/n, n)



#Age 71+
nhanes71 = subset(nhanes, Age >= 71)
fivenum(nhanes71$MeanNA)
quantileCI(nhanes71$MeanNA, probs = c(.25, .5, .75))

count = sum(nhanes71$MeanNA>1500)
n = nrow(nhanes71)
count/n
propint(count/n, n)

count = sum(nhanes71$MeanNA>2300)
n = nrow(nhanes71)
count/n
propint(count/n, n)


#Sex ----

#Male
nhanesMale = subset(nhanes, Sex == "M")
fivenum(nhanesMale$MeanNA)
quantileCI(nhanesMale$MeanNA, probs = c(.25, .5, .75))

count = sum(nhanesMale$MeanNA>1500)
n = nrow(nhanesMale)
count/n
propint(count/n, n)

count = sum(nhanesMale$MeanNA>2300)
n = nrow(nhanesMale)
count/n
propint(count/n, n)



#Female
nhanesFemale = subset(nhanesO20, Sex == "F")
fivenum(nhanesFemale$MeanNA)
quantileCI(nhanesFemale$MeanNA, probs = c(.25, .5, .75))

count = sum(nhanesFemale$MeanNA>1500)
n = nrow(nhanesFemale)
count/n
propint(count/n, n)

count = sum(nhanesFemale$MeanNA>2300)
n = nrow(nhanesFemale)
count/n
propint(count/n, n)


#Race ----

#Non-Hispanic White
nhanesNHW = subset(nhanes, Race == "Non-Hispanic White")
fivenum(nhanesNHW$MeanNA)
quantileCI(nhanesNHW$MeanNA, probs = c(.25, .5, .75))

count = sum(nhanesNHW$MeanNA>1500)
n = nrow(nhanesNHW)
count/n
propint(count/n, n)

count = sum(nhanesNHW$MeanNA>2300)
n = nrow(nhanesNHW)
count/n
propint(count/n, n)


#Non-Hispanic Black
nhanesNHB = subset(nhanes, Race == "Non-Hispanic Black")
fivenum(nhanesNHB$MeanNA)
quantileCI(nhanesNHB$MeanNA, probs = c(.25, .5, .75))


count = sum(nhanesNHB$MeanNA>1500)
n = nrow(nhanesNHB)
count/n
propint(count/n, n)

count = sum(nhanesNHB$MeanNA>2300)
n = nrow(nhanesNHB)
count/n
propint(count/n, n)


#Mexican-American
nhanesMA = subset(nhanes, Race == "Mexican American")
fivenum(nhanesMA$MeanNA)
quantileCI(nhanesMA$MeanNA, probs = c(.25, .5, .75))


count = sum(nhanesMA$MeanNA>1500)
n = nrow(nhanesMA)
count/n
propint(count/n, n)

count = sum(nhanesMA$MeanNA>2300)
n = nrow(nhanesMA)
count/n
propint(count/n, n)




#Income ----

nhanesIncome = subset(nhanes, nhanes$Income != "Refused" &
                        nhanes$Income != "11" &
                        nhanes$Income != "Don't know" &
                        nhanes$Income != "Over $20,000")
nhanesIncome = subset(nhanesIncome,!is.na(Income))
nhanesIncome$FPI130 = (nhanesIncome$Income == "$25,000 to $34,999" |
                        nhanesIncome$Income == "$100,000 and Over" |
                        nhanesIncome$Income == "$35,000 to $44,999" |
                        nhanesIncome$Income == "$45,000 to $54,999" |
                        nhanesIncome$Income == "$55,000 to $64,999" |
                        nhanesIncome$Income == "$65,000 to $74,999" |
                        nhanesIncome$Income == "$75,000 to $99,999")
nhanesGT130 = subset(nhanesIncome, FPI130 == T)


#Wealthy
fivenum(nhanesGT130$MeanNA)
quantileCI(nhanesGT130$MeanNA, probs = c(.25, .5, .75))


count = sum(nhanesGT130$MeanNA>1500)
n = nrow(nhanesGT130)
count/n
propint(count/n, n)

count = sum(nhanesGT130$MeanNA>2300)
n = nrow(nhanesGT130)
count/n
propint(count/n, n)


#Poor
nhanesLT130 = subset(nhanesIncome, FPI130 == F)

fivenum(nhanesLT130$MeanNA)
quantileCI(nhanesLT130$MeanNA, probs = c(.25, .5, .75))


count = sum(nhanesLT130$MeanNA>1500)
n = nrow(nhanesLT130)
count/n
propint(count/n, n)

count = sum(nhanesLT130$MeanNA>2300)
n = nrow(nhanesLT130)
count/n
propint(count/n, n)

#Education ----

#Less than HS
nhanesEDU12U = subset(nhanes, (Education == "9-11th Grade" |
                                    Education == "Less Than 9th Grade"))

fivenum(nhanesEDU12U$MeanNA)
quantileCI(nhanesEDU12U$MeanNA, probs = c(.25, .5, .75))


count = sum(nhanesEDU12U$MeanNA>1500)
n = nrow(nhanesEDU12U)
count/n
propint(count/n, n)

count = sum(nhanesEDU12U$MeanNA>2300)
n = nrow(nhanesEDU12U)
count/n
propint(count/n, n)


#HS Grad/GED equivalent
nhanesEDU12E = subset(nhanesO20, Education == "High School Grad/GED or Equivalent")

fivenum(nhanesEDU12E$MeanNA)
quantileCI(nhanesEDU12E$MeanNA, probs = c(.25, .5, .75))


count = sum(nhanesEDU12E$MeanNA>1500)
n = nrow(nhanesEDU12E)
count/n
propint(count/n, n)

count = sum(nhanesEDU12E$MeanNA>2300)
n = nrow(nhanesEDU12E)
count/n
propint(count/n, n)


#Some college/college grad
nhanesEDU12O = subset(nhanesO20, Education == "Some College or AA degree" |
                        Education == "College Graduate or above")

fivenum(nhanesEDU12O$MeanNA)
quantileCI(nhanesEDU12O$MeanNA, probs = c(.25, .5, .75))


count = sum(nhanesEDU12O$MeanNA>1500)
n = nrow(nhanesEDU12O)
count/n
propint(count/n, n)

count = sum(nhanesEDU12O$MeanNA>2300)
n = nrow(nhanesEDU12O)
count/n
propint(count/n, n)


#BMI ----
nhanesBMI = subset(nhanes, !is.na(BMI))

#BMI under 18.5
nhanesBMI18.5U = subset(nhanesBMI, BMI < 18.5)

fivenum(nhanesBMI18.5U$MeanNA)
quantileCI(nhanesBMI18.5U$MeanNA, probs = c(.25, .5, .75))


count = sum(nhanesBMI18.5U$MeanNA>1500)
n = nrow(nhanesBMI18.5U)
count/n
propint(count/n, n)

count = sum(nhanesBMI18.5U$MeanNA>2300)
n = nrow(nhanesBMI18.5U)
count/n
propint(count/n, n)


#BMI 18.5-24.9
nhanesBMI18.5_24.9 = subset(nhanesBMI, BMI >= 18.5 & BMI < 24.9)

fivenum(nhanesBMI18.5_24.9$MeanNA)
quantileCI(nhanesBMI18.5_24.9$MeanNA, probs = c(.25, .5, .75))


count = sum(nhanesBMI18.5_24.9$MeanNA>1500)
n = nrow(nhanesBMI18.5_24.9)
count/n
propint(count/n, n)

count = sum(nhanesBMI18.5_24.9$MeanNA>2300)
n = nrow(nhanesBMI18.5_24.9)
count/n
propint(count/n, n)


#BMI 25-29.9
nhanesBMI25_29.9 = subset(nhanesBMI, BMI >= 25 & BMI < 29.9)

fivenum(nhanesBMI25_29.9$MeanNA)
quantileCI(nhanesBMI25_29.9$MeanNA, probs = c(.25, .5, .75))


count = sum(nhanesBMI25_29.9$MeanNA>1500)
n = nrow(nhanesBMI25_29.9)
count/n
propint(count/n, n)

count = sum(nhanesBMI25_29.9$MeanNA>2300)
n = nrow(nhanesBMI25_29.9)
count/n
propint(count/n, n)


#BMI over 30
nhanesBMI30O = subset(nhanesBMI, BMI >= 30)

fivenum(nhanesBMI30O$MeanNA)
quantileCI(nhanesBMI30O$MeanNA, probs = c(.25, .5, .75))

count = sum(nhanesBMI30O$MeanNA>1500)
n = nrow(nhanesBMI30O)
count/n
propint(count/n, n)

count = sum(nhanesBMI30O$MeanNA>2300)
n = nrow(nhanesBMI30O)
count/n
propint(count/n, n)

#Hypertension ----
nhanes$Hypertension_Status = NA
nhanes$Hypertension_Status[nhanes$MeanSystolicBP >= 120 |
                                nhanes$MeanDiastolicBP >= 80] = "Prehypertension"
nhanes$Hypertension_Status[nhanes$MeanSystolicBP >= 140 |
                                nhanes$MeanDiastolicBP >= 90 |
                                nhanes$Antihypermed == "Yes"] = "Hypertension"
nhanes$Hypertension_Status[nhanes$MeanSystolicBP < 120 &
                                nhanes$MeanDiastolicBP < 90] = "Normal"


#Hypertensive
nhanesHypertension = subset(nhanes, Hypertension_Status == "Hypertension")

fivenum(nhanesHypertension$MeanNA)
quantileCI(nhanesHypertension$MeanNA, probs = c(.25, .5, .75))


count = sum(nhanesHypertension$MeanNA>1500)
n = nrow(nhanesHypertension)
count/n
propint(count/n, n)

count = sum(nhanesHypertension$MeanNA>2300)
n = nrow(nhanesHypertension)
count/n
propint(count/n, n)


#Prehypertensive
nhanesPrehypertension = subset(nhanes, Hypertension_Status == "Prehypertension")

fivenum(nhanesPrehypertension$MeanNA)
quantileCI(nhanesPrehypertension$MeanNA, probs = c(.25, .5, .75))


count = sum(nhanesPrehypertension$MeanNA>1500)
n = nrow(nhanesPrehypertension)
count/n
propint(count/n, n)

count = sum(nhanesPrehypertension$MeanNA>2300)
n = nrow(nhanesPrehypertension)
count/n
propint(count/n, n)


#Non-hypertensive
nhanesNormal = subset(nhanes, Hypertension_Status == "Normal")

fivenum(nhanesNormal$MeanNA)
quantileCI(nhanesNormal$MeanNA, probs = c(.25, .5, .75))


count = sum(nhanesNormal$MeanNA>1500)
n = nrow(nhanesNormal)
count/n
propint(count/n, n)

count = sum(nhanesNormal$MeanNA>2300)
n = nrow(nhanesNormal)
count/n
propint(count/n, n)


#Diabetes ----

#Diabetes
nhanesDiabetes = subset(nhanes, Diabetes == "Yes")

fivenum(nhanesDiabetes$MeanNA)
quantileCI(nhanesDiabetes$MeanNA, probs = c(.25, .5, .75))


count = sum(nhanesDiabetes$MeanNA>1500)
n = nrow(nhanesDiabetes)
count/n
propint(count/n, n)

count = sum(nhanesDiabetes$MeanNA>2300)
n = nrow(nhanesDiabetes)
count/n
propint(count/n, n)


#Non-diabetes
nhanesNoDiabetes = subset(nhanesO20, Diabetes == "No")

fivenum(nhanesNoDiabetes$MeanNA)
quantileCI(nhanesNoDiabetes$MeanNA, probs = c(.25, .5, .75))


count = sum(nhanesNoDiabetes$MeanNA>1500)
n = nrow(nhanesNoDiabetes)
count/n
propint(count/n, n)

count = sum(nhanesNoDiabetes$MeanNA>2300)
n = nrow(nhanesNoDiabetes)
count/n
propint(count/n, n)


#Kidney disease ----
nhanesO20$GFR = NA
nhanesKidney = subset(nhanesO20, !is.na(Creatinine.mg.dL.))

for(i in seq(1, nrow(nhanesKidney))) {
  if(nhanesKidney$Race[i] == "Non-Hispanic Black") {
    if(nhanesKidney$Sex[i] == "F") {
      if(nhanesKidney$Creatinine.mg.dL.[i] <= 62) {
        nhanesKidney$GFR[i] = 166*(nhanesKidney$Creatinine.mg.dL.[i]/.7)^(-.329)*.993^(nhanesKidney$Age[i])
      }
      else if(nhanesKidney$Creatinine.mg.dL.[i] > 62) {
        nhanesKidney$GFR[i] = 166*(nhanesKidney$Creatinine.mg.dL.[i]/.7)^(-1.209)*.993^(nhanesKidney$Age[i])
      }
    }
    else if(nhanesKidney$Sex[i] == "M") {
      if(nhanesKidney$Creatinine.mg.dL.[i] <= 80) {
        nhanesKidney$GFR[i] = 163*(nhanesKidney$Creatinine.mg.dL.[i]/.9)^(-.411)*.993^(nhanesKidney$Age[i])
      }
      else if(nhanesKidney$Creatinine.mg.dL.[i] > 80) {
        nhanesKidney$GFR[i] = 163*(nhanesKidney$Creatinine.mg.dL.[i]/.9)^(-1.209)*.993^(nhanesKidney$Age[i])
      }
    }
  }
  else {
    if(nhanesKidney$Sex[i] == "F") {
      if(nhanesKidney$Creatinine.mg.dL.[i] <= 62) {
        nhanesKidney$GFR[i] = 144*(nhanesKidney$Creatinine.mg.dL.[i]/.7)^(-.329)*.993^(nhanesKidney$Age[i])
      }
      else if(nhanesKidney$Creatinine.mg.dL.[i] > 62) {
        nhanesKidney$GFR[i] = 144*(nhanesKidney$Creatinine.mg.dL.[i]/.7)^(-1.209)*.993^(nhanesKidney$Age[i])
      }
    }
    else if(nhanesKidney$Sex[i] == "M") {
      if(nhanesKidney$Creatinine.mg.dL.[i] <= 80) {
        nhanesKidney$GFR[i] = 141*(nhanesKidney$Creatinine.mg.dL.[i]/.9)^(-.411)*.993^(nhanesKidney$Age[i])
      }
      else if(nhanesKidney$Creatinine.mg.dL.[i] > 80) {
        nhanesKidney$GFR[i] = 141*(nhanesKidney$Creatinine.mg.dL.[i]/.9)^(-1.209)*.993^(nhanesKidney$Age[i])
      }
    }
  }
}



#Density Functions ----

#Figure 1
plot(density(nhanesMale$MeanNA), xlim = c(0, 15000), ylim = c(0, .0004), main = "Sodium Intake by Sex", col = "green")
lines(density(nhanesFemale$MeanNA), col = "blue")
legend("topright", legend = c("Male", "Female"), col = c("green", "blue"), lty = c(1,2), pch = .8)

#Figure 2
plot(density(nhanesHypertension$MeanNA), xlim = c(0, 15000), ylim = c(0, .0004), main = "Sodium Intake by Hypertension Status", col = "green")
lines(density(nhanesPrehypertension$MeanNA), col = "blue")
lines(density(nhanesNormal$MeanNA), col = "purple")
legend("topright", legend = c("Hypertension", "Prehypertension", "Normal"), col = c("green", "blue", "purple"), lty = c(1,2), pch = .8)

#Figure 3
nhanes1500rec = subset(nhanes, Age>=51 | 
                         Race == "Non-Hispanic Black" |
                         Hypertension_Status == "Hypertension" |
                         Diabetes == "Yes")

plot(density(nhanes$MeanNA), xlim = c(0, 15000), ylim = c(0, .0004), main = "Sodium Intake by Recommended Sodium Intake Levels", col = "green")
lines(density(nhanes1500rec$MeanNA), col = "blue")
legend("topright", legend = c("<2300", "1500"), col = c("green", "blue"), lty = c(1,2), pch = .8)
