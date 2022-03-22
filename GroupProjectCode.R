#I used SASXport package 
install.packages("SASxport") #The package I used 
library(SASxport) 

#Import all datasets
Demo_0304 <- read.xport("D:/Biostatistics Group Project/Files/2003_2004DEMO_C.XPT")
Demo_0506 <- read.xport("D:/Biostatistics Group Project/Files/2005_2006DEMO_D.XPT")
Demo_0708 <- read.xport("D:/Biostatistics Group Project/Files/2007_2008DEMO_E.XPT")

BMX_0304 <- read.xport("D:/Biostatistics Group Project/Files/2003_2004BMX_C.XPT")#This file missed out in github collection, please download yourselves
BMX_0506 <- read.xport("D:/Biostatistics Group Project/Files/2005_2006BMX_D.XPT")
BMX_0708 <- read.xport("D:/Biostatistics Group Project/Files/2007_2008BMX_E.XPT")

BPX_0304 <- read.xport("D:/Biostatistics Group Project/Files/2003_2004BPX_C.XPT")#Missed out in github collection
BPX_0506 <- read.xport("D:/Biostatistics Group Project/Files/2005_2006BPX_D.XPT")
BPX_0708 <- read.xport("D:/Biostatistics Group Project/Files/2007_2008BPX_E.XPT")

BPQ_0304 <- read.xport("D:/Biostatistics Group Project/Files/2003_2004BPQ_C.XPT")#Missed out in github collection
BPQ_0506 <- read.xport("D:/Biostatistics Group Project/Files/2005_2006BPQ_D.XPT")
BPQ_0708 <- read.xport("D:/Biostatistics Group Project/Files/2007_2008BPQ_E.XPT")

DIQ_0304 <- read.xport("D:/Biostatistics Group Project/Files/2003_2004DIQ_C.XPT")
DIQ_0506 <- read.xport("D:/Biostatistics Group Project/Files/2005_2006DIQ_D.XPT")
DIQ_0708 <- read.xport("D:/Biostatistics Group Project/Files/2007_2008DIQ_E.XPT")

GLU_0304 <- read.xport("D:/Biostatistics Group Project/Files/2003_2004L10AM_C.XPT")
GLU_0506 <- read.xport("D:/Biostatistics Group Project/Files/2005_2006GLU_D.XPT")
GLU_0708 <- read.xport("D:/Biostatistics Group Project/Files/2007_2008GLU_E.XPT")

GHB_0304 <- read.xport("D:/Biostatistics Group Project/Files/2003_2004L10_C.XPT")
GHB_0506 <- read.xport("D:/Biostatistics Group Project/Files/2005_2006GHB_D.XPT")
GHB_0708 <- read.xport("D:/Biostatistics Group Project/Files/2007_2008GHB_E.XPT")

ALB_0304 <- read.xport("D:/Biostatistics Group Project/Files/2003_2004L16_C.XPT")
ALB_0506 <- read.xport("D:/Biostatistics Group Project/Files/2005_2006ALB_CR_D.XPT")
ALB_0708 <- read.xport("D:/Biostatistics Group Project/Files/2007_2008ALB_CR_E.XPT")

DR1_0304 <- read.xport("D:/Biostatistics Group Project/Files/2003_2004DR1TOT_C.XPT")#
DR1_0506 <- read.xport("D:/Biostatistics Group Project/Files/2005_2006DR1TOT_D.XPT")#
DR1_0708 <- read.xport("D:/Biostatistics Group Project/Files/2007_2008DR1TOT_E.XPT")#
#Those are updated database and in the first data collection, the database was wrong
DR2_0304 <- read.xport("D:/Biostatistics Group Project/Files/2003_2004DR2TOT_C.XPT")#
DR2_0506 <- read.xport("D:/Biostatistics Group Project/Files/2004_2005DR2TOT_D.XPT")#
DR2_0708 <- read.xport("D:/Biostatistics Group Project/Files/2007_2008DR2TOT_E.XPT")#

#Subsetting for ID, Sex, Age, Race, Education, Income (Sub1)
Sub_0304 <- Demo_0304[,c(1,5,6,9,16,22)]
Sub_0506 <- Demo_0506[,c(1,5,6,9,15,21)]
Sub_0708 <- Demo_0708[,c(1,5,6,9,15,21)]

colnames(Sub_0304) <- c("ID","Sex","Age","Race","Education","Income")
colnames(Sub_0506) <- c("ID","Sex","Age","Race","Education","Income")
colnames(Sub_0708) <- c("ID","Sex","Age","Race","Education","Income")

Sub1 <- rbind(Sub_0304,Sub_0506,Sub_0708)

for(i in 1:30619){
  ifelse(Sub1[i,2] == 1,Sub1[i,2] <- "M",Sub1[i,2] <- "F")
}

for(i in 1:30619){
  if (Sub1[i,4] == 1) Sub1[i,4] <- "Mexican American"
  if (Sub1[i,4] == 2) Sub1[i,4] <- "Other Hispanic"
  if (Sub1[i,4] == 3) Sub1[i,4] <- "Non-Hispanic White"
  if (Sub1[i,4] == 4) Sub1[i,4] <- "Non-Hispanic Black"
  if (Sub1[i,4] == 5) Sub1[i,4] <- "Other Race"
}

Sub1$Education <- as.character(Sub1$Education)
Sub1[which(Sub1$Education == "1"),5] <- "Less Than 9th Grade"
Sub1[which(Sub1$Education == "2"),5] <- "9-11th Grade" #(Includes 12th grade with no diploma)
Sub1[which(Sub1$Education == "3"),5] <- "High School Grad/GED or Equivalent"
Sub1[which(Sub1$Education == "4"),5] <- "Some College or AA degree"
Sub1[which(Sub1$Education == "5"),5] <- "College Graduate or above"
#There's no 6
Sub1[which(Sub1$Education == "7"),5] <- "Refused"
#There's no 8
Sub1[which(Sub1$Education == "9"),5] <- "Don't Know"

Sub1$Income <- as.character(Sub1$Income)
Sub1[which(Sub1$Income == "1"),6] <- "$0 to $4,999"
Sub1[which(Sub1$Income == "2"),6] <- "$5,000 to $9,999" 
Sub1[which(Sub1$Income == "3"),6] <- "$10,000 to $14,999"
Sub1[which(Sub1$Income == "4"),6] <- "$15,000 to $19,999"
Sub1[which(Sub1$Income == "5"),6] <- "$20,000 to $24,999"
Sub1[which(Sub1$Income == "6"),6] <- "$25,000 to $34,999"
Sub1[which(Sub1$Income == "7"),6] <- "$35,000 to $44,999"
Sub1[which(Sub1$Income == "8"),6] <- "$45,000 to $54,999"
Sub1[which(Sub1$Income == "9"),6] <- "$55,000 to $64,999"
Sub1[which(Sub1$Income == "10"),6] <- "$65,000 to $74,999"
#There's no 11
Sub1[which(Sub1$Income == "12"),6] <- "Over $20,000"
Sub1[which(Sub1$Income == "13"),6] <- "Under $20,000"
Sub1[which(Sub1$Income == "14"),6] <- "$75,000 to $99,999"
Sub1[which(Sub1$Income == "15"),6] <- "$100,000 and Over"
Sub1[which(Sub1$Income == "77"),6] <- "Refused"
Sub1[which(Sub1$Income == "99"),6] <- "Don't know"

rm(Demo_0304,Demo_0506,Demo_0708,Sub_0304,Sub_0506,Sub_0708)

#Subsetting for BMI (BMI)
#This index is token from the data set but in the paper, it's calculated
BMI_0304 <- BMX_0304[,c(1,17)]
BMI_0506 <- BMX_0506[,c(1,11)]
BMI_0708 <- BMX_0708[,c(1,11)]

colnames(BMI_0304) <- c("ID","BMI")
colnames(BMI_0506) <- c("ID","BMI")
colnames(BMI_0708) <- c("ID","BMI")

BMI <- rbind(BMI_0304,BMI_0506,BMI_0708)

rm(BMI_0304,BMI_0506,BMI_0708,BMX_0304,BMX_0506,BMX_0708)

#Subsetting for Systolic blood pressure and Diastolic blood pressure
#There's 4 reads for each pressure. In paper there's detail on how many reads they used. I took them all and make an average out of it
Sub2_0304 <- BPX_0304[,c(1,17,18,20,21,23,24,26,27)]
Sub2_0506 <- BPX_0506[,c(1,17,18,20,21,23,24,26,27)]
Sub2_0708 <- BPX_0708[,c(1,16,17,19,20,22,23,26,26)]

colnames(Sub2_0304) <- c("ID","SystolicBPR1","DiastolicBPR1","SystolicBPR2","DiastolicBPR2","SystolicBPR3","DiastolicBPR3","SystolicBPR4","DiastolicBPR4")
colnames(Sub2_0506) <- c("ID","SystolicBPR1","DiastolicBPR1","SystolicBPR2","DiastolicBPR2","SystolicBPR3","DiastolicBPR3","SystolicBPR4","DiastolicBPR4")
colnames(Sub2_0708) <- c("ID","SystolicBPR1","DiastolicBPR1","SystolicBPR2","DiastolicBPR2","SystolicBPR3","DiastolicBPR3","SystolicBPR4","DiastolicBPR4")

Sub2_0304$MeanSystolicBP <- 0
Sub2_0506$MeanSystolicBP <- 0
Sub2_0708$MeanSystolicBP <- 0
Sub2_0304$MeanDiastolicBP <- 0
Sub2_0506$MeanDiastolicBP <- 0
Sub2_0708$MeanDiastolicBP <- 0

Sub2 <- rbind(Sub2_0304,Sub2_0506,Sub2_0708)

for (i in 1:29355){
  A <- as.numeric(Sub2[i,c(2,4,6,8)])
  B <- as.numeric(Sub2[i,c(3,5,7,9)])
  Sub2[i,10] <- mean(A,na.rm = TRUE)
  Sub2[i,11] <- mean(B,na.rm = TRUE)
}

Sub2 <- round(Sub2,digits = 2)

Sub2 <- Sub2[,c(1,10,11)]

rm(BPX_0304,BPX_0506,BPX_0708,Sub2_0304,Sub2_0506,Sub2_0708)

#Subsetting for Antihypertensive medication
Antihypermed_0304 <- BPQ_0304[,c(1,15)]   
Antihypermed_0506 <- BPQ_0506[,c(1,5)]  
Antihypermed_0708 <- BPQ_0708[,c(1,6)] 

colnames(Antihypermed_0304) <- c("ID","Antihypermed")
colnames(Antihypermed_0506) <- c("ID","Antihypermed") 
colnames(Antihypermed_0708) <- c("ID","Antihypermed") 

Antihypermed <- rbind(Antihypermed_0304,Antihypermed_0506,Antihypermed_0708)

Antihypermed$Antihypermed <- as.character(Antihypermed$Antihypermed)
Antihypermed[which(Antihypermed$Antihypermed == 1),2] <- "Yes"
Antihypermed[which(Antihypermed$Antihypermed == 2),2] <- "No"
Antihypermed[which(Antihypermed$Antihypermed == 7),2] <- "Refused"
Antihypermed[which(Antihypermed$Antihypermed == 9),2] <- "Don't know"

rm(BPQ_0304,BPQ_0506,BPQ_0708,Antihypermed_0304,Antihypermed_0506,Antihypermed_0708)

#Subsetting for Diabetes
Diabetes_0304 <- DIQ_0304[,c(1,2)]
Diabetes_0506 <- DIQ_0506[,c(1,2)]
Diabetes_0708 <- DIQ_0708[,c(1,2)]

colnames(Diabetes_0304) <- c("ID","Diabetes")
colnames(Diabetes_0506) <- c("ID","Diabetes")
colnames(Diabetes_0708) <- c("ID","Diabetes")

Diabetes <- rbind(Diabetes_0304,Diabetes_0506,Diabetes_0708)

Diabetes$Diabetes <- as.character(Diabetes$Diabetes)
Diabetes[which(Diabetes$Diabetes == "1"),2] <- "Yes"
Diabetes[which(Diabetes$Diabetes == "2"),2] <- "No"
Diabetes[which(Diabetes$Diabetes == "3"),2] <- "Borderline"
Diabetes[which(Diabetes$Diabetes == "7"),2] <- "Refused"
Diabetes[which(Diabetes$Diabetes == "9"),2] <- "Don't know"

rm(DIQ_0304,DIQ_0506,DIQ_0708,Diabetes_0304,Diabetes_0506,Diabetes_0708)

#Subsetting for Plasma glucose
Glucose_0304 <- GLU_0304[,c(1,3,4)]
Glucose_0506 <- GLU_0506[,c(1,3,4)]
Glucose_0708 <- GLU_0708[,c(1,3,4)]

colnames(Glucose_0304) <- c("ID","Glucose(mg/dL)","Glucose(mmol/L)")
colnames(Glucose_0506) <- c("ID","Glucose(mg/dL)","Glucose(mmol/L)")
colnames(Glucose_0708) <- c("ID","Glucose(mg/dL)","Glucose(mmol/L)")

Glucose <- rbind(Glucose_0304,Glucose_0506,Glucose_0708)

rm(GLU_0304,GLU_0506,GLU_0708,Glucose_0304,Glucose_0506,Glucose_0708)

#Subsetting for Glycated hemoglobin
colnames(GHB_0304) <- c("ID","GlyHemoglobin")
colnames(GHB_0506) <- c("ID","GlyHemoglobin")
colnames(GHB_0708) <- c("ID","GlyHemoglobin")

GlyHemoglobin <- rbind(GHB_0304,GHB_0506,GHB_0708)

rm(GHB_0304,GHB_0506,GHB_0708)

#Subsetting for Urinary albumin, Urinary creatinine
colnames(ALB_0304) <- c("ID","Creatinine(mg/dL)","Creatinine(umol/L)","Albumin(ug/mL)","Albumin(mg/L)")
colnames(ALB_0506) <- c("ID","Albumin(ug/mL)","Albumin(mg/L)","Creatinine(mg/dL)","Creatinine(umol/L)")
colnames(ALB_0708) <- c("ID","Albumin(ug/mL)","Albumin(mg/L)","Creatinine(mg/dL)","Creatinine(umol/L)")

Frame <- data.frame(ALB_0304[,1],ALB_0304[,4],ALB_0304[,5],ALB_0304[,2],ALB_0304[,3])

ALB_0304 <- Frame

colnames(ALB_0304) <- c("ID","Albumin(ug/mL)","Albumin(mg/L)","Creatinine(mg/dL)","Creatinine(umol/L)")

AlbuminCreatinine <- rbind(ALB_0304,ALB_0506,ALB_0708)

rm(ALB_0304,ALB_0506,ALB_0708,Frame)

#Subsetting for Sodium and Potassium intake
NAKD1_0304 <- DR1_0304[,c(1,63,64)]
NAKD1_0506 <- DR1_0506[,c(1,64,65)]
NAKD1_0708 <- DR1_0708[,c(1,68,69)]

NAKD2_0304 <- DR2_0304[,c(1,50,51)]
NAKD2_0506 <- DR2_0506[,c(1,51,52)]
NAKD2_0708 <- DR2_0708[,c(1,53,54)]

colnames(NAKD1_0304) <- c("ID","NA","K")
colnames(NAKD1_0506) <- c("ID","NA","K")
colnames(NAKD1_0708) <- c("ID","NA","K")
colnames(NAKD2_0304) <- c("ID","NA","K")
colnames(NAKD2_0506) <- c("ID","NA","K")
colnames(NAKD2_0708) <- c("ID","NA","K")

NAKD1 <- rbind(NAKD1_0304,NAKD1_0506,NAKD1_0708)
NAKD2 <- rbind(NAKD2_0304,NAKD2_0506,NAKD2_0708)

rm(DR1_0304,DR1_0506,DR1_0708,DR2_0304,DR2_0506,DR2_0708,NAKD1_0304,NAKD1_0506,NAKD1_0708,NAKD2_0304,NAKD2_0506,NAKD2_0708)

rm(A,B,i)

#The whole dataset
AlbuminCreatinineID <- as.data.frame(AlbuminCreatinine$ID) 
AntihypermedID <- as.data.frame(Antihypermed$ID) 
BMIID <- as.data.frame(BMI$ID) 
DiabetesID <- as.data.frame(Diabetes$ID) 
GlucoseID <- as.data.frame(Glucose$ID) 
GlyHemoglobinID <- as.data.frame(GlyHemoglobin$ID) 
NAKD1ID <- as.data.frame(NAKD1$ID) 
NAKD2ID <- as.data.frame(NAKD2$ID) 
Sub1ID <- as.data.frame(Sub1$ID) 
Sub2ID <- as.data.frame(Sub2$ID) 

colnames(AlbuminCreatinineID) <- "ID"
colnames(AntihypermedID) <- "ID"
colnames(BMIID) <- "ID"
colnames(DiabetesID) <- "ID"
colnames(GlucoseID) <- "ID"
colnames(GlyHemoglobinID) <- "ID"
colnames(NAKD1ID) <- "ID"
colnames(NAKD2ID) <- "ID"
colnames(Sub1ID) <- "ID"
colnames(Sub2ID) <- "ID"

ALLID <- rbind(AlbuminCreatinineID,AntihypermedID,BMIID,DiabetesID,GlucoseID,GlyHemoglobinID,NAKD1ID,NAKD2ID,Sub1ID,Sub2ID)
ALLID <- unique(ALLID)
rm(AlbuminCreatinineID,AntihypermedID,BMIID,DiabetesID,GlucoseID,GlyHemoglobinID,NAKD1ID,NAKD2ID,Sub1ID,Sub2ID)

#Adding missed obs to databases
AlbuminCreatinineMissedID <- ALLID[which((ALLID$ID) %in% (AlbuminCreatinine$ID) == FALSE),1]
AntihypermedMissedID <- ALLID[which((ALLID$ID) %in% (Antihypermed$ID) == FALSE),1]
BMIMissedID <- ALLID[which((ALLID$ID) %in% (BMI$ID) == FALSE),1]
DiabetesMissedID <- ALLID[which((ALLID$ID) %in% (Diabetes$ID) == FALSE),1]
GlucoseMissedID <- ALLID[which((ALLID$ID) %in% (Glucose$ID) == FALSE),1]
GlyHemoglobinMissedID <- ALLID[which((ALLID$ID) %in% (GlyHemoglobin$ID) == FALSE),1]
NAKD1MisBMI[29356:30619,] <- NAsedID <- ALLID[which((ALLID$ID) %in% (NAKD1$ID) == FALSE),1]
NAKD2MissedID <- ALLID[which((ALLID$ID) %in% (NAKD2$ID) == FALSE),1]
Sub1MissedID <- ALLID[which((ALLID$ID) %in% (Sub1$ID) == FALSE),1]
Sub2MissedID <- ALLID[which((ALLID$ID) %in% (Sub2$ID) == FALSE),1]

rm(Sub1MissedID)#There's no missed ID in Sub1

AlbuminCreatinine[24201:30619,] <- NA
AlbuminCreatinine[24201:30619,1] <- AlbuminCreatinineMissedID

Antihypermed[18899:30619,] <- NA
Antihypermed[18899:30619,] <- AntihypermedMissedID

BMI[29356:30619,] <- NA
BMI[29356:30619,1] <- BMIMissedID

Diabetes[29134:30619,] <- NA
Diabetes[29134:30619,1] <- DiabetesMissedID

Glucose[10024:30619,] <- NA
Glucose[10024:30619,1] <- GlucoseMissedID

GlyHemoglobin[20888:30619,] <- NA
GlyHemoglobin[20888:30619,1] <- GlyHemoglobinMissedID

NAKD1[29356:30619,] <- NA
NAKD1[29356:30619,1] <- NAKD1MissedID

NAKD2[29356:30619,] <- NA
NAKD2[29356:30619,1] <- NAKD2MissedID

Sub2[29356:30619,] <- NA
Sub2[29356:30619,1] <- Sub2MissedID

rm(AlbuminCreatinineMissedID,AntihypermedMissedID,BMIMissedID,DiabetesMissedID,GlucoseMissedID,GlyHemoglobinMissedID,NAKD1MissedID,NAKD2MissedID,Sub2MissedID)

#Combine all datasets
ALLID <- as.data.frame(ALLID[order(ALLID$ID),])  

AlbuminCreatinine <- AlbuminCreatinine[order(AlbuminCreatinine$ID),]
Antihypermed <- Antihypermed[order(Antihypermed$ID),]
BMI <- BMI[order(BMI$ID),]
Diabetes <- Diabetes[order(Diabetes$ID),]
Glucose <- Glucose[order(Glucose$ID),]
GlyHemoglobin <- GlyHemoglobin[order(GlyHemoglobin$ID),]
NAKD1 <- NAKD1[order(NAKD1$ID),]
NAKD2 <- NAKD2[order(NAKD2$ID),]
Sub1 <- Sub1[order(Sub1$ID),]
Sub2 <- Sub2[order(Sub2$ID),]

WHOLE <- cbind(AlbuminCreatinine,Antihypermed,BMI,Diabetes,Glucose,GlyHemoglobin,NAKD1,NAKD2,Sub1,Sub2)
WHOLE <- WHOLE[,-c(6,8,10,12,15,17,20,23,29)]

row.names(WHOLE) <- c(1:30619)

WHOLE[,c(23,24)] <- 0
colnames(WHOLE) <- c("ID","Albumin(ug/mL)","Albumin(mg/L)","Creatinine(mg/dL)","Creatinine(umol/L)","Antihypermed","BMI","Diabetes","Glucose(mg/dL)","Glucose(mmol/L)","GlyHemoglobin","NADay1","KDay1","NADay2","KDay2","Sex","Age","Race","Education","Income","MeanSystolicBP","MeanDiastolicBP","MeanNA","MeanK")

for (i in 1:30619){
  A <- as.numeric(WHOLE[i,c(12,14)]) #NA
  B <- as.numeric(WHOLE[i,c(13,15)]) #K
  WHOLE[i,23] <- mean(A,na.rm = TRUE)
  WHOLE[i,24] <- mean(B,na.rm = TRUE)
}

rm(A,B,i)

write.csv(WHOLE,"D:/Biostatistics Group Project/Final_Dataset.csv",row.names = FALSE,col.names = FALSE)


# Update the dataset by changing the variable Income

Income=rep(0,length(Demo_0304[,1])+length(Demo_0506[,1])+length(Demo_0708[,1]))
Income[1:10122]=Demo_0304$INDFMPIR
Income[10123:20470]=Demo_0506$INDFMPIR
Income[20471:30619]=Demo_0708$INDFMPIR
Final_data$Income=Income*100
write.csv(Final_data,"C:/Users/lenovo/Desktop/BIST 5625/Group project/Final_Dataset2.csv",
          row.names=Final_data$ID)
