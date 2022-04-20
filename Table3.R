DATA <- read.csv("D:/Biostatistics Group Project/Final_Dataset.csv")

length(which(is.na(DATA$Age) == TRUE)) #0

#KidneyDisease##################################################################
KidneyDisease <- as.data.frame(DATA$Albumin.mg.L.*100/DATA$Creatinine.mg.dL.)
colnames(KidneyDisease) <- "KidneyDisease"
DATA <- cbind(DATA,KidneyDisease)
rm(KidneyDisease)

#Hypertension###################################################################
Hypertension <- rep("N0",30619)
Hyp <- which(DATA$MeanSystolicBP >= 140 | DATA$MeanDiastolicBP >= 90 | DATA$Antihypermed == "Yes")
Nor <- which(DATA$MeanSystolicBP < 120 & DATA$MeanDiastolicBP < 80)
pre1 <- which(DATA$MeanSystolicBP >= 120 & DATA$MeanSystolicBP < 140)
pre2 <- which(DATA$MeanDiastolicBP >= 80 & DATA$MeanDiastolicBP < 90)

Hypertension[Hyp] <- "Hypertension"
Hypertension[Nor] <- "Normal"
Hypertension[c(pre1,pre2)] <- "Prehypertension"
Hypertension <- as.data.frame(Hypertension)
colnames(Hypertension) <- "Hypertension"
DATA <- cbind(DATA,Hypertension)
rm(Hypertension,Hyp,Nor,pre1,pre2)

#Subseting######################################################################

T3All <- DATA[which(DATA$Age >= 20),c(1,8,16,17,18,23,25,26)]

A <- is.na(T3All$Diabetes)
T3All <- T3All[which(A == FALSE),]

A <- is.na(T3All$Sex)
T3All <- T3All[which(A == FALSE),]

A <- is.na(T3All$Race)
T3All <- T3All[which(A == FALSE),]

A <- is.na(T3All$MeanNA)
T3All <- T3All[which(A == FALSE),]

A <- is.na(T3All$KidneyDisease)
T3All <- T3All[which(A == FALSE),]

A <- which(T3All$Hypertension != "N0")
T3All <- T3All[A,]
  
row.names(T3All) <- c(1:13987)

rm(A)

#Subsetting the form data into 2 groups

A <- which(T3All$Age >= 51 | T3All$Race == "Non-Hispanic Black" | T3All$Hypertension == "Hypertension" | T3All$Diabetes == "Yes" | T3All$KidneyDisease > 30)

NA1500 <- T3All[A,]
NA2300 <- T3All[-A,]

rm(A)

#Data Forming###################################################################
library(jmuOutlier)

#All############################################################################
#Percentile
quantile(NA1500$MeanNA,c(0.25,0.5,0.75))
quantile(NA2300$MeanNA,c(0.25,0.5,0.75))
#>1500
length(which(NA1500$MeanNA > 1500))/8916
length(which(NA2300$MeanNA > 1500))/5071
#≥2300
length(which(NA1500$MeanNA > 2300))/8916
length(which(NA2300$MeanNA > 2300))/5071
#CI
quantileCI(NA1500$MeanNA,probs = c(0.25, 0.5, 0.75,(1-length(which(NA1500$MeanNA > 1500))/8916),(1-length(which(NA1500$MeanNA > 2300))/8916)), conf.level = 0.95)  
quantileCI(NA2300$MeanNA,probs = c(0.25, 0.5, 0.75,(1-length(which(NA2300$MeanNA > 1500))/5071),(1-length(which(NA2300$MeanNA > 2300))/5071)), conf.level = 0.95)  
#In 1500 Ci
length(which(NA1500$MeanNA > 1466))/8916
length(which(NA1500$MeanNA > 1523))/8916
length(which(NA1500$MeanNA > 2275.5))/8916
length(which(NA1500$MeanNA > 2335))/8916
#In 2300 CI
length(which(NA2300$MeanNA > 1452.5))/5071
length(which(NA2300$MeanNA > 1538.0))/5071
length(which(NA2300$MeanNA > 2264.5))/5071
length(which(NA2300$MeanNA > 2334.5))/5071  
  
#Age 20-30######################################################################
NA15002030 <- NA1500[which(NA1500$Age >= 20 & NA1500$Age <= 30),]
NA23002030 <- NA2300[which(NA2300$Age >= 20 & NA2300$Age <= 30),]

#Percentile
quantile(NA15002030$MeanNA,c(0.25,0.5,0.75))
quantile(NA23002030$MeanNA,c(0.25,0.5,0.75))
#≥1500
length(which(NA15002030$MeanNA > 1500))/771
length(which(NA23002030$MeanNA > 1500))/1970
#≥2300
length(which(NA15002030$MeanNA > 2300))/771
length(which(NA23002030$MeanNA > 2300))/1970
#CI
quantileCI(NA15002030$MeanNA,probs = c(0.25, 0.5, 0.75,(1-(length(which(NA15002030$MeanNA > 1500))/771)),(1-length(which(NA15002030$MeanNA > 2300))/771)), conf.level = 0.95)  
quantileCI(NA23002030$MeanNA,probs = c(0.25, 0.5, 0.75,(1-(length(which(NA23002030$MeanNA > 1500))/1970)),(1-length(which(NA23002030$MeanNA > 2300))/1970)), conf.level = 0.95)  
#In 1500 Ci
length(which(NA15002030$MeanNA > 1385))/771
length(which(NA15002030$MeanNA > 1678.5))/771
length(which(NA15002030$MeanNA > 2212))/771
length(which(NA15002030$MeanNA > 2373))/771
#In 2300 CI
length(which(NA23002030$MeanNA > 1426))/1970
length(which(NA23002030$MeanNA > 1543))/1970
length(which(NA23002030$MeanNA > 2250))/1970
length(which(NA23002030$MeanNA > 2358))/1970

rm(NA15002030,NA23002030)
  
 #Age 31-50##################################################################### 
NA15003150 <- NA1500[which(NA1500$Age >= 31 & NA1500$Age <= 50),]
NA23003150 <- NA2300[which(NA2300$Age >= 31 & NA2300$Age <= 50),]

#Percentile
quantile(NA15003150$MeanNA,c(0.25,0.5,0.75))
quantile(NA23003150$MeanNA,c(0.25,0.5,0.75))
#>1500
length(which(NA15003150$MeanNA > 1500))/1569
length(which(NA23003150$MeanNA > 1500))/3101
#≥2300
length(which(NA15003150$MeanNA > 2300))/1569
length(which(NA23003150$MeanNA > 2300))/3101
#CI
quantileCI(NA15003150$MeanNA,probs = c(0.25, 0.5, 0.75,(1-(length(which(NA15003150$MeanNA > 1500))/1569)),(1-length(which(NA15003150$MeanNA > 2300))/1569)), conf.level = 0.95)  
quantileCI(NA23003150$MeanNA,probs = c(0.25, 0.5, 0.75,(1-(length(which(NA23003150$MeanNA > 1500))/3101)),(1-length(which(NA23003150$MeanNA > 2300))/3101)), conf.level = 0.95)  
#In 1500 Ci
length(which(NA15003150$MeanNA > 1430.5))/1569
length(which(NA15003150$MeanNA > 1575))/1569
length(which(NA15003150$MeanNA > 2212))/1569
length(which(NA15003150$MeanNA > 2379))/1569
#In 2300 CI
length(which(NA23003150$MeanNA > 1438))/3101
length(which(NA23003150$MeanNA > 1566.5))/3101
length(which(NA23003150$MeanNA > 2248))/3101
length(which(NA23003150$MeanNA > 2361.5))/3101

rm(NA15003150,NA23003150)
  
#Age 51-70##################################################################### 
NA15005170 <- NA1500[which(NA1500$Age >= 51 & NA1500$Age <= 70),]

#Percentile
quantile(NA15005170$MeanNA,c(0.25,0.5,0.75))
#>1500
length(which(NA15005170$MeanNA > 1500))/4145
#≥2300
length(which(NA15005170$MeanNA > 2300))/4145
#CI
quantileCI(NA15005170$MeanNA,probs = c(0.25, 0.5, 0.75,(1-(length(which(NA15005170$MeanNA > 1500))/4145)),(1-length(which(NA15005170$MeanNA > 2300))/4145)), conf.level = 0.95)  
#In 1500 Ci
length(which(NA15005170$MeanNA > 1431))/4145
length(which(NA15005170$MeanNA > 1531.5))/4145
length(which(NA15005170$MeanNA > 2266))/4145
length(which(NA15005170$MeanNA > 2347))/4145
  
rm(NA15005170)
  
#Age ≥71######################################################################## 
NA150071 <- NA1500[which(NA1500$Age >= 71),]

#Percentile
quantile(NA150071$MeanNA,c(0.25,0.5,0.75))
#>1500
length(which(NA150071$MeanNA > 1500))/2431
#>2300
length(which(NA150071$MeanNA > 2300))/2431
#CI
quantileCI(NA150071$MeanNA,probs = c(0.25, 0.5, 0.75,(1-(length(which(NA150071$MeanNA > 1500))/2431)),(1-length(which(NA150071$MeanNA > 2300))/2431)), conf.level = 0.95)  
#In 1500 Ci
length(which(NA150071$MeanNA > 1444))/2431
length(which(NA150071$MeanNA > 1537.5))/2431
length(which(NA150071$MeanNA > 2246))/2431
length(which(NA150071$MeanNA > 2363))/2431

rm(NA150071)

#Age Male#######################################################################
NA1500M <- NA1500[which(NA1500$Sex == "M"),]
NA2300M <- NA2300[which(NA2300$Sex == "M"),]

#Percentile
quantile(NA1500M$MeanNA,c(0.25,0.5,0.75))
quantile(NA2300M$MeanNA,c(0.25,0.5,0.75))
#>1500
length(which(NA1500M$MeanNA > 1500))/4358
length(which(NA2300M$MeanNA > 1500))/2402
#≥2300
length(which(NA1500M$MeanNA > 2300))/4358
length(which(NA2300M$MeanNA > 2300))/2402
#CI
quantileCI(NA1500M$MeanNA,probs = c(0.25, 0.5, 0.75,(1-(length(which(NA1500M$MeanNA > 1500))/4358)),(1-length(which(NA1500M$MeanNA > 2300))/4358)), conf.level = 0.95)  
quantileCI(NA2300M$MeanNA,probs = c(0.25, 0.5, 0.75,(1-(length(which(NA2300M$MeanNA > 1500))/2402)),(1-length(which(NA2300M$MeanNA > 2300))/2402)), conf.level = 0.95)  
#In 1500 Ci
length(which(NA1500M$MeanNA > 1445))/4358
length(which(NA1500M$MeanNA > 1534))/4358
length(which(NA1500M$MeanNA > 2263))/4358
length(which(NA1500M$MeanNA > 2343))/4358
#In 2300 CI
length(which(NA2300M$MeanNA > 1400))/2402
length(which(NA2300M$MeanNA > 1568))/2402
length(which(NA2300M$MeanNA > 2245))/2402
length(which(NA2300M$MeanNA > 2358.5))/2402

rm(NA1500M,NA2300M)

#Age FeMale#####################################################################
NA1500F <- NA1500[which(NA1500$Sex == "F"),]
NA2300F <- NA2300[which(NA2300$Sex == "F"),]

#Percentile
quantile(NA1500F$MeanNA,c(0.25,0.5,0.75))
quantile(NA2300F$MeanNA,c(0.25,0.5,0.75))
#>1500
length(which(NA1500F$MeanNA > 1500))/4558
length(which(NA2300F$MeanNA > 1500))/2669
#≥2300
length(which(NA1500F$MeanNA > 2300))/4558
length(which(NA2300F$MeanNA > 2300))/2669
#CI
quantileCI(NA1500F$MeanNA,probs = c(0.25, 0.5, 0.75,(1-(length(which(NA1500F$MeanNA > 1500))/4558)),(1-length(which(NA1500F$MeanNA > 2300))/4558)), conf.level = 0.95)  
quantileCI(NA2300F$MeanNA,probs = c(0.25, 0.5, 0.75,(1-(length(which(NA2300F$MeanNA > 1500))/2669)),(1-length(which(NA2300F$MeanNA > 2300))/2669)), conf.level = 0.95)  
#In 1500 Ci
length(which(NA1500F$MeanNA > 1457))/4558
length(which(NA1500F$MeanNA > 1529.5))/4558
length(which(NA1500F$MeanNA > 2263))/4558
length(which(NA1500F$MeanNA > 2347))/4558
#In 2300 CI
length(which(NA2300F$MeanNA > 1444))/2669
length(which(NA2300F$MeanNA > 1551.5))/2669
length(which(NA2300F$MeanNA > 2252))/2669
length(which(NA2300F$MeanNA > 2357))/2669

rm(NA1500F,NA2300F)

#Non-Hispanic White#############################################################
NA1500W <- NA1500[which(NA1500$Race == "Non-Hispanic White"),]
NA2300W <- NA2300[which(NA2300$Race == "Non-Hispanic White"),]

#Percentile
quantile(NA1500W$MeanNA,c(0.25,0.5,0.75))
quantile(NA2300W$MeanNA,c(0.25,0.5,0.75))
#>1500
length(which(NA1500W$MeanNA > 1500))/4088
length(which(NA2300W$MeanNA > 1500))/2915
#≥2300
length(which(NA1500W$MeanNA > 2300))/4088
length(which(NA2300W$MeanNA > 2300))/2915
#CI
quantileCI(NA1500W$MeanNA,probs = c(0.25, 0.5, 0.75,(1-(length(which(NA1500W$MeanNA > 1500))/4088)),(1-length(which(NA1500W$MeanNA > 2300))/4088)), conf.level = 0.95)  
quantileCI(NA2300W$MeanNA,probs = c(0.25, 0.5, 0.75,(1-(length(which(NA2300W$MeanNA > 1500))/2915)),(1-length(which(NA2300W$MeanNA > 2300))/2915)), conf.level = 0.95)  
#In 1500 Ci
length(which(NA1500W$MeanNA > 1440.5))/4088
length(which(NA1500W$MeanNA > 1531.5))/4088
length(which(NA1500W$MeanNA > 2266))/4088
length(which(NA1500W$MeanNA > 2346))/4088
#In 2300 CI
length(which(NA2300W$MeanNA > 1420))/2915
length(which(NA2300W$MeanNA > 1560))/2915
length(which(NA2300W$MeanNA > 2245))/2915
length(which(NA2300W$MeanNA > 2356))/2915

rm(NA1500W,NA2300W)

#Non-Hispanic Black#############################################################
NA1500B <- NA1500[which(NA1500$Race == "Non-Hispanic Black"),]

#Percentile
quantile(NA1500B$MeanNA,c(0.25,0.5,0.75))
#>1500
length(which(NA1500B$MeanNA > 1500))/2912
#≥2300
length(which(NA1500B$MeanNA > 2300))/2912
#CI
quantileCI(NA1500B$MeanNA,probs = c(0.25, 0.5, 0.75,(1-(length(which(NA1500B$MeanNA > 1500))/2912)),(1-length(which(NA1500B$MeanNA > 2300))/2912)), conf.level = 0.95)  
#In 1500 Ci
length(which(NA1500B$MeanNA > 1457))/2912
length(which(NA1500B$MeanNA > 1543.5))/2912
length(which(NA1500B$MeanNA > 2243))/2912
length(which(NA1500B$MeanNA > 2346.5))/2912

rm(NA1500B)

#"Mexican American"#############################################################
NA1500Mex <- NA1500[which(NA1500$Race == "Mexican American"),]
NA2300Mex <- NA2300[which(NA2300$Race == "Mexican American"),]

#Percentile
quantile(NA1500Mex$MeanNA,c(0.25,0.5,0.75))
quantile(NA2300Mex$MeanNA,c(0.25,0.5,0.75))
#>1500
length(which(NA1500Mex$MeanNA > 1500))/1233
length(which(NA2300Mex$MeanNA > 1500))/1445
#≥2300
length(which(NA1500Mex$MeanNA > 2300))/1233
length(which(NA2300Mex$MeanNA > 2300))/1445
#CI
quantileCI(NA1500Mex$MeanNA,probs = c(0.25, 0.5, 0.75,(1-(length(which(NA1500Mex$MeanNA > 1500))/1233)),(1-length(which(NA1500Mex$MeanNA > 2300))/1233)), conf.level = 0.95)  
quantileCI(NA2300Mex$MeanNA,probs = c(0.25, 0.5, 0.75,(1-(length(which(NA2300Mex$MeanNA > 1500))/1445)),(1-length(which(NA2300Mex$MeanNA > 2300))/1445)), conf.level = 0.95)  
#In 1500 Ci
length(which(NA1500Mex$MeanNA > 1410))/1233
length(which(NA1500Mex$MeanNA > 1557.5))/1233
length(which(NA1500Mex$MeanNA > 2232.5))/1233
length(which(NA1500Mex$MeanNA > 2403))/1233
#In 2300 CI
length(which(NA2300Mex$MeanNA > 1422))/1445
length(which(NA2300Mex$MeanNA > 1554))/1445
length(which(NA2300Mex$MeanNA > 2241.5))/1445
length(which(NA2300Mex$MeanNA > 2370))/1445

rm(NA1500Mex,NA2300Mex)






