Final_data=read.csv("C:/Users/lenovo/Desktop/BIST 5625/Group project/Final_Table4.csv",
                     header=TRUE)

theta1=function(x){
      quantile(x,probs=c(0.25,0.5,0.75))
}
theta2=function(x){
      length(which(x>=4700))/length(x)*100
}
## Use Jackknife replicate weights to calculate the 95% CI

## Use Jacknife replicate weights to calculate standard error

### For all the data
potassium_all=Final_data$MeanK
summary(potassium_all)
all_result=jackknife(potassium_all,theta1) 
all_result$stats$Observed+qnorm(0.975)*all_result$stats$SE
all_result$stats$Observed-qnorm(0.975)*all_result$stats$SE
all_precent=jackknife(potassium_all,theta2)
all_precent$stats$Observed
all_precent$stats$Observed+qnorm(0.975)*all_precent$stats$SE
all_precent$stats$Observed-qnorm(0.975)*all_precent$stats$SE

### For the age group
age20_30=Final_data[which(Final_data$Age>=20 & Final_data$Age <=30),]
age31_50=Final_data[which(Final_data$Age>=31 & Final_data$Age <=50),]
age51_70=Final_data[which(Final_data$Age>=51 & Final_data$Age <=70),]
age71=Final_data[which(Final_data$Age>=71),]

summary(age20_30$MeanK)
age20_30_result=jackknife(age20_30$MeanK,theta1) 
age20_30_result$stats$Observed+qnorm(0.975)*age20_30_result$stats$SE
age20_30_result$stats$Observed-qnorm(0.975)*age20_30_result$stats$SE
age20_30_precent=jackknife(age20_30$MeanK,theta2)
age20_30_precent$stats$Observed
age20_30_precent$stats$Observed+qnorm(0.975)*age20_30_precent$stats$SE
age20_30_precent$stats$Observed-qnorm(0.975)*age20_30_precent$stats$SE

summary(age31_50$MeanK)
age31_50_result=jackknife(age31_50$MeanK,theta1) 
age31_50_result$stats$Observed+qnorm(0.975)*age31_50_result$stats$SE
age31_50_result$stats$Observed-qnorm(0.975)*age31_50_result$stats$SE
age31_50_precent=jackknife(age31_50$MeanK,theta2)
age31_50_precent$stats$Observed
age31_50_precent$stats$Observed+qnorm(0.975)*age31_50_precent$stats$SE
age31_50_precent$stats$Observed-qnorm(0.975)*age31_50_precent$stats$SE

summary(age51_70$MeanK)
age51_70_result=jackknife(age51_70$MeanK,theta1) 
age51_70_result$stats$Observed+qnorm(0.975)*age51_70_result$stats$SE
age51_70_result$stats$Observed-qnorm(0.975)*age51_70_result$stats$SE
age51_70_precent=jackknife(age51_70$MeanK,theta2)
age51_70_precent$stats$Observed
age51_70_precent$stats$Observed+qnorm(0.975)*age51_70_precent$stats$SE
age51_70_precent$stats$Observed-qnorm(0.975)*age51_70_precent$stats$SE

summary(age71$MeanK)
age71_result=jackknife(age71$MeanK,theta1) 
age71_result$stats$Observed+qnorm(0.975)*age71_result$stats$SE
age71_result$stats$Observed-qnorm(0.975)*age71_result$stats$SE
age71_precent=jackknife(age71$MeanK,theta2)
age71_precent$stats$Observed
age71_precent$stats$Observed+qnorm(0.975)*age71_precent$stats$SE
age71_precent$stats$Observed-qnorm(0.975)*age71_precent$stats$SE

### For the Sex group

male=Final_data[which(Final_data$Sex=="M"),]
female=Final_data[which(Final_data$Sex=="F"),]

summary(male$MeanK)
male_result=jackknife(male$MeanK,theta1) 
male_result$stats$Observed+qnorm(0.975)*male_result$stats$SE
male_result$stats$Observed-qnorm(0.975)*male_result$stats$SE
male_precent=jackknife(male$MeanK,theta2)
male_precent$stats$Observed
male_precent$stats$Observed+qnorm(0.975)*male_precent$stats$SE
male_precent$stats$Observed-qnorm(0.975)*male_precent$stats$SE

summary(female$MeanK)
female_result=jackknife(female$MeanK,theta1) 
female_result$stats$Observed+qnorm(0.975)*female_result$stats$SE
female_result$stats$Observed-qnorm(0.975)*female_result$stats$SE
female_precent=jackknife(female$MeanK,theta2)
female_precent$stats$Observed
female_precent$stats$Observed+qnorm(0.975)*female_precent$stats$SE
female_precent$stats$Observed-qnorm(0.975)*female_precent$stats$SE

### For the Race group

Non_white=Final_data[which(Final_data$Race=="Non-Hispanic White"),]
Non_black=Final_data[which(Final_data$Race=="Non-Hispanic Black"),]
Mexican=Final_data[which(Final_data$Race=="Mexican American"),]
Other=Final_data[which(Final_data$Race=="Other Hispanic" | Final_data$Race=="Other Race"),]

summary(Non_white$MeanK)
Non_white_result=jackknife(Non_white$MeanK,theta1) 
Non_white_result$stats$Observed+qnorm(0.975)*Non_white_result$stats$SE
Non_white_result$stats$Observed-qnorm(0.975)*Non_white_result$stats$SE
Non_white_precent=jackknife(Non_white$MeanK,theta2)
Non_white_precent$stats$Observed
Non_white_precent$stats$Observed+qnorm(0.975)*Non_white_precent$stats$SE
Non_white_precent$stats$Observed-qnorm(0.975)*Non_white_precent$stats$SE

summary(Non_black$MeanK)
Non_black_result=jackknife(Non_black$MeanK,theta1) 
Non_black_result$stats$Observed+qnorm(0.975)*Non_black_result$stats$SE
Non_black_result$stats$Observed-qnorm(0.975)*Non_black_result$stats$SE
Non_black_precent=jackknife(Non_black$MeanK,theta2)
Non_black_precent$stats$Observed
Non_black_precent$stats$Observed+qnorm(0.975)*Non_black_precent$stats$SE
Non_black_precent$stats$Observed-qnorm(0.975)*Non_black_precent$stats$SE

summary(Mexican$MeanK)
Mexican_result=jackknife(Mexican$MeanK,theta1) 
Mexican_result$stats$Observed+qnorm(0.975)*Mexican_result$stats$SE
Mexican_result$stats$Observed-qnorm(0.975)*Mexican_result$stats$SE
Mexican_precent=jackknife(Mexican$MeanK,theta2)
Mexican_precent$stats$Observed
Mexican_precent$stats$Observed+qnorm(0.975)*Mexican_precent$stats$SE
Mexican_precent$stats$Observed-qnorm(0.975)*Mexican_precent$stats$SE

summary(Other$MeanK)
Other_result=jackknife(Other$MeanK,theta1) 
Other_result$stats$Observed+qnorm(0.975)*Other_result$stats$SE
Other_result$stats$Observed-qnorm(0.975)*Other_result$stats$SE
Other_precent=jackknife(Other$MeanK,theta2)
Other_precent$stats$Observed
Other_precent$stats$Observed+qnorm(0.975)*Other_precent$stats$SE
Other_precent$stats$Observed-qnorm(0.975)*Other_precent$stats$SE

### For the Household income
low=Final_data[which(Final_data$Income<=130),]
high=Final_data[which(Final_data$Income>130),]

summary(low$MeanK)
low_result=jackknife(low$MeanK,theta1) 
low_result$stats$Observed+qnorm(0.975)*low_result$stats$SE
low_result$stats$Observed-qnorm(0.975)*low_result$stats$SE
low_precent=jackknife(low$MeanK,theta2)
low_precent$stats$Observed
low_precent$stats$Observed+qnorm(0.975)*low_precent$stats$SE
low_precent$stats$Observed-qnorm(0.975)*low_precent$stats$SE

summary(high$MeanK)
high_result=jackknife(high$MeanK,theta1) 
high_result$stats$Observed+qnorm(0.975)*high_result$stats$SE
high_result$stats$Observed-qnorm(0.975)*high_result$stats$SE
high_precent=jackknife(high$MeanK,theta2)
high_precent$stats$Observed
high_precent$stats$Observed+qnorm(0.975)*high_precent$stats$SE
high_precent$stats$Observed-qnorm(0.975)*high_precent$stats$SE

### For the Eduation
less12=Final_data[which(Final_data$Education=="9-11th Grade" | Final_data$Education=="Less Than 9th Grade"),]
equal12=Final_data[which(Final_data$Education=="High School Grad/GED or Equivalent"),]
great12=Final_data[which(Final_data$Education=="Some College or AA degree" | Final_data$Education=="College Graduate or above"),]

summary(less12$MeanK)
less12_result=jackknife(less12$MeanK,theta1) 
less12_result$stats$Observed+qnorm(0.975)*less12_result$stats$SE
less12_result$stats$Observed-qnorm(0.975)*less12_result$stats$SE
less12_precent=jackknife(less12$MeanK,theta2)
less12_precent$stats$Observed
less12_precent$stats$Observed+qnorm(0.975)*less12_precent$stats$SE
less12_precent$stats$Observed-qnorm(0.975)*less12_precent$stats$SE

summary(equal12$MeanK)
equal12_result=jackknife(equal12$MeanK,theta1) 
equal12_result$stats$Observed+qnorm(0.975)*equal12_result$stats$SE
equal12_result$stats$Observed-qnorm(0.975)*equal12_result$stats$SE
equal12_precent=jackknife(equal12$MeanK,theta2)
equal12_precent$stats$Observed
equal12_precent$stats$Observed+qnorm(0.975)*equal12_precent$stats$SE
equal12_precent$stats$Observed-qnorm(0.975)*equal12_precent$stats$SE

summary(great12$MeanK)
great12_result=jackknife(great12$MeanK,theta1) 
great12_result$stats$Observed+qnorm(0.975)*great12_result$stats$SE
great12_result$stats$Observed-qnorm(0.975)*great12_result$stats$SE
great12_precent=jackknife(great12$MeanK,theta2)
great12_precent$stats$Observed
great12_precent$stats$Observed+qnorm(0.975)*great12_precent$stats$SE
great12_precent$stats$Observed-qnorm(0.975)*great12_precent$stats$SE

### For the BMI
BMI1=Final_data[which(Final_data$BMI<18.5),]
BMI2=Final_data[which(Final_data$BMI>=18.5 & Final_data$BMI<=24.9),]
BMI3=Final_data[which(Final_data$BMI>=25.0 & Final_data$BMI<=29.9),]
BMI4=Final_data[which(Final_data$BMI>=30.0),]


summary(BMI1$MeanK)
BMI1_result=jackknife(BMI1$MeanK,theta1) 
BMI1_result$stats$Observed+qnorm(0.975)*BMI1_result$stats$SE
BMI1_result$stats$Observed-qnorm(0.975)*BMI1_result$stats$SE
BMI1_precent=jackknife(BMI1$MeanK,theta2)
BMI1_precent$stats$Observed
BMI1_precent$stats$Observed+qnorm(0.975)*BMI1_precent$stats$SE
BMI1_precent$stats$Observed-qnorm(0.975)*BMI1_precent$stats$SE

summary(BMI2$MeanK)
BMI2_result=jackknife(BMI2$MeanK,theta1) 
BMI2_result$stats$Observed+qnorm(0.975)*BMI2_result$stats$SE
BMI2_result$stats$Observed-qnorm(0.975)*BMI2_result$stats$SE
BMI2_precent=jackknife(BMI2$MeanK,theta2)
BMI2_precent$stats$Observed
BMI2_precent$stats$Observed+qnorm(0.975)*BMI2_precent$stats$SE
BMI2_precent$stats$Observed-qnorm(0.975)*BMI2_precent$stats$SE

summary(BMI3$MeanK)
BMI3_result=jackknife(BMI3$MeanK,theta1) 
BMI3_result$stats$Observed+qnorm(0.975)*BMI3_result$stats$SE
BMI3_result$stats$Observed-qnorm(0.975)*BMI3_result$stats$SE
BMI3_precent=jackknife(BMI3$MeanK,theta2)
BMI3_precent$stats$Observed
BMI3_precent$stats$Observed+qnorm(0.975)*BMI3_precent$stats$SE
BMI3_precent$stats$Observed-qnorm(0.975)*BMI3_precent$stats$SE


summary(BMI4$MeanK)
BMI4_result=jackknife(BMI4$MeanK,theta1) 
BMI4_result$stats$Observed+qnorm(0.975)*BMI4_result$stats$SE
BMI4_result$stats$Observed-qnorm(0.975)*BMI4_result$stats$SE
BMI4_precent=jackknife(BMI4$MeanK,theta2)
BMI4_precent$stats$Observed
BMI4_precent$stats$Observed+qnorm(0.975)*BMI4_precent$stats$SE
BMI4_precent$stats$Observed-qnorm(0.975)*BMI4_precent$stats$SE

### For the Hypertension status
Hypertension=Final_data[-c(which((Final_data$MeanSystolicBP>=120 & Final_data$MeanSystolicBP<=129) | (Final_data$MeanDiastolicBP>=80 & Final_data$MeanDiastolicBP<=89)),which(Final_data$MeanSystolicBP<120 & Final_data$MeanDiastolicBP<80)),]
Prehypertension=Final_data[which((Final_data$MeanSystolicBP>=120 & Final_data$MeanSystolicBP<=129) | (Final_data$MeanDiastolicBP>=80 & Final_data$MeanDiastolicBP<=89)),]
Normal=Final_data[which(Final_data$MeanSystolicBP<120 & Final_data$MeanDiastolicBP<80),]


summary(Hypertension$MeanK)
Hypertension_result=jackknife(Hypertension$MeanK,theta1) 
Hypertension_result$stats$Observed+qnorm(0.975)*Hypertension_result$stats$SE
Hypertension_result$stats$Observed-qnorm(0.975)*Hypertension_result$stats$SE
Hypertension_precent=jackknife(Hypertension$MeanK,theta2)
Hypertension_precent$stats$Observed
Hypertension_precent$stats$Observed+qnorm(0.975)*Hypertension_precent$stats$SE
Hypertension_precent$stats$Observed-qnorm(0.975)*Hypertension_precent$stats$SE

summary(Prehypertension$MeanK)
Prehypertension_result=jackknife(Prehypertension$MeanK,theta1) 
Prehypertension_result$stats$Observed+qnorm(0.975)*Prehypertension_result$stats$SE
Prehypertension_result$stats$Observed-qnorm(0.975)*Prehypertension_result$stats$SE
Prehypertension_precent=jackknife(Prehypertension$MeanK,theta2)
Prehypertension_precent$stats$Observed
Prehypertension_precent$stats$Observed+qnorm(0.975)*Prehypertension_precent$stats$SE
Prehypertension_precent$stats$Observed-qnorm(0.975)*Prehypertension_precent$stats$SE

summary(Normal$MeanK)
Normal_result=jackknife(Normal$MeanK,theta1) 
Normal_result$stats$Observed+qnorm(0.975)*Normal_result$stats$SE
Normal_result$stats$Observed-qnorm(0.975)*Normal_result$stats$SE
Normal_precent=jackknife(Normal$MeanK,theta2)
Normal_precent$stats$Observed
Normal_precent$stats$Observed+qnorm(0.975)*Normal_precent$stats$SE
Normal_precent$stats$Observed-qnorm(0.975)*Normal_precent$stats$SE

### For the Diabetes diagnosis
Diabete_Yes=Final_data[which(Final_data$Diabetes=="Yes"),]
Diabete_No=Final_data[which(Final_data$Diabetes=="No"),]

summary(Diabete_Yes$MeanK)
Diabete_Yes_result=jackknife(Diabete_Yes$MeanK,theta1) 
Diabete_Yes_result$stats$Observed+qnorm(0.975)*Diabete_Yes_result$stats$SE
Diabete_Yes_result$stats$Observed-qnorm(0.975)*Diabete_Yes_result$stats$SE
Diabete_Yes_precent=jackknife(Diabete_Yes$MeanK,theta2)
Diabete_Yes_precent$stats$Observed
Diabete_Yes_precent$stats$Observed+qnorm(0.975)*Diabete_Yes_precent$stats$SE
Diabete_Yes_precent$stats$Observed-qnorm(0.975)*Diabete_Yes_precent$stats$SE

summary(Diabete_No$MeanK)
Diabete_No_result=jackknife(Diabete_No$MeanK,theta1) 
Diabete_No_result$stats$Observed+qnorm(0.975)*Diabete_No_result$stats$SE
Diabete_No_result$stats$Observed-qnorm(0.975)*Diabete_No_result$stats$SE
Diabete_No_precent=jackknife(Diabete_No$MeanK,theta2)
Diabete_No_precent$stats$Observed
Diabete_No_precent$stats$Observed+qnorm(0.975)*Diabete_No_precent$stats$SE
Diabete_No_precent$stats$Observed-qnorm(0.975)*Diabete_No_precent$stats$SE


### For the Chronic kidney disease
Final_data$kidney_disease=Final_data$Albumin.mg.L./(0.01*Final_data$Creatinine.mg.dL.)
Kidney_Yes=Final_data[which(Final_data$kidney_disease>30),]
Kidney_No=Final_data[which(Final_data$kidney_disease<=30),]

summary(Kidney_Yes$MeanK)
Kidney_Yes_result=jackknife(Kidney_Yes$MeanK,theta1) 
Kidney_Yes_result$stats$Observed+qnorm(0.975)*Kidney_Yes_result$stats$SE
Kidney_Yes_result$stats$Observed-qnorm(0.975)*Kidney_Yes_result$stats$SE
Kidney_Yes_precent=jackknife(Kidney_Yes$MeanK,theta2)
Kidney_Yes_precent$stats$Observed
Kidney_Yes_precent$stats$Observed+qnorm(0.975)*Kidney_Yes_precent$stats$SE
Kidney_Yes_precent$stats$Observed-qnorm(0.975)*Kidney_Yes_precent$stats$SE


summary(Kidney_No$MeanK)
Kidney_No_result=jackknife(Kidney_No$MeanK,theta1) 
Kidney_No_result$stats$Observed+qnorm(0.975)*Kidney_No_result$stats$SE
Kidney_No_result$stats$Observed-qnorm(0.975)*Kidney_No_result$stats$SE
Kidney_No_precent=jackknife(Kidney_No$MeanK,theta2)
Kidney_No_precent$stats$Observed
Kidney_No_precent$stats$Observed+qnorm(0.975)*Kidney_No_precent$stats$SE
Kidney_No_precent$stats$Observed-qnorm(0.975)*Kidney_No_precent$stats$SE








