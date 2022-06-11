# Dependencies:
install.packages("writexl")
install.packages('visdat')
install.packages('naniar')
install.packages('mice')
install.packages("reprex")
install.packages("Rcpp")
install.packages("Amelia")
install.packages('missForest')
install.packages('Zelig')

library(readxl)
library(tibble)
library(writexl)
library(naniar)
library(visdat)
library(dplyr)
library(mice)
library(Amelia)
library(missForest)
library(caret)
library(Zelig)

# import the translated AP dataset:
AP <- read_xlsx('C:\\E DRIVE\\LANGARA COLLEGE\\DANA 4830-001\\Assignment 1\\APNotCleaned_translated.xlsm', na = c('', " ", 'NA'))

View(AP)
attach(AP)


# variable names of dataset:
names(AP)

# structure of dataset
str(AP)

# shape of dataset
dim(AP)

attributes(AP) # gives row names, column names in this one function

# summary of data
summary(AP)

# ACCURACY CHECK:
# we have determined which variables we have to remove from our dataset. Lets remove those variables and create a
# subset of selected variables. Then we can perform accuracy check on only the selected variables

ap_selected <- AP[-c(1,5:7,10,14,17,18,23,30,36,37,39:42,47:50,52:55,57:59,61:64,66:68,70:72,74:76,78:80,82:84,86:88,90:93,
                  95,96,99:101,103:105,107,108,110,112,113,115,116,118,119,121:123,126:128,130:133,135:138,140:143,
                  145:148,150:153,155:158,171,172,175:179,182,183,186:191)]

View(ap_selected)
names(ap_selected)

# Lets ecport this dataset for further use:
write_xlsx(ap_selected,"C:\\E DRIVE\\LANGARA COLLEGE\\DANA 4830-001\\Assignment 1\\AP_selected.xlsx")           
               
# Visualization of missing values:
               

windows()
vis_miss(ap_selected)

# clustering to see missing patterns
vis_miss(ap_selected, cluster = TRUE)  

# checking missing pattern for both the two groups of treatments - for patients with PEX and without
AP_gb_pex <- ap_selected %>%
  rename(PEX=`Patient with PEX or without PEX`)%>%
  filter(PEX == 1)
vis_miss(AP_gb_pex, cluster = TRUE)

## creating a dataset of patients without pEX
AP_gb_no_pex <- ap_selected %>%
  rename(PEX=`Patient with PEX or without PEX`)%>%
  filter(PEX == 0)
vis_miss(AP_gb_no_pex, cluster = TRUE)

# Lets visualise the missing percentages based on number of days in hospital:
# Lets create a variable 'hospital_duration' having values short, med and long
# the duration of hospital stays in our dataset is 1 to 18. with a median score 6. lets categorise 1-6 as short,
#7-12 as medium stay and 12-18 as long

ap_hospital <- ap_selected%>%
  rename(hospital_days=`Duration of staying in hospitals`)%>%
  mutate(ap_selected, hosp_duration = ifelse(hospital_days <=6, 'Short',
                                             ifelse(hospital_days %in% 7:12, 'Med','Long')))

ap_hospital_short<- ap_hospital %>%
  filter(hosp_duration =='Short')
  vis_miss(ap_hospital_short, cluster = T)

ap_hospital_med<- ap_hospital %>%
  filter(hosp_duration =='Med')
  vis_miss(ap_hospital_med, cluster = T)

ap_hospital_long<- ap_hospital %>%
  filter(hosp_duration =='Long')
  vis_miss(ap_hospital_long, cluster = T)
  
# looking at missings in variables and cases
gg_miss_var(ap_hospital)
gg_miss_var(ap_hospital, facet = hosp_duration)              
               
               
# List of all figures               
vis_miss(ap_selected, cluster = TRUE)              
vis_miss(AP_gb_pex, cluster = TRUE)              
vis_miss(AP_gb_no_pex, cluster = TRUE)              
vis_miss(ap_hospital_short, cluster = T)
vis_miss(ap_hospital_med, cluster = T)
vis_miss(ap_hospital_long, cluster = T)
gg_miss_var(ap_hospital)
gg_miss_var(ap_hospital, facet = hosp_duration)            


# Creating another dataset by keeping subclinical variables         
ap_selected2 <- AP[-c(5:7,10,14,17,18,23,30,36,37,39:42,171,172,175:179,182,183,186:191)]

write_xlsx(ap_selected2,"C:\\E DRIVE\\LANGARA COLLEGE\\DANA 4830-001\\Assignment 1\\AP_selected2.xlsx") 

###############################################################################################

# Importing AP data cleaned in Python:

AP_clean <- read.csv('C:\\E DRIVE\\LANGARA COLLEGE\\DANA 4830-001\\Assignment 1\\AP_selected_cleanFinal.csv',
                      na = c('', " ", 'NA'))

# Imputing missing values with MICE

# defining all the categorical variables as factor:
AP_clean$Gender = as.factor(AP_clean$Gender)
AP_clean$Main.reason.to.admit.hospital = as.factor(AP_clean$Main.reason.to.admit.hospital)
AP_clean$Hereditary.information = as.factor(AP_clean$Hereditary.information)
AP_clean$Gallbladder.problem = as.factor(AP_clean$Gallbladder.problem)
AP_clean$Drinking.problem = as.factor(AP_clean$Drinking.problem)
AP_clean$Vomitting = as.factor(AP_clean$Vomitting)
AP_clean$ranson.score.at.the.points.of.admitting.hospitals = as.factor(AP_clean$ranson.score.at.the.points.of.admitting.hospitals)
AP_clean$CTSI.score.at.the.points.of.admitting.hospitals = as.factor(AP_clean$CTSI.score.at.the.points.of.admitting.hospitals)
AP_clean$imre.score.at.the.points.of.admitting.hospitals = as.factor(AP_clean$imre.score.at.the.points.of.admitting.hospitals)
AP_clean$sofa.score.at.the.points.of.admitting.hospitals = as.factor(AP_clean$sofa.score.at.the.points.of.admitting.hospitals)
AP_clean$subclinical.examination...balthazar.sHavere..with.computer.tomography. = as.factor(AP_clean$subclinical.examination...balthazar.sHavere..with.computer.tomography.)
AP_clean$Clinical.symptoms.of.Abdominal.distension = as.factor(AP_clean$Clinical.symptoms.of.Abdominal.distension)
AP_clean$subclinical.examination....Abdominal.fluid..computer.tomography = as.factor(AP_clean$subclinical.examination....Abdominal.fluid..computer.tomography)
AP_clean$subclinical.examination....Abdominal.fluid..ultrasound..at.the.points.of.admitting.hospitals = as.factor(AP_clean$subclinical.examination....Abdominal.fluid..ultrasound..at.the.points.of.admitting.hospitals)
AP_clean$subclinical.examination...CTSI.score..with.computer.tomography. = as.factor(AP_clean$subclinical.examination...CTSI.score..with.computer.tomography.)
AP_clean$treatment...day.without.food.intake = as.factor(AP_clean$treatment...day.without.food.intake)
AP_clean$treatment...PEX.treatment.of.which.day.of.the.diagnosis = as.factor(AP_clean$treatment...PEX.treatment.of.which.day.of.the.diagnosis)
AP_clean$treatment...number.of.PEX.treatment = as.factor(AP_clean$treatment...number.of.PEX.treatment)
AP_clean$treatment...APAche.2.sHavere.before.first.time.PEX = as.factor(AP_clean$treatment...APAche.2.sHavere.before.first.time.PEX)
AP_clean$treatment...APAche.2.sHavere.after.first.time.PEX = as.factor(AP_clean$treatment...APAche.2.sHavere.after.first.time.PEX)
AP_clean$treatment...Imre.sHavere.before.first.time.of.PEX = as.factor(AP_clean$treatment...Imre.sHavere.before.first.time.of.PEX)
AP_clean$treatment...Imre.sHavere.after.first.time.of.PEX = as.factor(AP_clean$treatment...Imre.sHavere.after.first.time.of.PEX)
AP_clean$Result...dead.or.alive = as.factor(AP_clean$Result...dead.or.alive)
AP_clean$Potential.Complication = as.factor(AP_clean$Potential.Complication)
AP_clean$Patient.with.PEX.or.without.PEX = as.factor(AP_clean$Patient.with.PEX.or.without.PEX)


AP_clean_mice <- mice(AP_clean[,-1], m=1, maxit = 3, seed = 88) 
summary(AP_clean_mice)

# can specify method ='pmm' predictive mean matching for continuous variables. If dataset contains variables of
# different data types, dont specify the method. the program will use the suitable method of imputation.
# In general, the categorical variables will be imputed by 'polyreg' method which is multinomial poly regression

## clean data
AP_clean_miceimputed <-complete(AP_clean_mice,1)

# exporting the MICE imputed dataset for further use:
write.csv(AP_clean_miceimputed,"C:\\E DRIVE\\LANGARA COLLEGE\\DANA 4830-001\\Assignment 1\\AP_clean_MiceImputed.csv")

# Imputing missing values with Amelia:

nominal_vars <- c('Gender', 'Hereditary.information','Vomitting',
                  'Gallbladder.problem', 'Drinking.problem', 'Diabetes.problem', 
                  'Historical.cholecystitis.problem', 'Clinical.symptoms.of.Abdominal.distension',
                  "subclinical.examination....Abdominal.fluid..ultrasound..at.the.points.of.admitting.hospitals",
                  "subclinical.examination....Abdominal.fluid..computer.tomography",
                  "Result...dead.or.alive","Potential.Complication","Patient.with.PEX.or.without.PEX")

ordinal_vars <- c("A.breakdown.of.drinking.problem...13","apache.2.score.at.the.points.of.admitting.hospitals",                                                                         
                  "ranson.score.at.the.points.of.admitting.hospitals",                                                                           
                  "CTSI.score.at.the.points.of.admitting.hospitals",                                                                             
                  "imre.score.at.the.points.of.admitting.hospitals",                                                                             
                  "sofa.score.at.the.points.of.admitting.hospitals",
                  "subclinical.examination...balthazar.sHavere..with.computer.tomography.",
                  "subclinical.examination...CTSI.score..with.computer.tomography.",
                  "treatment...day.without.food.intake",
                  "treatment...PEX.treatment.of.which.day.of.the.diagnosis",
                  "treatment...number.of.PEX.treatment",
                  "treatment...APAche.2.sHavere.before.first.time.PEX",                                                                          
                  "treatment...APAche.2.sHavere.after.first.time.PEX",                                                                           
                  "treatment...Imre.sHavere.before.first.time.of.PEX",                                                                           
                  "treatment...Imre.sHavere.after.first.time.of.PEX")

AP_clean_amelia <- amelia(AP_clean[,-c(1,4)], m=3, p2s=2, noms = nominal_vars, ords = ordinal_vars, seed=88,
                       parallel = 'snow', empri = 0.8*nrow(AP_clean[,-c(1,4)]))

# Also removing main reason for hospital admission admission column above as it has only 1 value

write.amelia(obj =AP_clean_amelia, file.stem = "AP_cleanAmeliaImputed")
# The above written file can be found in the same directory, we imported our original data from

AP_clean_ameliaImputed <- read.csv("C:\\E DRIVE\\LANGARA COLLEGE\\DANA 4830-001\\Assignment 1\\AP_cleanAmeliaImputed2.csv")

z5_imp <- Zelig::zls$new()
z5_imp$zelig(Result...dead.or.alive~., data=AP_clean_ameliaImputed[,-3])
z5_imp

# Imputing missing values with MissForest:

AP_clean_missforest <- missForest(AP_clean[,-1])
AP_clean_missforest$ximp

# checking the imputation error of Miss forest
AP_clean_missforest$OOBerror

AP_clean_missforestImputed <- data.frame(AP_clean_missforest$ximp)

# exporting the missforest imputed dataset for further use:
write.csv(AP_clean_missforestImputed,"C:\\E DRIVE\\LANGARA COLLEGE\\DANA 4830-001\\Assignment 1\\AP_clean_missforestImputed.csv")



































               
               
               
               