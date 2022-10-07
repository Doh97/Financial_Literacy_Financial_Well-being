## Literacy Script ##
library(dplyr)
library(tidyr)
library(readxl)
library(glmnet)
library(RcppArmadillo)
library(Rcpp)
library(xtable)
library(DataCombine)
library(lmtest)
library(pastecs) #library for descriptive statistics
library(psych) #library for descriptive statistics

# read 2018 data
dataFL <- read.csv("2018NFCS.csv") 

# read in table of score values
datawellbeing <- read.csv("CFPBscoringworksheet.csv")

# assign values to the answers to the CFPB well-being questions
convert <- function(col) {
  foo = col
  foo[which(col==1)] = 4
  foo[which(col==2)] = 3
  foo[which(col==3)] = 2
  foo[which(col==4)] = 1
  foo[which(col==5)] = 0
  foo[which(col==98)] = 'DK'
  foo[which(col==99)] = 'RF'
  foo = as.numeric(foo)
  return(foo)
}
dataFL$J41_1 <- convert(dataFL$J41_1)
dataFL$J41_2 <- convert(dataFL$J41_2)
dataFL$J41_3 <- convert(dataFL$J41_3)
dataFL$J42_2 <- convert(dataFL$J42_2)
# The scale in J42_1 is different from other well-being questions, so it needs different codes
dataFL$J42_1 <- dataFL$J42_1 - 1
dataFL$J42_1[dataFL$J42_1==97|dataFL$J42_1==98] <- NA

# drop rows where not all financial well-being questions are answered.
dataFL = DropNA(dataFL,Var = c("J41_1","J41_2","J41_3","J42_1","J42_2"))

# calculate well-being value 
dataFL$wellbeingvalue =  dataFL$J41_1 + dataFL$J41_2 + dataFL$J41_3 + dataFL$J42_1 + dataFL$J42_2
summary(dataFL$wellbeingvalue)

# assign different well-being value by age
  # the first column of the datawellbeing table called 'score' is a well-being value
    # based on the age of the individual 
dataFL$columnchoice <- 2
dataFL$columnchoice [dataFL$A3A > 61] <- 3
# the indices correspond to the proper rows of datawellbeing, given the calculated scores, *scorewellness*
matched_indices = match(dataFL$wellbeingvalue,datawellbeing$score) 
wellbeingmat = datawellbeing[matched_indices,]
newvariable = rep(NA,nrow(wellbeingmat))
for(ii in 1:nrow(wellbeingmat)){
  iindex = dataFL$columnchoice[ii]
  newvariable[ii] = as.numeric(wellbeingmat[ii,iindex])
}
dataFL$wellbeingscore = newvariable # save into dataframe
summary(dataFL$wellbeingscore)

# creating the treatment variable called 'literacy'
dataFL$literacy = (dataFL$M6==1) + (dataFL$M7==3) + (dataFL$M8==2) + 
                  (dataFL$M9==1) + (dataFL$M10==2) + (dataFL$M31==2)

# replacing the answers of 98 ("Don't know") and 99 ("Prefer not to say") with NA
dataFL[dataFL==98|dataFL==99] <- NA

# subset the variables of interest
datnew = subset(dataFL, select = c("literacy", "wellbeingscore", "A3", "A3A", "A4A", "A5_2015", "A6", "A7", "A7A", "A8", "A9", "A11", "A41", "AM21", 
                                    "B4", "E20", "F1", "G1", "G20", "G25_1", "G25_2", "G25_3", "G25_4", "G25_5", "G38", 
                                    "J2", "J3", "J4", "J5", "J33_1", "J33_40", "J33_41", "M40", "STATEQ"))
datnew = DropNA(datnew) # drop rows with NA values

#renaming variables to friendlier descriptions
datnew <- dplyr::rename(datnew, FEducation = M40, Gender = A3, MaritalStatus = A6, Income = A8, Employment = A9, 
                        overdrawchecking = B4, creditcards = F1, autoloan = G1, unpaidhospital = G20, titleloan = G25_1, payday = G25_2, taxadvance = G25_3, 
                        pawnshop = G25_4, renttoown = G25_5, debtcollection = G38, risktaker = J2, worry = J33_1, nervous = J33_40, heartrace=J33_41, 
                        Children = A11, ArmedService = AM21, Age = A3A, Ethnic=A4A, EducLevel = A5_2015, MaritalCategory=A7A, Livingarrange=A7, OweonHome=E20,
                        Householdspending = J3, Educofwhoraised = A41, Difficulty_Bill = J4, Emergency_funds = J5) 

# X is a control and D is a treatment

# Create the dataframe of control variables excluding output and treatment variables
datnewx = subset(datnew, select = -c(wellbeingscore, literacy))

# log(age)
datnewx$Age <- log(datnewx$Age)

# change gender variable to binary variable called 'male' (1 if male, 0 if female)
datnewx$Gender[datnewx$Gender==2] <- 0
colnames(datnewx)[1] <- "Male"

# Ethnic (baseline: white) 
datnewx$Black <- ifelse(datnewx$Ethnic==2,1,0) #create Black 
datnewx$Hispanic <- ifelse(datnewx$Ethnic==3,1,0) #create Hispanic 
datnewx$Asian <- ifelse(datnewx$Ethnic==4,1,0) #create Asian 
datnewx$Other_Ethnicity <- ifelse(datnewx$Ethnic==5,1,0) #create Other_Ethnicity

# MaritalStatus (baseline: Married)
datnewx$Single <- ifelse(datnewx$MaritalStatus==2,1,0) #create Single
datnewx$Separated <- ifelse(datnewx$MaritalStatus==3,1,0) #create Separated
datnewx$Divorced <- ifelse(datnewx$MaritalStatus==4,1,0) #create Divorced
datnewx$Widow <- ifelse(datnewx$MaritalStatus==5,1,0) #create Widow

# Livingarrange (baseline: live with my spouse/partner/significant other)
datnewx$Live_Alone <- ifelse(datnewx$Livingarrange==1,1,0) #create Live_Alone (I am the only adult in the household)
datnewx$Live_Parents <- ifelse(datnewx$Livingarrange==3,1,0) #create Live_Parents col (I live in my parents' home) 
datnewx$Live_Others <- ifelse(datnewx$Livingarrange==4,1,0) #create Live_Others (I live with other family, friends, or roommates)

# Employment (baseline: Work full-time for an employer [or the military])
datnewx$Employment_SelfEmployed <- ifelse(datnewx$Employment==1,1,0) #self employed
datnewx$Employment_PartTime <- ifelse(datnewx$Employment==3,1,0) #work part-time for an employer [or the military]
datnewx$Employment_Homemaker <- ifelse(datnewx$Employment==4,1,0) #homemaker
datnewx$Employment_Student <- ifelse(datnewx$Employment==5,1,0) #full-time student
datnewx$Employment_Unable <- ifelse(datnewx$Employment==6,1,0) #who is permanently sick, disabled, or unable to work
datnewx$Employment_Unemployed <- ifelse(datnewx$Employment==7,1,0) #who is unemployed or temporarily laid off
datnewx$Employment_Retired <- ifelse(datnewx$Employment==8,1,0) #who is retired 

# Children (assign 0 to "Do not have any children" and "No financially dependent children")
datnewx$Children[datnewx$Children==5|datnewx$Children==6] <- 0

# ArmedService (baseline: Never a member of the U.S. Armed Services)
datnewx$Current_ArmedService <- ifelse(datnewx$ArmedService==1,1,0)
datnewx$Previous_ArmedService <- ifelse(datnewx$ArmedService==2,1,0)

# overdrawchecking (assign 0 to "No" and 1 to "Yes")
datnewx$overdrawchecking[datnewx$overdrawchecking==2] <- 0

# OweonHome (assign 0 to "No" and 1 to "Yes, owe more")
datnewx$OweonHome[datnewx$OweonHome==2] <- 0

# creditcards (assign 0 to "No credit cards")
datnewx$creditcards[datnewx$creditcards==7] <- 0

# autoloan (assign 0 to "No" and 1 to "Yes")
datnewx$autoloan[datnewx$autoloan==2] <- 0

# unpaidhospital (assign 0 to "No" and 1 to "Yes")
datnewx$unpaidhospital[datnewx$unpaidhospital==2] <- 0

# titleloan (assign 0 to "Never", 1 to "1 time", ... , 4 to "4 or more times")
datnewx$titleloan <- datnewx$titleloan - 1

# payday (assign 0 to "Never", 1 to "1 time", ... , 4 to "4 or more times")
datnewx$payday <- datnewx$payday - 1

# taxadvance (assign 0 to "Never", 1 to "1 time", ... , 4 to "4 or more times")
datnewx$taxadvance <- datnewx$taxadvance - 1

# pawnshop (assign 0 to "Never", 1 to "1 time", ... , 4 to "4 or more times")
datnewx$pawnshop <- datnewx$pawnshop - 1

# renttoown (assign 0 to "Never", 1 to "1 time", ... , 4 to "4 or more times")
datnewx$renttoown <- datnewx$renttoown - 1

# debtcollection (assign 0 to "No" and 1 to "Yes")
datnewx$debtcollection[datnewx$debtcollection==2] <- 0

# Householdspending (baseline: "Spending about equal to income")
datnewx$Spend_less <- ifelse(datnewx$Householdspending==1,1,0)
datnewx$Spend_more <- ifelse(datnewx$Householdspending==2,1,0)

# Difficulty_Bill 
# assign 0 to "Not at all difficult", 1 to "Somewhat difficult", 2 to "Very difficult"
datnewx$Difficulty_Bill[datnewx$Difficulty_Bill==3] <- 0
datnewx$Difficulty_Bill[datnewx$Difficulty_Bill==2] <- 1
datnewx$Difficulty_Bill[datnewx$Difficulty_Bill==1] <- 2

# Emergency_funds (assign 0 to "No" and 1 to "Yes")
datnewx$Emergency_funds[datnewx$Emergency_funds==2] <- 0

# FEducation (assign 0 to "No" and 1 to "Yes")
datnewx$FEducation[datnewx$FEducation==2] <- 0

# STATEQ (baseline: Alabama)
stateq <- read.csv("stateq.csv", header = FALSE)
datnewx$State_ <- stateq[datnewx$STATEQ,] # convert State code to State name
state <- model.matrix(~ State_ -1, data=datnewx)
state <- state[,2:length(colnames(state))] # exclude Alabama as it's a baseline
datnewx <- cbind(datnewx,state)

# drop unnecessary columns that are redefined into other columns:
  # Ethnic,MaritalStatus,MaritalCategory,Livingarrange,
  # Employment,ArmedService,Householdspending, STATEQ, State_
datnewx <- subset(datnewx, select = 
                    -c(Ethnic,MaritalStatus,MaritalCategory,
                       Livingarrange,Employment,
                       ArmedService,Householdspending,STATEQ,State_))

# final data
Y = datnew$wellbeingscore
X = datnewx
D = datnew$literacy
cleandata = list(Y=Y,X=X,D=D)
save(cleandata,file='cleandata.RData')
