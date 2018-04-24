#read in data#
setwd("/Users/huiyuhu/Desktop/Study/UCLA Biostat/BIOSTAT 203A/Midterm Project")
hh <- read.csv("ACS_Household_Sample_17.csv",header = TRUE)
person <- read.csv("ACS_Person_MasterSample.csv",header = TRUE)


merged.data1 <- merge(hh,person,by  = "serialno" )
nrow(hh)
nrow(person)
nrow(merged.data1)
mydata <- merged.data1
################################################################################################################################
############Household-Level Demographic Characteristics#############################################################################
################################################################################################################################
#Region Code
region <- hh$REGION
region[hh$REGION == 1] <- "Northeast"
region[hh$REGION == 2] <- "Midwest"
region[hh$REGION == 3] <- "South"
region[hh$REGION == 4] <- "West"
region[hh$REGION == 9] <- "Puerto Rico"
region_table <- table(region)
region_table
round(prop.table(region_table),4)

#Number of bedrooms
bedroom <- hh$BDSP
br_table <- table(bedroom)
br_table
summary(hh$BDSP)
round(table(hh$BDSP)/nrow(hh),4)
round(207/nrow(hh),4)

#Units in structure
units <- hh$BLD
units[hh$BLD == 2 | hh$BLD == 3] <- "One family house"
units[hh$BLD %in% 4:9] <- "Apartment"
units[hh$BLD == 1 | hh$BLD == 10] <- "Other"
units[is.na(hh$BLD)]<-"NA"
units_table <- table(units)
units_table
round(prop.table(units_table),4)


#Household/family type
familytype <- hh$HHT
familytype[hh$HHT == 1] <- "Married couple household"
familytype[hh$HHT == 2] <- "Other:Male"
familytype[hh$HHT == 3] <- "Other:Female"
familytype[hh$HHT == 4] <- "Nonfamily:Male alone"
familytype[hh$HHT == 5] <- "Nonfamily:Male Not alone"
familytype[hh$HHT == 6] <- "Nonfamily:Female alone"
familytype[hh$HHT == 7] <- "Nonfamily:Female Not alone"
familytype[is.na(hh$HHT)] <- "NA"
familytype_table <- table(familytype)
prop.table(familytype_table)

#Presence and age of children
child <- hh$HUPAC
child[hh$HUPAC == 1] <- "With children under 6 years only"
child[hh$HUPAC == 2] <- "With children 6 to 17 years only"
child[hh$HUPAC == 3] <- "With children under 6 years and 6 to 17 years"
child[hh$HUPAC == 4] <- "No children"
child[is.na(hh$HUPAC)] <- "NA"
child_table <- table(child)
child_table
prop.table(child_table)

#When moved into this house or apartment
move <- hh$MV
move[hh$MV %in% 1:4] <- "< 10 years ago"
move[hh$MV %in% 5:7] <- "10 or more years ago"
move[is.na(hh$MV)] <- "NA"
move_table <- table(move)
move_table
prop.table(move_table)

#Number of persons in family
numperson <- hh$NPF
numperson[hh$NPF == 2] <- "2 persons"
numperson[hh$NPF == 3] <- "3 persons"
numperson[hh$NPF == 4] <- "4 persons"
numperson[hh$NPF >= 5] <- "5 or more persons"
numperson[is.na(hh$NPF)] <- "NA"
numperson_table <- table(numperson)
numperson_table
prop.table(numperson_table)

#Family income past 12 months
income <- hh$FINCP
income[hh$FINCP < 30000] <- "< $30,000"
income[hh$FINCP >=30000 & hh$FINCP <= 49999] <- "$30,000 - $49,999"
income[hh$FINCP >=50000 & hh$FINCP <= 69999] <- "$50,000 - $69,999"
income[hh$FINCP >=70000 & hh$FINCP <= 99999] <- "$70,000 - $99,999"
income[hh$FINCP >= 100000] <- "$100,000 or more"
income[is.na(hh$FINCP)] <- "NA"
income_table <- table(income)
income_table
round(prop.table(income_table),4)

#Type of unit
typeunit <- hh$TYPE
typeunit[hh$TYPE == 1] <- "Housing unit"
typeunit[hh$TYPE == 2] <- "Institutional group quarters"
typeunit[hh$TYPE == 3] <- "Noninstitutional group quarters"
typeunit_table <- table(typeunit)
typeunit_table
prop.table(typeunit_table)

#Response mode
resmode <- hh$RESMODE
resmode[hh$RESMODE == 1] <- "Mail"
resmode[hh$RESMODE == 2] <- "CATI/CAPI"
resmode[hh$RESMODE == 3] <- "Internet"
resmode[is.na(hh$RESMODE)] <- "NA"
resmode_table <- table(resmode)
resmode_table
prop.table(resmode_table)

################################################################################################################################
#############Person-Level Demographic Characteristics############################################################################
################################################################################################################################
#sex
sex <- mydata$SEX
sex[mydata$SEX == 1] <- "Male"
sex[mydata$SEX == 2] <- "Female"
sex_table <- table(sex)
sex_table
round(prop.table(sex_table),4)


#Marital status
marital <- mydata$MAR
marital[mydata$MAR == 1] <- "Married"
marital[mydata$MAR == 2] <- "Widowed"
marital[mydata$MAR == 3] <- "Divorced"
marital[mydata$MAR == 4] <- "Separated"
marital[mydata$MAR == 5] <- "Never married or under 15 years old"
marital_table <- table(marital)
marital_table
round(prop.table(marital_table),4)


#Class of worker
worker <-mydata$COW
worker[mydata$COW %in% 1:2] <- "Employee of a private company, business or organization"
worker[mydata$COW %in% 3:5] <- "Government employee"
worker[mydata$COW %in% 6:7] <- "Self-employed"
worker[mydata$COW %in% 8:9] <- "Working without pay or unemployed"
worker[is.na(mydata$COW)] <- "NA"
worker_table <- table(worker)
worker_table
round(prop.table(worker_table),4)


#Military service
military <- mydata$MIL
military[mydata$MIL == 1] <-"Now on active duty"
military[mydata$MIL == 2] <-"On active duty in the past, but not now"
military[mydata$MIL == 3] <-"Only on active duty for training in Reserves/National Guard"
military[mydata$MIL == 4] <-"Never served in the military"
military[is.na(mydata$MIL)] <- "less than 17 years old"
military_table <- table (military)
military_table
round(prop.table(military_table),4)


#Relationship
relp <- mydata$RELP
table(relp)
round(prop.table(table(relp)),4)


#Educational attainment
edu <- mydata$SCHL
edu[mydata$SCHL == 1] <- "No schooling completed"
edu[mydata$SCHL %in% 2:3] <- "Preschool & Kindergarten"
edu[mydata$SCHL %in% 4:9] <- "Elementary school"
edu[mydata$SCHL %in% 10:15] <- "Middle school & High school"
edu[mydata$SCHL %in% 16:17] <- "high school diploma & GED"
edu[mydata$SCHL %in% 18:19] <- "College students (no degree)"
edu[mydata$SCHL %in% 20:21] <- "Associate's and Bachelor's degree"
edu[mydata$SCHL %in% 22:24] <- "Graduate students & Professional degree"
edu[is.na(mydata$SCHL)] <- "less than 3 years old"
table(edu)
round(prop.table(table(edu)),4)



#Age in years
age <- mydata$AGEP
round(mean(age),4)
round(sd(age),4)



#Wages or salary income past 12 months
wage <- mydata$WAGP
wage[is.na(mydata$WAGP)] <- 0
round(mean(wage, na.rm = T),4)
round(sd(wage, na.rm = T),4)



#Usual hours worked per week past 12 months
hours <-mydata$WKHP
hours[is.na(mydata$WKHP)] <- 0
round(mean(hours, na.rm = T),4)
round(sd(hours, na.rm = T),4)
summary(hours, na.rm = T)



#Weeks worked within the past year
weeks <- mydata$WKW
weeks[mydata$WKW == 1] <- 51
weeks[mydata$WKW == 2] <- 48.5
weeks[mydata$WKW == 3] <- 43.5
weeks[mydata$WKW == 4] <- 33
weeks[mydata$WKW == 5] <- 20
weeks[mydata$WKW == 6] <- 6.5
weeks[is.na(mydata$WKW)] <- 0 
weeks
round(mean(weeks, na.rm = T),4)
round(sd(weeks, na.rm = T),4)

##Average hourly wages or salary income throughout the past 12 months during weeks worked
######????
hourswages <- wage/(hours * weeks)
hourswages
hourswages[is.na(hourswages)] <- 0
round(mean(hourswages, na.rm = T),4)
round(sd(hourswages, na.rm = T),4)

##############Characteristics by Survey Response Mode among Single Persons################
singel_person <- hh[which(hh$NP == 1),]
table(singel_person$NP)
table(singel_person$RESMODE)
merged.data <- merge(singel_person,person,by  = "serialno" )

mode1 <- merged.data[which(merged.data$RESMODE == 1),]
mode2 <- merged.data[which(merged.data$RESMODE == 2),]
mode3 <- merged.data[which(merged.data$RESMODE == 3),]

summary(mode1$AGEP)
sd(mode1$AGEP)
summary(mode2$AGEP)
sd(mode2$AGEP)
summary(mode3$AGEP)
sd(mode3$AGEP)

mode1_sex <- mode1$SEX
table(mode1_sex)
round(prop.table(table(mode1_sex)),4)
mode2_sex <- mode2$SEX
table(mode2_sex)
round(prop.table(table(mode2_sex)),4)
mode3_sex <- mode3$SEX
table(mode3_sex)
round(prop.table(table(mode3_sex)),4)
#mail worker
mode1_worker <- mode1$COW
mode1_worker[mode1$COW %in% 1:2] <- "Employee of a private company, business or organization"
mode1_worker[mode1$COW %in% 3:5] <- "Government employee"
mode1_worker[mode1$COW %in% 6:7] <- "Self-employed"
mode1_worker[mode1$COW %in% 8:9] <- "Working without pay or unemployed"
mode1_worker[is.na(mode1$COW)] <- "NA"
table(mode1_worker)
round(prop.table(table(mode1_worker)),4)
#CATI/CAPI worker
mode2_worker <- mode2$COW
mode2_worker[mode2$COW %in% 1:2] <- "Employee of a private company, business or organization"
mode2_worker[mode2$COW %in% 3:5] <- "Government employee"
mode2_worker[mode2$COW %in% 6:7] <- "Self-employed"
mode2_worker[mode2$COW %in% 8:9] <- "Working without pay or unemployed"
mode2_worker[is.na(mode2$COW)] <- "NA"
table(mode2_worker)
round(prop.table(table(mode2_worker)),4)
#
mode3_worker <- mode3$COW
mode3_worker[mode3$COW %in% 1:2] <- "Employee of a private company, business or organization"
mode3_worker[mode3$COW %in% 3:5] <- "Government employee"
mode3_worker[mode3$COW %in% 6:7] <- "Self-employed"
mode3_worker[mode3$COW %in% 8:9] <- "Working without pay or unemployed"
mode3_worker[is.na(mode3$COW)] <- "NA"
table(mode3_worker)
round(prop.table(table(mode3_worker)),4)


# & merged_cp$SEX == 1 
#################Husband and Wife Employment Characteristics among Married Couples#############
married_cp1 <- merged.data1[which(merged.data1$HHT == 1),]
cp <- married_cp1[which((merged_cp$RELP == 0 | merged_cp$RELP == 1)),]
nrow(cp)
husband <- cp[which(cp$SEX == 1),]
wife <- cp[which(cp$SEX == 2),]
nrow(husband)
nrow(wife)

#Wages or salary income in the past 12 months
husband_wage <- husband$WAGP
husband_wage[is.na(husband_wage)] <- 0
mean(husband_wage,na.rm = T)
sd(husband_wage,na.rm = T)
#if remove na, 29422.07, 53205.67
# sum(na) = 189 for both husband and wife
wife_wage <- wife$WAGP
wife_wage[is.na(wife_wage)] <- 0
sum(is.na(wife_wage))
mean(wife_wage,na.rm = T)
sd(wife_wage,na.rm = T)

#Travel time to work
husband_travel <- husband$JWMNP
sum(is.na(husband_travel))
husband_travel[is.na(husband_travel)] <- 0
round(mean(husband_travel,na.rm = T),2)
round(sd(husband_travel,na.rm = T),2)


wife_travel <- wife$JWMNP
sum(is.na(wife_travel))
wife_travel[is.na(wife_travel)] <- 0
round(mean(wife_travel,na.rm = T),2)
round(sd(wife_travel, na.rm = T),2)

#Usual hours worked per week past 12 months
husband_hours <- husband$WKHP
husband_hours[is.na(husband_hours)] <- 0
round(mean(husband_hours,na.rm = T),2)
round(sd(husband_hours,na.rm = T),2)

wife_hours <- wife$WKHP
husband_hours[is.na(husband_hours)] <- 0
round(mean(wife_hours, na.rm = T),2)
round(sd(wife_hours, na.rm = T),2)

#Average hourly wages or salary income throughout the past 12 months
husband$weeks[husband$WKW == 1] <- 51
husband$weeks[husband$WKW == 2] <- 48.5
husband$weeks[husband$WKW == 3] <- 43.5
husband$weeks[husband$WKW == 4] <- 33
husband$weeks[husband$WKW == 5] <- 20
husband$weeks[husband$WKW == 6] <- 6.5

wife$weeks[wife$WKW == 1] <- 51
wife$weeks[wife$WKW == 2] <- 48.5
wife$weeks[wife$WKW == 3] <- 43.5
wife$weeks[wife$WKW == 4] <- 33
wife$weeks[wife$WKW == 5] <- 20
wife$weeks[wife$WKW == 6] <- 6.5

husband$hourlywage <- husband$WAGP/(husband$WKHP*husband$weeks)
wife$hourlywage <-wife$WAGP/(wife$WKHP*wife$weeks)

husband_work1 <- husband[husband$COW %in% 1:2,]
husband_work2 <- husband[husband$COW %in% 3:5,]
husband_work3 <- husband[husband$COW %in% 6:7,]
husband_work4 <- husband[husband$COW %in% 8:9,]

wife_work1 <- wife[wife$COW %in% 1:2,]
wife_work2 <- wife[wife$COW %in% 3:5,]
wife_work3 <- wife[wife$COW %in% 6:7,]
wife_work4 <- wife[wife$COW %in% 8:9,]

round(mean(husband_work1$hourlywage,na.rm = T),2)
round(sd(husband_work1$hourlywage,na.rm = T),2)
round(mean(husband_work2$hourlywage,na.rm = T),2)
round(sd(husband_work2$hourlywage,na.rm = T),2)
round(mean(husband_work3$hourlywage,na.rm = T),2)
round(sd(husband_work3$hourlywage,na.rm = T),2)
round(mean(husband_work4$hourlywage,na.rm = T),2)
round(sd(husband_work4$hourlywage,na.rm = T),2)

round(mean(wife_work1$hourlywage,na.rm = T),2)
round(sd(wife_work1$hourlywage,na.rm = T),2)
round(mean(wife_work2$hourlywage,na.rm = T),2)
round(sd(wife_work2$hourlywage,na.rm = T),2)
round(mean(wife_work3$hourlywage,na.rm = T),2)
round(sd(wife_work3$hourlywage,na.rm = T),2)
round(mean(wife_work4$hourlywage,na.rm = T),2)
round(sd(wife_work4$hourlywage,na.rm = T),2)

#field of degree science and engineering
table(husband$SCIENGP)
round(prop.table(table(husband$SCIENGP)),4)
table(wife$SCIENGP)
round(prop.table(table(wife$SCIENGP)),4)
#round(,2)
##############Plot of Couple Average Age by Difference in Salary##############

rawdata <- merged_cp[, c("serialno", "SPORDER","RELP", "SEX","AGEP","WAGP", "WKHP")]
# selet only husband and wife in a household
rawdata_cp <- rawdata[which(rawdata$RELP == 0 | rawdata$RELP == 1),]
#deal with NA in hours
rawdata_cp$WKHP[is.na(rawdata_cp$WKHP)] <- 1
rawdata_cp$hourly <- rawdata_cp$WAGP/rawdata_cp$WKHP
library(dplyr)
group_cp <- group_by(rawdata_cp,serialno)
group_cp <- arrange(group_cp,serialno,SEX)
age_hourly <- summarise(group_cp, mean(AGEP),diff(hourly))
age_hourly$`diff(hourly)`<- -age_hourly$`diff(hourly)`
plot(age_hourly$`mean(AGEP)`,age_hourly$`diff(hourly)`,
     col="blue",
     xlab = "Average Age of Husband and Wifes",
     ylab ="Difference in Average Hourly Wages(Husband - wife)",
     main ="Plot of Couple Average Age by Difference in Salary")
abline(lm(age_hourly$`mean(AGEP)` ~ age_hourly$`diff(hourly)`))
lm(age_hourly$`mean(AGEP)` ~ age_hourly$`diff(hourly)`)








