
#This is a file to test the 2018 data and merge household and individual data. 
#The data is from the 2018 VASYR survey in Lebanon. 
#The data is in csv format. Load dplyr, ggplot and necessary packages:
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

#Get the working directory:
getwd()

#Load the data:

#Load the household data:
household_data <- read_csv("/Users/sofialozano/Desktop/EconHonors/Data/Raw/Honors_Thesis_Data/UNHCR_LBN_2018_VASyR_data/UNHCR_LBN_VASYR_2018_household_v2.1.csv")
#Load the individual data:
individual_data <- read_csv("/Users/sofialozano/Desktop/EconHonors/Data/Raw/Honors_Thesis_Data/UNHCR_LBN_2018_VASyR_data/UNHCR_LBN_VASYR_2018_individual_v2.1.csv")

#Check the data:
head(household_data)
head(individual_data)

#how many rows and columns are in the household data?
dim(household_data)
#how many rows and columns are in the individual data?
dim(individual_data)

#How many common formid in both datasets?
common_formid <- intersect(household_data$formid, individual_data$formid)
length(common_formid) #This means all the households have individual data.

#Merge the data:
#Merge the data by formid:
merged_data <- merge(household_data, individual_data, by = "formid")

#Dimention of the merged data:
dim(merged_data)

#What is the type of the variable age?
class(merged_data$age)
#What are the unique values of the variable age?
unique(merged_data$age)

#Create a new variable called children that is equal to 1 if the individual is less than 18 years old and 0 otherwise:
merged_data <- merged_data %>% mutate(children = ifelse(age < 18, 1, 0))

#How many 1s and 0s are in the children variable?
table(merged_data$children) #This means 12,463 individuals are children and 8897 are not children.

#Create a new variable for children under 5 years old, which equals 1 if the character variable age is "00 to 04":
merged_data <- merged_data %>% mutate(children_under_5 = ifelse(age == "00 to 04", 1, 0))

#how many 1s and 0s are in the children_under_5 variable?
table(merged_data$children_under_5) #This means 0 individuals are children under 5 years old.

#what is the frequency of age= "00 to 04"?
table(merged_data$age) #This means 4578 individuals are children under 5 years old.

#Create a new variable that counts how many children are in each household:
children_per_household <- merged_data %>% group_by(formid) %>% summarise(children_per_household = sum(children))

#Check the data:
head(children_per_household)

#what are the unique values of the variable hoh_relation
unique(merged_data$hoh_relation)
#how many are "Head of Household"?
table(merged_data$hoh_relation) #This means 4323 individuals are head of household.

#Create a new variable that is equal to 1 if the individual is the head of the household and 0 otherwise:
merged_data <- merged_data %>% mutate(is_head_of_household = ifelse(hoh_relation == "Head of Household", 1, 0))

#how many 1s and 0s are in the is_head_of_household variable?
table(merged_data$is_head_of_household) #This means 4323 individuals are head of household. The variable has been correctly created. 

#civil_status
#what are the unique values of the variable civil_status
unique(merged_data$civil_status)

#create a new variable that is equal to 1 if the individual is married and 0 otherwise:
merged_data <- merged_data %>% mutate(is_married = ifelse(civil_status == "Married (MA)", 1, 0))

#how many 1s and 0s are in the is_married variable?
table(merged_data$is_married) #This means 7954 individuals are married. The variable has been correctly created.

#what are the unique values of the variable not_enrolled_reasonschool_finish:
unique(merged_data$not_enrolled_reasonschool_finish)

#Create a new variable that is equal to 1 if the individual is not enrolled because they finished school and 0 otherwise:
merged_data <- merged_data %>% mutate(child_finished_school = ifelse(not_enrolled_reasonschool_finish == "Yes", 1, 0))

#How many 1s and 0s are in the child_finished_school variable?
table(merged_data$child_finished_school) #This means only 15 children finished school, and 6337 didn't.

#what values does the variable sex take?
unique(merged_data$sex)

#what are the unique values of legal_residency
unique(merged_data$legal_residency)

#Create a new variable that is equal to 1 if the individual has legal residency and 0 otherwise:
merged_data <- merged_data %>% mutate(has_legal_residency = ifelse(legal_residency == "Yes", 1, 0))

#create a new dataframe with the variables formid, children_under_5, is_head_of_household, is_married, child_finished_school, has_legal_residency, sex:
final_data <- merged_data %>% select(formid, children_under_5, is_head_of_household, is_married, child_finished_school, has_legal_residency, sex)

#export the final data to a csv file:
write.csv(final_data, "/Users/sofialozano/Desktop/EconHonors/Data/Processed/2018Test.csv", row.names = FALSE)

#summarize the final data:
summary(final_data)

#group final_data by formid
final_data <- final_data %>% group_by(formid)

summary(final_data)

#Make a new variable that counts the number of members in each household that are children under 5 years old:
children_under_5_per_household <- final_data %>% group_by(formid) %>% summarise(children_under_5_per_household = sum(children_under_5))


#now one that counts how many people in the household are married:
married_per_household <- final_data %>% group_by(formid) %>% summarise(married_per_household = sum(is_married))

#now create a variable that counts the sex of the head of household:


#SECOND ATTEMPT AT CREATING A HOUSEHOLD DATASET WITH INDIVIDUAL CHARACTERISTICS!!!

# Group final_data by formid
final_data_grouped <- final_data %>% group_by(formid)

# Create a new variable for the gender of the head of household (assuming 'Male' represents the head)
head_gender <- final_data_grouped %>%
  summarise(head_gender = ifelse(sum(is_head_of_household & sex == "Male") > 0, "Male", "Female"))

# Create a new variable for the number of children under 5 in each household
children_under_5_per_household <- final_data_grouped %>%
  summarise(children_under_5_per_household = sum(children_under_5))

# Create a new variable for legal residency status in each household
legal_residency_per_household <- final_data_grouped %>%
  summarise(legal_residency_per_household = ifelse(sum(has_legal_residency) > 0, 1, 0))

# Create a new variable for the number of married individuals in each household
married_per_household <- final_data_grouped %>%
  summarise(married_per_household = sum(is_married))

# Combine the new variables into a single dataset at the household level
household_dataset <- bind_cols(
  final_data_grouped %>% select(formid),
  head_gender,
  children_under_5_per_household,
  legal_residency_per_household,
  married_per_household
)

# Export the household dataset to a CSV file
write.csv(household_dataset, "/Users/sofialozano/Desktop/EconHonors/Data/Processed/Household_Data_2018.csv", row.names = FALSE)


#THIRD ATTEMPT, USING DR. MOYANO'S GUIDELINES. 

#what is the datatype of the variable age?
class(individual_data$age)
#what are the possible values of the variable age?
unique(individual_data$age)
#Create a new numeric variable for age, if these are the current values it takes: "30 to 34"   "35 to 39"   "40 to 44"   "25 to 29"   "50 to 54"   "45 to 49"   "60 or more" "20 to 24"   NA
#"15 to 19"   "55 to 59"   "10 to 14"   "05 to 09"   "00 to 04":
individual_data$age_numeric <- NA
individual_data$age_numeric[individual_data$age == "30 to 34"] <- 32
individual_data$age_numeric[individual_data$age == "35 to 39"] <- 37
individual_data$age_numeric[individual_data$age == "40 to 44"] <- 42
individual_data$age_numeric[individual_data$age == "25 to 29"] <- 27
individual_data$age_numeric[individual_data$age == "50 to 54"] <- 52
individual_data$age_numeric[individual_data$age == "45 to 49"] <- 47
individual_data$age_numeric[individual_data$age == "60 or more"] <- 65
individual_data$age_numeric[individual_data$age == "20 to 24"] <- 22
individual_data$age_numeric[individual_data$age == "15 to 19"] <- 17
individual_data$age_numeric[individual_data$age == "55 to 59"] <- 57
individual_data$age_numeric[individual_data$age == "10 to 14"] <- 12
individual_data$age_numeric[individual_data$age == "05 to 09"] <- 7
individual_data$age_numeric[individual_data$age == "00 to 04"] <- 2

#Show the distribution of the new variable age_numeric:
summary(individual_data$age_numeric)

#show the distribution of the variable relation to head of household:
summary(individual_data$hoh_relation)
#what are the unique values of the variable hoh_relation?
unique(individual_data$hoh_relation)

#Create a variable mapping the relation to head of household to a numeric value, called hoh_relation_numeric:
relation_mapping <- c("Head of Household" = 1,
                      "Wife/Husband" = 2,
                      "Brother/Sister" = 3,
                      "NA" = NA,
                      "Father-in-law/Mother-in-law" = 5,
                      "Extended family (uncle/aunt/cousin/niece/nephew etc)" = 6,
                      "Brother-in-law/Sister-in-law" = 7,
                      "No family relationship � gues" = 8)

# Create a new column with numeric codes
individual_data$hoh_relation_numeric <- relation_mapping[individual_data$hoh_relation]

#create the variable has_spouse, from the civil_status variable:
individual_data$has_spouse <- ifelse(individual_data$civil_status == "Married (MA)", 1, 0)

#Create a numeric variable called legal_residency_numeric from the legal_residency variable:
individual_data$legal_residency_numeric <- ifelse(individual_data$legal_residency == "Yes", 1, 0)

#I will create a dataset from the individual data at the household level and then will merge that with your household data.
individual_grouped <- individual_data %>%
  group_by(formid) %>%
  summarise(
    num_children_under_5 = sum(age_numeric < 5),
    is_hoh_female_and_relation_1 = as.integer(any(hoh_relation == 1 & sex == "Female")),
    mean_age_when_relation_1 = mean(age_numeric[hoh_relation_numeric == 1], na.rm = TRUE),
    has_legal_residency = as.integer(any(legal_residency_numeric == 1)),
    has_spouse = as.integer(any(has_spouse == 1)),
    count_over_60 = sum(age > 60, na.rm = TRUE)
  )

#Merge the household data with the individual data:
merged_data_2018 <- merge(household_data, individual_grouped, by = "formid")

#Save the merged data to a csv file:
write.csv(merged_data_2018, "/Users/sofialozano/Desktop/EconHonors/Data/Processed/Merged_Data_2018.csv", row.names = FALSE)


#Preliminary descriptive statistics:
#run conditional means for the variables of interest:
install.packages("cem")
library(cem)



#run the conditional means for the variables of interest, which include num_children_under_5, has_spouse, has_legal_residency,
#is_hoh_female_and_relation_1, mean_age_when_relation_1, count_over_60:
conditional_means <- cem(merged_data_2018, "num_children_under_5", "has_spouse", "has_legal_residency", "is_hoh_female_and_relation_1", "mean_age_when_relation_1", "count_over_60")

#show the conditional means:
conditional_means



