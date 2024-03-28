
#This is a file to test the 2021 data and merge household and individual data. 
#The data is from the 2021 VASYR survey in Lebanon. 
#The data is in csv format. Load dplyr, ggplot and necessary packages:
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

#Get the working directory:
getwd()

#Load the data:

#Load the household data:
household_data <- read_csv("/Users/sofialozano/Desktop/EconHonors/Data/Raw/Honors_Thesis_Data/UNHCR_LBN_2021_VASYR_data_v2/UNHCR_LBN_2021_VASYR_data_household_v2.1.csv")
#Load the individual data:
individual_data <- read_csv("/Users/sofialozano/Desktop/EconHonors/Data/Raw/Honors_Thesis_Data/UNHCR_LBN_2021_VASYR_data_v2/UNHCR_LBN_2021_VASYR_data_individual_v2.1.csv")

#Check the data:
head(household_data)
head(individual_data)

#mutate household_id to formid:
individual_data <- individual_data %>% mutate(formid = household_id)
household_data <- household_data %>% mutate(formid = household_id)

#1. EDUCATION, IND: highest grade completed by the individual

#use grepl to see which variables have the name edu:
names(individual_data)[grepl("edu", names(individual_data))]

#highest_grade is the variable that contains the highest grade completed by the individual.
#unique values of highest_grade:
unique(individual_data$highest_level_edu_s)
#mutate highest_level_edu_s to highest_grade:
individual_data <- individual_data %>% mutate(highest_grade = highest_level_edu_s)

#divide into categories, kindergarten, primary, secondary, university, and other:
individual_data$highest_grade_category <- NA
individual_data$highest_grade_category[individual_data$highest_grade %in% c("a. Nursery", "b. Kindergarten 1", "c. Kindergarten 2")] <- "Kindergarten"
individual_data$highest_grade_category[individual_data$highest_grade %in% c("d. Grade 1", "e. Grade 2", "f. Grade 3", "g. Grade 4", "h. Grade 5", "i. Grade 6")] <- "Primary"
individual_data$highest_grade_category[individual_data$highest_grade %in% c("j. Grade 7", "k. Grade 8", "l. Grade 9", "m. Grade 10", "n. Grade 11", "o. Grade 12", "r. TVET: Grade 7", "s. TVET: Grade 8", "t. TVET: Grade 9", "u. TVET: Grade 10", "v. TVET: Grade 11", "w.TVET: Grade 12")] <- "Secondary"
individual_data$highest_grade_category[individual_data$highest_grade == "q. University"] <- "University"
individual_data$highest_grade_category[individual_data$highest_grade %in% c("p. Technical course", "x.TVET License Technique")] <- "Technical course"
individual_data$highest_grade_category[individual_data$highest_grade == "y.Don’t Know"] <- "Illiterate"
individual_data$highest_grade_category[individual_data$highest_grade %in% c("NA")] <- NA

#see distribution of highest_grade_category:
table(individual_data$highest_grade_category)

#make a numerical variable for highest_grade_category:
individual_data$highest_grade_category_numeric <- NA
individual_data$highest_grade_category_numeric[individual_data$highest_grade_category == "Illiterate"] <- 0
individual_data$highest_grade_category_numeric[individual_data$highest_grade_category == "Kindergarten"] <- 1
individual_data$highest_grade_category_numeric[individual_data$highest_grade_category == "Primary"] <- 2
individual_data$highest_grade_category_numeric[individual_data$highest_grade_category == "Secondary"] <- 3
individual_data$highest_grade_category_numeric[individual_data$highest_grade_category == "Technical course"] <- 4
individual_data$highest_grade_category_numeric[individual_data$highest_grade_category == "University"] <- 5

#mutate highest_grade_category_numeric in the individual_data dataframe, so that it shows the value of the highest grade category for the head of household:
individual_data <- individual_data %>%
  group_by(formid) %>%
  mutate(highest_grade_category_numeric = max(highest_grade_category_numeric))

#how many formid dont have a highest_grade_category_numeric?
sum(!is.na(individual_data$highest_grade_category_numeric))

#check the distribution of the new variable highest_grade_category_numeric:
table(individual_data$highest_grade_category_numeric)
#how many nas are in the highest_grade_category_numeric variable?
sum(!is.na(individual_data$highest_grade_category_numeric))

table(individual_data$highest_grade_category_numeric)

#2. AGE, INDIVIDUAL: age of the individual

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

#What is the type of the variable age?
class(individual_data$age_numeric)
#What are the unique values of the variable age?
unique(individual_data$age_numeric)

#Create a new variable called children that is equal to 1 if the individual is less than 18 years old and 0 otherwise:
individual_data <- individual_data %>% mutate(children = ifelse(age < 18, 1, 0))

#How many 1s and 0s are in the children variable?
table(individual_data$children) #This means 12,463 individuals are children and 8897 are not children.

#Create a new variable for children under 5 years old, which equals 1 if the character variable age is "00 to 04":
individual_data <- individual_data %>% mutate(children_under_5 = ifelse(age == "00 to 04", 1, 0))

#how many 1s and 0s are in the children_under_5 variable?
table(individual_data$children_under_5) #This means 4578 individuals are children under 5 years old.

#create children5_to_14 variable:
individual_data <- individual_data %>% mutate(children_5to_14 = ifelse(age == "05 to 09" | age == "10 to 14", 1, 0))

#Create a new variable in the individual_data dataframe that counts how many children are in each household:
individual_data <- individual_data %>%
  group_by(formid) %>%
  mutate(children_per_household = sum(children))

#create a variable children under 5 per household using mutate
individual_data <- individual_data %>%
  group_by(formid) %>%
  mutate(under5_per_household = sum(children_under_5))

#5 to 14 per hh variable: 
#individual_data <- individual_data %>%
#group_by(formid) %>%
#mutate(children_5to_14_per_household = sum(children_5to_14))

#HEAD OF HOUSEHOLD, LEGAL STATUS AND CIVIL STATUS: 

#show the distribution of the variable relation to head of household:
#mutate rel_to_hoh_s to hoh_relation:
individual_data <- individual_data %>% mutate(hoh_relation = rel_to_hoh_s)
summary(individual_data$hoh_relation)
#what are the unique values of the variable hoh_relation?
unique(individual_data$hoh_relation)

#Create a variable mapping the relation to head of household to a numeric value, called hoh_relation_numeric:
relation_mapping <- c("a. Head of Household" = 1,
                      "b. Wife/Husband", "c. Mother/Father" = 2,
                      "d. Daughter/Son", "e. Brother/Sister", "g. Grandson/Granddaughter" = 3,
                      "NA", "Unknown" = NA,
                      "h. Father-in-law/Mother-in-law" = 5,
                      "f. Grandfather/Grandmother", "j. Extended family (uncle/aunt/cousin/niece/nephew etc)" = 6,
                      "i. Brother-in-law/Sister-in-law" = 7,
                      "k. No family relationship – guest", "l. Other, please specify" = 8)

# Create a new column with numeric codes
individual_data$hoh_relation_numeric <- relation_mapping[individual_data$hoh_relation]

#create variable is_head_of_household using mutate:
individual_data <- individual_data %>% mutate(is_head_of_household = ifelse(hoh_relation_numeric == "1", 1, 0))

#how many 1s and 0s are in the is_head_of_household variable?
table(individual_data$is_head_of_household) #This means 4975 individuals are head of household.

#create the variable has_spouse, from the civil_status variable:
#mutate marital_status_s to civil_status:
individual_data <- individual_data %>% mutate(civil_status = marital_status_s)
unique(individual_data$civil_status)
individual_data$has_spouse <- ifelse(individual_data$civil_status == "b.      Married", 1, 0)

#Create a numeric variable called legal_residency_numeric from the legal_residency variable:
#mutate legal_res_yn to legal_residency:
individual_data <- individual_data %>% mutate(legal_residency = legal_res_yn)
individual_data$legal_residency_numeric <- ifelse(individual_data$legal_residency == "a. Yes", 1, 0)

#convert to dummies the temp_illness, chronic_illness, serious_med_cond and mental_illness variables:
#use grepl to find illness:

# Create dummy variables for dis_dif_hearing_s
individual_data$dis_dif_hearing_dummy <- ifelse(individual_data$dis_dif_hearing_s %in% c("3. A lot of difficulty", "2. Some difficulty", "4. Cannot do at all"), 1, 0)
# Create dummy variables for dis_dif_seeing_s
individual_data$dis_dif_seeing_dummy <- ifelse(individual_data$dis_dif_seeing_s %in% c("3. A lot of difficulty", "2. Some difficulty", "4. Cannot do at all"), 1, 0)
# Create dummy variables for dis_dif_hands_s
individual_data$dis_dif_hands_dummy <- ifelse(individual_data$dis_dif_hands_s %in% c("3. A lot of difficulty", "2. Some difficulty", "4. Cannot do at all"), 1, 0)
# Create dummy variables for dis_dif_rais_jug_s
individual_data$dis_dif_rais_jug_dummy <- ifelse(individual_data$dis_dif_rais_jug_s %in% c("3. A lot of difficulty", "2. Some difficulty", "4. Cannot do at all"), 1, 0)
# Create a dummy variable for dis_dif_walking_s:
individual_data$dis_dif_walking_dummy <- ifelse(individual_data$dis_dif_walking_s %in% c("3. A lot of difficulty", "2. Some difficulty", "4. Cannot do at all"), 1, 0)

#create a dummy called serious_med_cond:
individual_data$serious_med_cond_dummy <- ifelse(rowSums(individual_data[, c("dis_dif_hearing_dummy", "dis_dif_seeing_dummy", "dis_dif_hands_dummy", "dis_dif_rais_jug_dummy", "dis_dif_walking_dummy")]) > 0, 1, 0)

#temp_illness: table(individual_data$child_sick_yn) a. Yes  b. No 
#create temp_illness_dummy variable, that's 1 if child_sick_yn is "a. Yes" and 0 otherwise:
individual_data$temp_illness_dummy <- ifelse(individual_data$child_sick_yn == "a. Yes", 1, 0)

#create dummy chronic_illness_dummy variable, that's 1 if chronic_illness_yn is "a. Yes" and 0 otherwise:
individual_data$chronic_illness_dummy <- ifelse(individual_data$chronic_illness_yn == "a. Yes", 1, 0)

#mental illness: "dis_child_learn_s", "dis_dif_remembering_s":
#create a dummy variable for dis_child_learn_s:
individual_data$dis_child_learn_dummy <- ifelse(individual_data$dis_child_learn_s %in% c("3. A lot of difficulty", "2. Some difficulty", "4. Cannot do at all"), 1, 0)
#create a dummy variable for dis_dif_remembering_s:
individual_data$dis_dif_remembering_dummy <- ifelse(individual_data$dis_dif_remembering_s %in% c("3. A lot of difficulty", "2. Some difficulty", "4. Cannot do at all"), 1, 0)

#create a dummy variable for mental illness:
individual_data$mental_illness_dummy <- ifelse(rowSums(individual_data[, c("dis_child_learn_dummy", "dis_dif_remembering_dummy")]) > 0, 1, 0)

#remove the variables that are not needed:
#individual_data <- individual_data %>% select(-temp_illness, -chronic_illness, -serious_med_cond, -mental_illness)

#sex variable:
#what are the unique values of the variable
unique(individual_data$gender_s)
#mutate gender_s into sex:
individual_data <- individual_data %>% mutate(sex = gender_s)
#turn into a dummy variable, where 1 is if the individual is female:
individual_data$sex_dummy <- ifelse(individual_data$sex == "b. Female", 1, 0)

#create a dummy variable hh_fem if (is_head_of_household ==1, & sex_dummy ==1):
individual_data$hh_fem <- ifelse(individual_data$is_head_of_household == 1 & individual_data$sex_dummy == 1, 1, 0)

#Create over 60 variable:
individual_data$over_60 <- ifelse(individual_data$age_numeric > 60, 1, 0)

#create a variable for the age of the head of household:
individual_data$age_hoh <- ifelse(individual_data$is_head_of_household == 1, individual_data$age_numeric, NA)

#put it at the beginning of the dataframe as second
individual_data1 <- individual_data %>% select(formid, age_hoh)

#create a new variable that gives the age of the head of household if its female and 0 otherwise:
individual_data$age_of_female_head <- ifelse(individual_data$sex_dummy == 1 & individual_data$is_head_of_household == 1, individual_data$age_numeric, 0)

#age of male head of household:
individual_data$age_of_male_head <- ifelse(individual_data$sex_dummy == 0 & individual_data$is_head_of_household == 1, individual_data$age_numeric, 0)

#WORK REGULARLY: 

#create work_regular_dummy variable:
individual_data$work_regular_dummy <- ifelse(individual_data$work_as_s %in% c("a. Work for someone else for pay (as employee, labourer, apprentice)",
                                                                              "b. Work in own/family farming or fishing",
                                                                              "c. Work in any other kind of business activity"), 1, 0)
table(individual_data$work_regular_dummy) #4093 people work regularly. 

#individual_data$work_regular_dummy <- ifelse(individual_data$work_regular == "Yes", 1, 0)

#is female and works regularly:
individual_data$female_and_work_regular <- ifelse(individual_data$sex_dummy == 1 & individual_data$work_regular_dummy == 1, 1, 0)
#is male and works regularly:
individual_data$male_and_work_regular <- ifelse(individual_data$sex_dummy == 0 & individual_data$work_regular_dummy == 1, 1, 0)

#is child and works regularly:
individual_data$child_and_work_regular <- ifelse(individual_data$age_numeric < 18 & individual_data$work_regular_dummy == 1, 1, 0)

#work_regular variable distribution:
table(individual_data$male_and_work_regular)

#create a work_regular_and_female variable
individual_data$work_regular_and_female <- ifelse(individual_data$work_regular_dummy == 1 & individual_data$sex_dummy == 1, 1, 0)

#create a dataframe with the variables formid, age_numeric, children_under_5, is_head_of_household, has_spouse, legal_residency
#sex_dummy, temp_illness_dummy, chronic_illness_dummy, serious_med_cond_dummy, mental_illness_dummy, hh_fem, over_60, age_hoh, female_and_work_regular, male_and_work_regular, child_and_work_regular:
individual_data1 <- individual_data %>% select (formid, children_5to_14, work_regular_and_female, child_and_work_regular, male_and_work_regular, female_and_work_regular,
                                                work_regular_dummy, age_of_male_head, age_of_female_head, 
                                                age_hoh,over_60,hh_fem, sex_dummy,legal_residency_numeric, has_spouse, is_head_of_household,
                                                children_under_5, highest_grade_category_numeric, age_numeric,
                                                children, temp_illness_dummy, chronic_illness_dummy, serious_med_cond_dummy, mental_illness_dummy)

table(individual_data1$age_hoh)

# Calculate max(age_hoh) separately
max_age_hoh <- individual_data1 %>%
  summarise(age_hoh = max(age_hoh, na.rm = TRUE))

# Group the data by formid and summarize all variables except age_hoh
individual_data1 <- individual_data1 %>%
  group_by(formid) %>%
  summarise_at(
    vars(-age_hoh),  # Exclude age_hoh from summarization
    .funs = list(~ sum(., na.rm = TRUE))  # Apply sum() to all variables except age_hoh
  )

# Combine the summarized data with max_age_hoh
individual_data1 <- inner_join(individual_data1, max_age_hoh, by = "formid")

# View the resulting dataframe
print(individual_data1)
table(individual_data1$age_hoh)

#turn the -Inf values in the age_hoh variable to NA:
individual_data1$age_hoh[individual_data1$age_hoh == -Inf] <- NA
table(individual_data1$age_hoh)

#group the data in individual_data1 dataframe by formid:
individual_data1 <- individual_data1 %>%
  group_by(formid) %>%
  summarise_all(~ sum(., na.rm = TRUE))

#unique formids in individual_data1:
length(unique(individual_data1$formid))

#there's an issue with the children_5to_14_per_household variable, it has numbers up to 55, but it should have the number of children 5 to 14 in the household.
#I think it has the sum of the ages of the children!

#Update: I think I fixed it now with this line, and by commenting the above ones. 
table(individual_data1$children_5to_14)

#-----------

#SELECT THE VARIABLES FROM THE HOUSEHOLD DATA:

#Create household size variable, from the total_num_hh_i variable:
household_data$household_size <- household_data$total_num_hh_i
table(household_data$household_size)

#I'll create a RCSI index, which is the reduced coping strategies. It's based on the following variables:
#relying on less preferred or less expensive food:
#less_expensive
#reducing the portion size of meals and the number of meals eaten per day
#reduced_meals
#reduced_portion
#borrowing food or relying on help from friends or relatives
#borrowed_food
#restricting food consumption by adults in order for children to eat and by female heads of households
#restrict_consumption
#spending days without eating
#days_nofood
#sending household members to eat somewhere else
#eat_elsewhere

#Create a rCSI variable, that's the weighted average of the above variables, where each has a weight of less_expensive: 1, (reduced_meals+reduced_portion)/2: 1, borrowed_food: 2, restrict_consumption: 1, days_nofood: 3, eat_elsewhere: 1
#use grepl to find which variables have the name less expensive:
names(household_data)[grepl("else", names(household_data))]

household_data$rCSI <- (household_data$less_expensive_i +
                          (household_data$reduced_meals_i + household_data$reduced_portion_i)/2+
                          2*household_data$borrowed_food_i +
                          household_data$restrict_consumption_i +
                          3*household_data$days_nofood_i +
                          household_data$eat_elsewhere_i)/6

table(household_data$rCSI)
summary(household_data$rCSI)

#govenorate: perform the cleaning operations on governorate: 
table(household_data$governorate)

#create 8 dummies one for each governorate:
household_data$Akkar_dummy <- ifelse(household_data$governorate == "Akkar", 1, 0)
household_data$BaalbekHermel_dummy <- ifelse(household_data$governorate == "Baalbek Hermel", 1, 0)
household_data$Beirut_dummy <- ifelse(household_data$governorate == "Beirut", 1, 0)
household_data$Beqaa_dummy <- ifelse(household_data$governorate == "Beqaa", 1, 0)
household_data$MountLebanon_dummy <- ifelse(household_data$governorate == "Mount Lebanon", 1, 0)
household_data$North_dummy <- ifelse(household_data$governorate == "North Lebanon", 1, 0)
household_data$South_dummy <- ifelse(household_data$governorate == "South Lebanon", 1, 0)
household_data$Nabatieh_dummy <- ifelse(household_data$governorate == "El Nabatieh", 1, 0)

#coping mechanisms:

#cleaning the variables: 

#create a food_oncredit dummy: 
household_data$food_oncredit_dummy <- NA
household_data$food_oncredit_dummy[household_data$bought_food_credit_yn %in% c("a. Yes")] <- 1
household_data$food_oncredit_dummy[household_data$bought_food_credit_yn %in% c("c. No because HH had already done it and cannot continue doing it", "d. Non applicable HH do/did not have", "b. No, not need to do it")] <- 0

table(household_data$food_oncredit_dummy)

#create a spent_saving dummy variable: 
household_data$spent_saving_dummy <- NA
household_data$spent_saving_dummy[household_data$spent_hh_savings_yn %in% c("a. Yes")] <- 1
household_data$spent_saving_dummy[household_data$spent_hh_savings_yn %in% c("c. No because HH had already done it and cannot continue doing it", "d. Non applicable HH do/did not have", "b. No, not need to do it")] <- 0

table(household_data$spent_saving_dummy)

#create a selling_goods dummy variable: 
household_data$selling_goods_dummy <- NA
household_data$selling_goods_dummy[household_data$sold_hh_goods_yn %in% c("a. Yes")] <- 1
household_data$selling_goods_dummy[household_data$sold_hh_goods_yn %in% c("c. No because HH had already done it and cannot continue doing it", "d. Non applicable HH do/did not have", "b. No, not need to do it")] <- 0

table(household_data$selling_goods_dummy)

#create a variable called stress_coping, that is 1 if household has adopted either of the following
household_data$stress_coping <- ifelse(household_data$food_oncredit_dummy ==1 | household_data$spent_saving_dummy ==1 |
                                         household_data$selling_goods_dummy ==1, 1, 0)

table(household_data$stress_coping)

#crisis_coping

#mutate withd_chld_school_yn to no_school:
household_data <- household_data %>% mutate(no_school = withd_chld_school_yn)
#mutate red_non_food_exp_edu_yn to reduce_edu:
household_data <- household_data %>% mutate(reduce_edu = red_non_food_exp_edu_yn)
#mutate red_non_food_exp_health_yn to reduce_essential:
household_data <- household_data %>% mutate(reduce_essential = red_non_food_exp_health_yn)
#mutate child_mariage_yn to child_mariage:
household_data <- household_data %>% mutate(child_mariage = child_mariage_yn)

#create a dummy for each of the variables in the list:

# List of variables you want to apply the operation to
variables_to_process <- c("reduce_edu", "reduce_essential", "child_mariage", "no_school")

# Apply the operation to each variable and create new variables with "_dummy"
for (variable in variables_to_process) {
  household_data[[paste0(variable, "_dummy")]] <- ifelse(household_data[[variable]] %in% c("a. Yes"), 1,
                                                         ifelse(household_data[[variable]] %in% c("c. No because HH had already done it and cannot continue doing it", 
                                                                                                  "d. Non applicable HH do/did not have", 
                                                                                                  "b. No, not need to do it"), 0, NA))
}

table(household_data$child_mariage_dummy)

#create crisis_coping
household_data$crisis_coping <- ifelse(household_data$reduce_edu_dummy == 1 | 
                                         household_data$reduce_essential_dummy == 1 |
                                         household_data$child_mariage_dummy == 1 |
                                         household_data$no_school_dummy == 1, 1, 0)


table(household_data$crisis_coping)

#emergency_coping 
table(household_data$sold_house)
table(household_data$selling_assets)
table(household_data$child_exploitative_work)
table(household_data$begging)
table(household_data$child_labour)

#mutate sold_assets_yn to selling_assets:
household_data <- household_data %>% mutate(selling_assets = sold_assets_yn)
#mutate sold_house_yn to sold_house:
household_data <- household_data %>% mutate(sold_house = sold_house_yn)
#mutate child_exploit_work_yn to child_exploitative_work:
household_data <- household_data %>% mutate(child_exploitative_work = child_exploit_work_yn)
#mutate child_labour_yn to child_labour:
household_data <- household_data %>% mutate(child_labour = child_labour_yn)
#mutate begging_yn to begging:
household_data <- household_data %>% mutate(begging = begging_yn)

# List of variables you want to create dummy variables for
variables_to_process <- c("sold_house", "selling_assets", "child_exploitative_work", "begging", "child_labour")

# Apply the operation to each variable and create new variables with "_dummy"
for (variable in variables_to_process) {
  household_data[[paste0(variable, "_dummy")]] <- ifelse(household_data[[variable]] == "a. Yes", 1,
                                                         ifelse(household_data[[variable]] %in% c("c. No because HH had already done it and cannot continue doing it", 
                                                                                                  "d. Non applicable HH do/did not have", 
                                                                                                  "b. No, not need to do it"), 0, NA))
}

# Create dummy variable indicating emergency coping
household_data <- household_data %>%
  mutate(emergency_coping = ifelse(sold_house_dummy == 1 | selling_assets_dummy == 1 |
                                     begging_dummy == 1 |
                                     child_labour_dummy == 1 | child_exploitative_work_dummy == 1, 1, 0))

table(household_data$emergency_coping)

#borrow_sourcefriends_leb #3542 households. table(household_data$borrow_money_src_m_frnd_leb)
#borrow_sourcefriends_not_leb #91 households. borrow_money_src_m_frnd_no_leb
#borrow_sourcelocal_charity #9 households. borrow_money_src_m_loc_charity
#reason_borrowingfood #2975 households.prim_reason_credit_m_food

#create dummies: 
household_data$borrow_sourcefriends_leb_dummy <- household_data$borrow_money_src_m_frnd_leb
household_data$borrow_sourcefriends_not_leb_dummy <- household_data$borrow_money_src_m_frnd_no_leb
household_data$borrow_sourcelocal_charity_dummy <- household_data$borrow_money_src_m_loc_charity
household_data$reason_borrowingfood_dummy <- household_data$prim_reason_credit_m_food

#ASSETS: 
#cleaning_items_yn
#female_hygiene_yn
#baby_care_yn
#pers_hygiene_yn
#have_internet_phone_yn
#have_internet_wifi_yn
#electricity_access_yn
#smart_phone_yn
#mobile_telephone_yn

table(household_data$info_services_s_2)

#info_services_s_2
#expenditure_clothing
#expenditure_transport

#create a new variable sms_yn equal to 1 if info_services_s_2 is a. SMS:
household_data$sms_yn <- ifelse(household_data$info_services_s_2 == "a. SMS", 1, 0)
table(household_data$sms_yn)

#------ 
# 
# variables_to_process <- c("cleaning_items_yn", "female_hygiene_yn", "baby_care_yn",
#                           "pers_hygiene_yn", "have_internet_phone_yn", "have_internet_wifi_yn",
#                           "electricity_access_yn", "smart_phone_yn", "mobile_telephone_yn", "wash_drinking_yn" )
# 
# # Apply the operation to each variable and create new variables with "_dummy"
# for (variable in variables_to_process) {
#   household_data[[paste0(variable, "_dummy")]] <- ifelse(household_data[[variable]] == "a. Yes", 1,
#                                                          ifelse(household_data[[variable]] %in% c("b. No"), 0, NA))
# }
# 
# #summary(household_data$expenditure_clothing)
# #create transport_dummy if household spent more than 29417 on transport:
# #household_data$transport_dummy <- ifelse(household_data$expenditure_transport > 29417, 1, 0)
# #create clothing_dummy if household spent more than 5136 on clothing:
# #household_data$clothing_dummy <- ifelse(household_data$expenditure_clothing > 5136, 1, 0)
# 
# 
# #Create a variable called total assets that sums the following (all the assets are in the format TRUE FALSE)
# #create dummies with all of the assets: 
# variables_to_process <- c("cleaning_items_yn_dummy", "female_hygiene_yn_dummy", "baby_care_yn_dummy",
#                           "pers_hygiene_yn_dummy", "have_internet_phone_yn_dummy", "have_internet_wifi_yn_dummy",
#                           "electricity_access_yn_dummy", "smart_phone_yn_dummy","mobile_telephone_yn_dummy", "transport_dummy", "clothing_dummy")
# 
# 
# #For this year: refrigerator, sewing_machine, etc. doesn't exist. Excluded from asset list. There are limited data on assets.
# 
# # Apply the operation to each variable and create new dummy variables
# #for (variable in variables_to_process) {
# #household_data[[paste0(variable, "_dummy")]] <- as.integer(household_data[[variable]])
# #}
# 
# #mutate household_id into formid:
# #household_data$formid <- household_data$household_id
# 
# #create a new dataframe with the dummies:
# household_data1 <- household_data %>% select(formid, cleaning_items_yn_dummy, female_hygiene_yn_dummy, baby_care_yn_dummy,
#                                              pers_hygiene_yn_dummy, have_internet_phone_yn_dummy, have_internet_wifi_yn_dummy,
#                                              electricity_access_yn_dummy, smart_phone_yn_dummy, mobile_telephone_yn_dummy, transport_dummy, clothing_dummy)
# 
# #create total assets variable, that sums all the recent dummy variables:
# household_data1$total_assets <- rowSums(household_data1[, 2:12], na.rm = TRUE)
# 
# #create a basic_household_assets variable:
# # List of asset categories and corresponding dummy variables
# asset_categories <- list(
#   basic_household_assets = c("cleaning_items_yn_dummy", "female_hygiene_yn_dummy", "baby_care_yn_dummy", "pers_hygiene_yn_dummy", "clothing_dummy"),
#   appliance_assets = c("electricity_access_yn_dummy", "have_internet_phone_yn_dummy", "have_internet_wifi_yn_dummy")
# )
# 
# # Create basic_household_assets variable
# household_data$basic_household_assets <- rowSums(household_data[asset_categories$basic_household_assets], na.rm = TRUE)
# 
# # Create appliance_assets variable
# household_data$appliance_assets <- rowSums(household_data[asset_categories$appliance_assets], na.rm = TRUE)
# 
# table(household_data$basic_household_assets)
# table(household_data$appliance_assets)
# 
# #communication_assets:
# # Create communication_assets variable
# household_data$communication_assets <- rowSums(household_data[, c("have_internet_phone_yn_dummy", "have_internet_wifi_yn_dummy", "smart_phone_yn_dummy", "mobile_telephone_yn_dummy")])
# 
# table(household_data$communication_assets)

#mutate have_internet_phone_yn_dummy to mobile_phone_dummy
# household_data$mobile_phone_dummy <- household_data$have_internet_phone_yn_dummy
# table(household_data$mobile_phone_dummy)

#transportation_assets: 
# #create transportation_assets variable:
# household_data$transportation_assets <- rowSums(household_data[, c("transport_dummy")])
# table(household_data$transportation_assets)


#NEW ASSET VARIABLES WITH CONSISTENT NAMING AND MEASURING ACROSS YEARS: 
names(household_data)[grepl("shel", names(household_data))]

#products variables: 
table(household_data$pers_hygiene_yn)
table(household_data$female_hygiene_yn)
table(household_data$cleaning_items_yn)
table(household_data$baby_care_yn)

#mobile phone, internet, electricity access, wifi, make a table
table(household_data$mobile_telephone_yn)
table(household_data$have_internet_phone_yn)
table(household_data$electricity_access_yn)

#"damaged_shelter" "damaged_roof, leaking_roof, "damage_walls", "damage_plumbing", rottenness, unsealed_windows:
table(household_data$leaking_roof_yn)
table(household_data$damaged_shelter_yn)
table(household_data$damaged_roof_yn)
table(household_data$damage_walls_yn)
table(household_data$water_pipes_not_func_yn)
table(household_data$rottenness_leak_yn)
table(household_data$unsealed_win_doors_yn)

#turn into dummies:
household_data$leaking_roof_dummy <- ifelse(household_data$leaking_roof_yn == "a. Yes", 1, 0)
household_data$damaged_shelter_dummy <- ifelse(household_data$damaged_shelter_yn == "a. Yes", 1, 0)
household_data$damaged_roof_dummy <- ifelse(household_data$damaged_roof_yn == "a. Yes", 1, 0)
household_data$damage_walls_dummy <- ifelse(household_data$damage_walls_yn == "a. Yes", 1, 0)
household_data$pipes_dummy <- ifelse(household_data$water_pipes_not_func_yn == "a. Yes", 1, 0)
household_data$rottenness_dummy <- ifelse(household_data$rottenness_leak_yn == "a. Yes", 1, 0)
household_data$unsealed_windows_dummy <- ifelse(household_data$unsealed_win_doors_yn == "a. Yes", 1, 0)

#turn pers_hygiene_yn
household_data$pers_hygiene_dummy <- ifelse(household_data$pers_hygiene_yn == "a. Yes", 1, 0)
household_data$female_hygiene_dummy <- ifelse(household_data$female_hygiene_yn == "a.       Yes", 1, 0)
household_data$cleaning_items_dummy <- ifelse(household_data$cleaning_items_yn == "a. Yes", 1, 0)
household_data$baby_care_dummy <- ifelse(household_data$baby_care_yn == "a.       Yes", 1, 0)

#turn mobile_telephone_yn
household_data$mobile_phone_dummy <- ifelse(household_data$mobile_telephone_yn == "a. Yes", 1, 0)
household_data$have_internet_phone_dummy <- ifelse(household_data$have_internet_phone_yn == "a. Yes", 1, 0)
household_data$electricity_access_dummy <- ifelse(household_data$electricity_access_yn == "a. Yes", 1, 0)

# Create basic_household_assets variable, that sums "leaking_roof", "damaged_shelter", "damaged_roof", "damage_walls", "damage_plumbing", "rottenness", "unsealed_windows":
household_data$basic_household_assets <- rowSums(household_data[, c("leaking_roof_dummy", "damaged_shelter_dummy", "damaged_roof_dummy", "damage_walls_dummy", "pipes_dummy", "rottenness_dummy", "unsealed_windows_dummy")])

# Create appliance_assets variable, that sums "personal_hygiene_dummy", "baby_care_dummy", "cleaning_items_dummy", "female_hygiene_dummy":
household_data$appliance_assets <- rowSums(household_data[, c("pers_hygiene_dummy", "baby_care_dummy", "cleaning_items_dummy", "female_hygiene_dummy")])

#communication_assets:
household_data$communication_assets <- rowSums(household_data[, c("electricity_access_dummy", "have_internet_phone_dummy")])

#mobile phone dummy:
table(household_data$mobile_phone_dummy)

#-------

#water expenditure: 
#expenditure_water
table(household_data$wash_drinking_yn)

#create a dummy called spent_on_water, if the drink_water_source is "Bottled mineral water""
household_data$spent_on_water <- ifelse(household_data$wash_drinking_yn == "a. Yes", 1, 0)

#household expenditure:

#household expenditure:
#summary of the variable:
summary(household_data$total_expenditure)

#nas in total expenditure:
#sum(is.na(household_data$total_expenditure))

#nas in exp_dic
#sum(is.na(household_data$exp_dic))
#use grepl to see variables that have the word exp:
#names(household_data)[grepl("exp", names(household_data))]

#create dummy variable high_expenditure_d, if the expenditure_total is greater than the mean expenditure for the district to which the household belongs:
#mean_expenditure <- household_data %>% 
#  group_by(governorate) %>% 
#  summarise(mean_expenditure = mean(total_expenditure, na.rm = TRUE))

# Create dummy variable high_expenditure
#household_data <- household_data %>%
#  mutate(high_expenditure_d = total_expenditure > mean_expenditure)

#summary of the high_expenditure_d variable:
#table(household_data$high_expenditure_d)
#turn into a dummy:
#household_data$high_expenditure_dummy <- ifelse(household_data$high_expenditure_d == "TRUE", 1, 0)

#summary of the variable:
#summary(household_data$high_expenditure_dummy)

#Create a high expenditure per capita, by governorate variable: 
# Calculate expenditure per capita for each household
household_data <- household_data %>%
  mutate(expenditure_per_capita = total_expenditure / household_size)

# Calculate mean expenditure per capita by governorate
mean_expenditure_per_capita <- household_data %>% 
  group_by(governorate) %>% 
  summarise(mean_expenditure_per_capita = mean(expenditure_per_capita, na.rm = TRUE))

# Merge mean expenditure per capita back into the original dataframe
household_data <- merge(household_data, mean_expenditure_per_capita, by = "governorate", suffixes = c("", "_gov"))

# Create dummy variable high_expenditure_per_capita
household_data <- household_data %>%
  mutate(high_expenditure_per_capita = expenditure_per_capita > mean_expenditure_per_capita)

#turn into a dummy:
household_data$high_expenditure_per_capita_dummy <- ifelse(household_data$high_expenditure_per_capita == "TRUE", 1, 0)

table(household_data$high_expenditure_per_capita_dummy)

#food consumption variables:
#create the food consumption score:
#use grepl to find the variables that have the word cons in them:
names(household_data)[grepl("spice", names(household_data))]

household_data$FCScore <- (ifelse(is.na(household_data$num_days_tubers_cons_i), 0, household_data$num_days_tubers_cons_i) * 3) + 
  (ifelse(is.na(household_data$num_days_cereal_cons_i), 0, household_data$num_days_cereal_cons_i) * 2) +
  (ifelse(is.na(household_data$num_days_green_leafy_i), 0, household_data$num_days_green_leafy_i) * 1) + 
  (ifelse(is.na(household_data$num_days_orange_fruits_i), 0, household_data$num_days_orange_fruits_i) * 1) +
  (ifelse(is.na(household_data$num_days_meat_fish_i), 0, household_data$num_days_meat_fish_i) * 4) + 
  (ifelse(is.na(household_data$num_days_milk_i), 0, household_data$num_days_milk_i) * 4) +
  (ifelse(is.na(household_data$num_days_sugar_i), 0, household_data$num_days_sugar_i) * 0.5) + 
  (ifelse(is.na(household_data$num_days_oil_i), 0, household_data$num_days_oil_i) * 0.5) +
  (ifelse(is.na(household_data$value_food_spices_dec), 0, household_data$value_food_spices_dec) * 0)

summary(household_data$FCScore)

#curfew variables:
names(household_data)[grepl("curfew", names(household_data))]
table(household_data$curfews_yn)
household_data$curfew_dummy <- ifelse(household_data$curfews_yn == "a. Yes", 1, 0)

#technology access:
table(household_data$sms_yn) #most get their info from sms. 
household_data$info_servicessms_dummy <- household_data$sms_yn

#toilet sharing:
table(household_data$share_toilets_hh_yn)
#sharing_bathroom #number of people sharing bathroom
#shared_toilets #yes/no
household_data$shared_toilets_dummy <- ifelse(household_data$share_toilets_hh_yn == "a. Yes", 1, 0)

#arrival at same time:
table(household_data$free_movement_yn) #yes/no
#insecurity
household_data$insecurity_dummy <- ifelse(household_data$free_movement_yn == "a. Yes", 1, 0)

#plan to stay next 3 months
#changed_accom_yn
#changed_accom_6months_yn
table(household_data$changed_accom_6months_yn)
#see na values in changed_accom_yn
sum(is.na(household_data$changed_accom_6months_yn))
table(household_data$changed_accom_6months_yn)

household_data$accom_short_living_period_dummy <- ifelse(household_data$changed_accom_yn == "a. Yes", 1, 0)

#beneficiary of assistance:

#beneficiary of primary healthcare/received benefits:
table(household_data$health_required1) #yes/no. Convert to dummies to be able to use in regression: 

#mutate prim_health_care_yn to health required 
household_data$health_required1_dummy <- ifelse(household_data$prim_health_care_yn == "a. Yes", 1, 0)

#prim_health_case_ass_yn : health access
household_data$health_access1_dummy <- ifelse(household_data$prim_health_case_ass_yn == "a. Yes", 1, 0)

#create a ecard_dummy
household_data$ecard_dummy <- ifelse(
  replace(household_data$main_income_src_s == "j. E-cards used in WFP FOOD SHOPS", is.na(household_data$main_income_src_s), FALSE) |
    replace(household_data$sec_income_src_s == "j. E-cards used in WFP FOOD SHOPS", is.na(household_data$sec_income_src_s), FALSE) |
    replace(household_data$third_income_src_s == "j. E-cards used in WFP FOOD SHOPS", is.na(household_data$third_income_src_s), FALSE),
  1, 0)

table(household_data$ecard_dummy)

#include the following variables too: 
names(household_data)[grepl("inc", names(household_data))]

#mutate main_income_src_s to first_income_sources:
household_data <- household_data %>% mutate(first_income_sources = main_income_src_s)
#mutate total_income_dec to total_income: 
household_data <- household_data %>% mutate(total_income = total_income_dec)
#create calcexpenditure_food_usd with NA: 
household_data$calcexpenditure_food_usd <- NA
#expenditure_food


#include the variables in the household_data1 dataframe:

household_data1 <- household_data %>% select (formid, household_size, governorate, Akkar_dummy, BaalbekHermel_dummy, Beirut_dummy, Beqaa_dummy, MountLebanon_dummy, 
                                              North_dummy, South_dummy, Nabatieh_dummy, stress_coping, crisis_coping, emergency_coping, borrow_sourcefriends_not_leb_dummy,
                                              borrow_sourcefriends_leb_dummy, borrow_sourcelocal_charity_dummy, reason_borrowingfood_dummy, basic_household_assets,
                                              appliance_assets, communication_assets, mobile_phone_dummy, spent_on_water, FCScore, rCSI,
                                              curfew_dummy, info_servicessms_dummy, shared_toilets_dummy, insecurity_dummy, accom_short_living_period_dummy, health_required1_dummy, 
                                              health_access1_dummy, ecard_dummy, child_labour_dummy, high_expenditure_per_capita_dummy, calcexpenditure_food_usd,
                                              expenditure_food, total_income, first_income_sources
)

#excluded total_assets, as it's not in this DF. 
#transportation_assets, excluded. 

#-------------------------------------------------------------------------------

#Merging household_data1 and individual_data1 dataframes:
merged_df <- merge(household_data1, individual_data1, by = "formid", all = TRUE)

#Which variables have many NAs?
colSums(is.na(merged_df))

setwd("/Users/sofialozano/Desktop/EconHonors")

#save merged dataframe as a csv file:
write.csv(merged_df, "merged_df_2021.csv")

table(merged_df$North_dummy)

#running my regressions:
#run a first regression called model:
# Fit the linear regression model
model <- lm(FCScore ~ child_labour_dummy + children + child_labour_dummy:children, data = merged_df)

# Print the summary of the model
summary(model)

#run a second regression called model1:
# Fit the linear regression model
model1 <- lm(FCScore ~ info_servicessms_dummy * mobile_phone_dummy + ecard_dummy, data = merged_df)
summary(model1)

governorate_dummies <- merged_df[, c("Akkar_dummy", "Beirut_dummy", "MountLebanon_dummy", 
                                     "North_dummy", "South_dummy", "Beqaa_dummy", 
                                     "BaalbekHermel_dummy", "Nabatieh_dummy")]

governorate_dummies_subset <- governorate_dummies[, -which(names(governorate_dummies) == "Beirut_dummy")]

# Compute correlation matrix
correlation_matrix <- cor(governorate_dummies_subset)

# View correlation matrix
print(correlation_matrix)

#display the correlation matrix using the corrplot package:
install.packages("corrplot")
library(corrplot)

# Create a correlation matrix
correlation_matrix <- cor(governorate_dummies_subset)

# Create a correlation plot
corrplot(correlation_matrix, method = "number")

#Overall, the presence of a mobile phone and an e-card has a significant impact on FCScore, while the presence of sms services alone does not significantly affect FCScore.

#run a third regression called model2:
# Fit the linear regression model
model2 <- lm(FCScore ~ shared_toilets_dummy + insecurity_dummy + accom_short_living_period_dummy + health_required1_dummy + health_access1_dummy, data = merged_df)
summary(model2)

#run a fourth regression called model3:
model3 <- lm(FCScore ~ female_and_work_regular + hh_fem + Akkar_dummy + Beirut_dummy + MountLebanon_dummy + Beqaa_dummy + North_dummy + South_dummy + BaalbekHermel_dummy + health_access1_dummy, data = merged_df)
summary(model3)

#which variables in household_data contain the word "insecurity"?
#use grepl to find the variables that contain the word "insecurity"
names(household_data)[grepl("insecurity", names(household_data))]

table(household_data$insecurity_causelocal_orgs)

#run a model with all the variables: 
# Define the formula for the regression model. Here, i'm controlling for location using Akkar as the reference group: 
formula <- "FCScore ~ 
            stress_coping + crisis_coping + emergency_coping + 
            borrow_sourcefriends_not_leb_dummy + borrow_sourcefriends_leb_dummy + 
            borrow_sourcelocal_charity_dummy + reason_borrowingfood_dummy + 
            basic_household_assets + appliance_assets + communication_assets + 
            mobile_phone_dummy + transportation_assets + spent_on_water + 
            high_expenditure + curfew_dummy + info_servicessms_dummy + 
            shared_toilets_dummy + insecurity_dummy + accom_short_living_period_dummy + 
            health_required1_dummy + health_access1_dummy + ecard_dummy + 
            child_labour_dummy + work_regular_and_female + child_and_work_regular + 
            male_and_work_regular + female_and_work_regular + work_regular_dummy + 
            age_of_male_head + age_of_female_head + age_hoh + over_60 + hh_fem + 
            sex_dummy + legal_residency_numeric + has_spouse + is_head_of_household + 
            children_under_5 + highest_grade_category_numeric + age_numeric + 
            children + temp_illness_dummy + chronic_illness_dummy + 
            serious_med_cond_dummy + mental_illness_dummy + 
            BaalbekHermel_dummy + Beirut_dummy + Beqaa_dummy + 
            MountLebanon_dummy + North_dummy + South_dummy + Nabatieh_dummy"

# Run the OLS regression model
model4 <- lm(formula, data = merged_df)

# Summarize the model
summary(model4)

#Run a model5, another regression that has rCSI as an output, and the same variables as model4 as inputs:
# Define the formula for the regression model
formula <- "rCSI ~ 
            stress_coping + crisis_coping + emergency_coping + 
            borrow_sourcefriends_not_leb_dummy + borrow_sourcefriends_leb_dummy + 
            borrow_sourcelocal_charity_dummy + reason_borrowingfood_dummy + 
            basic_household_assets + appliance_assets + communication_assets + 
            mobile_phone_dummy + transportation_assets + spent_on_water + 
            high_expenditure + curfew_dummy + info_servicessms_dummy + 
            shared_toilets_dummy + insecurity_dummy + accom_short_living_period_dummy + 
            health_required1_dummy + health_access1_dummy + ecard_dummy + 
            child_labour_dummy + work_regular_and_female + child_and_work_regular + 
            male_and_work_regular + female_and_work_regular + work_regular_dummy + 
            age_of_male_head + age_of_female_head + age_hoh + over_60 + hh_fem + 
            sex_dummy + legal_residency_numeric + has_spouse + is_head_of_household + 
            children_under_5 + highest_grade_category_numeric + age_numeric + 
            children + temp_illness_dummy + chronic_illness_dummy + 
            serious_med_cond_dummy + mental_illness_dummy + 
            BaalbekHermel_dummy + Beirut_dummy + Beqaa_dummy + 
            MountLebanon_dummy + North_dummy + South_dummy + Nabatieh_dummy"

# Run the OLS regression model
model5 <- lm(formula, data = merged_df)

#create a new regression with rCSI as the output.Use the same inputs, except for borrowing related variables, required health dummy, work regular interaction variables, age of female, and children:

#formula 0: without coping mechanisms and government controls. 
formula00 <- "rCSI ~ 
            basic_household_assets + appliance_assets + communication_assets + 
            mobile_phone_dummy + transportation_assets + spent_on_water + 
            high_expenditure + curfew_dummy + info_servicessms_dummy + 
            shared_toilets_dummy + insecurity_dummy + accom_short_living_period_dummy + 
            + health_access1_dummy + ecard_dummy + 
            child_labour_dummy + work_regular_and_female + child_and_work_regular + 
            male_and_work_regular + 
            + age_hoh + over_60 + hh_fem + 
            sex_dummy + legal_residency_numeric + has_spouse + is_head_of_household + 
            children_under_5 + highest_grade_category_numeric + age_numeric + 
            temp_illness_dummy + chronic_illness_dummy + 
            serious_med_cond_dummy + mental_illness_dummy"

#now do the same, but exclude stress_coping, crisis_coping and emergency_coping:
formula10 <- "rCSI ~ 
            stress_coping + crisis_coping + emergency_coping +
            basic_household_assets + appliance_assets + communication_assets + 
            mobile_phone_dummy + transportation_assets + spent_on_water + 
            high_expenditure + curfew_dummy + info_servicessms_dummy + 
            shared_toilets_dummy + insecurity_dummy + accom_short_living_period_dummy + 
            + health_access1_dummy + ecard_dummy + 
            child_labour_dummy + work_regular_and_female + child_and_work_regular + 
            male_and_work_regular + 
            + age_hoh + over_60 + hh_fem + 
            sex_dummy + legal_residency_numeric + has_spouse + is_head_of_household + 
            children_under_5 + highest_grade_category_numeric + age_numeric + 
            temp_illness_dummy + chronic_illness_dummy + 
            serious_med_cond_dummy + mental_illness_dummy"

# Define the formula for the regression model, with coping mechanisms and controls (all variables)
formula20 <- "rCSI ~ 
            stress_coping + crisis_coping + emergency_coping + 
            basic_household_assets + appliance_assets + communication_assets + 
            mobile_phone_dummy + transportation_assets + spent_on_water + 
            high_expenditure + curfew_dummy + info_servicessms_dummy + 
            shared_toilets_dummy + insecurity_dummy + accom_short_living_period_dummy + 
            + health_access1_dummy + ecard_dummy + 
            child_labour_dummy + work_regular_and_female + child_and_work_regular + 
            male_and_work_regular + 
            + age_hoh + over_60 + hh_fem + 
            sex_dummy + legal_residency_numeric + has_spouse + is_head_of_household + 
            children_under_5 + highest_grade_category_numeric + age_numeric + 
            temp_illness_dummy + chronic_illness_dummy + 
            serious_med_cond_dummy + mental_illness_dummy + 
            BaalbekHermel_dummy + Beirut_dummy + Beqaa_dummy + 
            MountLebanon_dummy + North_dummy + South_dummy + Nabatieh_dummy"

#With this done you should have a table with model 1. Without coping mechs and governorate controls, another one adding the copying mechs, another one adding the governorate controls(so all the variables) same for food consumption.

#have formula

formula01 <- "FCScore ~ 
            basic_household_assets + appliance_assets + communication_assets + 
            mobile_phone_dummy + transportation_assets + spent_on_water + 
            high_expenditure + curfew_dummy + info_servicessms_dummy + 
            shared_toilets_dummy + insecurity_dummy + accom_short_living_period_dummy + 
            + health_access1_dummy + ecard_dummy + 
            child_labour_dummy + work_regular_and_female + child_and_work_regular + 
            male_and_work_regular + 
            + age_hoh + over_60 + hh_fem + 
            sex_dummy + legal_residency_numeric + has_spouse + is_head_of_household + 
            children_under_5 + highest_grade_category_numeric + age_numeric + 
            temp_illness_dummy + chronic_illness_dummy + 
            serious_med_cond_dummy + mental_illness_dummy"

formula11 <- "FCScore ~ 
            stress_coping + crisis_coping + emergency_coping +
            basic_household_assets + appliance_assets + communication_assets + 
            mobile_phone_dummy + transportation_assets + spent_on_water + 
            high_expenditure + curfew_dummy + info_servicessms_dummy + 
            shared_toilets_dummy + insecurity_dummy + accom_short_living_period_dummy + 
            + health_access1_dummy + ecard_dummy + 
            child_labour_dummy + work_regular_and_female + child_and_work_regular + 
            male_and_work_regular + 
            + age_hoh + over_60 + hh_fem + 
            sex_dummy + legal_residency_numeric + has_spouse + is_head_of_household + 
            children_under_5 + highest_grade_category_numeric + age_numeric + 
            temp_illness_dummy + chronic_illness_dummy + 
            serious_med_cond_dummy + mental_illness_dummy"

formula21 <- "FCScore ~ 
            stress_coping + crisis_coping + emergency_coping + 
            basic_household_assets + appliance_assets + communication_assets + 
            mobile_phone_dummy + transportation_assets + spent_on_water + 
            high_expenditure + curfew_dummy + info_servicessms_dummy + 
            shared_toilets_dummy + insecurity_dummy + accom_short_living_period_dummy + 
            + health_access1_dummy + ecard_dummy + 
            child_labour_dummy + work_regular_and_female + child_and_work_regular + 
            male_and_work_regular + 
            + age_hoh + over_60 + hh_fem + 
            sex_dummy + legal_residency_numeric + has_spouse + is_head_of_household + 
            children_under_5 + highest_grade_category_numeric + age_numeric + 
            temp_illness_dummy + chronic_illness_dummy + 
            serious_med_cond_dummy + mental_illness_dummy + 
            BaalbekHermel_dummy + Beirut_dummy + Beqaa_dummy + 
            MountLebanon_dummy + North_dummy + South_dummy + Nabatieh_dummy"

# Run the OLS regression model, with each of the above:
model1 <- lm(formula00, data = merged_df)
model2 <- lm(formula10, data = merged_df)
model3 <- lm(formula20, data = merged_df)
model4 <- lm(formula01, data = merged_df)
model5 <- lm(formula11, data = merged_df)
model6 <- lm(formula21, data = merged_df)

install.packages("stargazer")
library(stargazer)

# Create a list of models (in this case, just one model)
models <- list(model1, model2, model3, model4, model5, model6)

#Specify model names (optional)

names(models) <- c("OLS Model, all variables")

# Generate the table

stargazer(models, type = "text")

#Check if the children variable and the children under 5 are highly correlated 
cor(merged_df$children, merged_df$children_under_5)


#--------------------------------------------------------------------------------
# Load the summarytools package
install.packages("summarytools")
library(summarytools)

mothers_data <- data.frame(
  head_of_household = c(1, 1, 0, 0, 1),
  age = c(30, 35, 40, 45, 50),
  education_level = c(12, 16, 10, 14, 18)
)

children_data <- data.frame(
  child_age = c(5, 7, 6, 8, 9),
  education_level = c(1, 2, 1, 3, 2),
  household_size = c(4, 3, 5, 4, 6),
  child_labor = c(1, 0, 0, 1, 1)
)

# Create the summary statistics table for mothers
summary_mothers <- summarytools::descr(
  mothers_data, 
  var.labels = c("Variable", "Mean", "Standard Deviation"),
  stats = c("mean", "sd")
)

# Create the summary statistics table for children
summary_children <- summarytools::descr(
  children_data, 
  var.labels = c("Variable", "Mean", "Standard Deviation"),
  stats = c("mean", "sd")
)

# Combine the summary statistics tables
summary_table <- list(
  "Mothers" = summary_mothers,
  "Children" = summary_children
)

# Print the summary table
summary_table

#-----------
#install packages for graphing:
install.packages("ggplot2")
install.packages("sf")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")

library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

lebanon <- st_read("/Users/sofialozano/Desktop/EconHonors/Docs/ne_10m_land/ne_10m_land.shp")


# Merge FCScore data with shapefile
merged_data <- merge(lebanon, merged_df, by.x = "governorate", by.y = "geometry", all.x = TRUE)

# Plot the map
ggplot() +
  geom_sf(data = merged_data, aes(fill = FCScore)) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "FCScore") +
  labs(title = "Food Consumption Score (FCScore) per Governorate in Lebanon")

#make a list of all the variables in the merged_df dataframe:
names(merged_df)

#what are the unique values of the variable hoh_relation
unique(merged_data$hoh_relation)
#how many are "Head of Household"?
table(merged_data$hoh_relation) #This means 4323 individuals are head of household.

#Create a new variable that is equal to 1 if the individual is the head of the household and 0 otherwise:
merged_data <- merged_data %>% mutate(is_head_of_household = ifelse(hoh_relation == "Head of Household", 1, 0))

#create a new data frame  with head of household and gender:
head_of_household <- merged_data %>% select(formid, is_head_of_household, gender)

#FILTER BY HEAD OF HOUSEHOLD

#of the heads of household, create a dummy that is equal to 1 if the individual is female and 0 otherwise:


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
library(dplyr)
conditional_means <- df %>%
  group_by(governorate) %>%
  summarise(mean_variable = mean(FCScore, na.rm = TRUE))

#show the conditional means:
conditional_means

#round the variable mean_age_when_relation_1 to the nearest integer:
df$mean_age_when_relation_1 <- round(df$mean_age_when_relation_1)
#run the conditional means for the variables of interest, such as age of head of household, number of children under 5, has_spouse, has_legal_residency:
conditional_means <- df %>%
  group_by(mean_age_when_relation_1) %>%
  summarise(mean_variable = mean(FCScore, na.rm = TRUE))

conditional_means

#now by the number of children under 5:
conditional_means <- df %>%
  group_by(num_children_under_5) %>%
  summarise(mean_variable = mean(FCScore, na.rm = TRUE))

conditional_means

#now by main electricity expenditure in USD:
conditional_means <- df %>%
  group_by(calcexpenditure_electricity_usd) %>%
  summarise(mean_variable = mean(FCScore, na.rm = TRUE))

conditional_means

#now by legal residency: 
conditional_means <- df %>%
  group_by(log_total_income) %>%
  summarise(mean_variable = mean(FCScore, na.rm = TRUE))

conditional_means

#graph this relationship, restrict x axis to 0-200
library(ggplot2)

ggplot(data = df, aes(x = calcedu_cost_usd, y = FCScore)) +
  geom_point() +
  xlim(1, 200)


#show the conditional means:
conditional_means

#run the conditional means for the variables of interest, which include num_children_under_5, has_spouse, has_legal_residency,
#is_hoh_female_and_relation_1, mean_age_when_relation_1, count_over_60:
conditional_means <- cem(merged_data_2018, "num_children_under_5", "has_spouse", "has_legal_residency", "is_hoh_female_and_relation_1", "mean_age_when_relation_1", "count_over_60")

#show the conditional means:
conditional_means



