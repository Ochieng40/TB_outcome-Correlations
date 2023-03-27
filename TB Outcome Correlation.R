
library(tidyverse)
library(graphics)
library(dplyr)
library(stringr)
library(kableExtra)
library(ggtext)
library(ggplot2)
library(extrafont)
library(forcats) # Load the package
library(scales)
library(readr)








df <- read.csv("C:\\Users\\DENIS OCHIENG'\\Desktop\\Paul_Analysis\\CT_Abstract_union_Outliers_Removed.csv")




# get the total number of non-missing values for each column
total_values <- colSums(!is.na(df), na.rm = TRUE)
print(total_values)


# Compute the frequency count for my_var
freq_class_type <- table(df$Contact_type)

# Print the frequency count
print(freq_class_type)




#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

##   1. PERCENTAGE OF Social Contact With TB OUTCOME

# Count the number of 'Social_Contact' who have 'TB' outcome
social_tb_count <- sum(df$Contact_type == 'Social_Contact' & df$TB_OUTCOME == 'TB')

# Count the total number of 'Social_Contact'
social_count <- sum(df$Contact_type == 'Social_Contact')

# Calculate the percentage of 'Social_Contact' who have 'TB' outcome
social_tb_percentage <- social_tb_count / social_count * 100

# Print the result
cat(sprintf("Percentage of 'Social_Contact' with 'TB' outcome: %.2f%%\n", social_tb_percentage))



#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

##   2. PERCENTAGE OF Social Contact With NO TB OUTCOME



# Count the number of 'Social_Contact' with NO 'TB' outcome
social_No_tb_count <- sum(df$Contact_type == 'Social_Contact' & df$TB_OUTCOME == 'No_TB')

# Count the total number of 'Social_Contact'
social_count <- sum(df$Contact_type == 'Social_Contact')

# Calculate the percentage of 'Social_Contact' who have 'No TB' outcome
social_No_tb_percentage <- social_No_tb_count / social_count * 100

# Print the result
cat(sprintf("Percentage of 'Social_Contact' with NO 'TB' outcome: %.2f%%\n", social_No_tb_percentage))



#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


## 3. PERCENTAGE OF Households With TB OUTCOME


# Count the number of 'Social_Contact' who have 'TB' outcome
Household_tb_count <- sum(df$Contact_type == 'Household' & df$TB_OUTCOME == 'TB')

# Count the total number of 'Social_Contact'
Household_count <- sum(df$Contact_type == 'Household')

# Calculate the percentage of 'Social_Contact' who have 'TB' outcome
Household_tb_percentage <- Household_tb_count / Household_count * 100

# Print the result
cat(sprintf("Percentage of Households with 'TB' outcome: %.2f%%\n", Household_tb_percentage))



#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


## 4. PERCENTAGE OF Households With NO TB OUTCOME


# Count the number of Households with 'NO TB' outcome
Household_No_tb_count <- sum(df$Contact_type == 'Household' & df$TB_OUTCOME == 'No_TB')

# Count the total number of 'Social_Contact'
Household_count <- sum(df$Contact_type == 'Household')

# Calculate the percentage of 'Social_Contact' who have 'TB' outcome
Household_No_tb_percentage <- Household_No_tb_count / Household_count * 100

# Print the result
cat(sprintf("Percentage of Households with NO TB outcome: %.2f%%\n", Household_No_tb_percentage))


#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



## 5. Percentage of Males in Social Contact with TB


# Count the number of 'Social_Contact' who have 'TB' outcome and are male
social_tb_male_count <- sum(df$Contact_type == 'Social_Contact' & df$TB_OUTCOME == 'TB' & df$Contact_Gender == 'Male')

# Count the total number of 'Social_Contact' who are male
social_male_count <- sum(df$Contact_type == 'Social_Contact' & df$Contact_Gender == 'Male')

# Calculate the percentage of 'Social_Contact' who have 'TB' outcome and are male
social_tb_male_percentage <- social_tb_male_count / social_male_count * 100

# Print the result
cat(sprintf("Percentage of Males in 'Social_Contact' with 'TB' outcome: %.2f%%\n", social_tb_male_percentage))


#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



## 6. Percentage of Females in Social Contact with TB


# Count the number of 'Social_Contact' who have 'TB' outcome and are male
social_tb_Female_count <- sum(df$Contact_type == 'Social_Contact' & df$TB_OUTCOME == 'TB' & df$Contact_Gender == 'Female')

# Count the total number of 'Social_Contact' who are male
social_Female_count <- sum(df$Contact_type == 'Social_Contact' & df$Contact_Gender == 'Female')

# Calculate the percentage of 'Social_Contact' who have 'TB' outcome and are male
social_tb_Female_percentage <- social_tb_Female_count / social_Female_count * 100

# Print the result
cat(sprintf("Percentage of Females in 'Social_Contact' with 'TB' outcome: %.2f%%\n", social_tb_Female_percentage))




#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------





## 7. Percentage of Males in Social Contact with No TB


# Count the number of 'Social_Contact' who have 'TB' outcome and are male
social_No_tb_male_count <- sum(df$Contact_type == 'Social_Contact' & df$TB_OUTCOME == 'No_TB' & df$Contact_Gender == 'Male')

# Count the total number of 'Social_Contact' who are male
social_male_count <- sum(df$Contact_type == 'Social_Contact' & df$Contact_Gender == 'Male')

# Calculate the percentage of 'Social_Contact' who have 'TB' outcome and are male
social_No_tb_male_percentage <- social_No_tb_male_count / social_male_count * 100

# Print the result
cat(sprintf("Percentage of Males in 'Social_Contact' with NO 'TB' outcome: %.2f%%\n", social_No_tb_male_percentage))


#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



## 8. Percentage of Females in Social Contact with NO TB


# Count the number of 'Social_Contact' who have 'TB' outcome and are male
social_No_tb_Female_count <- sum(df$Contact_type == 'Social_Contact' & df$TB_OUTCOME == 'No_TB' & df$Contact_Gender == 'Female')

# Count the total number of 'Social_Contact' who are male
social_Female_count <- sum(df$Contact_type == 'Social_Contact' & df$Contact_Gender == 'Female')

# Calculate the percentage of 'Social_Contact' who have 'TB' outcome and are male
social_No_tb_Female_percentage <- social_No_tb_Female_count / social_Female_count * 100

# Print the result
cat(sprintf("Percentage of Females in Social Contact categoey with NO 'TB' outcome: %.2f%%\n", social_No_tb_Female_percentage))





#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------





## 5. Percentage of Males in Households with TB


# Count the number of Households who have 'TB' outcome and are male
Households_tb_male_count <- sum(df$Contact_type == 'Household' & df$TB_OUTCOME == 'TB' & df$Contact_Gender == 'Male')

# Count the total number of 'Household' who are male
Household_male_count <- sum(df$Contact_type == 'Household' & df$Contact_Gender == 'Male')

# Calculate the percentage of 'Household' who have 'TB' outcome and are male
Household_tb_male_percentage <- Households_tb_male_count / Household_male_count * 100

# Print the result
cat(sprintf("Percentage of Males in Households who have TB is: %.2f%%\n", Household_tb_male_percentage))


#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



## 6. Percentage of Females in Households category with TB


# Count the number of Households who have 'TB' outcome and are male
Household_tb_Female_count <- sum(df$Contact_type == 'Household' & df$TB_OUTCOME == 'TB' & df$Contact_Gender == 'Female')

# Count the total number of 'Households' who are male
Household_Female_count <- sum(df$Contact_type == 'Household' & df$Contact_Gender == 'Female')

# Calculate the percentage of 'Households' who have 'TB' outcome and are male
Household_tb_Female_percentage <- Household_tb_Female_count / Household_Female_count * 100

# Print the result
cat(sprintf("Percentage of Females in Households with TB is: %.2f%%\n", Household_tb_Female_percentage))




#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------





## 7. Percentage of Males inHouseholds with No TB


# Count the number of 'Household' who have 'TB' outcome and are male
Household_No_tb_male_count <- sum(df$Contact_type == 'Household' & df$TB_OUTCOME == 'No_TB' & df$Contact_Gender == 'Male')

# Count the total number of 'Household' who are male
Household_male_count <- sum(df$Contact_type == 'Household' & df$Contact_Gender == 'Male')

# Calculate the percentage of 'Household' who have 'TB' outcome and are male
Household_No_tb_male_percentage <- Household_No_tb_male_count / Household_male_count * 100

# Print the result
cat(sprintf("Percentage of Males in Households who have NO 'TB' outcome: %.2f%%\n", Household_No_tb_male_percentage))


#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



## 8. Percentage of Females in Households with NO TB


# Count the number of households who have 'TB' outcome and are male
Household_No_tb_Female_count <- sum(df$Contact_type == 'Household' & df$TB_OUTCOME == 'No_TB' & df$Contact_Gender == 'Female')

# Count the total number of 'Social_Contact' who are male
Household_Female_count <- sum(df$Contact_type == 'Household' & df$Contact_Gender == 'Female')

# Calculate the percentage of 'Social_Contact' who have 'TB' outcome and are male
Household_No_tb_Female_percentage <- Household_No_tb_Female_count / Household_Female_count * 100

# Print the result
cat(sprintf("Percentage of Females in Households with NO 'TB' outcome: %.2f%%\n", Household_No_tb_Female_percentage))





#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


# Mean age for Households with TB

# Subset the data to include only rows with Household contact type and TB outcome
household_tb <- df[df$Contact_type == "Household" & df$TB_OUTCOME == "TB", ]

# Calculate the mean age of respondents in the subset
mean_age_Household <- mean(household_tb$Source_Case_Age)



# Print the result
cat(sprintf("On average Households that had TB reported a mean of: %.2f%%\n", mean_age_Household))



#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


# Mean age for Households with TB

# Subset the data to include only rows with Household contact type and TB outcome
social_contact_tb <- df[df$Contact_type == "Social_Contact" & df$TB_OUTCOME == "TB", ]

# Calculate the mean age of respondents in the subset
mean_age_social <- mean(social_contact_tb$Contact_Age)

# Print the mean age 
print(mean_age_social)



# Print the result
cat(sprintf("On average Social contact that had TB reported a mean of: %.2f%%\n", mean_age_social))

















