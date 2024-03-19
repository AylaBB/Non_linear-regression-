

#loading packages 
library(dplyr)
library(tidyverse)
library(ggplot2)
library(Hmisc)
library(cowplot)
library(ordinal)
library(sjPlot)
library(caret)
library(pscl)
library(modelsummary)
library(stargazer)
library(patchwork) 

#loading the random dataset
data_org <- read.csv("StudentsPerformance.csv")
set.seed(sum(as.integer(charToRaw("Ayla"))))
my_data <- sample_n(data_org, 200)

#Qucik overview of the data 
View(my_data)
names(my_data)
summary(my_data)
str(my_data)
#checking for missing data
which(is.na(my_data)==T) #No missing data


###Preparing the Data###

##the DEPENDETS variables.

#The math, reading, and writing score are integer variables. 
#They need to be recoded into categorical (ordinal and binary) variables:

##MATH Variable

#transforming the math variable into ordinal variable with 6 categories (A, B, C, D, E, F)
#grade E(51-60), D(61-70), C(71-80), B(81-90), A(91-100) 
#anything else will be "F", in this case, any score equal to or less than 50

my_data <- my_data %>%
  mutate(math.grade = case_when(
    math.score >= 91 ~ 'A',
    math.score >= 81 ~ 'B',
    math.score >= 71 ~ 'C',
    math.score >= 61 ~ 'D',
    math.score >= 51 ~ 'E',
    TRUE ~ 'F')) %>% 
  mutate(math.grade = factor(math.grade, levels = c("F", "E", "D", "C", "B", "A"))) 

#checking the new factors 
levels(my_data$math.grade)


##READING and WRITING VARIABLES
#transforming the reading and writing score variable into categorical variable with 2 categories (P = Pass, F= Fail)
# The threshold is 51, meaning that a score of 51 or more will be passed grade and a score of 50 or less will be a failed grade

my_data <- my_data %>%
  mutate(reading.grade = ifelse(my_data$reading.score >= 51, 'P', 'F'),
         reading.grade= relevel(as.factor(reading.grade), ref="F"))  %>%  #setting the reference group "Fail" for easier interpretation 
  mutate(writing.grade = ifelse(my_data$writing.score >= 51, 'P', 'F'), 
         writing.grade = relevel(as.factor(writing.grade), ref="F"))

levels(my_data$reading.grade)
levels(my_data$writing.grade)


##Preparing the INDEPENDENT variables
#changing variables types from characters to factors
my_data <- my_data %>% 
  mutate(lunch = relevel(as.factor(lunch), ref = "standard"),
         test.preparation.course = relevel (as.factor(test.preparation.course), ref = "none"))


#Preparing the CONTROL variables 
#recording the parental education into three categories and renaming them. 
my_data <- my_data %>% 
  mutate(parental.education.cat = fct_collapse(parental.level.of.education,
                                               High_school= c("high school", "some high school"),
                                               undergraduate_education = c("some college","associate's degree", "bachelor's degree"),
                                               gradute_eduaction = c("master's degree")))

#changing control variables types to factors and choosing the reference group
my_data <- my_data %>% 
  mutate(gender = relevel(as.factor(gender), ref = "male"), 
         parental.education.cat = relevel(as.factor(parental.education.cat),ref ="High_school"),
         race.ethnicity = relevel(as.factor(race.ethnicity),ref= "group A"))

#Ensuring that the data types look as it should
str(my_data)






###Descritiv stat###

#Descriptiv stat for the DEPENDET varaibles

#Math grade

my_data %>% 	
  ggplot() +	
  aes(x = math.grade) +	
  geom_bar(fill = "darkorange3", alpha = 0.8, width = 0.8)	+
  labs(x = "Math Grade", y = "Count", title = "Distribution of Math Grade") +
  theme_classic()

describe(my_data$math.grade)


#writing grade

my_data %>% 	
  ggplot() +	
  aes(x = writing.grade) +	
  geom_bar(fill = "darkorange3")	+
  labs(x = "Writing Grade", y = "Count", title = "Distribution of Writing Grade") +
  theme_classic()

describe(my_data$writing.grade)


#reading grade

my_data %>% 	
  ggplot() +	
  aes(x = reading.grade) +	
  geom_bar(fill = "darkorange3")	+
  labs(x = "Reading Grade", y = "Count", title = "Distribution of Reading Grade") +
  theme_classic()

describe(my_data$reading.grade)

#Descriptiv stat for the INDEPENDET varaibles

#lunch variable

my_data %>% 	
  ggplot() +	
  aes(x = lunch) +	
  geom_bar(fill = "skyblue")	+
  labs(title = "Distribution of lunch") +
  theme_classic()

describe(my_data$lunch)

#course preparation

my_data %>% 	
  ggplot() +	
  aes(x = test.preparation.course) +	
  geom_bar(fill = "skyblue")	+
  labs(x = "preparation course", y = "Count", title = "Distribution of preparation course") +
  theme_classic()

describe(my_data$test.preparation.course)


#Desciptive stat for the CONTROL variables
levels(my_data$gender)
describe(my_data$gender)

levels(my_data$race.ethnicity)
describe(my_data$race.ethnicity)

levels(my_data$parental.education.cat)
describe(my_data$parental.education.cat)


###quick visualisation of the variables: exploratory analysis###

# regression 1 (DV: match grade)

plot1 <- ggplot(my_data, aes(x = math.grade, fill = lunch)) +
  geom_bar(position = "dodge", color = "black") +
  labs(x = "Math Grade", y = "Count", title = "Math grade predicted by lunch") +
  scale_fill_manual(values = c("darkorange", "darkseagreen")) +
  theme_classic()

plot2 <- ggplot(my_data, aes(x = math.grade, fill = test.preparation.course)) + 
  geom_bar(position = "dodge", color = "black") +
  labs(x = "Math Grade", y = "Proportion", title = "Math grade predicted by preparation course", fill = "Preparation course") +
  scale_fill_manual(values = c("darkorange", "darkseagreen")) + 
  theme_classic()

combined_plot1 <- plot1 + plot2 + plot_layout(ncol = 2)
combined_plot1


# regression 2(DV: reading grade)
plot3 <- ggplot(my_data, aes(x = reading.grade, fill = lunch)) +
  geom_bar(position = "dodge", color = "black") +
  labs(x = "Math Grade", y = "Count", title = "Reading Grade predicted by Lunch") +
  theme_classic()

plot4 <- ggplot(my_data, aes(x = reading.grade, fill = test.preparation.course)) +
  geom_bar(position = "dodge", color = "black") +
  labs(x = "Math Grade", y = "Count", title = "Reading Grade predicted by test.preparation.course") +
  theme_classic()

combined_plot2 <- plot3 + plot4 + plot_layout(ncol = 2)
combined_plot2

# regression 3 (DV: writing grade)
library(ggplot2)
plot5<- ggplot(my_data, aes(x = writing.grade, fill = lunch)) +
  geom_bar(position = "dodge", color = "black") +
  labs(x = "Math Grade", y = "Count", title = "Writing Grade predicted by Lunch") +
  theme_classic()

plot6<- ggplot(my_data, aes(x = writing.grade, fill = test.preparation.course)) +
  geom_bar(position = "dodge", color = "black") +
  labs(x = "Math Grade", y = "Count", title = "Writing Grade predicted by test.preparation.course") +
  theme_classic()

combined_plot3 <- plot5 + plot6 + plot_layout(ncol = 2)
combined_plot3




###REGRESSION ANALYSIS###

##ORDINAL REGRESSION: Math grade as DV (ordinal variable with 6 levels)

#Basic model
model_math <- clm(math.grade ~ lunch + test.preparation.course, data = my_data)
summary(model_math)
plot_model(model_math)

#Model with controll variabels

model1_math <- clm(math.grade ~ lunch + test.preparation.course + gender + race.ethnicity + parental.education.cat, data = my_data)
summary(model1_math)
plot_model(model1_math)


#the output is log odds /logit (p/(1 −p))/
#It indicates two main things: 
#(1) significant.
#(2) the positive/negative effect of predictors on math grade
#It is not formative more than that, therefore we need to convert the log-odds (logit) into odds ratio and probabilities for better interpretation 

# converting logit to probability :
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

# Convert logit coefficients to probabilities
prob_coef_model_math <- logit2prob(coef(model_math))
prob_coef_model1_math <- logit2prob(coef(model1_math))

print(prob_coef_model_math)
print(prob_coef_model1_math)

# convert the coefficients to odds-ratios 
odds_ratio_model_math <- exp(coef(model_math))
odds_ratio_model1_math <- exp(coef(model1_math))

print(odds_ratio_model_math)
print(odds_ratio_model1_math)

#create a summary table
stargazer(
  list("Model 1" = model_math, "Model 2" = model1_math),
  type = "html",
  align = TRUE,
  single.row = TRUE,
  df = TRUE,
  star.cutoffs = c(0.05, 0.01, 0.001),
  intercept.bottom = FALSE,
  covariate.labels = c(
    "lunch" = "Lunch: reduced/free",
    "test.preparation.coursecompleted" = "Preparation course: completed",
    "genderfemale" = "Gender: Female",
    "race.ethnicitygroup B" = "Ethnicity: Group B",
    "race.ethnicitygroup C" = "Ethnicity: Group C",
    "race.ethnicitygroup D" = "Ethnicity: Group D",
    "race.ethnicitygroup E" = "Ethnicity: Group E",
    "parental.education.catundergraduate_education" = "Parental education: Undergraduate",
    "parental.education.catgradute_eduaction" = "Parental education: Graduate"
  ),
  title = "Table 1. Ordinal Regression",
  keep.stat = c("n", "adj.rsq", "f"),
  no.space = TRUE,
  out = "models.htm"
)







##logistics regression: Reading and Writing as DV (nominal variables with two levels)

#DV: reading 
#basic model
model_reading <-  glm(reading.grade ~ lunch + test.preparation.course, family = binomial(), data = my_data)
summary(model_reading) 

#with control variables
model1_reading <-  glm(reading.grade ~ lunch + test.preparation.course + gender + race.ethnicity + parental.education.cat, family = binomial(), data = my_data)
summary(model1_reading) 

# Convert logit coefficients to probabilities
prob_coef_model_reading <- logit2prob(coef(model_reading))
prob_coef_model1_reading <- logit2prob(coef(model1_reading))


print(prob_coef_model_reading)
print(prob_coef_model1_reading)

# convert the coefficients to odds-ratios 
odds_ratio_model_reading <- exp(coef(model_reading))
odds_ratio_model1_reading <- exp(coef(model1_reading))

print(odds_ratio_model_reading)
print(odds_ratio_model1_reading)


#create summury table
stargazer(
  list("Model 1" = model_reading, "Model 2" = model1_reading),
  type = "html",
  align = TRUE,
  single.row = TRUE,
  df = TRUE,
  star.cutoffs = c(0.05, 0.01, 0.001),
  intercept.bottom = FALSE,
  covariate.labels = c("(Intercept)" = "(Intercept)",
                       "lunch" = "Lunch: reduced/free",
                       "test.preparation.coursecompleted" = "Preparation course: completed",
                       "genderfemale" = "Gender: Female",
                       "race.ethnicitygroup B" = "Ethnicity: Group B",
                       "race.ethnicitygroup C" = "Ethnicity: Group C",
                       "race.ethnicitygroup D" = "Ethnicity: Group D",
                       "race.ethnicitygroup E" = "Ethnicity: Group E",
                       "parental.education.catundergraduate_education" = "Parental education: Undergraduate",
                       "parental.education.catgradute_eduaction" = "Parental education: Graduate"
  ),
  title = "Table 2. Logistics  Regression",
  keep.stat = c("n", "adj.rsq", "f"),
  no.space = TRUE,
  out = "models.htm"
)




#DV: Writing 

#basic model
model_writing <-  glm(writing.grade ~ lunch + test.preparation.course, family = binomial(), data = my_data)
summary(model_writing)

#with control variables
model1_writing <-  glm(writing.grade ~ lunch + test.preparation.course + gender + race.ethnicity + parental.education.cat, family = binomial(), data = my_data)
summary(model1_writing) 

# Convert logit coefficients to probabilities
prob_coef_model_writing <- logit2prob(coef(model_writing))
prob_coef_model1_writing <- logit2prob(coef(model1_writing))

print(prob_coef_model_writing)
print(prob_coef_model1_writing)

# convert the coefficients to odds-ratios 
odds_ratio_model_writing <- exp(coef(model_writing))
odds_ratio_model1_writing <- exp(coef(model1_writing))

print(odds_ratio_model_writing)
print(odds_ratio_model1_writing)


#create table summary
stargazer(
  list("Model 1" = model_writing, "Model 2" = model1_writing),
  type = "html",
  align = TRUE,
  single.row = TRUE,
  df = TRUE,
  star.cutoffs = c(0.05, 0.01, 0.001),
  intercept.bottom = FALSE,
  covariate.labels = c("(Intercept)" = "(Intercept)",
                       "lunch" = "Lunch: reduced/free",
                       "test.preparation.coursecompleted" = "Preparation course: completed",
                       "genderfemale" = "Gender: Female",
                       "race.ethnicitygroup B" = "Ethnicity: Group B",
                       "race.ethnicitygroup C" = "Ethnicity: Group C",
                       "race.ethnicitygroup D" = "Ethnicity: Group D",
                       "race.ethnicitygroup E" = "Ethnicity: Group E",
                       "parental.education.catundergraduate_education" = "Parental education: Undergraduate",
                       "parental.education.catgradute_eduaction" = "Parental education: Graduate"
  ),
  title = "Table 3. Logistics  Regression",
  keep.stat = c("n", "adj.rsq", "f"),
  no.space = TRUE,
  out = "models.htm"
)







#model prediction (DV: writing)


#regression with sensitivity of at least 90% 
# The actual values for writing.grade
actual_values <- my_data$writing.grade
actual_values <- factor(actual_values, levels = c("P", "F")) #releveling to align with the predicted values

# Predicting the existing data
predicted <- predict(model1_writing, type = "response")  
predicted <- ifelse(predicted > 0.5, "P", "F")  
#This means that any prediction with a probability higher than 0.5 will be classified as Pass
#and anything below will be classified as fail (negative class).

# Convert predicted variable to a factor with levels "P" and "F" and assign it to the dataset

my_data$predicted_values <- predicted
predicted_values <- factor(predicted, levels = c("P", "F"))

# confusion matrix
conf_matrix <- confusionMatrix(predicted_values, actual_values)
print(conf_matrix)
sensitivity(predicted_values, actual_values)
specificity(predicted_values, actual_values)





#regression with perfect sensitivity 100%

# Predicting on the existing data (treshhold is adjusted to 0.3)
predicted_2<- predict(model1_writing, type = "response")  
predicted_2<- ifelse(predicted_2 > 0.3, "P", "F")  
#This means that any prediction with a probability higher than 0.9 will be classified Pass
#and anything below will be classified as 0 (negative class).

# Convert predicted variable to a factor with levels "P" and "F" and assign it to the dataset 
my_data$predicted_values_2 <- predicted_2
predicted_values_2 <- factor(predicted_2, levels = c("P", "F"))

# confusion matrix
conf_matrix <- confusionMatrix(predicted_values_2, actual_values)
print(conf_matrix)
sensitivity(predicted_values_2, actual_values)
specificity(predicted_values_2, actual_values)





