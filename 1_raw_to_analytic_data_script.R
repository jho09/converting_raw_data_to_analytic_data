library(tidyverse)

raw_data <- read_csv(file="rawData.csv")

str(raw_data)

View(raw_data)

# Here we load the data in a way that includes -999 and -888 as missing values
raw_data <- read_csv(file="rawData.csv",na=c("","NA","-999","-888"))


View(raw_data)

# Here we create a data set with just the categorical variables (i.e., group and gender)
categorical_variables <- select(raw_data,group,gender)

# Here we convert these variables to categorical ones by converting them to factors
categorical_variables$group <- as.factor(categorical_variables$group)

# Here we assign labels to Male/Female
levels(categorical_variables$gender) <- list("Male"=1,"Female"=2)


# To reverse key items (e.g., on a 5-point scale), use RK = 6 - #; but if the scale starts with zero, use the last number on the scale


# Here we break the scale items into separate data frames because they use different numbers of points
affective_commitment_items <- select (raw_data,AC1,AC2,AC3,AC4,AC5)
agreeableness_items <- select (raw_data,A1,A2,A3,A4,A5)
extroversion_items <- select (raw_data,E1,E2,E3,E4,E5)

View(affective_commitment_items)
View(agreeableness_items)
View(extroversion_items)

install.packages("psych",dep=T)

psych::describe(extroversion_items)
psych::describe(agreeableness_items)


agreeableness_items

# Here we identify values outisde the range of 1 to 5

is_bad_value <- agreeableness_items<1 | agreeableness_items>5
View(is_bad_value)


# Here we change the bad/impossible values to missing values
agreeableness_items[is_bad_value] <-NA
View(agreeableness_items)


psych::describe(affective_commitment_items)
View(affective_commitment_items)

# Here we repeat the process for the affective commitment items
is_bad_value_affectivecommitment <- affective_commitment_items<1 | affective_commitment_items>7
View(is_bad_value_affectivecommitment)

affective_commitment_items[is_bad_value_affectivecommitment] <- NA
View(affective_commitment_items)


View(agreeableness_items)


# Here we reverse-key the items for agreeableness (A5)
agreeableness_items <- mutate(agreeableness_items,A5=6-A5)

View(agreeableness_items)


# Here we reverse-key items AC4 and AC5 from the affective commitment scale

affective_commitment_items <- mutate(affective_commitment_items,AC4=8-AC4,AC5=8-AC5)
View(affective_commitment_items)


# Here we create three new variables comprising the mean scores for each person based on the items

agreeableness <- psych::alpha(as.data.frame(agreeableness_items),check.keys = FALSE)$scores
View(agreeableness)

extroversion <- psych::alpha(as.data.frame(extroversion_items), check.keys = FALSE)$scores
View(extroversion)

affective_commitment <- psych::alpha(as.data.frame(affective_commitment_items), check.keys = FALSE)$scores
View(affective_commitment)


# Here we create a data frame for the analytic data

analytic_data <- cbind(categorical_variables,agreeableness,extroversion,affective_commitment)

View(analytic_data)

save(analytic_data,file="study1_analytic_data.RData")
write_csv(analytic_data,path="study1_analytic_data.csv")
library(haven)
write_sav(analytic_data,path="study1_analytic_data.sav")