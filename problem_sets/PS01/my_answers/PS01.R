#####################
# load libraries
# set wd
# clear global .envir
#####################

library(stargazer)
library(ggplot2)
library(broom)

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

#####################
# Problem 1 1. confidence interval
#####################

school_sample_iq <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

# length of the school sample
length(school_sample_iq)

# calculate the mean of the school sample
mean_school_sample_iq <- mean(school_sample_iq)

# calculate the sd of the school sample
sd_school_sample_iq <- sd(school_sample_iq)

# calculate the standard error
se_school_sample_iq <- sd_school_sample_iq / sqrt(length(school_sample_iq))

# calculate lower bound and upper bound of the CI
ci_90_lower <- mean_school_sample_iq - 1.71 * se_school_sample_iq
ci_90_upper <- mean_school_sample_iq + 1.71 * se_school_sample_iq



#####################
# Problem 1 2.  hypothesis test
#####################

# run t-test
t_test_result <- t.test(school_sample_iq,mu = 100, alternative="greater",conf_level=0.95)

# print the result
print(t_test_result)

t_test_result_tidy <- tidy(t_test_result)

t_test_result_df <- as.data.frame(t_test_result_tidy)

output_stargazer <- function(outputFile, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file=outputFile, append=TRUE)
}

# execute function and check ls() to make sure it worked
output_stargazer("~/Documents/GitHub/StatsI_2026/problem_sets/PS01/my_answers/ttest_output.tex", 
                 t_test_result_df, 
                 type = "latex", 
                 summary = FALSE,
                 rownames = FALSE,
                 digits = 3)

#####################
# Problem 2.1
#####################

# read the expenditure data
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2026/main/datasets/expenditure.txt", header=T)

# Create a subset of the expenditure dataset by filtering data of 
# column Y, X1, X2, X3
plot_data <- expenditure[, c("Y","X1","X2","X3")]

# Create a scatter plot matrix to visualise all relationships
# among the four variables
pairs(plot_data, pch = 19, col = "lightblue")

# Calculate Pearson correlation coefficients
cor_result <- cor(plot_data)

# print the result
print(cor_result)

output_stargazer <- function(outputFile, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file=outputFile, append=TRUE)
}

# execute function and check ls() to make sure it worked
output_stargazer("~/Documents/GitHub/StatsI_2026/problem_sets/PS01/my_answers/cor_result_output.tex", 
                 cor_result, 
                 type = "latex", 
                 summary = FALSE,
                 rownames = FALSE,
                 digits = 3)



#####################
# Problem 2.2
#####################

# As the region variable is numeric, we need to convert it 
# into categorical factor variable.
expenditure$Region_name <- factor(expenditure$Region, levels = c(1,2,3,4),
                                  labels = c("Northeast", "North Central","South", "West"))

# Create a boxplot with ggplot
ggplot(expenditure, aes(x = Region_name, y = Y))+
    geom_boxplot(fill = "lightblue") +
  labs(x = "Region", y = "Per Capita Expenditure")+
  theme_minimal()


#####################
# Problem 2.3
#####################


# Create a scatter plot to show the relationship between housing assistance
# expenditure and personal income
ggplot(expenditure, aes(x=X1, y=Y))+
  geom_point(size= 3, color = "lightblue")+
  labs(x = "Personal Income (X1)",
       y = "Per Capita Expenditure(Y)")+
  theme_minimal()



# Create a scatter plot to show the relationship between housing assistance 
# expenditure and personal income, with regions distinguished by colour and shape
ggplot(expenditure, aes(x =X1, y= Y, color = Region_name, shape = Region_name))+
  geom_point(size = 3) +
  labs(x = "Personal Income per capita (X1)",
       y = "Expenditure(Y)",
       color="Region",
       shape="Region")+
  theme_minimal()



