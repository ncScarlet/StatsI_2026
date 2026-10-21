
#####################
# load libraries
# set wd
# clear global .envir
#####################


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

lapply(c("stargazer","broom"),  pkgTest)

# 1 a)

# Enter observed frequencies by row
observed <- matrix(
  c(14, 6,7,
    7,7,1),
  nrow = 2,
  byrow = TRUE,
  dimnames = list(
    Class = c("Upper","Lower"),
    Outcome = c("Not_stopped",
                "Bribe",
                "Warning")
  )
)

# Calculate marginal totals and sample size
row_total <- rowSums(observed)
col_total <- colSums(observed)
n <- sum(observed)

# Row total * column total /sample size
expected <- outer(row_total, col_total)/n


# Calculate each cell's contribution and sum them
contribution <- (observed - expected)^2/expected
chi_square <- sum(contribution)

output_stargazer <- function(outputFile, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file=outputFile, append=TRUE)
}

# execute function and check ls() to make sure it worked
output_stargazer("~/Documents/GitHub/StatsI_2026/problem_sets/PS02/my_answers/chi_square_output.tex", 
                 chi_square, 
                 type = "latex", 
                 summary = FALSE,
                 rownames = FALSE,
                 digits = 3)


# 1 b)

# Calculate  df
df = (nrow(observed)-1)*(ncol(observed)-1)

# Calculate p value
p_value <- pchisq(chi_square, df = df, lower.tail = FALSE)

output_stargazer <- function(outputFile, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file=outputFile, append=TRUE)
}

# execute function and check ls() to make sure it worked
output_stargazer("~/Documents/GitHub/StatsI_2026/problem_sets/PS02/my_answers/p_value_output.tex", 
                 p_value, 
                 type = "latex", 
                 summary = FALSE,
                 rownames = FALSE,
                 digits = 3)


# 1c)

# Calculate proportions
row_prop <- row_total/n
col_prop <- col_total/n

# Calculate standardised residuals
sr <- (observed - expected)/sqrt(expected*outer(1-row_prop,1-col_prop))
sr <- round(sr,3)


# 2 b)

# Load the data
women <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")

# Establish the regression model
model <- lm(water~reserved, data = women)

summary(model)
