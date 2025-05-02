# =============================================================================
# Title: Framing Causal Questions in Sports Analytics: A Case Study of Crossing in Soccer
# Authors: Shomoita Alam*, Erica E. M. Moodie, Lucas Y. Wu, and Tim B. Swartz
# Corresponding Author: Shomoita Alam (shomoita.alam@mcgill.ca)
# Date: 2025-03-03
# Description: This script contains the R code used for the analysis in the paper
#              "Framing Causal Questions in Sports Analytics: A Case Study of Crossing in Soccer."
#              It includes data preprocessing, statistical modeling, and visualization.
# =============================================================================


# ==============================================================
# Estimand: Average Treatment Effect (ATE)
# ==============================================================
# This script estimates the ATE of crossing attempts in soccer using propensity score matching.
# Author: Shomoita Alam
# Date: 2025-03-03
# ==============================================================

set.seed(2024)

# Load required libraries
library(tableone)  # For creating descriptive statistics tables
library(Matching)  # For propensity score matching
library(survey)    # For survey-weighted analyses
library(ggplot2)   # For creating plots

# Load the dataset
dat <- read.csv("~/crossing.csv", 
                stringsAsFactors = FALSE)

# Convert variables to appropriate types
dat$position <- factor(dat$position, levels = c("forward", "midfielder", "defender"))
dat$shot_attempt <- as.factor(dat$shot_attempt)
dat$cross_attempt <- ifelse(dat$cross_attempt == 1, 1, 0)  # Binary: 1 = cross, 0 = no cross
dat$ten_min_warning<- as.factor(dat$ten_min_warning)

# Create summary tables
vars <- names(dat)[-c(1, 2)]  # Exclude ID variables
tabUnmatchedOverall <- CreateTableOne(vars = names(dat), data = dat, test = FALSE)
tabUnmatched_Str <- CreateTableOne(vars = names(dat)[-(1:2)], data = dat, 
                                   strata = names(dat)[2], test = FALSE)

# Calculate Standardized Mean Differences (SMD) to assess covariate balance
smds <- ExtractSmd(tabUnmatched_Str) * 100

# Fit logistic regression model for propensity scores
ps.mod <- glm(dat$cross_attempt ~ ., data = dat[, -c(1, 2)], 
              family = binomial(link = "logit"))
dat$ps.lr <- predict(ps.mod, type = "response")  # Predicted propensity scores
## Predicted probability of being assigned to no crossing
dat$nps.lr <- 1 - dat$ps.lr

# Visualize propensity score distributions
ggplot(dat, aes(x = ps.lr, fill = factor(cross_attempt))) +
  geom_histogram(
    aes(y = after_stat(density)), 
    position = "identity", 
    alpha = 0.5,
    color = "black",
    binwidth = 0.05  # adjust as needed
  ) +
  scale_fill_discrete(name = "Cross Attempt", labels = c("No Cross", "Cross")) +
  labs(x = "Propensity Scores", y = "Density") +
  theme_minimal()


## treatment actually assigned (either crossed or not crossed)
dat$pAssign <- NA
dat$pAssign[dat$cross_attempt == 1]    <- dat$ps.lr[dat$cross_attempt   == 1]
dat$pAssign[dat$cross_attempt == 0] <- dat$nps.lr[dat$cross_attempt == 0]
## Smaller of pRhc vs pNoRhc for matching weight
dat$pMin <- pmin(dat$ps.lr, dat$nps.lr)


# Propensity score matching

dat$shot_attempt <- as.numeric(as.character(dat$shot_attempt))
dat$cross_attempt <- as.numeric(as.character(dat$cross_attempt))
dat <- na.omit(dat)

ps.lr.match <- Match(Y = dat$shot_attempt, Tr = dat$cross_attempt, X = dat$ps.lr, 
                     estimand = "ATE", M = 1, ties = TRUE, replace = TRUE)

ps.lr.match$est  # Average Treatment Effect
ps.lr.match$se   # Standard error of ATE

# Run bootstrapping with 1000 replicates
n_bootstrap <- 1000
ate<- list()
# Perform bootstrapping
for (i in 1:n_bootstrap) {
  cat("Bootstrap iteration:", i, "\n")
  boot_data <- dat[sample(1:nrow(dat), nrow(dat), replace = TRUE), ]
  boot_estimates <- Match(Y = boot_data$shot_attempt, Tr = boot_data$cross_attempt,
                          X = boot_data$ps.lr, estimand = "ATE",
                          M = 1, ties = TRUE, replace = TRUE)
  ate[[i]]<- boot_estimates$est
  
}

boot_ate <- do.call(rbind, ate)

# Calculate standard error of the ATE
boot_se <- sd(boot_ate)

## Extract matched data
matched.samp <- dat[unlist(ps.lr.match[c("index.treated","index.control")]), ]



# Check for dropped unmatched samples
(dropped.match <- ps.lr.match$ndrops.matches)
dim(matched.samp)

tabMatched<- CreateTableOne(vars = names(matched.samp)[-c(1,2, 11:14)], 
                            data = matched.samp, strata = names(matched.samp)[2], test = FALSE)
tabMatched

# Assess balance after matching
smds.Matched <- round(ExtractSmd(tabMatched) * 100, 2)
(balance <- cbind(smds, smds.Matched))  # Compare unmatched vs. matched balance


# Distribution of propensity scores after matching

ggplot(matched.samp, aes(x = ps.lr, fill = factor(cross_attempt))) +
  geom_histogram(
    aes(y = after_stat(density)), 
    position = "identity", 
    alpha = 0.5,
    color = "black",
    binwidth = 0.02  # adjust as needed
  ) +
  scale_fill_discrete(name = "Cross Attempt", labels = c("No Cross", "Cross")) +
  labs(x = "Propensity Scores", y = "Density") +
  theme_minimal()


######################## END ##########################


# ==============================================================
# Estimand: Average Treatment Effect on the Treated (ATT)
# ==============================================================
# This script estimates the ATT of crossing attempts in soccer using propensity score matching.
# Author: Shomoita Alam
# Date: 2025-03-03
# ==============================================================
set.seed(2024)

# Load required libraries
library(tableone)  # For creating descriptive statistics tables
library(Matching)  # For propensity score matching
library(survey)    # For survey-weighted analyses
library(ggplot2)   # For creating plots

# Load the dataset
dat <- read.csv("~/crossing.csv", 
                stringsAsFactors = FALSE)

# Convert variables to appropriate types
dat$position <- factor(dat$position, levels = c("forward", "midfielder", "defender"))
dat$shot_attempt <- as.factor(dat$shot_attempt)
dat$cross_attempt <- ifelse(dat$cross_attempt == 1, 1, 0)  # Binary: 1 = cross, 0 = no cross
dat$ten_min_warning<- as.factor(dat$ten_min_warning)

# Create summary tables
vars <- names(dat)[-c(1, 2)]  # Exclude ID variables
tabUnmatchedOverall <- CreateTableOne(vars = names(dat), data = dat, test = FALSE)
tabUnmatched_Str <- CreateTableOne(vars = names(dat)[-(1:2)], 
                                   data = dat, strata = names(dat)[2], test = FALSE)

# Calculate Standardized Mean Differences (SMD) to assess covariate balance
smds <- ExtractSmd(tabUnmatched_Str) * 100

# Fit logistic regression model for propensity scores
ps.mod <- glm(dat$cross_attempt ~ ., data = dat[, -c(1, 2)], 
              family = binomial(link = "logit"))
dat$ps.lr <- predict(ps.mod, type = "response")  # Predicted propensity scores
## Predicted probability of being assigned to no crossing
dat$nps.lr <- 1 - dat$ps.lr

# Visualize propensity score distributions
ggplot(dat, aes(x = ps.lr, fill = factor(cross_attempt))) +
  geom_histogram(
    aes(y = after_stat(density)), 
    position = "identity", 
    alpha = 0.5,
    color = "black",
    binwidth = 0.02  # adjust as needed
  ) +
  scale_fill_discrete(name = "Cross Attempt", labels = c("No Cross", "Cross")) +
  labs(x = "Propensity Scores", y = "Density") +
  theme_minimal()

## treatment actually assigned (either crossed or not crossed)
dat$pAssign <- NA
dat$pAssign[dat$cross_attempt == 1]    <- dat$ps.lr[dat$cross_attempt   == 1]
dat$pAssign[dat$cross_attempt == 0] <- dat$nps.lr[dat$cross_attempt == 0]
## Smaller of pRhc vs pNoRhc for matching weight
dat$pMin <- pmin(dat$ps.lr, dat$nps.lr)

# Propensity score matching

dat$shot_attempt <- as.numeric(as.character(dat$shot_attempt))
dat$cross_attempt <- as.numeric(as.character(dat$cross_attempt))
dat <- na.omit(dat)

ps.lr.match <- Match(Y = dat$shot_attempt, Tr = dat$cross_attempt, X = dat$ps.lr, 
                     estimand = "ATT", M = 1, ties = TRUE, replace = TRUE)


ps.lr.match$est  # Average Treatment Effect
ps.lr.match$se   # Standard error of ATE

# Run bootstrapping with 1000 replicates
n_bootstrap <- 1000
ate<- list()
# Perform bootstrapping
for (i in 1:n_bootstrap) {
  cat("Bootstrap iteration:", i, "\n")
  boot_data <- dat[sample(1:nrow(dat), nrow(dat), replace = TRUE), ]
  boot_estimates <- Match(Y = boot_data$shot_attempt, Tr = boot_data$cross_attempt,
                          X = boot_data$ps.lr, estimand = "ATT", 
                          M = 1, ties = TRUE, replace = TRUE)
  ate[[i]]<- boot_estimates$est
  
}

boot_ate <- do.call(rbind, ate)

# Calculate standard error of the ATE
boot_se <- sd(boot_ate)

## Extract matched data
matched.samp <- dat[unlist(ps.lr.match[c("index.treated","index.control")]), ]

# Check for dropped unmatched samples
(dropped.match <- ps.lr.match$ndrops.matches)
dim(matched.samp)



tabMatched<- CreateTableOne(vars = names(matched.samp)[-c(1:2, 11:14)], 
                            data = matched.samp, strata = names(matched.samp)[2], test = FALSE)
print(tabMatched, smd = TRUE)
# Assess balance after matching
smds.Matched <- ExtractSmd(tabMatched) * 100
(balance <- cbind(smds, smds.Matched))  # Compare unmatched vs. matched balance


# Distribution of propensity scores after matching
ggplot(matched.samp, aes(x = ps.lr, fill = factor(cross_attempt))) +
  geom_histogram(
    aes(y = after_stat(density)), 
    position = "identity", 
    alpha = 0.5,
    color = "black",
    binwidth = 0.02  # adjust as needed
  ) +
  scale_fill_discrete(name = "Cross Attempt", labels = c("No Cross", "Cross")) +
  labs(x = "Propensity Scores", y = "Density") +
  theme_minimal()


######################## END ##########################