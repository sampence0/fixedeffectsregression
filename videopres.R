## VIDEO PRESENTATION ##

# --- IMPORT LIBRARIES AND DATA --- #

# Libraries are necessary for data handling and analysis
library(haven)     
library(naniar)   
library(mice)    
library(plm)
library(ggplot2)

# Reading the .dta file and writing it to a .csv for versatility in usage
sasp <- read_dta("sasp.dta")
write.csv(sasp, "sasp.csv", row.names = FALSE)

# --- DATA CLEANING --- #
# Checking for missing values to understand the extent of missing data in the dataset
missing_values <- sapply(sasp, function(x) sum(is.na(x)))

# Converting 'session' to a factor since it is categorical data
sasp$session <- as.factor(sasp$session)

# Removing duplicated rows to ensure the uniqueness of each data entry
sasp <- sasp[!duplicated(sasp), ]

# Numeric Variables: Mean/Median Imputation
# 'bmi' is imputed with the median due to potential skewness in its distribution
sasp$bmi[is.na(sasp$bmi)] <- median(sasp$bmi, na.rm = TRUE)
# 'age' and 'schooling' are imputed with the mean assuming a normal distribution
sasp$age[is.na(sasp$age)] <- mean(sasp$age, na.rm = TRUE)
sasp$schooling[is.na(sasp$schooling)] <- mean(sasp$schooling, na.rm = TRUE)

# Categorical Variables: Mode Imputation
# Mode imputation is used for categorical variables as it assigns the most frequent category
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
sasp$asian_cl[is.na(sasp$asian_cl)] <- getmode(sasp$asian_cl)
sasp$black_cl[is.na(sasp$black_cl)] <- getmode(sasp$black_cl)
sasp$hispanic_cl[is.na(sasp$hispanic_cl)] <- getmode(sasp$hispanic_cl)

# Standardizing column names to lowercase for consistency across the dataset
colnames(sasp) <- tolower(colnames(sasp))

# Removing rows with any remaining missing values post-imputation
# This step ensures the dataset used in analysis is complete (no missing values)
sasp_complete <- sasp[complete.cases(sasp), ]

# Final check to ensure no missing values are left before proceeding to analysis
sapply(sasp_complete, function(x) sum(is.na(x)))

# --- EXPLORATORY DATA ANALYSIS --- #
summary(sasp_complete)  
hist(sasp_complete$age, main="Age Distribution of Workers", xlab="Age") 
hist(sasp_complete$lnw, main="Log of Hourly Wage Distribution", xlab="LNW") 
hist(sasp_complete$schooling, main="Schooling Distribution")
boxplot(sasp_complete$bmi)

# Creating a simple pie chart for the 'schooling' variable, excluding small categories
schooling_counts <- table(sasp_complete$schooling)
schooling_df <- as.data.frame(schooling_counts)
colnames(schooling_df) <- c("Schooling", "Count")
schooling_df$Proportion <- schooling_df$Count / sum(schooling_df$Count)
threshold <- 0.01  # 1% threshold
schooling_df <- schooling_df[schooling_df$Proportion > threshold,]
ggplot(schooling_df, aes(x = "", y = Proportion, fill = Schooling)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  theme(legend.title = element_blank()) +
  labs(fill = "Schooling Level", title = "Distribution of Schooling Levels")



# Significant Correlated Features
threshold <- 0.3
correlation_matrix <- cor(sasp_complete[
  , sapply(sasp_complete, is.numeric)], use = "complete.obs")
significant_correlations <- which(
  abs(correlation_matrix) > threshold, arr.ind = TRUE)
significant_correlations <- significant_correlations[
  significant_correlations[,1] != significant_correlations[,2],]
if (nrow(significant_correlations) > 0) {
  for (i in 1:nrow(significant_correlations)) {
    row <- significant_correlations[i, ]
    cat(colnames(correlation_matrix)[row[1]], "&",
          colnames(correlation_matrix)[row[2]], ":", correlation_matrix[row[1],
            row[2]], "\n")
  }
} else {
  cat("No significant correlations found above the threshold of", threshold, "\n")
}

# --- MODELING --- #
# Pooled OLS Regression
pooled_data <- sasp_complete[, !(names(sasp_complete) %in% c("id", "session"))]
pooled_ols_model <- lm(lnw ~ ., data = pooled_data)
summary(pooled_ols_model)


# Fixed Effects Regression
fixed_effects_model <- plm(lnw ~ ., data = sasp_complete,
                           index = c("id", "session"), model = "within")
summary(fixed_effects_model)

# Demeaned OLS Regression
sasp_demeaned <- sasp_complete
for (var in names(sasp_demeaned)) {
  if (!var %in% c("id", "session")) {
    sasp_demeaned[[var]] <- sasp_demeaned[[var]] - 
      ave(sasp_demeaned[[var]], sasp_demeaned$id, FUN = mean)
  }
}
demeaned_ols_model <- lm(lnw ~ . - 1, data = sasp_demeaned)
summary(demeaned_ols_model)

# remove variables causing singularities in demeaned model (NA coefs.)

variables_to_exclude <- c("age", "bmi", "schooling", "asian", "black", "hispanic", 
                          "other", "white", "asq", "cohab", "married", "divorced", 
                          "separated", "nevermarried", "widowed")
sasp_demeaned_new <- sasp_complete
for (var in setdiff(names(sasp_demeaned_new), c("id", "session", variables_to_exclude))) {
  sasp_demeaned_new[[var]] <- sasp_demeaned_new[[var]] - 
    ave(sasp_demeaned_new[[var]], sasp_demeaned_new$id, FUN = mean)
}
demeaned_ols_model_new <- lm(lnw ~ . - 1, data = sasp_demeaned_new)
summary(demeaned_ols_model_new)








