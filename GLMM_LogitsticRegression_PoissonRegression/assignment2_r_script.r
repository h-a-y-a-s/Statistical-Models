rm(list = ls()) 
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(knitr)
library(Hmisc)
library(lattice)
library(lme4)
library(MASS)
############################ Load Data ############################

# code from moodle to get data
require(Hmisc)
esm_data = spss.get("Data/ETT_ESM_Study1.sav", use.value.labels=TRUE)

# Plot of binary indicator for thinking of future, versus time of day,
# for first four subjects and all three days
require(lattice)
xyplot(future ~ TIME.S | DAY+Subject, esm_data[esm_data$Subject %in% unique(esm_data$Subject)[1:4],], 
       xlab = "Time of day", ylab = "Thinking of future?")

# view the structure of the data frame
str(esm_data)

# not really clear, so i will choose the relevant columns then continue
df_q1 <- esm_data %>%
  mutate(
    Subject = as.factor(Subject),
    DAY = as.factor(DAY),
    future = as.numeric(future),
    TIME.S = as.numeric(TIME.S)
  )
# view the structure of the data frame with the choosen cols
str(df_q1)


# num of subs
n_subjects <- df_q1 %>% 
  summarise(n_unique_subjects = n_distinct(Subject))
n_subjects

################################ Q1 ################################


################################ Missing data #################################

#### Missing data
colSums(is.na(df_q1))

#  -> no missing data in any variable 
# but we cant see this unless we have a "wide format"

num_missing_obs = 492*18 - nrow(df_q1)
num_missing_obs

num_missing_obs_p = (492*18 - nrow(df_q1)) / (492*18)
num_missing_obs_p

# check observations across days for each subject
incomplete_days_subs <- df_q1 %>%
  group_by(Subject, DAY) %>%
  summarise(obs_per_day = n(), .groups = 'drop') %>%
  group_by(Subject) %>%
  summarise(
    incomplete_days = sum(obs_per_day != 6),
    has_incomplete = any(obs_per_day != 6)
  )
head(incomplete_days_subs)
# number of incomplete subjects (who have at least one day with < 6 observations)
incomplete_subs <- incomplete_days_subs %>%
  summarise(
    n_incomplete = sum(has_incomplete),
    percent_incomplete = mean(has_incomplete) * 100
  )
incomplete_subs

# total num of observations per sub
subs_tot_observations <- df_q1 %>%
  group_by(Subject) %>%
  summarise(
    n_observations = n(),
    .groups = 'drop'
  ) 

# stats of total num of observations per sub
subs_tot_observations_stats  <- subs_tot_observations %>%
  summarise(
    mean_obs = mean(n_observations),
    median_obs = median(n_observations),
    min_obs = min(n_observations),
    max_obs = max(n_observations),
    sd_obs = sd(n_observations)
  )
subs_tot_observations_stats
# -> some subs have a very low number of observations (1 obs)

# frequency table of total num of observations per sub
obs_freq <- subs_tot_observations %>%
  count(n_observations) %>%
  mutate(proportion = n/sum(n)*100)
obs_freq
# -> 12 subs have one obs, 15 subs have 2 obs

# histogram of total num of observations per sub
ggplot(subs_tot_observations, aes(x = n_observations)) +
  geom_histogram(fill = "skyblue", color = "black") +
  labs(x = "Total Number of Observaions", y = "Frequency") +
  theme_minimal()

# compare missing observations across days
missing_per_day <- df_q1 %>%
  group_by(DAY) %>%
  summarise(
    total_expected = 487 * 6,  # Using consistent subject count
    actual_obs = n(),
    missing_obs = total_expected - actual_obs,
    missing_rate = (missing_obs/total_expected) * 100
  ) %>%
  arrange(DAY)
missing_per_day

# check missing data patterns 

# create a grid for all combinations of day and sub
all_combinations <- expand_grid(
  Subject = unique(df_q1$Subject),
  DAY = unique(df_q1$DAY),
  Obs = 1:6  # Six observations per day
)

# add an observation number to the original data
df_q1 <- df_q1 %>%
  group_by(Subject, DAY) %>%
  mutate(Obs = row_number()) %>%
  ungroup()

# join the original data with the complete grid
expanded_df <- all_combinations %>%
  left_join(df_q1, by = c("Subject", "DAY", "Obs"))

# fill missing in time and future values with NA
expanded_df <- expanded_df %>%
  mutate(
    TIME.S = ifelse(is.na(TIME.S), NA, TIME.S),
    future = ifelse(is.na(future), NA, future)
  )

# check if each sub and day has 6 observations
validation <- expanded_df %>%
  group_by(Subject, DAY) %>%
  summarise(n_obs = n(), .groups = "drop") %>%
  filter(n_obs != 6)

if (nrow(validation) == 0) {
  print("All subjects have 6 observations per day.")
} 

# check if missing num is correct
colSums(is.na(expanded_df))
# view the expanded dataframe
head(expanded_df)

mar_data <- expanded_df %>% 
  dplyr::select(Subject, DAY, future) %>%
  mutate(missing_future = is.na(future))

# test for MAR in long format
mar_test <- glm(missing_future ~ DAY , family = binomial, 
                     data = mar_data)

summary(mar_test)

# missingness by day
ggplot(mar_data, aes(x = DAY, fill = missing_future)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Missing future", y = "Proportion", fill = "Missing future")

################################ Stats #################################
#### Stats

#### DAY and future
# Frequency table for DAY
day_freq <- df_q1 %>%
  count(DAY) %>%
  mutate(proportion = n/sum(n))
day_freq

# Frequency table for future thinking
future_freq <- df_q1 %>%
  count(future) %>%
  mutate(proportion = n/sum(n))
future_freq

future_day_summary <- df_q1 %>%
  group_by(DAY, future) %>%
  summarise(
    count = n(),
    .groups = 'drop'
  ) %>%
  group_by(DAY) %>%
  mutate(
    proportion = count / sum(count),
    percentage = round(proportion * 100, 1)
  ) %>%
  arrange(DAY, future)
future_day_summary

# grouped bar plot
ggplot(future_day_summary, aes(x = DAY, y = proportion, fill = factor(future))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("lightblue", "darkblue"), 
                    name = "Future Thinking",
                    labels = c("No", "Yes")) +
  labs(
    title = "Future-Oriented Thinking by Day",
    x = "Day",
    y = "Proportion"
  ) +
  theme_minimal() 

#### Time

time_stats <- df_q1 %>%
  summarise(
    min_hours = min(TIME.S)/3600,
    max_hours = max(TIME.S)/3600,
    mean_hours = mean(TIME.S)/3600,
    median_hours = median(TIME.S)/3600,
    var_hours = var(TIME.S)/(3600^2)
  )
time_stats

# histogram of time in hours
ggplot(df_q1, aes(x = TIME.S/3600)) +
  geom_histogram(bins = 24, fill = "skyblue", color = "black") +
  labs(x = "Time (Hours)", y = "Frequency") +
  theme_minimal()

# time by day
time_by_day_stats <- df_q1 %>%
  group_by(DAY) %>%
  summarise(
    min_hours = min(TIME.S)/3600,
    max_hours = max(TIME.S)/3600,
    mean_hours = mean(TIME.S)/3600,
    median_hours = median(TIME.S)/3600,
    var_hours = var(TIME.S)/(3600^2)
  )
time_by_day_stats

ggplot(df_q1, aes(x = TIME.S/3600)) +
  geom_histogram(bins = 24, fill = "skyblue", color = "black") +
  facet_wrap(~DAY, labeller = labeller(DAY = function(x) paste("Day", x))) +
  labs(x = "Time (Hours)", y = "Frequency", title = "Histogram of Time by Day") +
  theme_minimal()

# time by future
time_by_fut_stats <- df_q1 %>%
  group_by(future) %>%
  summarise(
    min_hours = min(TIME.S)/3600,
    max_hours = max(TIME.S)/3600,
    mean_hours = mean(TIME.S)/3600,
    median_hours = median(TIME.S)/3600,
    var_hours = var(TIME.S)/(3600^2)
  )
time_by_fut_stats

ggplot(df_q1, aes(x = TIME.S/3600, fill = factor(future))) +
  geom_density(color = "black", alpha = 0.6) +
  scale_fill_manual(values = c("lightblue", "darkblue"), 
                    name = "Future Thinking",
                    labels = c("No", "Yes")) +
  labs(x = "Time (Hours)", y = "Density", title = "Density of Time by Future") +
  theme_minimal()


# time by future and day
time_by_day_by_future_stats <- df_q1 %>%
  group_by(DAY, future) %>%
  summarise(
    min_hours = min(TIME.S)/3600,
    max_hours = max(TIME.S)/3600,
    mean_hours = mean(TIME.S)/3600,
    median_hours = median(TIME.S)/3600,
    var_hours = var(TIME.S)/(3600^2)
  )
time_by_day_by_future_stats 

ggplot(df_q1, aes(x = TIME.S/3600, fill = factor(future))) +
  geom_density(alpha = 0.7) +
  facet_wrap(~DAY, labeller = labeller(DAY = function(x) paste("Day", x))) +
  scale_fill_manual(values = c("lightblue", "darkblue"), 
                    name = "Future Thinking",
                    labels = c("No", "Yes")) +
  labs(x = "Time (Hours)", y = "Density", title = "Distribution of Time by Day and Future Thinking") +
  theme_minimal()


################################ Logistic Reg #################################

# conver time to hours
df_q1$TIME.H <- df_q1$TIME.S/3600

# fit simple logistic regression
simple_model <- glm(future ~ TIME.H, family = binomial, data = df_q1)

# fit mixed effects model
mixed_model <- glmer(future ~ TIME.H + (1|Subject/DAY), 
                     family = binomial, data = df_q1)

# model comparisons
# summary of both models
summary(simple_model)
summary(mixed_model)

anova(mixed_model, simple_model,  test = "Chisq")

# compare AIC values
aic_comparison <- data.frame(
  Model = c("Simple Logistic", "Mixed Effects"),
  AIC = c(AIC(simple_model), AIC(mixed_model))
)
print(aic_comparison)

# 3. compare fixed effects coefs
fixed_effects <- data.frame(
  Model = c("Simple Logistic", "Mixed Effects"),
  Intercept = c(coef(simple_model)[1], fixef(mixed_model)[1]),
  Time_Coefficient = c(coef(simple_model)[2], fixef(mixed_model)[2])
)
print(fixed_effects)

# 4. random effects variance from mixed model
print("Random Effects Variance:")
print(VarCorr(mixed_model))

# 5. calculate R-squared
# for simple model
r2_simple <- 1 - simple_model$deviance/simple_model$null.deviance

# for mixed model using MuMIn package
# R² for mixed model calculated two ways:
# 1. Marginal R² - variance explained by fixed effects only:
#    var(fixed)/(var(fixed) + var(random) + var(residual))
# 2. Conditional R² - variance explained by entire model:
#    (var(fixed) + var(random))/(var(fixed) + var(random) + var(residual))
library(MuMIn)
r2_mixed <- r.squaredGLMM(mixed_model)
r2_mixed

r2_comparison <- data.frame(
  Model = c("Simple Logistic", "Mixed Effects (Marginal)", "Mixed Effects (Conditional)"),
  R_squared = c(r2_simple, r2_mixed[1], r2_mixed[3])
)
print(r2_comparison)

report::report(simple_model)
report::report(mixed_model)

################################ Q2 ################################

# make sure traits are constant for subs
cols <- c("A", "O", "C", "E", "N", "age", "sex")
for (col in cols){
  trait_variation <- esm_data %>%
    group_by(Subject) %>%
    summarise(
      unique_values = n_distinct(col, na.rm = TRUE),
      has_variation = unique_values > 1
    )
  cat(paste("variation in col ", col, " "))
  cat(any(trait_variation$has_variation), "\n")
}
## -> all values are ok, they are in fact fixed vars

# create subject-level dataset
subject_data <- esm_data %>%
  group_by(Subject) %>%
  summarise(
    n_observations = n(),
    n_future = sum(future),
    # take the first occurrence of these variables 
    # since they're constant per subject
    age = first(age),
    sex = first(sex),
    A = first(A),
    C = first(C),
    E = first(E),
    N = first(N),
    O = first(O)
  )
head(subject_data)

################################ Missing data #################################
# check for missing data
colSums(is.na(subject_data))

# count missing variables per subject
missing_per_subject <- data.frame(
  Subject = subject_data$Subject,
  num_missing = rowSums(is.na(subject_data[, c("A", "C", "E", "N", "O")]))
) %>%
  arrange(desc(num_missing))
max_missing <- max(missing_per_subject$num_missing)
max_missing
print(table(missing_per_subject$num_missing))

# remove subjects with all 5 trait variables missing
subject_data_clean <- subject_data %>%
  filter(!Subject %in% missing_per_subject$Subject[missing_per_subject$num_missing == 5])

cat("Removed", sum(missing_per_subject$num_missing == 5), 
    "subjects with all variables missing\n")
cat("Remaining subjects:", nrow(subject_data_clean), "\n")

# check the new distribution of missing values
missing_per_subject_new <- data.frame(
  Subject = subject_data_clean$Subject,
  num_missing = rowSums(is.na(subject_data_clean[, c("age", "sex", "A", "C", "E", "N", "O")]))
)
# print new frequency table
print(table(missing_per_subject_new$num_missing))

subject_data <- subject_data_clean
colSums(is.na(subject_data))

################################ Stats #################################
numeric_cols <- c("A", "O", "C", "E", "N", "age", "n_observations", "n_future")
numeric_cols_names <- c(
  "A" = "Agreeableness",
  "C" = "Conscientiousness",
  "E" = "Extraversion",
  "N" = "Neuroticism",
  "O" = "Openness",
  "age" = "Age",
  "n_observations" = "Number of observations",
  "n_future" = "Number of future thoughts"
)
# numeric descriptive statistics
for (var in numeric_cols){
  x <- subject_data[[var]]
  # basic stats
  cat("\n")
  stats <- data.frame(
    Variable = var,
    n_obs = length(x),
    n_missing = sum(is.na(x)),
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE)
  )
  print(stats)
  cat("\n")
}

library(moments)
skewness(subject_data$n_future)

sex_freq <- subject_data %>%
  count(sex) %>%
  mutate(proportion = n/sum(n))
sex_freq

## histograms  
# create plots list
plots <- list()
for(var in numeric_cols) {
  plots[[var]] <- ggplot(subject_data, aes(x = .data[[var]])) +
    geom_histogram(fill = "lightblue", color = "black", bins = 30) +
    labs(title =  numeric_cols_names[[var]],
         y = "Frequency",
         x = var) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}

# Print all plots
for(p in plots) {
  print(p)
}

library(gridExtra)
combined_plot <- grid.arrange(
  plots$A, plots$C, plots$E,
  plots$N, plots$O,plots$age,
  ncol = 3
)
combined_plot

## Numeric variables by sex histograms
# Create plots list
plots <- list()
for(var in numeric_cols) {
  plots[[var]] <- ggplot(subject_data, aes(x = .data[[var]], fill = sex,)) +
    geom_density(alpha=0.6) +
    labs(title = numeric_cols_names[[var]],
         y = "Density",
         x = var) +
    scale_fill_manual(values = c("lightblue", "darkblue"), 
                      name = "Sex",
                      labels = c("male", "female")) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}

# Print all plots
for(p in plots) {
  print(p)
}

library(gridExtra)
combined_plot <- grid.arrange(
  plots$A, plots$C, plots$E, plots$N, 
  plots$O,plots$age, plots$n_observations, plots$n_future,
  ncol = 4
)
combined_plot

## Numeric x Numeric 
# correlation plot matrix using GGally
library(GGally)
predictors <- c("A", "O", "C", "E", "N", "age")
pairs_plot <- ggpairs(subject_data[predictors],
                      lower = list(continuous = wrap("smooth", 
                                                     alpha = 0.3, 
                                                     color = "lightblue")),
                      upper = list(continuous = wrap("cor", size = 5))) +
  theme_minimal() +
  labs(title = "Correlation Matrix Plot") 
pairs_plot
ggsave("pairs_plot.png", pairs_plot, width = 12, height = 8, dpi = 300)

# Loop over pairs of traits
for (i in 1:(length(predictors)-1)) {
  for (j in (i+1):length(predictors)) {
    test <- cor.test(subject_data[[predictors[i]]], 
                     subject_data[[predictors[j]]], 
                     use = "pairwise.complete.obs")
    
    cat("\nCorrelation between", predictors[i], "and", numeric_cols[j], ":\n")
    cat("r =", round(test$estimate, 3), "\n")
    cat("p-value =", round(test$p.value, 10), "\n")
  }
}

################################ Poisson Reg #################################
# Define variables to model

predictors <- c("A", "C", "E", "N", "O", "age", "sex")

models <- list()
results <- data.frame(
  variable = predictors,
  coefficient = NA,
  std_error = NA,
  p_value = NA
)

# fit models in a loop
for(i in seq_along(predictors)) {
  # Create formula with offset
  formula <- as.formula(paste("n_future ~", predictors[i], "+ offset(log(n_observations))"))
  
  models[[i]] <- glm(formula, family = poisson(link = "log"), data = subject_data)
  
  coef_summary <- summary(models[[i]])$coefficients[2,]
  results$coefficient[i] <- coef_summary[1]
  results$std_error[i] <- coef_summary[2]
  results$p_value[i] <- coef_summary[4]
}

#  absolute coefficient column
results$abs_coef <- abs(results$coefficient)

# significance indicators
results$significance <- ifelse(results$p_value < 0.001, "***",
                               ifelse(results$p_value < 0.01, "**",
                                      ifelse(results$p_value < 0.05, "*", "ns")))

# Create formatted output sorted by p-value
formatted_results <- results %>%
  mutate(
    coefficient = round(coefficient, 3),
    std_error = round(std_error, 3),
    p_value = format.pval(p_value, digits = 3),
    abs_coef = round(abs_coef, 3)
  ) %>%
  arrange(p_value)

# Print results
cat("\nResults sorted by p-value:\n")
print(formatted_results)

# Print results sorted by absolute coefficient value
cat("\nResults sorted by absolute coefficient size:\n")
print(formatted_results[order(formatted_results$abs_coef, decreasing = TRUE), ])

test <- formatted_results[order(formatted_results$abs_coef, decreasing = TRUE), ]
png("test.png",  width  = 8, height= 5,
    units= "in", res = 1200)
grid.table(test)
dev.off()

############################# Model Selection #####################
n <- nrow(subject_data)

# basic model with all main effects
model_all <- glm(n_future ~ A + C + E + N + O + age + sex + 
                   offset(log(n_observations)), 
                 family=poisson, data=subject_data)

# full model with all two way interactions
model_all_int <- glm(n_future ~ (A + C + E + N + O + age + sex)^2 + 
                       offset(log(n_observations)), 
                     family=poisson, data=subject_data)

# stepwise selection with AIC 
step_aic <- stepAIC(model_all_int, direction="both", trace=FALSE)

# stepwise selection with BIC
step_bic <- stepAIC(model_all_int, direction="both", k=log(n), trace=FALSE)

models <- list(
  main_effects = model_all,
  full_interactions = model_all_int,
  step_aic = step_aic,
  step_bic = step_bic
)

# comparison
metrics <- data.frame(
  model = names(models),
  AIC = sapply(models, AIC),
  BIC = sapply(models, BIC),
  df = sapply(models, function(x) length(coef(x))),
  dispersion = sapply(models, function(x) {
    pearson_chi2 <- sum(residuals(x, type="pearson")^2)
    df <- x$df.residual
    return(pearson_chi2/df)
  })
)

# model comparison
cat("\nModel Comparison (sorted by BIC):\n")
print(metrics[order(metrics$BIC),])

png("BIC.png",  width  = 8, height= 5,
    units= "in", res = 1200)
grid.table(metrics[order(metrics$BIC),])
dev.off()

# Get best model (using BIC as criterion)
best_model_bic <- models[[which.min(metrics$BIC)]]
best_model_aic <- models[[which.min(metrics$AIC)]]

sjPlot::tab_model(best_model_bic)
sjPlot::tab_model(best_model_aic)

best_model <- best_model_bic

print(summary(best_model))

print(anova(model_all, best_model, test="Chisq"))


############ Over-dispersion C + D #########################
# the Pearson Chi-Square Dispersion 
pearson_chi2 <- sum(residuals(best_model, type="pearson")^2)
df <- best_model$df.residual
dispersion <- pearson_chi2/df

library(AER)
dispersiontest(best_model)


# Or alternatively using:
qp_model <- glm(formula = n_future ~ A + C + O + age + A:C + C:age + offset(log(n_observations)), 
                family = quasipoisson, data = subject_data)
summary(qp_model)
summary(qp_model)$dispersion

# fit negative binomial model
nb_model <- MASS::glm.nb(n_future ~ A + C + O + age + A:C + C:age + offset(log(n_observations)), data = subject_data)
summary(nb_model)

poisson_bic <- BIC(best_model)
poisson_bic
nb_bic <- BIC(nb_model)
nb_bic
