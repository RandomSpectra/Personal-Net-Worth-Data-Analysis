# Survey-design-adjusted inference for Personal Networth in the U.S.
# Run analysis.R first. This script uses Census Fay replicate weights.

if (!exists("du")) stop("First run source('analysis.R').")
if (!requireNamespace("survey", quietly = TRUE)) install.packages("survey")
library(survey)

# Replicate file uses a DIFFERENT delimiter from the main SIPP file on some releases.
# Let fread detect it, rather than forcing sep = '|'.
repcols <- paste0("REPWGT", 1:240)
needed <- c("SSUID", "PNUM", "MONTHCODE", repcols)
repfile <- "data/rw2019.csv.gz"

header <- names(fread(repfile, nrows = 0))
print(head(header, 8))
if (!all(needed %in% toupper(header))) {
  stop("Replicate file columns are not recognized. Check the printed header.")
}
actual <- header[match(needed, toupper(header))]
rw <- fread(repfile, select = actual,
            colClasses = list(character = header[match("SSUID", toupper(header))]))
setnames(rw, toupper(names(rw)))
rw <- rw[rw$MONTHCODE == 12, ]

# Match replicate weights to the eligible December respondents in analysis.R.
survey_data <- merge(du, rw, by = c("SSUID", "PNUM", "MONTHCODE"))
if (nrow(survey_data) != nrow(du)) {
  stop("Replicate weights did not match every person in du.")
}
rm(rw)

sipp <- svrepdesign(data = survey_data, weights = ~WPFINWGT,
                    repweights = survey_data[, ..repcols],
                    type = "Fay", rho = 0.5, combined.weights = TRUE,
                    mse = TRUE)

# Population age-group means and their design-adjusted confidence intervals.
age_design <- subset(sipp, decades %in% 2:8)
age_means <- svyby(~TNETWORTH, ~decades, age_design,
                   svymean, na.rm = TRUE, vartype = c("se", "ci"))
print(age_means)
write.csv(as.data.frame(age_means), "results/age_survey_ci.csv",
          row.names = FALSE)

# Weighted individual-level age regression.
age_fit <- svyglm(TNETWORTH ~ TAGE_EHC, design = sipp)
print(summary(age_fit))
write.csv(as.data.frame(coef(summary(age_fit))),
          "results/age_survey_regression.csv")

# Survey-design-adjusted comparison of mean wealth by sex.
sex_means <- svyby(~TNETWORTH, ~ESEX, sipp, svymean,
                   na.rm = TRUE, vartype = c("se", "ci"))
sex_test <- svyttest(TNETWORTH ~ ESEX, design = sipp)
print(sex_means)
print(sex_test)
write.csv(as.data.frame(sex_means), "results/sex_survey_means.csv",
          row.names = FALSE)
write.csv(data.frame(t_statistic = unname(sex_test$statistic),
                     p_value = sex_test$p.value),
          "results/sex_survey_test.csv", row.names = FALSE)

cat("Survey inference complete. See results/ for age and sex confidence intervals/tests.\n")
