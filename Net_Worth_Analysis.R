# Run this file first. Run survey_inference.R afterwards for design-adjusted tests.

packages <- c("data.table", "R.utils")
for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
library(data.table)

dir.create("results", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)

# Data ---------------------------------------------------------------------
# The main public-use file is pipe-delimited. fread reads the .gz file directly.
cols <- c("SSUID", "PNUM", "MONTHCODE", "WPFINWGT", "TAGE_EHC",
          "ESEX", "TRACE", "EEDUC", "TNETWORTH")
pu <- fread("data/pu2019.csv.gz", sep = "|", select = cols,
            colClasses = list(character = "SSUID"))

# December only, one observation per person; net worth applies to ages 15+.
du <- pu[pu$MONTHCODE == 12 & pu$TAGE_EHC >= 15 &
         !is.na(pu$TNETWORTH) & is.finite(pu$TNETWORTH) &
         !is.na(pu$WPFINWGT) & pu$WPFINWGT > 0, ]

cat("Eligible December observations:", nrow(du), "\n")
print(summary(du$TNETWORTH))
print(table(du$TRACE))
print(table(du$EEDUC))

# Original Census code meanings, written explicitly rather than by list position.
du$ESEX <- factor(du$ESEX, levels = c(1, 2),
                  labels = c("Male", "Female"))
du$TRACE <- factor(du$TRACE)

# We define a weighted median function
weighted_median <- function(x, w) {
  keep <- !is.na(x) & !is.na(w) & w > 0
  x <- x[keep]; w <- w[keep]
  if (length(x) == 0) return(NA_real_)
  ord <- order(x)
  x <- x[ord]; w <- w[ord]
  x[which(cumsum(w) >= sum(w)/2)[1]]
}

# Age ----------------------------------------------------------------------
mean(du$TAGE_EHC, na.rm = TRUE)
median(du$TAGE_EHC, na.rm = TRUE)
sd(du$TAGE_EHC, na.rm = TRUE)

fit_age <- lm(du$TNETWORTH/100000 ~ du$TAGE_EHC)
print(summary(fit_age))
print(cor(du$TAGE_EHC, du$TNETWORTH, use = "complete.obs"))

png("figures/age_individual.png", width = 1000, height = 650)
plot(du$TNETWORTH/100000 ~ du$TAGE_EHC,
     xlab = "Age", ylab = "Net Worth in $100,000", pch = 20, cex = .3)
abline(fit_age, col = "red", lwd = 2)   
 dev.off()

du$decades <- floor(du$TAGE_EHC/10)
agegroups <- split(du, du$decades)
decades <- 2:8
meansagegroup <- sapply(as.character(decades), function(k)
  mean(agegroups[[k]]$TNETWORTH, na.rm = TRUE))
weightedmeansagegroup <- sapply(as.character(decades), function(k)
  weighted.mean(agegroups[[k]]$TNETWORTH, agegroups[[k]]$WPFINWGT,
                na.rm = TRUE))

fit_bydec <- lm(meansagegroup/100000 ~ decades)
ci <- predict(fit_bydec, newdata = data.frame(decades = decades),
              interval = "confidence", level = .95)
print(summary(fit_bydec))
print(ci)

age_table <- data.frame(decades, unweighted_mean = meansagegroup,
                        weighted_mean = weightedmeansagegroup,
                        fitted = ci[, "fit"], lower = ci[, "lwr"],
                        upper = ci[, "upr"])
write.csv(age_table, "results/age_decades.csv", row.names = FALSE)

png("figures/age_decades.png", width = 1000, height = 650)
plot(meansagegroup/100000 ~ decades, xlab = "Age in Decades",
     ylab = "Mean Networth in Hundred-Thousand $", pch = 19)
abline(fit_bydec, col = "red", lwd = 2)
 dev.off()

# Race ---------------------------------------------------------------------
racebreakup <- split(du, du$TRACE)
racecodes <- c("1", "2", "3", "4", "6", "8", "10")
racevector <- c("White", "Black", "AIAN", "Asian", "WB", "WA", "Other")

medianbyrace <- sapply(racecodes, function(k)
  median(racebreakup[[k]]$TNETWORTH, na.rm = TRUE))
weightedbyrace <- sapply(racecodes, function(k)
  weighted_median(racebreakup[[k]]$TNETWORTH,
                  racebreakup[[k]]$WPFINWGT))
race_n <- sapply(racecodes, function(k) nrow(racebreakup[[k]]))

race_table <- data.frame(race = racevector, census_code = racecodes,
                         n = race_n, unweighted_median = medianbyrace,
                         weighted_median = weightedbyrace)
print(race_table)
write.csv(race_table, "results/race_medians.csv", row.names = FALSE)

png("figures/race_medians.png", width = 1000, height = 650)
barplot(rbind(medianbyrace, weightedbyrace), beside = TRUE,
        names.arg = racevector, xlab = "Race", ylab = "Median Networth ($)",
        col = c("gray70", "gray35"),
        legend.text = c("Unweighted", "Survey weighted"))
 dev.off()

# Sex ----------------------------------------------------------------------
X <- split(du, du$ESEX)
male <- X[["Male"]]
female <- X[["Female"]]

malemean <- mean(male$TNETWORTH, na.rm = TRUE)
femalemean <- mean(female$TNETWORTH, na.rm = TRUE)
malevar <- var(male$TNETWORTH, na.rm = TRUE)
femalevar <- var(female$TNETWORTH, na.rm = TRUE)
i <- sum(!is.na(male$TNETWORTH)); j <- sum(!is.na(female$TNETWORTH))
diff <- malemean - femalemean
pooledvariance <- ((i-1)*malevar + (j-1)*femalevar)/(i+j-2)
test_statistic <- diff/sqrt(pooledvariance*(1/i + 1/j))
p_value <- 2*pt(-abs(test_statistic), df = i+j-2) 

sex_table <- data.frame(sex = c("Male", "Female"),
                        n = c(i, j), mean = c(malemean, femalemean),
                        weighted_mean = c(
                          weighted.mean(male$TNETWORTH, male$WPFINWGT),
                          weighted.mean(female$TNETWORTH, female$WPFINWGT)))
print(sex_table)
print(c(t_statistic = test_statistic, p_value = p_value))
write.csv(sex_table, "results/sex_means.csv", row.names = FALSE)
write.csv(data.frame(mean_difference = diff, pooled_t = test_statistic,
                     two_sided_p = p_value), "results/sex_test.csv",
          row.names = FALSE)

# Cumulative, deliberately overlapping, net-worth thresholds.
limits <- c("Below -$30,000", "Below $0", "Above $200,000", "Above $800,000")
subsets <- list(du[du$TNETWORTH < -30000, ], du[du$TNETWORTH < 0, ],
                du[du$TNETWORTH > 200000, ], du[du$TNETWORTH > 800000, ])

sex_proportions <- data.frame()
for (k in seq_along(subsets)) {
  g <- subsets[[k]]
  unweighted <- prop.table(table(g$ESEX))
  weighted <- tapply(g$WPFINWGT, g$ESEX, sum)
  weighted <- weighted/sum(weighted)
  sex_proportions <- rbind(sex_proportions,
    data.frame(threshold = limits[k], sex = names(unweighted),
               unweighted_share = as.numeric(unweighted),
               weighted_share = as.numeric(weighted)))
  png(paste0("figures/sex_threshold_", k, ".png"), width = 1000, height = 550)
  par(mfrow = c(1, 2))
  pie(unweighted, main = paste(limits[k], "(unweighted)"))
  pie(weighted, main = paste(limits[k], "(weighted)"))
  dev.off()
}
write.csv(sex_proportions, "results/sex_thresholds.csv", row.names = FALSE)

# Education ----------------------------------------------------------------
educgroups <- split(du, du$EEDUC)
educcodes <- names(educgroups)
medianbyeduc <- sapply(educgroups, function(g)
  median(g$TNETWORTH, na.rm = TRUE))
weightedbyeduc <- sapply(educgroups, function(g)
  weighted_median(g$TNETWORTH, g$WPFINWGT))
educ_n <- sapply(educgroups, nrow)

education_table <- data.frame(education_code = educcodes, n = educ_n,
                              unweighted_median = medianbyeduc,
                              weighted_median = weightedbyeduc)
print(education_table)
write.csv(education_table, "results/education_medians.csv", row.names = FALSE)

png("figures/education_medians.png", width = 1150, height = 650)
barplot(rbind(medianbyeduc, weightedbyeduc), beside = TRUE,
        names.arg = educcodes, xlab = "Education (EEDUC code)",
        ylab = "Median Networth ($)", col = c("gray70", "gray35"),
        legend.text = c("Unweighted", "Survey weighted"))
 dev.off()

cat("Descriptive analysis complete. See results/ and figures/.\n")
cat("Next: source('survey_inference.R') for design-adjusted inference.\n")
