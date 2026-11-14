# Personal Networth in the U.S.

An analysis of personal net worth using the U.S. Census Bureau's 2019 Survey of Income and Program Participation (SIPP). The study examines relationships with age, race, sex, and educational attainment using R. The revised analysis presents both unweighted and survey-weighted estimates, as well as survey-design-adjusted inference for age and sex comparisons.

The [revised paper](reports/Personal_Networth_Final.pdf) contains the results and discussion.

## Reproducing the analysis

Download `pu2019.csv.gz` and `rw2019.csv.gz` from the [Census Bureau's 2019 SIPP directory](https://www2.census.gov/programs-surveys/sipp/data/datasets/2019/) and place them in a local `data/` folder. From the repository directory, run in R:

```r
source("analysis.R")
source("survey_inference.R")
```

The scripts generate CSV tables in `results/` and figures in `figures/`. The second script uses the Census-provided 240 Fay replicate weights to estimate standard errors and perform survey-adjusted inference. The large raw Census files are not included in the repository.

## Historical note

This project was originally completed in **2021**. The [original 2021 report](reports/original_2021.pdf) is included unchanged for historical comparison. In 2026, the code and statistical analysis were revisited to correct errors, add survey-weighted results and uncertainty estimates, and expand the paper to include educational attainment. The revised calculations use the publicly available 2019 SIPP dataset, which has been updated since 2021; numerical results may therefore differ from the original report.

## AI disclosure

The decision to extend the 2021 analysis with survey weighting and Fay's replicate-weight inference arose from a discussion of the project with **OpenAI's GPT-6** in 2026. The original 2021 analysis predates this assistance. The revised scripts were run on the Census data, and their generated outputs were used for the updated report.
