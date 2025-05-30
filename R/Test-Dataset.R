# ─────────────────────────────────────────────────────────────
# Simulierte Rohdaten für eine klinische Studie
# ─────────────────────────────────────────────────────────────

library(tidyverse)

set.seed(24)
n_patients <- 200
# Ausschlussgründe
exclusion_reasons <- c("MRI missing", "Consent withdrawn",
                       "Screen failure", "Other protocol violation")

# 1) IDs
raw_df <- tibble(id = sprintf("%03d", 1:n_patients)) |> 
  dplyr::mutate(
    eligible   = rbinom(n(), 1, 0.85) == 1,
    randomised = eligible  & (rbinom(n(), 1, 0.93) == 1), # bei EINARMIG: randomised = FALSE
    analysed   = randomised & (rbinom(n(), 1, 0.95) == 1) # bei EINARMIG: analysed   = eligible & (rbinom(n(), 1, 0.95) == 1),
  ) |> 
  dplyr::mutate(
    exclusion_reason = if_else(
      !eligible,
      sample(exclusion_reasons, n(), replace = TRUE),
      NA_character_
    )
  ) |> # EINARMIG: treatment_arm entfällt – hier Dummy-Spalte, falls benötigttreatment_arm = NA_character_
  dplyr::mutate(
    treatment_arm = case_when(
      randomised & (rbinom(n(), 1, 0.5) == 1) ~ "intervention",
      randomised                              ~ "control",
      TRUE                                    ~ NA_character_
    )
  ) |> 
  dplyr::mutate(
    primary_event = if_else(
      analysed,
      sample(c("yes", "no"), n(), replace = TRUE, prob = c(0.30, 0.70)),
      NA_character_
    ),
    age = if_else(
      analysed,
      round(rnorm(n(), mean = 65, sd = 10)),
      NA_real_
    ),
    sex = if_else(
      analysed,
      sample(c("m", "f"), n(), replace = TRUE),
      NA_character_
    ),
    bmi = if_else(
      analysed,
      round(rnorm(n(), mean = 27, sd = 4), 1),
      NA_real_
    ),
    biomarker1 = if_else(
      analysed,
      round(rnorm(n(), mean = 3.5, sd = 1.2), 2),
      NA_real_
    ),
    biomarker2 = if_else(
      analysed,
      round(rlnorm(n(), meanlog = 1.2, sdlog = 0.5), 2),
      NA_real_
    )
  ) |>
  readr::write_csv("study_raw.csv")


raw_df |>
  dplyr::mutate(eligible = ifelse(TRUE == 1, "Aborted by participant", NA),
                exclusion_reasons = ifelse(if_any(age:biomarker2, is.na), "Missing answers", NA),
                exc_overall = coalesce(eligible,exclusion_reasons)) |>
  consort::consort_plot(
    orders = c(id = "Initial participants",
               eligible = "Exclusion",
               id = "Submitted questionnaires",
               exclusion_reasons = "Exclusion",
               id = "Final sample"),
    side_box = c("eligible", "exclusion_reasons"))
