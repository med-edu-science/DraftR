# ─────────────────────────────────────────────────────────────
# Simulierte Rohdaten für eine klinische Studie
# erstellt: 24-May-2025
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

############################################################################
##  CONSORT-Diagramm (ein- oder zweiarmig)  –  dynamische Phasen-Labels   ##
############################################################################
library(dplyr)
library(consort)        # ≥ 1.2.2
library(grid)

show_phase_labels <- TRUE   # Buttons ja/nein?

## ── 0)  Design erkennen ────────────────────────────────────────────────
randomised_trial <- any(raw_df$randomised, na.rm = TRUE)

## ── 1)  Dispositionstabelle aufbereiten ────────────────────────────────
flow_df <- raw_df |> 
  dplyr::mutate(
    excl_screen = if_else(!eligible, exclusion_reason, NA_character_),
    included_id = if_else( eligible, id, NA_character_)
  )

if (randomised_trial) {                              # ── ZWEIARMIG ──
  
  flow_df <- flow_df |> 
    dplyr::mutate(
      arm         = if_else( eligible & randomised, treatment_arm, NA_character_),
      excl_fu     = if_else( randomised & !analysed, "Lost to follow-up", NA_character_),
      analysed_id = if_else( analysed, id, NA_character_)
    ) |> 
    dplyr::mutate(
      included_id = if_else(!is.na(excl_screen), NA_character_, included_id),
      arm         = if_else(!is.na(excl_screen), NA_character_, arm),
      excl_fu     = if_else(!is.na(excl_screen), NA_character_, excl_fu),
      analysed_id = if_else(!is.na(excl_screen) | !is.na(excl_fu), NA_character_, analysed_id)
    )
  
  side_vars  <- c("excl_screen", "excl_fu")
  side_boxes <- side_vars[vapply(flow_df[side_vars], \(x) any(!is.na(x)), logical(1))]
  
  orders <- list(
    c(id          = "Assessed for eligibility"),
    c(excl_screen = "Excluded\n(screening)"),
    c(included_id = "Eligible"),
    c(arm         = "Randomised"),
    c(excl_fu     = "Lost to follow-up"),
    c(analysed_id = "Analysed")
  )
  
  g <- consort::consort_plot(
    data       = flow_df,
    orders     = orders,
    allocation = "arm",
    side_box   = side_boxes,
    cex        = 0.9, text_width = 26
  )
  
} else {                                              # ── EINARMIG ──
  
  flow_df <- flow_df |> 
    dplyr::mutate(
      excl_fu     = if_else( eligible & !analysed, "Lost to follow-up", NA_character_),
      analysed_id = if_else( analysed, id, NA_character_)
    ) |> 
    dplyr::mutate(
      included_id = if_else(!is.na(excl_screen), NA_character_, included_id),
      excl_fu     = if_else(!is.na(excl_screen), NA_character_, excl_fu),
      analysed_id = if_else(!is.na(excl_screen) | !is.na(excl_fu), NA_character_, analysed_id)
    )
  
  side_vars  <- c("excl_screen", "excl_fu")
  side_boxes <- side_vars[vapply(flow_df[side_vars], \(x) any(!is.na(x)), logical(1))]
  
  orders <- list(
    c(id          = "Assessed for eligibility"),
    c(excl_screen = "Excluded\n(screening)"),
    c(included_id = "Included in study"),
    c(excl_fu     = "Lost to follow-up"),
    c(analysed_id = "Analysed")
  )
  
  g <- consort::consort_plot(
    data       = flow_df,
    orders     = orders,
    side_box   = side_boxes,
    cex        = 0.9, text_width = 26
  )
}

## ---------------------------------------------------------------------
## 2) Phasen-Positionen ermitteln  ─────────────────────────────────────
## ---------------------------------------------------------------------
if (show_phase_labels) {
  
  side_vars   <- names(side_boxes)                               # »excl_screen«, …
  main_orders <- Filter(function(o) !names(o) %in% side_vars, orders)
  
  order_names <- vapply(main_orders, names, FUN.VALUE = character(1))
  row_map     <- stats::setNames(seq_along(order_names), order_names)
  # Beispiel: id = 1, included_id = 2, arm = 3, …
  
  ## +1 verschiebt Allocation, Follow-up, Analysis eine Zeile nach unten
  row_vec <- c(
    Screening   = unname(row_map["id"]),
    Allocation  = unname(row_map["arm"]        + 1L),
    `Follow-up` = unname(row_map["excl_fu"]    + 1L),
    Analysis    = unname(row_map["analysed_id"]+ 1L)
  )
  row_vec <- row_vec[!is.na(row_vec)]
}

## ---------------------------------------------------------------------
## 3) Diagramm zeichnen und als PNG (300 dpi) sichern  ─────────────────
## ---------------------------------------------------------------------
# a) interaktiv anzeigen ------------------------------------------------
grid::grid.newpage()
graphics::plot(g)      # consort-Diagramm

if (show_phase_labels && length(row_vec)) {
  max_row <- max(row_vec)
  for (phase in names(row_vec)) {
    y_pos <- grid::unit(1 - (row_vec[phase] - 0.5) / max_row, "npc")
    
    grid::grid.text(
      label = phase,
      x     = grid::unit(0.02, "npc"),   # 2 % vom linken Rand
      y     = y_pos,
      just  = c("centre", "centre"),
      rot   = 90,                        # um 90° gedreht
      gp    = grid::gpar(col = "grey30",
                         cex = 0.9,
                         fontface = "bold")
    )
  }
}

# b) als PNG (300 dpi) exportieren --------------------------------------
#  → ragg::agg_png liefert saubere Rastergrafiken
ragg::agg_png(filename = "fig_2.png",
              width    = 6,      # inch
              height   = 8,      # inch
              units    = "in",
              res      = 300)

grid::grid.newpage()
graphics::plot(g)

if (show_phase_labels && length(row_vec)) {
  max_row <- max(row_vec)
  for (phase in names(row_vec)) {
    y_pos <- grid::unit(1 - (row_vec[phase] - 0.5) / max_row, "npc")
    
    grid::grid.text(
      label = phase,
      x     = grid::unit(0.02, "npc"),
      y     = y_pos,
      rot   = 90,
      gp    = grid::gpar(col = "grey30",
                         cex = 0.9,
                         fontface = "bold")
    )
  }
}

grDevices::dev.off()   # Grafik-Device schließen

## PDF-Gerät öffnen  -----------------------------------------------------
grDevices::pdf("fig_2.pdf", width = 6, height = 8)   # Maße in inch

## Konsort-Diagramm + gedrehte Labels zeichnen --------------------------
grid::grid.newpage()
graphics::plot(g)                           # consort-Objekt ausgeben

if (show_phase_labels && base::length(row_vec)) {
  for (phase in base::names(row_vec)) {
    
    y_pos <- grid::unit(
      1 - (row_vec[phase] - 0.5) / base::max(row_vec),
      "npc"
    )
    
    grid::grid.text(
      label = phase,
      x     = grid::unit(0.02, "npc"),      # 2 % vom linken Rand
      y     = y_pos,
      rot   = 90,                           # 90° drehen
      gp    = grid::gpar(col = "grey30",
                         cex = 0.9,
                         fontface = "bold")
    )
  }
}

## PDF schließen  -------------------------------------------------------
grDevices::dev.off()

#  Table 1
tbl1_plot <- raw_df |> 
  gtsummary::tbl_summary(by = treatment_arm, include = c(age, sex)) |>
  gtsummary::add_p(pvalue_fun = gtsummary::label_style_pvalue(digits = 2)) |>
  gtsummary::add_overall() |>
  gtsummary::add_n() |>
  gtsummary::modify_header(label ~ "**Variable**") |>
  gtsummary::modify_spanning_header(c("stat_1", "stat_2") ~ "**Intervention**") |>
  gtsummary:: modify_footnote(
    gtsummary::all_stat_cols() ~ "Median (IQR) or Frequency (%)"
  ) |>
  gtsummary::modify_caption("**Table 1. Participant Characteristics**") |>
  gtsummary::bold_labels()

tbl1_plot |>
  bstfun::as_ggplot() |>
  ggplot2::ggsave(filename = "table_1.png", width = 7, height = 4, dpi = 300)

tbl1_plot |>
  gtsummary::as_gt() |>
  apa7::apa_style() |>
  gt::gtsave("tab_1.docx")

## ggstatsplot – z. B. Vergleich BMI nach Gruppe
fig2_plot <- raw_df |>
  dplyr::filter(analysed == TRUE) |>
  dplyr::select(treatment_arm, biomarker1) |>
  ggstatsplot::ggbetweenstats(
  x           = treatment_arm,
  y           = biomarker1,
  outlier.tagging = TRUE
)
ggplot2::ggsave("fig_2.png", plot = fig2_plot, width = 7, height = 4, dpi = 300)
