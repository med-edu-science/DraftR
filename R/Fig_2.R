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