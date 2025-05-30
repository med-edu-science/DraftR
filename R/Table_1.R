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