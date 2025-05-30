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
flow_df <- raw_df %>% 
  mutate(
    excl_screen = if_else(!eligible, exclusion_reason, NA_character_),
    included_id = if_else( eligible, id, NA_character_)
  )

if (randomised_trial) {                              # ── ZWEIARMIG ──
  
  flow_df <- flow_df %>% 
    mutate(
      arm         = if_else( eligible & randomised, treatment_arm, NA_character_),
      excl_fu     = if_else( randomised & !analysed, "Lost to follow-up", NA_character_),
      analysed_id = if_else( analysed, id, NA_character_)
    ) %>% 
    mutate(
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
  
  g <- consort_plot(
    data       = flow_df,
    orders     = orders,
    allocation = "arm",
    side_box   = side_boxes,
    cex        = 0.9, text_width = 26
  )
  
} else {                                              # ── EINARMIG ──
  
  flow_df <- flow_df %>% 
    mutate(
      excl_fu     = if_else( eligible & !analysed, "Lost to follow-up", NA_character_),
      analysed_id = if_else( analysed, id, NA_character_)
    ) %>% 
    mutate(
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
  
  g <- consort_plot(
    data       = flow_df,
    orders     = orders,
    side_box   = side_boxes,
    cex        = 0.9, text_width = 26
  )
}

## ── 2)  Phasen-Positionen bestimmen (für die gedrehten Labels) ─────────
if (show_phase_labels) {
  
  side_vars   <- names(side_boxes)                 # kann Länge 0 sein
  main_orders <- Filter(\(o) !names(o) %in% side_vars, orders)
  
  order_names <- vapply(main_orders, names, character(1))
  row_map     <- setNames(seq_along(order_names), order_names)
  ## bspw.  id = 1, included_id = 2, arm = 3, excl_fu = 4, analysed_id = 5
  
  ## +1 verschiebt Allocation, Follow-up, Analysis eine Zeile tiefer
  row_vec <- c(
    Screening   = unname(row_map["id"]),
    Allocation  = unname(row_map["arm"]        + 1L),
    `Follow-up` = unname(row_map["excl_fu"]    + 1L),
    Analysis    = unname(row_map["analysed_id"]+ 1L)
  )
  row_vec <- row_vec[!is.na(row_vec)]
}

## ── 3)  Diagramm zeichnen ──────────────────────────────────────────────
grid.newpage()
plot(g)                      # consort-Diagramm

## ── 4)  Gedrehte Buttons links daneben setzen ──────────────────────────
if (show_phase_labels && length(row_vec)) {
  
  max_row <- max(row_vec)
  
  for (phase in names(row_vec)) {
    
    ## y-Koordinate: Mitte der jeweiligen Zeile
    y_pos <- unit(1 - (row_vec[phase] - 0.5) / max_row, "npc")
    
    grid.text(
      label = phase,                        # z. B. "Allocation"
      x     = unit(0.02, "npc"),            # 2 % von links
      y     = y_pos,
      just  = c("centre", "centre"),
      rot   = 90,                           # 90° gegen den Uhrzeiger
      gp    = gpar(col = "grey30",          # Farbe/SCHRIFT
                   cex = 0.9,
                   fontface = "bold")
    )
  }
}