############################################################################
# KOMPLETTES ALL-IN-ONE-SKRIPT MIT CLOUD-PDF-INTEGRATION + RCR + BIB-TLDR
# + DOI-ALS-CITATION-KEYS + RAGNAR-PDF-KONVERTIERUNG
############################################################################
# (einfach komplett kopieren & in RStudio einfügen)
############################################################################

# Pakete laden -------------------------------------------------------------
library(tidyverse)
library(httr2)
library(jsonlite)
library(glue)
library(ellmer)
library(ragnar)          #  <--  NEU
library(rentrez)
library(xml2)
library(purrr)

# --------------------------------------------------------------------------
# 0) EINSTELLUNGEN
# --------------------------------------------------------------------------
ZOTERO_API_KEY  <- "UUB4QNARgvhm08trkYE5cc5Z"   # ggf. anpassen
LIBRARY_ID      <- "9537692"
LIBRARY_TYPE    <- "user"
COLLECTION_NAME <- "OA_in_MedEd"
PDF_FOLDER      <- "/Users/hfriederichs/Desktop/Open Access"
TLDR_OUTPUT_FILE <- file.path(PDF_FOLDER, "all_tldrs.txt")
BIB_OUTPUT_FILE  <- file.path(PDF_FOLDER, "references.bib")

# --------------------------------------------------------------------------
# 1) HELPER-FUNKTIONEN
# --------------------------------------------------------------------------
## 1.1 Collection-Key abrufen
get_collection_key_by_name <- function(collection_name, library_type, library_id, api_key) {
  base_url <- glue("https://api.zotero.org/{library_type}s/{library_id}/collections")
  resp <- request(base_url) |> req_headers("Zotero-API-Key" = api_key) |> req_perform()
  collections <- resp_body_json(resp, simplifyVector = FALSE)
  collection <- detect(collections, ~ .x$data$name == collection_name)
  if (is.null(collection)) stop("Collection nicht gefunden.")
  collection$key
}

## 1.2 Cloud-PDF via ZIP (Sciebo) herunterladen
download_pdf_from_cloud_temporarily <- function(zotero_id) {
  base_url <- "https://uni-bielefeld.sciebo.de/s/Jn1xMawAp1IFYbC/download"
  file_url <- glue("{base_url}?path=/&files={zotero_id}.zip")
  temp_zip <- tempfile(fileext = ".zip")
  resp <- request(file_url) |> req_perform(path = temp_zip)
  if (!resp_status(resp) %in% 200:299) stop("Fehler beim Download: ", zotero_id)
  temp_unzip_dir <- tempfile(); dir.create(temp_unzip_dir)
  utils::unzip(temp_zip, exdir = temp_unzip_dir)
  pdf_files <- list.files(temp_unzip_dir, pattern = "\\.pdf$", full.names = TRUE)
  if (length(pdf_files) == 0) stop("Kein PDF in ZIP: ", zotero_id)
  file.remove(temp_zip)
  pdf_files[[1]]
}

## 1.3 PDF → Markdown (via Ragnar)  ------------------------  NEU ----------
convert_pdf_to_markdown <- function(pdf_path) {
  md <- ragnar::read_as_markdown(pdf_path)        # char-Vektor je Absatz/Zeile
  paste(md, collapse = "\n")                      # zu einem String vereinen
}

## 1.4 TLDR erstellen (Einzel-Call – wird später durch parallel_chat ersetzt)
create_tldr_for_pdf <- function(pdf_md_text, ellmer_model) {
  chat <- ellmer::chat_openai(model = ellmer_model, echo = TRUE)
  chat$chat(
    "Formuliere ein deutschsprachiges TLDR für den Text der Publikation, den man in einen Absatz zur Literaturübersicht einer wissenschaftlichen Arbeit einfügen kann. Erwähne im ersten Satz die Autoren (z.B. 'Smith et al.'). Ergänze konkrete Aussagen zu Effekten mit den Effektstärken und füge am Ende jedes entsprechenden Satzes die Referenz [@DOI], z. B. [@10.3205/zma001642] ein. Kontrolliere anhand des Abstracts und gib nur das endgültige TLDR aus.",
    pdf_md_text
  )
}

## 1.5 iCite-RCR abrufen
get_rcr_data <- function(pmids) {
  if (!length(pmids)) return(tibble())
  url <- "https://icite.od.nih.gov/api/pubs"
  batch_size <- 200; all_data <- list(); i <- 1
  while (i <= length(pmids)) {
    batch <- pmids[i:min(i + batch_size - 1, length(pmids))]
    resp  <- request(url) |> req_url_query(pmids = paste(batch, collapse = ",")) |> req_perform()
    j     <- resp_body_json(resp)
    recs  <- lapply(j$data, function(pub) list(
      pmid = pub$pmid %||% NA, doi = pub$doi %||% NA,
      relative_citation_ratio = pub$relative_citation_ratio %||% NA,
      citation_count          = pub$citation_count %||% NA))
    all_data <- c(all_data, recs); i <- i + batch_size
  }
  bind_rows(all_data) |> mutate(pmid = as.character(pmid))
}

## 1.6 BibTeX für eine Collection abrufen
fetch_all_items_bibtex <- function(library_type, library_id, api_key, collection_key) {
  base_url <- glue("https://api.zotero.org/{library_type}s/{library_id}/collections/{collection_key}/items")
  start <- 0; limit <- 100; all_bib <- character(0)
  while (TRUE) {
    resp <- request(base_url) |>
      req_url_query(format = "bibtex", key = api_key, start = start, limit = limit) |>
      req_headers("Zotero-API-Version" = "3") |>
      req_perform()
    if (!resp_has_body(resp)) break
    btxt <- resp_body_string(resp)
    if (!nchar(btxt)) break
    all_bib <- c(all_bib, btxt)
    start <- start + limit
  }
  paste(all_bib, collapse = "\n")
}

## 1.7 DOI normalisieren (keine Groß-/Kleinschreibung, keine /)
normalize_doi <- function(x) gsub("/", "", tolower(trimws(x)))

## 1.8 DOI des Parent-Items zu einem Attachment holen
get_parent_doi <- function(att, lib_id, lib_type, api_key) {
  if (is.null(att$data$parentItem) || att$data$parentItem == "")
    return(NA_character_)
  parent_key <- att$data$parentItem
  p_url <- glue("https://api.zotero.org/{lib_type}s/{lib_id}/items/{parent_key}")
  p_resp <- request(p_url) |> req_headers("Zotero-API-Key" = api_key) |> req_perform()
  p_json <- resp_body_json(p_resp, simplifyVector = TRUE)
  p_json$data$DOI %||% p_json$data$doi %||% NA_character_
}

## 1.9 TLDR in BibTeX einfügen (annote-Feld)
insert_tldr_into_bib <- function(bib_content, tldr_df) {
  
  # 1) alte Notiz/annote-Zeilen rauswerfen -------------------------------
  lines <- str_split(bib_content, "\n")[[1]]
  lines <- lines[!str_detect(lines, "^\\s*(annote|Notiz)\\s*=\\s*\\{")]
  
  # 2) vorbereiten --------------------------------------------------------
  tldr_df$doi_norm <- normalize_doi(tldr_df$doi)
  new_lines    <- character()
  inside_entry <- FALSE
  current_doi  <- NA
  
  # 3) Einträge durchlaufen ----------------------------------------------
  for (line in lines) {
    
    if (str_detect(line, "^@\\w+\\{")) {          # Beginn @article{...
      inside_entry <- TRUE
      current_doi  <- NA
    }
    
    if (inside_entry && str_detect(line, "\\bdoi\\s*=\\s*\\{.+\\}")) {
      current_doi <- str_replace(line, ".*doi\\s*=\\s*\\{([^}]+)\\}.*", "\\1") |>
        normalize_doi()
    }
    
    if (inside_entry && str_detect(line, "^\\}\\s*$")) {   # Ende des Eintrags
      if (!is.na(current_doi)) {
        hit <- tldr_df |> filter(doi_norm == current_doi)
        if (nrow(hit) == 1) {
          val <- hit$tldr[1] |>
            str_replace_all("%", "\\\\%") |>  # % escapen
            str_replace_all("[{}]", "")       # {} entfernen
          new_lines <- c(new_lines,
                         paste0("  annote = {", val, "},"))
        }
      }
      inside_entry <- FALSE
    }
    
    new_lines <- c(new_lines, line)
  }
  
  paste(new_lines, collapse = "\n")
}

## 1.10 Citation-Keys in DOI umwandeln  (fix: case-insensitive)
rename_bibkeys_to_doi <- function(bib_content) {
  
  bib_lines   <- str_split(bib_content, "\n")[[1]]
  start_idx   <- NULL
  current_doi <- NULL
  
  for (i in seq_along(bib_lines)) {
    line <- bib_lines[i]
    
    # Start eines neuen Eintrags?
    if (str_detect(line, "^@\\w+\\{")) {
      start_idx   <- i
      current_doi <- NULL
    }
    
    # DOI innerhalb des Eintrags finden (case-insensitive!)
    if (!is.null(start_idx) &&
        str_detect(line,
                   regex("\\bdoi\\s*=\\s*\\{[^}]+\\}", ignore_case = TRUE))) {
      current_doi <- str_match(line, "\\{([^}]+)\\}")[, 2] |> trimws()
      # Alternative ohne Slash:
      # current_doi <- str_replace_all(current_doi, "/", "_")
    }
    
    # Ende des Eintrags?
    if (!is.null(start_idx) && str_detect(line, "^\\}\\s*$")) {
      if (!is.null(current_doi)) {
        old_line <- bib_lines[start_idx]
        bib_lines[start_idx] <- str_replace(
          old_line,
          "^(@\\w+\\{)[^,]+",
          paste0("\\1", current_doi)
        )
      }
      start_idx   <- NULL
      current_doi <- NULL
    }
  }
  
  paste(bib_lines, collapse = "\n")
}

# --------------------------------------------------------------------------
# 2) START WORKFLOW
# --------------------------------------------------------------------------
cat("🔁 Starte TLDR-Workflow mit Cloud-PDFs, iCite-RCR und BibTeX...\n")

## 2.1 Collection-Key
collection_key <- get_collection_key_by_name(COLLECTION_NAME, LIBRARY_TYPE, LIBRARY_ID, ZOTERO_API_KEY)

## 2.2 Alle Items dieser Collection abrufen (1-Page reicht hier)
base_url <- glue("https://api.zotero.org/{LIBRARY_TYPE}s/{LIBRARY_ID}/collections/{collection_key}/items")
resp <- request(base_url) |>
  req_url_query(limit = 100, format = "json") |>
  req_headers("Zotero-API-Key" = ZOTERO_API_KEY) |>
  req_perform()
items <- resp_body_json(resp, simplifyVector = FALSE)

## 2.3 PDF-Attachments + DOIs ermitteln
pdf_attachments <- keep(items, ~ .x$data$itemType == "attachment" &&
                          .x$data$contentType == "application/pdf")

zotero_ids <- map_chr(pdf_attachments, "key")
dois       <- map_chr(pdf_attachments,
                      get_parent_doi,
                      lib_id  = LIBRARY_ID,
                      lib_type = LIBRARY_TYPE,
                      api_key = ZOTERO_API_KEY)

att_tbl <- tibble(zotero_id = zotero_ids, doi = dois)
if (any(is.na(att_tbl$doi))) {
  cat("⚠️  Für einige Attachments wurde kein DOI gefunden – diese werden übersprungen.\n")
  att_tbl <- filter(att_tbl, !is.na(doi) & nzchar(doi))
}
if (nrow(att_tbl) == 0) stop("Keine PDFs mit DOI gefunden, Workflow abgebrochen.")

############################################################################
# 3) TLDRs ERZEUGEN – parallel_chat ---------------------------------------
############################################################################
ellmer::params(temperature = 0)

## 3.1 PDF herunterladen + Markdown konvertieren
pdf_info <- att_tbl |>
  mutate(
    pdf_path = map_chr(zotero_id, download_pdf_from_cloud_temporarily),
    md_text  = map_chr(pdf_path, convert_pdf_to_markdown)
  )

## 3.2 System-Prompt + Prompts
sys_prompt <- paste(
  "Formuliere ein deutschsprachiges TLDR für den Text der Publikation,",
  "den man in einen Absatz zur Literaturübersicht einer wissenschaftlichen",
  "Arbeit einfügen kann. Erwähne im ersten Satz die Autoren (z. B. 'Smith et al.') und die Studienart.",
  "Ergänze konkrete Aussagen zu Effekten mit den Effektstärken und füge am Ende jedes entsprechenden Satzes die Referenz [@DOI], z. B. [@10.3205/zma001642] ein.",
  "Kontrolliere anhand des Abstracts und gib nur das endgültige TLDR aus."
)

prompts <- map2(pdf_info$md_text, pdf_info$doi, function(body, the_doi) {
  # DOI extra an Chat schicken, damit [@DOI] stimmt
  c(sys_prompt,
    paste("Die DOI der Publikation lautet:", the_doi),
    body)
})

## 3.3 Chat-Basis & parallel_chat
chat_base <- ellmer::chat_openai(
  model = "o3-2025-04-16",
  echo  = TRUE
)

chats <- ellmer::parallel_chat(
  chat_base,
  prompts,
  max_active = 10,   # Parallelität
  rpm        = 450   # Provider-Limit beachten
)

## 3.4 TLDR-Texte einsammeln
tldr_results <- tibble(
  doi  = pdf_info$doi,
  tldr = map_chr(chats, ~ .x$last_turn()@text)
)

## 3.5 Aufräumen & Abspeichern
walk(pdf_info$pdf_path, file.remove)
writeLines(
  sprintf("DOI: %s\nTLDR:\n%s\n\n", tldr_results$doi, tldr_results$tldr),
  TLDR_OUTPUT_FILE
)
cat("✅ TLDRs gespeichert in:", TLDR_OUTPUT_FILE, "\n")

############################################################################
# 4) BibTeX erweitern + Citation-Keys anpassen ----------------------------
############################################################################
bib_raw    <- fetch_all_items_bibtex(LIBRARY_TYPE, LIBRARY_ID, ZOTERO_API_KEY, collection_key)
bib_tldr   <- insert_tldr_into_bib(bib_raw, tldr_results)
bib_final  <- rename_bibkeys_to_doi(bib_tldr)   # <-- NEUER SCHRITT
writeLines(bib_final, BIB_OUTPUT_FILE)
cat("📚 BibTeX mit TLDRs & DOI-Keys gespeichert in:", BIB_OUTPUT_FILE, "\n")

cat("🎉 Workflow abgeschlossen!\n")