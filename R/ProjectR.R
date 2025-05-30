# Anlegen eines Projekt: apaquarto-Style 
# Ausgabe als HTML über GitLab-Pages
# Ausgabe als DOCX über sciebo

#############################################
# Anlegen eines Projekt-Ordners mit apaquarto
#############################################

apa7::install_apaquarto()

############################################################
# Anlegen eines Projekts in GitLab, 
# aktuell unter Medical-Education / Research (group-id 1897)
# Nach dem Anlegen wird der ganze Projekt-Ordner gepusht
############################################################

# -------------------------------------------------------------------
# 1) GitLab-Verbindung (REST-API) – benötigt immer den PAT
# -------------------------------------------------------------------
gitlabr::set_gitlab_connection(
  gitlab_url    = "https://gitlab.ub.uni-bielefeld.de",
  private_token = Sys.getenv("GITLAB_BIE_TOKEN")   # PAT mit api-Scope
)

# -------------------------------------------------------------------
# 2) Projekt direkt in der Ziel-Gruppe anlegen
# -------------------------------------------------------------------
grp_id <- 1897   # Medical-Education / Research

proj_info <- gitlabr::gl_new_project(
  name         = "demo-rstudio-gitlabr-27-05-2025",
  description  = "Spielwiese für {gitlabr}-Tests",
  namespace_id = grp_id,
  visibility   = "private",
  default_branch = "main",
  initialize_with_readme = FALSE
)

# -------------------------------------------------------------------
# 3) PAT einmal im Credential-Store hinterlegen
# -------------------------------------------------------------------
credentials::credential_approve(
  list(
    protocol = "https",
    host     = "gitlab.ub.uni-bielefeld.de",
    username = "git",                         # Benutzername beliebig
    password = Sys.getenv("GITLAB_BIE_TOKEN") # PAT
  )
)

# -------------------------------------------------------------------
# 4) Remote auf HTTPS-URL setzen
# -------------------------------------------------------------------
if (!"origin" %in% gert::git_remote_list()$name) {
  gert::git_remote_add("origin", proj_info$http_url_to_repo)
} else {
  gert::git_remote_set_url(proj_info$http_url_to_repo, remote = "origin")
}

#################################################
# Für das Anlegen der Webseite über GitLab-Pages
#################################################

# Erzeugen einer index.qmd-Datei im Projekt-Root
glue::glue(
  "
  ---
  format: {fmt}
  ---

  {{< include {include_file} >}}
  ",
  fmt          = "apaquarto-html",
  include_file = "Draft-Test.qmd",
  .trim = FALSE
) |>
  readr::write_lines(here::here("index.qmd"))

# Erzeugen einer.gitlab-ci.yml-Datei
glue::glue(
  "
  pages:
    stage: deploy
    script:
      - echo 'Nothing to do...'
    artifacts:
      paths:
        - public
    only:
      - main
  ",
  .trim = FALSE
) |>
  readr::write_lines(here::here('.gitlab-ci.yml'))

# Erzeugen einer .gitignore-Datei
glue::glue(
  "
  .Rproj.user
  .Rhistory
  .RData
  .Ruserdata
  ",
  .trim = FALSE
) |>
  readr::write_lines(here::here('.gitignore'))


# -------------------------------------------------------------------
# 5) Push
# -------------------------------------------------------------------
gert::git_push(set_upstream = TRUE)