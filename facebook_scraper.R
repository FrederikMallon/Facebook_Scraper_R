# Facebook Follower Scraper
# Liest Seiten-IDs aus CSV und scraped Likes

# Pakete installieren falls nötig
if (!require("pak")) install.packages("pak")

pak::pak(c(
  "favstats/metatargetr",
  "tidyverse",
  "lubridate",
  "scales"
))

library(metatargetr)
library(tidyverse)
library(lubridate)
library(scales)

# Funktion zum Auslesen der Likes für eine einzelne Seite
get_page_likes <- function(page_id) {
  tryCatch({
    page_info <- get_page_insights(page_id, include_info = "page_info")
    likes <- as.numeric(page_info$likes)
    return(likes)
  }, error = function(e) {
    warning(paste("Fehler bei Seite", page_id, ":", e$message))
    return(NA)
  })
}

# Hauptfunktion: Liste von Seiten abfragen
scrape_facebook_likes <- function(seiten_liste) {
  if (!all(c("Seitenname", "Target.ID") %in% names(seiten_liste))) {
    stop("Die Liste muss die Spalten 'Seitenname' und 'Target.ID' enthalten")
  }
  
  cat("Starte Abfrage für", nrow(seiten_liste), "Seiten...\n")
  
  ergebnisse <- seiten_liste %>%
    mutate(
      Likes = sapply(Target.ID, function(id) {
        cat("Frage Seite ab:", id, "\n")
        likes <- get_page_likes(id)
        Sys.sleep(1)
        return(likes)
      }),
      Datum = Sys.Date()
    )
  
  cat("\nAbfrage abgeschlossen!\n")
  return(ergebnisse)
}

# Liste aus CSV-Datei einlesen (von GitHub Repository)
csv_path <- "data/seiten_liste.csv"
meine_seiten <- read.csv(csv_path, stringsAsFactors = FALSE)

# Abrufen
ergebnis <- scrape_facebook_likes(meine_seiten)

# Kompakte Ansicht
ergebnis_kompakt <- ergebnis %>%
  select(Seitenname, Likes, Datum)

print(ergebnis_kompakt)

# Ergebnisse speichern
output_dir <- "results"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
output_file <- file.path(output_dir, paste0("facebook_likes_", timestamp, ".csv"))
write.csv(ergebnis_kompakt, output_file, row.names = FALSE)

cat("\nErgebnisse gespeichert in:", output_file, "\n")
