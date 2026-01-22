# Facebook Follower Scraper
# Liest Seiten-IDs aus CSV und scraped Likes

# Pakete laden (Installation erfolgt via GitHub Actions)
if (!requireNamespace("metatargetr", quietly = TRUE)) {
  stop("Pakete nicht installiert. Bitte zuerst installieren.")
}

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
    rowwise() %>%
    mutate(
      Likes = {
        cat("Frage Seite ab:", Target.ID, "\n")
        likes <- get_page_likes(Target.ID)
        Sys.sleep(1)
        as.numeric(likes)
      },
      Datum = as.character(Sys.Date())
    ) %>%
    ungroup()
  
  cat("\nAbfrage abgeschlossen!\n")
  return(ergebnisse)
}

# Liste aus CSV-Datei einlesen (von GitHub Repository)
csv_path <- "data/seiten_liste.csv"
meine_seiten <- read.csv(csv_path, stringsAsFactors = FALSE)

# Abrufen
ergebnis <- scrape_facebook_likes(meine_seiten)

# Kompakte Ansicht - sicherstellen dass alle Spalten atomare Werte haben
ergebnis_kompakt <- ergebnis %>%
  select(Seitenname, Likes, Datum) %>%
  mutate(
    Likes = as.numeric(Likes),
    Datum = as.character(Datum)
  )

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
