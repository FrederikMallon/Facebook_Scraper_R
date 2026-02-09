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
  # Wenn page_id NA ist, direkt NA zurückgeben
  if (is.na(page_id)) {
    return(NA_real_)
  }
  
  tryCatch({
    page_info <- get_page_insights(page_id, include_info = "page_info")
    likes <- as.numeric(page_info$likes)
    
    # Falls likes leer oder NULL ist, NA zurückgeben
    if (length(likes) == 0 || is.null(likes)) {
      return(NA_real_)
    }
    
    return(likes)
  }, error = function(e) {
    warning(paste("Fehler bei Seite", page_id, ":", e$message))
    return(NA_real_)
  })
}

# Hauptfunktion: Liste von Seiten abfragen
scrape_facebook_likes <- function(seiten_liste) {
  if (!all(c("Seitenname", "Target.ID") %in% names(seiten_liste))) {
    cat("FEHLER: Erwartete Spalten nicht gefunden!\n")
    cat("Vorhandene Spalten:", paste(names(seiten_liste), collapse = ", "), "\n")
    cat("Erwartete Spalten: Seitenname, Target.ID\n")
    stop("Die Liste muss die Spalten 'Seitenname' und 'Target.ID' enthalten")
  }
  
  cat("Starte Abfrage für", nrow(seiten_liste), "Seiten...\n")
  
  ergebnisse <- seiten_liste %>%
    rowwise() %>%
    mutate(
      Likes = {
        # Target.ID als String behandeln
        id_string <- as.character(Target.ID)
        cat("Frage Seite ab:", id_string, "\n")
        
        likes_result <- get_page_likes(id_string)
        Sys.sleep(1)
        
        # Sicherstellen dass immer ein einzelner numerischer Wert zurückgegeben wird
        if (length(likes_result) == 0) NA_real_ else as.numeric(likes_result)[1]
      },
      Datum = as.character(Sys.Date())
    ) %>%
    ungroup()
  
  cat("\nAbfrage abgeschlossen!\n")
  return(ergebnisse)
}

# Liste aus CSV-Datei einlesen (von GitHub Repository)
csv_path <- "data/seiten_liste.csv"

cat("Lese CSV-Datei:", csv_path, "\n")

# CSV mit expliziten Einstellungen einlesen
meine_seiten <- read.csv(
  csv_path, 
  stringsAsFactors = FALSE,
  sep = ",",
  header = TRUE,
  check.names = FALSE,  # Verhindert dass R Spaltennamen ändert!
  colClasses = c("character", "character"),
  encoding = "UTF-8"
)

# Spaltennamen bereinigen (Leerzeichen entfernen)
names(meine_seiten) <- trimws(names(meine_seiten))

# Debug: Detaillierte Ausgabe
cat("\n=== CSV Debug Info ===\n")
cat("Gefundene Spalten:", paste(names(meine_seiten), collapse = " | "), "\n")
cat("Anzahl Spalten:", ncol(meine_seiten), "\n")
cat("Anzahl Zeilen:", nrow(meine_seiten), "\n")
cat("Erste 3 Zeilen:\n")
print(head(meine_seiten, 3))
cat("======================\n\n")

# Sicherstellen dass Target.ID als Character vorliegt (keine wissenschaftliche Notation)
meine_seiten <- meine_seiten %>%
  mutate(Target.ID = as.character(Target.ID))

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
