# Analyse der wöchentlichen Veränderungen
library(tidyverse)
library(lubridate)

# Funktion zum Finden der beiden neuesten CSV-Dateien
find_latest_files <- function(results_dir = "results") {
  files <- list.files(results_dir, pattern = "facebook_likes_.*\\.csv$", full.names = TRUE)
  
  if (length(files) < 2) {
    stop("Nicht genügend Dateien für Vergleich vorhanden!")
  }
  
  # Extrahiere Timestamp aus Dateinamen und sortiere
  # Format: facebook_likes_YYYYMMDD_HHMMSS.csv
  file_data <- data.frame(
    path = files,
    filename = basename(files),
    stringsAsFactors = FALSE
  )
  
  # Timestamp aus Dateinamen extrahieren
  file_data$timestamp <- gsub("facebook_likes_(\\d+_\\d+)\\.csv", "\\1", file_data$filename)
  
  # Nach Timestamp sortieren (neueste zuerst)
  file_data <- file_data[order(file_data$timestamp, decreasing = TRUE), ]
  
  cat("Gefundene Dateien (sortiert nach Datum):\n")
  print(file_data[, c("filename", "timestamp")])
  cat("\n")
  
  return(list(
    aktuell = file_data$path[1],
    vorherig = file_data$path[2]
  ))
}

# Daten laden und vergleichen
analyse_veraenderungen <- function() {
  cat("Starte Analyse der Veränderungen...\n")
  
  # Neueste Dateien finden
  dateien <- find_latest_files()
  
  cat("Aktuelle Datei:", dateien$aktuell, "\n")
  cat("Vorherige Datei:", dateien$vorherig, "\n")
  
  # Daten laden
  aktuell <- read.csv(dateien$aktuell, stringsAsFactors = FALSE)
  vorherig <- read.csv(dateien$vorherig, stringsAsFactors = FALSE)
  
  # Vergleich durchführen
  vergleich <- aktuell %>%
    rename(Likes_aktuell = Likes, Datum_aktuell = Datum) %>%
    left_join(
      vorherig %>% rename(Likes_vorherig = Likes, Datum_vorherig = Datum),
      by = "Seitenname"
    ) %>%
    mutate(
      # Absolute Veränderung
      Absolute_Veraenderung = Likes_aktuell - Likes_vorherig,
      
      # Prozentuale Veränderung
      Prozentuale_Veraenderung = round(
        (Likes_aktuell - Likes_vorherig) / Likes_vorherig * 100, 
        2
      ),
      
      # Vorzeichen für bessere Lesbarkeit
      Veraenderung_Text = case_when(
        Absolute_Veraenderung > 0 ~ paste0("+", Absolute_Veraenderung),
        Absolute_Veraenderung < 0 ~ as.character(Absolute_Veraenderung),
        TRUE ~ "0"
      )
    ) %>%
    # NAs behandeln (falls neue Seiten hinzugekommen sind)
    mutate(
      Absolute_Veraenderung = ifelse(is.na(Absolute_Veraenderung), Likes_aktuell, Absolute_Veraenderung),
      Prozentuale_Veraenderung = ifelse(is.na(Prozentuale_Veraenderung), 100, Prozentuale_Veraenderung)
    )
  
  cat("\nVergleich abgeschlossen!\n")
  cat("Zeitraum:", vorherig$Datum[1], "bis", aktuell$Datum[1], "\n")
  
  return(vergleich)
}

# Top 10 Listen erstellen
erstelle_top_listen <- function(vergleich) {
  
  # Top 10 nach absoluter Veränderung
  top_absolut <- vergleich %>%
    arrange(desc(Absolute_Veraenderung)) %>%
    head(10) %>%
    select(Seitenname, Likes_aktuell, Absolute_Veraenderung, Prozentuale_Veraenderung)
  
  # Top 10 nach prozentualer Veränderung
  top_prozentual <- vergleich %>%
    arrange(desc(Prozentuale_Veraenderung)) %>%
    head(10) %>%
    select(Seitenname, Likes_aktuell, Absolute_Veraenderung, Prozentuale_Veraenderung)
  
  # Flop 10 (größte Verluste)
  flop_absolut <- vergleich %>%
    arrange(Absolute_Veraenderung) %>%
    head(10) %>%
    select(Seitenname, Likes_aktuell, Absolute_Veraenderung, Prozentuale_Veraenderung)
  
  return(list(
    top_absolut = top_absolut,
    top_prozentual = top_prozentual,
    flop_absolut = flop_absolut
  ))
}

# HTML-E-Mail erstellen
erstelle_email_html <- function(vergleich, top_listen, datum_alt, datum_neu) {
  
  # Gesamtstatistiken
  gesamt_likes_alt <- sum(vergleich$Likes_vorherig, na.rm = TRUE)
  gesamt_likes_neu <- sum(vergleich$Likes_aktuell, na.rm = TRUE)
  gesamt_veraenderung <- gesamt_likes_neu - gesamt_likes_alt
  gesamt_prozent <- round((gesamt_veraenderung / gesamt_likes_alt) * 100, 2)
  
  html <- paste0('
<!DOCTYPE html>
<html>
<head>
    <style>
        body { font-family: Arial, sans-serif; max-width: 800px; margin: 0 auto; padding: 20px; background-color: #f5f5f5; }
        .header { background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; padding: 30px; border-radius: 10px; text-align: center; margin-bottom: 30px; }
        .header h1 { margin: 0; font-size: 28px; }
        .header p { margin: 10px 0 0 0; opacity: 0.9; }
        .stats-box { background: white; padding: 20px; border-radius: 10px; margin-bottom: 20px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); }
        .stats-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 15px; margin-top: 15px; }
        .stat-card { background: #f8f9fa; padding: 15px; border-radius: 8px; text-align: center; }
        .stat-number { font-size: 24px; font-weight: bold; color: #667eea; }
        .stat-label { font-size: 12px; color: #6c757d; margin-top: 5px; }
        .section { background: white; padding: 20px; border-radius: 10px; margin-bottom: 20px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); }
        h2 { color: #333; margin-top: 0; border-bottom: 2px solid #667eea; padding-bottom: 10px; }
        table { width: 100%; border-collapse: collapse; margin-top: 15px; }
        th { background: #667eea; color: white; padding: 12px; text-align: left; font-size: 14px; }
        td { padding: 10px; border-bottom: 1px solid #ddd; font-size: 14px; }
        tr:hover { background: #f8f9fa; }
        .positive { color: #28a745; font-weight: bold; }
        .negative { color: #dc3545; font-weight: bold; }
        .footer { text-align: center; color: #6c757d; margin-top: 30px; font-size: 12px; }
    </style>
</head>
<body>
    <div class="header">
        <h1>📊 Wöchentlicher Facebook Likes Report</h1>
        <p>Zeitraum: ', datum_alt, ' bis ', datum_neu, '</p>
    </div>
    
    <div class="stats-box">
        <h2>📈 Gesamtstatistik</h2>
        <div class="stats-grid">
            <div class="stat-card">
                <div class="stat-number">', format(gesamt_likes_neu, big.mark = ".", decimal.mark = ","), '</div>
                <div class="stat-label">Gesamt Likes</div>
            </div>
            <div class="stat-card">
                <div class="stat-number ', ifelse(gesamt_veraenderung >= 0, 'positive', 'negative'), '">',
                    ifelse(gesamt_veraenderung >= 0, '+', ''), 
                    format(gesamt_veraenderung, big.mark = ".", decimal.mark = ","), '</div>
                <div class="stat-label">Absolute Veränderung</div>
            </div>
            <div class="stat-card">
                <div class="stat-number ', ifelse(gesamt_prozent >= 0, 'positive', 'negative'), '">',
                    ifelse(gesamt_prozent >= 0, '+', ''), gesamt_prozent, '%</div>
                <div class="stat-label">Prozentuale Veränderung</div>
            </div>
        </div>
    </div>
    
    <div class="section">
        <h2>🏆 Top 10 - Absolute Veränderung</h2>
        <table>
            <thead>
                <tr>
                    <th>#</th>
                    <th>Seitenname</th>
                    <th>Likes</th>
                    <th>Veränderung</th>
                    <th>%</th>
                </tr>
            </thead>
            <tbody>')
  
  # Top 10 Absolut einfügen
  for (i in 1:nrow(top_listen$top_absolut)) {
    row <- top_listen$top_absolut[i, ]
    html <- paste0(html, '
                <tr>
                    <td>', i, '</td>
                    <td><strong>', row$Seitenname, '</strong></td>
                    <td>', format(row$Likes_aktuell, big.mark = ".", decimal.mark = ","), '</td>
                    <td class="positive">+', format(row$Absolute_Veraenderung, big.mark = ".", decimal.mark = ","), '</td>
                    <td class="positive">+', row$Prozentuale_Veraenderung, '%</td>
                </tr>')
  }
  
  html <- paste0(html, '
            </tbody>
        </table>
    </div>
    
    <div class="section">
        <h2>📈 Top 10 - Prozentuale Veränderung</h2>
        <table>
            <thead>
                <tr>
                    <th>#</th>
                    <th>Seitenname</th>
                    <th>Likes</th>
                    <th>Veränderung</th>
                    <th>%</th>
                </tr>
            </thead>
            <tbody>')
  
  # Top 10 Prozentual einfügen
  for (i in 1:nrow(top_listen$top_prozentual)) {
    row <- top_listen$top_prozentual[i, ]
    html <- paste0(html, '
                <tr>
                    <td>', i, '</td>
                    <td><strong>', row$Seitenname, '</strong></td>
                    <td>', format(row$Likes_aktuell, big.mark = ".", decimal.mark = ","), '</td>
                    <td class="positive">+', format(row$Absolute_Veraenderung, big.mark = ".", decimal.mark = ","), '</td>
                    <td class="positive">+', row$Prozentuale_Veraenderung, '%</td>
                </tr>')
  }
  
  html <- paste0(html, '
            </tbody>
        </table>
    </div>
    
    <div class="section">
        <h2>📉 Flop 10 - Größte Verluste</h2>
        <table>
            <thead>
                <tr>
                    <th>#</th>
                    <th>Seitenname</th>
                    <th>Likes</th>
                    <th>Veränderung</th>
                    <th>%</th>
                </tr>
            </thead>
            <tbody>')
  
  # Flop 10 einfügen
  for (i in 1:nrow(top_listen$flop_absolut)) {
    row <- top_listen$flop_absolut[i, ]
    html <- paste0(html, '
                <tr>
                    <td>', i, '</td>
                    <td><strong>', row$Seitenname, '</strong></td>
                    <td>', format(row$Likes_aktuell, big.mark = ".", decimal.mark = ","), '</td>
                    <td class="negative">', format(row$Absolute_Veraenderung, big.mark = ".", decimal.mark = ","), '</td>
                    <td class="negative">', row$Prozentuale_Veraenderung, '%</td>
                </tr>')
  }
  
  html <- paste0(html, '
            </tbody>
        </table>
    </div>
    
    <div class="footer">
        <p>📊 Automatisch generiert von Facebook Scraper | Powered by GitHub Actions</p>
    </div>
</body>
</html>')
  
  return(html)
}

# Hauptausführung
cat("=== Starte wöchentliche Analyse ===\n\n")

vergleich <- analyse_veraenderungen()
top_listen <- erstelle_top_listen(vergleich)

# Daten für E-Mail
datum_alt <- unique(vergleich$Datum_vorherig)[1]
datum_neu <- unique(vergleich$Datum_aktuell)[1]

# HTML erstellen
email_html <- erstelle_email_html(vergleich, top_listen, datum_alt, datum_neu)

# HTML speichern
output_file <- "reports/weekly_report.html"
dir.create("reports", showWarnings = FALSE)
writeLines(email_html, output_file)

cat("\n✅ Report erstellt:", output_file, "\n")

# Vollständige Vergleichstabelle speichern
vergleich_file <- paste0("reports/vergleich_", format(Sys.Date(), "%Y%m%d"), ".csv")
write.csv(vergleich, vergleich_file, row.names = FALSE)

cat("✅ Vergleichsdaten gespeichert:", vergleich_file, "\n")
cat("\n=== Analyse abgeschlossen! ===\n")
