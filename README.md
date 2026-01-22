# Facebook Follower Scraper

Automatischer Scraper für Facebook-Seiten-Likes, der wöchentlich via GitHub Actions läuft.

## Setup

### 1. Repository-Struktur

```
.
├── .github/
│   └── workflows/
│       └── scraper.yml
├── data/
│   └── seiten_liste.csv
├── results/
│   └── (generierte CSV-Dateien)
├── facebook_scraper.R
└── README.md
```

### 2. Seiten-IDs hinzufügen

Bearbeite `data/seiten_liste.csv` und füge deine Facebook-Seiten hinzu:

```csv
Seitenname,Target.ID
Meine Seite,123456789
Andere Seite,987654321
```

### 3. GitHub Actions aktivieren

1. Gehe zu deinem Repository auf GitHub
2. Klicke auf "Actions"
3. Aktiviere Workflows falls nötig
4. Der Scraper läuft automatisch jeden Montag um 8:00 UTC

### 4. Manuell ausführen

Du kannst den Scraper auch manuell starten:

1. Gehe zu "Actions" in deinem Repository
2. Wähle "Facebook Scraper Wöchentlich"
3. Klicke "Run workflow"

## Funktionsweise

- **Input**: `data/seiten_liste.csv` mit Spaltennamen `Seitenname` und `Target.ID`
- **Output**: Timestamped CSV-Dateien in `results/` mit Seitenname, Likes und Datum
- **Zeitplan**: Jeden Montag um 8:00 UTC (kann in `.github/workflows/scraper.yml` angepasst werden)

## Zeitplan anpassen

In `.github/workflows/scraper.yml` kannst du den Cron-Ausdruck ändern:

```yaml
schedule:
  - cron: '0 8 * * 1'  # Montag 8:00 UTC
```

Beispiele:
- `'0 8 * * 1'` - Jeden Montag um 8:00
- `'0 12 * * 5'` - Jeden Freitag um 12:00
- `'0 0 1 * *'` - Jeden 1. des Monats um Mitternacht

## Lokale Nutzung

```r
# R installieren und dann:
Rscript facebook_scraper.R
```

## Hinweise

- Stelle sicher, dass die `Target.ID`s korrekt sind
- Die Ergebnisse werden automatisch ins Repository committed
- Bei Fehlern kannst du die Logs unter "Actions" einsehen
