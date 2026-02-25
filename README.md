# League App

Shiny app scaffold for a shared league experience with 4 suites:

- Leaderboard (default)
- Comparison Tool
- League Data
- Heatmaps

## Data Rules Implemented

- Neon-backed loading via `pitch_data_service.R`
- Uses only v3 data
- Excludes filenames containing:
  - `unverified`
  - `playerpositioning`
  - `battracking`
- Date filter: `2026-02-13` and newer

## Files

- `app.R` main app
- `pitch_data_service.R` Neon data backend service
- `db/pitch_data_schema.sql` schema/indexes
- `scripts/sync_league_data_to_neon.R` FTP -> Neon sync

## Sync

```bash
Rscript scripts/sync_league_data_to_neon.R
```

## Required Environment

- `PITCH_DATA_BACKEND=postgres`
- `PITCH_DATA_DB_URL=postgresql://...`
- Optional explicit targeting:
  - `PITCH_DATA_DB_SCHEMA=public`
  - `PITCH_DATA_DB_TABLE=pitch_events`

## Deploy

```bash
Rscript install_packages.R
Rscript deploy_script.R
```
