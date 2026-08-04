# Sturgeon Lock Use Analysis 

## Overview
R workflow for processing acoustic telemetry detections of sturgeon and related species to evaluate movement patterns in relation to Santee Cooper lock operations 

Data sources include Fathom detections, SCDNR tagging metadata, and lock operation logs.

<img width="1499" height="835" alt="image" src="https://github.com/user-attachments/assets/a9e012cc-ded6-46ae-bf06-08cd60835ddb" />

---

## Workflow
- Load and merge detection datasets (Fathom central + live + receiver downloads)
- Clean and standardize detection records
- Join tagging metadata and assign species IDs
- Map receiver locations to study sites
- Derive temporal variables (UTC → EST, season, date)
- Integrate lock operation timing data
- Perform QA/QC on tag metadata coverage
- Generate summary statistics and exploratory figures

---

## Outputs
- Cleaned detection dataset (`DF`)
- Combined detection table (`DF_join`)
- Tag metadata table (`Tags_clean`)
- Processed lock logs (`ll_final`)
- Figures saved to `Figures_Working/`

---

## Key Analyses
- Detection frequency by receiver, species, and season
- Internal vs external test tag comparisons
- Daily and hourly detection patterns
- Detection timing relative to lock gate status
- Individual fish detection histories

---

## Dependencies
- tidyverse
- lubridate
- ggplot2
- dplyr
- tidyr
- readr
- janitor
- hms
- glatos
- officer

---

## Notes
- File paths are hardcoded and must be updated for new environments
- Lock status is simplified to fixed open/closed windows (20:00–06:30)
- Detection filtering is minimal (no formal false detection screening)
- Workflow is exploratory and visualization-focused

---

## Contact
Rachel Roday 631-827-8007; rer1019@gmail.com
