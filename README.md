# Ebola Epidemiological Intelligence Dashboard
## © 2024 Spp Data Wizards & Healthcare Intelligence

---

### Overview
A professional Shiny dashboard for analysing the 2014–2016 West Africa Ebola outbreak
using WHO surveillance data. The dashboard features six analytical modules across
**17,585 surveillance records** spanning 12 countries from August 2014 to March 2016.

---

### Quick Start

**Step 1 — Install R (≥ 4.2) and RStudio**  
https://posit.co/download/rstudio-desktop/

**Step 2 — Install required packages** (run once in R console):
```r
install.packages(c(
  "shiny", "shinydashboard", "shinyWidgets",
  "ggplot2", "plotly", "dplyr", "tidyr",
  "lubridate", "scales", "DT", "viridis",
  "RColorBrewer", "zoo", "reshape2"
))
```

**Step 3 — Place files in the same folder:**
```
your_folder/
├── ebola_dashboard.R
└── ebola_data_db_format.csv
```

**Step 4 — Launch the dashboard:**
```r
shiny::runApp("ebola_dashboard.R")
```

---

### Dashboard Modules

| Tab | Contents |
|-----|----------|
| **Overview** | KPI boxes, cumulative cases/deaths, CFR bar chart, case classification donut |
| **Epidemic Curves** | 7-day rolling incidence, log-scale growth, active 21-day cases, weekly heatmap |
| **Mortality Analysis** | Death trajectories, CFR over time, survival ratios, 21-day deaths |
| **Incidence Dynamics** | Case type stacked area, 7-day & 21-day transmission velocity |
| **Country Deep-Dive** | Per-country KPIs, epidemic phases, new-case bars, CFR trend, scatter |
| **Data Explorer** | Filtered data table with CSV/Excel export |

---

### Data Source
WHO Ebola Situation Reports — 2014–2016 West Africa Outbreak  
Countries: Guinea, Liberia, Sierra Leone (core) + Nigeria, Mali, Senegal, Spain, Italy, UK, USA

---

### Copyright
© 2024 **Spp Data Wizards & Healthcare Intelligence**  
All rights reserved. Unauthorised reproduction or redistribution of this work is prohibited.
