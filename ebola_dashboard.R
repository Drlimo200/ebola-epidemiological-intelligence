# ============================================================================
#  EBOLA OUTBREAK EPIDEMIOLOGICAL INTELLIGENCE DASHBOARD
#  West Africa 2014–2016 | WHO Surveillance Data
# ============================================================================
#  © 2024 Spp Data Wizards & Healthcare Intelligence. All rights reserved.
#  Unauthorised reproduction or redistribution of this work is prohibited.
# ============================================================================
#
#  REQUIRED PACKAGES — install once before running:
#  install.packages(c("shiny","shinydashboard","shinydashboardPlus",
#                     "shinyWidgets","ggplot2","plotly","dplyr","tidyr",
#                     "lubridate","scales","DT","viridis","RColorBrewer",
#                     "ggthemes","patchwork","zoo","reshape2"))
# ============================================================================

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(lubridate)
library(scales)
library(DT)
library(viridis)
library(RColorBrewer)
library(zoo)
library(reshape2)

# ── 0.  DATA LOADING & PREPROCESSING ─────────────────────────────────────────

raw <- read.csv("ebola_data_db_format.csv", stringsAsFactors = FALSE)
raw$Date  <- as.Date(raw$Date)
raw$value <- as.numeric(raw$value)

# Core epidemic countries (exclude imported / secondary outbreak suffixes)
CORE_COUNTRIES <- c("Guinea", "Liberia", "Sierra Leone")
ALL_COUNTRIES  <- sort(unique(raw$Country))

# Palette — consistent across the dashboard
COUNTRY_COLS <- c(
  "Guinea"       = "#E63946",
  "Liberia"      = "#457B9D",
  "Sierra Leone" = "#F4A261",
  "Nigeria"      = "#2A9D8F",
  "Mali"         = "#9B59B6",
  "Senegal"      = "#F1C40F",
  "Spain"        = "#E67E22",
  "Italy"        = "#1ABC9C",
  "United Kingdom" = "#3498DB",
  "United States of America" = "#E74C3C",
  "Liberia 2"    = "#95A5A6",
  "Guinea 2"     = "#BDC3C7"
)

# Indicator short-names
INDICATORS <- list(
  total_cases   = "Cumulative number of confirmed, probable and suspected Ebola cases",
  conf_cases    = "Cumulative number of confirmed Ebola cases",
  prob_cases    = "Cumulative number of probable Ebola cases",
  susp_cases    = "Cumulative number of suspected Ebola cases",
  total_deaths  = "Cumulative number of confirmed, probable and suspected Ebola deaths",
  conf_deaths   = "Cumulative number of confirmed Ebola deaths",
  prob_deaths   = "Cumulative number of probable Ebola deaths",
  cfr_total     = "Case fatality rate (CFR) of confirmed, probable and suspected Ebola cases",
  cfr_conf      = "Case fatality rate (CFR) of confirmed Ebola cases",
  cases_21d     = "Number of confirmed, probable and suspected Ebola cases in the last 21 days",
  cases_7d      = "Number of confirmed, probable and suspected Ebola cases in the last 7 days",
  deaths_21d    = "Number of confirmed, probable and suspected Ebola deaths in the last 21 days"
)

get_ind <- function(ind_key, countries = ALL_COUNTRIES) {
  raw %>%
    filter(Indicator == INDICATORS[[ind_key]], Country %in% countries) %>%
    arrange(Country, Date)
}

# Latest values per country
latest <- function(ind_key) {
  get_ind(ind_key, ALL_COUNTRIES) %>%
    group_by(Country) %>%
    slice_max(Date, n = 1, with_ties = FALSE) %>%
    ungroup()
}

# Rolling 7-day incidence (new cases = diff of cumulative)
new_cases_df <- raw %>%
  filter(Indicator == INDICATORS[["total_cases"]], Country %in% CORE_COUNTRIES) %>%
  arrange(Country, Date) %>%
  group_by(Country) %>%
  mutate(
    new_cases  = pmax(0, value - lag(value, default = 0)),
    roll7_cases = rollmean(new_cases, k = 7, fill = NA, align = "right")
  ) %>%
  ungroup()

new_deaths_df <- raw %>%
  filter(Indicator == INDICATORS[["total_deaths"]], Country %in% CORE_COUNTRIES) %>%
  arrange(Country, Date) %>%
  group_by(Country) %>%
  mutate(
    new_deaths = pmax(0, value - lag(value, default = 0)),
    roll7_deaths = rollmean(new_deaths, k = 7, fill = NA, align = "right")
  ) %>%
  ungroup()

# ── SUMMARY STATS (for KPI boxes) ────────────────────────────────────────────
total_cases_val  <- sum(latest("total_cases")$value,  na.rm = TRUE)
total_deaths_val <- sum(latest("total_deaths")$value, na.rm = TRUE)
overall_cfr      <- round(total_deaths_val / total_cases_val * 100, 1)
peak_date        <- new_cases_df %>%
  filter(Country %in% CORE_COUNTRIES) %>%
  group_by(Date) %>%
  summarise(total_new = sum(new_cases, na.rm = TRUE)) %>%
  slice_max(total_new, n = 1)

# ── THEME ─────────────────────────────────────────────────────────────────────
epi_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.background   = element_rect(fill = "#0d1117", color = NA),
    panel.background  = element_rect(fill = "#161b22", color = NA),
    panel.grid.major  = element_line(color = "#21262d"),
    panel.grid.minor  = element_blank(),
    plot.title        = element_text(color = "#e6edf3", size = 14, face = "bold"),
    plot.subtitle     = element_text(color = "#8b949e", size = 11),
    plot.caption      = element_text(color = "#484f58", size = 8),
    axis.text         = element_text(color = "#8b949e"),
    axis.title        = element_text(color = "#c9d1d9"),
    legend.background = element_rect(fill = "#161b22", color = NA),
    legend.text       = element_text(color = "#c9d1d9"),
    legend.title      = element_text(color = "#e6edf3"),
    strip.text        = element_text(color = "#e6edf3", face = "bold")
  )

# ── UI ─────────────────────────────────────────────────────────────────────────

header <- dashboardHeader(
  title = tags$span(
    tags$img(src = "https://img.icons8.com/color/48/virus.png",
             height = "28px", style = "margin-right:8px; vertical-align:middle;"),
    tags$b("Ebola Epidemiological Intelligence", style = "font-size:16px;")
  ),
  titleWidth = 380
)

sidebar <- dashboardSidebar(
  width = 240,
  sidebarMenu(
    menuItem("Overview",          tabName = "overview",   icon = icon("globe-africa")),
    menuItem("Epidemic Curves",   tabName = "epicurves",  icon = icon("chart-line")),
    menuItem("Mortality Analysis",tabName = "mortality",  icon = icon("heartbeat")),
    menuItem("Incidence Dynamics",tabName = "incidence",  icon = icon("chart-bar")),
    menuItem("Country Deep-Dive", tabName = "country",    icon = icon("search")),
    menuItem("Data Explorer",     tabName = "datatab",    icon = icon("table")),

    hr(),
    tags$div(style = "padding:10px 15px; color:#8b949e; font-size:11px;",
      tags$b("Filters", style = "color:#c9d1d9;"),
      pickerInput(
        "sel_countries", "Countries",
        choices  = ALL_COUNTRIES,
        selected = CORE_COUNTRIES,
        multiple = TRUE,
        options  = list(`actions-box` = TRUE, `live-search` = TRUE,
                        style = "btn-default btn-sm")
      ),
      dateRangeInput(
        "date_range", "Date Range",
        start = min(raw$Date), end = max(raw$Date),
        min   = min(raw$Date), max = max(raw$Date),
        format = "yyyy-mm-dd"
      )
    )
  )
)

body <- dashboardBody(

  # ── CSS ──────────────────────────────────────────────────────────────────
  tags$head(tags$style(HTML("
    body, .wrapper { background-color:#0d1117; }
    .content-wrapper { background-color:#0d1117; }
    .main-header .logo, .main-header .navbar { background-color:#161b22 !important; border-bottom:1px solid #30363d; }
    .main-sidebar { background-color:#161b22 !important; }
    .sidebar-menu > li > a { color:#8b949e !important; }
    .sidebar-menu > li.active > a, .sidebar-menu > li > a:hover { color:#e6edf3 !important; background:#21262d !important; }
    .box { background-color:#161b22; border:1px solid #30363d; border-top:3px solid #E63946; }
    .box-title { color:#e6edf3; font-size:13px; font-weight:600; }
    .info-box { background:#161b22; border:1px solid #30363d; min-height:70px; }
    .info-box-icon { width:65px; line-height:70px; font-size:30px; }
    .info-box-content { padding:5px 10px; }
    .info-box-text { color:#8b949e; font-size:11px; text-transform:uppercase; letter-spacing:1px; }
    .info-box-number { color:#e6edf3; font-size:22px; font-weight:700; }
    .small-box { background-color:#161b22 !important; }
    .nav-tabs-custom > .nav-tabs > li.active { border-top-color:#E63946; }
    .nav-tabs-custom > .nav-tabs > li > a { color:#8b949e; }
    .nav-tabs-custom > .tab-content { background:#161b22; }
    .dataTables_wrapper { color:#c9d1d9 !important; }
    table.dataTable thead th { color:#e6edf3 !important; background:#21262d !important; }
    table.dataTable tbody tr { background:#161b22 !important; color:#c9d1d9 !important; }
    table.dataTable tbody tr:hover { background:#21262d !important; }
    .dataTables_filter input, .dataTables_length select { background:#21262d; color:#c9d1d9; border:1px solid #30363d; border-radius:4px; }
    .copyright-bar { background:#161b22; border-top:1px solid #30363d; padding:8px 20px;
                     text-align:center; color:#484f58; font-size:11px; margin-top:15px; }
    h4.section-heading { color:#e6edf3; font-size:13px; font-weight:600; border-left:3px solid #E63946;
                         padding-left:10px; margin:10px 0 5px; }
    .kpi-badge { display:inline-block; padding:2px 8px; border-radius:3px;
                 font-size:10px; font-weight:700; letter-spacing:1px; margin-left:8px; }
    .badge-red { background:#E63946; color:white; }
    .badge-yellow { background:#F4A261; color:#0d1117; }
  "))),

  tabItems(

    # ── TAB 1: OVERVIEW ───────────────────────────────────────────────────
    tabItem("overview",

      fluidRow(
        infoBox("Total Cases", format(round(total_cases_val), big.mark = ","),
                icon = icon("virus"), color = "red", fill = TRUE, width = 3),
        infoBox("Total Deaths", format(round(total_deaths_val), big.mark = ","),
                icon = icon("skull"), color = "maroon", fill = TRUE, width = 3),
        infoBox("Overall CFR", paste0(overall_cfr, "%"),
                icon = icon("percent"), color = "yellow", fill = TRUE, width = 3),
        infoBox("Outbreak Duration", "19 months",
                subtitle = "Aug 2014 – Mar 2016",
                icon = icon("calendar"), color = "blue", fill = TRUE, width = 3)
      ),

      fluidRow(
        box(title = "Cumulative Cases — Core Epidemic Countries", width = 8, solidHeader = TRUE, status = "danger",
            plotlyOutput("p_cumulative_cases", height = "320px")),
        box(title = "Case Classification Breakdown (All-Time Peak)", width = 4, solidHeader = TRUE, status = "warning",
            plotlyOutput("p_case_donut", height = "320px"))
      ),

      fluidRow(
        box(title = "Total Cases vs Deaths by Country", width = 6, solidHeader = TRUE, status = "danger",
            plotlyOutput("p_cases_deaths_bar", height = "280px")),
        box(title = "Case Fatality Rate (%) — Final Estimate", width = 6, solidHeader = TRUE, status = "warning",
            plotlyOutput("p_cfr_bar", height = "280px"))
      ),

      fluidRow(
        tags$div(class = "copyright-bar",
          HTML("&copy; 2024 <strong>Spp Data Wizards &amp; Healthcare Intelligence</strong> &mdash;
               All rights reserved. Data source: WHO Ebola Situation Reports 2014&ndash;2016.
               Unauthorised reproduction prohibited.")
        )
      )
    ),

    # ── TAB 2: EPIDEMIC CURVES ────────────────────────────────────────────
    tabItem("epicurves",

      fluidRow(
        box(title = "Epidemic Curve — 7-Day Rolling New Cases (Core Countries)", width = 12,
            solidHeader = TRUE, status = "danger",
            plotlyOutput("p_epicurve_rolling", height = "350px"))
      ),

      fluidRow(
        box(title = "Cumulative Cases (Log Scale) — All Selected Countries", width = 6,
            solidHeader = TRUE, status = "warning",
            plotlyOutput("p_log_cumulative", height = "320px")),
        box(title = "Active Transmission — Cases in Last 21 Days", width = 6,
            solidHeader = TRUE, status = "primary",
            plotlyOutput("p_active_21d", height = "320px"))
      ),

      fluidRow(
        box(title = "Weekly New Cases Heatmap by Country", width = 12,
            solidHeader = TRUE, status = "danger",
            plotlyOutput("p_heatmap", height = "300px"))
      ),

      fluidRow(
        tags$div(class = "copyright-bar",
          HTML("&copy; 2024 <strong>Spp Data Wizards &amp; Healthcare Intelligence</strong>")
        )
      )
    ),

    # ── TAB 3: MORTALITY ─────────────────────────────────────────────────
    tabItem("mortality",

      fluidRow(
        box(title = "Cumulative Deaths Over Time", width = 8, solidHeader = TRUE, status = "danger",
            plotlyOutput("p_deaths_time", height = "320px")),
        box(title = "Death Classification Mix (Core Countries)", width = 4, solidHeader = TRUE, status = "warning",
            plotlyOutput("p_death_class_bar", height = "320px"))
      ),

      fluidRow(
        box(title = "Case Fatality Rate Trajectory — Confirmed Cases", width = 12,
            solidHeader = TRUE, status = "primary",
            plotlyOutput("p_cfr_time", height = "310px"))
      ),

      fluidRow(
        box(title = "Deaths in Last 21 Days — Temporal Pattern", width = 6,
            solidHeader = TRUE, status = "danger",
            plotlyOutput("p_deaths_21d", height = "280px")),
        box(title = "Survival Ratio (Cases − Deaths) by Country", width = 6,
            solidHeader = TRUE, status = "warning",
            plotlyOutput("p_survival_ratio", height = "280px"))
      ),

      fluidRow(
        tags$div(class = "copyright-bar",
          HTML("&copy; 2024 <strong>Spp Data Wizards &amp; Healthcare Intelligence</strong>")
        )
      )
    ),

    # ── TAB 4: INCIDENCE DYNAMICS ─────────────────────────────────────────
    tabItem("incidence",

      fluidRow(
        box(title = "7-Day Incidence by Case Type — Core Countries", width = 12,
            solidHeader = TRUE, status = "danger",
            plotlyOutput("p_incidence_type", height = "360px"))
      ),

      fluidRow(
        box(title = "Proportion of Cases from Last 7 Days (Transmission Velocity)", width = 6,
            solidHeader = TRUE, status = "warning",
            plotlyOutput("p_prop_7d", height = "310px")),
        box(title = "Proportion of Cases from Last 21 Days (Epidemic Momentum)", width = 6,
            solidHeader = TRUE, status = "primary",
            plotlyOutput("p_prop_21d", height = "310px"))
      ),

      fluidRow(
        tags$div(class = "copyright-bar",
          HTML("&copy; 2024 <strong>Spp Data Wizards &amp; Healthcare Intelligence</strong>")
        )
      )
    ),

    # ── TAB 5: COUNTRY DEEP-DIVE ──────────────────────────────────────────
    tabItem("country",

      fluidRow(
        column(3,
          selectInput("sel_single_country", "Select Country",
                      choices = CORE_COUNTRIES, selected = "Sierra Leone")
        )
      ),

      fluidRow(
        infoBoxOutput("kpi_peak_cases",  width = 3),
        infoBoxOutput("kpi_peak_deaths", width = 3),
        infoBoxOutput("kpi_cfr",         width = 3),
        infoBoxOutput("kpi_duration",    width = 3)
      ),

      fluidRow(
        box(title = "Confirmed vs Probable vs Suspected Cases", width = 6,
            solidHeader = TRUE, status = "danger",
            plotlyOutput("p_country_type_area", height = "310px")),
        box(title = "New Cases — Daily (7-Day Smooth)", width = 6,
            solidHeader = TRUE, status = "warning",
            plotlyOutput("p_country_new_cases", height = "310px"))
      ),

      fluidRow(
        box(title = "CFR Over Time", width = 4, solidHeader = TRUE, status = "primary",
            plotlyOutput("p_country_cfr", height = "260px")),
        box(title = "Cases vs Deaths Correlation", width = 4, solidHeader = TRUE, status = "danger",
            plotlyOutput("p_country_scatter", height = "260px")),
        box(title = "Epidemic Phase Indicators", width = 4, solidHeader = TRUE, status = "warning",
            plotlyOutput("p_country_phase", height = "260px"))
      ),

      fluidRow(
        tags$div(class = "copyright-bar",
          HTML("&copy; 2024 <strong>Spp Data Wizards &amp; Healthcare Intelligence</strong>")
        )
      )
    ),

    # ── TAB 6: DATA EXPLORER ─────────────────────────────────────────────
    tabItem("datatab",

      fluidRow(
        box(title = "Filtered Dataset", width = 12, solidHeader = TRUE, status = "primary",
            DTOutput("data_table"))
      ),

      fluidRow(
        tags$div(class = "copyright-bar",
          HTML("&copy; 2024 <strong>Spp Data Wizards &amp; Healthcare Intelligence</strong> &mdash;
               Ebola Outbreak Surveillance Data 2014–2016 (WHO).")
        )
      )
    )
  )
)

ui <- dashboardPage(header, sidebar, body, skin = "black")

# ── SERVER ─────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # Reactive filtered data
  filt <- reactive({
    raw %>%
      filter(Country %in% input$sel_countries,
             Date >= input$date_range[1],
             Date <= input$date_range[2])
  })

  # ── OVERVIEW ─────────────────────────────────────────────────────────

  output$p_cumulative_cases <- renderPlotly({
    d <- raw %>%
      filter(Indicator == INDICATORS$total_cases,
             Country %in% CORE_COUNTRIES,
             Date >= input$date_range[1], Date <= input$date_range[2])
    p <- ggplot(d, aes(Date, value, color = Country, group = Country)) +
      geom_line(linewidth = 1.1) +
      geom_area(aes(fill = Country), alpha = 0.08) +
      scale_y_continuous(labels = comma) +
      scale_color_manual(values = COUNTRY_COLS) +
      scale_fill_manual(values = COUNTRY_COLS) +
      labs(x = NULL, y = "Cumulative Cases", color = NULL, fill = NULL) +
      epi_theme
    ggplotly(p, tooltip = c("x","y","colour")) %>%
      layout(paper_bgcolor = "#161b22", plot_bgcolor = "#161b22",
             font = list(color = "#c9d1d9"),
             legend = list(bgcolor = "#161b22"))
  })

  output$p_case_donut <- renderPlotly({
    d <- raw %>%
      filter(Indicator %in% c(INDICATORS$conf_cases, INDICATORS$prob_cases, INDICATORS$susp_cases),
             Country %in% CORE_COUNTRIES) %>%
      group_by(Indicator, Country) %>% slice_max(Date, n=1) %>% ungroup() %>%
      group_by(Indicator) %>% summarise(total = sum(value, na.rm=TRUE)) %>%
      mutate(label = case_when(
        Indicator == INDICATORS$conf_cases ~ "Confirmed",
        Indicator == INDICATORS$prob_cases ~ "Probable",
        TRUE                              ~ "Suspected"
      ))
    plot_ly(d, labels = ~label, values = ~total, type = "pie",
            hole = 0.55,
            marker = list(colors = c("#E63946","#F4A261","#457B9D"),
                          line = list(color="#0d1117", width=2)),
            textinfo = "label+percent",
            textfont = list(color="#e6edf3", size=11)) %>%
      layout(paper_bgcolor="#161b22", plot_bgcolor="#161b22",
             font=list(color="#c9d1d9"),
             showlegend=FALSE,
             annotations=list(text="Cases", showarrow=FALSE,
                              font=list(size=14,color="#e6edf3")))
  })

  output$p_cases_deaths_bar <- renderPlotly({
    cases  <- latest("total_cases")  %>% rename(Cases = value)
    deaths <- latest("total_deaths") %>% rename(Deaths = value)
    d <- left_join(cases %>% select(Country, Cases),
                   deaths %>% select(Country, Deaths), by="Country") %>%
      pivot_longer(c(Cases, Deaths)) %>%
      mutate(Country = reorder(Country, value))
    p <- ggplot(d, aes(Country, value, fill = name)) +
      geom_col(position = "dodge", width = 0.65) +
      coord_flip() +
      scale_y_continuous(labels = comma) +
      scale_fill_manual(values = c(Cases="#457B9D", Deaths="#E63946")) +
      labs(x=NULL, y=NULL, fill=NULL) + epi_theme
    ggplotly(p, tooltip=c("x","y","fill")) %>%
      layout(paper_bgcolor="#161b22", plot_bgcolor="#161b22",
             font=list(color="#c9d1d9"))
  })

  output$p_cfr_bar <- renderPlotly({
    d <- latest("cfr_total") %>%
      filter(!is.na(value), value > 0) %>%
      mutate(Country = reorder(Country, value))
    p <- ggplot(d, aes(Country, value, fill = value)) +
      geom_col(width=0.6) +
      geom_text(aes(label=paste0(round(value,1),"%")), hjust=-0.1,
                color="#e6edf3", size=3.5) +
      coord_flip() +
      scale_fill_gradient(low="#F4A261", high="#E63946", guide="none") +
      scale_y_continuous(limits=c(0, max(d$value, na.rm=TRUE)*1.2)) +
      labs(x=NULL, y="CFR (%)") + epi_theme
    ggplotly(p, tooltip=c("x","y")) %>%
      layout(paper_bgcolor="#161b22", plot_bgcolor="#161b22",
             font=list(color="#c9d1d9"))
  })

  # ── EPIDEMIC CURVES ───────────────────────────────────────────────────

  output$p_epicurve_rolling <- renderPlotly({
    d <- new_cases_df %>%
      filter(Country %in% input$sel_countries,
             Date >= input$date_range[1], Date <= input$date_range[2])
    p <- ggplot(d, aes(Date, roll7_cases, color = Country, group = Country)) +
      geom_line(linewidth = 1) +
      geom_area(aes(fill=Country), alpha=0.07) +
      scale_color_manual(values=COUNTRY_COLS) +
      scale_fill_manual(values=COUNTRY_COLS) +
      scale_y_continuous(labels=comma) +
      labs(x=NULL, y="New Cases (7-day rolling avg)", color=NULL, fill=NULL) +
      epi_theme
    ggplotly(p, tooltip=c("x","y","colour")) %>%
      layout(paper_bgcolor="#161b22", plot_bgcolor="#161b22",
             font=list(color="#c9d1d9"))
  })

  output$p_log_cumulative <- renderPlotly({
    d <- filt() %>%
      filter(Indicator==INDICATORS$total_cases, value>0)
    p <- ggplot(d, aes(Date, value, color=Country, group=Country)) +
      geom_line(linewidth=1) +
      scale_y_log10(labels=comma, breaks=c(1,10,100,1000,10000)) +
      scale_color_manual(values=COUNTRY_COLS) +
      labs(x=NULL, y="Cumulative Cases (log₁₀)", color=NULL) +
      epi_theme
    ggplotly(p, tooltip=c("x","y","colour")) %>%
      layout(paper_bgcolor="#161b22", plot_bgcolor="#161b22",
             font=list(color="#c9d1d9"))
  })

  output$p_active_21d <- renderPlotly({
    d <- filt() %>%
      filter(Indicator==INDICATORS$cases_21d)
    p <- ggplot(d, aes(Date, value, color=Country, group=Country)) +
      geom_line(linewidth=1) +
      geom_hline(yintercept=0, color="#E63946", linetype="dashed", linewidth=0.5) +
      scale_color_manual(values=COUNTRY_COLS) +
      scale_y_continuous(labels=comma) +
      labs(x=NULL, y="Cases in Last 21 Days", color=NULL) +
      epi_theme
    ggplotly(p, tooltip=c("x","y","colour")) %>%
      layout(paper_bgcolor="#161b22", plot_bgcolor="#161b22",
             font=list(color="#c9d1d9"))
  })

  output$p_heatmap <- renderPlotly({
    d <- new_cases_df %>%
      filter(Country %in% input$sel_countries,
             Date >= input$date_range[1], Date <= input$date_range[2]) %>%
      mutate(week = floor_date(Date, "week")) %>%
      group_by(Country, week) %>%
      summarise(new_cases=sum(new_cases,na.rm=TRUE), .groups="drop")
    p <- ggplot(d, aes(week, Country, fill=new_cases)) +
      geom_tile(color="#0d1117", linewidth=0.3) +
      scale_fill_gradientn(colors=c("#161b22","#F4A261","#E63946"),
                           labels=comma, name="New Cases") +
      labs(x=NULL, y=NULL) + epi_theme +
      theme(axis.text.x=element_text(angle=45,hjust=1))
    ggplotly(p, tooltip=c("x","y","fill")) %>%
      layout(paper_bgcolor="#161b22", plot_bgcolor="#161b22",
             font=list(color="#c9d1d9"))
  })

  # ── MORTALITY ─────────────────────────────────────────────────────────

  output$p_deaths_time <- renderPlotly({
    d <- filt() %>% filter(Indicator==INDICATORS$total_deaths)
    p <- ggplot(d, aes(Date, value, color=Country, group=Country)) +
      geom_line(linewidth=1.1) +
      scale_color_manual(values=COUNTRY_COLS) +
      scale_y_continuous(labels=comma) +
      labs(x=NULL, y="Cumulative Deaths", color=NULL) + epi_theme
    ggplotly(p, tooltip=c("x","y","colour")) %>%
      layout(paper_bgcolor="#161b22", plot_bgcolor="#161b22",
             font=list(color="#c9d1d9"))
  })

  output$p_death_class_bar <- renderPlotly({
    d <- raw %>%
      filter(Indicator %in% c(INDICATORS$conf_deaths,
                              INDICATORS$prob_deaths),
             Country %in% CORE_COUNTRIES) %>%
      group_by(Indicator, Country) %>% slice_max(Date, n=1) %>% ungroup() %>%
      mutate(label=ifelse(Indicator==INDICATORS$conf_deaths,"Confirmed","Probable"))
    p <- ggplot(d, aes(Country, value, fill=label)) +
      geom_col(position="stack", width=0.6) +
      scale_fill_manual(values=c(Confirmed="#E63946", Probable="#F4A261")) +
      scale_y_continuous(labels=comma) +
      labs(x=NULL, y="Deaths", fill=NULL) + epi_theme
    ggplotly(p, tooltip=c("x","y","fill")) %>%
      layout(paper_bgcolor="#161b22", plot_bgcolor="#161b22",
             font=list(color="#c9d1d9"))
  })

  output$p_cfr_time <- renderPlotly({
    d <- filt() %>% filter(Indicator==INDICATORS$cfr_conf)
    p <- ggplot(d, aes(Date, value, color=Country, group=Country)) +
      geom_line(linewidth=1) +
      geom_smooth(se=FALSE, linewidth=0.5, linetype="dashed", alpha=0.6) +
      scale_color_manual(values=COUNTRY_COLS) +
      scale_y_continuous(labels=function(x) paste0(x,"%")) +
      labs(x=NULL, y="Confirmed CFR (%)", color=NULL) + epi_theme
    ggplotly(p, tooltip=c("x","y","colour")) %>%
      layout(paper_bgcolor="#161b22", plot_bgcolor="#161b22",
             font=list(color="#c9d1d9"))
  })

  output$p_deaths_21d <- renderPlotly({
    d <- filt() %>% filter(Indicator==INDICATORS$deaths_21d)
    p <- ggplot(d, aes(Date, value, color=Country, group=Country)) +
      geom_line(linewidth=0.9) +
      scale_color_manual(values=COUNTRY_COLS) +
      scale_y_continuous(labels=comma) +
      labs(x=NULL, y="Deaths in Last 21 Days", color=NULL) + epi_theme
    ggplotly(p, tooltip=c("x","y","colour")) %>%
      layout(paper_bgcolor="#161b22", plot_bgcolor="#161b22",
             font=list(color="#c9d1d9"))
  })

  output$p_survival_ratio <- renderPlotly({
    cases  <- latest("total_cases")  %>% rename(Cases=value)
    deaths <- latest("total_deaths") %>% rename(Deaths=value)
    d <- inner_join(cases%>%select(Country,Cases),
                    deaths%>%select(Country,Deaths), by="Country") %>%
      mutate(Survivors=Cases-Deaths,
             Country=reorder(Country, Cases)) %>%
      pivot_longer(c(Survivors,Deaths))
    p <- ggplot(d, aes(Country, value, fill=name)) +
      geom_col(position="fill", width=0.6) +
      coord_flip() +
      scale_fill_manual(values=c(Survivors="#2A9D8F", Deaths="#E63946")) +
      scale_y_continuous(labels=percent) +
      labs(x=NULL, y="Proportion", fill=NULL) + epi_theme
    ggplotly(p, tooltip=c("x","y","fill")) %>%
      layout(paper_bgcolor="#161b22", plot_bgcolor="#161b22",
             font=list(color="#c9d1d9"))
  })

  # ── INCIDENCE DYNAMICS ─────────────────────────────────────────────────

  output$p_incidence_type <- renderPlotly({
    inds <- c(
      "Confirmed" = "Number of confirmed Ebola cases in the last 7 days",
      "Probable"  = "Number of probable Ebola cases in the last 7 days",
      "Suspected" = "Number of suspected Ebola cases in the last 7 days"
    )
    d <- raw %>%
      filter(Indicator %in% inds,
             Country %in% intersect(input$sel_countries, CORE_COUNTRIES),
             Date >= input$date_range[1], Date <= input$date_range[2]) %>%
      mutate(Type = names(inds)[match(Indicator, inds)])
    p <- ggplot(d, aes(Date, value, fill=Type)) +
      geom_area(alpha=0.75, position="stack") +
      facet_wrap(~Country, scales="free_y") +
      scale_fill_manual(values=c(Confirmed="#E63946",Probable="#F4A261",Suspected="#457B9D")) +
      scale_y_continuous(labels=comma) +
      labs(x=NULL, y="Cases in Last 7 Days", fill="Classification") +
      epi_theme
    ggplotly(p, tooltip=c("x","y","fill")) %>%
      layout(paper_bgcolor="#161b22", plot_bgcolor="#161b22",
             font=list(color="#c9d1d9"))
  })

  output$p_prop_7d <- renderPlotly({
    d <- filt() %>%
      filter(Indicator=="Proportion of confirmed, probable and suspected Ebola cases that are from the last 7 days")
    p <- ggplot(d, aes(Date, value, color=Country, group=Country)) +
      geom_line(linewidth=0.9) +
      geom_hline(yintercept=5, color="#E63946", linetype="dashed", linewidth=0.5) +
      scale_color_manual(values=COUNTRY_COLS) +
      scale_y_continuous(labels=function(x) paste0(x,"%")) +
      labs(x=NULL, y="% Cases from Last 7 Days", color=NULL,
           caption="Dashed line = 5% threshold") + epi_theme
    ggplotly(p, tooltip=c("x","y","colour")) %>%
      layout(paper_bgcolor="#161b22", plot_bgcolor="#161b22",
             font=list(color="#c9d1d9"))
  })

  output$p_prop_21d <- renderPlotly({
    d <- filt() %>%
      filter(Indicator=="Proportion of confirmed, probable and suspected Ebola cases that are from the last 21 days")
    p <- ggplot(d, aes(Date, value, color=Country, group=Country)) +
      geom_line(linewidth=0.9) +
      scale_color_manual(values=COUNTRY_COLS) +
      scale_y_continuous(labels=function(x) paste0(x,"%")) +
      labs(x=NULL, y="% Cases from Last 21 Days", color=NULL) + epi_theme
    ggplotly(p, tooltip=c("x","y","colour")) %>%
      layout(paper_bgcolor="#161b22", plot_bgcolor="#161b22",
             font=list(color="#c9d1d9"))
  })

  # ── COUNTRY DEEP-DIVE ─────────────────────────────────────────────────

  ctry <- reactive(input$sel_single_country)

  output$kpi_peak_cases <- renderInfoBox({
    v <- raw %>% filter(Indicator==INDICATORS$total_cases, Country==ctry()) %>%
      pull(value) %>% max(na.rm=TRUE)
    infoBox("Peak Cumulative Cases", format(round(v), big.mark=","),
            icon=icon("virus"), color="red", fill=TRUE)
  })
  output$kpi_peak_deaths <- renderInfoBox({
    v <- raw %>% filter(Indicator==INDICATORS$total_deaths, Country==ctry()) %>%
      pull(value) %>% max(na.rm=TRUE)
    infoBox("Peak Cumulative Deaths", format(round(v), big.mark=","),
            icon=icon("skull"), color="maroon", fill=TRUE)
  })
  output$kpi_cfr <- renderInfoBox({
    v <- raw %>% filter(Indicator==INDICATORS$cfr_total, Country==ctry()) %>%
      slice_max(Date, n=1) %>% pull(value)
    infoBox("Final CFR", paste0(round(v,1),"%"),
            icon=icon("percent"), color="yellow", fill=TRUE)
  })
  output$kpi_duration <- renderInfoBox({
    d <- raw %>% filter(Indicator==INDICATORS$total_cases, Country==ctry(), value>0) %>%
      summarise(start=min(Date), end=max(Date))
    dur <- as.numeric(d$end - d$start)
    infoBox("Outbreak Span", paste0(round(dur/30.4)," months"),
            subtitle=paste(format(d$start,"%b %Y"), "–", format(d$end,"%b %Y")),
            icon=icon("calendar"), color="blue", fill=TRUE)
  })

  output$p_country_type_area <- renderPlotly({
    d <- raw %>%
      filter(Indicator %in% c(INDICATORS$conf_cases,
                              INDICATORS$prob_cases,
                              INDICATORS$susp_cases),
             Country==ctry()) %>%
      mutate(Type=case_when(
        Indicator==INDICATORS$conf_cases ~ "Confirmed",
        Indicator==INDICATORS$prob_cases ~ "Probable",
        TRUE                             ~ "Suspected"
      ))
    p <- ggplot(d, aes(Date, value, fill=Type)) +
      geom_area(alpha=0.8, position="stack") +
      scale_fill_manual(values=c(Confirmed="#E63946",Probable="#F4A261",Suspected="#457B9D")) +
      scale_y_continuous(labels=comma) +
      labs(x=NULL, y="Cumulative Cases", fill=NULL) + epi_theme
    ggplotly(p, tooltip=c("x","y","fill")) %>%
      layout(paper_bgcolor="#161b22", plot_bgcolor="#161b22",
             font=list(color="#c9d1d9"))
  })

  output$p_country_new_cases <- renderPlotly({
    d <- new_cases_df %>% filter(Country==ctry())
    p <- ggplot(d, aes(Date)) +
      geom_col(aes(y=new_cases), fill="#30363d", width=1) +
      geom_line(aes(y=roll7_cases), color="#E63946", linewidth=1.1) +
      scale_y_continuous(labels=comma) +
      labs(x=NULL, y="New Cases", caption="Red line = 7-day rolling average") +
      epi_theme
    ggplotly(p, tooltip=c("x","y")) %>%
      layout(paper_bgcolor="#161b22", plot_bgcolor="#161b22",
             font=list(color="#c9d1d9"))
  })

  output$p_country_cfr <- renderPlotly({
    d <- raw %>% filter(Indicator==INDICATORS$cfr_total, Country==ctry())
    p <- ggplot(d, aes(Date, value)) +
      geom_line(color="#F4A261", linewidth=1.1) +
      geom_area(fill="#F4A261", alpha=0.15) +
      scale_y_continuous(labels=function(x) paste0(x,"%")) +
      labs(x=NULL, y="CFR (%)") + epi_theme
    ggplotly(p, tooltip=c("x","y")) %>%
      layout(paper_bgcolor="#161b22", plot_bgcolor="#161b22",
             font=list(color="#c9d1d9"))
  })

  output$p_country_scatter <- renderPlotly({
    tc <- raw %>% filter(Indicator==INDICATORS$total_cases,  Country==ctry()) %>%
      select(Date, cases=value)
    td <- raw %>% filter(Indicator==INDICATORS$total_deaths, Country==ctry()) %>%
      select(Date, deaths=value)
    d <- inner_join(tc, td, by="Date") %>% na.omit()
    p <- ggplot(d, aes(cases, deaths, color=as.numeric(Date))) +
      geom_point(size=1.5, alpha=0.7) +
      geom_smooth(method="lm", se=FALSE, color="#E63946", linewidth=0.8) +
      scale_color_viridis_c(option="plasma", labels=function(x) as.Date(x, origin="1970-01-01"),
                            name="Date") +
      scale_x_continuous(labels=comma) +
      scale_y_continuous(labels=comma) +
      labs(x="Cumulative Cases", y="Cumulative Deaths") + epi_theme +
      theme(legend.position="none")
    ggplotly(p, tooltip=c("x","y")) %>%
      layout(paper_bgcolor="#161b22", plot_bgcolor="#161b22",
             font=list(color="#c9d1d9"))
  })

  output$p_country_phase <- renderPlotly({
    d <- raw %>%
      filter(Indicator %in% c(INDICATORS$cases_7d, INDICATORS$cases_21d),
             Country==ctry()) %>%
      mutate(label=ifelse(Indicator==INDICATORS$cases_7d,"7-Day","21-Day"))
    p <- ggplot(d, aes(Date, value, color=label)) +
      geom_line(linewidth=1) +
      scale_color_manual(values=c("7-Day"="#E63946","21-Day"="#457B9D")) +
      scale_y_continuous(labels=comma) +
      labs(x=NULL, y="New Cases", color="Window") + epi_theme
    ggplotly(p, tooltip=c("x","y","colour")) %>%
      layout(paper_bgcolor="#161b22", plot_bgcolor="#161b22",
             font=list(color="#c9d1d9"))
  })

  # ── DATA EXPLORER ─────────────────────────────────────────────────────

  output$data_table <- renderDT({
    filt() %>%
      select(Country, Date, Indicator, value) %>%
      arrange(desc(Date)) %>%
      datatable(
        extensions = c("Buttons","Scroller"),
        options = list(
          dom       = "Bfrtip",
          buttons   = c("copy","csv","excel"),
          scrollY   = 450,
          scroller  = TRUE,
          pageLength= 50,
          columnDefs = list(list(className="dt-center", targets="_all"))
        ),
        rownames = FALSE,
        class    = "compact hover"
      ) %>%
      formatRound("value", digits=1) %>%
      formatStyle("value",
        background = styleColorBar(filt()$value, "#E63946"),
        backgroundSize = "100% 88%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      )
  })
}

# ── RUN ───────────────────────────────────────────────────────────────────────

shinyApp(ui = ui, server = server)

# ============================================================================
#  © 2024 Spp Data Wizards & Healthcare Intelligence. All rights reserved.
# ============================================================================
