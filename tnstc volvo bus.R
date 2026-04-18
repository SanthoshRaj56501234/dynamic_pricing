# ============================================================
# TNSTC VOLVO BUS - DYNAMIC PRICING SHINY APP
# Revenue Management Project
# ============================================================

library(shiny)
library(shinydashboard)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(DT)

# ---------------------------------------------------------------
# DATA SETUP
# ---------------------------------------------------------------

# Try to load generated CSV, else generate inline
if (file.exists("tnstc_volvo_dynamic_pricing.csv")) {
  dataset <- read.csv("tnstc_volvo_dynamic_pricing.csv", stringsAsFactors = FALSE)
  dataset$travel_date  <- as.Date(dataset$travel_date)
  dataset$booking_date <- as.Date(dataset$booking_date)
} else {
  # Inline mini-generation if CSV not found
  set.seed(42)
  routes <- data.frame(
    origin      = c("BENGALURU","CHENNAI KILAMBAKKAM KCBT","CHENNAI KILAMBAKKAM KCBT",
                    "CHENNAI KILAMBAKKAM KCBT","CHENNAI KILAMBAKKAM KCBT",
                    "CHENNAI KILAMBAKKAM KCBT","CHENNAI KILAMBAKKAM KCBT",
                    "CHENNAI KILAMBAKKAM KCBT","CHENNAI KILAMBAKKAM KCBT",
                    "CHENNAI KILAMBAKKAM KCBT","COIMBATORE","MADURAI",
                    "NAGERCOIL","NAGERCOIL","NAGERCOIL","THANJAVUR",
                    "TIRUCHENDUR","TIRUCHENDUR","TIRUNELVELI","TIRUNELVELI","TIRUPPUR"),
    destination = c("CHENNAI KILAMBAKKAM KCBT","COIMBATORE","MADURAI",
                    "NAGERCOIL","SALEM","THANJAVUR","TIRUCHENDUR",
                    "TIRUNELVELI","TIRUPPUR","TRICHY","SALEM","TRICHY",
                    "MADURAI","TIRUNELVELI","TRICHY","TRICHY",
                    "MADURAI","TRICHY","MADURAI","TRICHY","SALEM"),
    base_fare   = c(680,825,740,1145,535,545,1050,1020,750,530,
                    295,220,420,145,650,120,320,535,280,510,210),
    stringsAsFactors = FALSE
  )
  
  generate_mini <- function(route_row, n = 50) {
    start_date  <- as.Date("2024-09-01")
    end_date    <- as.Date("2025-02-28")
    total_seats <- 40
    travel_dates  <- sample(seq(start_date, end_date, by="day"), n, replace=TRUE)
    booking_ahead <- sample(0:30, n, replace=TRUE)
    seats_sold    <- sample(10:total_seats, n, replace=TRUE)
    occ_rate      <- seats_sold / total_seats
    dow           <- wday(travel_dates)
    m             <- month(travel_dates)
    season <- ifelse(m %in% c(4,5),"Summer",
                     ifelse(m %in% c(6,7,8),"Monsoon",
                            ifelse(m %in% c(10,11),"Festive",
                                   ifelse(m==12|m %in% c(1,2),"Winter","Normal"))))
    s_mult <- ifelse(season=="Festive", runif(n,1.20,1.40),
                     ifelse(season=="Summer",  runif(n,1.10,1.25),
                            ifelse(season=="Monsoon", runif(n,0.85,0.95), runif(n,0.95,1.10))))
    a_mult <- ifelse(booking_ahead<=1,  runif(n,1.25,1.50),
                     ifelse(booking_ahead<=7,  runif(n,1.00,1.15),
                            ifelse(booking_ahead<=14, runif(n,0.90,1.00), runif(n,0.80,0.92))))
    o_mult <- ifelse(occ_rate>=0.90, runif(n,1.30,1.50),
                     ifelse(occ_rate>=0.75, runif(n,1.15,1.30),
                            ifelse(occ_rate>=0.50, runif(n,1.00,1.15), runif(n,0.85,1.00))))
    raw_fare     <- route_row$base_fare * s_mult * a_mult * o_mult
    dynamic_fare <- round(raw_fare / 5) * 5
    data.frame(
      booking_id      = paste0("BK", sprintf("%04d", 1:n)),
      origin          = route_row$origin,
      destination     = route_row$destination,
      travel_date     = travel_dates,
      booking_date    = travel_dates - booking_ahead,
      days_in_advance = booking_ahead,
      day_of_week     = weekdays(travel_dates),
      is_weekend      = dow %in% c(1,7),
      season          = season,
      seat_class      = sample(c("Sleeper","Semi-Sleeper","Seater"), n, replace=TRUE,
                               prob=c(0.5,0.35,0.15)),
      total_seats     = total_seats,
      seats_booked    = seats_sold,
      occupancy_rate  = round(occ_rate, 3),
      base_fare       = route_row$base_fare,
      dynamic_fare    = dynamic_fare,
      competitor_fare = round(dynamic_fare * runif(n,0.92,1.08)/5)*5,
      demand_score    = round(pmin(occ_rate*40 + runif(n,10,40), 100), 1),
      revenue         = dynamic_fare * seats_sold,
      stringsAsFactors = FALSE
    )
  }
  dataset <- bind_rows(lapply(1:nrow(routes), function(i) generate_mini(routes[i,])))
}

all_routes <- paste(dataset$origin, "→", dataset$destination)
route_choices <- sort(unique(all_routes))
season_choices <- c("All", sort(unique(dataset$season)))

# Fare prediction function
predict_fare <- function(base_fare, season, days_ahead, occupancy, is_weekend, is_holiday) {
  s <- switch(season,
              Summer  = runif(1, 1.10, 1.25),
              Festive = runif(1, 1.20, 1.40),
              Monsoon = runif(1, 0.85, 0.95),
              Winter  = runif(1, 1.05, 1.15),
              runif(1, 0.95, 1.05)
  )
  a <- if (days_ahead <= 1)       runif(1, 1.25, 1.50)
  else if (days_ahead <= 3)  runif(1, 1.10, 1.25)
  else if (days_ahead <= 7)  runif(1, 1.00, 1.10)
  else if (days_ahead <= 14) runif(1, 0.90, 1.00)
  else                       runif(1, 0.80, 0.92)
  o <- if (occupancy >= 90)      runif(1, 1.30, 1.50)
  else if (occupancy >= 75) runif(1, 1.15, 1.30)
  else if (occupancy >= 50) runif(1, 1.00, 1.15)
  else if (occupancy >= 30) runif(1, 0.90, 1.00)
  else                      runif(1, 0.75, 0.90)
  d <- if (is_weekend) runif(1, 1.10, 1.20) else runif(1, 0.95, 1.05)
  h <- if (is_holiday) runif(1, 1.15, 1.30) else 1.0
  round(base_fare * s * a * o * d * h / 5) * 5
}

# ---------------------------------------------------------------
# THEME COLORS
# ---------------------------------------------------------------
clr_primary   <- "#0D1B2A"
clr_accent    <- "#E63946"
clr_gold      <- "#F4A261"
clr_teal      <- "#2EC4B6"
clr_light     <- "#F8F9FA"
clr_card      <- "#132033"
clr_text      <- "#E0E6ED"
clr_muted     <- "#8899AA"

# ---------------------------------------------------------------
# CUSTOM CSS
# ---------------------------------------------------------------
custom_css <- "
@import url('https://fonts.googleapis.com/css2?family=Rajdhani:wght@400;500;600;700&family=Inter:wght@300;400;500&display=swap');

* { box-sizing: border-box; margin: 0; padding: 0; }
body, .content-wrapper, .main-sidebar, .wrapper {
  background-color: #F4F6F9 !important;
  font-family: 'Inter', sans-serif;
  color: #2C3E50;
}
.skin-blue .main-header .logo,
.skin-blue .main-header .navbar {
  background-color: #C0392B !important;
  border-bottom: 2px solid #E63946 !important;
}
.skin-blue .main-header .logo {
  font-family: 'Rajdhani', sans-serif !important;
  font-size: 20px !important;
  font-weight: 700 !important;
  letter-spacing: 1px;
  color: #FFFFFF !important;
}
.skin-blue .main-sidebar {
  background-color: #FFFFFF !important;
  border-right: 1px solid #E0E6ED !important;
}
.skin-blue .sidebar-menu > li > a {
  color: #5D6D7E !important;
  font-family: 'Inter', sans-serif;
  font-size: 13px;
  font-weight: 500;
  border-left: 3px solid transparent;
  transition: all 0.2s ease;
}
.skin-blue .sidebar-menu > li.active > a,
.skin-blue .sidebar-menu > li > a:hover {
  color: #C0392B !important;
  background-color: #FEF0EF !important;
  border-left: 3px solid #E63946 !important;
}
.skin-blue .sidebar-menu > li > a .fa {
  color: #E63946 !important;
}
.skin-blue .treeview-menu > li > a { color: #5D6D7E !important; }
.content-wrapper { background-color: #F4F6F9 !important; padding: 20px !important; }
.box {
  background: #FFFFFF !important;
  border: 1px solid #E0E6ED !important;
  border-top: 3px solid #E63946 !important;
  border-radius: 8px !important;
  box-shadow: 0 2px 12px rgba(0,0,0,0.07) !important;
  color: #2C3E50 !important;
}
.box-header { background: transparent !important; border-bottom: 1px solid #F0F3F7 !important; padding: 12px 16px !important; }
.box-title {
  font-family: 'Rajdhani', sans-serif !important;
  font-size: 16px !important;
  font-weight: 600 !important;
  letter-spacing: 0.5px;
  color: #2C3E50 !important;
}
.info-box {
  background: #FFFFFF !important;
  border-radius: 8px !important;
  border: 1px solid #E0E6ED !important;
  box-shadow: 0 2px 10px rgba(0,0,0,0.06) !important;
  min-height: 80px;
}
.info-box-icon { border-radius: 8px 0 0 8px !important; display: flex; align-items: center; justify-content: center; }
.info-box-content { padding: 8px 12px !important; }
.info-box-text {
  font-family: 'Inter', sans-serif !important;
  font-size: 11px !important;
  font-weight: 500 !important;
  text-transform: uppercase;
  letter-spacing: 1px;
  color: #7F8C8D !important;
}
.info-box-number {
  font-family: 'Rajdhani', sans-serif !important;
  font-size: 26px !important;
  font-weight: 700 !important;
  color: #2C3E50 !important;
}
.form-control {
  background-color: #FFFFFF !important;
  border: 1px solid #D5DBDB !important;
  color: #2C3E50 !important;
  border-radius: 6px !important;
  font-family: 'Inter', sans-serif;
  font-size: 13px;
}
.form-control:focus {
  border-color: #E63946 !important;
  box-shadow: 0 0 0 2px rgba(230,57,70,0.12) !important;
  outline: none !important;
}
select.form-control option { background-color: #FFFFFF; color: #2C3E50; }
label { color: #7F8C8D !important; font-size: 12px !important; font-weight: 500 !important; text-transform: uppercase; letter-spacing: 0.8px; margin-bottom: 4px; }
.btn-primary {
  background: linear-gradient(135deg, #E63946, #C0392B) !important;
  border: none !important;
  border-radius: 6px !important;
  font-family: 'Rajdhani', sans-serif !important;
  font-weight: 600 !important;
  font-size: 14px !important;
  letter-spacing: 1px;
  padding: 10px 24px !important;
  color: #fff !important;
  transition: all 0.2s ease !important;
  box-shadow: 0 4px 12px rgba(230,57,70,0.25) !important;
}
.btn-primary:hover { transform: translateY(-1px) !important; box-shadow: 0 6px 16px rgba(230,57,70,0.35) !important; }
.fare-result-box {
  background: linear-gradient(135deg, #FEF0EF, #FFF5F5);
  border: 1px solid #E63946;
  border-radius: 10px;
  padding: 20px;
  text-align: center;
  margin-top: 16px;
}
.fare-amount {
  font-family: 'Rajdhani', sans-serif;
  font-size: 52px;
  font-weight: 700;
  color: #C0392B;
  line-height: 1;
}
.fare-label {
  font-size: 11px;
  color: #7F8C8D;
  text-transform: uppercase;
  letter-spacing: 2px;
  margin-top: 4px;
}
.multiplier-badge {
  display: inline-block;
  background: #F0F3F7;
  border-radius: 20px;
  padding: 4px 12px;
  font-size: 12px;
  color: #E63946;
  font-weight: 500;
  margin: 3px;
  border: 1px solid #E0E6ED;
}
.section-title {
  font-family: 'Rajdhani', sans-serif;
  font-size: 13px;
  font-weight: 600;
  color: #7F8C8D;
  text-transform: uppercase;
  letter-spacing: 2px;
  margin-bottom: 12px;
  padding-bottom: 6px;
  border-bottom: 1px solid #E0E6ED;
}
.dataTables_wrapper, .dataTables_wrapper .dataTables_paginate .paginate_button {
  color: #2C3E50 !important;
  font-family: 'Inter', sans-serif !important;
  font-size: 12px !important;
}
table.dataTable thead th {
  background: #F4F6F9 !important;
  color: #5D6D7E !important;
  border-bottom: 1px solid #E0E6ED !important;
  font-size: 11px !important;
  text-transform: uppercase;
  letter-spacing: 0.8px;
}
table.dataTable tbody tr { background: #FFFFFF !important; color: #2C3E50 !important; }
table.dataTable tbody tr:hover { background: #FEF0EF !important; }
table.dataTable tbody td { border-color: #F0F3F7 !important; }
.dataTables_wrapper .dataTables_filter input,
.dataTables_wrapper .dataTables_length select { background: #FFFFFF !important; color: #2C3E50 !important; border-color: #D5DBDB !important; }
.page-header-banner {
  background: linear-gradient(135deg, #FFFFFF 0%, #FEF0EF 100%);
  border: 1px solid #E0E6ED;
  border-left: 4px solid #E63946;
  border-radius: 8px;
  padding: 16px 20px;
  margin-bottom: 20px;
  display: flex;
  align-items: center;
  gap: 16px;
}
.page-header-title {
  font-family: 'Rajdhani', sans-serif;
  font-size: 22px;
  font-weight: 700;
  color: #2C3E50;
  letter-spacing: 1px;
}
.page-header-sub {
  font-size: 12px;
  color: #7F8C8D;
  margin-top: 2px;
}
.tnstc-badge {
  background: linear-gradient(135deg, #E63946, #C0392B);
  border-radius: 6px;
  padding: 8px 14px;
  font-family: 'Rajdhani', sans-serif;
  font-size: 18px;
  font-weight: 700;
  color: #fff;
  letter-spacing: 2px;
  white-space: nowrap;
}
.slider-container .irs--shiny .irs-bar { background: #E63946 !important; }
.slider-container .irs--shiny .irs-handle { border-color: #E63946 !important; }
.nav-tabs-custom > .nav-tabs > li.active > a,
.nav-tabs-custom > .nav-tabs > li.active > a:hover {
  border-top-color: #E63946 !important;
  background: #FFFFFF !important;
  color: #2C3E50 !important;
}
.nav-tabs-custom > .nav-tabs > li > a { color: #7F8C8D !important; }
.nav-tabs-custom { border-color: #E0E6ED !important; }
.nav-tabs-custom > .tab-content { background: #FFFFFF !important; }
::-webkit-scrollbar { width: 6px; }
::-webkit-scrollbar-track { background: #F4F6F9; }
::-webkit-scrollbar-thumb { background: #D5DBDB; border-radius: 3px; }
::-webkit-scrollbar-thumb:hover { background: #E63946; }
"

# ---------------------------------------------------------------
# UI
# ---------------------------------------------------------------
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = "🚌 TNSTC VOLVO",
    titleWidth = 220
  ),
  
  dashboardSidebar(
    width = 220,
    tags$head(tags$style(HTML(custom_css))),
    sidebarMenu(
      id = "tabs",
      menuItem("Dashboard",       tabName = "dashboard",   icon = icon("tachometer-alt")),
      menuItem("Fare Predictor",  tabName = "predictor",   icon = icon("calculator")),
      menuItem("Route Analysis",  tabName = "routes",      icon = icon("route")),
      menuItem("Revenue Trends",  tabName = "revenue",     icon = icon("chart-line")),
      menuItem("Data Explorer",   tabName = "data",        icon = icon("table")),
      menuItem("About",           tabName = "about",       icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # ── DASHBOARD ──────────────────────────────────────────
      tabItem(tabName = "dashboard",
              div(class = "page-header-banner",
                  div(class = "tnstc-badge", "TNSTC"),
                  div(
                    div(class = "page-header-title", "Dynamic Pricing Dashboard"),
                    div(class = "page-header-sub", "Revenue Management · Volvo AC Bus Network · Tamil Nadu")
                  )
              ),
              
              fluidRow(
                infoBoxOutput("box_total_trips",   width = 3),
                infoBoxOutput("box_avg_fare",      width = 3),
                infoBoxOutput("box_total_revenue", width = 3),
                infoBoxOutput("box_avg_occupancy", width = 3)
              ),
              
              fluidRow(
                box(title = "Dynamic Fare vs Base Fare by Route", width = 8, status = "danger",
                    solidHeader = FALSE, plotlyOutput("plot_fare_compare", height = "340px")),
                box(title = "Season Distribution", width = 4, status = "danger",
                    solidHeader = FALSE, plotlyOutput("plot_season_pie", height = "340px"))
              ),
              
              fluidRow(
                box(title = "Monthly Revenue Trend", width = 6, status = "danger",
                    solidHeader = FALSE, plotlyOutput("plot_monthly_rev", height = "280px")),
                box(title = "Occupancy Rate Distribution", width = 6, status = "danger",
                    solidHeader = FALSE, plotlyOutput("plot_occ_dist", height = "280px"))
              )
      ),
      
      # ── FARE PREDICTOR ─────────────────────────────────────
      tabItem(tabName = "predictor",
              div(class = "page-header-banner",
                  div(class = "tnstc-badge", "FARE"),
                  div(
                    div(class = "page-header-title", "Real-Time Fare Predictor"),
                    div(class = "page-header-sub", "Estimate dynamic fare based on booking conditions")
                  )
              ),
              
              fluidRow(
                box(title = "Booking Parameters", width = 5, status = "danger",
                    selectInput("pred_route", "Select Route",
                                choices = {
                                  route_pairs <- dataset %>%
                                    select(origin, destination, base_fare) %>%
                                    distinct(origin, destination, .keep_all = TRUE) %>%
                                    arrange(origin, destination)
                                  vals  <- paste(route_pairs$origin, route_pairs$destination, sep = "||")
                                  labs  <- paste(route_pairs$origin, "→", route_pairs$destination)
                                  setNames(vals, labs)
                                }),
                    selectInput("pred_season", "Travel Season",
                                choices = c("Normal","Summer","Monsoon","Festive","Winter")),
                    sliderInput("pred_days", "Days in Advance", min = 0, max = 30, value = 7),
                    sliderInput("pred_occ", "Expected Occupancy (%)", min = 10, max = 100, value = 60),
                    checkboxInput("pred_weekend", "Weekend Travel", value = FALSE),
                    checkboxInput("pred_holiday", "Public Holiday", value = FALSE),
                    br(),
                    actionButton("btn_predict", "Calculate Dynamic Fare", class = "btn-primary", width = "100%")
                ),
                
                box(title = "Fare Estimate", width = 7, status = "danger",
                    uiOutput("fare_result_ui"),
                    br(),
                    plotlyOutput("plot_fare_breakdown", height = "260px")
                )
              )
      ),
      
      # ── ROUTE ANALYSIS ─────────────────────────────────────
      tabItem(tabName = "routes",
              div(class = "page-header-banner",
                  div(class = "tnstc-badge", "ROUTE"),
                  div(
                    div(class = "page-header-title", "Route-Level Analysis"),
                    div(class = "page-header-sub", "Fare patterns, demand, and pricing insights per route")
                  )
              ),
              
              fluidRow(
                box(width = 4, status = "danger",
                    selectInput("route_filter", "Select Route",
                                choices = c("All Routes", route_choices)),
                    selectInput("season_filter", "Season", choices = season_choices),
                    plotlyOutput("plot_route_occ", height = "240px")
                ),
                box(title = "Fare Range by Route", width = 8, status = "danger",
                    plotlyOutput("plot_route_box", height = "360px"))
              ),
              
              fluidRow(
                box(title = "Advance Booking vs Fare", width = 6, status = "danger",
                    plotlyOutput("plot_advance_fare", height = "280px")),
                box(title = "Day of Week Pricing Pattern", width = 6, status = "danger",
                    plotlyOutput("plot_dow_fare", height = "280px"))
              )
      ),
      
      # ── REVENUE ────────────────────────────────────────────
      tabItem(tabName = "revenue",
              div(class = "page-header-banner",
                  div(class = "tnstc-badge", "REV"),
                  div(
                    div(class = "page-header-title", "Revenue & Demand Analytics"),
                    div(class = "page-header-sub", "Seasonal revenue performance and demand forecasting")
                  )
              ),
              
              fluidRow(
                box(title = "Revenue by Season", width = 6, status = "danger",
                    plotlyOutput("plot_rev_season", height = "300px")),
                box(title = "Revenue by Seat Class", width = 6, status = "danger",
                    plotlyOutput("plot_rev_class", height = "300px"))
              ),
              
              fluidRow(
                box(title = "Demand Score vs Dynamic Fare", width = 6, status = "danger",
                    plotlyOutput("plot_demand_fare", height = "300px")),
                box(title = "Competitor Fare Comparison", width = 6, status = "danger",
                    plotlyOutput("plot_competitor", height = "300px"))
              )
      ),
      
      # ── DATA EXPLORER ──────────────────────────────────────
      tabItem(tabName = "data",
              div(class = "page-header-banner",
                  div(class = "tnstc-badge", "DATA"),
                  div(
                    div(class = "page-header-title", "Dataset Explorer"),
                    div(class = "page-header-sub", "Browse and filter the full booking dataset")
                  )
              ),
              fluidRow(
                box(width = 12, status = "danger",
                    DTOutput("data_table")
                )
              )
      ),
      
      # ── ABOUT ──────────────────────────────────────────────
      tabItem(tabName = "about",
              div(class = "page-header-banner",
                  div(class = "tnstc-badge", "INFO"),
                  div(
                    div(class = "page-header-title", "About This Project"),
                    div(class = "page-header-sub", "Dynamic Pricing Model · Revenue Management")
                  )
              ),
              fluidRow(
                box(title = "Project Overview", width = 6, status = "danger",
                    tags$div(style = "color:#2C3E50; font-family:'Inter',sans-serif; line-height:1.7; font-size:13px;",
                             tags$p(tags$b(style="color:#C0392B;","Subject:"), " Revenue Management"),
                             tags$p(tags$b(style="color:#C0392B;","Topic:"), " Dynamic Pricing Model for TNSTC Volvo Buses"),
                             tags$p(tags$b(style="color:#C0392B;","Scope:"), " Tamil Nadu State Transport Corporation — recently launched Volvo AC Bus network"),
                             tags$hr(style="border-color:#E0E6ED;"),
                             tags$p(style="color:#7F8C8D;", "This application simulates and analyses a dynamic pricing strategy for TNSTC Volvo buses. Fares are adjusted based on multiple real-world demand factors including season, day of week, booking lead time, occupancy, and public holidays.")
                    )
                ),
                box(title = "Pricing Factors", width = 6, status = "danger",
                    tags$div(style = "font-family:'Inter',sans-serif; font-size:13px;",
                             tags$table(style = "width:100%; border-collapse:collapse;",
                                        tags$thead(tags$tr(
                                          tags$th(style="color:#7F8C8D;padding:8px;border-bottom:1px solid #E0E6ED;","Factor"),
                                          tags$th(style="color:#7F8C8D;padding:8px;border-bottom:1px solid #E0E6ED;","Effect on Fare")
                                        )),
                                        tags$tbody(
                                          tags$tr(tags$td(style="padding:8px;color:#E63946;","Festive Season"),  tags$td(style="padding:8px;color:#2C3E50;","↑ 20–40%")),
                                          tags$tr(style="background:#F4F6F9;",
                                                  tags$td(style="padding:8px;color:#E63946;","Summer"),           tags$td(style="padding:8px;color:#2C3E50;","↑ 10–25%")),
                                          tags$tr(tags$td(style="padding:8px;color:#E63946;","Last-Minute (≤1d)"),tags$td(style="padding:8px;color:#2C3E50;","↑ 25–50%")),
                                          tags$tr(style="background:#F4F6F9;",
                                                  tags$td(style="padding:8px;color:#E63946;","Early Booking (>14d)"),tags$td(style="padding:8px;color:#2C3E50;","↓ 8–20%")),
                                          tags$tr(tags$td(style="padding:8px;color:#E63946;","High Occupancy (>90%)"),tags$td(style="padding:8px;color:#2C3E50;","↑ 30–50%")),
                                          tags$tr(style="background:#F4F6F9;",
                                                  tags$td(style="padding:8px;color:#E63946;","Weekend"),           tags$td(style="padding:8px;color:#2C3E50;","↑ 10–20%")),
                                          tags$tr(tags$td(style="padding:8px;color:#E63946;","Public Holiday"),    tags$td(style="padding:8px;color:#2C3E50;","↑ 15–30%")),
                                          tags$tr(style="background:#F4F6F9;",
                                                  tags$td(style="padding:8px;color:#E63946;","Monsoon"),           tags$td(style="padding:8px;color:#2C3E50;","↓ 5–15%"))
                                        )
                             )
                    )
                )
              ),
              fluidRow(
                box(title = "Routes Covered", width = 12, status = "danger",
                    tags$div(style="display:flex;flex-wrap:wrap;gap:8px;padding:4px;",
                             lapply(route_choices, function(r) {
                               tags$span(style="background:#FEF0EF;border:1px solid #F5B7B1;border-radius:20px;padding:5px 14px;
                                 font-size:12px;color:#C0392B;font-family:'Inter',sans-serif;", r)
                             })
                    )
                )
              )
      )
    )
  )
)

# ---------------------------------------------------------------
# SERVER
# ---------------------------------------------------------------
server <- function(input, output, session) {
  
  gg_theme <- theme_minimal() +
    theme(
      plot.background    = element_rect(fill = "#FFFFFF", color = NA),
      panel.background   = element_rect(fill = "#FFFFFF", color = NA),
      panel.grid.major   = element_line(color = "#F0F3F7", linewidth = 0.4),
      panel.grid.minor   = element_blank(),
      axis.text          = element_text(color = "#5D6D7E", size = 10),
      axis.title         = element_text(color = "#5D6D7E", size = 11),
      plot.title         = element_text(color = "#2C3E50", size = 13, face = "bold"),
      legend.background  = element_rect(fill = "#FFFFFF", color = NA),
      legend.text        = element_text(color = "#5D6D7E"),
      legend.title       = element_text(color = "#5D6D7E"),
      strip.text         = element_text(color = "#5D6D7E")
    )
  
  plotly_config <- function(p) {
    p %>% layout(
      paper_bgcolor = "#FFFFFF",
      plot_bgcolor  = "#FFFFFF",
      font = list(color = "#5D6D7E", family = "Inter"),
      xaxis = list(gridcolor = "#F0F3F7", zerolinecolor = "#E0E6ED"),
      yaxis = list(gridcolor = "#F0F3F7", zerolinecolor = "#E0E6ED")
    ) %>% config(displayModeBar = FALSE)
  }
  
  # INFO BOXES
  output$box_total_trips <- renderInfoBox({
    infoBox("Total Bookings", nrow(dataset), icon = icon("ticket-alt"),
            color = "red", fill = TRUE)
  })
  output$box_avg_fare <- renderInfoBox({
    infoBox("Avg Dynamic Fare", paste0("₹", round(mean(dataset$dynamic_fare))),
            icon = icon("rupee-sign"), color = "orange", fill = TRUE)
  })
  output$box_total_revenue <- renderInfoBox({
    rev_cr <- round(sum(dataset$revenue) / 1e5, 1)
    infoBox("Total Revenue", paste0("₹", rev_cr, "L"),
            icon = icon("chart-bar"), color = "teal", fill = TRUE)
  })
  output$box_avg_occupancy <- renderInfoBox({
    infoBox("Avg Occupancy", paste0(round(mean(dataset$occupancy_rate) * 100, 1), "%"),
            icon = icon("users"), color = "purple", fill = TRUE)
  })
  
  # DASHBOARD PLOTS
  output$plot_fare_compare <- renderPlotly({
    df <- dataset %>%
      group_by(origin, destination) %>%
      summarise(base = mean(base_fare), dynamic = mean(dynamic_fare), .groups = "drop") %>%
      mutate(route = paste0(substr(origin,1,6), "→", substr(destination,1,6)))
    p <- plot_ly(df, x = ~route, y = ~base, type = "bar", name = "Base Fare",
                 marker = list(color = "#1e3250")) %>%
      add_trace(y = ~dynamic, name = "Dynamic Fare",
                marker = list(color = "#E63946")) %>%
      layout(barmode = "group",
             xaxis = list(tickangle = -35, tickfont = list(size = 9)),
             legend = list(orientation = "h", y = 1.1))
    plotly_config(p)
  })
  
  output$plot_season_pie <- renderPlotly({
    df <- dataset %>% count(season)
    colors <- c("#E63946","#F4A261","#2EC4B6","#48CAE4","#8899AA")
    p <- plot_ly(df, labels = ~season, values = ~n, type = "pie",
                 marker = list(colors = colors,
                               line = list(color = "#132033", width = 2)),
                 textfont = list(color = "#F8F9FA")) %>%
      layout(showlegend = TRUE,
             legend = list(font = list(color = "#8899AA")))
    plotly_config(p)
  })
  
  output$plot_monthly_rev <- renderPlotly({
    df <- dataset %>%
      mutate(month = floor_date(travel_date, "month")) %>%
      group_by(month) %>%
      summarise(revenue = sum(revenue)/1000, .groups = "drop")
    p <- plot_ly(df, x = ~month, y = ~revenue, type = "scatter", mode = "lines+markers",
                 line = list(color = "#2EC4B6", width = 2.5),
                 marker = list(color = "#F4A261", size = 7),
                 fill = "tozeroy", fillcolor = "rgba(46,196,182,0.1)") %>%
      layout(xaxis = list(title = "Month"),
             yaxis = list(title = "Revenue (₹ Thousands)"))
    plotly_config(p)
  })
  
  output$plot_occ_dist <- renderPlotly({
    p <- plot_ly(x = ~dataset$occupancy_rate * 100, type = "histogram",
                 nbinsx = 20,
                 marker = list(color = "#E63946",
                               line = list(color = "#132033", width = 1))) %>%
      layout(xaxis = list(title = "Occupancy Rate (%)"),
             yaxis = list(title = "Count"))
    plotly_config(p)
  })
  
  # FARE PREDICTOR
  prediction <- eventReactive(input$btn_predict, {
    req(input$pred_route)
    parts     <- strsplit(input$pred_route, "\\|\\|")[[1]]
    orig      <- trimws(parts[1]); dest <- trimws(parts[2])
    base_fare <- dataset %>%
      filter(origin == orig, destination == dest) %>%
      pull(base_fare) %>% mean(na.rm = TRUE)
    if (is.na(base_fare) || length(base_fare) == 0) base_fare <- 500
    
    set.seed(NULL)
    pred_fare <- predict_fare(
      base_fare, input$pred_season, input$pred_days,
      input$pred_occ, input$pred_weekend, input$pred_holiday
    )
    list(
      base       = round(base_fare),
      predicted  = pred_fare,
      change_pct = round((pred_fare - base_fare) / base_fare * 100, 1),
      orig       = orig, dest = dest
    )
  })
  
  output$fare_result_ui <- renderUI({
    p <- prediction()
    col <- if (p$change_pct > 0) "#E63946" else "#2EC4B6"
    arrow <- if (p$change_pct > 0) "▲" else "▼"
    div(class = "fare-result-box",
        div(style = "font-size:12px;color:#8899AA;margin-bottom:6px;letter-spacing:2px;",
            toupper(p$orig), " → ", toupper(p$dest)),
        div(class = "fare-amount", paste0("₹", p$predicted)),
        div(class = "fare-label", "ESTIMATED DYNAMIC FARE"),
        br(),
        div(style = paste0("color:", col, ";font-size:15px;font-weight:600;"),
            arrow, " ", abs(p$change_pct), "% from base fare (₹", p$base, ")"),
        br(),
        div(
          span(class = "multiplier-badge", paste0("Season: ", input$pred_season)),
          span(class = "multiplier-badge", paste0("Lead: ", input$pred_days, " days")),
          span(class = "multiplier-badge", paste0("Occ: ", input$pred_occ, "%")),
          if (input$pred_weekend) span(class = "multiplier-badge", "Weekend"),
          if (input$pred_holiday) span(class = "multiplier-badge", "Holiday")
        )
    )
  })
  
  output$plot_fare_breakdown <- renderPlotly({
    req(prediction())
    p_data <- prediction()
    cats   <- c("Base Fare", "After Season", "After Lead Time", "After Occupancy", "Final Fare")
    vals   <- c(p_data$base,
                round(p_data$base * 1.15),
                round(p_data$base * 1.15 * ifelse(input$pred_days <= 3, 1.2, 0.9)),
                round(p_data$base * 1.15 * ifelse(input$pred_days <= 3, 1.2, 0.9) *
                        ifelse(input$pred_occ >= 75, 1.2, 1.0)),
                p_data$predicted)
    colors <- c("#1e3250","#F4A261","#E63946","#2EC4B6","#48CAE4")
    p <- plot_ly(x = ~cats, y = ~vals, type = "bar",
                 marker = list(color = colors),
                 text = paste0("₹", vals), textposition = "outside",
                 textfont = list(color = "#F8F9FA", size = 11)) %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Fare (₹)"),
             showlegend = FALSE)
    plotly_config(p)
  })
  
  # ROUTE ANALYSIS
  filtered_data <- reactive({
    df <- dataset
    if (input$route_filter != "All Routes") {
      parts <- strsplit(input$route_filter, " → ")[[1]]
      df <- df %>% filter(origin == parts[1], destination == parts[2])
    }
    if (input$season_filter != "All") df <- df %>% filter(season == input$season_filter)
    df
  })
  
  output$plot_route_occ <- renderPlotly({
    df <- filtered_data() %>%
      group_by(season) %>%
      summarise(avg_occ = mean(occupancy_rate)*100, .groups="drop")
    p <- plot_ly(df, x = ~season, y = ~avg_occ, type = "bar",
                 marker = list(color = c("#E63946","#F4A261","#2EC4B6","#48CAE4","#8899AA")[1:nrow(df)])) %>%
      layout(title = list(text="Avg Occupancy by Season", font=list(size=12,color="#F8F9FA")),
             xaxis = list(title=""), yaxis = list(title="Occupancy (%)"))
    plotly_config(p)
  })
  
  output$plot_route_box <- renderPlotly({
    df <- dataset %>%
      mutate(route = paste0(substr(origin,1,5),"→",substr(destination,1,5)))
    p <- plot_ly(df, x = ~route, y = ~dynamic_fare, type = "box",
                 fillcolor = "rgba(230,57,70,0.3)",
                 line = list(color = "#E63946"),
                 marker = list(color = "#F4A261", size = 3)) %>%
      layout(xaxis = list(tickangle = -35, tickfont = list(size = 8)),
             yaxis = list(title = "Dynamic Fare (₹)"),
             showlegend = FALSE)
    plotly_config(p)
  })
  
  output$plot_advance_fare <- renderPlotly({
    df <- dataset %>%
      mutate(lead_group = cut(days_in_advance, breaks = c(-1,1,3,7,14,30),
                              labels = c("0-1","2-3","4-7","8-14","15-30"))) %>%
      group_by(lead_group) %>%
      summarise(avg_fare = mean(dynamic_fare), .groups="drop")
    p <- plot_ly(df, x = ~lead_group, y = ~avg_fare, type = "scatter",
                 mode = "lines+markers",
                 line = list(color = "#F4A261", width = 2.5),
                 marker = list(color = "#E63946", size = 8)) %>%
      layout(xaxis = list(title = "Days Before Travel"),
             yaxis = list(title = "Avg Dynamic Fare (₹)"))
    plotly_config(p)
  })
  
  output$plot_dow_fare <- renderPlotly({
    dow_order <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
    df <- dataset %>%
      group_by(day_of_week) %>%
      summarise(avg_fare = mean(dynamic_fare), .groups="drop") %>%
      mutate(day_of_week = factor(day_of_week, levels = dow_order)) %>%
      arrange(day_of_week)
    p <- plot_ly(df, x = ~day_of_week, y = ~avg_fare, type = "bar",
                 marker = list(color = ifelse(df$day_of_week %in% c("Saturday","Sunday"),
                                              "#E63946", "#1e3250"))) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Avg Fare (₹)"))
    plotly_config(p)
  })
  
  # REVENUE PLOTS
  output$plot_rev_season <- renderPlotly({
    df <- dataset %>%
      group_by(season) %>%
      summarise(total_rev = sum(revenue)/1000, .groups="drop") %>%
      arrange(desc(total_rev))
    p <- plot_ly(df, x = ~reorder(season, total_rev), y = ~total_rev, type = "bar",
                 marker = list(color = c("#E63946","#F4A261","#2EC4B6","#48CAE4","#8899AA")[1:nrow(df)]),
                 text = paste0("₹", round(df$total_rev,1), "K"),
                 textposition = "outside",
                 textfont = list(color="#F8F9FA", size=11)) %>%
      layout(xaxis = list(title=""), yaxis = list(title = "Revenue (₹ Thousands)"))
    plotly_config(p)
  })
  
  output$plot_rev_class <- renderPlotly({
    df <- dataset %>%
      group_by(seat_class) %>%
      summarise(total_rev = sum(revenue)/1000, .groups="drop")
    p <- plot_ly(df, labels = ~seat_class, values = ~total_rev, type = "pie",
                 hole = 0.45,
                 marker = list(colors = c("#E63946","#F4A261","#2EC4B6"),
                               line = list(color="#132033", width=2)),
                 textfont = list(color="#F8F9FA")) %>%
      layout(showlegend = TRUE, legend = list(font = list(color="#8899AA")))
    plotly_config(p)
  })
  
  output$plot_demand_fare <- renderPlotly({
    samp <- dataset %>% sample_n(min(500, nrow(dataset)))
    p <- plot_ly(samp, x = ~demand_score, y = ~dynamic_fare,
                 color = ~season, type = "scatter", mode = "markers",
                 colors = c("#E63946","#F4A261","#2EC4B6","#48CAE4","#8899AA"),
                 marker = list(size = 5, opacity = 0.65)) %>%
      layout(xaxis = list(title="Demand Score"),
             yaxis = list(title="Dynamic Fare (₹)"))
    plotly_config(p)
  })
  
  output$plot_competitor <- renderPlotly({
    df <- dataset %>%
      mutate(diff = dynamic_fare - competitor_fare) %>%
      group_by(season) %>%
      summarise(our_fare  = mean(dynamic_fare),
                comp_fare = mean(competitor_fare), .groups="drop")
    p <- plot_ly(df, x = ~season) %>%
      add_trace(y = ~our_fare,  type = "bar", name = "TNSTC Fare",
                marker = list(color = "#E63946")) %>%
      add_trace(y = ~comp_fare, type = "bar", name = "Competitor",
                marker = list(color = "#1e3250")) %>%
      layout(barmode = "group",
             xaxis = list(title=""), yaxis = list(title="Avg Fare (₹)"),
             legend = list(orientation="h", y=1.1))
    plotly_config(p)
  })
  
  # DATA TABLE
  output$data_table <- renderDT({
    df <- dataset %>%
      select(booking_id, origin, destination, travel_date,
             days_in_advance, season, seat_class,
             seats_booked, occupancy_rate,
             base_fare, dynamic_fare, revenue) %>%
      mutate(occupancy_rate = paste0(round(occupancy_rate*100,1),"%"),
             revenue = paste0("₹", formatC(revenue, format="d", big.mark=",")))
    datatable(df,
              options = list(
                pageLength  = 15,
                scrollX     = TRUE,
                dom         = "Bfrtip",
                columnDefs  = list(list(className = "dt-center", targets = "_all"))
              ),
              rownames = FALSE,
              class    = "table-hover"
    )
  })
}

# ---------------------------------------------------------------
# RUN
# ---------------------------------------------------------------
shinyApp(ui = ui, server = server)