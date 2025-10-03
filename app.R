# app.R
library(shiny)
library(shinydashboard)
library(tidyverse)
library(tidyquant)   # for stock prices (uses Yahoo)
library(plotly)
library(DT)
library(httr)
library(jsonlite)
library(DBI)

# library(RPostgres)  # for PostgreSQL
# library(RMySQL)     # for MySQL


OMDB_API_KEY <- "2412e4ae"
SCORECARD_API_KEY <- "AMqNJJMaUhM8TucgC30iuJVBh1k3NY2ZlpCHqX7j"


# Helper functions

get_stock_data <- function(symbol, from = Sys.Date() - 365, to = Sys.Date()) {
  # tidyquant returns tibble with date and OHLC
  tryCatch({
    tidyquant::tq_get(symbol, get = "stock.prices", from = from, to = to)
  }, error = function(e) {
    NULL
  })
}

get_movie_by_title <- function(title) {
  req_url <- modify_url("http://www.omdbapi.com/",
                        query = list(apikey = OMDB_API_KEY, t = title, r = "json"))
  res <- GET(req_url)
  if (status_code(res) != 200) return(NULL)
  parsed <- content(res, as = "parsed", simplifyVector = TRUE)
  if (!is.null(parsed$Response) && parsed$Response == "False") return(NULL)
  parsed
}

get_collegescorecard_data <- function(query = "computer science", per_page = 20) {
  # Example search by name/program. Without key, function returns NULL.
  if (is.null(SCORECARD_API_KEY) || SCORECARD_API_KEY == "") return(NULL)
  base <- "https://api.data.gov/ed/collegescorecard/v1/schools"
  q <- paste0("?api_key=", SCORECARD_API_KEY,
              "&school.name=", URLencode(query),
              "&fields=school.name,latest.cost.tuition,institution.degree_awarded_predominant,latest.earnings.10_yrs_after_entry")
  url <- paste0(base, q, "&per_page=", per_page)
  res <- GET(url)
  if (status_code(res) != 200) return(NULL)
  parsed <- content(res, as = "parsed", simplifyVector = TRUE)
  if (!"results" %in% names(parsed)) return(NULL)
  results <- parsed$results %>%
    map_df(~list(
      name = .x$school.name %||% NA,
      tuition = .x$latest.cost.tuition %||% NA,
      predominant_degree = .x$institution.degree_awarded_predominant %||% NA,
      earnings_10yr = .x$latest.earnings.10_yrs_after_entry %||% NA
    ))
  results
}

# fallback sample education data if API not available
sample_education_df <- tibble(
  name = c("State University A","Tech Institute B","Liberal Arts C"),
  tuition = c(12000, 22000, 38000),
  predominant_degree = c("Bachelor's", "Associate's", "Bachelor's"),
  earnings_10yr = c(60000, 45000, 52000)
)


# UI
ui <- dashboardPage(
  dashboardHeader(title = "Multi-Source Monitoring Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Stocks", tabName = "stocks", icon = icon("chart-line")),
      menuItem("Movies", tabName = "movies", icon = icon("film")),
      menuItem("Education", tabName = "education", icon = icon("graduation-cap")),
      menuItem("Database Monitor", tabName = "db", icon = icon("database")),
      menuItem("Settings", tabName = "settings", icon = icon("cog"))
    )
  ),
  dashboardBody(
    tabItems(
      # Stocks tab
      tabItem("stocks",
              fluidRow(
                box(width = 4,
                    textInput("stock_symbol", "Stock ticker (comma-separated):", value = "AAPL,MSFT"),
                    dateRangeInput("stock_range", "Date range:",
                                   start = Sys.Date() - 180, end = Sys.Date()),
                    actionButton("stock_go", "Get Stocks")
                ),
                box(width = 8,
                    uiOutput("stock_tabs_ui")
                )
              )
      ),
      
      # Movies tab
      tabItem("movies",
              fluidRow(
                box(width = 4,
                    textInput("movie_title", "Movie title (exact):", value = "Inception"),
                    actionButton("movie_go", "Get Movie")
                ),
                box(width = 8, verbatimTextOutput("movie_status"), DTOutput("movie_table"))
              )
      ),
      
      # Education tab
      tabItem("education",
              fluidRow(
                box(width = 4,
                    textInput("school_query", "Search school name or keyword:", value = "State"),
                    numericInput("school_perpage", "Max results:", value = 10, min = 1, max = 100),
                    actionButton("school_go", "Search")
                ),
                box(width = 8, DTOutput("school_table"))
              )
      ),
      
      # DB Monitor
      tabItem("db",
              fluidRow(
                box(width = 4,
                    selectInput("db_type", "DB Type:", choices = c("Postgres","MySQL")),
                    textInput("db_host", "Host", value = "127.0.0.1"),
                    numericInput("db_port", "Port", value = 5432),
                    textInput("db_name", "Database", value = "mydb"),
                    textInput("db_user", "User", value = ""),
                    passwordInput("db_password", "Password", value = ""),
                    textInput("db_query", "SQL Query (select):", value = "SELECT * FROM some_table LIMIT 50"),
                    numericInput("poll_seconds", "Poll interval (seconds):", value = 15, min = 5),
                    actionButton("db_connect", "Connect & Start Poll")
                ),
                box(width = 8,
                    verbatimTextOutput("db_status"),
                    DTOutput("db_table")
                )
              )
      ),
      
      # Settings
      tabItem("settings",
              fluidRow(
                box(width = 12,
                    h4("API keys / Notes"),
                    p("This app expects OMDb and College Scorecard API keys in environment variables:"),
                    tags$ul(
                      tags$li(code("OMDB_API_KEY") ," — Get one at http://www.omdbapi.com/"),
                      tags$li(code("SCORECARD_API_KEY") ," — Get one at https://collegescorecard.ed.gov/data/documentation/ (or https://api.data.gov/)")
                    ),
                    p("If keys are missing, the app will fallback to sample data where possible.")
                )
              )
      )
    )
  )
)

# --------------------
# SERVER
# --------------------
server <- function(input, output, session) {
  
  # ---- STOCKS ----
  stock_requested <- eventReactive(input$stock_go, {
    req(input$stock_symbol)
    symbols <- str_split(input$stock_symbol, ",")[[1]] %>% str_trim() %>% unique()
    symbols
  })
  
  stock_data_cache <- reactiveVal(list())
  
  observeEvent(stock_requested(), {
    syms <- stock_requested()
    from <- input$stock_range[1]
    to <- input$stock_range[2]
    out <- list()
    for (s in syms) {
      df <- get_stock_data(s, from = from, to = to)
      if (!is.null(df)) {
        out[[s]] <- df
      } else {
        out[[s]] <- tibble(date = as.Date(NA), open = NA)
      }
    }
    stock_data_cache(out)
  }, ignoreNULL = TRUE)
  
  output$stock_tabs_ui <- renderUI({
    cache <- stock_data_cache()
    if (length(cache) == 0) {
      return(h4("No stock data. Enter tickers and press Get Stocks."))
    }
    tabs <- lapply(names(cache), function(sym) {
      tabPanel(title = sym,
               plotlyOutput(paste0("plot_", sym)),
               DTOutput(paste0("tbl_", sym))
      )
    })
    do.call(tabsetPanel, tabs)
  })
  
  observe({
    cache <- stock_data_cache()
    for (sym in names(cache)) {
      local({
        s <- sym
        df <- cache[[s]]
        output[[paste0("plot_", s)]] <- renderPlotly({
          req(df)
          if (nrow(df) == 0) return(NULL)
          p <- plot_ly(df, x = ~date, y = ~close, type = "scatter", mode = "lines",
                       name = paste0(s, " Close")) %>%
            layout(title = paste("Price:", s), xaxis = list(title = "Date"), yaxis = list(title = "Price"))
          p
        })
        output[[paste0("tbl_", s)]] <- renderDT({
          df %>% select(date, open, high, low, close, volume)
        })
      })
    }
  })
  
  # ---- MOVIES ----
  movie_data <- eventReactive(input$movie_go, {
    req(input$movie_title)
    if (is.null(OMDB_API_KEY) || OMDB_API_KEY == "") {
      return(list(error = "OMDb API key missing. Set OMDB_API_KEY env var."))
    }
    res <- get_movie_by_title(input$movie_title)
    if (is.null(res)) {
      list(error = "Movie not found or API error.")
    } else {
      list(ok = TRUE, data = res)
    }
  })
  
  output$movie_status <- renderText({
    md <- movie_data()
    if (is.null(md)) return("")
    if (!is.null(md$error)) md$error else paste0("Fetched: ", md$data$Title, " (", md$data$Year, ")")
  })
  
  output$movie_table <- renderDT({
    md <- movie_data()
    if (is.null(md) || !is.null(md$error)) {
      return(datatable(tibble(Message = md$error %||% "No data"), options = list(dom = 't')))
    }
    d <- md$data
    rows <- tibble(
      Field = c("Title","Year","Rated","Released","Runtime","Genre","Director","Actors","IMDb Rating","Metascore","BoxOffice","Plot"),
      Value = c(d$Title, d$Year, d$Rated, d$Released, d$Runtime, d$Genre, d$Director, d$Actors, d$imdbRating %||% NA, d$Metascore %||% NA, d$BoxOffice %||% NA, d$Plot %||% NA)
    )
    datatable(rows, options = list(dom = 't'), rownames = FALSE)
  })
  
  # ---- EDUCATION (College Scorecard) ----
  school_results <- eventReactive(input$school_go, {
    q <- input$school_query
    n <- input$school_perpage
    if (is.null(SCORECARD_API_KEY) || SCORECARD_API_KEY == "") {
      # fallback to sample data (local)
      return(sample_education_df)
    } else {
      res <- get_collegescorecard_data(query = q, per_page = n)
      if (is.null(res) || nrow(res) == 0) {
        return(sample_education_df)
      } else {
        return(res)
      }
    }
  })
  
  output$school_table <- renderDT({
    df <- school_results()
    datatable(df, options = list(pageLength = 10))
  })
  
  # ---- DATABASE MONITOR ----
  db_con <- reactiveVal(NULL)
  db_poll_data <- reactiveVal(tibble())
  
  observeEvent(input$db_connect, {
    # close existing connection if present
    if (!is.null(db_con())) {
      tryCatch(dbDisconnect(db_con()), error = function(e) NULL)
      db_con(NULL)
    }
    # create connection
    host <- input$db_host
    port <- input$db_port
    name <- input$db_name
    user <- input$db_user
    pw <- input$db_password
    type <- input$db_type
    
    conn <- NULL
    tryCatch({
      if (type == "Postgres") {
        conn <- dbConnect(RPostgres::Postgres(), dbname = name, host = host, port = port, user = user, password = pw)
      } else {
        conn <- dbConnect(RMySQL::MySQL(), dbname = name, host = host, port = port, user = user, password = pw)
      }
      db_con(conn)
      output$db_status <- renderText({
        paste("Connected to", type, "at", host, ":", port)
      })
      # perform initial query
      q <- input$db_query
      res <- dbGetQuery(conn, q)
      db_poll_data(as_tibble(res))
    }, error = function(e) {
      output$db_status <- renderText({ paste("Connection error:", e$message) })
      db_con(NULL)
    })
  })
  
  # Poll the DB on interval if connected
  observe({
    conn <- db_con()
    if (is.null(conn)) return()
    invalidateLater(ms = input$poll_seconds * 1000, session = session)
    isolate({
      q <- input$db_query
      tryCatch({
        res <- dbGetQuery(conn, q)
        db_poll_data(as_tibble(res))
        output$db_status <- renderText({ paste("Last poll:", Sys.time()) })
      }, error = function(e) {
        output$db_status <- renderText({ paste("Query error:", e$message) })
      })
    })
  })
  
  output$db_table <- renderDT({
    df <- db_poll_data()
    datatable(df, options = list(pageLength = 10))
  })
  
  # On session end: disconnect DB
  session$onSessionEnded(function() {
    if (!is.null(db_con())) tryCatch(dbDisconnect(db_con()), error = function(e) NULL)
  })
  
}

shinyApp(ui, server)
