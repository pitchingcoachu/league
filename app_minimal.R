# app_minimal.R
# Minimal League app: Leaderboard, Comparison Tool, League Tables, Heatmaps.

suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(DT)
  library(ggplot2)
  library(readr)
})

source("pitch_data_service.R")

`%||%` <- function(x, y) if (is.null(x) || !length(x)) y else x

pick_first_col <- function(df, candidates) {
  nms <- names(df)
  hit <- which(tolower(nms) %in% tolower(candidates))
  if (!length(hit)) return(NULL)
  nms[hit[[1]]]
}

read_local_pitch_data <- function(base_dir = "data") {
  if (!dir.exists(base_dir)) return(data.frame())
  csvs <- list.files(base_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
  csvs <- csvs[!grepl("video_map|manual|upload_sessions", tolower(basename(csvs)))]
  if (!length(csvs)) return(data.frame())

  parts <- lapply(csvs, function(path) {
    tryCatch(
      suppressMessages(readr::read_csv(path, show_col_types = FALSE, progress = FALSE)),
      error = function(e) NULL
    )
  })
  parts <- Filter(Negate(is.null), parts)
  if (!length(parts)) return(data.frame())
  dplyr::bind_rows(parts)
}

to_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

is_strike_call <- function(pitch_call) {
  pc <- tolower(trimws(as.character(pitch_call)))
  pc %in% c(
    "calledstrike", "foulball", "foul", "foulballnotfieldable",
    "foulballfieldable", "swingingstrike", "swingingstrikewiff",
    "inplay", "inplay, out", "inplay, run(s)", "inplay, no out"
  )
}

is_whiff_call <- function(pitch_call) {
  pc <- tolower(trimws(as.character(pitch_call)))
  pc %in% c("swingingstrike", "swingingstrikewiff")
}

build_pitch_dataset <- function() {
  backend_obj <- tryCatch(load_pitch_data_with_backend(), error = function(e) NULL)
  raw <- if (is.list(backend_obj) && is.data.frame(backend_obj$data)) {
    backend_obj$data
  } else {
    read_local_pitch_data("data")
  }
  if (!is.data.frame(raw) || !nrow(raw)) return(data.frame())

  pitcher_col <- pick_first_col(raw, c("Pitcher"))
  pitch_type_col <- pick_first_col(raw, c("TaggedPitchType", "AutoPitchType", "PitchType"))
  date_col <- pick_first_col(raw, c("Date", "session_date", "SessionDate"))
  batter_col <- pick_first_col(raw, c("Batter"))
  pitch_call_col <- pick_first_col(raw, c("PitchCall"))
  plate_x_col <- pick_first_col(raw, c("PlateLocSide", "PlateX"))
  plate_z_col <- pick_first_col(raw, c("PlateLocHeight", "PlateZ"))
  velo_col <- pick_first_col(raw, c("RelSpeed", "Velo", "Velocity"))
  ivb_col <- pick_first_col(raw, c("InducedVertBreak", "IVB"))
  hb_col <- pick_first_col(raw, c("HorzBreak", "HB"))

  out <- tibble::tibble(
    Date = as.character(if (!is.null(date_col)) raw[[date_col]] else NA_character_),
    Pitcher = as.character(if (!is.null(pitcher_col)) raw[[pitcher_col]] else NA_character_),
    Batter = as.character(if (!is.null(batter_col)) raw[[batter_col]] else NA_character_),
    TaggedPitchType = as.character(if (!is.null(pitch_type_col)) raw[[pitch_type_col]] else "Unknown"),
    PitchCall = as.character(if (!is.null(pitch_call_col)) raw[[pitch_call_col]] else NA_character_),
    PlateLocSide = to_num(if (!is.null(plate_x_col)) raw[[plate_x_col]] else NA_real_),
    PlateLocHeight = to_num(if (!is.null(plate_z_col)) raw[[plate_z_col]] else NA_real_),
    RelSpeed = to_num(if (!is.null(velo_col)) raw[[velo_col]] else NA_real_),
    InducedVertBreak = to_num(if (!is.null(ivb_col)) raw[[ivb_col]] else NA_real_),
    HorzBreak = to_num(if (!is.null(hb_col)) raw[[hb_col]] else NA_real_)
  )

  out <- out %>%
    mutate(
      DateParsed = suppressWarnings(as.Date(Date)),
      DateParsed = ifelse(is.na(DateParsed), suppressWarnings(as.Date(Date, format = "%m/%d/%Y")), DateParsed),
      DateParsed = as.Date(DateParsed, origin = "1970-01-01"),
      Pitcher = trimws(Pitcher),
      Batter = trimws(Batter),
      TaggedPitchType = ifelse(is.na(TaggedPitchType) | !nzchar(trimws(TaggedPitchType)), "Unknown", trimws(TaggedPitchType)),
      is_strike = is_strike_call(PitchCall),
      is_whiff = is_whiff_call(PitchCall)
    ) %>%
    filter(!is.na(Pitcher), nzchar(Pitcher))

  out
}

pitch_data <- build_pitch_dataset()
has_data <- nrow(pitch_data) > 0

all_pitchers <- if (has_data) sort(unique(pitch_data$Pitcher)) else "(No data)"
all_types <- if (has_data) sort(unique(pitch_data$TaggedPitchType)) else "Unknown"
date_min <- if (has_data) min(pitch_data$DateParsed, na.rm = TRUE) else Sys.Date() - 30
date_max <- if (has_data) max(pitch_data$DateParsed, na.rm = TRUE) else Sys.Date()
if (!is.finite(as.numeric(date_min)) || !is.finite(as.numeric(date_max))) {
  date_min <- Sys.Date() - 30
  date_max <- Sys.Date()
}

ui <- fluidPage(
  titlePanel("League Dashboard (Minimal)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("pitchers", "Pitcher", choices = all_pitchers, selected = all_pitchers, multiple = TRUE),
      selectInput("pitch_types", "Pitch Type", choices = all_types, selected = all_types, multiple = TRUE),
      dateRangeInput("date_range", "Date Range", start = date_min, end = date_max),
      width = 3
    ),
    mainPanel(
      if (!has_data) {
        tags$div(
          style = "margin-bottom:12px;padding:10px;border:1px solid #ddd;background:#fafafa;",
          "No pitch data is currently available. Check Neon credentials or local CSV files."
        )
      },
      tabsetPanel(
        tabPanel(
          "Leaderboard",
          selectInput("lb_metric", "Metric", choices = c("Pitch Count", "Avg Velo", "Max Velo", "Strike %", "Whiff %")),
          DTOutput("leaderboard_tbl")
        ),
        tabPanel(
          "Comparison Tool",
          fluidRow(
            column(4, selectInput("cmp_a", "Player A", choices = all_pitchers, selected = all_pitchers[[1]])),
            column(4, selectInput("cmp_b", "Player B", choices = all_pitchers, selected = all_pitchers[[min(2, length(all_pitchers))]])),
            column(4, selectInput("cmp_metric", "Metric", choices = c("RelSpeed", "InducedVertBreak", "HorzBreak"), selected = "RelSpeed"))
          ),
          plotOutput("comparison_plot", height = "360px"),
          DTOutput("comparison_summary")
        ),
        tabPanel(
          "League Data Tables",
          DTOutput("league_table")
        ),
        tabPanel(
          "Heatmaps",
          fluidRow(
            column(4, selectInput("hm_player", "Pitcher", choices = all_pitchers, selected = all_pitchers[[1]])),
            column(4, selectInput("hm_type", "Pitch Type", choices = c("All", all_types), selected = "All")),
            column(4, selectInput("hm_metric", "Heatmap Metric", choices = c("Pitch Frequency", "Avg Velo", "Strike %"), selected = "Pitch Frequency"))
          ),
          plotOutput("heatmap_plot", height = "480px")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  filtered <- reactive({
    req(has_data)
    req(input$pitchers, input$pitch_types, input$date_range)
    pitch_data %>%
      filter(
        Pitcher %in% input$pitchers,
        TaggedPitchType %in% input$pitch_types,
        is.na(DateParsed) | (DateParsed >= input$date_range[1] & DateParsed <= input$date_range[2])
      )
  })

  output$leaderboard_tbl <- renderDT({
    d <- filtered()
    req(nrow(d) > 0)
    summary <- d %>%
      group_by(Pitcher) %>%
      summarise(
        `Pitch Count` = n(),
        `Avg Velo` = round(mean(RelSpeed, na.rm = TRUE), 1),
        `Max Velo` = round(max(RelSpeed, na.rm = TRUE), 1),
        `Strike %` = round(100 * mean(is_strike, na.rm = TRUE), 1),
        `Whiff %` = round(100 * mean(is_whiff, na.rm = TRUE), 1),
        .groups = "drop"
      )

    metric <- input$lb_metric %||% "Pitch Count"
    summary <- summary %>% arrange(desc(.data[[metric]]))
    datatable(summary, rownames = FALSE, options = list(pageLength = 25))
  })

  cmp_data <- reactive({
    d <- filtered()
    d %>% filter(Pitcher %in% c(input$cmp_a, input$cmp_b))
  })

  output$comparison_plot <- renderPlot({
    d <- cmp_data()
    req(nrow(d) > 0, input$cmp_metric)
    ggplot(d, aes(x = Pitcher, y = .data[[input$cmp_metric]], fill = Pitcher)) +
      geom_boxplot(outlier.alpha = 0.2, na.rm = TRUE) +
      geom_jitter(width = 0.15, alpha = 0.15, size = 1, na.rm = TRUE) +
      labs(x = NULL, y = input$cmp_metric, title = paste("Comparison:", input$cmp_a, "vs", input$cmp_b)) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
  })

  output$comparison_summary <- renderDT({
    d <- cmp_data()
    req(nrow(d) > 0, input$cmp_metric)
    metric <- input$cmp_metric
    s <- d %>%
      group_by(Pitcher) %>%
      summarise(
        Pitches = n(),
        Mean = round(mean(.data[[metric]], na.rm = TRUE), 2),
        SD = round(sd(.data[[metric]], na.rm = TRUE), 2),
        P90 = round(stats::quantile(.data[[metric]], probs = 0.9, na.rm = TRUE, names = FALSE), 2),
        .groups = "drop"
      )
    datatable(s, rownames = FALSE, options = list(dom = "t"))
  })

  output$league_table <- renderDT({
    d <- filtered() %>%
      transmute(
        Date = DateParsed,
        Pitcher,
        Batter,
        `Pitch Type` = TaggedPitchType,
        Velo = round(RelSpeed, 1),
        IVB = round(InducedVertBreak, 1),
        HB = round(HorzBreak, 1),
        `Plate X` = round(PlateLocSide, 2),
        `Plate Z` = round(PlateLocHeight, 2),
        `Pitch Call` = PitchCall
      )
    datatable(d, rownames = FALSE, options = list(pageLength = 25, scrollX = TRUE))
  })

  output$heatmap_plot <- renderPlot({
    d <- filtered() %>%
      filter(Pitcher == input$hm_player)
    if (!identical(input$hm_type, "All")) {
      d <- d %>% filter(TaggedPitchType == input$hm_type)
    }
    d <- d %>% filter(is.finite(PlateLocSide), is.finite(PlateLocHeight))
    req(nrow(d) > 0)

    binned <- d %>%
      mutate(
        x_bin = floor(PlateLocSide / 0.25) * 0.25,
        y_bin = floor(PlateLocHeight / 0.25) * 0.25
      ) %>%
      group_by(x_bin, y_bin) %>%
      summarise(
        n = n(),
        avg_velo = mean(RelSpeed, na.rm = TRUE),
        strike_rate = 100 * mean(is_strike, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        value = dplyr::case_when(
          input$hm_metric == "Pitch Frequency" ~ n,
          input$hm_metric == "Avg Velo" ~ avg_velo,
          TRUE ~ strike_rate
        )
      )

    ggplot(binned, aes(x = x_bin, y = y_bin, fill = value)) +
      geom_tile(width = 0.25, height = 0.25) +
      scale_fill_viridis_c(option = "C", na.value = "grey90") +
      coord_fixed(xlim = c(-2.5, 2.5), ylim = c(0, 4.5)) +
      labs(
        title = paste(input$hm_metric, "-", input$hm_player, if (identical(input$hm_type, "All")) "(All Pitch Types)" else paste("(", input$hm_type, ")")),
        x = "PlateLocSide",
        y = "PlateLocHeight",
        fill = input$hm_metric
      ) +
      theme_minimal(base_size = 13)
  })
}

shinyApp(ui, server)
