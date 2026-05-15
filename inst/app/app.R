# ============================================================
# GitProfiler — Shiny Dashboard
# ============================================================

library(shiny)
library(bslib)
library(DBI)
library(duckdb)
library(plotly)
library(DT)
library(dplyr)
library(tidyr)
library(lubridate)

# ---------- Подключение пакета с функциями ----------
if (requireNamespace("gitTest", quietly = TRUE)) {
  library(gitTest)
}

# ---------- Вспомогательные функции ----------

open_con <- function(path = "git.duckdb") {
  DBI::dbConnect(duckdb::duckdb(), dbdir = path)
}

table_exists <- function(con, tbl) {
  tryCatch({
    tbls <- DBI::dbListTables(con)
    tbl %in% tbls
  }, error = function(e) FALSE)
}

table_has_data <- function(con, tbl) {
  if (!table_exists(con, tbl)) return(FALSE)
  tryCatch({
    count <- DBI::dbGetQuery(con, sprintf("SELECT COUNT(*) AS n FROM %s", tbl))$n
    return(count > 0)
  }, error = function(e) FALSE)
}

get_authors <- function(con) {
  if (!table_has_data(con, "git_commit_history")) return(character(0))
  tryCatch(
    DBI::dbGetQuery(con, "SELECT DISTINCT author_name FROM git_commit_history ORDER BY author_name")$author_name,
    error = function(e) character(0)
  )
}

get_repos <- function(con) {
  if (!table_has_data(con, "repo_path")) return(data.frame())
  tryCatch(
    DBI::dbGetQuery(con, "SELECT id, repo, path FROM repo_path ORDER BY id"),
    error = function(e) data.frame()
  )
}

# Функции-заглушки
if (!exists("get_developer_role")) {
  get_developer_role <- function(con, author_name) { return("Разработчик") }
}
if (!exists("get_developer_stats")) {
  get_developer_stats <- function(con, username = NULL, since = NULL, until = NULL, russian_names = FALSE, repo_id = NULL) {
    return(data.frame())
  }
}
if (!exists("get_commit_type_groups_ru")) {
  get_commit_type_groups_ru <- function(con, author_name) { return(data.frame()) }
}
if (!exists("get_commit_size_profile")) {
  get_commit_size_profile <- function(con, author_name, since = NULL, until = NULL, repo_id = NULL) {
    return(list(tiny = 0, small = 0, medium = 0, large = 0, avg_size = 0, median_size = 0))
  }
}
if (!exists("get_tech_stack")) {
  get_tech_stack <- function(con, author_name, repo_id = NULL) { return(data.frame()) }
}
if (!exists("get_user_anomalies")) {
  get_user_anomalies <- function(con, author_name) { return(data.frame()) }
}
if (!exists("get_all_anomalies")) {
  get_all_anomalies <- function(conn, username = NULL, limit = Inf, since = NULL, until = NULL, repo_id = NULL) {
    return(data.frame())
  }
}
if (!exists("get_summary_stats")) {
  get_summary_stats <- function(con) {
    return(list(overview = data.frame(total_developers = 0, total_commits = 0), top_5_developers = data.frame()))
  }
}

palette_violet <- c("#7c5cbf", "#9b7dd4", "#b89fe0", "#d4c5f0", "#5b3fa8", "#3b8dd4", "#5cb85c", "#f59040", "#e05252")

# ============================================================
# UI
# ============================================================
ui <- page_navbar(
  title = tags$span(tags$i(class = "ti ti-git-branch", style = "margin-right:6px"), "GitProfiler"),
  window_title = "GitProfiler | Анализ Git-репозиториев",
  theme = bs_theme(
    base_font = font_google("Inter"), bg = "#faf9fd", fg = "#2d2540",
    primary = "#7c5cbf", secondary = "#9b7dd4", success = "#5cb85c",
    warning = "#f59040", danger = "#e05252", "navbar-bg" = "#ffffff"
  ),
  header = tags$head(
    tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/@tabler/icons-webfont@latest/tabler-icons.min.css"),
    tags$style("
      .nav-link.active { color: #7c5cbf !important; font-weight: 500; }
      .card { border: 1px solid #e3daf0; border-radius: 12px; margin-bottom: 16px; }
      .metric-box { background:#fff; border:1px solid #e3daf0; border-radius:8px; padding:12px 16px; }
      .metric-label { font-size:10px; text-transform:uppercase; letter-spacing:.5px; color:#9e93b8; margin-bottom:4px; }
      .metric-value { font-size:22px; font-weight:500; color:#2d2540; }
      .metric-sub { font-size:11px; color:#9e93b8; margin-top:2px; }
      .dev-output-box { background:#f2eeff; border:1px solid #d4c5f0; border-radius:8px; padding:12px; font-size:13px; line-height:1.7; }
      .tech-tag { background:#f2eeff; color:#4a2f8a; border-radius:10px; padding:3px 10px; font-size:11px; display:inline-block; margin:2px; }
      .badge-role { background:#7c5cbf; color:white; font-size:10px; padding:4px 8px; border-radius:20px; }
      .scrollable-content { max-height: calc(100vh - 80px); overflow-y: auto; padding-right: 10px; }
      .no-data-message { text-align: center; padding: 40px; color: #9e93b8; }
      .delete-btn { background-color: #e05252; color: white; border: none; padding: 2px 8px; border-radius: 6px; font-size: 11px; cursor: pointer; }
      .delete-btn:hover { background-color: #c03939; }
      .selectize-input, .selectize-control { width: 100% !important; }
    ")
  ),
  
  tags$div(class = "scrollable-content",
           
           # ----- 1. Репозитории -----
           nav_panel(
             title = tagList(tags$i(class = "ti ti-database"), " Репозитории"),
             value = "repos",
             layout_columns(
               col_widths = 12,
               card(
                 card_header("Добавить репозиторий"),
                 radioButtons("repo_mode", "Режим:", choices = c("Локальный" = "local", "Удаленный (GitHub)" = "remote"), selected = "local", inline = TRUE),
                 conditionalPanel(
                   "input.repo_mode == 'local'",
                   layout_columns(
                     col_widths = c(9, 3),
                     textInput("local_path", NULL, placeholder = "D:/путь/к/репозиторию"),
                     actionButton("add_local", "Добавить в базу", icon = icon("plus"), class = "btn-primary w-100")
                   )
                 ),
                 conditionalPanel(
                   "input.repo_mode == 'remote'",
                   tags$div(
                     style = "margin-bottom: 10px; font-size: 12px; color: #9e93b8;",
                     "Введите URL репозитория GitHub (например: https://github.com/username/repo.git)"
                   ),
                   layout_columns(
                     col_widths = c(6, 4, 2),
                     textInput("remote_url", NULL, placeholder = "https://github.com/user/repo.git"),
                     textInput("remote_dir", NULL, value = "repos", placeholder = "Папка для клонирования"),
                     actionButton("add_remote", "Добавить", icon = icon("plus"), class = "btn-primary w-100")
                   )
                 ),
                 hr(),
                 layout_columns(
                   col_widths = c(4, 4, 4),
                   actionButton("load_demo", tagList(tags$i(class = "ti ti-database-import"), " Загрузить демо"), class = "btn-outline-secondary"),
                   actionButton("refresh_analytics", tagList(tags$i(class = "ti ti-refresh"), " Обновить аналитику"), class = "btn-outline-primary"),
                   actionButton("reset_db", tagList(tags$i(class = "ti ti-trash"), " Очистить БД"), class = "btn-outline-danger")
                 )
               ),
               card(
                 card_header(layout_columns(col_widths = c(8, 4), tagList("Список репозиториев ", uiOutput("repo_count_badge")), textInput("repo_search", NULL, placeholder = "Поиск...", width = "100%"))),
                 DTOutput("repo_table")
               )
             )
           ),
           
           # ----- 2. Анализ репозитория -----
           nav_panel(
             title = tagList(tags$i(class = "ti ti-folder"), " Анализ репозитория"),
             value = "repo_analysis",
             layout_columns(
               col_widths = c(3, 9),
               card(
                 card_header("Параметры"),
                 selectInput("sel_repo", "Репозиторий:", choices = c("Нет репозиториев" = ""), width = "100%"),
                 hr(),
                 uiOutput("repo_metrics"),
                 hr(),
                 uiOutput("repo_metadata_info")
               ),
               card(
                 navset_tab(
                   nav_panel("Вклад участников", plotlyOutput("plot_contributions", height = "300px")),
                   nav_panel("Типы изменений", plotlyOutput("plot_changes", height = "300px")),
                   nav_panel("Динамика коммитов", plotlyOutput("plot_dynamics", height = "300px")),
                   nav_panel("Активность по часам", plotlyOutput("plot_hours", height = "300px"))
                 )
               )
             )
           ),
           
           # ----- 3. Профиль разработчика -----
           nav_panel(
             title = tagList(tags$i(class = "ti ti-user"), " Профиль разработчика"),
             value = "profile",
             layout_columns(
               col_widths = 12,
               card(
                 layout_columns(
                   col_widths = c(3, 2, 2, 2, 3),
                   selectInput("sel_dev", "Разработчик:", choices = c("Выберите разработчика" = ""), width = "100%"),
                   dateInput("since", "От:", value = Sys.Date() - 365, width = "100%"),
                   dateInput("until", "До:", value = Sys.Date(), width = "100%"),
                   actionButton("apply_filters", "Применить фильтр", icon = icon("filter"), class = "btn-primary w-100"),
                   actionButton("gen_output", "Сформировать вывод", icon = icon("file-description"), class = "btn-outline-secondary w-100")
                 )
               )
             ),
             uiOutput("dev_header"),
             layout_columns(col_widths = c(6, 6), uiOutput("dev_metrics"), uiOutput("dev_hr_summary")),
             layout_columns(
               col_widths = c(6, 6),
               card(card_header("Типы коммитов"), plotlyOutput("plot_commit_types", height = "240px")),
               card(card_header("Размеры коммитов"), plotlyOutput("plot_commit_sizes", height = "240px"))
             ),
             card(card_header("Тепловая карта активности"), plotlyOutput("plot_dev_heatmap", height = "220px")),
             card(card_header("Технологический стек"), uiOutput("dev_stack")),
             card(card_header("Аномалии"), DTOutput("dev_anomalies_table")),
             uiOutput("dev_output_panel")
           ),
           
           # ----- 4. Общая статистика -----
           nav_panel(
             title = tagList(tags$i(class = "ti ti-building"), " Общая статистика"),
             value = "org",
             uiOutput("org_metrics"),
             layout_columns(
               col_widths = c(7, 5),
               card(card_header("Динамика активности"), plotlyOutput("plot_org_dynamics", height = "260px")),
               card(card_header("Топ разработчиков"), tags$p("по количеству коммитов", style = "font-size:11px;color:#9e93b8"), uiOutput("top_devs"))
             ),
             layout_columns(
               col_widths = c(6, 6),
               card(card_header("Коммиты по репозиториям"), plotlyOutput("plot_repos_bar", height = "220px")),
               card(card_header("Языки программирования"), plotlyOutput("plot_langs", height = "220px"))
             ),
             card(card_header("Тепловая карта активности организации"), plotlyOutput("plot_org_heatmap", height = "240px"))
           ),
           
           # ----- 5. Аномалии -----
           nav_panel(
             title = tagList(tags$i(class = "ti ti-alert-triangle"), " Аномалии"),
             value = "anomalies",
             layout_columns(
               col_widths = 12,
               card(
                 layout_columns(
                   col_widths = c(4, 4, 4),
                   selectInput("anom_dev_filter", "Разработчик:", choices = c("Все" = ""), width = "100%"),
                   selectInput("anom_type_filter", "Тип аномалии:",
                               choices = c("Все типы" = "", "Ночной коммит" = "night_commit", "В выходной" = "weekend_commit",
                                           "Большой коммит" = "large_commit", "Длинный перерыв" = "long_break",
                                           "Пустое сообщение" = "empty_message", "ML-аномалия" = "ml_anomaly"), width = "100%"),
                   div()
                 )
               ),
               card(card_header("Распределение по типам аномалий"), plotlyOutput("plot_anom_types", height = "240px")),
               card(card_header("Список аномалий"), DTOutput("anom_list_table"))
             )
           )
  )
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {
  
  con <- reactiveVal(NULL)
  db_ready <- reactiveVal(FALSE)
  
  refresh_ui <- function() {
    c <- con()
    if (!is.null(c)) {
      authors <- get_authors(c)
      repos_df <- get_repos(c)
      updateSelectInput(session, "sel_dev", choices = c("Выберите разработчика" = "", setNames(authors, authors)))
      updateSelectInput(session, "sel_repo", choices = c("Нет репозиториев" = "", setNames(repos_df$repo, repos_df$repo)))
      updateSelectInput(session, "anom_dev_filter", choices = c("Все" = "", setNames(authors, authors)))
      db_ready(table_has_data(c, "git_commit_history"))
    }
  }
  
  observe({
    tryCatch({
      c <- open_con("git.duckdb")
      con(c)
      refresh_ui()
    }, error = function(e) {
      showNotification(paste("Нет подключения к БД:", e$message), type = "warning", duration = 5)
    })
  })
  
  onStop(function() {
    c <- con()
    if (!is.null(c)) tryCatch(DBI::dbDisconnect(c, shutdown = TRUE), error = function(e) NULL)
  })
  
  # Добавление репозиториев
  observeEvent(input$add_local, {
    req(nchar(trimws(input$local_path)) > 0)
    withProgress(message = "Загрузка репозитория...", {
      if (exists("run_etl_pipeline")) {
        res <- run_etl_pipeline(mode = 0, local_path = trimws(input$local_path))
        if (res$status == "success") {
          showNotification(res$message, type = "message")
          refresh_ui()
        } else {
          showNotification(res$message, type = "error", duration = 8)
        }
      } else {
        showNotification("Функция run_etl_pipeline не найдена", type = "error")
      }
    })
  })
  
  observeEvent(input$add_remote, {
    req(nchar(trimws(input$remote_url)) > 0)
    withProgress(message = "Клонирование репозитория...", {
      if (exists("run_etl_pipeline")) {
        clone_dir <- if(nchar(trimws(input$remote_dir)) > 0) trimws(input$remote_dir) else "repos"
        if (!dir.exists(clone_dir)) dir.create(clone_dir, recursive = TRUE)
        res <- run_etl_pipeline(mode = 1, repo_url = trimws(input$remote_url), clone_dir = clone_dir)
        if (res$status == "success") {
          showNotification(res$message, type = "message")
          refresh_ui()
        } else {
          showNotification(res$message, type = "error", duration = 8)
        }
      } else {
        showNotification("Функция run_etl_pipeline не найдена", type = "error")
      }
    })
  })
  
  # Удаление репозитория
  observeEvent(input$delete_repo, {
    req(input$delete_repo)
    withProgress(message = "Удаление репозитория...", {
      if (exists("delete_repository")) {
        res <- delete_repository(input$delete_repo)
        if (res) {
          showNotification(paste("Репозиторий", input$delete_repo, "удален"), type = "message")
          refresh_ui()
        } else {
          showNotification("Ошибка при удалении", type = "error")
        }
      } else {
        showNotification("Функция delete_repository не найдена", type = "error")
      }
    })
  })
  
  # Сброс БД
  observeEvent(input$reset_db, {
    showModal(modalDialog(
      title = "Очистка базы данных",
      "Вы уверены? Все данные будут удалены безвозвратно.",
      footer = tagList(modalButton("Отмена"), actionButton("confirm_reset", "Да, очистить", class = "btn-danger"))
    ))
  })
  
  observeEvent(input$confirm_reset, {
    removeModal()
    withProgress(message = "Очистка базы данных...", {
      if (exists("reset_db")) {
        reset_db("git.duckdb")
      } else {
        if (file.exists("git.duckdb")) file.remove("git.duckdb")
        if (file.exists("git.duckdb.wal")) file.remove("git.duckdb.wal")
      }
      showNotification("База данных очищена", type = "message")
      c <- open_con("git.duckdb")
      con(c)
      refresh_ui()
    })
  })
  
  # Загрузка демо
  observeEvent(input$load_demo, {
    withProgress(message = "Загрузка демо-репозитория...", {
      tryCatch({
        if (exists("example_db")) {
          c <- example_db()
          showNotification("Демо-репозиторий подключен (только чтение)", type = "message")
          con(c)
          refresh_ui()
        } else {
          showNotification("Функция example_db не найдена", type = "error")
        }
      }, error = function(e) {
        showNotification(paste("Ошибка:", e$message), type = "error")
      })
    })
  })
  
  # Обновление аналитики
  observeEvent(input$refresh_analytics, {
    withProgress(message = "Обновление метрик...", {
      c <- con()
      if (!is.null(c) && table_has_data(c, "git_commit_history")) {
        if (exists("refresh_developer_metrics")) refresh_developer_metrics(c)
        if (exists("cache_anomalies")) cache_anomalies(c)
        showNotification("Аналитика обновлена", type = "message")
      } else {
        showNotification("Нет данных для обновления", type = "warning")
      }
    })
  })
  
  # Таблица репозиториев
  output$repo_count_badge <- renderUI({
    c <- con()
    req(!is.null(c))
    n <- nrow(get_repos(c))
    tags$span(n, class = "badge bg-primary rounded-pill")
  })
  
  output$repo_table <- renderDT({
    c <- con()
    req(!is.null(c))
    
    if (!table_has_data(c, "repo_path")) {
      return(datatable(data.frame(Сообщение = "Нет загруженных репозиториев"), colnames = "", options = list(dom = "t"), rownames = FALSE))
    }
    
    df <- DBI::dbGetQuery(c, "
      SELECT rp.id, rp.repo, COUNT(ch.commit) AS commits,
             rm.primary_language AS lang,
             MAX(CAST(ch.date AS DATE)) AS last_commit
      FROM repo_path rp
      LEFT JOIN git_commit_history ch ON rp.id = ch.repo_id
      LEFT JOIN repo_metadata rm ON rp.id = rm.repo_id
      GROUP BY rp.id, rp.repo, rm.primary_language
      ORDER BY rp.id
    ")
    req(nrow(df) > 0)
    
    q <- trimws(input$repo_search)
    if (nchar(q) > 0) {
      mask <- apply(df, 1, function(r) any(grepl(q, r, ignore.case = TRUE)))
      df <- df[mask, ]
    }
    
    df$delete <- sprintf('<button class="delete-btn" onclick="Shiny.setInputValue(\'delete_repo\', \'%s\', {priority: \'event\'})">Удалить</button>', df$repo)
    
    datatable(df[, c("repo", "commits", "lang", "last_commit", "delete")],
              colnames = c("Репозиторий", "Коммиты", "Язык", "Последний коммит", ""),
              rownames = FALSE, escape = FALSE,
              options = list(pageLength = 10, dom = "tp", columnDefs = list(list(className = "dt-center", targets = 4))),
              selection = "none")
  })
  
  # ========== АНАЛИЗ РЕПОЗИТОРИЯ ==========
  
  repo_analysis_data <- reactive({
    c <- con()
    req(!is.null(c), input$sel_repo, input$sel_repo != "")
    if (!table_has_data(c, "repo_path") || !table_has_data(c, "git_commit_history")) return(data.frame())
    repo_info <- DBI::dbGetQuery(c, sprintf("SELECT id FROM repo_path WHERE repo = '%s'", input$sel_repo))
    if (nrow(repo_info) == 0) return(data.frame())
    rid <- repo_info$id[1]
    DBI::dbGetQuery(c, sprintf("
      SELECT c.author_name, c.commit, c.date, COALESCE(SUM(fc.count_add), 0) AS added,
             COALESCE(SUM(fc.count_del), 0) AS deleted, EXTRACT(HOUR FROM c.date) AS hour,
             DATE_TRUNC('month', c.date) AS month
      FROM git_commit_history c
      LEFT JOIN git_file_changes fc ON c.commit = fc.commit AND c.repo_id = fc.repo_id
      WHERE c.repo_id = %d
      GROUP BY c.author_name, c.commit, c.date
    ", rid))
  })
  
  output$repo_metrics <- renderUI({
    df <- repo_analysis_data()
    if (nrow(df) == 0) return(tags$div(class = "no-data-message", "Выберите репозиторий для анализа"))
    commits <- length(unique(df$commit))
    devs <- length(unique(df$author_name))
    tagList(
      tags$p(tags$b("Статистика:"), style = "font-size:12px;color:#9e93b8;margin-bottom:10px"),
      tags$p(paste("Коммитов:", commits), style = "font-size:13px"),
      tags$p(paste("Разработчиков:", devs), style = "font-size:13px")
    )
  })
  
  output$repo_metadata_info <- renderUI({ NULL })
  
  output$plot_contributions <- renderPlotly({
    df <- repo_analysis_data()
    if (nrow(df) == 0) return(plotly_empty(type = "scatter") |> layout(title = "Нет данных"))
    agg <- aggregate(commit ~ author_name, data = df, FUN = function(x) length(unique(x)))
    names(agg) <- c("author_name", "commits")
    agg <- agg[order(-agg$commits), ]
    plot_ly(agg, x = ~author_name, y = ~commits, type = "bar", marker = list(color = "#7c5cbf")) |>
      layout(xaxis = list(title = "", tickangle = -30), yaxis = list(title = "Коммиты"), showlegend = FALSE)
  })
  
  output$plot_changes <- renderPlotly({
    df <- repo_analysis_data()
    if (nrow(df) == 0) return(plotly_empty(type = "scatter") |> layout(title = "Нет данных"))
    agg <- aggregate(cbind(added, deleted) ~ author_name, data = df, sum, na.rm = TRUE)
    agg <- agg[order(-agg$added), ]
    plot_ly() |>
      add_trace(data = agg, x = ~author_name, y = ~added, name = "Добавлено", type = "bar", marker = list(color = "#7c5cbf")) |>
      add_trace(data = agg, x = ~author_name, y = ~deleted, name = "Удалено", type = "bar", marker = list(color = "#f59040")) |>
      layout(barmode = "group", xaxis = list(title = "", tickangle = -30), yaxis = list(title = "Строки"), legend = list(orientation = "h", x = 0, y = 1.1))
  })
  
  output$plot_dynamics <- renderPlotly({
    df <- repo_analysis_data()
    if (nrow(df) == 0) return(plotly_empty(type = "scatter") |> layout(title = "Нет данных"))
    monthly <- df %>% group_by(month) %>% summarise(commits = n_distinct(commit), .groups = "drop") %>% filter(!is.na(month))
    if (nrow(monthly) == 0) return(plotly_empty(type = "scatter") |> layout(title = "Нет данных"))
    plot_ly(monthly, x = ~month, y = ~commits, type = "scatter", mode = "lines+markers", line = list(color = "#7c5cbf", width = 2), marker = list(color = "#7c5cbf")) |>
      layout(xaxis = list(title = ""), yaxis = list(title = "Коммиты"), showlegend = FALSE)
  })
  
  output$plot_hours <- renderPlotly({
    df <- repo_analysis_data()
    if (nrow(df) == 0) return(plotly_empty(type = "scatter") |> layout(title = "Нет данных"))
    hourly <- df %>% group_by(hour) %>% summarise(commits = n_distinct(commit), .groups = "drop") %>% filter(!is.na(hour))
    if (nrow(hourly) == 0) return(plotly_empty(type = "scatter") |> layout(title = "Нет данных"))
    plot_ly(hourly, x = ~hour, y = ~commits, type = "bar", marker = list(color = "#7c5cbf")) |>
      layout(xaxis = list(title = "Час суток", dtick = 2), yaxis = list(title = "Коммиты"), showlegend = FALSE)
  })
  
  # ========== ПРОФИЛЬ РАЗРАБОТЧИКА ==========
  
  dev_stats_data <- reactive({
    c <- con(); req(!is.null(c), input$sel_dev, input$sel_dev != ""); input$apply_filters
    if (!db_ready()) return(data.frame())
    tryCatch(get_developer_stats(c, username = input$sel_dev, since = as.character(input$since), until = as.character(input$until), russian_names = FALSE), error = function(e) data.frame())
  })
  
  dev_commit_types_data <- reactive({
    c <- con(); req(!is.null(c), input$sel_dev, input$sel_dev != ""); input$apply_filters
    if (!db_ready()) return(data.frame())
    tryCatch(get_commit_type_groups_ru(c, input$sel_dev), error = function(e) data.frame())
  })
  
  dev_size_profile_data <- reactive({
    c <- con(); req(!is.null(c), input$sel_dev, input$sel_dev != ""); input$apply_filters
    if (!db_ready()) return(list(tiny = 0, small = 0, medium = 0, large = 0, avg_size = 0, median_size = 0))
    tryCatch(get_commit_size_profile(c, input$sel_dev, since = as.character(input$since), until = as.character(input$until)), error = function(e) list(tiny = 0, small = 0, medium = 0, large = 0, avg_size = 0, median_size = 0))
  })
  
  dev_anomalies_data <- reactive({
    c <- con(); req(!is.null(c), input$sel_dev, input$sel_dev != ""); input$apply_filters
    if (!db_ready()) return(data.frame())
    tryCatch(get_user_anomalies(c, input$sel_dev), error = function(e) data.frame())
  })
  
  output$dev_header <- renderUI({
    if (!db_ready() || is.null(input$sel_dev) || input$sel_dev == "") return(card(tags$div(class = "no-data-message", "Выберите разработчика")))
    s <- dev_stats_data()
    if (nrow(s) == 0) return(card(tags$div(class = "no-data-message", "Нет данных по разработчику")))
    role <- tryCatch(get_developer_role(con(), input$sel_dev), error = function(e) "Разработчик")
    card(layout_columns(col_widths = c(1, 9, 2),
                        div(style = "width:48px;height:48px;border-radius:50%;background:#7c5cbf;display:flex;align-items:center;justify-content:center;color:#fff;font-size:16px;font-weight:500", toupper(substr(s$author_name[1], 1, 2))),
                        div(tags$h5(s$author_name[1], style = "margin:0"), tags$p(style = "font-size:12px;color:#9e93b8;margin:4px 0 6px", tags$i(class = "ti ti-code"), s$primary_language[1] %||% "—", " · ", tags$span(class = "badge-role", role))),
                        div(style = "text-align:right", tags$span(paste("Вклад:", if (!is.na(s$contribution_share[1])) round(s$contribution_share[1] * 100, 1) else 0, "%"), class = "badge bg-secondary"))
    ))
  })
  
  output$dev_metrics <- renderUI({
    if (!db_ready() || is.null(input$sel_dev) || input$sel_dev == "") return(card(tags$div(class = "no-data-message", "Нет данных")))
    s <- dev_stats_data()
    if (nrow(s) == 0) return(card(tags$div(class = "no-data-message", "Нет метрик")))
    metric <- function(label, value, sub = NULL) div(class = "metric-box", div(label, class = "metric-label"), div(value, class = "metric-value"), if (!is.null(sub)) div(sub, class = "metric-sub"))
    night_pct <- if (s$total_commits[1] > 0) round(s$night_commits[1] / s$total_commits[1] * 100) else 0
    weekend_pct <- if (s$total_commits[1] > 0) round(s$weekend_commits[1] / s$total_commits[1] * 100) else 0
    card(card_header("Метрики"),
         layout_columns(col_widths = c(3, 3, 3, 3), metric("Всего коммитов", s$total_commits[1]), metric("Активных дней", s$active_days[1]), metric("Средний размер", round(s$avg_commit_size[1], 1), "строк"), metric("Ночных коммитов", paste0(night_pct, "%"))),
         layout_columns(col_widths = c(3, 3, 3, 3), metric("Уникальных файлов", s$unique_files[1]), metric("Репозиториев", s$repos_count[1]), metric("Среднее время", if(!is.na(s$avg_commit_time[1])) s$avg_commit_time[1] else "--"), metric("Коммитов в выходные", paste0(weekend_pct, "%")))
    )
  })
  
  output$dev_hr_summary <- renderUI({
    if (!db_ready() || is.null(input$sel_dev) || input$sel_dev == "") return(card(tags$div(class = "no-data-message", "Нет данных")))
    s <- dev_stats_data()
    sp <- dev_size_profile_data()
    if (nrow(s) == 0) return(card(tags$div(class = "no-data-message", "Нет данных")))
    
    night_pct <- round(s$night_commits[1] / max(s$total_commits[1], 1) * 100)
    wkend_pct <- round(s$weekend_commits[1] / max(s$total_commits[1], 1) * 100)
    
    hr_row <- function(ok, title, detail) {
      icon_class <- if (ok) "ti-check text-success" else "ti-alert-triangle text-warning"
      div(style = "display:flex;align-items:flex-start;gap:8px;padding:7px 0;border-bottom:1px solid #e3daf0;font-size:12px",
          tags$i(class = icon_class, style = "margin-top:2px;font-size:14px"),
          div(tags$b(title, style = "font-size:12px"), tags$div(detail, style = "color:#9e93b8;font-size:11px")))
    }
    
    card(card_header("HR-сводка"),
         hr_row(night_pct < 20, "Рабочие часы", if (night_pct < 5) "Работает в дневное время" else if (night_pct < 20) paste0("Иногда работает ночью (", night_pct, "%)") else paste0("Часто работает по ночам (", night_pct, "%) - риск выгорания")),
         hr_row(wkend_pct < 20, "Баланс работы и отдыха", if (wkend_pct < 5) "Редко коммитит в выходные" else if (wkend_pct < 30) paste0("Иногда работает в выходные (", wkend_pct, "%)") else paste0("Часто работает в выходные (", wkend_pct, "%) - риск переработок")),
         hr_row(sp$avg_size < 300, "Качество коммитов", paste0("Средний размер: ", round(sp$avg_size, 0), " строк. ", if (sp$avg_size < 100) "Атомарные коммиты" else if (sp$avg_size < 300) "Умеренный размер" else "Крупные коммиты"))
    )
  })
  
  output$plot_commit_types <- renderPlotly({
    if (!db_ready() || is.null(input$sel_dev) || input$sel_dev == "") return(plotly_empty(type = "scatter") |> layout(title = "Нет данных"))
    df <- dev_commit_types_data()
    if (nrow(df) == 0) return(plotly_empty(type = "scatter") |> layout(title = "Нет данных о типах коммитов. Запустите классификацию."))
    plot_ly(df, x = ~n, y = ~reorder(group, n), type = "bar", orientation = "h", marker = list(color = "#7c5cbf"), text = ~paste0(percentage, "%"), textposition = "outside") |>
      layout(xaxis = list(title = "Количество коммитов"), yaxis = list(title = ""), showlegend = FALSE, margin = list(l = 120))
  })
  
  output$plot_commit_sizes <- renderPlotly({
    if (!db_ready() || is.null(input$sel_dev) || input$sel_dev == "") return(plotly_empty(type = "scatter") |> layout(title = "Нет данных"))
    sp <- dev_size_profile_data()
    df <- data.frame(label = c("Микро (<10)", "Малые (10-99)", "Средние (100-499)", "Крупные (>=500)"), n = c(sp$tiny, sp$small, sp$medium, sp$large))
    if (sum(df$n) == 0) return(plotly_empty(type = "scatter") |> layout(title = "Нет данных"))
    plot_ly(df, x = ~label, y = ~n, type = "bar", marker = list(color = c("#d4c5f0","#9b7dd4","#7c5cbf","#5b3fa8")), text = ~n, textposition = "auto") |>
      layout(xaxis = list(title = "", tickangle = -20), yaxis = list(title = "Коммитов"), showlegend = FALSE)
  })
  
  output$plot_dev_heatmap <- renderPlotly({
    if (!db_ready() || is.null(input$sel_dev) || input$sel_dev == "") return(plotly_empty(type = "scatter") |> layout(title = "Нет данных"))
    c <- con()
    df <- tryCatch(DBI::dbGetQuery(c, sprintf("SELECT EXTRACT(HOUR FROM date) AS hour, EXTRACT(DOW FROM date) AS dow, COUNT(*) AS n FROM git_commit_history WHERE author_name = '%s' AND date >= '%s' AND date <= '%s' GROUP BY hour, dow", input$sel_dev, input$since, input$until)), error = function(e) data.frame())
    if (nrow(df) == 0) return(plotly_empty(type = "scatter") |> layout(title = "Нет данных"))
    mat <- matrix(0, nrow = 7, ncol = 24)
    for (i in seq_len(nrow(df))) { r <- as.integer(df$dow[i]) + 1; k <- as.integer(df$hour[i]) + 1; if (r >= 1 && r <= 7 && k >= 1 && k <= 24) mat[r, k] <- df$n[i] }
    days_ru <- c("Вс","Пн","Вт","Ср","Чт","Пт","Сб")
    plot_ly(z = mat, x = as.character(0:23), y = days_ru, type = "heatmap", colorscale = list(c(0,"#f2eeff"), c(1,"#5b3fa8")), showscale = TRUE, colorbar = list(title = "коммитов", len = 0.6)) |>
      layout(xaxis = list(title = "Час суток", dtick = 3), yaxis = list(title = ""))
  })
  
  output$dev_stack <- renderUI({
    if (!db_ready() || is.null(input$sel_dev) || input$sel_dev == "") return(tags$div(class = "no-data-message", "Нет данных"))
    df <- tryCatch(get_tech_stack(con(), input$sel_dev), error = function(e) data.frame())
    if (nrow(df) == 0) return(tags$div(class = "no-data-message", "Технологический стек не определен"))
    tags$div(style = "display:flex;flex-wrap:wrap;gap:6px;margin-top:8px", lapply(seq_len(nrow(df)), function(i) tags$span(df$technology[i], class = "tech-tag")))
  })
  
  output$dev_anomalies_table <- renderDT({
    if (!db_ready() || is.null(input$sel_dev) || input$sel_dev == "") return(datatable(data.frame(Сообщение = "Нет данных"), options = list(dom = "t"), rownames = FALSE))
    df <- dev_anomalies_data()
    if (nrow(df) == 0) return(datatable(data.frame(Сообщение = "Аномалий не обнаружено"), options = list(dom = "t"), rownames = FALSE))
    cols <- intersect(c("anomaly_type", "description", "date"), names(df))
    if (length(cols) == 0) return(datatable(data.frame(Сообщение = "Данные об аномалиях недоступны")))
    df <- df[, cols, drop = FALSE]
    col_names <- c("Тип", "Описание", "Дата")[seq_along(cols)]
    datatable(df, rownames = FALSE, colnames = col_names, options = list(pageLength = 10, dom = "tp"))
  })
  
  # Аналитическое заключение (без эмодзи, большим текстом)
  dev_output_text <- eventReactive(input$gen_output, {
    if (!db_ready() || is.null(input$sel_dev) || input$sel_dev == "") return("Выберите разработчика для генерации аналитического заключения.")
    s <- dev_stats_data()
    if (nrow(s) == 0) return(paste("По разработчику", input$sel_dev, "нет данных за выбранный период."))
    
    ct <- dev_commit_types_data()
    sp <- dev_size_profile_data()
    role <- tryCatch(get_developer_role(con(), input$sel_dev), error = function(e) "разработчик")
    
    night_pct <- round(s$night_commits[1] / max(s$total_commits[1], 1) * 100)
    if (night_pct > 30) style_txt <- "предпочитает работать в ночное время (\"сова\")"
    else if (night_pct > 10) style_txt <- "иногда работает по ночам"
    else style_txt <- "работает преимущественно в дневные часы (\"жаворонок\")"
    
    main_type <- if (nrow(ct) > 0) ct$group[1] else "разнообразные изменения"
    
    if (sp$avg_size < 30) size_note <- "предпочитает атомарные коммиты (мелкие изменения)"
    else if (sp$avg_size < 100) size_note <- "использует коммиты умеренного размера"
    else size_note <- "склонен к крупным, объединённым изменениям"
    
    an <- dev_anomalies_data()
    if (is.null(an) || nrow(an) == 0) anom_note <- "Аномалий в поведении не выявлено — стабильный режим работы."
    else anom_note <- paste("Обнаружено", nrow(an), "аномалий, требующих внимания.")
    
    paste(
      s$author_name[1], "-", role, ".",
      "Специализация:", if (!is.na(s$primary_language[1])) s$primary_language[1] else "не определена",
      if (!is.na(s$secondary_language[1]) && nchar(s$secondary_language[1]) > 0) paste0(", также работает с", s$secondary_language[1]) else "", ".",
      "Активность:", s$total_commits[1], "коммитов за", s$active_days[1], "активных дней.",
      "Доля вклада в проектах составляет", if (!is.na(s$contribution_share[1])) round(s$contribution_share[1] * 100, 1) else 0, "%.",
      "Режим работы:", style_txt, ".",
      "Тип коммитов: преимущественно", main_type, ".", size_note, ".",
      anom_note
    )
  })
  
  output$dev_output_panel <- renderUI({
    txt <- dev_output_text()
    req(!is.null(txt))
    card(card_header("Аналитическое заключение"), div(class = "dev-output-box", style = "white-space:pre-wrap; font-size:13px; line-height:1.6;", HTML(gsub("\n", "<br>", txt))))
  })
  
  # ========== ОБЩАЯ СТАТИСТИКА ==========
  
  org_stats <- reactive({
    c <- con()
    if (!db_ready()) return(list(overview = data.frame(total_developers = 0, total_commits = 0), top_5_developers = data.frame()))
    tryCatch(get_summary_stats(c), error = function(e) list(overview = data.frame(total_developers = 0, total_commits = 0), top_5_developers = data.frame()))
  })
  
  output$org_metrics <- renderUI({
    if (!db_ready()) {
      return(layout_columns(col_widths = c(3, 3, 3, 3),
                            div(class = "metric-box", div("Всего коммитов", class = "metric-label"), div(0, class = "metric-value")),
                            div(class = "metric-box", div("Разработчиков", class = "metric-label"), div(0, class = "metric-value")),
                            div(class = "metric-box", div("Репозиториев", class = "metric-label"), div(0, class = "metric-value")),
                            div(class = "metric-box", div("Среднее/разраб", class = "metric-label"), div(0, class = "metric-value"))
      ))
    }
    sm <- org_stats()
    ov <- sm$overview
    metric_box <- function(label, val) div(class = "metric-box", div(label, class = "metric-label"), div(val, class = "metric-value"))
    layout_columns(col_widths = c(3, 3, 3, 3),
                   metric_box("Всего коммитов", if (!is.na(ov$total_commits[1])) format(ov$total_commits[1], big.mark = " ") else 0),
                   metric_box("Разработчиков", if (!is.na(ov$total_developers[1])) ov$total_developers[1] else 0),
                   metric_box("Репозиториев", tryCatch(nrow(get_repos(con())), error = function(e) 0)),
                   metric_box("Среднее/разраб", if (!is.na(ov$avg_commits_per_dev[1])) round(ov$avg_commits_per_dev[1], 1) else 0)
    )
  })
  
  output$top_devs <- renderUI({
    if (!db_ready()) return(tags$div(class = "no-data-message", "Нет данных"))
    sm <- org_stats()
    top <- sm$top_5_developers
    if (is.null(top) || nrow(top) == 0) return(tags$div(class = "no-data-message", "Нет данных о топ разработчиках"))
    tagList(lapply(seq_len(nrow(top)), function(i) {
      div(style = "display:flex;align-items:center;gap:10px;padding:8px 0;border-bottom:1px solid #e3daf0",
          tags$span(i, style = "font-size:11px;color:#9e93b8;width:16px"),
          div(style = "flex:1", tags$div(top$author_name[i], style = "font-size:12px;font-weight:500")),
          tags$span(format(top$total_commits[i], big.mark = " "), style = "font-size:13px;font-weight:500;color:#2d2540"))
    }))
  })
  
  output$plot_org_dynamics <- renderPlotly({
    if (!db_ready()) return(plotly_empty(type = "scatter") |> layout(title = "Нет данных"))
    c <- con()
    df <- tryCatch(DBI::dbGetQuery(c, "SELECT DATE_TRUNC('month', date) AS month, COUNT(*) AS commits FROM git_commit_history GROUP BY month ORDER BY month"), error = function(e) data.frame())
    if (nrow(df) == 0) return(plotly_empty(type = "scatter") |> layout(title = "Нет данных"))
    plot_ly(df, x = ~month, y = ~commits, type = "scatter", mode = "lines+markers", line = list(color = "#7c5cbf", width = 2), marker = list(color = "#7c5cbf"), fill = "tozeroy", fillcolor = "rgba(124,92,191,0.07)") |>
      layout(xaxis = list(title = ""), yaxis = list(title = "Коммиты"), showlegend = FALSE)
  })
  
  output$plot_repos_bar <- renderPlotly({
    if (!db_ready()) return(plotly_empty(type = "scatter") |> layout(title = "Нет данных"))
    c <- con()
    df <- tryCatch(DBI::dbGetQuery(c, "SELECT rp.repo, COUNT(ch.commit) AS commits FROM repo_path rp LEFT JOIN git_commit_history ch ON rp.id = ch.repo_id GROUP BY rp.repo ORDER BY commits DESC LIMIT 15"), error = function(e) data.frame())
    if (nrow(df) == 0) return(plotly_empty(type = "scatter") |> layout(title = "Нет данных"))
    plot_ly(df, x = ~reorder(repo, commits), y = ~commits, type = "bar", marker = list(color = "#7c5cbf"), text = ~commits, textposition = "auto") |>
      layout(xaxis = list(title = "", tickangle = -35), yaxis = list(title = "Коммиты"), showlegend = FALSE)
  })
  
  output$plot_langs <- renderPlotly({
    if (!db_ready()) return(plotly_empty(type = "scatter") |> layout(title = "Нет данных"))
    c <- con()
    df <- tryCatch(DBI::dbGetQuery(c, "SELECT primary_language AS lang, COUNT(*) AS n FROM developer_metrics WHERE primary_language IS NOT NULL AND primary_language != 'unknown' GROUP BY lang ORDER BY n DESC LIMIT 10"), error = function(e) data.frame())
    if (nrow(df) == 0) return(plotly_empty(type = "scatter") |> layout(title = "Нет данных"))
    plot_ly(df, x = ~n, y = ~reorder(lang, n), type = "bar", orientation = "h", marker = list(color = "#7c5cbf"), text = ~n, textposition = "outside") |>
      layout(xaxis = list(title = "Количество разработчиков"), yaxis = list(title = ""), showlegend = FALSE)
  })
  
  output$plot_org_heatmap <- renderPlotly({
    if (!db_ready()) return(plotly_empty(type = "scatter") |> layout(title = "Нет данных"))
    c <- con()
    df <- tryCatch(DBI::dbGetQuery(c, "SELECT EXTRACT(HOUR FROM date) AS hour, EXTRACT(DOW FROM date) AS dow, COUNT(*) AS n FROM git_commit_history GROUP BY hour, dow"), error = function(e) data.frame())
    if (nrow(df) == 0) return(plotly_empty(type = "scatter") |> layout(title = "Нет данных"))
    mat <- matrix(0, nrow = 7, ncol = 24)
    for (i in seq_len(nrow(df))) { r <- as.integer(df$dow[i]) + 1; k <- as.integer(df$hour[i]) + 1; if (r >= 1 && r <= 7 && k >= 1 && k <= 24) mat[r, k] <- df$n[i] }
    days_ru <- c("Вс","Пн","Вт","Ср","Чт","Пт","Сб")
    plot_ly(z = mat, x = as.character(0:23), y = days_ru, type = "heatmap", colorscale = list(c(0,"#f2eeff"), c(1,"#5b3fa8")), showscale = TRUE, colorbar = list(title = "коммитов", len = 0.7)) |>
      layout(xaxis = list(title = "Час суток", dtick = 3), yaxis = list(title = ""))
  })
  
  # ========== АНОМАЛИИ ==========
  
  all_anomalies <- reactive({
    c <- con()
    if (!db_ready()) return(data.frame())
    if (table_exists(c, "anomalies")) {
      tryCatch(DBI::dbGetQuery(c, "SELECT * FROM anomalies"), error = function(e) data.frame())
    } else {
      tryCatch(get_all_anomalies(c), error = function(e) data.frame())
    }
  })
  
  anom_stats_data <- reactive({
    df <- all_anomalies()
    if (nrow(df) == 0) return(data.frame())
    if ("anomaly_type" %in% names(df)) {
      agg <- as.data.frame(table(df$anomaly_type))
      names(agg) <- c("anomaly_type", "count")
      agg$percentage <- round(100 * agg$count / sum(agg$count), 1)
      agg
    } else data.frame()
  })
  
  output$plot_anom_types <- renderPlotly({
    if (!db_ready()) return(plotly_empty(type = "scatter") |> layout(title = "Нет данных"))
    df <- anom_stats_data()
    if (nrow(df) == 0) return(plotly_empty(type = "scatter") |> layout(title = "Аномалий не обнаружено"))
    labels_ru <- c(night_commit = "Ночные коммиты", weekend_commit = "Коммиты в выходные", large_commit = "Крупные коммиты", long_break = "Длинные перерывы", empty_message = "Пустые сообщения", ml_anomaly = "ML-аномалии")
    df$label <- ifelse(df$anomaly_type %in% names(labels_ru), labels_ru[df$anomaly_type], df$anomaly_type)
    plot_ly(df, x = ~reorder(label, -count), y = ~count, type = "bar", marker = list(color = "#7c5cbf"), text = ~paste0(percentage, "%"), textposition = "auto") |>
      layout(xaxis = list(title = "", tickangle = -20), yaxis = list(title = "Количество"), showlegend = FALSE)
  })
  
  output$anom_list_table <- renderDT({
    if (!db_ready()) return(datatable(data.frame(Сообщение = "Нет данных"), options = list(dom = "t"), rownames = FALSE))
    df <- all_anomalies()
    if (nrow(df) == 0) return(datatable(data.frame(Сообщение = "Аномалий не обнаружено"), options = list(dom = "t"), rownames = FALSE))
    
    dev_f <- input$anom_dev_filter
    type_f <- input$anom_type_filter
    if (nchar(dev_f) > 0 && "author_name" %in% names(df)) df <- df[df$author_name == dev_f, ]
    if (nchar(type_f) > 0 && "anomaly_type" %in% names(df)) df <- df[df$anomaly_type == type_f, ]
    if (nrow(df) == 0) return(datatable(data.frame(Сообщение = "Нет аномалий по выбранным фильтрам"), options = list(dom = "t"), rownames = FALSE))
    
    show_cols <- intersect(c("author_name", "anomaly_type", "description", "date"), names(df))
    df <- df[, show_cols, drop = FALSE]
    col_names <- c(author_name = "Разработчик", anomaly_type = "Тип", description = "Описание", date = "Дата")
    datatable(df, rownames = FALSE, colnames = col_names[show_cols], options = list(pageLength = 15, dom = "tp"))
  })
}

shinyApp(ui = ui, server = server)