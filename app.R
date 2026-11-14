
library(shiny)
library(plotly)
library(DT)
library(dplyr)
library(tidyr)
library(forecast)
library(randomForest)

source("C:/Users/anjal/OneDrive/Documents/Desktop/R project/helpers.R")

df <- load_data()
res <- holdout_eval(df, test_year = 2026)
acc <- accuracy_table(res)
best <- best_methods(acc)
fc <- forecast_all(df, best, h = 2)

languages <- sort(unique(df$language))
default_top <- c("Python", "JavaScript", "Java", "TypeScript", "Rust")

ui <- navbarPage(
  "Programming Language Trend Forecaster & Learning Advisor",
  
  tabPanel("Trends",
           sidebarLayout(
             sidebarPanel(
               selectInput("t_metric", "Metric",
                           choices = setNames(names(METRIC_LABELS), METRIC_LABELS),
                           selected = "github_repositories"
               ),
               selectInput("t_langs", "Languages",
                           choices = languages,
                           selected = default_top,
                           multiple = TRUE
               ),
               sliderInput("t_years", "Year range",
                           min = 2020, max = 2026,
                           value = c(2020, 2026), step = 1, sep = ""
               )
             ),
             mainPanel(plotlyOutput("trend_plot", height = "450px"))
           )
  ),
  
  tabPanel("Source Agreement",
           fluidRow(
             column(12,
                    h4("Do TIOBE and PYPL agree?"),
                    p("Spearman rank correlation per year (1 = identical ranking).")
             )
           ),
           fluidRow(
             column(6, plotlyOutput("agree_year")),
             column(6,
                    selectInput("a_year", "Year for scatter plot",
                                choices = 2020:2026, selected = 2026
                    ),
                    plotlyOutput("agree_scatter")
             )
           ),
           fluidRow(
             column(12,
                    h4("Languages where the two sources disagree most"),
                    p("Gap = TIOBE rank minus PYPL rank, averaged over 2020-2026. Positive = PYPL ranks the language higher."),
                    DTOutput("gap_table")
             )
           )
  ),
  
  tabPanel("Salary Relationships",
           sidebarLayout(
             sidebarPanel(
               selectInput("r_x", "Compare salary with",
                           choices = c(
                             "Learning difficulty" = "learning_difficulty",
                             "Demand score" = "demand_score",
                             "Performance score" = "performance_score"
                           )
               ),
               selectInput("r_year", "Year",
                           choices = c("All years", 2020:2026),
                           selected = 2026
               ),
               textOutput("r_text")
             ),
             mainPanel(plotlyOutput("rel_plot", height = "450px"))
           )
  ),
  
  tabPanel("Forecast",
           sidebarLayout(
             sidebarPanel(
               selectInput("f_lang", "Language",
                           choices = languages, selected = "Python"
               ),
               selectInput("f_metric", "Metric to forecast",
                           choices = setNames(FORECAST_METRICS, METRIC_LABELS[FORECAST_METRICS])
               ),
               textOutput("f_method_text"),
               hr(),
               p("Models were trained on 2020-2025 and tested on 2026. Lower error is better.")
             ),
             mainPanel(
               plotlyOutput("fc_plot", height = "380px"),
               h4("Model accuracy on the 2026 hold-out"),
               DTOutput("acc_table")
             )
           )
  ),
  
  tabPanel("Language Advisor",
           sidebarLayout(
             sidebarPanel(
               selectInput("g_goal", "I want to work in...",
                           choices = setNames(names(GOAL_LABELS), GOAL_LABELS)
               ),
               sliderInput("w_goal", "Importance: fit with my goal",
                           0, 1, 0.4, step = 0.1
               ),
               sliderInput("w_growth", "Importance: forecast growth",
                           0, 1, 0.3, step = 0.1
               ),
               sliderInput("w_salary", "Importance: salary",
                           0, 1, 0.3, step = 0.1
               ),
               sliderInput("g_diff", "Maximum learning difficulty (10 = hardest)",
                           1, 10, 10, step = 0.5
               ),
               sliderInput("g_min", "Minimum fit with goal (out of 100)",
                           0, 80, 30, step = 5
               )
             ),
             mainPanel(
               plotlyOutput("adv_plot", height = "320px"),
               DTOutput("adv_table")
             )
           )
  ),
  
  tabPanel("Data",
           DTOutput("data_table")
  )
)

server <- function(input, output, session) {
  
  output$trend_plot <- renderPlotly({
    req(input$t_langs)
    
    d <- df[
      df$language %in% input$t_langs &
        df$year >= input$t_years[1] &
        df$year <= input$t_years[2],
    ]
    
    p <- plot_ly(
      d,
      x = ~year,
      y = as.formula(paste0("~", input$t_metric)),
      color = ~language,
      type = "scatter",
      mode = "lines+markers"
    ) %>%
      layout(
        xaxis = list(title = "Year", dtick = 1),
        yaxis = list(title = METRIC_LABELS[[input$t_metric]])
      )
    
    if (input$t_metric %in% c("tiobe_rank", "pypl_rank")) {
      p <- layout(
        p,
        yaxis = list(
          title = METRIC_LABELS[[input$t_metric]],
          autorange = "reversed"
        )
      )
    }
    
    p
  })
  
  output$agree_year <- renderPlotly({
    a <- agreement_by_year(df)
    
    plot_ly(
      a,
      x = ~year,
      y = ~spearman,
      type = "scatter",
      mode = "lines+markers"
    ) %>%
      layout(
        xaxis = list(title = "Year", dtick = 1),
        yaxis = list(title = "Spearman correlation", range = c(0.9, 1))
      )
  })
  
  output$agree_scatter <- renderPlotly({
    d <- df[df$year == as.integer(input$a_year), ]
    
    plot_ly(
      d,
      x = ~tiobe_rank,
      y = ~pypl_rank,
      text = ~language,
      type = "scatter",
      mode = "markers+text",
      textposition = "top center"
    ) %>%
      layout(
        xaxis = list(title = "TIOBE rank"),
        yaxis = list(title = "PYPL rank")
      )
  })
  
  output$gap_table <- renderDT({
    g <- rank_gaps(df)
    
    g$mean_gap <- round(g$mean_gap, 2)
    g$mean_abs_gap <- round(g$mean_abs_gap, 2)
    
    datatable(g, rownames = FALSE, options = list(pageLength = 8))
  })
  
  rel_data <- reactive({
    if (input$r_year == "All years") {
      df
    } else {
      df[df$year == as.integer(input$r_year), ]
    }
  })
  
  output$rel_plot <- renderPlotly({
    d <- rel_data()
    
    plot_ly(
      d,
      x = as.formula(paste0("~", input$r_x)),
      y = ~average_salary_usd,
      text = ~language,
      type = "scatter",
      mode = "markers"
    ) %>%
      layout(
        xaxis = list(title = input$r_x),
        yaxis = list(title = "Average salary (USD)")
      )
  })
  
  output$r_text <- renderText({
    d <- rel_data()
    
    rho <- cor(
      d[[input$r_x]],
      d$average_salary_usd,
      method = "spearman"
    )
    
    paste0("Spearman correlation with salary: ", round(rho, 2))
  })
  
  output$f_method_text <- renderText({
    paste(
      "Method used for this metric:",
      best[[input$f_metric]],
      "(lowest hold-out error)"
    )
  })
  
  output$fc_plot <- renderPlotly({
    h <- df[
      df$language == input$f_lang,
      c("year", input$f_metric)
    ]
    
    names(h)[2] <- "value"
    
    f <- fc[
      fc$language == input$f_lang &
        fc$metric == input$f_metric,
      c("year", "value")
    ]
    
    f <- rbind(tail(h, 1), f)
    
    plot_ly() %>%
      add_trace(
        data = h,
        x = ~year,
        y = ~value,
        type = "scatter",
        mode = "lines+markers",
        name = "History"
      ) %>%
      add_trace(
        data = f,
        x = ~year,
        y = ~value,
        type = "scatter",
        mode = "lines+markers",
        name = "Forecast",
        line = list(dash = "dash")
      ) %>%
      layout(
        xaxis = list(title = "Year", dtick = 1),
        yaxis = list(title = METRIC_LABELS[[input$f_metric]])
      )
  })
  
  output$acc_table <- renderDT({
    a <- acc[
      acc$metric == input$f_metric,
      c("method", "MAE", "RMSE", "MAPE")
    ]
    
    a$MAE <- round(a$MAE, 1)
    a$RMSE <- round(a$RMSE, 1)
    a$MAPE <- round(a$MAPE, 2)
    
    names(a)[4] <- "MAPE (%)"
    
    datatable(a, rownames = FALSE, options = list(dom = "t"))
  })
  
  adv <- reactive({
    advisor_scores(
      df, fc, input$g_goal,
      input$w_goal, input$w_growth,
      input$w_salary, input$g_diff,
      input$g_min
    )
  })
  
  output$adv_plot <- renderPlotly({
    top <- head(adv(), 5)
    
    validate(
      need(
        nrow(top) > 0,
        "No language matches these filters. Lower the minimum fit or raise the difficulty limit."
      )
    )
    
    plot_ly(
      top,
      x = ~score,
      y = ~reorder(language, score),
      type = "bar",
      orientation = "h"
    ) %>%
      layout(
        xaxis = list(title = "Advisor score (0-100)"),
        yaxis = list(title = "")
      )
  })
  
  output$adv_table <- renderDT({
    top <- head(adv(), 5)
    
    validate(need(nrow(top) > 0, ""))
    
    t <- data.frame(
      Language = top$language,
      Score = round(top$score, 1),
      Why = top$reason
    )
    
    datatable(t, rownames = FALSE, options = list(dom = "t"))
  })
  
  output$data_table <- renderDT({
    datatable(
      df,
      options = list(scrollX = TRUE, pageLength = 15)
    )
  })
}

shinyApp(ui, server)
