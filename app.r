library(shiny)
library(bslib)
library(tidyverse)
library(ggplot2)
library(memoise) 
library(usmap)

# Load data and functions
source("load_data.r")

# ===== HELPER FUNCTIONS =====

aggregate_by_breakout <- function(qDf, cat_id, group_var = NULL) {
  
  if (nrow(qDf) == 0) {
    return(NULL)
  }
  
  if (is.null(group_var)) {
    plotDf <- qDf |>
      filter(BreakOutCategoryID == cat_id) |>
      select(Response, Sample_Size) |>
      na.omit() |>
      rename(persons = Sample_Size) |>
      group_by(Response) |>
      summarize(agg_persons = sum(persons), .groups = "drop")
    
    if (nrow(plotDf) == 0) {
      return(NULL)
    }
    
    total_ss <- sum(plotDf$agg_persons)
    
    plotDf <- plotDf |>
      mutate(
        agg_ss = total_ss,
        agg_percent = agg_persons * 100 / agg_ss,
        agg_percent_sdev = sqrt(agg_percent * (100 - agg_percent) / agg_ss),
        agg_low_ci_limit = agg_percent - 2 * agg_percent_sdev,
        agg_high_ci_limit = agg_percent + 2 * agg_percent_sdev
      ) |>
      select(Response, agg_percent, agg_low_ci_limit, agg_high_ci_limit)
    
  } else {
    plotDf <- qDf |>
      filter(BreakOutCategoryID == cat_id) |>
      select(all_of(group_var), Response, Sample_Size) |>
      na.omit() |>
      rename(persons = Sample_Size) |>
      group_by(across(all_of(c(group_var, "Response")))) |>
      summarize(agg_persons = sum(persons), .groups = "drop")
    
    if (nrow(plotDf) == 0) {
      return(NULL)
    }
    
    group_totals <- plotDf |>
      group_by(across(all_of(group_var))) |>
      summarize(agg_ss = sum(agg_persons), .groups = "drop")
    
    plotDf <- plotDf |>
      left_join(group_totals, by = group_var) |>
      mutate(
        agg_percent = agg_persons * 100 / agg_ss,
        agg_percent_sdev = sqrt(agg_percent * (100 - agg_percent) / agg_ss),
        agg_low_ci_limit = agg_percent - 2 * agg_percent_sdev,
        agg_high_ci_limit = agg_percent + 2 * agg_percent_sdev
      ) |>
      select(all_of(group_var), Response, agg_percent, agg_low_ci_limit, agg_high_ci_limit, agg_persons, agg_ss)
  }
  
  return(plotDf)
}

# Income aggregation helper
aggregate_income <- function(plotDf, level = "medium") {
  if (is.null(plotDf) || nrow(plotDf) == 0) return(NULL)
  
  if (level == "low") {
    plotDf <- plotDf |>
      mutate(Income_Group = case_when(
        grepl("Less than \\$15,000", Break_Out, ignore.case = TRUE) ~ "Low (≤$15K)",
        grepl("\\$50,000|\\$75,000 or more", Break_Out, ignore.case = TRUE) ~ "High (≥$50K)",
        grepl("\\$15,000|\\$25,000|\\$35,000", Break_Out, ignore.case = TRUE) ~ "Medium ($15K-$50K)",
        TRUE ~ "Other"
      )) |>
      filter(Income_Group != "Other") |>
      group_by(Income_Group, Response) |>
      summarize(agg_persons = sum(agg_persons), .groups = "drop")
    
    group_totals <- plotDf |>
      group_by(Income_Group) |>
      summarize(agg_ss = sum(agg_persons), .groups = "drop")
    
    plotDf <- plotDf |>
      left_join(group_totals, by = "Income_Group") |>
      mutate(
        agg_percent = agg_persons * 100 / agg_ss,
        agg_percent_sdev = sqrt(agg_percent * (100 - agg_percent) / agg_ss),
        agg_low_ci_limit = agg_percent - 2 * agg_percent_sdev,
        agg_high_ci_limit = agg_percent + 2 * agg_percent_sdev,
        Break_Out = Income_Group
      ) |>
      select(Break_Out, Response, agg_percent, agg_low_ci_limit, agg_high_ci_limit)
  } else {
    plotDf <- plotDf |>
      select(Break_Out, Response, agg_percent, agg_low_ci_limit, agg_high_ci_limit)
  }
  
  return(plotDf)
}

# Age aggregation helper
aggregate_age <- function(plotDf, level = "standard") {
  if (is.null(plotDf) || nrow(plotDf) == 0) return(NULL)
  
  if (level == "simple") {
    plotDf <- plotDf |>
      mutate(Age_Group = case_when(
        grepl("18-24|25-34", Break_Out) ~ "Young (<35)",
        grepl("65", Break_Out) ~ "Senior (65+)",
        TRUE ~ "Middle (35-64)"
      )) |>
      group_by(Age_Group, Response) |>
      summarize(agg_persons = sum(agg_persons), .groups = "drop")
    
    group_totals <- plotDf |>
      group_by(Age_Group) |>
      summarize(agg_ss = sum(agg_persons), .groups = "drop")
    
    plotDf <- plotDf |>
      left_join(group_totals, by = "Age_Group") |>
      mutate(
        agg_percent = agg_persons * 100 / agg_ss,
        agg_percent_sdev = sqrt(agg_percent * (100 - agg_percent) / agg_ss),
        agg_low_ci_limit = agg_percent - 2 * agg_percent_sdev,
        agg_high_ci_limit = agg_percent + 2 * agg_percent_sdev,
        Break_Out = Age_Group
      ) |>
      select(Break_Out, Response, agg_percent, agg_low_ci_limit, agg_high_ci_limit)
  } else {
    plotDf <- plotDf |>
      select(Break_Out, Response, agg_percent, agg_low_ci_limit, agg_high_ci_limit)
  }
  
  return(plotDf)
}

# Temporal aggregation helper
aggregate_temporal <- function(plotDf, level = "yearly") {
  if (is.null(plotDf) || nrow(plotDf) == 0) return(NULL)
  
  if (level == "period") {
    plotDf <- plotDf |>
      mutate(Period = case_when(
        Year >= 2011 & Year <= 2013 ~ "2011-2013",
        Year >= 2014 & Year <= 2016 ~ "2014-2016",
        Year >= 2017 & Year <= 2019 ~ "2017-2019",
        Year >= 2020 & Year <= 2022 ~ "2020-2022",
        TRUE ~ "2023"
      )) |>
      group_by(Period, Response) |>
      summarize(agg_persons = sum(agg_persons), .groups = "drop")
    
    group_totals <- plotDf |>
      group_by(Period) |>
      summarize(agg_ss = sum(agg_persons), .groups = "drop")
    
    plotDf <- plotDf |>
      left_join(group_totals, by = "Period") |>
      mutate(
        agg_percent = agg_persons * 100 / agg_ss,
        agg_percent_sdev = sqrt(agg_percent * (100 - agg_percent) / agg_ss),
        agg_low_ci_limit = agg_percent - 2 * agg_percent_sdev,
        agg_high_ci_limit = agg_percent + 2 * agg_percent_sdev,
        Year = Period
      ) |>
      select(Year, Response, agg_percent, agg_low_ci_limit, agg_high_ci_limit)
  } else {
    plotDf <- plotDf |>
      select(Year, Response, agg_percent, agg_low_ci_limit, agg_high_ci_limit)
  }
  
  return(plotDf)
}

# State/region mapping
state_to_region <- function(state) {
  northeast <- c("CT", "ME", "MA", "NH", "RI", "VT", "NJ", "NY", "PA")
  midwest <- c("IL", "IN", "MI", "OH", "WI", "IA", "KS", "MN", "MO", "NE", "ND", "SD")
  south <- c("DE", "FL", "GA", "MD", "NC", "SC", "VA", "WV", "AL", "KY", "MS", "TN", "AR", "LA", "OK", "TX", "DC")
  west <- c("AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY", "AK", "CA", "HI", "OR", "WA")
  
  case_when(
    state %in% northeast ~ "Northeast",
    state %in% midwest ~ "Midwest",
    state %in% south ~ "South",
    state %in% west ~ "West",
    TRUE ~ "Other"
  )
}

# State aggregation helper
aggregate_states <- function(plotDf, level = "states") {
  if (is.null(plotDf) || nrow(plotDf) == 0) return(NULL)
  
  if (level == "regions") {
    plotDf <- plotDf |>
      mutate(Region = state_to_region(Locationabbr)) |>
      group_by(Region, Response) |>
      summarize(agg_persons = sum(agg_persons), .groups = "drop")
    
    group_totals <- plotDf |>
      group_by(Region) |>
      summarize(agg_ss = sum(agg_persons), .groups = "drop")
    
    plotDf <- plotDf |>
      left_join(group_totals, by = "Region") |>
      mutate(
        agg_percent = agg_persons * 100 / agg_ss,
        agg_percent_sdev = sqrt(agg_percent * (100 - agg_percent) / agg_ss),
        agg_low_ci_limit = agg_percent - 2 * agg_percent_sdev,
        agg_high_ci_limit = agg_percent + 2 * agg_percent_sdev,
        Locationabbr = Region
      ) |>
      select(Locationabbr, Response, agg_percent, agg_low_ci_limit, agg_high_ci_limit)
  } else {
    plotDf <- plotDf |>
      select(Locationabbr, Response, agg_percent, agg_low_ci_limit, agg_high_ci_limit)
  }
  
  return(plotDf)
}

# ===== UI =====

ui <- page_fluid(
  
  tags$head(
    tags$style(HTML("
      .table-container table {
        font-size: 10px !important;
        margin-top: 5px;
      }
      .table-container th, .table-container td {
        padding: 3px 5px !important;
      }
      .form-group {
        margin-bottom: 5px !important;
      }
      label {
        font-size: 14px !important;
        margin-bottom: 2px !important;
        font-weight: 600;
      }
      .selectize-input {
        font-size: 14px !important;
        padding: 4px 8px !important;
        min-height: 32px !important;
        line-height: 24px !important;
      }
      .selectize-dropdown {
        font-size: 14px !important;
      }
      .selectize-dropdown-content {
        max-height: 200px !important;
      }
      .checkbox {
        font-size: 14px !important;
        margin-top: 0px !important;
        margin-bottom: 0px !important;
      }
      .checkbox label {
        padding-left: 20px !important;
        font-size: 14px !important;
      }
      .irs--shiny {
        height: 55px !important;
        margin-top: 10px !important;
      }
      .irs--shiny .irs-line {
        top: 32px !important;
        height: 4px !important;
      }
      .irs--shiny .irs-bar {
        top: 32px !important;
        height: 4px !important;
      }
      .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
        font-size: 12px !important;
        top: 10px !important;
        padding: 2px 4px !important;
      }
      .irs--shiny .irs-min, .irs--shiny .irs-max {
        font-size: 12px !important;
        top: 38px !important;
      }
      .irs--shiny .irs-handle {
        top: 26px !important;
        width: 18px !important;
        height: 18px !important;
      }
      select {
        height: 32px !important;
        padding: 4px 8px !important;
        font-size: 14px !important;
      }
      .shiny-input-container {
        margin-bottom: 3px !important;
      }
      .plot-clickable {
        cursor: pointer;
      }
      .plot-clickable:hover {
        opacity: 0.9;
      }
    "))
  ),
  
  # Header
  fluidRow(
    style = "background-color: #003366; color: white; padding: 10px; margin: 0px;",
    column(12, 
           h2("BRFSS Health Insights Dashboard", 
              style = "margin: 0px; color: white; font-weight: bold; text-align: center; font-size: 18px;")
    )
  ),
  
  # Filters
  fluidRow(
    style = "background-color: #f8f9fa; padding: 5px 15px 3px 15px; margin: 0px; border-bottom: 2px solid #dee2e6;",
    
    column(2, selectInput("class_select", "Class:", 
                          choices = c("(All)" = "", sort(unique(layerQ$Class))), selected = "", width = "100%")),
    column(2, selectInput("topic_select", "Topic:", choices = NULL, selected = NULL, width = "100%")),
    column(7, selectInput("question_select", "Question:", choices = NULL, selected = NULL, width = "100%")),
    column(1, div(style = "margin-top: 26px; text-align: center;",
                  checkboxInput("show_ci", "CI", value = TRUE))),
    
    column(4, div(style = "margin-top: 0px;",
                  sliderInput("year_range", "Year:", min = 2011, max = 2023, 
                              value = c(2011, 2023), step = 1, sep = "", width = "100%"))),
    column(8,
           fluidRow(
             column(3, selectizeInput("state_filter", "States:", 
                                      choices = sort(unique(df$Locationabbr[!df$Locationabbr %in% c("US", "UW")])),
                                      multiple = TRUE, options = list(placeholder = "All"), width = "100%")),
             column(2, selectInput("temporal_granularity", "Temporal:",
                                   choices = c("Year" = "yearly", "3-Yr" = "period"),
                                   selected = "yearly", width = "100%", selectize = FALSE)),
             column(2, selectInput("age_granularity", "Age:",
                                   choices = c("Std" = "standard", "Sim" = "simple"),
                                   selected = "standard", width = "100%", selectize = FALSE)),
             column(2, selectInput("income_granularity", "Income:", 
                                   choices = c("Std" = "medium", "Sim" = "low"),
                                   selected = "medium", width = "100%", selectize = FALSE)),
             column(3, selectInput("state_granularity", "Geography:",
                                   choices = c("States" = "states", "Regions" = "regions"),
                                   selected = "states", width = "100%", selectize = FALSE))
           ))
  ),
  
  # Selected Question
  fluidRow(
    style = "padding: 5px 15px; background-color: #e9ecef;",
    column(12, h5(textOutput("selected_question"), 
                  style = "color: #003366; font-weight: bold; margin: 2px 0px; font-size: 12px;"))
  ),
  
  # MAIN CONTENT
  fluidRow(
    style = "padding: 8px 15px 8px 15px;",
    
    # COLUMN 1
    column(4,
           div(class = "card", style = "height: 310px;",
               div(class = "card-header", style = "background-color: #003366; color: white; padding: 5px;",
                   h5("Overall Distribution", style = "margin: 0px; font-size: 12px;")),
               div(class = "card-body plot-clickable", style = "padding: 6px; overflow-y: auto;",
                   plotOutput("overallPlot", height = "230px", click = "overall_click"),
                   conditionalPanel(
                     condition = "input.show_ci == true",
                     div(hr(style = "margin: 3px 0px;"),
                         div(class = "table-container", style = "font-size: 9px;",
                             tableOutput("overall_ci_table")))))),
           
           div(class = "card", style = "margin-top: 8px; height: 310px;",
               div(class = "card-header", style = "background-color: #003366; color: white; padding: 5px;",
                   h5("By State/Territory", style = "margin: 0px; font-size: 12px;")),
               div(class = "card-body plot-clickable", style = "padding: 6px; overflow-y: auto;",
                   plotOutput("stateMapPlot", height = "230px", click = "state_click"),
                   conditionalPanel(
                     condition = "input.show_ci == true",
                     div(hr(style = "margin: 3px 0px;"),
                         div(class = "table-container", style = "font-size: 8px;",
                             tableOutput("state_ci_table"))))))
    ),
    
    # COLUMN 2
    column(4,
           div(class = "card", style = "height: 205px;",
               div(class = "card-header", style = "background-color: #003366; color: white; padding: 5px;",
                   h5("Temporal (by Year)", style = "margin: 0px; font-size: 12px;")),
               div(class = "card-body plot-clickable", style = "padding: 6px; overflow-y: auto;",
                   plotOutput("temporalPlot", height = "136px", click = "temporal_click"),
                   conditionalPanel(
                     condition = "input.show_ci == true",
                     div(hr(style = "margin: 3px 0px;"),
                         div(class = "table-container", style = "font-size: 8px;",
                             tableOutput("temporal_ci_table")))))),
           
           div(class = "card", style = "margin-top: 8px; height: 205px;",
               div(class = "card-header", style = "background-color: #003366; color: white; padding: 5px;",
                   h5("By Age Group", style = "margin: 0px; font-size: 12px;")),
               div(class = "card-body plot-clickable", style = "padding: 6px; overflow-y: auto;",
                   plotOutput("agePlot", height = "136px", click = "age_click"),
                   conditionalPanel(
                     condition = "input.show_ci == true",
                     div(hr(style = "margin: 3px 0px;"),
                         div(class = "table-container", style = "font-size: 8px;",
                             tableOutput("age_ci_table")))))),
           
           div(class = "card", style = "margin-top: 8px; height: 205px;",
               div(class = "card-header", style = "background-color: #003366; color: white; padding: 5px;",
                   h5("By Income Level", style = "margin: 0px; font-size: 12px;")),
               div(class = "card-body plot-clickable", style = "padding: 6px; overflow-y: auto;",
                   plotOutput("incomePlot", height = "136px", click = "income_click"),
                   conditionalPanel(
                     condition = "input.show_ci == true",
                     div(hr(style = "margin: 3px 0px;"),
                         div(class = "table-container", style = "font-size: 8px;",
                             tableOutput("income_ci_table"))))))
    ),
    
    # COLUMN 3
    column(4,
           div(class = "card", style = "height: 205px;",
               div(class = "card-header", style = "background-color: #003366; color: white; padding: 5px;",
                   h5("By Gender", style = "margin: 0px; font-size: 12px;")),
               div(class = "card-body plot-clickable", style = "padding: 6px; overflow-y: auto;",
                   plotOutput("genderPlot", height = "136px", click = "gender_click"),
                   conditionalPanel(
                     condition = "input.show_ci == true",
                     div(hr(style = "margin: 3px 0px;"),
                         div(class = "table-container", style = "font-size: 8px;",
                             tableOutput("gender_ci_table")))))),
           
           div(class = "card", style = "margin-top: 8px; height: 205px;",
               div(class = "card-header", style = "background-color: #003366; color: white; padding: 5px;",
                   h5("By Education", style = "margin: 0px; font-size: 12px;")),
               div(class = "card-body plot-clickable", style = "padding: 6px; overflow-y: auto;",
                   plotOutput("eduPlot", height = "136px", click = "edu_click"),
                   conditionalPanel(
                     condition = "input.show_ci == true",
                     div(hr(style = "margin: 3px 0px;"),
                         div(class = "table-container", style = "font-size: 8px;",
                             tableOutput("edu_ci_table")))))),
           
           div(class = "card", style = "margin-top: 8px; height: 205px;",
               div(class = "card-header", style = "background-color: #003366; color: white; padding: 5px;",
                   h5("By Race/Ethnicity", style = "margin: 0px; font-size: 12px;")),
               div(class = "card-body plot-clickable", style = "padding: 6px; overflow-y: auto;",
                   plotOutput("racePlot", height = "136px", click = "race_click"),
                   conditionalPanel(
                     condition = "input.show_ci == true",
                     div(hr(style = "margin: 3px 0px;"),
                         div(class = "table-container", style = "font-size: 8px;",
                             tableOutput("race_ci_table"))))))
    )
  )
)

# ===== SERVER =====

server <- function(input, output, session) {
  
  observe({
    if (input$class_select == "") {
      topics <- c("(All)" = "", sort(unique(layerQ$Topic)))
    } else {
      topics <- layerQ |>
        filter(Class == input$class_select) |>
        pull(Topic) |>
        unique() |>
        sort()
      topics <- c("(All)" = "", topics)
    }
    
    current_selection <- input$topic_select
    new_selection <- if (!is.null(current_selection) && current_selection %in% topics) {
      current_selection
    } else {
      ""
    }
    
    updateSelectInput(session, "topic_select", choices = topics, selected = new_selection)
  })
  
  observe({
    req(input$topic_select)
    
    if (input$class_select == "" && input$topic_select == "") {
      questions <- sort(unique(layerQ$Question))
    } else if (input$class_select == "") {
      questions <- layerQ |>
        filter(Topic == input$topic_select) |>
        pull(Question) |>
        unique() |>
        sort()
    } else if (input$topic_select == "") {
      questions <- layerQ |>
        filter(Class == input$class_select) |>
        pull(Question) |>
        unique() |>
        sort()
    } else {
      questions <- layerQ |>
        filter(Class == input$class_select, Topic == input$topic_select) |>
        pull(Question) |>
        unique() |>
        sort()
    }
    
    updateSelectInput(session, "question_select", choices = questions,
                      selected = if(length(questions) > 0) questions[1] else NULL)
  })
  
  observeEvent(session$clientData, {
    first_class <- sort(unique(layerQ$Class))[1]
    updateSelectInput(session, "class_select", selected = first_class)
  }, once = TRUE, ignoreNULL = TRUE, ignoreInit = FALSE)
  
  observeEvent(input$class_select, {
    if (input$class_select != "" && (is.null(input$topic_select) || input$topic_select == "")) {
      first_topic <- layerQ |>
        filter(Class == input$class_select) |>
        pull(Topic) |>
        unique() |>
        sort() |>
        head(1)
      
      if (length(first_topic) > 0) {
        updateSelectInput(session, "topic_select", selected = first_topic)
      }
    }
  })
  
  output$selected_question <- renderText({
    req(input$question_select)
    paste("Analyzing:", input$question_select)
  })
  
  qDf_reactive <- reactive({
    req(input$question_select)
    
    qDf <- df |>
      filter(Question == input$question_select) |>
      filter(!(Locationabbr %in% c("US", "UW"))) |>
      filter(Year >= input$year_range[1], Year <= input$year_range[2])
    
    if (!is.null(input$state_filter) && length(input$state_filter) > 0) {
      qDf <- qDf |> filter(Locationabbr %in% input$state_filter)
    }
    
    return(qDf)
  }) |> bindCache(input$question_select, input$year_range, input$state_filter)
  
  # ===== MAIN PLOTS =====
  
  output$overallPlot <- renderPlot({
    req(qDf_reactive())
    plotDf <- aggregate_by_breakout(qDf_reactive(), "CAT1", NULL)
    validate(need(!is.null(plotDf) && nrow(plotDf) > 0, "No data available"))
    
    plotDf <- plotDf |>
      arrange(desc(Response)) |>
      mutate(cumulative = cumsum(agg_percent), 
             pos = cumulative - agg_percent/2,
             label = paste0(round(agg_percent, 1), "%"))
    
    ggplot(plotDf, aes(x = 1, y = agg_percent, fill = Response)) +
      geom_col(position = "stack", width = 0.7, color = "white", size = 0.5) +
      geom_text(aes(y = pos, label = label), color = "white", fontface = "bold", size = 4.5) +
      coord_flip() +
      labs(x = NULL, y = "Percentage (%)", fill = "Response Options") +
      scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_text(face = "bold", size = 12),
            legend.text = element_text(size = 10), legend.key.size = unit(0.8, "cm"),
            axis.text.y = element_blank(), axis.ticks.y = element_blank(),
            axis.text.x = element_text(size = 10), panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank(), text = element_text(size = 11)) +
      guides(fill = guide_legend(nrow = 2, byrow = TRUE))
  })
  
  output$temporalPlot <- renderPlot({
  req(qDf_reactive(), input$temporal_granularity)
  plotDf <- aggregate_by_breakout(qDf_reactive(), "CAT1", "Year")
  validate(need(!is.null(plotDf) && nrow(plotDf) > 0, "No data available"))
  
  # Ensure Year is numeric and sorted BEFORE aggregation
  plotDf <- plotDf |>
    mutate(Year = as.numeric(as.character(Year))) |>
    arrange(Year)
  
  plotDf <- aggregate_temporal(plotDf, input$temporal_granularity)
  
  plotDf <- plotDf |>
    mutate(Year_Short = case_when(
      grepl("2011-2013", Year) ~ "'11-'13",
      grepl("2014-2016", Year) ~ "'14-'16",
      grepl("2017-2019", Year) ~ "'17-'19",
      grepl("2020-2022", Year) ~ "'20-'22",
      Year == "2023" ~ "'23",
      TRUE ~ as.character(Year)
    )) |>
    # Create proper ordering factor
    mutate(Year_Short = factor(Year_Short, levels = unique(Year_Short))) |>
    group_by(Year_Short) |>
    arrange(desc(Response)) |>
    mutate(cumulative = cumsum(agg_percent),
           pos = cumulative - agg_percent/2,
           label = ifelse(agg_percent > 5, paste0(round(agg_percent, 1), "%"), "")) |>
    ungroup()
  
  ggplot(plotDf, aes(x = Year_Short, y = agg_percent, fill = Response)) +
    geom_col(position = "stack") +
    geom_text(aes(y = pos, label = label), color = "white", fontface = "bold", size = 3) +
    labs(x = "Year", y = "Percentage (%)") +
    scale_y_continuous(limits = c(0, 100)) +
    theme_minimal() +
    theme(legend.position = "none", text = element_text(size = 11),
          axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9, face = "bold"),
          axis.text.y = element_text(size = 10), axis.title = element_text(size = 11),
          axis.title.x = element_text(margin = margin(t = 5)),
          panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
          plot.margin = margin(5, 5, 2, 5))
})
  output$genderPlot <- renderPlot({
    req(qDf_reactive())
    plotDf <- aggregate_by_breakout(qDf_reactive(), "CAT2", "Break_Out")
    validate(need(!is.null(plotDf) && nrow(plotDf) > 0, "No data available"))
    plotDf <- plotDf |> select(Break_Out, Response, agg_percent, agg_low_ci_limit, agg_high_ci_limit)
    
    plotDf <- plotDf |>
      group_by(Break_Out) |>
      arrange(desc(Response)) |>
      mutate(cumulative = cumsum(agg_percent),
             pos = cumulative - agg_percent/2,
             label = ifelse(agg_percent > 5, paste0(round(agg_percent, 1), "%"), "")) |>
      ungroup()
    
    ggplot(plotDf, aes(x = Break_Out, y = agg_percent, fill = Response)) +
      geom_col(position = "stack") +
      geom_text(aes(y = pos, label = label), color = "white", fontface = "bold", size = 3) +
      labs(x = "Gender", y = "Percentage (%)") +
      scale_y_continuous(limits = c(0, 100)) +
      theme_minimal() +
      theme(legend.position = "none", text = element_text(size = 11),
            axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9, face = "bold"),
            axis.text.y = element_text(size = 10), axis.title = element_text(size = 11),
            axis.title.x = element_text(margin = margin(t = 5)),
            panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
            plot.margin = margin(5, 5, 2, 5))
  })
  
  output$racePlot <- renderPlot({
    req(qDf_reactive())
    plotDf <- aggregate_by_breakout(qDf_reactive(), "CAT4", "Break_Out")
    validate(need(!is.null(plotDf) && nrow(plotDf) > 0, "No data available"))
    plotDf <- plotDf |> select(Break_Out, Response, agg_percent, agg_low_ci_limit, agg_high_ci_limit)
    
    plotDf <- plotDf |>
      mutate(Break_Out_Short = case_when(
        grepl("American Indian", Break_Out, ignore.case = TRUE) ~ "AI/AN",
        grepl("Asian", Break_Out, ignore.case = TRUE) ~ "Asian",
        grepl("Black", Break_Out, ignore.case = TRUE) ~ "Black",
        grepl("Hispanic", Break_Out, ignore.case = TRUE) ~ "Hispanic",
        grepl("Multiracial", Break_Out, ignore.case = TRUE) ~ "Multiracial",
        grepl("White", Break_Out, ignore.case = TRUE) ~ "White",
        grepl("Other", Break_Out, ignore.case = TRUE) ~ "Other",
        TRUE ~ Break_Out
      )) |>
      group_by(Break_Out_Short) |>
      arrange(desc(Response)) |>
      mutate(cumulative = cumsum(agg_percent),
             pos = cumulative - agg_percent/2,
             label = ifelse(agg_percent > 5, paste0(round(agg_percent, 1), "%"), "")) |>
      ungroup()
    
    ggplot(plotDf, aes(x = Break_Out_Short, y = agg_percent, fill = Response)) +
      geom_col(position = "stack") +
      geom_text(aes(y = pos, label = label), color = "white", fontface = "bold", size = 3) +
      labs(x = "Race/Ethnicity", y = "Percentage (%)") +
      scale_y_continuous(limits = c(0, 100)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9, face = "bold"),
            axis.text.y = element_text(size = 10), 
            axis.title = element_text(size = 11),
            axis.title.x = element_text(margin = margin(t = 5)),
            legend.position = "none", 
            text = element_text(size = 11),
            panel.grid.minor = element_blank(), 
            panel.grid.major.x = element_blank(),
            plot.margin = margin(5, 5, 2, 5))
  })
  
  output$agePlot <- renderPlot({
    req(qDf_reactive(), input$age_granularity)
    plotDf <- aggregate_by_breakout(qDf_reactive(), "CAT3", "Break_Out")
    validate(need(!is.null(plotDf) && nrow(plotDf) > 0, "No data available"))
    plotDf <- aggregate_age(plotDf, input$age_granularity)
    
    plotDf <- plotDf |>
      mutate(Break_Out_Short = case_when(
        grepl("18.*24", Break_Out, ignore.case = TRUE) ~ "18-24",
        grepl("25.*34", Break_Out, ignore.case = TRUE) ~ "25-34",
        grepl("35.*44", Break_Out, ignore.case = TRUE) ~ "35-44",
        grepl("45.*54", Break_Out, ignore.case = TRUE) ~ "45-54",
        grepl("55.*64", Break_Out, ignore.case = TRUE) ~ "55-64",
        grepl("65.*older|65 or older|65\\+", Break_Out, ignore.case = TRUE) ~ "65+",
        grepl("Young.*35", Break_Out, ignore.case = TRUE) ~ "< 35",
        grepl("Middle.*35.*64", Break_Out, ignore.case = TRUE) ~ "35-64",
        grepl("Senior.*65", Break_Out, ignore.case = TRUE) ~ "65+",
        TRUE ~ Break_Out
      )) |>
      group_by(Break_Out_Short) |>
      arrange(desc(Response)) |>
      mutate(cumulative = cumsum(agg_percent),
             pos = cumulative - agg_percent/2,
             label = ifelse(agg_percent > 5, paste0(round(agg_percent, 1), "%"), "")) |>
      ungroup()
    
    ggplot(plotDf, aes(x = Break_Out_Short, y = agg_percent, fill = Response)) +
      geom_col(position = "stack") +
      geom_text(aes(y = pos, label = label), color = "white", fontface = "bold", size = 3) +
      labs(x = "Age Group", y = "Percentage (%)") +
      scale_y_continuous(limits = c(0, 100)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9, face = "bold"),
            axis.text.y = element_text(size = 10), 
            axis.title = element_text(size = 11),
            axis.title.x = element_text(margin = margin(t = 5)),
            legend.position = "none", 
            text = element_text(size = 11),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            plot.margin = margin(5, 5, 2, 5))
  })
  
  output$eduPlot <- renderPlot({
    req(qDf_reactive())
    plotDf <- aggregate_by_breakout(qDf_reactive(), "CAT5", "Break_Out")
    validate(need(!is.null(plotDf) && nrow(plotDf) > 0, "No data available"))
    plotDf <- plotDf |> select(Break_Out, Response, agg_percent, agg_low_ci_limit, agg_high_ci_limit)
    
    plotDf <- plotDf |>
      mutate(Break_Out_Short = case_when(
        grepl("Did not graduate", Break_Out, ignore.case = TRUE) ~ "< HS",
        grepl("less than high school", Break_Out, ignore.case = TRUE) ~ "< HS",
        grepl("Graduated high school", Break_Out, ignore.case = TRUE) ~ "HS Grad",
        grepl("high school graduate", Break_Out, ignore.case = TRUE) ~ "HS Grad",
        grepl("Attended college", Break_Out, ignore.case = TRUE) ~ "Some College",
        grepl("some college", Break_Out, ignore.case = TRUE) ~ "Some College",
        grepl("Graduated from college", Break_Out, ignore.case = TRUE) ~ "College Grad",
        grepl("college graduate", Break_Out, ignore.case = TRUE) ~ "College Grad",
        TRUE ~ Break_Out
      )) |>
      group_by(Break_Out_Short) |>
      arrange(desc(Response)) |>
      mutate(cumulative = cumsum(agg_percent),
             pos = cumulative - agg_percent/2,
             label = ifelse(agg_percent > 5, paste0(round(agg_percent, 1), "%"), "")) |>
      ungroup()
    
    ggplot(plotDf, aes(x = Break_Out_Short, y = agg_percent, fill = Response)) +
      geom_col(position = "stack") +
      geom_text(aes(y = pos, label = label), color = "white", fontface = "bold", size = 3) +
      labs(x = "Education Level", y = "Percentage (%)") +
      scale_y_continuous(limits = c(0, 100)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9, face = "bold"),
            axis.text.y = element_text(size = 10), 
            axis.title = element_text(size = 11),
            axis.title.x = element_text(margin = margin(t = 5)),
            legend.position = "none", 
            text = element_text(size = 11),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            plot.margin = margin(5, 5, 2, 5))
  })
  
  output$incomePlot <- renderPlot({
    req(qDf_reactive(), input$income_granularity)
    plotDf <- aggregate_by_breakout(qDf_reactive(), "CAT6", "Break_Out")
    validate(need(!is.null(plotDf) && nrow(plotDf) > 0, "No data available"))
    plotDf <- aggregate_income(plotDf, input$income_granularity)
    
    plotDf <- plotDf |>
      mutate(Break_Out_Short = case_when(
        grepl("Less than.*15", Break_Out, ignore.case = TRUE) ~ "< $15K",
        grepl("15,000.*less than.*25", Break_Out, ignore.case = TRUE) ~ "$15-25K",
        grepl("25,000.*less than.*35", Break_Out, ignore.case = TRUE) ~ "$25-35K",
        grepl("35,000.*less than.*50", Break_Out, ignore.case = TRUE) ~ "$35-50K",
        grepl("50,000.*less than.*75", Break_Out, ignore.case = TRUE) ~ "$50-75K",
        grepl("75,000 or more", Break_Out, ignore.case = TRUE) ~ "≥ $75K",
        grepl("Low.*15K", Break_Out, ignore.case = TRUE) ~ "Low",
        grepl("Medium.*15K.*50K", Break_Out, ignore.case = TRUE) ~ "Medium",
        grepl("High.*50K", Break_Out, ignore.case = TRUE) ~ "High",
        TRUE ~ Break_Out
      )) |>
      group_by(Break_Out_Short) |>
      arrange(desc(Response)) |>
      mutate(cumulative = cumsum(agg_percent),
             pos = cumulative - agg_percent/2,
             label = ifelse(agg_percent > 5, paste0(round(agg_percent, 1), "%"), "")) |>
      ungroup()
    
    ggplot(plotDf, aes(x = Break_Out_Short, y = agg_percent, fill = Response)) +
      geom_col(position = "stack") +
      geom_text(aes(y = pos, label = label), color = "white", fontface = "bold", size = 3) +
      labs(x = "Income Level", y = "Percentage (%)") +
      scale_y_continuous(limits = c(0, 100)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9, face = "bold"),
            axis.text.y = element_text(size = 10), 
            axis.title = element_text(size = 11),
            axis.title.x = element_text(margin = margin(t = 5)),
            legend.position = "none", 
            text = element_text(size = 11),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            plot.margin = margin(5, 5, 2, 5))
  })
  
  output$stateMapPlot <- renderPlot({
    req(qDf_reactive(), input$state_granularity)
    
    plotDf <- aggregate_by_breakout(qDf_reactive(), "CAT1", "Locationabbr")
    validate(need(!is.null(plotDf) && nrow(plotDf) > 0, "No data available"))
    plotDf <- aggregate_states(plotDf, input$state_granularity)
    
    available_responses <- unique(plotDf$Response)
    if ("Yes" %in% available_responses) {
      selected_response <- "Yes"
      plotDf <- plotDf |> filter(Response == "Yes")
    } else if ("yes" %in% available_responses) {
      selected_response <- "yes"
      plotDf <- plotDf |> filter(Response == "yes")
    } else {
      selected_response <- sort(available_responses)[1]
      plotDf <- plotDf |> filter(Response == selected_response)
    }
    
    if (input$state_granularity == "regions") {
      ggplot(plotDf, aes(x = reorder(Locationabbr, -agg_percent), y = agg_percent, fill = reorder(Locationabbr, -agg_percent))) +
        geom_col() +
        geom_text(aes(label = paste0(round(agg_percent, 1), "%")), vjust = -0.5, size = 4, fontface = "bold") +
        labs(x = "Region", y = paste0(selected_response, " (%)"),
             title = paste("Geographic Distribution:", selected_response)) +
        scale_y_continuous(limits = c(0, max(plotDf$agg_percent) * 1.15), expand = c(0, 0)) +
        scale_fill_manual(values = c("#26A69A", "#4DB6AC", "#80CBC4", "#B2DFDB", "#A5D6A7")) +
        theme_minimal() +
        theme(legend.position = "none", axis.text.x = element_text(size = 11, face = "bold"),
              axis.text.y = element_text(size = 10), axis.title = element_text(size = 11, face = "bold"),
              axis.title.x = element_text(margin = margin(t = 10)),
              axis.title.y = element_text(margin = margin(r = 10)),
              plot.title = element_text(size = 13, face = "bold", hjust = 0.5, margin = margin(b = 10)),
              panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
              plot.margin = margin(10, 10, 10, 10))
    } else {
      mapData <- plotDf |> rename(state = Locationabbr, value = agg_percent)
      midpoint_value <- mean(mapData$value, na.rm = TRUE)
      
      plot_usmap(data = mapData, values = "value", color = "white", size = 0.3) +
        scale_fill_gradient2(
          low = "#fee5d9", mid = "#fc8d59", high = "#d7301f",
          midpoint = midpoint_value, name = paste0(selected_response, " (%)"),
          limits = c(min(mapData$value, na.rm = TRUE), max(mapData$value, na.rm = TRUE)),
          guide = guide_colorbar(barwidth = 15, barheight = 0.8,
                                 title.position = "top", title.hjust = 0.5)
        ) +
        labs(title = paste("Geographic Distribution:", selected_response)) +
        theme_void() +
        theme(legend.position = "bottom", 
              legend.title = element_text(size = 11, face = "bold"),
              legend.text = element_text(size = 10),
              plot.title = element_text(size = 13, face = "bold", hjust = 0.5, margin = margin(b = 5, t = 0)),
              plot.margin = margin(0, 0, 5, 0))
    }
  })
  
  # ===== MODAL PLOTS (ENLARGED) =====
  
  observeEvent(input$overall_click, {
    showModal(modalDialog(
      title = "Overall Distribution - Enlarged View",
      plotOutput("overallPlot_modal", height = "500px"),
      size = "l", easyClose = TRUE, footer = modalButton("Close")
    ))
  })
  
  output$overallPlot_modal <- renderPlot({
    req(qDf_reactive())
    plotDf <- aggregate_by_breakout(qDf_reactive(), "CAT1", NULL)
    validate(need(!is.null(plotDf) && nrow(plotDf) > 0, "No data available"))
    
    plotDf <- plotDf |> arrange(desc(Response)) |>
      mutate(cumulative = cumsum(agg_percent), pos = cumulative - agg_percent/2,
             label = paste0(round(agg_percent, 1), "%"))
    
    ggplot(plotDf, aes(x = 1, y = agg_percent, fill = Response)) +
      geom_col(position = "stack", width = 0.7, color = "white", size = 0.5) +
      geom_text(aes(y = pos, label = label), color = "white", fontface = "bold", size = 7) +
      coord_flip() + labs(x = NULL, y = "Percentage (%)", fill = "Response") +
      scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_text(face = "bold", size = 16),
            legend.text = element_text(size = 14, face = "bold"),
            legend.key.size = unit(1.5, "cm"), axis.text.x = element_text(size = 14),
            axis.text.y = element_blank(), axis.ticks.y = element_blank(),
            text = element_text(size = 14), panel.grid.major.y = element_blank()) +
      guides(fill = guide_legend(nrow = 1, byrow = TRUE))
  })
  
  observeEvent(input$temporal_click, {
    showModal(modalDialog(
      title = "Temporal Trend - Enlarged View",
      plotOutput("temporalPlot_modal", height = "500px"),
      size = "l", easyClose = TRUE, footer = modalButton("Close")
    ))
  })
  
  output$temporalPlot_modal <- renderPlot({
    req(qDf_reactive(), input$temporal_granularity)
    plotDf <- aggregate_by_breakout(qDf_reactive(), "CAT1", "Year")
    validate(need(!is.null(plotDf) && nrow(plotDf) > 0, "No data available"))
    plotDf <- aggregate_temporal(plotDf, input$temporal_granularity)
    
    plotDf <- plotDf |>
      mutate(Year_Short = case_when(
        grepl("2011-2013", Year) ~ "'11-'13", grepl("2014-2016", Year) ~ "'14-'16",
        grepl("2017-2019", Year) ~ "'17-'19", grepl("2020-2022", Year) ~ "'20-'22",
        Year == "2023" ~ "'23", TRUE ~ as.character(Year)
      )) |>
      group_by(Year_Short) |> arrange(desc(Response)) |>
      mutate(cumulative = cumsum(agg_percent), pos = cumulative - agg_percent/2,
             label = ifelse(agg_percent > 5, paste0(round(agg_percent, 1), "%"), "")) |>
      ungroup()
    
    ggplot(plotDf, aes(x = as.factor(Year_Short), y = agg_percent, fill = Response)) +
      geom_col(position = "stack") +
      geom_text(aes(y = pos, label = label), color = "white", fontface = "bold", size = 5) +
      labs(x = "Year", y = "Percentage (%)") + scale_y_continuous(limits = c(0, 100)) +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_text(face = "bold", size = 16),
            legend.text = element_text(size = 14), axis.text = element_text(size = 14),
            axis.title = element_text(size = 16), text = element_text(size = 14)) +
      guides(fill = guide_legend(nrow = 1))
  })
  
  observeEvent(input$gender_click, {
    showModal(modalDialog(
      title = "By Gender - Enlarged View",
      plotOutput("genderPlot_modal", height = "500px"),
      size = "l", easyClose = TRUE, footer = modalButton("Close")
    ))
  })
  
  output$genderPlot_modal <- renderPlot({
    req(qDf_reactive())
    plotDf <- aggregate_by_breakout(qDf_reactive(), "CAT2", "Break_Out")
    validate(need(!is.null(plotDf) && nrow(plotDf) > 0, "No data available"))
    plotDf <- plotDf |> select(Break_Out, Response, agg_percent, agg_low_ci_limit, agg_high_ci_limit)
    
    plotDf <- plotDf |> group_by(Break_Out) |> arrange(desc(Response)) |>
      mutate(cumulative = cumsum(agg_percent), pos = cumulative - agg_percent/2,
             label = ifelse(agg_percent > 5, paste0(round(agg_percent, 1), "%"), "")) |>
      ungroup()
    
    ggplot(plotDf, aes(x = Break_Out, y = agg_percent, fill = Response)) +
      geom_col(position = "stack") +
      geom_text(aes(y = pos, label = label), color = "white", fontface = "bold", size = 5) +
      labs(x = "Gender", y = "Percentage (%)") + scale_y_continuous(limits = c(0, 100)) +
      theme_minimal() +
      theme(legend.position = "bottom", axis.text = element_text(size = 14),
            axis.title = element_text(size = 16), text = element_text(size = 14)) +
      guides(fill = guide_legend(nrow = 1))
  })
  
  observeEvent(input$race_click, {
    showModal(modalDialog(
      title = "By Race/Ethnicity - Enlarged View",
      plotOutput("racePlot_modal", height = "500px"),
      size = "l", easyClose = TRUE, footer = modalButton("Close")
    ))
  })
  
  output$racePlot_modal <- renderPlot({
    req(qDf_reactive())
    plotDf <- aggregate_by_breakout(qDf_reactive(), "CAT4", "Break_Out")
    validate(need(!is.null(plotDf) && nrow(plotDf) > 0, "No data available"))
    plotDf <- plotDf |> select(Break_Out, Response, agg_percent, agg_low_ci_limit, agg_high_ci_limit)
    
    plotDf <- plotDf |>
      mutate(Break_Out_Short = case_when(
        grepl("American Indian", Break_Out, ignore.case = TRUE) ~ "AI/AN",
        grepl("Asian", Break_Out, ignore.case = TRUE) ~ "Asian",
        grepl("Black", Break_Out, ignore.case = TRUE) ~ "Black",
        grepl("Hispanic", Break_Out, ignore.case = TRUE) ~ "Hispanic",
        grepl("Multiracial", Break_Out, ignore.case = TRUE) ~ "Multiracial",
        grepl("White", Break_Out, ignore.case = TRUE) ~ "White",
        grepl("Other", Break_Out, ignore.case = TRUE) ~ "Other",
        TRUE ~ Break_Out
      )) |>
      group_by(Break_Out_Short) |> arrange(desc(Response)) |>
      mutate(cumulative = cumsum(agg_percent), pos = cumulative - agg_percent/2,
             label = ifelse(agg_percent > 5, paste0(round(agg_percent, 1), "%"), "")) |>
      ungroup()
    
    ggplot(plotDf, aes(x = Break_Out_Short, y = agg_percent, fill = Response)) +
      geom_col(position = "stack") +
      geom_text(aes(y = pos, label = label), color = "white", fontface = "bold", size = 5) +
      labs(x = "Race/Ethnicity", y = "Percentage (%)") +
      scale_y_continuous(limits = c(0, 100)) +
      theme_minimal() +
      theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16),
            legend.position = "bottom", text = element_text(size = 14)) +
      guides(fill = guide_legend(nrow = 1))
  })
  
  observeEvent(input$age_click, {
    showModal(modalDialog(
      title = "By Age Group - Enlarged View",
      plotOutput("agePlot_modal", height = "500px"),
      size = "l", easyClose = TRUE, footer = modalButton("Close")
    ))
  })
  
  output$agePlot_modal <- renderPlot({
    req(qDf_reactive(), input$age_granularity)
    plotDf <- aggregate_by_breakout(qDf_reactive(), "CAT3", "Break_Out")
    validate(need(!is.null(plotDf) && nrow(plotDf) > 0, "No data available"))
    plotDf <- aggregate_age(plotDf, input$age_granularity)
    
    plotDf <- plotDf |>
      mutate(Break_Out_Short = case_when(
        grepl("18.*24", Break_Out, ignore.case = TRUE) ~ "18-24",
        grepl("25.*34", Break_Out, ignore.case = TRUE) ~ "25-34",
        grepl("35.*44", Break_Out, ignore.case = TRUE) ~ "35-44",
        grepl("45.*54", Break_Out, ignore.case = TRUE) ~ "45-54",
        grepl("55.*64", Break_Out, ignore.case = TRUE) ~ "55-64",
        grepl("65.*older|65 or older|65\\+", Break_Out, ignore.case = TRUE) ~ "65+",
        grepl("Young.*35", Break_Out, ignore.case = TRUE) ~ "< 35",
        grepl("Middle.*35.*64", Break_Out, ignore.case = TRUE) ~ "35-64",
        grepl("Senior.*65", Break_Out, ignore.case = TRUE) ~ "65+",
        TRUE ~ Break_Out
      )) |>
      group_by(Break_Out_Short) |> arrange(desc(Response)) |>
      mutate(cumulative = cumsum(agg_percent), pos = cumulative - agg_percent/2,
             label = ifelse(agg_percent > 5, paste0(round(agg_percent, 1), "%"), "")) |>
      ungroup()
    
    ggplot(plotDf, aes(x = Break_Out_Short, y = agg_percent, fill = Response)) +
      geom_col(position = "stack") +
      geom_text(aes(y = pos, label = label), color = "white", fontface = "bold", size = 5) +
      labs(x = "Age Group", y = "Percentage (%)") +
      scale_y_continuous(limits = c(0, 100)) +
      theme_minimal() +
      theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16),
            legend.position = "bottom", text = element_text(size = 14)) +
      guides(fill = guide_legend(nrow = 1))
  })
  
  observeEvent(input$edu_click, {
    showModal(modalDialog(
      title = "By Education Level - Enlarged View",
      plotOutput("eduPlot_modal", height = "500px"),
      size = "l", easyClose = TRUE, footer = modalButton("Close")
    ))
  })
  
  output$eduPlot_modal <- renderPlot({
    req(qDf_reactive())
    plotDf <- aggregate_by_breakout(qDf_reactive(), "CAT5", "Break_Out")
    validate(need(!is.null(plotDf) && nrow(plotDf) > 0, "No data available"))
    plotDf <- plotDf |> select(Break_Out, Response, agg_percent, agg_low_ci_limit, agg_high_ci_limit)
    
    plotDf <- plotDf |>
      mutate(Break_Out_Short = case_when(
        grepl("Did not graduate", Break_Out, ignore.case = TRUE) ~ "< HS",
        grepl("less than high school", Break_Out, ignore.case = TRUE) ~ "< HS",
        grepl("Graduated high school", Break_Out, ignore.case = TRUE) ~ "HS Grad",
        grepl("high school graduate", Break_Out, ignore.case = TRUE) ~ "HS Grad",
        grepl("Attended college", Break_Out, ignore.case = TRUE) ~ "Some College",
        grepl("some college", Break_Out, ignore.case = TRUE) ~ "Some College",
        grepl("Graduated from college", Break_Out, ignore.case = TRUE) ~ "College Grad",
        grepl("college graduate", Break_Out, ignore.case = TRUE) ~ "College Grad",
        TRUE ~ Break_Out
      )) |>
      group_by(Break_Out_Short) |> arrange(desc(Response)) |>
      mutate(cumulative = cumsum(agg_percent), pos = cumulative - agg_percent/2,
             label = ifelse(agg_percent > 5, paste0(round(agg_percent, 1), "%"), "")) |>
      ungroup()
    
    ggplot(plotDf, aes(x = Break_Out_Short, y = agg_percent, fill = Response)) +
      geom_col(position = "stack") +
      geom_text(aes(y = pos, label = label), color = "white", fontface = "bold", size = 5) +
      labs(x = "Education Level", y = "Percentage (%)") +
      scale_y_continuous(limits = c(0, 100)) +
      theme_minimal() +
      theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16),
            legend.position = "bottom", text = element_text(size = 14)) +
      guides(fill = guide_legend(nrow = 1))
  })
  observeEvent(input$income_click, {
    showModal(modalDialog(
      title = "By Income Level - Enlarged View",
      plotOutput("incomePlot_modal", height = "500px"),
      size = "l", easyClose = TRUE, footer = modalButton("Close")
    ))
  })
  output$incomePlot_modal <- renderPlot({
    req(qDf_reactive(), input$income_granularity)
    plotDf <- aggregate_by_breakout(qDf_reactive(), "CAT6", "Break_Out")
    validate(need(!is.null(plotDf) && nrow(plotDf) > 0, "No data available"))
    plotDf <- aggregate_income(plotDf, input$income_granularity)
    
    plotDf <- plotDf |>
      mutate(Break_Out_Short = case_when(
        grepl("Less than.*15", Break_Out, ignore.case = TRUE) ~ "< $15K",
        grepl("15,000.*less than.*25", Break_Out, ignore.case = TRUE) ~ "$15-25K",
        grepl("25,000.*less than.*35", Break_Out, ignore.case = TRUE) ~ "$25-35K",
        grepl("35,000.*less than.*50", Break_Out, ignore.case = TRUE) ~ "$35-50K",
        grepl("50,000.*less than.*75", Break_Out, ignore.case = TRUE) ~ "$50-75K",
        grepl("75,000 or more", Break_Out, ignore.case = TRUE) ~ "≥ $75K",
        grepl("Low.*15K", Break_Out, ignore.case = TRUE) ~ "Low",
        grepl("Medium.*15K.*50K", Break_Out, ignore.case = TRUE) ~ "Medium",
        grepl("High.*50K", Break_Out, ignore.case = TRUE) ~ "High",
        TRUE ~ Break_Out
      )) |>
      group_by(Break_Out_Short) |> arrange(desc(Response)) |>
      mutate(cumulative = cumsum(agg_percent), pos = cumulative - agg_percent/2,
             label = ifelse(agg_percent > 5, paste0(round(agg_percent, 1), "%"), "")) |>
      ungroup()
    
    ggplot(plotDf, aes(x = Break_Out_Short, y = agg_percent, fill = Response)) +
      geom_col(position = "stack") +
      geom_text(aes(y = pos, label = label), color = "white", fontface = "bold", size = 5) +
      labs(x = "Income Level", y = "Percentage (%)") +
      scale_y_continuous(limits = c(0, 100)) +
      theme_minimal() +
      theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16),
            legend.position = "bottom", text = element_text(size = 14)) +
      guides(fill = guide_legend(nrow = 1))
  })
  observeEvent(input$state_click, {
    showModal(modalDialog(
      title = "Geographic Distribution - Enlarged View",
      plotOutput("stateMapPlot_modal", height = "600px"),
      size = "xl", easyClose = TRUE, footer = modalButton("Close")
    ))
  })
  output$stateMapPlot_modal <- renderPlot({
    req(qDf_reactive(), input$state_granularity)
    plotDf <- aggregate_by_breakout(qDf_reactive(), "CAT1", "Locationabbr")
    validate(need(!is.null(plotDf) && nrow(plotDf) > 0, "No data available"))
    plotDf <- aggregate_states(plotDf, input$state_granularity)
    
    available_responses <- unique(plotDf$Response)
    if ("Yes" %in% available_responses) {
      selected_response <- "Yes"
      plotDf <- plotDf |> filter(Response == "Yes")
    } else if ("yes" %in% available_responses) {
      selected_response <- "yes"
      plotDf <- plotDf |> filter(Response == "yes")
    } else {
      selected_response <- sort(available_responses)[1]
      plotDf <- plotDf |> filter(Response == selected_response)
    }
    
    if (input$state_granularity == "regions") {
      ggplot(plotDf, aes(x = reorder(Locationabbr, -agg_percent), y = agg_percent, fill = reorder(Locationabbr, -agg_percent))) +
        geom_col() +
        geom_text(aes(label = paste0(round(agg_percent, 1), "%")), vjust = -0.5, size = 6, fontface = "bold") +
        labs(x = "Region", y = paste0(selected_response, " (%)"),
             title = paste("Geographic Distribution:", selected_response)) +
        scale_y_continuous(limits = c(0, max(plotDf$agg_percent) * 1.15), expand = c(0, 0)) +
        scale_fill_manual(values = c("#26A69A", "#4DB6AC", "#80CBC4", "#B2DFDB", "#A5D6A7")) +
        theme_minimal() +
        theme(legend.position = "none", axis.text = element_text(size = 16, face = "bold"),
              axis.title = element_text(size = 18, face = "bold"),
              plot.title = element_text(size = 20, face = "bold", hjust = 0.5))
    } else {
      mapData <- plotDf |> rename(state = Locationabbr, value = agg_percent)
      midpoint_value <- mean(mapData$value, na.rm = TRUE)
      
      plot_usmap(data = mapData, values = "value", color = "white", size = 0.5) +
        scale_fill_gradient2(
          low = "#fee5d9", mid = "#fc8d59", high = "#d7301f",
          midpoint = midpoint_value, name = paste0(selected_response, " (%)"),
          limits = c(min(mapData$value, na.rm = TRUE), max(mapData$value, na.rm = TRUE)),
          guide = guide_colorbar(barwidth = 20, barheight = 1.2,
                                 title.position = "top", title.hjust = 0.5)
        ) +
        labs(title = paste("Geographic Distribution:", selected_response)) +
        theme_void() +
        theme(legend.position = "bottom",
              legend.title = element_text(size = 16, face = "bold"),
              legend.text = element_text(size = 14),
              plot.title = element_text(size = 20, face = "bold", hjust = 0.5, margin = margin(b = 10)))
    }
  })
  
  
  # ===== CI TABLES =====
    
  output$overall_ci_table <- renderTable({
      req(qDf_reactive())
      plotDf <- aggregate_by_breakout(qDf_reactive(), "CAT1", NULL)
      validate(need(!is.null(plotDf) && nrow(plotDf) > 0, "No data available"))
      plotDf |>
        mutate(Percent = paste0(round(agg_percent, 1), "%"),
               CI = paste0("[", round(agg_low_ci_limit, 1), "%, ", round(agg_high_ci_limit, 1), "%]")) |>
        select(Response, Percent, CI)
    }, striped = TRUE, spacing = "xs", width = "100%", align = "lrr")
  output$temporal_ci_table <- renderTable({
    req(qDf_reactive(), input$temporal_granularity)
    plotDf <- aggregate_by_breakout(qDf_reactive(), "CAT1", "Year")
    validate(need(!is.null(plotDf) && nrow(plotDf) > 0, "No data available"))
    plotDf <- aggregate_temporal(plotDf, input$temporal_granularity)
    plotDf |>
      mutate(Percent = paste0(round(agg_percent, 1), "%"),
             CI = paste0("[", round(agg_low_ci_limit, 1), "%, ", round(agg_high_ci_limit, 1), "%]")) |>
      select(Year, Response, Percent, CI)
  }, striped = TRUE, spacing = "xs", width = "100%", align = "lllr")
  output$gender_ci_table <- renderTable({
    req(qDf_reactive())
    plotDf <- aggregate_by_breakout(qDf_reactive(), "CAT2", "Break_Out")
    validate(need(!is.null(plotDf) && nrow(plotDf) > 0, "No data available"))
    plotDf |>
      select(Break_Out, Response, agg_percent, agg_low_ci_limit, agg_high_ci_limit) |>
      mutate(Percent = paste0(round(agg_percent, 1), "%"),
             CI = paste0("[", round(agg_low_ci_limit, 1), "%, ", round(agg_high_ci_limit, 1), "%]")) |>
      rename(Gender = Break_Out) |>
      select(Gender, Response, Percent, CI)
  }, striped = TRUE, spacing = "xs", width = "100%", align = "lllr")
  output$race_ci_table <- renderTable({
    req(qDf_reactive())
    plotDf <- aggregate_by_breakout(qDf_reactive(), "CAT4", "Break_Out")
    validate(need(!is.null(plotDf) && nrow(plotDf) > 0, "No data available"))
    plotDf |>
      select(Break_Out, Response, agg_percent, agg_low_ci_limit, agg_high_ci_limit) |>
      mutate(Percent = paste0(round(agg_percent, 1), "%"),
             CI = paste0("[", round(agg_low_ci_limit, 1), "%, ", round(agg_high_ci_limit, 1), "%]")) |>
      rename(Race = Break_Out) |>
      select(Race, Response, Percent, CI)
  }, striped = TRUE, spacing = "xs", width = "100%", align = "lllr")
  output$age_ci_table <- renderTable({
    req(qDf_reactive(), input$age_granularity)
    plotDf <- aggregate_by_breakout(qDf_reactive(), "CAT3", "Break_Out")
    validate(need(!is.null(plotDf) && nrow(plotDf) > 0, "No data available"))
    plotDf <- aggregate_age(plotDf, input$age_granularity)
    plotDf |>
      mutate(Percent = paste0(round(agg_percent, 1), "%"),
             CI = paste0("[", round(agg_low_ci_limit, 1), "%, ", round(agg_high_ci_limit, 1), "%]")) |>
      rename(Age = Break_Out) |>
      select(Age, Response, Percent, CI)
  }, striped = TRUE, spacing = "xs", width = "100%", align = "lllr")
  output$edu_ci_table <- renderTable({
    req(qDf_reactive())
    plotDf <- aggregate_by_breakout(qDf_reactive(), "CAT5", "Break_Out")
    validate(need(!is.null(plotDf) && nrow(plotDf) > 0, "No data available"))
    plotDf |>
      select(Break_Out, Response, agg_percent, agg_low_ci_limit, agg_high_ci_limit) |>
      mutate(Percent = paste0(round(agg_percent, 1), "%"),
             CI = paste0("[", round(agg_low_ci_limit, 1), "%, ", round(agg_high_ci_limit, 1), "%]")) |>
      rename(Education = Break_Out) |>
      select(Education, Response, Percent, CI)
  }, striped = TRUE, spacing = "xs", width = "100%", align = "lllr")
  output$income_ci_table <- renderTable({
    req(qDf_reactive(), input$income_granularity)
    plotDf <- aggregate_by_breakout(qDf_reactive(), "CAT6", "Break_Out")
    validate(need(!is.null(plotDf) && nrow(plotDf) > 0, "No data available"))
    plotDf <- aggregate_income(plotDf, input$income_granularity)
    plotDf |>
      mutate(Percent = paste0(round(agg_percent, 1), "%"),
             CI = paste0("[", round(agg_low_ci_limit, 1), "%, ", round(agg_high_ci_limit, 1), "%]")) |>
      rename(Income = Break_Out) |>
      select(Income, Response, Percent, CI)
  }, striped = TRUE, spacing = "xs", width = "100%", align = "lllr")
  output$state_ci_table <- renderTable({
    req(qDf_reactive(), input$state_granularity)
    plotDf <- aggregate_by_breakout(qDf_reactive(), "CAT1", "Locationabbr")
    validate(need(!is.null(plotDf) && nrow(plotDf) > 0, "No data available"))
    plotDf <- aggregate_states(plotDf, input$state_granularity)
    plotDf |>
      mutate(Percent = paste0(round(agg_percent, 1), "%"),
             CI = paste0("[", round(agg_low_ci_limit, 1), "%, ", round(agg_high_ci_limit, 1), "%]")) |>
      rename(Location = Locationabbr) |>
      select(Location, Response, Percent, CI)
  }, striped = TRUE, spacing = "xs", width = "100%", align = "lllr")
}
shinyApp(ui = ui, server = server)
