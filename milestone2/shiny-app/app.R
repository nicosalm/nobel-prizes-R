# Nobel Prize Explorer
# Team 12: Owen, Arnesh, Jian, Nico
# Mar 21, 2025

# TODOs:
# - Add more countries to selection

pkgs <- c("shiny", "ggplot2", "dplyr", "plotly", "DT",
          "lubridate", "maps", "reshape2", "zoo")
new_pkgs <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
if(length(new_pkgs)) install.packages(new_pkgs)

library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)
library(lubridate)
library(maps)
library(reshape2)
library(zoo)

if(requireNamespace("conflicted", quietly=TRUE)) {
  conflicted::conflict_prefer("layout", "plotly")
  conflicted::conflict_prefer("filter", "dplyr")
}

ui <- fluidPage(
  titlePanel("Nobel Prize Data Explorer"),

  sidebarLayout(
    sidebarPanel(
      h4("Explore Nobel Prize data"),
      p("This app visualizes patterns in Nobel Prize awards."),

      sliderInput("yearRange",
                  "Select Time Period:",
                  min = 1901, max = 2023,
                  value = c(1901, 2023),
                  step = 1,
                  width = "100%"),

      selectInput("category", "Select Category:",
                  c("All Categories" = "all",
                    "Chemistry" = "Chemistry",
                    "Economics" = "Economics",
                    "Literature" = "Literature",
                    "Medicine" = "Medicine",
                    "Peace" = "Peace",
                    "Physics" = "Physics"),
                  selected = "all"),

      radioButtons("vizType", "Choose Visualization:",
                  c("Time Series" = "time",
                    "Category Distribution" = "category",
                    "Gender Distribution" = "gender",
                    "Geographic Map" = "map",
                    "Age Distribution" = "age",
                    "Categories Over Time" = "category_time",
                    "Countries Over Time" = "country_time"),
                  selected = "time"),

      conditionalPanel(
        condition = "input.vizType == 'category_time'",
        checkboxGroupInput("selectedCategories", "Select Categories to Compare:",
                          c("Chemistry", "Economics", "Literature",
                            "Medicine", "Peace", "Physics"),
                          selected = c("Chemistry", "Physics", "Medicine"))
      ),

      conditionalPanel(
        condition = "input.vizType == 'country_time'",
        checkboxGroupInput("selectedCountries", "Select Countries to Compare:",
                          c("United States of America", "United Kingdom", "Germany",
                            "France", "Sweden", "Japan", "Russia",
                            "Canada", "Italy", "Netherlands"),
                          selected = c("United States of America", "United Kingdom",
                                       "Germany", "France", "Sweden"))
      ),

      conditionalPanel(
        condition = "input.vizType == 'age'",
        radioButtons("ageVizType", "Age Visualization Type:",
                    c("Boxplot by Category" = "boxplot",
                      "Density Plot" = "density",
                      "Age Trends Over Time" = "trend"),
                    selected = "boxplot")
      ),

      hr(),
      p("Data source: Nobel Prize official records", style = "font-size: 0.8em;")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Visualization",
                 br(),
                 plotlyOutput("mainPlot", height = "500px"),
                 br(),
                 conditionalPanel(
                   condition = "input.vizType == 'map'",
                   p("The map shows the distribution of Nobel Prizes by laureate birth country.")
                 ),
                 conditionalPanel(
                   condition = "input.vizType == 'category_time'",
                   p("This visualization shows how the distribution of Nobel Prizes across categories has changed over time.")
                 ),
                 conditionalPanel(
                   condition = "input.vizType == 'country_time'",
                   p("This visualization shows how different countries' contributions to Nobel Prizes have changed over time.")
                 )),

        tabPanel("Comparative Analysis",
                 br(),
                 plotlyOutput("comparisonPlot", height = "500px"),
                 br(),
                 p("This panel shows gender distribution across categories over the selected time period.")),

        tabPanel("Data Table",
                 br(),
                 DTOutput("dataTable"))
      )
    )
  )
)

server <- function(input, output, session) {

  nobel_data <- reactive({
    df <- read.csv('nobel.csv')

    if(nrow(df) < 1) return(data.frame())

    if(is.character(df$year)) {
      if(grepl("-", df$year[1])) {
        df$year <- as.integer(substring(df$year, 1, 4))
      } else {
        df$year <- as.integer(df$year)
      }
    }

    if(is.character(df$birth_date)) {
      df$birth_year <- sapply(df$birth_date, function(x) {
        if(is.na(x) || x == "") return(NA)
        as.integer(substring(x, 1, 4))
      })
    } else {
      df$birth_year <- NA
    }

    df$age_at_award <- df$year - df$birth_year
    df$decade <- floor(df$year / 10) * 10

    return(df)
  })

  filtered_data <- reactive({
    data <- nobel_data() %>%
      filter(year >= input$yearRange[1], year <= input$yearRange[2])

    if(input$category != "all") {
      data <- data %>% filter(category == input$category)
    }

    return(data)
  })

  time_series_data <- reactive({
    filtered_data() %>%
      group_by(year) %>%
      summarize(count = n())
  })

  category_data <- reactive({
    filtered_data() %>%
      group_by(category) %>%
      summarize(count = n())
  })

  gender_data <- reactive({
    filtered_data() %>%
      group_by(sex) %>%
      summarize(count = n()) %>%
      filter(!is.na(sex))
  })

  map_data <- reactive({
    filtered_data() %>%
      filter(!is.na(birth_country)) %>%
      group_by(birth_country) %>%
      summarize(count = n()) %>%
      arrange(desc(count))
  })

  category_time_data <- reactive({
    if(length(input$selectedCategories) == 0) {
      return(data.frame())
    }

    data <- filtered_data() %>%
      filter(category %in% input$selectedCategories) %>%
      group_by(decade, category) %>%
      summarize(count = n()) %>%
      ungroup()

    decades <- unique(data$decade)
    categories <- unique(data$category)

    complete_data <- expand.grid(decade = decades, category = categories)
    complete_data <- left_join(complete_data, data, by = c("decade", "category"))
    complete_data$count[is.na(complete_data$count)] <- 0

    return(complete_data)
  })

  country_time_data <- reactive({
    if(length(input$selectedCountries) == 0) {
      return(data.frame())
    }

    yearly_data <- filtered_data() %>%
      filter(birth_country %in% input$selectedCountries) %>%
      group_by(year, birth_country) %>%
      summarize(count = n()) %>%
      ungroup()

    if(nrow(yearly_data) < 1) return(data.frame())

    years <- seq(min(yearly_data$year), max(yearly_data$year))
    countries <- unique(yearly_data$birth_country)

    complete_data <- expand.grid(
      year = years,
      birth_country = countries
    )

    full_data <- left_join(complete_data, yearly_data, by = c("year", "birth_country"))
    full_data$count[is.na(full_data$count)] <- 0

    result <- full_data %>%
      arrange(birth_country, year) %>%
      group_by(birth_country) %>%
      mutate(
        total_award = rollmean(count, k = 5, fill = NA, align = "right")
      ) %>%
      filter(!is.na(total_award)) %>%
      ungroup()

    return(result)
  })

  age_distribution_data <- reactive({
    filtered_data() %>%
      filter(!is.na(age_at_award)) %>%
      filter(age_at_award >= 15 & age_at_award <= 100)
  })

  age_trends_data <- reactive({
    filtered_data() %>%
      filter(!is.na(age_at_award)) %>%
      filter(age_at_award >= 15 & age_at_award <= 100) %>%
      group_by(decade) %>%
      summarize(
        mean_age = mean(age_at_award, na.rm = TRUE),
        median_age = median(age_at_award, na.rm = TRUE),
        min_age = min(age_at_award, na.rm = TRUE),
        max_age = max(age_at_award, na.rm = TRUE)
      )
  })

  output$mainPlot <- renderPlotly({
    if(is.null(input$vizType)) return(NULL)

    tryCatch({
      if(input$vizType == "time") {
        data <- time_series_data()

        if(nrow(data) == 0) {
          p <- ggplot() +
            annotate("text", x = 0.5, y = 0.5, label = "No data available") +
            theme_void()
        } else {
          p <- ggplot(data, aes(x = year, y = count, text = paste0(
                "Year: ", year, "<br>",
                "Awards: ", count
              ))) +
            geom_line() +
            geom_point() +
            labs(title = "Nobel Prizes Over Time",
                 x = "Year",
                 y = "Number of Awards") +
            theme_minimal()
        }

      } else if(input$vizType == "category") {
        data <- category_data()

        if(nrow(data) == 0) {
          p <- ggplot() +
            annotate("text", x = 0.5, y = 0.5, label = "No data available") +
            theme_void()
        } else {
          p <- ggplot(data, aes(x = reorder(category, -count), y = count, fill = category,
                               text = paste0(
                                 "Category: ", category, "<br>",
                                 "Awards: ", count
                               ))) +
            geom_bar(stat = "identity") +
            labs(title = "Nobel Prizes by Category",
                 x = "Category",
                 y = "Number of Awards") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            theme(legend.position = "none")
        }

      } else if(input$vizType == "gender") {
        data <- gender_data()

        if(nrow(data) == 0) {
          p <- ggplot() +
            annotate("text", x = 0.5, y = 0.5, label = "No data available") +
            theme_void()
          return(ggplotly(p))
        } else {
          gender_counts <- data.frame(
            gender = data$sex,
            count = data$count,
            percent = 100 * data$count / sum(data$count)
          )

          p <- ggplot(gender_counts, aes(x = gender, y = count, fill = gender,
                                        text = paste0(
                                          "Gender: ", gender, "<br>",
                                          "Count: ", count, "<br>",
                                          "Percentage: ", round(percent, 1), "%"
                                        ))) +
            geom_bar(stat = "identity") +
            labs(title = "Gender Distribution of Nobel Prize Laureates",
                 x = "Gender",
                 y = "Number of Laureates",
                 fill = "Gender") +
            theme_minimal()

          return(ggplotly(p, tooltip = "text"))
        }

      } else if(input$vizType == "map") {
        data <- map_data()

        if(nrow(data) == 0) {
          p <- ggplot() +
            annotate("text", x = 0.5, y = 0.5, label = "No data available") +
            theme_void()
          return(ggplotly(p))
        } else {
          top_countries <- head(data, 10)

          p <- ggplot(top_countries, aes(x = reorder(birth_country, count), y = count, fill = count,
                                        text = paste0(
                                          "Country: ", birth_country, "<br>",
                                          "Laureates: ", count
                                        ))) +
            geom_bar(stat = "identity") +
            coord_flip() +
            labs(title = "Top 10 Countries by Nobel Prize Laureates",
                 subtitle = paste(input$yearRange[1], "-", input$yearRange[2]),
                 x = "Country",
                 y = "Number of Laureates") +
            scale_fill_gradient(low = "#c6dbef", high = "#08519c", name = "Laureates") +
            theme_minimal() +
            theme(legend.position = "none")

          return(ggplotly(p, tooltip = "text"))
        }

      } else if(input$vizType == "age") {
        data <- age_distribution_data()

        if(nrow(data) == 0) {
          p <- ggplot() +
            annotate("text", x = 0.5, y = 0.5, label = "No data available") +
            theme_void()
        } else {
          if(input$ageVizType == "boxplot") {
            p <- ggplot(data, aes(x = category, y = age_at_award, fill = category,
                                 text = paste0(
                                   "Category: ", category, "<br>",
                                   "Age: ", age_at_award, "<br>",
                                   "Laureate: ", full_name
                                 ))) +
              geom_boxplot() +
              labs(title = "Age Distribution by Category",
                   x = "Category",
                   y = "Age at Award") +
              theme_minimal() +
              theme(legend.position = "none")

          } else if(input$ageVizType == "density") {
            p <- ggplot(data, aes(x = age_at_award, fill = category,
                                  text = paste0(
                                    "Age: ", round(age_at_award), "<br>",
                                    "Category: ", category
                                  ))) +
              geom_density(alpha = 0.5) +
              labs(title = "Age Density by Category",
                   x = "Age at Award",
                   y = "Density") +
              theme_minimal()

          } else {
            trend_data <- age_trends_data()

            p <- ggplot(trend_data, aes(x = decade, y = mean_age,
                                       text = paste0(
                                         "Decade: ", decade, "s<br>",
                                         "Mean Age: ", round(mean_age, 1), "<br>",
                                         "Median Age: ", round(median_age, 1), "<br>",
                                         "Range: ", min_age, "-", max_age
                                       ))) +
              geom_line() +
              geom_point(size = 2) +
              labs(title = "Average Age of Nobel Laureates Over Time",
                   x = "Decade",
                   y = "Mean Age at Award") +
              theme_minimal()
          }
        }

      } else if(input$vizType == "category_time") {
        data <- category_time_data()

        if(nrow(data) == 0) {
          p <- ggplot() +
            annotate("text", x = 0.5, y = 0.5, label = "No data available") +
            theme_void()
        } else {
          p <- ggplot(data, aes(x = decade, y = count, color = category, group = category,
                               text = paste0(
                                 "Decade: ", decade, "s<br>",
                                 "Category: ", category, "<br>",
                                 "Awards: ", count
                               ))) +
            geom_line(linewidth = 1) +
            geom_point(size = 2) +
            labs(title = "Nobel Prizes by Category Over Time",
                 x = "Decade",
                 y = "Number of Awards",
                 color = "Category") +
            theme_minimal() +
            theme(legend.position = "bottom")
        }

      } else if(input$vizType == "country_time") {
        data <- country_time_data()

        if(nrow(data) == 0) {
          p <- ggplot() +
            annotate("text", x = 0.5, y = 0.5, label = "No data available") +
            theme_void()
        } else {
          p <- ggplot(data, aes(x = year, y = total_award, color = birth_country, group = birth_country,
                               text = paste0(
                                 "Year: ", year, "<br>",
                                 "Country: ", birth_country, "<br>",
                                 "Awards: ", count, "<br>",
                                 "5-Year Avg: ", round(total_award, 2)
                               ))) +
            geom_line(linewidth = 1) +
            labs(title = "Nation's trend for nobel awards",
                 x = "year",
                 y = "total_award") +
            theme_minimal() +
            theme(legend.position = "right",
                  legend.title = element_blank())
        }
      }

      plt <- ggplotly(p, tooltip = "text")
      plotly::layout(plt, margin = list(l = 50, r = 20, b = 50, t = 50))

    }, error = function(e) {
      message("Error in plot: ", e$message)
      p <- ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Error in visualization") +
        theme_void()
      ggplotly(p)
    })
  })

  output$comparisonPlot <- renderPlotly({
    tryCatch({
      data <- filtered_data() %>%
        filter(!is.na(sex)) %>%
        group_by(category, sex) %>%
        summarize(count = n()) %>%
        group_by(category) %>%
        mutate(percent = count / sum(count) * 100)

      if(nrow(data) == 0) {
        p <- ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "No data available") +
          theme_void()
      } else {
        p <- ggplot(data, aes(x = category, y = percent, fill = sex,
                             text = paste0(
                               "Category: ", category, "<br>",
                               "Gender: ", sex, "<br>",
                               "Percentage: ", round(percent, 1), "%<br>",
                               "Count: ", count
                             ))) +
          geom_bar(stat = "identity", position = "stack") +
          labs(title = "Gender Distribution by Category",
               x = "Category",
               y = "Percentage",
               fill = "Gender") +
          scale_fill_manual(values = c("Female" = "#FF9999", "Male" = "#6699CC")) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }

      plt <- ggplotly(p, tooltip = "text")
      plotly::layout(plt, margin = list(l = 50, r = 20, b = 80, t = 50))

    }, error = function(e) {
      message("Error in comparison plot: ", e$message)
      p <- ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Error in visualization") +
        theme_void()
      ggplotly(p)
    })
  })

  output$dataTable <- renderDT({
    data <- filtered_data() %>%
      select(year, category, full_name, sex, birth_country, organization_country, age_at_award)

    datatable(data,
              options = list(pageLength = 15,
                             scrollX = TRUE),
              rownames = FALSE)
  })
}

shinyApp(ui = ui, server = server)
