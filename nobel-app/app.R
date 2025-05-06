pkgs <- c("shiny", "ggplot2", "dplyr", "plotly", "DT",
          "lubridate", "maps", "reshape2", "zoo",
          "rnaturalearth", "rnaturalearthdata", "sf")
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
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

if(requireNamespace("conflicted", quietly=TRUE)) {
  conflicted::conflict_prefer("layout", "plotly")
  conflicted::conflict_prefer("filter", "dplyr")
}

ui <- fluidPage(
  titlePanel("Nobel Prize Data Explorer"),

  sidebarLayout(
    sidebarPanel(
      h4("Explore Nobel Prize data"),
      p("This app visualizes patterns in Nobel Prize awards from 1901 to 2023."),

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
                    "Gender Over Time" = "gender_time",
                    "World Map" = "choropleth",
                    "Age Distribution" = "age",
                    "Countries Over Time" = "country_time",
                    "Institutional Affiliations" = "institutions"),
                  selected = "time"),

      conditionalPanel(
        condition = "input.vizType == 'country_time'",
        checkboxGroupInput("selectedCountries", "Select Countries to Compare:",
                          c("United States of America", "United Kingdom", "Germany",
                            "France", "Sweden", "Japan", "Switzerland"),
                          selected = c("United States of America", "United Kingdom",
                                       "Germany", "France"))
      ),

      conditionalPanel(
        condition = "input.vizType == 'choropleth'",
        sliderInput("topNCountries",
                    "Show Top N Countries:",
                    min = 5,
                    max = 50,
                    value = 30,
                    step = 5)
      ),

      conditionalPanel(
        condition = "input.vizType == 'country_time'",
        checkboxGroupInput("timelineEvents", "Include Historical Context:",
                          c("World Wars" = "wars",
                            "Major Economic Events" = "economic",
                            "Scientific Breakthroughs" = "science",
                            "Category Introductions" = "categories"),
                          selected = c("wars", "categories"))
      ),

      conditionalPanel(
        condition = "input.vizType == 'institutions'",
        sliderInput("topInstitutions",
                    "Show Top N Institutions:",
                    min = 5,
                    max = 20,
                    value = 10,
                    step = 5),
        checkboxInput("normalizeByDecade", "Normalize by Decade", TRUE)
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
      p("Data source: Nobel Prize official records (nobel.csv)", style = "font-size: 0.8em;")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Visualization",
                 br(),
                 plotlyOutput("mainPlot", height = "500px"),
                 br(),
                 conditionalPanel(
                   condition = "input.vizType == 'choropleth'",
                   p("The map shows the distribution of Nobel Prizes by laureate birth country.")
                 ),
                 conditionalPanel(
                   condition = "input.vizType == 'country_time'",
                   p("This visualization shows how different countries' Nobel Prize awards have changed over time, with significant historical events marked by vertical lines.")
                 )),

        tabPanel("Comparative Analysis",
                 br(),
                 selectInput("comparisonType", "Select Comparison Type:",
                           c("Gender by Category" = "gender_category",
                             "Age by Gender" = "age_gender",
                             "Gender Equality Metrics" = "gender_equality"),
                           selected = "gender_equality"),
                 plotlyOutput("comparisonPlot", height = "500px"),
                 br(),
                 uiOutput("comparisonDescription")),

        tabPanel("Data Table",
                 br(),
                 DTOutput("dataTable"))
      )
    )
  )
)

server <- function(input, output, session) {

  # let's load and process the Nobel Prize data
  nobel_data <- reactive({
    if (file.exists('nobel.csv')) {
      df <- read.csv('nobel.csv')
    } else if (file.exists('/srv/shiny-server/nobel.csv')) {
      df <- read.csv('/srv/shiny-server/nobel.csv')
    } else {
      message("Could not find nobel.csv file")
      return(data.frame())
    }

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

  # filter data based on user selections
  filtered_data <- reactive({
    data <- nobel_data() %>%
      filter(year >= input$yearRange[1], year <= input$yearRange[2])

    if(input$category != "all") {
      data <- data %>% filter(category == input$category)
    }

    return(data)
  })

  world_map_data <- reactive({
    world <- ne_countries(scale = "medium", returnclass = "sf")

    top_countries <- filtered_data() %>%
      filter(!is.na(birth_country)) %>%
      group_by(birth_country) %>%
      summarize(total_award = n()) %>%
      arrange(desc(total_award)) %>%
      head(input$topNCountries)

    world_modified <- world %>%
      left_join(top_countries, by = c("admin" = "birth_country"))

    return(world_modified)
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

  gender_time_data <- reactive({
    data <- filtered_data() %>%
      filter(!is.na(sex)) %>%
      mutate(decade = floor(year / 10) * 10) %>%
      group_by(decade, sex) %>%
      summarize(count = n()) %>%
      group_by(decade) %>%
      mutate(
        total = sum(count),
        percent = count / total * 100,
        female_ratio = sum(count[sex == "Female"]) / sum(count)
      ) %>%
      ungroup()

    return(data)
  })

  gender_equality_metrics <- reactive({
    data <- filtered_data() %>%
      filter(!is.na(sex)) %>%
      mutate(decade = floor(year / 10) * 10) %>%
      group_by(decade) %>%
      summarize(
        total_prizes = n(),
        female_prizes = sum(sex == "Female"),
        male_prizes = sum(sex == "Male"),
        female_percent = female_prizes / total_prizes * 100,
        gender_gap = male_prizes - female_prizes,
        equality_index = 1 - abs((female_prizes/total_prizes) - 0.5) / 0.5  # 1 = perfect equality, 0 = complete inequality
      ) %>%
      arrange(decade)

    return(data)
  })

  historical_events <- reactive({
    events <- data.frame(
      year = numeric(),
      event_type = character(),
      event = character(),
      stringsAsFactors = FALSE
    )

    if("wars" %in% input$timelineEvents) {
      war_events <- data.frame(
        year = c(1914, 1918, 1939, 1945, 1950, 1953, 1955, 1975, 1979, 1989, 2001),
        event_type = "wars",
        event = c("World War I begins", "World War I ends", "World War II begins",
                 "World War II ends", "Korean War begins", "Korean War ends",
                 "Vietnam War begins", "Vietnam War ends", "Soviet-Afghan War begins",
                 "Berlin Wall falls", "9/11 attacks"),
        stringsAsFactors = FALSE
      )
      events <- rbind(events, war_events)
    }

    if("economic" %in% input$timelineEvents) {
      economic_events <- data.frame(
        year = c(1929, 1933, 1971, 1973, 1987, 2000, 2008, 2020),
        event_type = "economic",
        event = c("Wall Street Crash", "Great Depression peak", "Gold standard abandoned",
                 "Oil Crisis", "Black Monday", "Dot-com bubble burst",
                 "Financial Crisis", "COVID-19 pandemic"),
        stringsAsFactors = FALSE
      )
      events <- rbind(events, economic_events)
    }

    if("science" %in% input$timelineEvents) {
      science_events <- data.frame(
        year = c(1905, 1915, 1928, 1953, 1957, 1969, 1977, 1989, 1990, 1996, 2003, 2012),
        event_type = "science",
        event = c("Special Relativity", "General Relativity", "Penicillin discovered",
                 "DNA structure discovered", "Sputnik launched", "Moon landing",
                 "First personal computer", "World Wide Web", "Human Genome Project begins",
                 "Dolly the sheep cloned", "Human Genome sequenced", "Higgs boson discovered"),
        stringsAsFactors = FALSE
      )
      events <- rbind(events, science_events)
    }

    if("categories" %in% input$timelineEvents) {
      category_events <- data.frame(
        year = c(1901, 1901, 1901, 1901, 1901, 1969),
        event_type = "categories",
        event = c("Physics Prize established", "Chemistry Prize established",
                 "Medicine Prize established", "Literature Prize established",
                 "Peace Prize established", "Economics Prize established"),
        stringsAsFactors = FALSE
      )
      events <- rbind(events, category_events)
    }

    return(events)
  })

  institution_data <- reactive({
    if(!("organization_name" %in% colnames(filtered_data()))) {
      return(data.frame())
    }

    data <- filtered_data() %>%
      filter(!is.na(organization_name)) %>%
      mutate(decade = floor(year / 10) * 10)

    if(input$normalizeByDecade) {
      inst_by_decade <- data %>%
        group_by(organization_name, decade) %>%
        summarize(count = n()) %>%
        ungroup()

      top_institutions <- inst_by_decade %>%
        group_by(organization_name) %>%
        summarize(total_count = sum(count)) %>%
        arrange(desc(total_count)) %>%
        head(input$topInstitutions)

      result <- inst_by_decade %>%
        filter(organization_name %in% top_institutions$organization_name) %>%
        complete(decade = unique(decade), organization_name = unique(organization_name)) %>%
        mutate(count = ifelse(is.na(count), 0, count))
    } else {
      result <- data %>%
        group_by(organization_name) %>%
        summarize(count = n()) %>%
        arrange(desc(count)) %>%
        head(input$topInstitutions)
    }

    return(result)
  })

  country_time_data <- reactive({
    if(length(input$selectedCountries) == 0) {
      return(data.frame())
    }

    yearly_data <- filtered_data() %>%
      filter(birth_country %in% input$selectedCountries)

    if(nrow(yearly_data) < 1) return(data.frame())

    yearly_grouped <- yearly_data %>%
      group_by(year, birth_country) %>%
      summarize(count = n()) %>%
      ungroup()

    years <- seq(min(yearly_grouped$year), max(yearly_grouped$year))
    countries <- unique(yearly_grouped$birth_country)

    complete_data <- expand.grid(
      year = years,
      birth_country = countries
    )

    full_data <- left_join(complete_data, yearly_grouped, by = c("year", "birth_country"))
    full_data$count[is.na(full_data$count)] <- 0

    result <- full_data %>%
      arrange(birth_country, year) %>%
      group_by(birth_country) %>%
      mutate(
        rolling_avg = rollmean(count, k = 5, fill = NA, align = "right")
      ) %>%
      filter(!is.na(rolling_avg)) %>%
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
      p <- NULL

      if(input$vizType == "time") {
        data <- time_series_data()

        if(nrow(data) == 0) {
          p <- ggplot() +
            annotate("text", x = 0.5, y = 0.5, label = "No data available") +
            theme_void()
        } else {
          p <- ggplot(data, aes(x = year, y = count)) +
            geom_smooth(method = "loess", span = 0.3, color = "#FF5500", linewidth = 1.5, se = TRUE, fill = "#FF5500", alpha = 0.2) +
            geom_point(aes(text = paste0(
                "Year: ", year, "<br>",
                "Awards: ", count
              )), color = "#3366CC", size = 2.5, alpha = 0.8) +
            labs(title = "Nobel Prizes Over Time",
                 subtitle = "With trend line",
                 x = "Year",
                 y = "Number of Awards") +
            theme_minimal() +
            theme(plot.title = element_text(face = "bold", size = 14),
                  plot.subtitle = element_text(size = 11, color = "#666666"),
                  axis.title = element_text(size = 12),
                  panel.grid.minor = element_blank(),
                  panel.grid.major = element_line(color = "#E8E8E8")) +
            scale_y_continuous(breaks = seq(0, 20, by = 2))
        }
      }

      else if(input$vizType == "category") {
        data <- category_data()

        if(nrow(data) == 0) {
          p <- ggplot() +
            annotate("text", x = 0.5, y = 0.5, label = "No data available") +
            theme_void()
        } else {
          category_colors <- c("Chemistry" = "#4285F4", "Economics" = "#EA4335",
                              "Literature" = "#FBBC05", "Medicine" = "#34A853",
                              "Peace" = "#7BAAF7", "Physics" = "#FF6D01")

          p <- ggplot(data, aes(x = reorder(category, -count), y = count, fill = category,
                               text = paste0(
                                 "Category: ", category, "<br>",
                                 "Awards: ", count
                               ))) +
            geom_bar(stat = "identity") +
            labs(title = "Nobel Prizes by Category",
                 x = "Category",
                 y = "Number of Awards") +
            scale_fill_manual(values = category_colors) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  plot.title = element_text(face = "bold", size = 14),
                  axis.title = element_text(size = 12)) +
            theme(legend.position = "none")
        }
      }

      else if(input$vizType == "gender_time") {
        equality_data <- gender_equality_metrics()
        gender_data <- gender_time_data()

        if(nrow(equality_data) == 0) {
          p <- ggplot() +
            annotate("text", x = 0.5, y = 0.5, label = "No data available") +
            theme_void()
        } else {
          p <- ggplot() +
            geom_bar(data = gender_data,
                    aes(x = decade, y = count, fill = sex,
                        text = paste0(
                          "Decade: ", decade, "s<br>",
                          "Gender: ", sex, "<br>",
                          "Count: ", count, "<br>",
                          "Percentage: ", round(percent, 1), "%"
                        )),
                    stat = "identity", position = "stack") +
            geom_line(data = equality_data,
                     aes(x = decade, y = female_percent * 5,
                         text = paste0(
                           "Decade: ", decade, "s<br>",
                           "Female: ", female_percent, "%<br>",
                           "Total prizes: ", total_prizes
                         )),
                     color = "#000000", linewidth = 1.5) +
            geom_point(data = equality_data,
                      aes(x = decade, y = female_percent * 5,
                          text = paste0(
                            "Decade: ", decade, "s<br>",
                            "Female: ", round(female_percent, 1), "%<br>",
                            "Total prizes: ", total_prizes
                          )),
                      color = "#000000", size = 3) +
            geom_hline(yintercept = 50 * 5, linetype = "dashed", color = "#000000", alpha = 0.5) +
            scale_fill_manual(values = c("Female" = "#FF9999", "Male" = "#6699CC", "org" = "#CCCCCC")) +
            scale_y_continuous(
              name = "Number of Laureates",
              sec.axis = sec_axis(~./5, name = "Female Percentage (%)")
            ) +
            labs(title = "Gender Distribution of Nobel Prizes Over Time",
                 subtitle = "Bars show counts by gender, line shows female percentage (right axis)",
                 x = "Decade",
                 fill = "Gender") +
            theme_minimal() +
            theme(plot.title = element_text(face = "bold", size = 14),
                  plot.subtitle = element_text(size = 11, color = "#666666"),
                  axis.title = element_text(size = 12),
                  axis.title.y.right = element_text(color = "#000000"),
                  panel.grid.minor = element_blank())
        }
      }

      else if(input$vizType == "choropleth") {
        world_data <- world_map_data()

        if(all(is.na(world_data$total_award))) {
          p <- ggplot() +
            annotate("text", x = 0.5, y = 0.5, label = "No data available") +
            theme_void()
        } else {
          p <- ggplot(data = world_data) +
            geom_sf(aes(fill = total_award,
                       text = paste0(
                         "Country: ", admin, "<br>",
                         "Awards: ", ifelse(is.na(total_award), 0, total_award)
                       ))) +
            scale_fill_gradient(low = "#c6dbef", high = "#08519c",
                               name = "Number of Laureates",
                               na.value = "lightgrey") +
            labs(title = "Global Distribution of Nobel Prize Laureates",
                 subtitle = paste(input$yearRange[1], "-", input$yearRange[2])) +
            theme_minimal() +
            theme(plot.title = element_text(face = "bold", size = 14),
                  plot.subtitle = element_text(size = 12),
                  legend.position = "bottom")
        }
      }

      else if(input$vizType == "age") {
        data <- age_distribution_data()

        if(nrow(data) == 0) {
          p <- ggplot() +
            annotate("text", x = 0.5, y = 0.5, label = "No data available") +
            theme_void()
        } else {
          category_colors <- c("Chemistry" = "#4285F4", "Economics" = "#EA4335",
                              "Literature" = "#FBBC05", "Medicine" = "#34A853",
                              "Peace" = "#7BAAF7", "Physics" = "#FF6D01")

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
              scale_fill_manual(values = category_colors) +
              theme_minimal() +
              theme(plot.title = element_text(face = "bold", size = 14),
                    axis.title = element_text(size = 12),
                    axis.text.x = element_text(angle = 45, hjust = 1)) +
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
              scale_fill_manual(values = category_colors) +
              theme_minimal() +
              theme(plot.title = element_text(face = "bold", size = 14),
                    axis.title = element_text(size = 12))

          } else {
            trend_data <- age_trends_data()

            p <- ggplot(trend_data, aes(x = decade, y = mean_age,
                                       text = paste0(
                                         "Decade: ", decade, "s<br>",
                                         "Mean Age: ", round(mean_age, 1), "<br>",
                                         "Median Age: ", round(median_age, 1), "<br>",
                                         "Range: ", min_age, "-", max_age
                                       ))) +
              geom_line(color = "#3366CC", linewidth = 1) +
              geom_point(color = "#3366CC", size = 3) +
              labs(title = "Average Age of Nobel Laureates Over Time",
                   x = "Decade",
                   y = "Mean Age at Award") +
              theme_minimal() +
              theme(plot.title = element_text(face = "bold", size = 14),
                    axis.title = element_text(size = 12))
          }
        }
      }

      else if(input$vizType == "country_time") {
        data <- country_time_data()
        events_data <- historical_events()

        if(nrow(data) == 0) {
          p <- ggplot() +
            annotate("text", x = 0.5, y = 0.5, label = "No data available") +
            theme_void()
        } else {
          country_palette <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00",
                             "#FFFF33", "#A65628", "#F781BF", "#999999", "#66C2A5", "#FC8D62", "#8DA0CB")

          p <- ggplot(data, aes(x = year, y = rolling_avg, color = birth_country, group = birth_country,
                               text = paste0(
                                 "Year: ", year, "<br>",
                                 "Country: ", birth_country, "<br>",
                                 "Awards: ", count, "<br>",
                                 "5-Year Avg: ", round(rolling_avg, 2)
                               ))) +
            geom_line(linewidth = 1.2) +
            labs(title = "Countries' Nobel Prize Trends with Historical Context",
                 subtitle = "5-year rolling average with key historical events marked",
                 x = "Year",
                 y = "Awards (5-year rolling average)") +
            scale_color_manual(values = country_palette, name = "Country") +
            theme_minimal() +
            theme(plot.title = element_text(face = "bold", size = 14),
                  plot.subtitle = element_text(size = 11, color = "#666666"),
                  axis.title = element_text(size = 12),
                  legend.position = "right")

          if(nrow(events_data) > 0) {
            event_colors <- c("wars" = "#E41A1C", "economic" = "#4DAF4A",
                             "science" = "#984EA3", "categories" = "#FF7F00")

            p <- p +
              geom_vline(data = events_data,
                       aes(xintercept = year,
                           text = paste0(
                             "Year: ", year, "<br>",
                             "Event: ", event
                           )),
                       linetype = "dashed", color = event_colors[events_data$event_type],
                       alpha = 0.75, linewidth = 1)
          }
        }
      }

      else if(input$vizType == "institutions") {
        data <- institution_data()

        if(nrow(data) == 0) {
          p <- ggplot() +
            annotate("text", x = 0.5, y = 0.5, label = "No institution data available") +
            theme_void()
        } else if(input$normalizeByDecade) {
          p <- ggplot(data, aes(x = decade, y = reorder(organization_name, -count), fill = count,
                               text = paste0(
                                 "Institution: ", organization_name, "<br>",
                                 "Decade: ", decade, "s<br>",
                                 "Laureates: ", count
                               ))) +
            geom_tile(color = "white") +
            scale_fill_gradient(low = "#c6dbef", high = "#08519c", name = "Laureates") +
            labs(title = "Top Institutional Affiliations Over Time",
                 subtitle = paste("Top", input$topInstitutions, "institutions"),
                 x = "Decade",
                 y = "Institution") +
            theme_minimal() +
            theme(plot.title = element_text(face = "bold", size = 14),
                  plot.subtitle = element_text(size = 12),
                  axis.text.y = element_text(size = 10),
                  legend.position = "right")
        } else {
          p <- ggplot(data, aes(x = reorder(organization_name, count), y = count, fill = count,
                               text = paste0(
                                 "Institution: ", organization_name, "<br>",
                                 "Laureates: ", count
                               ))) +
            geom_bar(stat = "identity") +
            coord_flip() +
            scale_fill_gradient(low = "#c6dbef", high = "#08519c", name = "Laureates") +
            labs(title = "Top Institutional Affiliations",
                 subtitle = paste("Top", input$topInstitutions, "institutions"),
                 x = "Institution",
                 y = "Number of Laureates") +
            theme_minimal() +
            theme(plot.title = element_text(face = "bold", size = 14),
                  plot.subtitle = element_text(size = 12)) +
            theme(legend.position = "none")
        }
      }

      if(is.null(p)) {
        p <- ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "Visualization not available") +
          theme_void()
      }

      plt <- ggplotly(p, tooltip = "text")
      plotly::layout(plt, margin = list(l = 50, r = 20, b = 50, t = 50))

    }, error = function(e) {
      message("Error in plot: ", e$message)
      p <- ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = paste("Error in visualization:", e$message)) +
        theme_void()
      ggplotly(p)
    })
  })

  output$comparisonDescription <- renderUI({
    if(input$comparisonType == "gender_category") {
      p("This visualization shows the gender distribution across Nobel Prize categories over the selected time period.")
    } else if(input$comparisonType == "age_gender") {
      p("This visualization compares the age distribution of Nobel laureates by gender.")
    } else if(input$comparisonType == "gender_equality") {
      p("This visualization shows gender equality metrics over time, including female percentage and equality index (100% = perfect equality).")
    }
  })

  output$comparisonPlot <- renderPlotly({
    tryCatch({
      if(input$comparisonType == "gender_category") {
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
          gender_colors <- c("Female" = "#FF9999", "Male" = "#6699CC", "org" = "#CCCCCC")

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
            scale_fill_manual(values = gender_colors) +
            theme_minimal() +
            theme(plot.title = element_text(face = "bold", size = 14),
                  axis.title = element_text(size = 12),
                  axis.text.x = element_text(angle = 45, hjust = 1))
        }

      } else if(input$comparisonType == "age_gender") {
        data <- filtered_data() %>%
          filter(!is.na(age_at_award) & !is.na(sex)) %>%
          filter(age_at_award >= 15 & age_at_award <= 100)

        if(nrow(data) == 0) {
          p <- ggplot() +
            annotate("text", x = 0.5, y = 0.5, label = "No data available") +
            theme_void()
        } else {
          gender_colors <- c("Female" = "#FF9999", "Male" = "#6699CC", "org" = "#CCCCCC")

          p <- ggplot(data, aes(x = sex, y = age_at_award, fill = sex,
                               text = paste0(
                                 "Gender: ", sex, "<br>",
                                 "Age: ", age_at_award, "<br>",
                                 "Laureate: ", full_name, "<br>",
                                 "Category: ", category
                               ))) +
            geom_boxplot() +
            labs(title = "Age Distribution by Gender",
                 x = "Gender",
                 y = "Age at Award") +
            scale_fill_manual(values = gender_colors) +
            theme_minimal() +
            theme(plot.title = element_text(face = "bold", size = 14),
                  axis.title = element_text(size = 12)) +
            theme(legend.position = "none")
        }

      } else if(input$comparisonType == "gender_equality") {
        equality_data <- gender_equality_metrics()

        if(nrow(equality_data) == 0) {
          p <- ggplot() +
            annotate("text", x = 0.5, y = 0.5, label = "No data available") +
            theme_void()
        } else {
          p <- ggplot(equality_data) +
            geom_line(aes(x = decade, y = female_percent, color = "Female %",
                          text = paste0(
                            "Decade: ", decade, "s<br>",
                            "Female %: ", round(female_percent, 1), "%<br>",
                            "Female prizes: ", female_prizes, "<br>",
                            "Total prizes: ", total_prizes
                          )),
                      linewidth = 1.2) +
            geom_point(aes(x = decade, y = female_percent, color = "Female %",
                           text = paste0(
                             "Decade: ", decade, "s<br>",
                             "Female %: ", round(female_percent, 1), "%<br>",
                             "Female prizes: ", female_prizes, "<br>",
                             "Total prizes: ", total_prizes
                           )),
                       size = 3) +
            geom_line(aes(x = decade, y = equality_index * 100, color = "Equality Index",
                          text = paste0(
                            "Decade: ", decade, "s<br>",
                            "Equality Index: ", round(equality_index * 100, 1), "%<br>",
                            "(100% = perfect equality)"
                          )),
                      linewidth = 1.2) +
            geom_point(aes(x = decade, y = equality_index * 100, color = "Equality Index",
                           text = paste0(
                             "Decade: ", decade, "s<br>",
                             "Equality Index: ", round(equality_index * 100, 1), "%<br>",
                             "(100% = perfect equality)"
                           )),
                       size = 3) +
            geom_hline(yintercept = 50, linetype = "dashed", color = "gray50", alpha = 0.7) +
            scale_color_manual(values = c("Female %" = "#FF9999", "Equality Index" = "#6699CC")) +
            labs(title = "Gender Equality Metrics in Nobel Prizes Over Time",
                 subtitle = "Female percentage and equality index (100% = perfect equality)",
                 x = "Decade",
                 y = "Percentage / Index Value",
                 color = "Metric") +
            ylim(0, 100) +
            theme_minimal() +
            theme(plot.title = element_text(face = "bold", size = 14),
                  plot.subtitle = element_text(size = 11, color = "#666666"),
                  axis.title = element_text(size = 12),
                  legend.position = "bottom")
        }
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
