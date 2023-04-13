library(readr) # read_csv
library(rgdal) # read shapefiles
library(shiny) # interactive shinyapp
library(dplyr) # data manipulation
library(leaflet) # interactive maps
library(RColorBrewer) # color palettes
library(stringr) # string manipulation
library(r2r) # hashmap
library(rmapshaper) # map simplification
library(plotly) # interactive plots

# Download world shape data if doesn't exist
if (!file.exists("world_shape_file")) {
    if (!file.exists("world_shape_file.zip")) {
    download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip", destfile = "world_shape_file.zip")
    }
    system("unzip world_shape_file.zip -d world_shape_file")
}

# Read data
covid <- read_csv("owid-covid-data.csv")
world_spdf <- readOGR(
    dsn = paste0(getwd(), "/world_shape_file/"),
    layer = "TM_WORLD_BORDERS_SIMPL-0.3",
    verbose = FALSE
)
gdp <- read_csv("gdp_per_capita.csv")

# Clean the data
covid <- covid[, c(1, 3:6, 8:9)]
covid[is.na(covid)] <- 0
covid <- subset(covid, !str_detect(iso_code, "OWID")) # not a country
world_spdf <- ms_simplify(world_spdf, keep = 0.5, keep_shapes = TRUE)
gdp <- gdp %>% rename(code = "Code", country = "Country Name")
gdp[is.na(gdp)] <- 0
# THIS TAKES A WHILE
# convert columns from [country, code, 1960, 1961, 1962, ...] to [country, code, date, value]
temp_gdp <- data.frame(country = character(), code = character(), date = as.Date(character()), value = double())
for (i in 1:nrow(gdp)) { # nolint
    for (year in 1960:2020) {
        temp_gdp <- temp_gdp %>% add_row(country = gdp$country[i], code = gdp$code[i], date = as.Date(paste0(year, "-01-01")), value = as.double(gdp[as.character(year)][i, 1]))
    }
}
gdp <- temp_gdp
rm(temp_gdp)

# get global average gdp
avg_gdp <- subset(gdp, value != 0) %>%
    group_by(date) %>%
    summarise(avg = mean(value)) # nolint

# UI
ui <- fluidPage(
    titlePanel("Choropleth of Covid-19"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("date", "Date", min = min(covid$date), max = max(covid$date), value = min(covid$date)),
            selectInput("column", "View", choices = c("total_cases", "new_cases", "total_deaths", "new_deaths")),
            numericInput("days", "Animate days", value = 0, min = 0, max = 10)
        ),
        mainPanel(
            leafletOutput("map")
        )
    ),
    titlePanel("Stats of Covid-19 by Country"),
    sidebarLayout(
        sidebarPanel(
            selectInput("country", "Country", choices = unique(covid$location), multiple = TRUE),
            selectInput("trace", "View", choices = c("total_cases", "new_cases", "total_deaths", "new_deaths"))
        ),
        mainPanel(
            plotlyOutput("stats")
        )
    ),
    titlePanel("GDP per capita"),
    sidebarLayout(
        sidebarPanel(
            selectInput("gdpCountry", "Country", choices = unique(gdp$country), multiple = TRUE),
        ),
        mainPanel(
            plotlyOutput("gdp")
        )
    )
)

# Profiling
time_spent <- function(last, text) {
    print(sprintf("%-12s: %.2fms", text, abs(as.numeric(Sys.time()) * 1000 - last)))
    current_time()
}

current_time <- function() {
    as.numeric(Sys.time()) * 1000
}

# server
server <- function(input, output, session) {
    # Map animation
    observe({
        req(input$days)
        if (input$days > 0) {
            updateSliderInput(session, "date", value = input$date + input$days)
        }
    })
    output$map <- renderLeaflet({
        # profiling time
        last <- time_spent(current_time(), "Start Map")

        # Subset data
        filtered_covid <- subset(covid, date == input$date)
        last <- time_spent(last, "Subset")

        # Generate hashmap for faster lookup
        # Though, it is miniscule compared to the leaflet itself (375ms out of 400ms is spent on leaflet)
        # With map simplification, leaflet rendering is 5x faster
        covid_hashmap <- hashmap(default = 0)
        for (i in 1:nrow(filtered_covid)) { # nolint
            covid_hashmap[[filtered_covid$iso_code[i]]] <- as.numeric(filtered_covid[input$column][i, 1])
        }
        last <- time_spent(last, "Hashmap")

        # Modify the original data$POP2005 because I couldn't get the map working with external data
        world_spdf@data$POP2005 <- 0
        for (i in 1:length(world_spdf@data$ISO3)) { # nolint
            world_spdf@data$POP2005[i] <- covid_hashmap[[world_spdf@data$ISO3[i]]]
        }
        last <- time_spent(last, "Modifying")

        # Color palette
        bins <- switch(input$column,
               "total_cases" = c(0, 1000, 5000, 10000, 50000, 100000, 500000, 10000000, 50000000, Inf),
               "new_cases" = c(0, 100, 500, 1000, 5000, 10000, 50000, 100000, 500000, Inf),
               "total_deaths" = c(0, 500, 1000, 5000, 10000, 50000, 100000, 500000, Inf),
               "new_deaths" = c(0, 10, 50, 100, 200, 500, 1000, 5000, 10000, Inf))
        mypalette <- colorBin(palette = "Blues", domain = world_spdf@data$POP2005, na.color = "transparent", bins = bins)
        last <- time_spent(last, "Color")

        # Label on hover
        mytext <- paste(
            "Country: ", world_spdf@data$NAME, "<br/>",
            "Area: ", world_spdf@data$AREA, "<br/>",
            input$column, ": ", format(world_spdf@data$POP2005, big.mark = ","),
            sep = "") %>%
            lapply(htmltools::HTML)
        last <- time_spent(last, "Label")

        # Plot
        plot <- leaflet(world_spdf) %>%
                addTiles() %>%
                setView(lat = 25, lng = 0, zoom = 1) %>%
                addPolygons(
                    fillColor = ~mypalette(POP2005),
                    stroke = TRUE,
                    fillOpacity = 0.9,
                    color = "white",
                    weight = 0.3,
                    label = mytext,
                    labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "13px",
                        direction = "auto"
                    )
                ) %>%
                addLegend(pal = mypalette, values = ~POP2005, opacity = 0.9, title = input$column, position = "bottomleft")
        last <- time_spent(last, "Plot")
        plot
    })

    # Covid stats graph by country
    output$stats <- renderPlotly({
        # profiling time
        last <- time_spent(current_time(), "Start Stats")

        # filter based on country
        filtered_covid <- subset(covid, location %in% input$country)

        # trace
        trace <- switch(input$trace, # nolint
            "total_cases" = filtered_covid$total_cases,
            "new_cases" = filtered_covid$new_cases,
            "total_deaths" = filtered_covid$total_deaths,
            "new_deaths" = filtered_covid$new_deaths
        )

        # title
        title <- switch(input$trace,
            "total_cases" = "Total Cases",
            "new_cases" = "New Cases",
            "total_deaths" = "Total Deaths",
            "new_deaths" = "New Deaths"
        )

        # plot by selected countries with legends
        plot <- plot_ly(filtered_covid, x = ~date, y = ~trace, color = ~location, type = "scatter", mode = "lines") %>%
            layout(title = title, xaxis = list(title = "Date"), yaxis = list(title = title))

        last <- time_spent(last, "Plot")
        plot
    })

    # GDP per capita graph by country
    output$gdp <- renderPlotly({
        # profiling time
        last <- time_spent(current_time(), "Start GDP")
        plot <- NULL

        if (length(input$gdpCountry) > 0) {
            # filter based on country
            filtered_gdp <- subset(gdp, country %in% input$gdpCountry) # nolint

            # plot by selected countries with legends
            plot <- plot_ly(filtered_gdp, x = ~date, y = ~value, color = ~country, type = "scatter", mode = "lines") %>%
                layout(title = "GDP per capita", xaxis = list(title = "Date"), yaxis = list(title = "GDP per capita"))

        } else {
            plot <- plot_ly(avg_gdp, x = ~date, y = ~avg, type = "scatter", mode = "lines") %>%
                layout(title = "Global Average GDP per Capita", xaxis = list(title = "Date"), yaxis = list(title = "GDP per capita"))
        }
        last <- time_spent(last, "Plot")
        plot
    })
}

shinyApp(ui, server)
