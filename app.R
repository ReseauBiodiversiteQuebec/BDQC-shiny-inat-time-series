#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(sf)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Phenologie"),

    # regionplot
    fluidRow(
        column(12,
               plotOutput("regionplot")
        )
    ),

    fluidRow(
        column(12,
               plotOutput("ganttplot")
        )
    ),

    fluidRow(
        column(12,
               plotOutput("countplot")
        )
    )
)

# select site and time series
server <- function(input, output) {

    regio <- st_read("iNatQC/regio_s.shp")

    output$regionplot <- renderPlot({
        regio %>%
            ggplot() + geom_sf() + theme_void()
    })



    inatqc <- readr::read_csv("iNatQC/iNatQC.csv")



    # top species in each region

    top10species <- inatqc %>%
        group_by(region, taxon_species_name) %>%
        tally %>%
        arrange(region,desc(n)) %>%
        nest %>%
        mutate(top10 = map(data, head, 10)) %>%
        select(-data) %>%
        unnest(top10)


    count_taxa <- inatqc %>% mutate(julianday = lubridate::yday(observed_on)) %>%
        group_by(region, taxon_species_name, julianday) %>% tally

    count_taxa %>%
        semi_join(top10species %>% select(-n)) %>%
        ggplot(aes(x = julianday, y = n)) +
        geom_point() +
        facet_wrap(~region)

    chosen_species_range_days <- count_taxa %>%
        semi_join(top10species %>% select(-n)) %>%
        filter(region == "Mauricie") %>%
        summarize(jday = range(julianday)) %>%
        mutate(dayname = if_else(jday == min(jday), "start", "end")) %>%
        ungroup



    # count days in the "range" for each species
    nper_day <- chosen_species_range_days %>%
        pivot_wider(names_from = dayname, values_from = jday) %>%
        mutate(dayrange = map2(start, end, ~.x:.y)) %>%
        select(dayrange) %>%
        unnest(cols = c(dayrange)) %>%
        group_by(dayrange) %>% tally %>%
        # fill in missing days:
        right_join(tibble(dayrange = 1:365)) %>%
        replace_na(list(n = 0)) %>%
        arrange(dayrange)


    output$ganttplot <- renderPlot({
        chosen_species_range_days %>%
            ggplot(aes(x = jday, y = taxon_species_name)) +
            geom_line(size = 10, col = "darkgreen") +
            theme_minimal() +
            coord_cartesian(xlim = c(0,365))
        })


    output$countplot <- renderPlot({
        nper_day %>%
            ggplot(aes(x = dayrange, y = n)) + geom_polygon() + theme_minimal()
    })

}

# Run the application
shinyApp(ui = ui, server = server)
