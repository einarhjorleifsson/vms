# data prep
library(sf)
library(lubridate)
library(tidyverse)
library(mar)
library(leaflet)

year <- 2018

# Gear colours to use in the time graph
gid <- tibble(gid = c(6, 9, 14, 15, 38, 40),
              name = c("fish", "lobster", "shrimp", "Chlamys", "Cyprine", "urchins"),
              colour = c("yellow", "red", "orange", "cyan", "grey", "pink"))
gid2 <- gid %>% select(name, colour)

con <- connect_mar()

# Logbook data
tows.all <-
  afli_stofn(con) %>%
  filter(ar %in% year,
         veidarf %in% gid$gid) %>%
  select(visir, skipnr, veidarf, vedags, ldags) %>%
  left_join(afli_toga(con) %>%
              select(visir, ibotni, togtimi),
            by = "visir") %>%
  rename(gid = veidarf,
         vid = skipnr) %>%
  collect(n = Inf) %>%
  mutate(hhmm = str_pad(ibotni, 4, "left", pad = "0"),
         hhmm = paste0(str_sub(hhmm, 1, 2),
                       ":",
                       str_sub(hhmm, 3, 4)),
         t1 = ymd_hm(paste(as.character(vedags), hhmm)),
         t2 = t1 + minutes(togtimi)) %>%
  left_join(lesa_skipaskra(con) %>%
              select(vid = skip_nr, name = heiti) %>%
              collect(n = Inf))
#trip <-
#  tows.all %>%
#  select(vid, ldags) %>%
#  distinct() %>%
#  arrange(vid, ldags) %>%
#  group_by(vid) %>%
#  mutate(trip = 1:n()) %>%
#  ungroup()

# Get the vms corresponding to the selected vessels from the logbook data
VID <- as.character(unique(tows.all$vid))
vms.raw <-
  vms(con, year) %>%
  filter(vid %in% VID,
         between(lon, -179, 179),
         between(lat, -89, 89),
         is.na(in_out_of_harbor)) %>%
  #select(-c(rectime, hid, in_out_of_harbor)) %>%
  distinct() %>%
  collect(n = Inf) %>%
  arrange(mobileid, date) %>%
  select(vid, date, speed, lon, lat) %>%
  mutate(vid = as.integer(vid)) %>%
  st_as_sf(coords = c("lon", "lat"),
           remove = FALSE,
           crs = 4326) %>%
  mutate(speedr = as.integer(round(speed)))

vessels <-
  tows.all %>%
  select(vid, name) %>%
  distinct()

vessels.choice <- vessels$vid
names(vessels.choice) <- vessels$name


iceland <- gisland::read_sf_ftp("iceland")

library(shiny)
library(xts)
library(dygraphs)
library(geo)
library(leaflet)
library(mapview)
library(mapdeck)
key <- "pk.eyJ1IjoiZmlzaHZpY2UiLCJhIjoiY2p0MXQ5dThpMDZqeDQ5bHM0MGx4dHI5cyJ9.Fed_z7mv_TgTWDRjiavU3A"
set_token(key)


ui <- fluidPage(

  sidebarLayout(

    sidebarPanel(
      selectInput(inputId = "Vessel", label = "Vessel:",
                  choices = vessels.choice, selected = 1277),
      #actionButton(inputId = "go",
      #             label = "Update"),
      hr(),
      div(strong("From: "), textOutput("from", inline = TRUE)),
      div(strong("To: "), textOutput("to", inline = TRUE)),
      br(),
      width = 2,
      actionButton(inputId = "go",
                   label = "Update"),
      br(),
      p(gid2[1,]),
      p(gid2[2,]),
      p(gid2[3,]),
      p(gid2[4,]),
      p(gid2[5,]),
      p(gid2[6,])),


    mainPanel(
      p("Time"),
      dygraphOutput(outputId = "dygraph",
                    width = 1000, height = 200),
      p("Space"),
      mapdeckOutput("space",
                    width = 1000,
                    height = 350)
    )
  ))

server <- function(input, output) {

  rd.xts <- reactive({
    i <- vms.raw$vid == input$Vessel
    xts(vms.raw$speed[i], order.by = vms.raw$date[i])
    #tow <- tows.all %>% filter(vid == input$Vessel)
  })
  rd.tows <- reactive({
    tows <- tows.all %>% filter(vid == input$Vessel)
  })
  rd.space <- reactive({
    vms.raw %>% filter(vid == input$Vessel)
  })

  output$dygraph <- renderDygraph(
    expr = {
      dg <-
        dygraph(rd.xts()) %>%
        dyRangeSelector(dateWindow = c("2018-01-01", "2018-12-31")) %>%
        dyOptions(stepPlot = TRUE) %>%
        dyAxis(name = 'y', label = 'Speed', valueRange = c(0, 14))
      for(j in 1:nrow(rd.tows())) {
        my.col <- gid %>% filter(gid == rd.tows()$gid[j]) %>% pull(colour)
        dg <-
          dg %>%
          dyShading(from = as.character(rd.tows()$t1[j]),
                    to = as.character(rd.tows()$t2[j]), color = my.col)
      }
      dg
    })

  # Display time
  output$from <- renderText({
    strftime(req(input$dygraph_date_window[[1]]), "%d %b %Y")
  })
  output$to <- renderText({
    strftime(req(input$dygraph_date_window[[2]]), "%d %b %Y")
  })

  # For use in reactive leaflet - not working
  output$from2 <-
    eventReactive(input$go,
                  {req(input$dygraph_date_window[[1]])})
  output$to2 <-
    eventReactive(input$go,
                  {req(input$dygraph_date_window[[2]])})

  output$space <- renderMapdeck({
      mapdeck(style = mapdeck_style('dark'))
    })

  observeEvent({input$go}, {
    if(is.null(input$go)) return()
    x <- rd.space() %>%
      filter(date >= req(input$dygraph_date_window[[1]]),
             date <= req(input$dygraph_date_window[[2]])) %>%
      select(speedr) %>%
      mutate(speedr = -speedr)
    if(nrow(x) == 0) return(NULL)

    mapdeck_update(map_id = 'space') %>%
      add_scatterplot(data = x, fill_colour = "speedr",
                      layer_id = "track")
  })

}

shinyApp(ui, server)
