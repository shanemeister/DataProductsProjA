

library(shiny)
library(shinythemes)
shinyUI(fixedPage(theme = shinytheme("spacelab"),
  fluidRow(
        h2("NOAA Storm Data Distribution", align = "center"),
        h6('**See Instructions Below**', align = "center"),
        column(4, wellPanel(
                tags$div(title = "Select Decade of interest, or \'All\' for complete time period",
                        selectInput("decade", "Select Decade: ",
                            choices = DecadeL,
                            selected = 'All')),
                tags$p("Fatalities:"),
                verbatimTextOutput("fatalities")
                ),
               tags$div(title = "Only applicable for Heat Map",
                        radioButtons("jitter", label = ("Heat Map or Locations:"),
                            choices = list('Heat Map' = "heat",
                                           'Storm Location (jitter)' = 'jit'),
                            selected = 'heat')),
               br(),
               tags$div(title = "Uses derived State Center of Mass where NOAA did not capture LONG/LAT readings - makes some Event Casualties visible that would otherwise not be seen, such as Cold",
                        checkboxInput('com', 'Use COM Data:*', value = TRUE)),
               tags$div(title = "Enables manual ZOOM control and LONG/LAT Adjustments",
                        checkboxInput('czoom', 'Enable Zoom (Heat Map Only): ', value = FALSE))
               ),

          column(4, wellPanel(
                  tags$div(title = "Select Region of the U.S.",
                           selectInput("region", "Select Region of U.S.: ",
                              choices = RegionL,
                              selected = 'All')),
                  tags$p("Injuries:"),
                  verbatimTextOutput("injuries")),
                 tags$div(title = "Adjust # bins for density plot on Heat Map (Heat Map Only)",
                          sliderInput('binSize', 'Heat Map Only: Bin Size:',
                        min = 5, max = 15,
                        value = 10,
                        step = 1, round = 0)),
                tags$div(title = "Must check \'Enable Zoom\' first",
                        sliderInput('zoom', 'Heat Map Only: Zoom (Enable Zoom First)',
                                min = 3, max = 10,
                                value = 3,
                                step = 1, round = 0))
             ),
        column(4, wellPanel(
          tags$div(title = "Select Weather Event of interest. NOTE: Events that appear depend on the Decade, and Region selections.",
                selectInput("event", "Select Event: ",
                      choices = EventL,
                      selected = 'All')),
          tags$p("Total Casualties:"),
          verbatimTextOutput("casualties")),
          tags$div(title = "Make sure Zoom is enabled (Heat Map Only)",
          sliderInput("lat", label = 'Heat Map Only: South <-->North',
                min = MinLat, max = MaxLat,
                value = CenterLat, step = 1, sep = "", ticks = FALSE)),
          tags$div(title = "Make sure Zoom is enabled (Heat Map Only)",
          sliderInput("long", label = 'Heat Map Only: West <--> East',
                      min = MinLong, max = MaxLong,
                      value = CenterLong, step = 1, sep = "", ticks = FALSE)),

          tags$head(tags$script(src = "message-handler.js")),
          verbatimTextOutput("error"),
          tags$head(tags$style("#error{color: red;
                               font-size: 12px;
                               font-style: italic;
                               }"
               )),

          actionButton("do", "Reset MSG & LONG/LAT")
          ),


        mainPanel(
                hr(),
                hr(),
                tabsetPanel(type = "pills",
                        tabPanel('Casualty Table', dataTableOutput('mytable3')),
                        tabPanel("Heat Map",plotOutput("maps",height = 800, width = 800)),
                        tabPanel("Casualty Distribution", plotOutput("plot", height = 800, width = 800))))
          ),
  h3('Purpose:'),
  p('The purpose of this application is to quickly show the distribution of storms across the United States
    that resulted in either an injury or fatality according to the ', tags$a(href='http://www.ncdc.noaa.gov/stormevents/', 'U.S. National Oceanic and Atmospheric Administration’s (NOAA)'), 'storm database.'),
  p('The data has gone through a fairly exhaustive ‘tidying’ process, and is presented in the application selectable by any combination of:'),
  br(),
  tags$div(class="header", checked=NA,
           tags$ol(style='list-style-type: decimal',
           list(
                   tags$p("1. Decade (1950s-2010s"),
                   tags$p('2. Region of the U.S. (as defined by the ', tags$a(href="http://www.census.gov/econ/census/help/geography/regions_and_divisions.html", "U.S. Census Bureau"),
                   ")"),
                   tags$p('3. By Storm Event')
  ))
  ),
  br(),
  h3('Instructions:'),
  br(),
  p('The application is fairly straightforward and intuitve. There are three drop down boxes at the top that allows the
    user to select the Decade of Interest, the Region of the United States, and the Weather Event. Once selected, the Total
    Number of Fatalities, Injuries, and Casualties are presented in the boxes below.'),
br(),
p('Below the selection criteria, there are three
tabs that display the data in a tabular format, a heatmap using Google Maps,
and a Casualty Distribution map of the U.S.'),
br(),

tags$ol(
        tags$li("Casualty Table Tab: presents casualties in a searchable format based on State, Decade, Region, Event, Fatalities, Injuries, and Casualties."),
        tags$li("Heat Map Tab: presents the density of events (storms with casualties) based on the selection criteria (Decade, Region, and/or Event)"),
        tags$li("Casualty Distribution Tab: a simpler map that highlights the states by color according to the total number of causalties. This map is interesting because it shows very clearly where NOAA did not record data. States that appear in YELLOW (when ALL Regions are selected) have no storm data in the NOAA Storm Database where casualties occurred. When a specific Region is selected, the states not belonging to that Region will appear in YELLOW as well.")
),

br(),

h3('A couple of caveats about the NOAA data:'),
br(),
tags$ol(
        tags$li('It was not until the 1980s that NOAA recorded, or classified storm fatalities for any event other thant Tornado. Also, I reclassified some EVENTS where NOAA had duplicates which would dilute the number of fatalities from certains types of events. For instance, there were events for TORANDO, and TORNADOES, for HEAT, and EXTREME HEAT, and for COLD, and EXTREME COLD. These were all grouped together in the EVENT TYPES to avoid diluting the results for that given event.'),
        tags$li('For many of the storms, no specific location was provided (no longitude/latitude). In these cases, rather than excluding the casualties, the location was taken to be the CENTER of the State in which the event occurred. In some instances, I am sure the reason for missing locations was that the event was wide spread and a location other than the area/state would be inappropriate. For instance, a cold weather event would not have a specific longititude and latitude reading, but would rather would occur over a wide area. Using the CENTER OF MASS for location, causes the Heat Map to show more density of storms toward the center of the states. Deselecting the COM check box removes data where the Center of State was the only location available.'),
        tags$li('The ZOOM, BIN SIZE, LONGITUDE, and LATITUDE sliders only work when ENABLE ZOOM is checked and the HEAT MAP tab is selected. If you get lost, simply click on the \'Reset LONG/LAT/Msg\' button.'),
        tags$li('There is a sparcity of data, and surprisingly incomplete. For instance, there is no evidence I could find of the Katrina Hurricane in 2005 that hit New Orleans where numerous people were injured or killed. I guess this demonstrates the importance of the data collection process.')
),
br(),
tags$p('*CAUTION: If you have an EVENT selected that only has CENTER OF MASS data (such as a Cold Weather Event), and you deselect the COM checkbox, an error will occur. I believe that I have adequately captured these errors, and provide warning messages. Feedback on any uncaptured errors would be apprecited. Worse case, you should be able to refresh the page and reinitialize the application.'),
br(),
hr()

        )


)

