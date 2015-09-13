
shinyServer(function(input, output, session) {
  v <- reactiveValues()
  v$com = FALSE
  v$event = 'All'
  v$decade = 'All'
  v$region = 'All'
  v$zoom = 3 #value to set initial zoom level
  v$czoom = FALSE #value setting manual zoom changes off
  v$long = CenterLong # Mean of Longitude readings for US
  v$lat = CenterLat # Mean for Latitude readings for US
  comDataOnly <- c('Cold', 'Heat', 'Hurricane', 'OceanCurrent', 'Wildfire')

  observe({
    v$decade = input$decade
    v$region = input$region
    v$event  = input$event
    v$com = input$com #value setting use of COM data
    v$zoom = input$zoom #value of zoom when czoom is TRUE
    v$czoom = input$czoom # VALUE USED TO MANUAL ZOOM CONTROL ON
    v$long = input$long # value of Longitude when manual is on
    v$lat = input$lat # value of Latitude when manual is on

    EventT <- GetSummaryData(v$decade, v$region, v$event, v$com)
    DecadeL <- GetDecade(v$region, v$event, v$com)
    RegionL <- GetRegion(v$decade, v$event, v$com)
    EventL <- GetEvent(v$decade, v$region, v$com)

    if (!(v$event %in% EventL) || !(v$region %in% RegionL) || !(v$decade %in% DecadeL)) {
            validate(
                    need((v$event %in% EventL && v$region %in% RegionL && v$decade %in% DecadeL),
                         'Certain events, like \'Cold\', only have Center of Mass data.\nPlease select an event from the drop down list above.'
                    ))

            output$error <- renderText({
                    '1Certain events, like \'Cold\', only \nhave Center of Mass data.\nPlease select an event from the \ndrop down list above.'
            })
    }
    Fatal <- sum(EventT$Fatalities)
    Injury <- sum(EventT$Injuries)
    Casualties <- sum(EventT$TotalCasualties)

    updateSelectInput(session, "decade",
                      choices = DecadeL,
                      selected = v$decade)

    updateSelectInput(session, "region",
                      choices = RegionL,
                      selected = v$region)

    updateSelectInput(session, "event",
               choices = EventL,
               selected = v$event)

    if (!v$czoom){
        v$long <- get(paste(v$region,'X', sep = ''))
        updateSliderInput(session, "long", value = v$long)
        v$lat <- get(paste(v$region,'Y', sep = ''))
        updateSliderInput(session, "lat", value = v$lat)
    }

    output$fatalities <- renderText({
           Fatal
    })

    output$injuries <- renderText({
           Injury
    })

    output$casualties <- renderText({
           Casualties
    })
  })

 observeEvent(v$region, {
          if (v$czoom){
                  v$long <- get(paste(v$region,'X', sep = ''))
                  updateSliderInput(session, "long", value = v$long)
                  v$lat <- get(paste(v$region,'Y', sep = ''))
                  updateSliderInput(session, "lat", value = v$lat)
          }
 })

 observeEvent(v$long, {
         if (v$czoom){
                 v$long <- v$long
                 } else {
                         v$long <- get(paste(v$region,'X', sep = ''))
                         updateSliderInput(session, "long", value = v$long)
                 }
 })

 observeEvent(v$lat, {
         if (v$czoom){
                 v$lat <- v$lat
         } else {
                 v$lat <- get(paste(v$region,'Y', sep = ''))
                 updateSliderInput(session, "lat", value = v$lat)
         }
 })
  observeEvent(input$do, {

                output$error <- renderText({
                        NULL
                })
                v$long <- get(paste(v$region,'X', sep = ''))
                updateSliderInput(session, "long", value = v$long)
                v$lat <- get(paste(v$region,'Y', sep = ''))
                updateSliderInput(session, "lat", value = v$lat)
 })

observe({
        validate(
                  need((v$event %in% EventL && v$region %in% RegionL && v$decade %in% DecadeL),
                       'Certain events, like \'Cold\', only have Center of Mass data.\nPlease select an event from the drop down list above.'
                  ))
        output$mytable3 <- renderDataTable({
                          updateCheckboxInput(session, "com",value = TRUE)
                          if (v$event %in% comDataOnly && v$com == FALSE){
                                  output$error <- renderText({
                                          'Certain events, like \'Cold\', only \nhave Center of Mass data.\nPlease select Event \'All\' from the \ndrop down list above then \nuncheck \'Use COM Data\' check box.'

                                  })
                          }

                          validate(
                                  need((v$event %in% EventL && v$region %in% RegionL && v$decade %in% DecadeL),
                                       'Certain events, like \'Cold\', only \nhave Center of Mass data.\nPlease select Event \'All\' from the \ndrop down list above then \nuncheck \'Use COM Data\' check box.'
                                  ))

                          SummaryData <- GetJitterData(v$decade, v$region, v$event, com = TRUE)
                          SummaryData <- select(SummaryData, -Long, -Lat, -COMUsed)%>%
                                  group_by(State, Decade, Region, Event)%>%
                                  summarise(sum(Fatalities), sum(Injuries), sum(TotalCasualties))
                          names(SummaryData)[5:7] <- c('Fatalities','Injuries','Casualties,')
                          SummaryData <- top_n(SummaryData,n = 1)
                          SummaryData  } , options = list(lengthMenu = c(5, 10, 15, 20), pageLength = 15), searchDelay = 1000)

         output$maps <- renderPlot({


                    HeatMapJittData <- GetJitterData(v$decade, v$region, v$event, v$com)

                    if (v$event %in% comDataOnly && v$com == FALSE){
                            output$error <- renderText({
                                    '*Certain events, like \'Cold\', only \nhave Center of Mass data.\nPlease select Event \'All\' from the \ndrop down list above then \nuncheck \'Use COM Data\' check box.'

                            })
                        updateCheckboxInput(session, "com",value = TRUE)

                    }

                    validate(
                            need(length(HeatMapJittData) > 0,
                                 '***Certain events, like \'Cold\', only \nhave Center of Mass data.\nPlease select Event \'All\' from the \ndrop down list above then \nuncheck \'Use COM Data\' check box.'
                            ))
                    if (v$czoom){
                                bbox <- c(x = v$long, y = v$lat)
                                zoom1 = v$zoom
                        } else {
                                bbox <- c(left = MAPS[MAPS$Region == v$region,5], bottom = MAPS[MAPS$Region == v$region,3],
                                        right = MAPS[MAPS$Region == v$region,4], top = MAPS[MAPS$Region == v$region,2])
                                zoom1 <- calc_zoom(bbox)
                                if(zoom1 > 6){zoom1 <- zoom1 - 1}
                                updateSliderInput(session, "zoom", value = zoom1)
                        }



                        Map_Plot <-  NULL
                        mapBox <- get_map(location = bbox,
                                maptype = "terrain", zoom = zoom1,
                                messaging = FALSE,
                                api_key = '<YOUR API KEY GOES HERE>')

                        p <-  ggmap(mapBox, extent = "device")
                        if (input$jitter == 'heat') {
                               p <- p + geom_density2d(data = HeatMapJittData,
                               aes(x = Long, y = Lat, position = "identity", na.rm = TRUE), size = 0.3) +
                               stat_density2d(data = HeatMapJittData, aes(x = Long, y = Lat, fill = ..level..,
                               alpha = ..level..), size = 0.5, bins = input$binSize,
                               geom = "polygon") +
                               scale_fill_gradient(low = "green", high = "red") +
                               scale_alpha(range = c(0,0.75), guide = FALSE)
                        }
                        if (input$jitter == 'jit'){
                                p <- p + geom_jitter(data = HeatMapJittData, position=position_jitter(width= 0.5,
                                              height= 0.5),  aes(x = Long, y = Lat,
                                              size = TotalCasualties, color = Event, alpha = 0.25)) +
                                              scale_size(name = 'TotalCasualties') +
                                              scale_colour_brewer(palette='Set1','Event')
                        }
                        p <- p + theme( axis.line = element_line(colour = "black"),
                                        axis.ticks = element_line( color = "blue", size =.03),
                                        axis.text = element_text( angle = 90),
                                        legend.position = 'none',
                                        panel.background = element_rect(fill = "whitesmoke"))
                        p
                })

                output$plot <- renderPlot({

                        SummaryData <- GetSummaryData(v$decade, v$region, v$event, v$com)

                        if (length(SummaryData) <= 0){
                                output$error <- renderText({
                                        'MSG: Certain events, like \'Cold\', \nonly have Center of Mass data.\nPlease select Event \'All\' from the \ndrop down list above before \nunchecking \'Use COM Data\' check box.'

                                })
                        #        updateCheckboxInput(session, "com",value = TRUE)

                        }

                        validate(
                                need(length(SummaryData) > 0,
                                     'Certain events, like \'Cold\', only \nhave Center of Mass data.\nPlease select Event \'All\' from the \ndrop down list above before \nunchecking \'Use COM Data\' check box.'
                                ))

                        us_state_map <- inner_join(us_state_map, SummaryData, by = 'State')
                        us_state_map <- arrange(us_state_map, order)
                        p1 <- ggplot()
                        p1 <- p1 + geom_polygon( data=all_states, aes(x=long, y=lat, group = group),
                                        colour="black", fill="yellow" )
                        p1 <- p1 + geom_polygon(data = us_state_map, aes(x = long, y = lat, group = group,
                                                fill= cut_interval(TotalCasualties,n = 5)), colour="black")
                        p1 <- p1 + geom_path(colour = 'black', linestyle = 2)
                        p1 <- p1 + scale_fill_brewer('Total Casualties Storm Data', palette  = 'PuRd')
                        p1 <- p1 + coord_map()
                        p1 <- p1 + geom_text(data = states, aes(x = x, y = y, label = state.abb, group = NULL), size = 2)
                        p1 <- p1 + theme_bw()
                        p1
                        })
        })
})




