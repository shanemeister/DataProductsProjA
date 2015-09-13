library(shiny)
library(lubridate)
require(ggplot2)
library(ggmap)
library(dplyr)
library(maps)



NOAA <- read.csv("./data/COM.csv", header=TRUE, stringsAsFactors=FALSE)
NOAA <- arrange(NOAA, desc(TotalCasualties))

All_NOAAp <- NOAA[, c('State', 'Decade', 'Region', 'Event', 'Lat', 'Long', 
                'TotalFatalities', 'TotalInjuries', 'TotalCasualties', 'COMUsed')]
names(All_NOAAp) <- c('State', 'Decade', 'Region', 'Event', 'Lat', 'Long', 
                      'Fatalities', 'Injuries', 'TotalCasualties', 'COMUsed')
NOAAp <- All_NOAAp


################# Maps
# 207,020 events are cold related, and of those none contain LAT/LONG readings --
# just the state data. Therefore it is necessary to use COM when looking at this data.

All <- select(NOAA, Lat, Long)%>%
    summarise(max(Lat), min(Lat), max(Long), min(Long))
All$Region  <- 'All'
names(All)  <-   c('top_lat','bottom_lat','right_long', 'left_long', 'Region')
All <- select(All, c(5, 1:4))
MAPS <- select(NOAA, Region, Lat, Long)%>%
        group_by(Region)%>%
        summarise(max(Lat), min(Lat), max(Long), min(Long))
names(MAPS)  <-   c('Region','top_lat','bottom_lat','right_long', 'left_long')
MAPS <- rbind(All, MAPS)

MaxLong <- max(All_NOAAp$Long)
MinLong <- min(All_NOAAp$Long)

MaxLat <- max(All_NOAAp$Lat)
MinLat <- min(All_NOAAp$Lat)

CenterLong <- mean(All_NOAAp$Long)
CenterLat <- mean(All_NOAAp$Lat)

All50 <- data.frame(ST = c("CT", "ME", "MA", "NH", "RI", "VT","NJ", "NY", 
                           "PA","IL", "IN", "MI", "OH", "WI","IA", "KS", "MN", "MO", "NE", 
                           "ND", "SD","DE", "FL", "GA", "MD", "NC", "SC", "VA", "WV",
                           "AL","KY", "MS", "TN","AR", "LA", "OK", "TX","AZ", "CO", "ID", 
                           "MT", "NV", "NM", "UT", "WY", "AK", "CA", "HI", "OR","WA"),  
                    State = c("connecticut", "maine", "massachusetts", "new hampshire", 
                              "rhode island", "vermont", "new jersey", "new york", "pennsylvania", 
                              "illinois", "indiana", "michigan", "ohio", "wisconsin", "iowa", 
                              "kansas", "minnesota", "missouri", "nebraska", "north dakota", 
                              "south dakota", "delaware", "florida", "georgia", "maryland", 
                              "north carolina", "south carolina", "virginia",  
                              "west virginia", "alabama", "kentucky", "mississippi", "tennessee", 
                              "arkansas", "louisiana", "oklahoma", "texas", "arizona", "colorado", 
                              "idaho", "montana", "nevada", "new mexico", "utah", "wyoming", 
                              "alaska", "california", "hawaii", "oregon", "washington"), 
                    stringsAsFactors = FALSE)

NewEngland <- c("CT", "ME", "MA", "NH", "RI", "VT")
MidAtlantic <- c("NJ", "NY", "PA")
MidwestEastNC <- c("IL", "IN", "MI", "OH", "WI")
MidwestWestNC <- c("IA", "KS", "MN", "MO", "NE", "ND", "SD")
SouthSouthAtlantic <- c("DE", "FL", "GA", "MD", "NC", "SC", "VA", "DC", "WV")
SouthEastSC <- c("AL","KY", "MS", "TN")
SouthWestSC <- c("AR", "LA", "OK", "TX")
WestMountain <- c("AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY")
WestPacific <- c("AK", "CA", "HI", "OR","WA")

AllX <- mean(state.center$x[state.abb == All50[1]])
NewEnglandX <- mean(state.center$x[state.abb %in% NewEngland])
MidAtlanticX <- mean(state.center$x[state.abb %in% MidAtlantic])
MidwestEastNCX <- mean(state.center$x[state.abb %in% MidwestEastNC])
MidwestWestNCX <- mean(state.center$x[state.abb %in% MidwestWestNC])
SouthSouthAtlanticX <- mean(state.center$x[state.abb %in% SouthSouthAtlantic])
SouthEastSCX <- mean(state.center$x[state.abb %in% SouthEastSC])
SouthWestSCX <- mean(state.center$x[state.abb %in% SouthWestSC])
WestMountainX <- mean(state.center$x[state.abb %in% WestMountain])
WestPacificX <- mean(state.center$x[state.abb %in% WestPacific])


AllY <- mean(state.center$y[state.abb == All50[1]])
NewEnglandY <- mean(state.center$y[state.abb %in% NewEngland])
MidAtlanticY <- mean(state.center$y[state.abb %in% MidAtlantic])
MidwestEastNCY <- mean(state.center$y[state.abb %in% MidwestEastNC])
MidwestWestNCY <- mean(state.center$y[state.abb %in% MidwestWestNC])
SouthSouthAtlanticY <- mean(state.center$y[state.abb %in% SouthSouthAtlantic])
SouthEastSCY <- mean(state.center$y[state.abb %in% SouthEastSC])
SouthWestSCY <- mean(state.center$y[state.abb %in% SouthWestSC])
WestMountainY <- mean(state.center$y[state.abb %in% WestMountain])
WestPacificY <- mean(state.center$y[state.abb %in% WestPacific])

Map_Plot <- MAPS[MAPS$Region == 'All',]
############### End Maps
CasualtiesByCOM <- select(All_NOAAp, Decade, Region, Event, Fatalities,
                          Injuries, TotalCasualties, COMUsed)%>%
        group_by(Decade, Region, Event, COMUsed)%>%
        summarise(sum(Fatalities), sum(Injuries), sum(TotalCasualties))
names(CasualtiesByCOM) <- c('Decade','Region', 'Event', 'COMUsed', 'Fatalities', 
                            'Injuries', 'TotalCasualties')
CasualtiesByCOM <- ungroup(CasualtiesByCOM)

DecadeL <- c("All", '1950s', '1960s', '1970s', '1980s', '1990s','2000s','2010s')

RegionL <- c('All','NewEngland','MidAtlantic','MidwestEastNC',
             'MidwestWestNC','SouthSouthAtlantic','SouthEastSC','SouthWestSC',
             'WestMountain','WestPacific')

EventL <- c("All",'Cold','Flood','Hail','Heat','Hurricane','Lightning',
            'OceanCurrent','OTHER','Tornado','Wildfire')

Fatal <- sum(CasualtiesByCOM$Fatalities)
Injury <- sum(CasualtiesByCOM$Injuries)
Casualties <- sum(CasualtiesByCOM$TotalCasualties)

start_Data <- All_NOAAp

all_states <- map_data("state")
us_state_map <- map_data('state')
us_state_map$region <- toupper(us_state_map$region) 
names(us_state_map)[5] <- 'State'

states <- data.frame(state.center, state.abb)

SummaryData  <- select (NOAA, everything())%>% 
        group_by(month(BeginDateTime,label = TRUE, abbr = TRUE), Decade, State, Region, Event)%>% 
        tally(TotalCasualties)%>% 
        top_n(1)
names(SummaryData) <- c('Month', 'Decade', 'State', 
                        'Region', 'Event', 'Casualties')


GetSummaryData <- function(decade = 'All', region = 'All', event = 'All', com = NULL){
        if (is.null(com) || com == FALSE){
                (NOAAp <- filter(All_NOAAp, COMUsed == FALSE))
        } else(NOAAp <- All_NOAAp)
        
        if(decade == 'All' && region == 'All' && event == 'All'){
                cdata <- select(NOAAp, State, Injuries, Fatalities, TotalCasualties)%>%
                        group_by(State)%>%
                        summarise(sum(Fatalities), sum(Injuries), sum(TotalCasualties))
                names(cdata)[2:4] <- c('Fatalities','Injuries', 'TotalCasualties')
        }
        if(decade == 'All' && region == 'All' && event != 'All'){
                cdata <- select(NOAAp, State, Event, Injuries, Fatalities, TotalCasualties)%>%
                        filter(Event == event)%>%
                        group_by(State, Event)%>%
                        summarise(sum(Fatalities), sum(Injuries), sum(TotalCasualties))
                names(cdata)[3:5] <- c('Fatalities','Injuries', 'TotalCasualties')
        }
        if(decade == 'All' && region != 'All' && event == 'All'){
                cdata <- select(NOAAp, State, Region, Injuries, Fatalities, TotalCasualties)%>%
                        filter(Region == region)%>%
                        group_by(State, Region)%>%
                        summarise(sum(Fatalities), sum(Injuries), sum(TotalCasualties))
                names(cdata)[3:5] <- c('Fatalities','Injuries', 'TotalCasualties')
        }
        if(decade == 'All' && region != 'All' && event != 'All'){
                cdata <- select(NOAAp, State, Region, Event, Injuries, Fatalities, TotalCasualties)%>%
                        filter(Region == region, Event == event)%>%
                        group_by(State, Region, Event)%>%
                        summarise(sum(Fatalities), sum(Injuries), sum(TotalCasualties))
                names(cdata)[4:6] <- c('Fatalities','Injuries', 'TotalCasualties')
        }
        if(decade != 'All' && region == 'All' && event == 'All'){
                cdata <- select(NOAAp, State, Decade, Injuries, Fatalities, TotalCasualties)%>%
                        filter(Decade == decade)%>%
                        group_by(State, Decade)%>%
                        summarise(sum(Fatalities), sum(Injuries), sum(TotalCasualties))
                names(cdata)[3:5] <- c('Fatalities','Injuries', 'TotalCasualties')
        }
        if(decade != 'All' && region == 'All' && event != 'All'){
                cdata <- select(NOAAp, State, Decade, Event, Injuries, Fatalities, TotalCasualties)%>%
                        filter(Decade == decade, Event == event)%>%
                        group_by(State, Decade, Event)%>%
                        summarise(sum(Fatalities), sum(Injuries), sum(TotalCasualties))
                names(cdata)[4:6] <- c('Fatalities','Injuries', 'TotalCasualties')
        }
        if(decade != 'All' && region != 'All' && event == 'All'){
                cdata <- select(NOAAp, State, Decade, Region, Injuries, Fatalities, TotalCasualties)%>%
                        filter(Decade == decade, Region == region)%>%
                        group_by(State, Decade, Region)%>%
                        summarise(sum(Fatalities), sum(Injuries), sum(TotalCasualties))
                names(cdata)[4:6] <- c('Fatalities','Injuries', 'TotalCasualties')
        }
        if(decade != 'All' && region != 'All' && event != 'All'){
                cdata <- select(NOAAp, State, Decade, Region, Event, Injuries, Fatalities, TotalCasualties)%>%
                        filter(Decade == decade, Region == region, Event == event)%>%
                        group_by(State, Decade, Region, Event)%>%
                        summarise(sum(Fatalities), sum(Injuries), sum(TotalCasualties))
                names(cdata)[5:7] <- c('Fatalities','Injuries', 'TotalCasualties')
        }
        if (nrow(cdata) == 0)(return(NULL))
                else (return(cdata) )
}

GetDecade <- function(region = 'All', event = 'All', com = NULL){
        
        if (is.null(com) || com == FALSE){
                (ddata <- filter(CasualtiesByCOM, COMUsed == FALSE))
        } else(ddata <- CasualtiesByCOM)
        
        if (region == "All" && event == 'All'){
                DecadeT <- select(ddata, everything())%>%
                        group_by(Decade, Region, Event)%>%
                        ungroup()%>%
                        arrange(Decade)
                DecadeL <- c("All")
                DecadeL <- c(DecadeL, unique(DecadeT$Decade))  
        }
        
        if (region != "All" && event == 'All'){
                DecadeT <- select(ddata, everything())%>%
                        group_by(Decade, Region)%>%
                        filter(Region == region)%>%
                        ungroup()%>%
                        arrange(Decade)
                DecadeL <- c("All")
                DecadeL <- c(DecadeL, unique(DecadeT$Decade))  
        }
        
        if (region == "All"  && event != 'All'){
                DecadeT <- select(ddata, everything())%>%
                        group_by(Decade, Event)%>%
                        filter(Event==event)%>%
                        ungroup()%>%
                        arrange(Decade)
                DecadeL <- c("All")
                DecadeL <- c(DecadeL, unique(DecadeT$Decade))  
        }
        
        if (region != "All" && event != 'All'){
                DecadeT <- select(ddata, everything())%>%
                        group_by(Decade, Region, Event)%>%
                        filter(Region == region, Event==event)%>%
                        ungroup()%>%
                        arrange(Decade)
                DecadeL <- c("All")
                DecadeL <- c(DecadeL, unique(DecadeT$Decade))  
        }
        return(DecadeL)
}

GetRegion <- function(decade = 'All', event = 'All', com = NULL){
        
        if (is.null(com) || com == FALSE){
                (rdata <- filter(CasualtiesByCOM, COMUsed == FALSE))
        } else(rdata <- CasualtiesByCOM)
        
        if (decade == "All" && event == 'All'){
                RegionT <- select(rdata, everything())%>%
                        group_by(Decade, Region, Event)%>%
                        ungroup()%>%
                        arrange(Region)
                RegionL <- c("All")
                RegionL <- c(RegionL, unique(RegionT$Region))  
        }
        
        if (decade != "All" && event == 'All'){
                RegionT <- select(rdata, everything())%>%
                        group_by(Decade, Region)%>%
                        filter(Decade == decade)%>%
                        ungroup()%>%
                        arrange(Region)
                RegionL <- c("All")
                RegionL <- c(RegionL, unique(RegionT$Region))  
        }
        
        if (decade == "All"  && event != 'All'){
                RegionT <- select(rdata, everything())%>%
                        group_by(Region, Event)%>%
                        filter(Event==event)%>%
                        ungroup()%>%
                        arrange(Region)
                RegionL <- c("All")
                RegionL <- c(RegionL, unique(RegionT$Region))  
        }
        
        if (decade != "All" && event != 'All'){
                RegionT <- select(rdata, everything())%>%
                        group_by(Decade, Region, Event)%>%
                        filter(Decade == decade, Event==event)%>%
                        ungroup()%>%
                        arrange(Region)
                RegionL <- c("All")
                RegionL <- c(RegionL, unique(RegionT$Region))  
        }
        return(RegionL)
}

GetEvent <- function(decade = 'All', region = 'All', com = NULL){

        if (is.null(com) || com == FALSE){
                (edata <- filter(CasualtiesByCOM, COMUsed == FALSE))
        } else(edata <- CasualtiesByCOM)
        
        if (decade == "All" && region == 'All'){
                EventT <- select(edata, everything())%>%
                        group_by(Decade, Region, Event)%>%
                        ungroup()%>%
                        arrange(Event)
                EventL <- c("All")
                EventL <- c(EventL, unique(EventT$Event))  
        }
        
        if (decade != "All" && region == 'All'){
                EventT <- select(edata, everything())%>%
                        group_by(Decade, Event)%>%
                        filter(Decade == decade)%>%
                        ungroup()%>%
                        arrange(Event)
                EventL <- c("All")
                EventL <- c(EventL, unique(EventT$Event))  
        }
        
        if (decade == "All"  && region != 'All'){
                EventT <- select(edata, everything())%>%
                        group_by(Region, Event)%>%
                        filter(Region==region)%>%
                        ungroup()%>%
                        arrange(Event)
                EventL <- c("All")
                EventL <- c(EventL, unique(EventT$Event))  
        }
        
        if (decade != "All" && region != 'All'){
                EventT <- select(edata, everything())%>%
                        group_by(Decade, Region, Event)%>%
                        filter(Decade == decade, Region==region)%>%
                        ungroup()%>%
                        arrange(Event)
                EventL <- c("All")
                EventL <- c(EventL, unique(EventT$Event))  
        }
        return(EventL)
}

GetJitterData <- function(decade, region, event, com = NULL){
        
        if (is.null(com) || com == FALSE){
                (NOAAp <- filter(All_NOAAp, COMUsed == FALSE))
        } else(NOAAp <- All_NOAAp)
        
        if(decade == 'All' && region == 'All' && event == 'All'){
                cdata <- select(NOAAp, everything())
                names(cdata)[7:9] <- c('Fatalities','Injuries', 'TotalCasualties')
        }
        if(decade == 'All' && region == 'All' && event != 'All'){
                cdata <- select(NOAAp, everything())
                names(cdata)[7:9] <- c('Fatalities','Injuries', 'TotalCasualties')
                cdata <- filter(cdata, Event == event)
        }
        if(decade == 'All' && region != 'All' && event == 'All'){
                cdata <- select(NOAAp, everything())
                names(cdata)[7:9] <- c('Fatalities','Injuries', 'TotalCasualties')
                cdata <- filter(cdata, Region == region)
        }
        if(decade == 'All' && region != 'All' && event != 'All'){
                cdata <- select(NOAAp, everything())
                names(cdata)[7:9] <- c('Fatalities','Injuries', 'TotalCasualties')
                cdata <- filter(cdata, Region == region, Event == event)
        }
        if(decade != 'All' && region == 'All' && event == 'All'){
                cdata <- select(NOAAp, everything())
                names(cdata)[7:9] <- c('Fatalities','Injuries', 'TotalCasualties')
                cdata <- filter(cdata, Decade == decade)
        }
        if(decade != 'All' && region == 'All' && event != 'All'){
                cdata <- select(NOAAp, everything())
                names(cdata)[7:9] <- c('Fatalities','Injuries', 'TotalCasualties')
                cdata <- filter(cdata, Decade == decade, Event == event)
        }
        if(decade != 'All' && region != 'All' && event == 'All'){
                cdata <- select(NOAAp, everything())
                names(cdata)[7:9] <- c('Fatalities','Injuries', 'TotalCasualties')
                cdata <- filter(cdata, Decade == decade, Region == region)
        }
        if(decade != 'All' && region != 'All' && event != 'All'){
                cdata <- select(NOAAp, everything())
                names(cdata)[7:9] <- c('Fatalities','Injuries', 'TotalCasualties')
                cdata <- filter(cdata, Decade == decade, Region == region, Event == event)
        }
        
        if (nrow(cdata) == 0)(return(NULL))
                else (return(cdata) )
}
 