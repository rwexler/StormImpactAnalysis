explore_NOAA <- function() {
        
        # Load librarys necessary for data analysis.
        
        library(lubridate)
        library(gridExtra)
        library(grid)
        library(maps)
        library(RColorBrewer)
        
        ################
        # GETTING DATA #
        ################
        
        # Download the file from the course web site and read it to a data frame. This step
        # may take a few minutes.
        
        Url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
        download.file(Url, "noaa.csv.bz2")
        StormData <- read.csv("noaa.csv.bz2")
        
        #################
        # CLEANING DATA #
        #################
        
        # Trim the data set to include only the variables relevant to the date, location,
        # weather event types, and the health or economic harm done by them.
        
        TrimStormData <- StormData[,c(2,7,8,23,24,25:28)]
        
        # Reformat date to M-D-Y HH:MM:SS.
        
        TrimStormData$BGN_DATE <- mdy_hms(TrimStormData$BGN_DATE)
        
        # The storm data contains a number of enigmatic state abbreviations. As such, I choose
        # to only keep data for the 50 US states.
        
        UsStormData <- TrimStormData[TrimStormData$STATE %in% state.abb,]
        
        # There are a number of mispelled and combined event types in the storm data. I only 
        # choose those that intersect with the event types mentioned in the National Weather 
        # Service Storm Data Documentation.
        
        UsStormData$EVTYPE <- toupper(UsStormData$EVTYPE) #Make all events uppercase
        keepEvents <- c("ASTRONOMICAL LOW TIDE","AVALANCHE","BLIZZARD","COASTAL FLOOD","COLD/WIND CHILL","DEBRIS FLOW","DENSE FOG","DENSE SMOKE","DROUGHT",
                        "DUST DEVIL","DUST STORM","EXCESSIVE HEAT","EXTREME COLD/WIND CHILL","FLASH FLOOD","FLOOD","FREEZING FOG","FROST/FREEZE","FUNNEL CLOUD",
                        "HAIL","HEAT","HEAVY RAIN","HEAVY SNOW","HIGH SURF","HIGH WIND","HURRICANE/TYPHOON","ICE STORM","LAKESHORE FLOOD","LAKE-EFFECT SNOW",
                        "LIGHTNING","MARINE HAIL","MARINE HIGH WIND","MARINE STRONG WIND","MARINE THUNDERSTORM WIND","RIP CURRENT","SEICHE","SLEET","STORM TIDE",
                        "STRONG WIND","THUNDERSTORM WIND","TORNADO","TROPICAL DEPRESSION","TROPICAL STORM","TSUNAMI","VOLCANIC ASH","WATERSPOUT","WILDFIRE",
                        "WINTER STORM","WINTER WEATHER")
        EventStormData <- UsStormData[UsStormData$EVTYPE %in% keepEvents,]
        
        # There are erroneous and seriously confusing exponent lables in the storm data, see
        # the variable badSymbols below. Although there is some published work on how to define
        # these mislabeled observations (https://rpubs.com/cgwhitehead/noaadata), I elected to
        # include on those exponents with the accepted alphabetic naming scheme (K, M, B) as 
        # described by the National Weather Service Storm Data Documentation
        
        badSymbols <- c("","-","?","+","H","0","1","2","3","4","5","6","7","8")
        ExpStormData <- EventStormData[!(EventStormData$PROPDMGEXP %in% badSymbols | EventStormData$CROPDMGEXP %in% badSymbols),]
        levels(ExpStormData$PROPDMGEXP)[levels(ExpStormData$PROPDMGEXP) == "m"] <- "M"
        levels(ExpStormData$CROPDMGEXP)[levels(ExpStormData$CROPDMGEXP) == "k"] <- "K"
        
        # A flood was reported on January 1, 2006 in California that supposedly incurred 115 
        # billion dollars of damages. Based on a government report 
        # (http://pubs.usgs.gov/of/2006/1182/pdf/ofr2006-1182.pdf), the total damages were in 
        # the hundreds of millions. As such, I believe the exponent was mistakenly labeled as 
        # "B" instead of "M." I corrected this by dividing the property damage by 1000.
        
        ExpStormData$PROPDMG[ExpStormData$PROPDMGEXP=="B" & ExpStormData$PROPDMG>110] <- 
                as.numeric(as.character(ExpStormData$PROPDMG[ExpStormData$PROPDMGEXP=="B" & ExpStormData$PROPDMG>110]))/1000
        
        # Convert alphabetical exponent lables to numeric powers of ten
        
        # Property damage
        
        levels(ExpStormData$PROPDMGEXP)[levels(ExpStormData$PROPDMGEXP) == "K"] <- 3
        levels(ExpStormData$PROPDMGEXP)[levels(ExpStormData$PROPDMGEXP) == "M"] <- 6
        levels(ExpStormData$PROPDMGEXP)[levels(ExpStormData$PROPDMGEXP) == "B"] <- 9
       
        # Crop damage
        
        levels(ExpStormData$CROPDMGEXP)[levels(ExpStormData$CROPDMGEXP) == "K"] <- 3
        levels(ExpStormData$CROPDMGEXP)[levels(ExpStormData$CROPDMGEXP) == "M"] <- 6
        levels(ExpStormData$CROPDMGEXP)[levels(ExpStormData$CROPDMGEXP) == "B"] <- 9
        
        # Since we are limited in the number of plots we can make, I choose to combine property
        # and crop damages into a total damages variable.
        
        ExpStormData$TOTALPDMG <- as.numeric(as.character(ExpStormData$PROPDMG))*10.0^as.numeric(as.character(ExpStormData$PROPDMGEXP))
        ExpStormData$TOTALCDMG <- as.numeric(as.character(ExpStormData$CROPDMG))*10.0^as.numeric(as.character(ExpStormData$CROPDMGEXP))
        ExpStormData$TOTALDMG <- ExpStormData$TOTALPDMG + ExpStormData$TOTALCDMG
        
        # Roughly determine the season in which the event took place.
        
        ExpStormData$SEASON <- factor(quarter(ExpStormData$BGN_DATE), levels = 1:4, 
                                   labels = c("WINTER","SPRING","SUMMER","FALL"))
        
        # Find which region a particular state and timezone belong to.
        
        ExpStormData$REGION <- state.region[match(as.character(ExpStormData$STATE),state.abb)]
        
        # Combine fatalities and injuries in one variable.
        
        ExpStormData$HARMED <- ExpStormData$FATALITIES + ExpStormData$INJURIES
        
        #Keep only the variables directly relevant to this project and reorder them.
        
        KeepStormData <- ExpStormData[,c(2,13:14,3:5,15,10:12,7)]
        
        #############################
        # EXPLORATORY DATA ANALYSIS #
        #############################
        
        # Which types of events are most harmful with respect to population health?
        
        # Calculate the total harm done by weather event type, season, and region.
        
        HealthHarmSeason <- with(KeepStormData, aggregate(x = HARMED, by = list(EVTYPE,SEASON), FUN = sum, na.rm = TRUE))
        HealthHarmRegion <- with(KeepStormData, aggregate(x = HARMED, by = list(EVTYPE,REGION), FUN = sum, na.rm = TRUE))
        names(HealthHarmSeason) <- c("EVTYPE","SEASON","HARMED")
        names(HealthHarmRegion) <- c("EVTYPE","REGION","HARMED")
        
        # Sort the total harm done descending to show which events were most harmful and how
        # this changes by season and region.
        
        SortHealthHarmSeason <- HealthHarmSeason[with(HealthHarmSeason, order(-HealthHarmSeason$HARMED)),]
        SortHealthHarmRegion <- HealthHarmRegion[with(HealthHarmRegion, order(-HealthHarmRegion$HARMED)),]
        
        # Plot table of top 5 most harmful events by season and region
        
        pdf("Top5HealthHarmSeasonRegion.pdf", height=10, width=9)
        grid.arrange(
                textGrob("By Season"), textGrob("By Region"),
                tableGrob(SortHealthHarmSeason[SortHealthHarmSeason$SEASON=="WINTER",][1:5,1:3], rows = NULL, 
                          cols = c("Event Type", "Season", "People Harmed")),
                tableGrob(SortHealthHarmRegion[SortHealthHarmRegion$REGION=="Northeast",][1:5,1:3], rows = NULL, 
                          cols = c("Event Type", "Region", "People Harmed")),
                tableGrob(SortHealthHarmSeason[SortHealthHarmSeason$SEASON=="SPRING",][1:5,1:3], rows = NULL, 
                          cols = c("Event Type", "Season", "People Harmed")),
                tableGrob(SortHealthHarmRegion[SortHealthHarmRegion$REGION=="South",][1:5,1:3], rows = NULL, 
                          cols = c("Event Type", "Region", "People Harmed")),
                tableGrob(SortHealthHarmSeason[SortHealthHarmSeason$SEASON=="SUMMER",][1:5,1:3], rows = NULL, 
                          cols = c("Event Type", "Season", "People Harmed")),
                tableGrob(SortHealthHarmRegion[SortHealthHarmRegion$REGION=="North Central",][1:5,1:3], rows = NULL, 
                          cols = c("Event Type", "Region", "People Harmed")),
                tableGrob(SortHealthHarmSeason[SortHealthHarmSeason$SEASON=="FALL",][1:5,1:3], rows = NULL, 
                          cols = c("Event Type", "Season", "People Harmed")),
                tableGrob(SortHealthHarmRegion[SortHealthHarmRegion$REGION=="West",][1:5,1:3], rows = NULL, 
                          cols = c("Event Type", "Region", "People Harmed")),
                nrow=5,ncol=2)
        dev.off()
        
        # Which types of events have the greatest economic consequences?
        
        # Calculate the total economic harm done by weather event type, season, and region.
        
        EconHarmSeason <- with(KeepStormData, aggregate(x = TOTALDMG, by = list(EVTYPE,SEASON), FUN = sum, na.rm = TRUE))
        EconHarmRegion <- with(KeepStormData, aggregate(x = TOTALDMG, by = list(EVTYPE,REGION), FUN = sum, na.rm = TRUE))
        names(EconHarmSeason) <- c("EVTYPE","SEASON","TOTALDMG")
        names(EconHarmRegion) <- c("EVTYPE","REGION","TOTALDMG")
        
        # Sort the total economic harm done descending to show which events were most harmful 
        # and how this changes by season and region.
        
        SortEconHarmSeason <- EconHarmSeason[with(EconHarmSeason, order(-EconHarmSeason$TOTALDMG)),]
        SortEconHarmRegion <- EconHarmRegion[with(EconHarmRegion, order(-EconHarmRegion$TOTALDMG)),]
        
        # Report the economic harm done in billions of USD
        
        SortEconHarmSeason$TOTALDMG <- as.numeric(SortEconHarmSeason$TOTALDMG)/1E9
        SortEconHarmRegion$TOTALDMG <- as.numeric(SortEconHarmRegion$TOTALDMG)/1E9
        
        # Plot table of top 5 most economically harmful events by season and region
        
        pdf("Top5EconHarmSeasonRegion.pdf", height=10, width=9)
        grid.arrange(
                textGrob("By Season"), textGrob("By Region"),
                tableGrob(SortEconHarmSeason[SortEconHarmSeason$SEASON=="WINTER",][1:5,1:3], rows = NULL, 
                          cols = c("Event Type", "Season", "Total Damages")),
                tableGrob(SortEconHarmRegion[SortEconHarmRegion$REGION=="Northeast",][1:5,1:3], rows = NULL, 
                          cols = c("Event Type", "Region", "Total Damages")),
                tableGrob(SortEconHarmSeason[SortEconHarmSeason$SEASON=="SPRING",][1:5,1:3], rows = NULL, 
                          cols = c("Event Type", "Season", "Total Damages")),
                tableGrob(SortEconHarmRegion[SortEconHarmRegion$REGION=="South",][1:5,1:3], rows = NULL, 
                          cols = c("Event Type", "Region", "Total Damages")),
                tableGrob(SortEconHarmSeason[SortEconHarmSeason$SEASON=="SUMMER",][1:5,1:3], rows = NULL, 
                          cols = c("Event Type", "Season", "Total Damages")),
                tableGrob(SortEconHarmRegion[SortEconHarmRegion$REGION=="North Central",][1:5,1:3], rows = NULL, 
                          cols = c("Event Type", "Region", "Total Damages")),
                tableGrob(SortEconHarmSeason[SortEconHarmSeason$SEASON=="FALL",][1:5,1:3], rows = NULL, 
                          cols = c("Event Type", "Season", "Total Damages")),
                tableGrob(SortEconHarmRegion[SortEconHarmRegion$REGION=="West",][1:5,1:3], rows = NULL, 
                          cols = c("Event Type", "Region", "Total Damages")),
                nrow=5,ncol=2)
        dev.off()
        
        # Plot a US state map shaded by the amount of economic hardship faced by each state in
        # response to nautral disasters.
        
        values <- tapply(KeepStormData$TOTALDMG, KeepStormData$STATE, sum, na.rm = TRUE)
        values <- values[!is.na(values)]
        values <- values/max(values)
        values <- 1-values
        names(values) <- state.abb
        pdf("EconHarmState.pdf", height=10, width=10)
        
        # Getting the names used by map
        
        tmp <- map('state',plot=FALSE,namesonly=TRUE)
        
        # Matching (after adjusting using gsub and tolower)
        
        tmp <- match(gsub('(:.*)','',tmp),tolower(state.name))
        
        # Convert your numbers to grey-scale and selct using the match
        
        map('state',fill=TRUE,col=grey(values)[tmp])
        dev.off()
}