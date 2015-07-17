library(shiny)
library(googleVis)
library(d3heatmap)
source("helpers.R")

api_key <- "api-key=4afa5e239fc8c4847a7f7fc0b537d285:2:72422982"
base_url <- "http://api.nytimes.com/svc/events/v2/listings.json?"
coordinate <- list(nyt = "40.756146,-73.99021") 


shinyServer(function(input, output, session) {
    
    retrieved <- reactive({  
        if(input$webData) {
            # construct NYT EVENT API searching url
            url <- getURL(base_url, api_key, 
                          category=input$category, borough=input$borough, 
                          ll="40.7127,-74.0059", radius=input$radius, limit=input$limit)
            print(url)
            # retrive events from NYT event api
            return(retrieveEvent(url))
        } else {
            if(input$category == "Everything") {
                return(events)
            } else {
                return(events[events$category == input$category,])
            }   
        } 
    })
    
    output$gvis <- renderGvis({ 
        # plot events on google map
        eventToPlot <- formatEvent(retrieved())
        print(paste0("Plotting events: ", min(input$limit, nrow(eventToPlot))))
        gvisMap(eventToPlot[1:min(input$limit, nrow(eventToPlot)),], locationvar = "LatLong" , tipvar = "info", 
                options=list(width=200,
                             height=500,
                             showTip=TRUE, 
                             showLine=TRUE, 
                             enableScrollWheel=TRUE,
                             mapType="styledMap",
                             showLine=TRUE,
                             useMapTypeControl=TRUE,
                             icons=paste0("{",
                                          "'default': {'normal': 'http://icons.iconarchive.com/",
                                          "icons/icons-land/vista-map-markers/48/",
                                          "Map-Marker-Ball-Pink-icon.png',\n",
                                          "'selected': 'http://icons.iconarchive.com/",
                                          "icons/icons-land/vista-map-markers/48/",
                                          "Map-Marker-Ball-Right-Pink-icon.png'",
                                          "}}"),
                             maps=paste0("{",
                                         "'styledMap': {",
                                         "'name': 'Styled Map',\n",
                                         "'styles': [",
                                         "{'featureType': 'landscape',",
                                         "'stylers': [{'hue': '#259b24'}, {'saturation': 10}, {'lightness': -22}]",
                                         "}",
                                         "]}}")
                             ))
    })
    
    writeToDB <- eventReactive(input$writeToDB, {
        if(input$webData) {
            toDB(dbPath, tbName, retrieved())
        }        
    })
    
    readFromDB <- eventReactive(input$readFromDB, {
        events <<- fromDB(dbPath, tbName)
        days <<- dayFreq(events)
        event.day <<- eventByDay(events, days)
    })
    
    headMapAxis <- eventReactive(input$heatmapAxis, {
        c(input$yaxis,input$xaxis)
    })
    
    observe(writeToDB())
    observe(readFromDB())
    
    output$byDay <- renderGvis({
        
        eventByDay <- data.frame("Day"          = c("SUN", "MON", "TUE", "WED", "THU", "FRI", "SAT"),
                              "Total"        = sapply(days, sum),
                              "Times Pick"   = sapply(days, function(x) sum(events$times_pick==1 & x)),
                              "Kid Friendly" = sapply(days, function(x) sum(events$kid_friendly==1 & x)),
                              "Free"         = sapply(days, function(x) sum(events$free==1 & x))
                              )
        checkTotal <- input$checkTotal
        if(!(input$checkPick || input$checkKid || input$checkFree)) {
            checkTotal <- TRUE
        }
        print(nrow(eventByDay))
        gvisAreaChart(eventByDay[, c(TRUE, checkTotal, 
                                 input$checkPick,
                                 input$checkKid,
                                 input$checkFree 
                                 )], 
                      options=list(width="900px", height="600px"))
    })
    
    output$byCategory <- renderGvis({
        df2 <- events[,c("category", "times_pick", "kid_friendly", "free")]
        df2.category <- df2 %>% group_by(category) %>% count(category)
        df2.pick <- df2 %>% filter(times_pick==1) %>% group_by(category) %>% count(category)
        df2.kid <- df2 %>% filter(kid_friendly==1) %>% group_by(category) %>% count(category)
        df2.free <- df2 %>% filter(free==1) %>% group_by(category) %>% count(category)
        
        pie.category <- gvisPieChart(df2.category, 
                                     options=list(width="400px", height="400px", 
                                                  title="Total", legend='none'))
        pie.pick <- gvisPieChart(df2.pick, 
                                 options=list(width="400px", height="400px", 
                                              title="Times Pick", legend='none'))
        pie.kid <- gvisPieChart(df2.kid, 
                                options=list(width="400px", height="400px", 
                                             title="Kid Friendly", legend='none'))
        pie.free <- gvisPieChart(df2.free, 
                                 options=list(width="400px", height="400px", 
                                              title="Free", legend='none'))
        gvisMerge(
                  gvisMerge(pie.free, pie.category,horizontal=TRUE),
                  gvisMerge(pie.pick, pie.kid, horizontal=TRUE),
                  horizontal=FALSE)
    })
    
    output$heatmap <- renderD3heatmap({
        axis <- headMapAxis()
        if(axis[1] ==  axis[2]) {
            return()
        }
        print(axis)
        d3heatmap(table(event.day[,axis]), colors="Greens", scale = "column", Colv = FALSE)
    })
    
    output$eventTable <- renderGvis({
        df3 <- events
        df3$name <- apply(df3, 1, 
                             function(x) paste0("<a href=\"", 
                                                x["event_detail_url"], "\" target=\"_blank\">", 
                                                x["event_name"], "</a>"))
        if(input$checkPick) {
            df3 <- df3[df3$times_pick==1,]
        }
        if(input$checkKid) {
            df3 <- df3[df3$kid_friendly==1,]
        }
        if(input$checkFree) {
            df3 <- df3[df3$free==1,]
        }
        gvisTable(data.frame("Name"=df3$name, "Venue"=df3$venue_name, 
                             "Category"=df3$category, "Neighborhood"=df3$neighborhood),
                  options=list(page='enable', pageSize=20))
    })
        
 
})
