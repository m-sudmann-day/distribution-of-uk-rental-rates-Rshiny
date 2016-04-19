# Matthew Sudmann-Day
# Barcelona GSE Data Science
#
# Data visualization of 175,000 rental ads that I scraped from a UK
# housing website in early 2015.
#
# The visualization shows variance and a quasi-histogram in circular
# format on a postcode-by-postcode basis.

library(shiny)
library(sp)
library(leaflet)
library(RColorBrewer)
library(dplyr)

ui <- fluidPage(
  headerPanel('Distribution of UK Rental Rates by Postcode'),
  leafletOutput("mymap", width="800", height="800"))
  
load("data.RData")
data$one <- 1

quantiles <- quantile(data$price, probs=seq(0,1,0.25))[-1]

data$quantile1 <- (data$price > quantiles[1])
data$quantile2 <- (data$price > quantiles[2])
data$quantile3 <- (data$price > quantiles[3])

summary <- summarise(group_by(data, outCode),
                     median_price=median(price),
                     var_price=var(price),
                     qty1=sum(quantile1),
                     qty2=sum(quantile2),
                     qty3=sum(quantile3),
                     total=sum(one),
                     outCodeLat=median(lat),
                     outCodeLong=median(long))

summary$radius <- (250+(log(summary$var_price)-7)*400)
summary$radius1 <- sqrt((summary$qty1/summary$total)*(summary$radius^2))
summary$radius2 <- sqrt((summary$qty2/summary$total)*(summary$radius^2))
summary$radius3 <- sqrt((summary$qty3/summary$total)*(summary$radius^2))
summary$q1 <- summary$total-summary$qty1
summary$q2 <- summary$qty1-summary$qty2
summary$q3 <- summary$qty2-summary$qty3
summary$q4 <- summary$qty3
summary$p1 <- round(100*summary$q1/summary$total)
summary$p2 <- round(100*summary$q2/summary$total)
summary$p3 <- round(100*summary$q3/summary$total)
summary$p4 <- round(100*summary$q4/summary$total)

pal <- colorQuantile(palette=c("green", "yellow", "orange", "red"), domain=log(summary$median_price))

server <- function(input, output, session) {

  output$mymap <- renderLeaflet({

    m <- leaflet()
    m <- addProviderTiles(m, "CartoDB.Positron", options = providerTileOptions(noWrap = TRUE))

    m <- addCircles(m,
                    lng=summary$outCodeLong,
                    lat=summary$outCodeLat,
                    radius=summary$radius,
                    weight=3,
                    opacity=0.5,
                    color="green",
                    label=paste(summary$outCode, ": Q1=", summary$p1, "%", sep=""))
    
    m <- addCircles(m,
                    lng=summary$outCodeLong[summary$q1>0],
                    lat=summary$outCodeLat[summary$q1>0],
                    radius=summary$radius1[summary$q1>0],
                    weight=2,
                    opacity=0.8,
                    color="yellow",
                    label=paste(summary$outCode[summary$q1>0], ": Q2=", summary$p2[summary$q1>0], "%", sep=""))
                    
    m <- addCircles(m,
                    lng=summary$outCodeLong[summary$q2>0],
                    lat=summary$outCodeLat[summary$q2>0],
                    radius=summary$radius2[summary$q2>0],
                    weight=1,
                    opacity=0.7,
                    color="orange",
                    label=paste(summary$outCode[summary$q2>0], ": Q3=", summary$p3[summary$q2>0], "%", sep=""))
                    
    m <- addCircles(m,
                    lng=summary$outCodeLong[summary$q3>0],
                    lat=summary$outCodeLat[summary$q3>0],
                    radius=summary$radius3[summary$q3>0],
                    weight=1,
                    opacity=0.7,
                    color="red",
                    label=paste(summary$outCode[summary$q3>0], ": Q4=", summary$p4[summary$q3>0], "%", sep=""))
                    
    m <- addLegend(m, "bottomleft", pal = pal, values = log(summary$median_price),
             title = "Median Price Quartile",
             opacity = 1)
             
    m <- setView(m, lng = -2.5, lat = 54.5, zoom = 6)
    return(m)
  })
}

shinyApp(ui, server)
