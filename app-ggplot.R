library(shiny)
library(ggplot2)
library(reshape2)
library(plyr)

# From waste spreadsheet:
# "For correct totals, you should only sum values for unitary and disposal authorities. Summing values for unitary, collection and disposal authorities will result in double counting."

# For total waste, and proportion recycled or not, use:
# 'Total local authority collected waste (tonnes)',
# 'Local authority collected waste - sent for recycling/composting/reuse (tonnes)'
# 'Local authority collected waste - not sent for recycling (tonnes)'

# Total.local.authority.collected.waste..tonnes.
# Local.authority.collected.waste...sent.for.recycling.composting.reuse..tonnes.
# Local.authority.collected.waste...not.sent.for.recycling..tonnes.

dfWide <- read.csv('data/LA_and_Regional_Spreadsheet_201718.csv')
dfWide['Recycled'] <- 100 * (dfWide['Local.authority.collected.waste...sent.for.recycling.composting.reuse..tonnes.'] / dfWide['Total.local.authority.collected.waste..tonnes.'])
dfWide['NotRecycled'] <- 100 * (dfWide['Local.authority.collected.waste...not.sent.for.recycling..tonnes.'] / dfWide['Total.local.authority.collected.waste..tonnes.'])
df <- melt(dfWide, id.vars=c('Financial.Year','Region','ONS.Code','JPP.order','Local.Authority','Authority.type'))
df <- subset(df,Authority.type!='Collection' & variable == 'Recycled')

dfRegion <- ddply(df, .(Financial.Year, Region, variable), summarize,
                  mean = round(mean(value), 1))
regionPal <- c('#41ab5d','#006d2c','#f7fcf5','#e5f5e0','#00441b','#a1d99b','#238b45','#c7e9c0','#74c476')


ui <- fluidPage(
  radioButtons(inputId = "Year", label = "Select Year", choices = unique(dfRegion$Financial.Year), selected='2017-18', inline=TRUE),
  plotOutput(outputId = "region", click='click_region'),
  plotOutput(outputId = "local")
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    region = 'London',
    pal = c(regionPal[1:2],regionPal[4:9])
  )
  
  observeEvent(input$click_region, {
    
    res <- nearPoints(subset(dfRegion, Financial.Year==input$Year & dfRegion$Region!=rv$region), coordinfo=input$click_region, addDist=TRUE, allRows=TRUE)
    print(res)
    print(res[res$dist_ == min(res$dist_),])
    
    #d <- event_data("plotly_click")
    #if (!is.null(d)) {
    #  newRegion <- d[,'x']
    #  if (newRegion %in% levels(dfRegion$Region)) {
    #    rv$region <- newRegion
    #    regionIndex <- which(!is.na(match(levels(dfRegion$Region), newRegion)))
    #    rv$pal <- c(regionPal[1:regionIndex-1],regionPal[regionIndex+1:9])
    #  }
    #}
  })
  
  output$region <- renderPlot({
    ggplot(subset(dfRegion, Financial.Year==input$Year & dfRegion$Region!=rv$region)) +
      geom_bar(aes(x=Region, y=mean, fill=mean), stat='identity') + 
      scale_fill_continuous(type = "gradient") +
      theme_bw()
  })
  
  output$local <- renderPlot({
    ggplot(droplevels(subset(df, Region==rv$region & Financial.Year==input$Year))) +
      geom_bar(aes(x=Local.Authority, y=value, fill=value), stat='identity') + 
      scale_fill_continuous(type = "gradient") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
}

shinyApp(ui, server)