library(shiny)
library(plotly)
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
  plotlyOutput(outputId = "region"),
  plotlyOutput(outputId = "local")
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    region = 'London',
    pal = c(regionPal[1:2],regionPal[4:9])
  )
  
  observeEvent(event_data("plotly_click"), {
    d <- event_data("plotly_click")
    if (!is.null(d)) {
      newRegion <- d[,'x']
      if (newRegion %in% levels(dfRegion$Region)) {
        rv$region <- newRegion
        regionIndex <- which(!is.na(match(levels(dfRegion$Region), newRegion)))
        rv$pal <- c(regionPal[1:regionIndex-1],regionPal[regionIndex+1:9])
      }
    }
  })
  
  output$region <- renderPlotly({
    
    plot_ly(subset(dfRegion, Financial.Year==input$Year & dfRegion$Region!=rv$region),
              name='Region',
              type = 'bar',
              x = ~Region,
              y = ~mean,
              marker = list(
                line = list(
                  color = 'rgb(201,201,201)',
                  width = 1
                ),
                color=rv$pal
              ),
              hovertemplate = '<i>%{x}</i>: %{y:.1f}%') %>%
              add_bars(
                name='Selected Region',
                x = rv$region,
                y = dfRegion[dfRegion$Region==rv$region & dfRegion$Financial.Year==input$Year,]$mean,
                marker = list(
                  color = 'rgba(201,201,201,1)',
                  line = list(
                    color = 'rgb(0,0,0)',
                    width = 4
                  )
                ),
                showlegend = F) %>%
                layout(title=sprintf('<b>UK Region Recycling Rates (%s)</b>', input$Year),
                       xaxis = list(title = ''),
                       yaxis = list(title = 'Recycling Rate (%)')
                )
  })
  
  output$local <- renderPlotly({
    
      plot_ly(droplevels(subset(df, Region==rv$region & Financial.Year==input$Year)),
                name='Local Authority',
                type = 'bar',
                x = ~Local.Authority,
                y = ~value,
                marker = list(
                  line = list(
                    color = 'rgb(0,0,0)',
                    width = 1
                  ),
                  color = ~value,
                  colorscale='Greens',
                  reversescale=TRUE
                ),
                hovertemplate = '<i>%{x}</i>: %{y:.1f}%') %>%
                layout(title=sprintf('<b>%s Recycling Rates by Local Authority (%s)</b>', rv$region, input$Year),
                       xaxis = list(title = ''),
                       yaxis = list(title = 'Recycling rate (%)')
                )
  })
  
}

shinyApp(ui, server)