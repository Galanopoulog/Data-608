library(shiny)
library(viridisLite)
library(plotly)

# Overall:
# Must use 'shiny' package
# Maybe use plotly or vegalite
# Must submit 4 files: 2 'ui.R' and 2 'server.R'


mortality = read.csv(url("https://raw.githubusercontent.com/charleyferrari/CUNY_DATA_608/master/module3/data/cleaned-cdc-mortality-1999-2010-2.csv"))
mort10 = subset(mortality, Year == 2010)
maxed = max(mort10$Crude.Rate)

# Question 1
# Compare mortality rates from particular causes across the US.
# Visualization of crude mortality rates across all states from one cause (for 2010 only).
# Visualization that allows to rank states by crude mortality for each cause of death.


shinyServer(function(input, output) {
  
  output$hist = renderPlot({
    # generate a State vs Crude.Rate plot
    ok = subset(mort10, mort10$ICD.Chapter == input$Chapters)
    ok2  = ok[order(ok[,"Crude.Rate"],decreasing=TRUE),]
    barplot(ok2$Crude.Rate, names.arg= ok2$State, ylim=c(0,max(ok2$Crude.Rate)))

  })
})

