library(shiny)
library(viridisLite)
library(plotly)

# Overall:
# Must use 'shiny' package
# Maybe use plotly or vegalite
# Must submit 4 files: 2 'ui.R' and 2 'server.R'


mortality = read.csv(url("https://raw.githubusercontent.com/charleyferrari/CUNY_DATA_608/master/module3/data/cleaned-cdc-mortality-1999-2010-2.csv"))
mort10 = subset(mortality, Year == 2010)

# Question 1
# Compare mortality rates from particular causes across the US.
# Visualization of crude mortality rates across all states from one cause (for 2010 only).
# Visualization that allows to rank states by crude mortality for each cause of death.

shinyUI(pageWithSidebar(
  # Title
  headerPanel("CDC Mortality Rates for 2010 Across States"),
  # Side Selection Bar
  fluidPage(
    selectInput("Chapters", "Type of Disease:", levels(mort10$ICD.Chapter))
  ),
  # Create Histogram Plot
  mainPanel(
    plotOutput("hist")
  )
))