library(shiny)
library(ggplot2)
library(dplyr)

# Overall:
# Must use 'shiny' package
# Maybe use plotly or vegalite
# Must submit 4 files: 2 'ui.R' and 2 'server.R'


mortality = read.csv(url("https://raw.githubusercontent.com/charleyferrari/CUNY_DATA_608/master/module3/data/cleaned-cdc-mortality-1999-2010-2.csv"))
AvgMort = aggregate(cbind(Deaths, Population) ~ ICD.Chapter + Year, mortality, FUN=sum) %>%
  mutate(NatAvg = round(Deaths / Population * 500000, 4))
merged = merge(mortality, AvgMort[,-c(3:4)], by=c("ICD.Chapter","Year"))

# Question 2
# Are states improving mortality rates (per cause) faster/slower than national average?
# Visualization for one cause of death at a time.
# National average should be weighted average by the national population.


shinyUI(pageWithSidebar(
  # Title
  headerPanel("CDC Mortality Rates Across States vs National Average"),
  # Side Selection Bar
  fluidPage(
    selectInput("Chapters", "Type of Disease:", unique(merged$ICD.Chapter)),
    selectInput("State", "State:", unique(merged$State)),
    p(strong("Red Line = National Average")),
    p(strong("Black Line = Crude Mortality Rate"))
  ),
  # Create Histogram Plot
  mainPanel(
    plotOutput("plot"),
    tableOutput("Top10")
  )
))
