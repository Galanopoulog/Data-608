# Question 1
# Compare mortality rates from particular causes across the US.
# Visualization of crude mortality rates across all states from one cause (for 2010 only).
# Visualization that allows to rank states by crude mortality for each cause of death.

shinyServer(function(input, output) {
  output$hist = renderPlot({
    # generate a State vs Crude.Rate plot
    data = mort10 %>% filter(mort10$ICD.Chapter == input$Chapters)
    ggplot(data, aes(x=reorder(State, -Crude.Rate), y=Crude.Rate)) +
      geom_bar(stat="identity", fill="steelblue") +
      geom_text(aes(label=Crude.Rate), vjust=1.6, color="white", size=3) +
      xlab("State") +
      theme_minimal()
  })
  
  # Top 10 States Table
  output$Top10 = renderTable({
    data = mort10 %>% filter(mort10$ICD.Chapter == input$Chapters)
    data2 = data[with(data,order(-Crude.Rate)),]
    data2[1:10, c(-1,-3)]
  },
  caption = "TOP 10 STATES",
  caption.placement = getOption("xtable.caption.placement", "top")
  )
})


