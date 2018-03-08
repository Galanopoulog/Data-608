# Question 2
# Are states improving mortality rates (per cause) faster/slower than national average?
# Visualization for one cause of death at a time.
# National average should be weighted average by the national population.

shinyServer(function(input, output, session) {
  # filtering reactive data
  picking = reactive({
    data = merged %>% filter(merged$ICD.Chapter == input$Chapters, merged$State == input$State)
  })
  
  output$plot = renderPlot({
    data = merged %>% filter(merged$ICD.Chapter == input$Chapters, merged$State == input$State)

    # generate a Year vs Mortality Rate plot
    ggplot(picking(), aes(x=Year, y=Crude.Rate)) +
      geom_line() +
      geom_line(data = picking(), aes(x=Year, y=NatAvg, color = "red"))
  })
  
  # Top 10 States Table
  output$Top10 = renderTable({
    data = merged %>% filter(merged$ICD.Chapter == input$Chapters, merged$State == input$State)
    data2 = data[with(data, order(-Crude.Rate)),]
    data2[1:10, -c(1,3)]
  },
  caption = "STATE MORTALITY TABLE",
  caption.placement = getOption("xtable.caption.placement", "top")
  )
})
