library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(reshape2)
library(maps)
library(mapproj)
library(choroplethr)
library(choroplethrMaps)
library(rworldmap)
library(gapminder)
library(grid)


### US DATASET ###
usData = read.csv("https://raw.githubusercontent.com/Galanopoulog/Data-608/master/Final/USguns.csv", header = T)

# Format date and spread it out by day, month, year
usData$Incident.Date = as.Date(usData$Incident.Date, format = "%d-%b-%y")
usData$Year = as.numeric(format(usData$Incident.Date, format = "%Y"))
usData$Month = as.factor(format(usData$Incident.Date, format = "%m"))
usData$Day = as.factor(format(usData$Incident.Date, format = "%d"))

# Remove outlier
us = usData[-15037,]

### GLOBAL DATASET ###
one = read.csv("https://raw.githubusercontent.com/Galanopoulog/Data-608/master/Final/WorldwideGunData_1.csv", header = T)
two = read.csv("https://raw.githubusercontent.com/Galanopoulog/Data-608/master/Final/WorldwideGunData_2.csv", header = T)
three = read.csv("https://raw.githubusercontent.com/Galanopoulog/Data-608/master/Final/WorldwideGunData_3.csv", header = T)
world = do.call("rbind", list(one, two, three))


worlds = subset(world, metric == "Percent") %>% 
  group_by(location, year) %>% summarise(percent = sum(val))

################################################################
################################################################
################################################################

ui = fluidPage(
   
   # Title
   titlePanel("Gun Violence"),
   
   # Slider
   sidebarLayout(
      sidebarPanel(
        # US INPUTS
         sliderInput("year",
                     "US Years:",
                     min = min(us$Year),
                     max = max(us$Year),
                     value = min(us$Year),
                     animate = TRUE,
                     sep= ""
         ),
        
        selectInput("status", 
                    label = "Choose Status:",
                    choices = c("Killed", "Injured"),
                    selected = "Killed")
        ,
        # GLOBAL INPUTS
        sliderInput("globyears",
                    "Global Years:",
                    round = TRUE,
                    min = floor(world$year),
                    max = ceiling(max(world$year)),
                    value = min(world$year),
                    animate = TRUE,
                    sep= "",
                    ticks = TRUE
        ),
        selectInput("location", "Location:", choices = unique(worlds$location)),
        
        # STATS INPUTS
        radioButtons("logs", "Variable Transformation:", choices = c("None", 
                                                                    "Log(Physical Violence)", 
                                                                    "Log(Self-Harm)",
                                                                    "Log of Both"))
        ),
        
      # Show plot
      mainPanel(
        
        # Output: Tabset w/ plot, summary, and table ----
        tabsetPanel(type = "tabs",
                    tabPanel("United States", plotOutput("us1"), plotOutput("us2"), plotOutput("usOverall")),
                    tabPanel("Global", plotOutput("glob1"), plotOutput("glob2")),
                    tabPanel("Comparisons", dataTableOutput("stats"), plotOutput("stats2"), plotOutput("stats3"))
        )
      )
   )
)

################################################################
server = function(input, output) {
  
  # Reactive US Datasets
  usdata = reactive({
    subset(us, Year == input$year)
  })
  
  # Reactive Global Datasets
  globdata = reactive({
    subset(worlds, location == input$location)
  })
  globdata2 = reactive({
    subset(world, metric == "Percent" & year == input$globyears & location == input$location)
  })
  globdata3 = reactive({
    subset(world, metric == "Percent" & year == input$globyears, select = c(location, val))
  })
  
  # US PLOTS
  output$us1 = renderPlot({
    us2 = usdata() %>% group_by(State) %>% summarise(Kill = sum(Killed), Inj = sum(Injured))
    if(input$status == "Killed"){
      chorodata = data.frame(region=tolower(us2$State), value=us2$Kill)
      chorodata$region = as.character(chorodata$region)
      state_choropleth(chorodata)
    }else{
      chorodata = data.frame(region=tolower(us2$State), value=us2$Inj)
      chorodata$region = as.character(chorodata$region)
      state_choropleth(chorodata)
    }
  })
   output$us2 = renderPlot({
     sums = subset(usdata(), select = c("State", "Killed", "Injured")) %>% group_by(State) %>% summarise(kill = sum(Killed), inj = sum(Injured))
     if(input$status == "Killed"){
       ggplot(sums, aes(x=reorder(State, -kill), y=kill)) + 
         geom_bar(stat="identity", position=position_dodge(), fill = "firebrick") +
         theme(axis.text.x=element_text(angle=90,hjust=1)) + 
         labs(title = "Number Killed by State Per Year") +
         xlab("State") +
         ylab("Number Killed")
     } else {
       ggplot(sums, aes(x=reorder(State, -inj), y=inj)) + 
         geom_bar(stat="identity", position=position_dodge(), fill = "cornflowerblue") +
         theme(axis.text.x=element_text(angle=90,hjust=1))+ 
         labs(title = "Number Injured by State Per Year") +
         xlab("State") +
         ylab("Number Injured")
     }
   })
   output$usOverall = renderPlot({   
     sums2 = usData[-15037,]#subset(us, select = c("Year", "Month", "Killed", "Injured")) %>% group_by(Incident.Date) %>% summarise(kill = sum(Killed), inj = sum(Injured))
     if(input$status == "Killed"){
       ggplot(sums2, aes(Month, Day )) +
         geom_tile(aes(fill = Killed), color = "white") +
         scale_fill_gradient(low = "white", high = "firebrick") +
         ylab("Day ") +
         xlab("Month") +
         theme(legend.title = element_text(size = 10),
               legend.text = element_text(size = 12),
               plot.title = element_text(size=16),
               axis.title=element_text(size=14,face="bold"),
               axis.text.x = element_text(angle = 90, hjust = 1)) +
         labs(title = "Heatmap of Number Killed", fill = "Number Killed")
     } else {
       ggplot(sums2, aes(Month, Day )) +
         geom_tile(aes(fill = Injured), color = "white") +
         scale_fill_gradient(low = "white", high = "black") +
         ylab("Day ") +
         xlab("Month") +
         theme(legend.title = element_text(size = 10),
               legend.text = element_text(size = 12),
               plot.title = element_text(size=16),
               axis.title=element_text(size=14,face="bold"),
               axis.text.x = element_text(angle = 90, hjust = 1)) +
         labs(title = "Heatmap of Number Injured", fill = "Number Injured")
     }
    })

   # GLOBAL PLOTS
   output$glob1 = renderPlot({
     ok = globdata3() %>% group_by(location) %>% summarise(value = sum(val))
     names(ok) = c("region", "value")
     ok$region = tolower(ok$region)
     ok$region[ok$region == "united states"] = "united states of america"
     ok = data.frame(ok)
     country_choropleth(ok, "Global Percent Deaths by Firearm")
   })
   output$glob2= renderPlot({
     x = subset(worlds, location == "United States" | location == input$location)
     ggplot(data.frame(x), aes(x=year, y=percent)) +
       geom_line(color="darkred", lwd=1) +
       geom_point() +
       facet_wrap(~location, scales = "free_y") +
       labs(title = "Percent Deaths Per Year") +
       xlab("Year") +
       ylab("Percent Deceased")
   })
   
   # STATS TAB
   output$stats = renderDataTable({
     worlds %>% group_by(location) %>% summarise(Mean = mean(percent))
   })
   output$stats2 = renderPlot({
     subset(world, metric == "Percent" & year == input$globyears & location == input$location)
     ggplot(globdata2(), aes(x=age, y=val, fill = cause)) + 
       geom_bar(stat = "identity") + 
       facet_wrap(~cause) +
       theme(axis.text.x=element_text(angle=90,hjust=1)) + 
       labs(title = "Percent Deceased Per Cause", subtitle = "By Country Per Year")
   })
   output$stats3 = renderPlot({
     try = subset(world, metric == "Percent", select = c(location, year, cause, val)) %>% 
       group_by(location, year,cause) %>% 
       summarize(val = sum(val)) %>% 
       spread(cause, val)
     try = data.frame(try)
     
     if (input$logs == "None"){
       ggplot(try, aes(x= Self.harm.by.firearm, y = Physical.violence.by.firearm)) + 
         geom_point() + 
         geom_smooth(method=lm) +
         labs(title = "Deaths: % Violent vs % Self-harm") +
         xlab("Self-Harm") +
         ylab("Physical Violence")
     } else if (input$logs == "Log(Self-Harm)"){
       ggplot(try, aes(x= log(Self.harm.by.firearm), y = Physical.violence.by.firearm)) + 
         geom_point() + 
         geom_smooth(method=lm) +
         labs(title = "Deaths: % Violent vs % Self-harm") +
         xlab("Log(Self-Harm)") +
         ylab("Physical Violence")
     } else if (input$logs == "Log(Physical Violence)"){
       ggplot(try, aes(x= Self.harm.by.firearm, y = log(Physical.violence.by.firearm))) + 
         geom_point() + 
         geom_smooth(method=lm) +
         labs(title = "Deaths: % Violent vs % Self-harm") +
         xlab("Self-Harm") +
         ylab("Log(Physical Violence)")
     } else {
       ggplot(try, aes(x= log(Self.harm.by.firearm), y = log(Physical.violence.by.firearm))) + 
         geom_point() + 
         geom_smooth(method=lm) +
         labs(title = "Deaths: % Violent vs % Self-harm") +
         xlab("Log(Self-Harm)") +
         ylab("Log(Physical Violence)")
     }
   })
   
}

# Run the app
shinyApp(ui = ui, server = server)

