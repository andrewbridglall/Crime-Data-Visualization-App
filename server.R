#Lab04

# Define server logic required to draw a time series
shinyServer(function(input, output) {
    
    
    output$plotOptions <- renderUI({
        
        if(input$typeofdata == "US")
        {
            
                radioButtons("charttype","Pick Chart Type", choices=list("Line", "Bubble"))
 
        }
        
        else if(input$typeofdata == "State")
        {
            tagList(
                sliderInput("currentYear",
                            "Choose a Year",
                            min = 1960,
                            max = 2014,
                            value = c(1960,2014),
                            sep=""),
                selectInput("stateOfInterest", "Pick States to Highlight",
                            levels(as.factor(crime$State)), multiple = T),
                
                selectInput("state_crime_rates", "Pick a Crime Rate",
                            c("Murder Rate" = "`Murder and nonnegligent manslaughter rate`",
                              "Rape Rate" = "`Legacy rape rate /1`", 
                              "Assault Rate" = "`Aggravated assault rate`",
                              "Larceny Theft Rate" = "`Larceny-theft rate`", 
                              "Motor Vehicle Theft Rate" = "`Motor vehicle theft rate`",
                              "Property Crime Rate" = "`Property crime rate`",
                              "Burglary Rate" = "`Burglary rate`",
                              "Robbery Rate" = "`Robbery rate`"))
                
            )
            
        }
        else if(input$typeofdata == "Region")
        {
            tagList(
                sliderInput("currentYear",
                            "Choose a Year",
                            min = 1960,
                            max = 2014,
                            value = c(1960,2014),
                            sep=""),
                selectInput("regionOfInterest", "Pick Regions to Highlight",
                            levels(as.factor(regional_rates$region)), multiple = T),
                
                selectInput("regional_rates", "Pick a Crime Rate",
                            c("Violent Crime Rate" = "violent_rate",
                              "Murder Rate" = "murder_rate", 
                              "Rape Rate" = "rape_rate",
                              "Assault Rate" = "assault_rate", 
                              "Robbery Rate" = "robbery_rate"))
            )
            
            
            
        }
    })
    
    requestedPlot <- reactive({ 
        if(input$typeofdata=="US")
        {
            plot_data1 <- filter(crime,State == "United States-Total")
            
            
            if(input$charttype=="Line"){ggplot(plot_data1)+ 
                geom_line(aes_string(x="Year", y=input$crime_rates, color="State"))+ 
                labs(title=paste("Total US Crime Rates from", input$currentYear[1], "to",input$currentYear[2], "for", input$crime_rates), x="Year", y="Crimes per 100,000 residents") +
                xlim(input$currentYear) +
                theme_classic()+guides(color=FALSE)}
            else if(input$charttype=="Bubble"){
                #this allows you to only show the bubble plots for the chosen years
                plotdata3 <- filter(crime, Year %in% input$chosenYears)
                ggplot(plotdata3,aes_string(x=input$crime_rates1, y=input$crime_rates2, size="Population",color="Population"))+geom_point(alpha=0.6) + facet_wrap(~Year) +theme_classic()+guides(size=FALSE)+scale_color_gradient(name="Population",low="blue",high="red")
            }}
        
        
        else if(input$typeofdata=="State")
        {
            plot_data2 <- filter(crime, State %in% input$stateOfInterest)
            
            ggplot(plot_data2)+ 
                geom_line(aes_string(x="Year", y=input$state_crime_rates, color="State"))+ 
                labs(title=paste("State Crime Rates from", input$currentYear[1], "to",input$currentYear[2], "for", input$state_crime_rates), x="Year", y="Crimes per 100,000 residents") +
                xlim(input$currentYear) +
                theme_classic()
        }
        else if(input$typeofdata=="Region")
        {
            state_crime <- filter(crime,State != "United States-Total")
            
            northeast_states<- c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", "Vermont", "New Jersey", "New York", "Pennsylvania")
            midwest_states <- c("Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin", "Iowa", "Kansas", "Minnesota", "Missouri", "Nebraska", "North Dakota", "South Dakota")
            south_states <- c("Delaware", "Florida", "Georgia", "Maryland", "North Carolina", "South Carolina", "Virginia", "District of Columbia", "West Virginia", "Alabama", "Kentucky", "Mississippi", "Tennessee", "Arkansas", "Louisiana", "Oklahoma", "Texas")
            west_states <- c("Arizona", "Colorado", "Idaho", "Montana", "Nevada", "New Mexico", "Utah", "Wyoming", "Alaska", "California", "Hawaii", "Oregon", "Washington")
            
            state_crime <- mutate(state_crime, region=ifelse(State %in% northeast_states, "Northeast",
                                                             ifelse(State %in% midwest_states, "Midwest",
                                                                    ifelse(State %in% south_states, "South",
                                                                           ifelse(State %in% west_states, "West","NA")))))
            regional_rates <- summarize(group_by(state_crime, region,Year),
                                        violent_rate = sum(`Violent crime total`)/sum(Population)*100000,
                                        murder_rate = sum(`Murder and nonnegligent Manslaughter`)/sum(Population)*100000,
                                        rape_rate = sum(`Legacy rape /1`)/sum(Population)*100000,
                                        assault_rate = sum(`Aggravated assault`)/sum(Population)*100000,
                                        robbery_rate = sum(`Robbery`)/sum(Population)*100000)
            
            
            plot_data3 <- filter(regional_rates, region %in% input$regionOfInterest)
            
            ggplot(plot_data3)+ 
                geom_line(aes_string(x="Year", y=input$regional_rates, color="region"))+ 
                labs(title=paste("Regional Crime Rates from", input$currentYear[1],"to", input$currentYear[2], "for", input$regional_rates), x="Year", y="Crimes per 100,000 residents") +
                xlim(input$currentYear) +
                theme_classic()
            
        }
        
        
    })
    
    output$linePlot1 <- renderPlot({
      print(requestedPlot())
    }) 
    output$plotOptions2 <- renderUI({
        
        
        if(input$typeofdata=="US" & input$charttype=="Bubble")
        {
            
            tagList(
                
         
            selectInput("crime_rates1", "Pick a Crime Rate for X Axis",
                        c("Murder Rate" = "`Murder and nonnegligent manslaughter rate`",
                          "Rape Rate" = "`Legacy rape rate /1`", 
                          "Assault Rate" = "`Aggravated assault rate`",
                          "Larceny Theft Rate" = "`Larceny-theft rate`", 
                          "Motor Vehicle Theft Rate" = "`Motor vehicle theft rate`",
                          "Property Crime Rate" = "`Property crime rate`",
                          "Burglary Rate" = "`Burglary rate`",
                          "Robbery Rate" = "`Robbery rate`")),
            
            
            selectInput("crime_rates2", "Pick a Crime Rate for Y Axis",
                        c("Murder Rate" = "`Murder and nonnegligent manslaughter rate`",
                          "Rape Rate" = "`Legacy rape rate /1`", 
                          "Assault Rate" = "`Aggravated assault rate`",
                          "Larceny Theft Rate" = "`Larceny-theft rate`", 
                          "Motor Vehicle Theft Rate" = "`Motor vehicle theft rate`",
                          "Property Crime Rate" = "`Property crime rate`",
                          "Burglary Rate" = "`Burglary rate`",
                          "Robbery Rate" = "`Robbery rate`")) ,
            
            #this allows a third option for the user to chose years for the bubble plot
            selectInput("chosenYears","Pick Years to Analyze",1960:2014,multiple=T,selected=1960) 
            )
        }
        
        else if(input$typeofdata=="US" & input$charttype=="Line")
        {tagList(sliderInput("currentYear",
                             "Choose a Year",
                             min = 1960,
                             max = 2014,
                             value = c(1960,2014),
                             sep=""),
                 selectInput("crime_rates", "Pick a Crime Rate",
                             c("Murder Rate" = "`Murder and nonnegligent manslaughter rate`",
                               "Rape Rate" = "`Legacy rape rate /1`", 
                               "Assault Rate" = "`Aggravated assault rate`",
                               "Larceny Theft Rate" = "`Larceny-theft rate`", 
                               "Motor Vehicle Theft Rate" = "`Motor vehicle theft rate`",
                               "Property Crime Rate" = "`Property crime rate`",
                               "Burglary Rate" = "`Burglary rate`",
                               "Robbery Rate" = "`Robbery rate`"), selected="Murder Rate"))
            
        }
       
    })
   output$downloadPlot <- downloadHandler(
        filename = "customMPDplot.png",
        content = function(file){
          ggsave(file,requestedPlot(),scale=3,limitsize=F)
       })
})






