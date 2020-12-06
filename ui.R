# Lab04

# Define UI for application
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Crime State by State Exploratory Application"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            
            radioButtons("typeofdata","Pick a variable",c("US", "State","Region")),
            
            uiOutput("plotOptions"),
            
            uiOutput("plotOptions2"),
            downloadButton('downloadPlot', 'Download Hi-Res PNG Plot')
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("linePlot1",height="300px"),
            p(strong("Notes:"), "This application allows the user to explore the rates of different types of crime in the United States across different states and regions over time. The data for this application is sourced from the", a ("FBI’s Uniform Crime Reporting Survey.", href = "https://www.ucrdatatool.gov"), "It is important to note that in our analysis, the variable Rape Rate refers to “legacy rape,” which tallies the number of cases of rape in the United States given the original definition of rape, instead of “revised rape” which tallies the number of rape cases from 2013 onwards, given the updated definition of rape. This decision was made in order to keep the data consistent over time."),
            p(strong("App Instructions:"),"This application allows users to visualize both time series plots and bubble plots for different crime rates and years. To create a time series plot, first choose between total US data, state data, and region data. Then select a range of years to analyze and the crime rate of interest. To create a bubble plot with faceting, click on US data, then select the Bubble graph option. Next, select two crime rates and  the years of interest for side-by-side comparison."),
            
            p(strong("Authors:"), "Andrew Bridglall, Meghan Buonanno, Lindsey Clark, Katherine McDonald")
            
        
    )
)))


