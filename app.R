#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("Election Simulation Functions.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Monte Carlo Election Simulation"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "graph_type", label = "What Do You Want to Plot?", choices = list("Win Probability", 
                                                                                      "Possible Electoral Scenarios"), selected = "Win Probability"),
            h4("Enter a Probability of a Biden Victory in Each State"),
            "Preset probabilities are those found on a variety of betting sites", 
            textInput("pa", "PA", value = 0.75), 
            textInput("ga", "GA", value = 0.4), 
            textInput("nc", "NC", value = 0.3), 
            textInput("nv", "NV", value = 0.8), 
            textInput("az", "AZ", value = 0.787), 
            actionButton("run", label = "Run Simulation"), 
            h4("Enter the Number of Iterations to Conduct"), 
            "Performs best around 10,000", 
            textInput("iter_num", "Number of Iterations", value = 10000)
            
        ), 

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    observeEvent(input$run, {
        
        pa <<- as.numeric(input$pa)
        ga <<- as.numeric(input$ga)
        nc <<- as.numeric(input$nc)
        nv <<- as.numeric(input$nv)
        az <<- as.numeric(input$az) 
        pa_elec <<- 20
        ga_elec <<- 16 
        nc_elec <<- 15
        nv_elec <<- 6
        az_elec <<- 11
        
        biden_elec <<- 253 
        trump_elec <<- 214 
        state_names <<- c("pa", "ga", "nc", "nv", "az")
        swing_odds <<- c(pa, ga, nc, nv, az)
        names(swing_odds) <- state_names
        swing_elec <<- c(pa_elec, ga_elec, nc_elec, nv_elec, az_elec)
        names(swing_elec) <- state_names
       
        win <- game_election(as.numeric(input$iter_num))
        
        if(input$graph_type == "Possible Electoral Scenarios") {
            color_scheme <- color_scheme <- c("Trump Victory or Tie" = "red", "Biden Victory" =  "blue")
            
            output$plot <- renderPlot({
                ggplot(win %>% group_by(biden_points) %>% summarise(n = n()) %>%  
                           ungroup() %>% 
                           mutate(Victory = ifelse(biden_points >= 270, "Biden Victory", "Trump Victory or Tie"), 
                                  Percent = (n/sum(n))*100))+
                    geom_col(aes(biden_points, Percent, fill = Victory))+
                    scale_fill_manual(values = color_scheme)+
                    theme_bw()+labs(x = "Electoral Points for Biden")
            
            })
        }
        else{
            chances <- win %>%  #see functions script for details
                group_by(Winner) %>% 
                summarise(n = n()) %>%  
                ungroup() %>%  
                mutate(win_percent = (n/sum(n))*100)
            
            
            color_scheme <- c("Trump Wins" = "red", "Biden Wins" =  "blue", "Tie" = "green")
            output$plot <- renderPlot({
                ggplot(chances)+
                geom_col(aes(Winner, win_percent, fill = Winner), color = "black")+
                geom_text(aes(Winner, win_percent+5, label = paste0(win_percent, "%")))+
                scale_fill_manual(values =color_scheme)+ 
                theme(legend.position = "none")+
                theme_bw()+
                labs(x = "Winner", y = "Percent Chance of Winning")
                
            })
        }
            
        })
}

    



# Run the application 
shinyApp(ui = ui, server = server)
