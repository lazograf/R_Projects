
#Following the book "Economics with Heterogeneous Interacting Agents:
#A Practical Guide to Agent-Based Modeling " (Caiani et al., 2016)
#and using the "Shiny" package from RStudio


library(shiny)
library(ggplot2)

set.seed(20190131)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Agent Based Modeling: Simulation of the Economy"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("Time",
                        "Time:",
                        min = 2,
                        max = 5000,
                        value = 1000),
            sliderInput("Firms",
                        "Number of Companies:",
                        min = 2,
                        max = 500,
                        value = 100),
            sliderInput("gamma",
                        "gamma_parameter:",
                        min = 0.1,
                        max = 2,
                        value = 1.1),
            sliderInput("phi",
                        "phi_parameter:",
                        min=0,
                        max = 1,
                        value = 0.1),
            sliderInput("r",
                        "Interest rate - r:",
                        min= 0.01,
                        max = 0.99,
                        value = 0.1),
            sliderInput("Pbar",
                        "Random_price_constant:",
                        min = 0.01,
                        max = 0.99,
                        value=0.01),
            checkboxInput("inv_neg",
                          "Do NOT Permit investment in case of negative profits", TRUE),
            
            checkboxInput("cap_depr",
                          "Allow capital depreciation", TRUE)            
        ),

        # Plot of Aggregate Production
        mainPanel(
           plotOutput("Aggregate_Production")
        )
    )
)



server <- function(input, output) {
  
  output$Aggregate_Production <- renderPlot({   
    
    set.seed(20190131)
    
        #ALLOCATING VARIABLES AND INITIAL CONDITIONS
        #Firms' net worth
        A <-  matrix(data=1,ncol=1,nrow=input$Firms) 
        #Firms' capital
        K <- matrix(data=1,ncol=1,nrow=input$Firms) 
        #Firms' debt
        B <- matrix(data=0,ncol=1,nrow=input$Firms) 
        #Firms' investment
        I <- matrix(data=0,ncol=1,nrow=input$Firms) 
        #Stochastic price
        P <- matrix(data=0,ncol=1,nrow=input$Firms) 
        #Firms' production
        Y <- matrix(data=0,ncol=1,nrow=input$Firms) 
        #Firms' profit
        Z <-  matrix(2*runif(input$Firms)+input$Pbar,ncol=1,nrow=input$Firms)
        #Aggregate production
        YY <- matrix(data=0,ncol=1,nrow=input$Time) 
        
        #Initialize Net Worth and Debt matrix
        AA <- matrix(data=0,ncol=1,nrow = input$Time) 
        BB <- matrix(data=0,ncol=1,nrow = input$Time)  
        
        #Capital Depreciation rate
        delta = 0.01
                     
    
     
    
    
    #SEQUENCE OF EVENTS
  for (t in 2:input$Time) { #t=1 used to initialize the firms' variables
                    
        I <- input$gamma * Z #Investment choice
        if (input$inv_neg == TRUE) {
        I[I<0] <- 0 #no investment in case of negative profits
        }
        if (input$cap_depr == TRUE) {
        K <- K - K*delta #capital depreciation
        }
        K <- K + I #Capital accumulation
        Y <- input$phi * K #Production
        B <- K - A #Debt
        B[B<0] <- 0 #Self-financed firms - Not permitting here negative debt
        P <- 2*runif(input$Firms)+ input$Pbar #Stochastic price
        Z <- P * Y - input$r * K #Profit
        A <- A + Z #Net worth
        Z[A<0] <- 0 #Entry condition
        K[A<0] <- 1 #Entry condition
        A[A<0] <- 1 #Entry condition
        YY[t] <- sum(Y) #Aggregate production
        AA[t] <- sum(A) #Net worth
        BB[t] <- sum(B) #Debt
      
        
        } 
           
              
      

    

    #PLOTTING AGGREGATE PRODUCTION
   plot(YY[50:(length(YY)),],type="l",ylab="Aggregate Production of the Economy",xlab="time") })
    

}
# Run the application 
shinyApp(ui = ui, server = server)
