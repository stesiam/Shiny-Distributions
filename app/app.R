library(shiny)
library(shinydashboard)


## Add dependent files

source("global.R")

## Header


header = dashboardHeader(title = "Shiny Distributions", titleWidth = 230)

## Sidebar

sidebar = dashboardSidebar(
  selectInput("type_of_distribution", "Select type of Distribution:",
              c("Discrete" = "dis",
                "Continuous" = "cont")),
  
  
  # Conditional Panel for Discrete Distributions
  
  conditionalPanel(
    condition = "input.type_of_distribution == 'dis'",
    selectInput("discrete_dist", "Select Distribution:",
                c("Bernoulli" = "bernoulli",
                  "Binomial" = "binom",
                  "Poisson" = "poisson",
                  "Geometric" = "geom",
                  "Negative Binomial (0)" = "negbinom0",
                  "Negative Binomial (1)" = "negbinom1",
                  "Hypergeometric" = "hypergeom",
                  "Discrete Uniform" = "dunif")),
    
    ### Conditional Panels for each distribution
    
    ### Bernoulli,
    
    conditionalPanel(
      condition = "input.discrete_dist == 'bernoulli'",
      sliderInput("bernoulli_p", "Probability", min = 0, max = 1, value = 0.5)),
    
   ## Binomial 
   
   conditionalPanel(
     condition = "input.discrete_dist == 'binom'",
     numericInput("n_binom", "Number of events" , value = 2),
     sliderInput("binom_p", "Probability", min = 0, max = 1, value = 0.5)),
   
   
   ## Poisson
   
   conditionalPanel(
     condition = "input.discrete_dist == 'poisson'",
     numericInput("lambda_poisson", "Lambda" , value = 2)),
   
   ## Geometric
   
   conditionalPanel(
     condition = "input.discrete_dist == 'geom'",
     numericInput("n_binom", "p" , value = 0.5)),
   
   
   ## NB0
   
   conditionalPanel(
     condition = "input.discrete_dist == 'negbinom0'",
     numericInput("r1", "r" , value = 0.5),
     numericInput("p1", "p" , value = 0.5)),
   
   
   
   ## NB1
   
   
   conditionalPanel(
     condition = "input.discrete_dist == 'negbinom1'",
     numericInput("r1", "r" , value = 0.5),
     numericInput("p1", "p" , value = 0.5)),
   
   
   ## Hypergeometric
   
   conditionalPanel(
     condition = "input.discrete_dist == 'hypergeom'",
     numericInput("r1", "r" , value = 0.5),
     numericInput("p1", "p" , value = 0.5),
     numericInput("r1", "r" , value = 0.5)),
   
   
   ## Discrete Uniform
   
   conditionalPanel(
     condition = "input.discrete_dist == 'dunif'",
     sliderInput("a_dunif", "a" , value = 10, min = 1, max = 45),
     sliderInput("b_dunif", "b" , value = 16, min = 1, max = 100))
   
   
    
  ),
  
  
  
  
  # Conditional Panel for Continuous Distributions
  
  conditionalPanel(
    condition = "input.type_of_distribution == 'cont'",
    selectInput("contin_distributions", "Select Distribution:",
                c("Normal" = "normal",
                  "Uniform" = "uniform",
                  "Expenential" = "exp",
                  "Beta" = "beta",
                  "Cauchy" = "cauchy",
                  "Weibull" = "weibull")),
    
    ## Normal Distribution
    
    conditionalPanel(
      condition = "input.contin_distributions == 'normal'",
      numericInput("mean_normal", "mu" , value = 10, min = 1, max = 45),
      numericInput("var_normal", "sigma" , value = 16, min = 1, max = 100)),
    
    ## Uniform Distribution
    
    conditionalPanel(
      condition = "input.contin_distributions== 'uniform'",
      sliderInput("a_unif", "a" , value = 10, min = 1, max = 45),
      sliderInput("b_unif", "b" , value = 16, min = 1, max = 100)),
    
    ## Exponential Distribution
    
    conditionalPanel(
      condition = "input.contin_distributions== 'exp'",
      sliderInput("lambda_exp", "lambda" , value = 10, min = 1, max = 45)),
    
    
    ## Beta Distribution
    
    conditionalPanel(
      condition = "input.contin_distributions== 'beta'",
      sliderInput("a_beta", "a" , value = 10, min = 1, max = 45),
      sliderInput("b_beta", "b" , value = 16, min = 1, max = 100)),
    
    
    ## Cauchy Distribution
    
    conditionalPanel(
      condition = "input.contin_distributions== 'cauchy'",
      sliderInput("x0_cauchy", "x" , value = 10, min = 1, max = 45),
      sliderInput("gamma_cauchy", "Gamma" , value = 16, min = 1, max = 100)),
    
    
    ## Weibull Distribution
    
    conditionalPanel(
      condition = "input.contin_distributions== 'weibull'",
      sliderInput("lambda_wei", "lambda" , value = 10, min = 1, max = 45),
      sliderInput("k_wei", "k" , value = 16, min = 1, max = 100))
    
  )
)

## Main panel

body  = dashboardBody(
  fluidRow(
    box(
      title = "Plot - PDF",
      width = 6,
      status = "primary",
      solidHeader = TRUE,
      collapsible = FALSE
    ),
    box(
      title = "Plot - CDF",
      width = 6,
      status = "primary",
      solidHeader = TRUE,
      collapsible = FALSE
    )
  ),
  
  fluidRow(
    box(
      title = "Mean",
      width = 3,
      status = "primary",
      
       # Bernoulli 
      conditionalPanel(
        condition = "input.discrete_dist== 'bernoulli'",
        verbatimTextOutput("mean")
        
      # Binomial
      
      #
      
      
      )
    ),
    box(
      title = "Variance",
      width = 3,
      status = "primary",
      verbatimTextOutput("var")
    ),
    box(
      title = "Median",
      width = 3,
      status = "primary",
      verbatimTextOutput("median")
    ),
    box(
      title = "Mode",
      width = 3,
      status = "primary",
      verbatimTextOutput("mode")
    )
  ),
  
  fluidRow(
    box(
      title = "Skewness",
      width = 3,
      status = "primary",
      verbatimTextOutput("skewness")
    ),
    box(
      title = "Kurtosis",
      width = 3,
      status = "primary",
      verbatimTextOutput("kurtosis")
    ),
    box(
      title = "Entropy",
      width = 3,
      status = "primary",
      verbatimTextOutput("entropy")
    ),
    box(
      title = "Fisher Information",
      width = 3,
      status = "primary",
      "Box content"
    )
  )
  
)



## Gather all the components
ui <- dashboardPage( 
  header = header,
  sidebar = sidebar,
  body = body
)

###############################
###############################
###############################


server <- function(input, output) {
  
  
  # Plot PDF
  
  
  # Plot CDF
  
  
  
  
  # Mean 

  output$mean <- renderText({ bern_mean(input$bernoulli_p) })
  
  # Variance
  
  output$var <- renderText(({ bern_var(input$bernoulli_p) }))
  
  # Median
  
  output$median <- renderText(({ bern_median(input$bernoulli_p) }))
  
  # Mode
  
  output$mode <- renderText(({ bern_mode(input$bernoulli_p) }))
  
  # Skewness
  
  output$skewness <- renderText(({ bern_skewness(input$bernoulli_p) }))
  
  # Kurtosis
  
  output$kurtosis <- renderText(({ bern_kurtosis(input$bernoulli_p) }))
  
  # Entropy
  
  output$entropy <- renderText(({ bern_entropy(input$bernoulli_p) }))

}

shinyApp(ui = ui, server = server)