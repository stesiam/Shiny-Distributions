library(shiny)
library(shinydashboard)

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
     numericInput("lambda", "Lambda" , value = 2)),
   
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
                  "Gamma" = "gamma",
                  "Beta" = "beta",
                  "Cauchy" = "cauchy",
                  "Weibull" = "weibull")),
  )
)

## Main panel

body  = dashboardBody()



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
}

shinyApp(ui = ui, server = server)