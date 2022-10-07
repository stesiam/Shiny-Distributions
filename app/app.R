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
    title = "Plot",width = 4, status = "primary", solidHeader = TRUE,
    collapsible = FALSE
  ),
  box(
    title = "Plot",width = 4, status = "primary", solidHeader = TRUE,
    collapsible = FALSE
  ),
  box(
    title = "Plot",width = 4, status = "primary", solidHeader = TRUE,
    collapsible = FALSE
  )
),
  
  fluidRow(
    box(
      title = "Box title", width = 2, status = "primary",
      "Box content"
    ),
    box(
      status = "warning", width = 2,
      "Box content"
    ),
    box(
      status = "warning", width = 2,
      "Box content"
    ),
    box(
      status = "warning", width = 2,
      "Box content"
    ),
    box(
      status = "warning", width = 2,
      "Box content"
    ),
    box(
      status = "warning", width = 2,
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
  
  
}

shinyApp(ui = ui, server = server)