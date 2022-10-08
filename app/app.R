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
                  "Geometric (0)" = "geom0",
                  "Geometric (1)" = "geom1",
                  "Negative Binomial (0)" = "negbinom0",
                  "Negative Binomial (1)" = "negbinom1",
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
   
   ## Geometric0
   
   conditionalPanel(
     condition = "input.discrete_dist == 'geom0'",
     sliderInput("p0_geom", "p" , value = 0.5, max = 1, min = 0)),
   
   conditionalPanel(
     condition = "input.discrete_dist == 'geom1'",
     sliderInput("p1_geom", "p" , value = 0.5, max = 1, min = 0)),
   
   ## NB0
   
   conditionalPanel(
     condition = "input.discrete_dist == 'negbinom0'",
     numericInput("r0", "r" , value = 5),
     sliderInput("p0", "p" , value = 0.5, max = 1, min = 0)),
   
   
   
   ## NB1
   
   
   conditionalPanel(
     condition = "input.discrete_dist == 'negbinom1'",
     numericInput("r1", "r" , value = 5),
     sliderInput("p1", "p" , value = 0.5, max = 1, min = 0)),
   
   
   
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
      
      conditionalPanel(
        condition = "input.type_of_distribution == 'dis'",
      
       # Bernoulli 
      conditionalPanel(
        condition = "input.discrete_dist== 'bernoulli'",
        verbatimTextOutput("mean_bern")),
        
      # Binomial
    
      conditionalPanel(
        condition = "input.discrete_dist== 'binom'",
        verbatimTextOutput("mean_binom")),
      
      # Poisson
      
      conditionalPanel(
        condition = "input.discrete_dist== 'poisson'",
        verbatimTextOutput("mean_poisson")),
      
      # Geom0
      
      conditionalPanel(
        condition = "input.discrete_dist== 'geom0'",
        verbatimTextOutput("mean_geom0")),
      
      # Geom1
      
      conditionalPanel(
        condition = "input.discrete_dist== 'geom1'",
        verbatimTextOutput("mean_geom1")),
      
      # NB0
      
      conditionalPanel(
        condition = "input.discrete_dist== 'negbinom0'",
        verbatimTextOutput("mean_nb0")),
    
      
      # Discrete Uniform
      
      conditionalPanel(
        condition = "input.discrete_dist== 'dunif'",
        verbatimTextOutput("mean_dunif"))
     
      ),
      
      conditionalPanel(
        condition = "input.type_of_distribution == 'cont'",
       
      # Normal
      
      conditionalPanel(
        condition = "input.contin_distributions== 'normal'",
        verbatimTextOutput("mean_norm")),
      
      # Continuous uniform
      
      conditionalPanel(
        condition = "input.contin_distributions== 'uniform'",
        verbatimTextOutput("mean_unif")),
      
      # Exponential
      
      conditionalPanel(
        condition = "input.contin_distributions== 'exp'",
        verbatimTextOutput("mean_exp"))
      
     
      )
      
    ),
    box(
      title = "Variance",
      width = 3,
      status = "primary",
      
      conditionalPanel(
        condition = "input.type_of_distribution == 'dis'",
        
        # Bernoulli 
        conditionalPanel(
          condition = "input.discrete_dist== 'bernoulli'",
          verbatimTextOutput("var_bern")),
        
        # Binomial
        
        conditionalPanel(
          condition = "input.discrete_dist== 'binom'",
          verbatimTextOutput("var_binom")),
        
        # Poisson
        
        conditionalPanel(
          condition = "input.discrete_dist== 'poisson'",
          verbatimTextOutput("var_poisson")),
        
        # Geom0
        
        conditionalPanel(
          condition = "input.discrete_dist== 'geom0'",
          verbatimTextOutput("var_geom0")),
        
        # Geom1
        
        conditionalPanel(
          condition = "input.discrete_dist== 'geom1'",
          verbatimTextOutput("var_geom1")),
        
        # NB0
        
        conditionalPanel(
          condition = "input.discrete_dist== 'negbinom0'",
          verbatimTextOutput("var_nb0")),
        
        # Discrete Uniform
        
        conditionalPanel(
          condition = "input.discrete_dist== 'dunif'",
          verbatimTextOutput("var_dunif"))
        
      ),
      
      conditionalPanel(
        condition = "input.type_of_distribution == 'cont'",
        
        # Normal
        
        conditionalPanel(
          condition = "input.contin_distributions== 'normal'",
          verbatimTextOutput("var_norm")),
        
        # Continuous uniform
        
        conditionalPanel(
          condition = "input.contin_distributions== 'uniform'",
          verbatimTextOutput("var_unif")),
        
        # Exponential
        
        conditionalPanel(
          condition = "input.contin_distributions== 'exp'",
          verbatimTextOutput("var_exp"))
        
        
      )
    ),
    box(
      title = "Median",
      width = 3,
      status = "primary",
      
      conditionalPanel(
        condition = "input.type_of_distribution == 'dis'",
        
        # Bernoulli 
        conditionalPanel(
          condition = "input.discrete_dist== 'bernoulli'",
          verbatimTextOutput("median_bern")),
        
        # Binomial
        
        conditionalPanel(
          condition = "input.discrete_dist== 'binom'",
          verbatimTextOutput("median_binom")),
        
        # Poisson
        
        conditionalPanel(
          condition = "input.discrete_dist== 'poisson'",
          verbatimTextOutput("median_poisson")),
        
        # Geom0
        
        conditionalPanel(
          condition = "input.discrete_dist== 'geom0'",
          verbatimTextOutput("median_geom0")),
        
        # Geom1
        
        conditionalPanel(
          condition = "input.discrete_dist== 'geom1'",
          verbatimTextOutput("median_geom1")),
        
        # NB0
        
        conditionalPanel(
          condition = "input.discrete_dist== 'negbinom0'",
          verbatimTextOutput("median_nb0")),
        
        
        # Discrete Uniform
        
        conditionalPanel(
          condition = "input.discrete_dist== 'dunif'",
          verbatimTextOutput("median_dunif"))
        
      ),
      
      conditionalPanel(
        condition = "input.type_of_distribution == 'cont'",
        
        # Normal
        
        conditionalPanel(
          condition = "input.contin_distributions== 'normal'",
          verbatimTextOutput("median_norm")),
        
        # Continuous uniform
        
        conditionalPanel(
          condition = "input.contin_distributions== 'uniform'",
          verbatimTextOutput("median_unif")),
        
        # Exponential
        
        conditionalPanel(
          condition = "input.contin_distributions== 'exp'",
          verbatimTextOutput("median_exp"))
        
        
      )
    ),
    box(
      title = "Mode",
      width = 3,
      status = "primary",
      
      conditionalPanel(
        condition = "input.type_of_distribution == 'dis'",
        
        # Bernoulli 
        conditionalPanel(
          condition = "input.discrete_dist== 'bernoulli'",
          verbatimTextOutput("mode_bern")),
        
        # Binomial
        
        conditionalPanel(
          condition = "input.discrete_dist== 'binom'",
          verbatimTextOutput("mode_binom")),
        
        # Poisson
        
        conditionalPanel(
          condition = "input.discrete_dist== 'poisson'",
          verbatimTextOutput("mode_poisson")),
        
        # Geom0
        
        conditionalPanel(
          condition = "input.discrete_dist== 'geom0'",
          verbatimTextOutput("mode_geom0")),
        
        # Geom1
        
        conditionalPanel(
          condition = "input.discrete_dist== 'geom1'",
          verbatimTextOutput("mode_geom1")),
        
        # NB0
        
        conditionalPanel(
          condition = "input.discrete_dist== 'negbinom0'",
          verbatimTextOutput("mode_nb0")),
      
        
        # Discrete Uniform
        
        conditionalPanel(
          condition = "input.discrete_dist== 'dunif'",
          verbatimTextOutput("mode_dunif"))
        
      ),
      
      conditionalPanel(
        condition = "input.type_of_distribution == 'cont'",
        
        # Normal
        
        conditionalPanel(
          condition = "input.contin_distributions== 'normal'",
          verbatimTextOutput("mode_norm")),
        
        # Continuous uniform
        
        conditionalPanel(
          condition = "input.contin_distributions== 'uniform'",
          verbatimTextOutput("mode_unif")),
        
        # Exponential
        
        conditionalPanel(
          condition = "input.contin_distributions== 'exp'",
          verbatimTextOutput("mode_exp"))
        
        
      )
    )
  ),
  
  fluidRow(
    box(
      title = "Skewness",
      width = 3,
      status = "primary",
      
      conditionalPanel(
        condition = "input.type_of_distribution == 'dis'",
        
        # Bernoulli 
        conditionalPanel(
          condition = "input.discrete_dist== 'bernoulli'",
          verbatimTextOutput("skew_bern")),
        
        # Binomial
        
        conditionalPanel(
          condition = "input.discrete_dist== 'binom'",
          verbatimTextOutput("skew_binom")),
        
        # Poisson
        
        conditionalPanel(
          condition = "input.discrete_dist== 'poisson'",
          verbatimTextOutput("skew_poisson")),
        
        # Geom0
        
        conditionalPanel(
          condition = "input.discrete_dist== 'geom0'",
          verbatimTextOutput("skew_geom0")),
        
        # Geom1
        
        conditionalPanel(
          condition = "input.discrete_dist== 'geom1'",
          verbatimTextOutput("skew_geom1")),
        
        # NB0
        
        conditionalPanel(
          condition = "input.discrete_dist== 'negbinom0'",
          verbatimTextOutput("skew_nb0")),
    
        
        # Discrete Uniform
        
        conditionalPanel(
          condition = "input.discrete_dist== 'dunif'",
          verbatimTextOutput("skew_dunif"))
        
      ),
      
      conditionalPanel(
        condition = "input.type_of_distribution == 'cont'",
        
        # Normal
        
        conditionalPanel(
          condition = "input.contin_distributions== 'normal'",
          verbatimTextOutput("skew_norm")),
        
        # Continuous uniform
        
        conditionalPanel(
          condition = "input.contin_distributions== 'uniform'",
          verbatimTextOutput("skew_unif")),
        
        # Exponential
        
        conditionalPanel(
          condition = "input.contin_distributions== 'exp'",
          verbatimTextOutput("skew_exp"))
        
        
      )
    ),
    box(
      title = "Kurtosis",
      width = 3,
      status = "primary",
      
      conditionalPanel(
        condition = "input.type_of_distribution == 'dis'",
        
        # Bernoulli 
        conditionalPanel(
          condition = "input.discrete_dist== 'bernoulli'",
          verbatimTextOutput("kurtosis_bern")),
        
        # Binomial
        
        conditionalPanel(
          condition = "input.discrete_dist== 'binom'",
          verbatimTextOutput("kurtosis_binom")),
        
        # Poisson
        
        conditionalPanel(
          condition = "input.discrete_dist== 'poisson'",
          verbatimTextOutput("kurtosis_poisson")),
        
        # Geom0
        
        conditionalPanel(
          condition = "input.discrete_dist== 'geom0'",
          verbatimTextOutput("kurtosis_geom0")),
        
        # Geom1
        
        conditionalPanel(
          condition = "input.discrete_dist== 'geom1'",
          verbatimTextOutput("kurtosis_geom1")),
        
        # NB0
        
        conditionalPanel(
          condition = "input.discrete_dist== 'negbinom0'",
          verbatimTextOutput("kurtosis_nb0")),
      
        
        # Discrete Uniform
        
        conditionalPanel(
          condition = "input.discrete_dist== 'dunif'",
          verbatimTextOutput("kurtosis_dunif"))
        
      ),
      
      conditionalPanel(
        condition = "input.type_of_distribution == 'cont'",
        
        # Normal
        
        conditionalPanel(
          condition = "input.contin_distributions== 'normal'",
          verbatimTextOutput("kurtosis_norm")),
        
        # Continuous uniform
        
        conditionalPanel(
          condition = "input.contin_distributions== 'uniform'",
          verbatimTextOutput("kurtosis_unif")),
        
        # Exponential
        
        conditionalPanel(
          condition = "input.contin_distributions== 'exp'",
          verbatimTextOutput("kurtosis_exp"))
        
        
      )
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
      status = "primary"
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

  #output$mean <- renderText({ bern_mean(input$bernoulli_p) })
  
  output$mean_bern <- renderText({ bern(input$bernoulli_p)$mean })
  output$mean_binom <- renderText({ binom(input$n_binom, input$binom_p)$mean })
  output$mean_poisson <- renderText(({ poisson(input$lambda_poisson)$mean }))
  output$mean_geom0 <- renderText(({ geometric0(input$p0_geom)$mean }))
  output$mean_geom1 <- renderText(({ geometric1(input$p1_geom)$mean }))
  output$mean_nb0 <- renderText(({ nb0(input$r0, input$p0)$mean }))
  output$mean_dunif <- renderText(({ duniform(input$a_dunif, input$b_dunif)$mean }))
  
  
  output$mean_norm <- renderText({ normaldist(input$mean_normal, input$var_normal)$mean })
  output$mean_unif <- renderText({ unifdist(input$a_unif, input$b_unif)$mean })
  output$mean_exp <- renderText({ expdist(input$lambda_exp)$mean })
  
  
  
  
  # Variance
  
  output$var_bern <- renderText({ bern(input$bernoulli_p)$var })
  output$var_binom <- renderText({ binom(input$n_binom, input$binom_p)$var })
  output$var_poisson <- renderText(({ poisson(input$lambda_poisson)$var }))
  output$var_geom0 <- renderText(({ geometric0(input$p0_geom)$var }))
  output$var_geom1 <- renderText(({ geometric1(input$p1_geom)$var }))
  output$var_nb0 <- renderText(({ nb0(input$r0, input$p0)$var }))
  output$var_dunif <- renderText(({ duniform(input$a_dunif, input$b_dunif)$var }))
  
  
  output$var_norm <- renderText({ normaldist(input$mean_normal, input$var_normal)$var })
  output$var_unif <- renderText({ unifdist(input$a_unif, input$b_unif)$var })
  output$var_exp <- renderText({ expdist(input$lambda_exp)$var })
  
  # Median
  
  output$median_bern <- renderText({ bern(input$bernoulli_p)$median })
  output$median_binom <- renderText({ binom(input$n_binom, input$binom_p)$median })
  output$median_poisson <- renderText(({ poisson(input$lambda_poisson)$median }))
  output$median_geom0 <- renderText(({ geometric0(input$p0_geom)$median }))
  output$median_geom1 <- renderText(({ geometric1(input$p1_geom)$median }))
  output$median_nb0 <- renderText(({ nb0(input$r0, input$p0)$median }))
  output$median_dunif <- renderText(({ duniform(input$a_dunif, input$b_dunif)$median }))
  
  
  output$median_norm <- renderText({ normaldist(input$mean_normal, input$var_normal)$median })
  output$median_unif <- renderText({ unifdist(input$a_unif, input$b_unif)$median })
  output$median_exp <- renderText({ expdist(input$lambda_exp)$median })
  
  # Mode
  
  output$mode_bern <- renderText({ bern(input$bernoulli_p)$mode })
  output$mode_binom <- renderText({ binom(input$n_binom, input$binom_p)$mode })
  output$mode_poisson <- renderText(({ poisson(input$lambda_poisson)$mode }))
  output$mode_geom0 <- renderText(({ geometric0(input$p0_geom)$mode }))
  output$mode_geom1 <- renderText(({ geometric1(input$p1_geom)$mode }))
  output$mode_nb0 <- renderText(({ nb0(input$r0, input$p0)$mode }))
  output$mode_dunif <- renderText(({ duniform(input$a_dunif, input$b_dunif)$mode }))
  
  
  output$mode_norm <- renderText({ normaldist(input$mean_normal, input$var_normal)$mode })
  output$mode_unif <- renderText({ unifdist(input$a_unif, input$b_unif)$mode })
  output$mode_exp <- renderText({ expdist(input$lambda_exp)$mode })
  
  # Skewness
  
  output$skew_bern <- renderText({ bern(input$bernoulli_p)$skew })
  output$skew_binom <- renderText({ binom(input$n_binom, input$binom_p)$skew })
  output$skew_poisson <- renderText(({ poisson(input$lambda_poisson)$skew }))
  output$skew_geom0 <- renderText(({ geometric0(input$p0_geom)$skew }))
  output$skew_geom1 <- renderText(({ geometric1(input$p1_geom)$skew }))
  output$skew_nb0 <- renderText(({ nb0(input$r0, input$p0)$skew }))
  output$skew_dunif <- renderText(({ duniform(input$a_dunif, input$b_dunif)$skew}))
  
  
  output$skew_norm <- renderText({ normaldist(input$mean_normal, input$var_normal)$skew })
  output$skew_unif <- renderText({ unifdist(input$a_unif, input$b_unif)$skew })
  output$skew_exp <- renderText({ expdist(input$lambda_exp)$skew })
  
  # Kurtosis
  
  output$kurtosis_bern <- renderText({ bern(input$bernoulli_p)$kurtosis })
  output$kurtosis_binom <- renderText({ binom(input$n_binom, input$binom_p)$kurtosis })
  output$kurtosis_poisson <- renderText(({ poisson(input$lambda_poisson)$kurtosis }))
  output$kurtosis_geom0 <- renderText(({ geometric0(input$p0_geom)$kurtosis }))
  output$kurtosis_geom1 <- renderText(({ geometric1(input$p1_geom)$kurtosis }))
  output$kurtosis_nb0 <- renderText(({ nb0(input$r0, input$p0)$kurtosis }))
  output$kurtosis_dunif <- renderText(({ duniform(input$a_dunif, input$b_dunif)$kurtosis }))
  
  
  output$kurtosis_norm <- renderText({ normaldist(input$mean_normal, input$var_normal)$kurtosis })
  output$kurtosis_unif <- renderText({ unifdist(input$a_unif, input$b_unif)$kurtosis })
  output$kurtosis_exp <- renderText({ expdist(input$lambda_exp)$kurtosis })
  
  # Entropy
  

}

shinyApp(ui = ui, server = server)