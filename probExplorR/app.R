# Load packages and source files
libs = c("shiny", "shinythemes")
lapply(libs, require, character.only = T)

source("./utils.R")
source("./viewDistr.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("sandstone"),
    
    h2("Probability Distribution Viewer"),
    
    tabsetPanel(
        # Discrete Panel ####
        tabPanel("Discrete", navlistPanel(
            #####
            tabPanel(title = "Binomial", 
                     h3("Parameters"),
                     fluidRow(
                         column(6,
                                h4("Number of Trials:"), 
                                numericInput("binom_n", label = NULL, 
                                             min = 1, step = 1, value = 1))
                         ,
                         column(6,
                                h4("Probability of Success:"),
                                sliderInput01(id = "binom_p", label = NULL))
                     ) # End row
                     ,
                     h3("Inputs"),
                     fluidRow(
                         column(6,
                                h4("Minimum Trials:"), 
                                numericInput("binom_min", label = NULL, value = 1, 
                                             min = 1, step = 1)), 
                         column(6,
                                h4("Maximum Trials:"),
                                numericInput("binom_max", label = NULL, value = 10, 
                                             step = 1, min = 3))
                     ) # End row
                     ,
                     plotOutput("plot_binom")), # End of inner tabPanel
            #####
            tabPanel(title = "Bernoulli", 
                     h3("Parameters")
                     ,
                     fluidRow(
                         column(12, 
                                h4("Probability of Success:"),
                                sliderInput("bern_p", label = NULL, 
                                            min = 0, max = 1, step = 0.01, value = 0.5))
                     ) # End row
                     ,
                     plotOutput("plot_bern")), # End of inner tabPanel
            #####
            tabPanel(title = "Poisson",
                     h3("Parameters")
                     ,
                     fluidRow(
                         column(12, 
                                h4("Rate At Which Events Occur:"),
                                numericInput("pois_lambda", label = NULL, 
                                             min = 0, step = 1, value = 1))
                     ) # End row
                     ,
                     h3("Inputs"),
                     fluidRow(
                         column(6,
                                h4("Minimum Count:"), 
                                numericInput("pois_min", label = NULL, value = 0, 
                                             min = 0, step = 1))
                         , 
                         column(6,
                                h4("Maximum Count:"),
                                numericInput("pois_max", label = NULL, value = 10, 
                                             step = 1, min = 3))
                     ) # End row
                     ,
                     plotOutput("plot_pois")), # End of inner tabPanel
            #####
            tabPanel(title = "Negative Binomial", 
                     h3("Parameters")
                     ,
                     fluidRow(
                         column(6,
                                h4("Number of Successes Required:"), 
                                numericInput("nbinom_r", label = NULL, 
                                             min = 1, step = 1, value = 1))
                         ,
                         column(6,
                                h4("Probability of Success:"),
                                sliderInput01(id = "nbinom_p", label = NULL))
                     ) # End row
                     ,
                     h3("Inputs"),
                     fluidRow(
                         column(6,
                                h4("Minimum Trials Until rth Success:"), 
                                numericInput("nbinom_min", label = NULL, value = 1, 
                                             min = 1, step = 1))
                         , 
                         column(6,
                                h4("Maximum Trials Until rth Success:"),
                                numericInput("nbinom_max", label = NULL, value = 10, 
                                             step = 1, min = 3))
                     ) # End row
                     ,
                     plotOutput("plot_nbinom")), # End of inner tabPanel
            #####
            tabPanel(title = "Geometric", 
                     h3("Parameters")
                     ,
                     fluidRow(
                         column(6,
                                h4("Probability of Success:"),
                                sliderInput("geom_p", label = NULL, 
                                            min = 0, max = 1, step = 0.01, value = 0.5))
                     ) # End row
                     ,
                     h3("Inputs"),
                     fluidRow(
                         column(6,
                                h4("Minimum Trials Until 1st Success:"), 
                                numericInput("geom_min", label = NULL, value = 1, 
                                             min = 1, step = 1))
                         , 
                         column(6,
                                h4("Maximum Trials Until 1st Success:"),
                                numericInput("geom_max", label = NULL, value = 10, 
                                             step = 1, min = 3))
                     ) # End row
                     ,
                     plotOutput("plot_geom")), # End of inner tabPanel
            #####
            tabPanel(title = "Hyper Geometric", 
                     h3("Parameters")
                     ,
                     fluidRow(
                         column(6,
                                h4("Population of Type 1's:"), 
                                numericInput("hyper_type1", label = NULL, value = 5, 
                                             min = 1, step = 1))
                         , 
                         column(6,
                                h4("Population of Type 2's"),
                                numericInput("hyper_type2", label = NULL, value = 5, 
                                             step = 1, min = 1))
                     ) # End row
                     ,  
                     h3("Inputs"),
                     fluidRow(
                         column(6,
                                h4("Number of Type 1's Drawn:"), 
                                numericInput("hyper_draws", label = NULL, 
                                             min = 1, step = 1, value = 1))
                         ,
                         column(6,
                                h4("Number of Trials"),
                                numericInput("hyper_trials", label = NULL, 
                                             min = 1, step = 1, value = 1))
                     ) # End row
                     ,
                     plotOutput("plot_hyper"))# End of inner tabPanel
            #####
        ) # End of navlistPanel
        ) # End of outer tabPanel
        ,
        # Continuous Panel ####
        tabPanel("Continuous", navlistPanel(
            #####
            tabPanel(title = "Uniform", 
                     h3("Parameters")
                     ,
                     fluidRow(
                         column(6,
                                h4("Minimum Value (a):"), 
                                numericInput("unif_a", label = NULL, value = 6, 
                                             step = 0.5)), 
                         column(6,
                                h4("Maximum Value (b):"),
                                numericInput("unif_b", label = NULL, value = 9, 
                                             step = 0.5))
                     ) # End row
                     ,               
                     h3("Inputs"),
                     fluidRow(
                         column(6,
                                h4("Minimum Outcome:"), 
                                numericInput("unif_min", label = NULL, step = 0.5, 
                                             value = 1))
                         ,
                         column(6,
                                h4("Maximum Outcome:"),
                                numericInput("unif_max", label = NULL, step = 0.5,
                                             value = 15))
                     ) # End row
                     , 
                     plotOutput("plot_unif")), # End of inner tabPanel
            #####
            tabPanel(title = "Normal", 
                     h3("Parameters")
                     ,                
                     fluidRow(
                         column(6,
                                h4("Mean Outcome:"), 
                                numericInput("norm_mu", label = NULL, value = 5, 
                                             step = 0.1)), 
                         column(6,
                                h4("Standard Deviation:"),
                                numericInput("norm_sigma", label = NULL, value = 2.5, 
                                             step = 0.1))
                     ) # End row
                     ,                 
                     h3("Inputs"),
                     fluidRow(
                         column(6,
                                h4("Minimum Outcome:"), 
                                numericInput("norm_min", label = NULL, step = 0.5, 
                                             value = 1))
                         ,
                         column(6,
                                h4("Maximum Outcome:"),
                                numericInput("norm_max", label = NULL, step = 0.5,
                                             value = 15))
                     ) # End row
                     , 
                     plotOutput("plot_norm")), # End of inner tabPanel
            #####
            tabPanel(title = "Exponential", 
                     h3("Parameters")
                     ,
                     fluidRow(
                         column(6,
                                h4("Rate At Which Events Occur:"),
                                numericInput("exp_rate", label = NULL, value = 2.5, 
                                             step = 0.1))
                     ) # End row
                     ,
                     h3("Inputs"),
                     fluidRow(
                         column(6,
                                h4("Minimum Outcome:"), 
                                numericInput("exp_min", label = NULL, step = 0.5, 
                                             value = 1))
                         ,
                         column(6,
                                h4("Maximum Outcome:"),
                                numericInput("exp_max", label = NULL, step = 0.5,
                                             value = 15))
                     ) # End row
                     , 
                     plotOutput("plot_exp")), # End of inner tabPanel
            #####
            tabPanel(title = "Gamma", 
                     h3("Parameters"),
                     fluidRow(
                         column(6,
                                h4("Alpha:"), 
                                numericInput("gamma_alpha", label = NULL, value = 5, 
                                             step = 0.1)), 
                         column(6,
                                h4("Beta:"),
                                numericInput("gamma_beta", label = NULL, value = 2.5, 
                                             step = 0.1))
                     ) # End row
                     ,   
                     h3("Inputs"),
                     fluidRow(
                         column(6,
                                h4("Minimum Outcome:"), 
                                numericInput("gamma_min", label = NULL, step = 0.5, 
                                             value = 1))
                         ,
                         column(6,
                                h4("Maximum Outcome:"),
                                numericInput("gamma_max", label = NULL, step = 0.5,
                                             value = 15))
                     ) # End row
                     , 
                     plotOutput("plot_gamma")), # End of inner tabPanel
            #####
            tabPanel(title = "Beta", 
                     h3("Parameters")
                     ,
                     fluidRow(
                         column(6,
                                h4("Alpha:"), 
                                numericInput("beta_alpha", label = NULL, value = 1, 
                                             step = 0.1)), 
                         column(6,
                                h4("Beta:"),
                                numericInput("beta_beta", label = NULL, value = 1, 
                                             step = 0.1))
                     ) # End row
                     ,
                     plotOutput("plot_beta")), # End of inner tabPanel
            #####
            tabPanel(title = "Von Mises", 
                     h3("Parameters")
                     , 
                     fluidRow(
                         column(6,
                                h4("Mean Outcome:"), 
                                numericInput("vonmises_mu", label = NULL, value = 0, 
                                             step = 0.1)), 
                         column(6,
                                h4("Concentration:"),
                                numericInput("vonmises_kappa", label = NULL, value = 1, 
                                             step = 0.1))
                     ) # End row
                     ,
                     plotOutput("plot_vonmises")) # End of inner tabPanel
            #####
        ) # end of navlistPanel
        ) # End of outer tabPanel
    ) # End of tabsetPanel
) # End of UI

# Define server logic required to draw a histogram
server <- function(input, output) {
    # Discrete Plots
    output$plot_binom <- renderPlot({
        x = seq(input$binom_min, input$binom_max, by = 1)
        pbinomd(x = x, n = input$binom_n, p = input$binom_p)
    })
    output$plot_bern <- renderPlot({
        pbernd(p = input$bern_p)
    })
    output$plot_pois <- renderPlot({
        x = seq(input$pois_min, input$pois_max, by = 1)
        ppoisd(x = x, lambda = input$pois_lambda)
    })
    output$plot_nbinom <- renderPlot({
        x = seq(input$nbinom_min, input$nbinom_max, by = 1)
        pnbinomd(x = x, r = input$nbinom_r, p = input$nbinom_p)
    })
    output$plot_geom <- renderPlot({
        x = seq(input$geom_min, input$geom_max, by = 1)
        pgeomd(x = x, p = input$geom_p)
    })
    output$plot_hyper <- renderPlot({
        phyperd(draws = input$hyper_draws, type1 = input$hyper_type1, 
                type2 = input$hyper_type2, trials = input$hyper_trials)
    })
    # Continuous Plots
    output$plot_unif <- renderPlot({
        x = seq(input$unif_min, input$unif_max, by = 0.01)
        punifd(x = x, a = input$unif_a, b = input$unif_b)
    })
    output$plot_norm <- renderPlot({
        x = seq(input$norm_min, input$norm_max, by = 0.01)
        pnormd(x = x, mu = input$norm_mu, sigma = input$norm_sigma)
    })
    output$plot_exp <- renderPlot({
        x = seq(input$exp_min, input$exp_max, by = 0.01)
        pexpd(x = x, rate = input$exp_rate)
    })
    output$plot_gamma <- renderPlot({
        x = seq(input$gamma_min, input$gamma_max, by = 0.01)
        pgammad(x = x, alpha = input$gamma_alpha, beta = input$gamma_beta)
    })
    output$plot_beta <- renderPlot({
        pbetad(alpha = input$beta_alpha, beta = input$beta_beta)
    })
    output$plot_vonmises <- renderPlot({
        pvonmisesd(mu = input$vonmises_mu, kappa = input$vonmises_kappa)
    })
} # end of server function

# Run the application 
shinyApp(ui = ui, server = server)
# shiny::reactlogShow()