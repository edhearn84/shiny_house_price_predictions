
rm(list = ls())

options(scipen = 10)

#setwd("~/Desktop/Shiny Apps/clt_house_preds")

library(tidyverse)
library(broom)

##########################################################################
# Hedonic model estimation and data set-up
##########################################################################

load("full_clt.Rdata")

# Munge data with tax value of house
clt_lrsp = full_clt_df %>%
  mutate( ext = ifelse(exterior %in% c("Brick Veneer Full" , "Vinyl" , "Hardboard Siding" , "Wood") ,
                       exterior, "other") ,
          ext_fct = relevel( as.factor(ext) , ref = "other") ,
          age = 2010 - year_built ,
          age_sq = age^2 ,
          lrsp = log(rsp) ,
          h_area = as.factor( substring(harea , 6) ), 
          hla = total_hla ,
          uhla = unheated_sqft ,
          hla_sq = (hla)^2 ,
          uhla_sq = (uhla)^2 ) %>%
  filter( age >= 0 ,
          lrsp > 1 )

# Hedonic model 
clt_reg = lm( lrsp ~ age + age_sq + hla + hla_sq + uhla + uhla_sq + 
                beds + baths + acres + ext_fct + h_area , data = clt_lrsp )

##########################################################################
# Predictive distribution - Charlotte house prices given attributes
##########################################################################

# Set up model sigma and fitted (yhat) values for predictive draws
s_sq = (glance(clt_reg)$sigma)^2
fits = augment(clt_reg)$.fitted

# Two-step composition sampling process to predict house prices given data (Carlin and Louis p.452)
# Set up model sigma and fitted (yhat) values for predictive draws
s_sq = (glance(clt_reg)$sigma)^2
fits = augment(clt_reg)$.fitted

# Two-step composition sampling process to predict house prices given data (Carlin and Louis p.452)

# Step 1: Draw values of sigma_squared from inverse gamma distribution
sigma_pred = 1 / rgamma( length(fits) , 
                         0.5*(length(fits) - clt_reg$rank) ,
                         0.5*(length(fits) - clt_reg$rank)*s_sq )

# Step 2: Using sqrt(sigma_pred) and fitted model values to draw log(house prices) from normal distribution
lrsp_preds = rnorm( length(fits) , fits , sqrt(sigma_pred) )

# Functionalizes data-input parameters into quantiles output
preds = function(new_x) {
  data_x = new_x %>%
    mutate( lrsp = log(rsp) ,
            age = age ,
            age_sq = age^2 ,
            hla = hla ,
            hla_sq = hla^2 ,
            uhla = uhla ,
            uhla_sq = uhla^2 ,
            beds = beds ,
            baths = baths ,
            acres = acres ,
            exterior = exterior ,
            harea = harea ) %>%
    select( lrsp , age , age_sq , hla , hla_sq , uhla , uhla_sq , beds , baths , acres , exterior , harea )
  
  betas = MASS::mvrnorm( length(sigma_pred) , clt_reg$coefficients , vcov(clt_reg) )
  betas_lvl = cbind( betas[,1:10] , 0 , betas[,11:14] , 0 , betas[,15:65] )
  mod_mat = t( as.matrix( cbind( 1 , data_x[,2:10] , 1 , 1) ) )
  beta_mat = as.matrix( cbind(betas[,1:10] , 
                              betas[ , 10 + match(data_x$exterior , levels(clt_lrsp$ext_fct)) ]) )
  beta_mat2 = as.matrix( cbind(beta_mat , betas[ , 15 + match(data_x$harea , levels(clt_lrsp$h_area)) ]) )
  mu_pred = beta_mat2 %*% mod_mat
  y_pred = rnorm( length(sigma_pred) , mu_pred , sqrt(sigma_pred) )
  
  return( list(quantile( y_pred , probs = c(.025 , .05 , .25 , .5 , .75 , .95 , .975) ) , y_pred ) )
}

new_house = tibble(rsp = 100000 , age = 35 , hla = 4000 , beds = 2 , baths = 2 , uhla = 600 ,
                   acres = 0.5 , harea = "01-01" , exterior = "Vinyl")

pr = preds(new_house)
pr[[1]]

##########################################################################
# The following is a Shiny web application.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
##########################################################################

library(shiny)

fields = c("rsp" , "age" , "hla" , "beds" , "baths" , "uhla" , "acres" , "exterior" , "harea")

# Define UI for application that draws a histogram
ui = fluidPage(
   
   # Application title
   titlePanel("House Attributes"),
   
   # Number input form 
   sidebarLayout(
      sidebarPanel(
         textInput("rsp" , "Prospective Sale Price") ,
         textInput("age" , "Age of House in Years") ,
         textInput("hla" , "Total Heated Living Area") ,
         textInput("beds" , "Beds") ,
         textInput("baths" , "Baths") ,
         textInput("uhla" , "Unheated Living Area") ,
         textInput("acres" , "Acres of House Plot") ,
         selectInput("exterior", "Exterior",
                     c("Other" = "other" ,
                       "Brick Veneer Full" = "Brick Veneer Full" , 
                       "Vinyl" = "Vinyl", 
                       "Hardboard Siding" = "Hardboard Siding" , 
                       "Wood" = "Wood") ) ,
         selectInput("harea" , "House Area Quadrant" , levels(clt_lrsp$h_area)) ,
         actionButton("add" , "Compute Predictive Quantiles")
      ) ,
      
      # Show a plot of the generated distribution
      mainPanel(
        
      tableOutput("quants") ,
      plotOutput("qplot")
      )
  )
)

# Define server logic required to draw a histogram
server = function(input , output , session) {
  
# Observe the add click and perform a reactive expression
  observeEvent( input$add , {
    rsp = as.numeric(input$rsp)
    age = as.numeric(input$age)
    hla = as.numeric(input$hla)
    beds = as.numeric(input$beds)
    baths = as.numeric(input$baths)
    uhla = as.numeric(input$uhla)
    acres = as.numeric(input$acres)
    exterior = as.character(input$exterior)
    harea = as.character(input$harea)

    # Reactive expression
    house_attributes = tibble(rsp = rsp , age = age , hla = hla , beds = beds , baths = baths , uhla = uhla , 
                              acres = acres , exterior = exterior , harea = harea)
    q_pred = preds(house_attributes)
    names(q_pred[[1]]) = c( "2.5%" , "5%" , "15%" , "50%" , "85%" , "95%" , "97.5%" )
    
    # Tabular output
    output$quants = renderTable({
      t(as.matrix(exp(q_pred[[1]])))
    })
    
    output$qplot = renderPlot({
      plot(density(exp(q_pred[[2]])) , 
           main = "Predictive House Price Distribution\n(Red line is prospective sale price)" , 
           ylab = "" , 
           yaxt = "n" ,
           xlab = "House Sale Price (2010$)")
      abline(v = input$rsp , col = "red")
    })
  } )
}

# Run the application 
shinyApp(ui = ui , server = server)

