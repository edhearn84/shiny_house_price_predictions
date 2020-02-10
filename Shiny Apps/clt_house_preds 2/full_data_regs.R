
rm(list = ls())

library(tidyverse)
library(broom)

load("~/Desktop/Shiny Apps/clt_house_preds/full_clt.Rdata")

# Data munge
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

# Standard hedonic model
clt_reg = lm( lrsp ~ age + age_sq + hla + hla_sq + uhla + uhla_sq + 
                beds + baths + acres + ext_fct + h_area , data = clt_lrsp )

# Model results and fit diagnostics
View(tidy(clt_reg))
glance(clt_reg)

# Incorporating tax assesor's valuation
table(full_clt_df$tax_value == 0)

# Munge data with tax value of house
clt_lrsp_tax = full_clt_df %>%
  filter( tax_value > 0 ) %>%
  mutate( age = 2010 - year_built ,
          age_sq = age^2 ,
          lrsp = log(rsp) ,
          hla = total_hla ,
          hla_sq = (hla)^2 ,
          h_area = as.factor( substring(harea , 6) ), 
          lrtax_val = log(tax_value / `2010$ Rate`)) %>%
  filter( age >= 0 ,
          lrsp > 1 )

# Hedonic model - with tax-assessed value
clt_reg_tax = lm( lrsp ~ age + age_sq + hla + hla_sq + acres + lrtax_val + h_area , data = clt_lrsp_tax )

# Model results and fit diagnostics
tidy(clt_reg_tax)
glance(clt_reg_tax)

#############################################################################

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

# Quantiles of predictive house-price distribution
quantile( exp(lrsp_preds) , probs = c(.025 , .05 , .25 , .5 , .75 , .95 , .975) )

#############################################################################

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
  
  quantile( y_pred , probs = c(.025 , .05 , .25 , .5 , .75 , .95 , .975) )
}

new_house = tibble(rsp = 100000 , age = 35 , hla = 4000 , beds = 2 , baths = 2 , uhla = 600 ,
                   acres = 0.5 , harea = "01-01" , exterior = "Vinyl")

preds(new_house)
exp(preds(new_house))

# Full data model 
clt_housing_full = read_csv("Desktop/Shiny Apps/clt_house_preds/clt_housing_full.csv")

clt_housing_df = clt_housing_full %>%
  mutate( ext = ifelse(exterior %in% c("Brick Veneer Full" , "Vinyl" , "Hardboard Siding" , "Wood") ,
                       exterior, "other") ,
          ext_fct = relevel( as.factor(ext) , ref = "other") ,
          age = 2010 - year_built ,
          age_sq = age^2 ,
          lrsp = log(sp) ,
          h_area = substring(harea , 6), 
          hla = total_hla ,
          uhla = unheated_sqft ,
          hla_sq = (hla)^2 ,
          uhla_sq = (uhla)^2 ) %>%
  filter( age >= 0 ,
          lrsp > 1 )

clt_upd = clt_housing_df %>%
  filter( h_area %in% levels(clt_lrsp$h_area) )

# Standard hedonic model - full data
clt_reg = lm( lrsp ~ age + age_sq + hla + hla_sq + uhla + uhla_sq + 
                beds + baths + acres + ext_fct + h_area , data = clt_upd )

# Model results and fit diagnostics
View(tidy(clt_reg))
glance(clt_reg)

