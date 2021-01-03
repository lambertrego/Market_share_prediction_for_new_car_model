# mlogit ------------------------------------------------------------------
### Clear memory
rm(list = ls())

# Apollo 0.1.0
# www.ApolloChoiceModelling.com

library("apollo")
library(stargazer)
library(ggplot2)
library(reshape2)

library(mlogit)
library(readr)
library(xtable)
library(tidyr)
library(data.table)
library(tidyverse)
library(dplyr)
## Load dataset from mlogit package
data("Car", package = "mlogit")

## display the column names from the dataset 
names(Car)

Car$chosen <- 0
Car$chosen[Car$choice == "choice1"] <- 1
Car$chosen[Car$choice == "choice2"] <- 2
Car$chosen[Car$choice == "choice3"] <- 3
Car$chosen[Car$choice == "choice4"] <- 4
Car$chosen[Car$choice == "choice5"] <- 5
Car$chosen[Car$choice == "choice6"] <- 6

summary(Car)

c <- mlogit.data(Car, shape="wide",choice="chosen",varying=c(5:70), sep="")

# Initialize new fuel type to constant
c$newfuel <- 0
c$newfuel[c$fuel == "electric" | c$fuel == "gasoline" | c$fuel == "methanol"] <- 1


# Apollo Automobile ------------------------------------------------------------------

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName ="car_purchase_mnl",
  modelDescr ="Standard logit model on Train's car purchase data",
  indivID   ="ID",  
  #mixing    = TRUE,
  nCores    = 7
)


# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #
data("Car")
Car$ID      = 1:nrow(Car)
Car$Choice  = as.numeric(substring(Car$choice, 7))
database = Car


# ################################################################# #
#### ANALYSIS OF CHOICES                                         ####
# ################################################################# #

choiceAnalysis_settings <- list(
  alternatives = c(car1=1, car2=2,car3=3,car4=4,car5=5,car6=6),
  avail        = 1,
  explanators  = database[,c("college","hsg2","coml5")],
  choiceVar    = database$Choice,
  rows = "all"
  #rows         = database$income>30000
)

apollo_choiceAnalysis(choiceAnalysis_settings, apollo_control, database)

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(#priceLogInc    = -0.2447,
               # mu_range       =  0.0048,
               # sigma_range        = 0.0042,
               # mu_size = 1.7396,
               # sigma_size =  10.5604,
               # sigma_ev = -2.5431,
                cost = -0.0552
                # sigma_cng = 0,
                #meth           = 0,
                #cng            = 0,
                #mu_b_ev        = 0,
                #sigma_large     =0
               # truck          =  -1.3051,
                #suv             = 0.8492,
               # sportcar       = 0.6362,
               # van            = -1.1006
)



### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
#apollo_fixed = c("sigma_large")
apollo_fixed = c()


# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

### Set parameters for generating draws
# apollo_draws = list(
#   interDrawsType = "halton",
#   interNDraws    = 0,
#   interUnifDraws = c(),
#   interNormDraws = c(),
#   intraDrawsType = "halton",
#   intraNDraws    = 1000,
#   intraUnifDraws = c(),
#  # intraNormDraws = c("draws_size", "draws_range", "draws_ev", "draws_large")
#   intraNormDraws = c( "draws_cost")
# )

### Create random parameters
# apollo_randCoeff = function(apollo_beta, apollo_inputs){
#   randcoeff = list()
# 
#  # randcoeff[["size"]]   =  mu_size  + sigma_size   * draws_size
#  # randcoeff[["range"]]  =  mu_range + sigma_range  * draws_range
#  # randcoeff[["ev"]]     =             sigma_ev     * draws_ev
#  # randcoeff[["large"]]  =             sigma_large  * draws_large
#   randcoeff[["cost"]] = 
#   return(randcoeff)
# }

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #


apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Function initialisation: do not change the following three commands
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  
  V[['car1']] = cost*(cost1) 
  V[['car2']] = cost*(cost2) 
  V[['car3']] = cost*(cost3)
  V[['car4']] = cost*(cost4)
  V[['car5']] = cost*(cost5)
  V[['car6']] = cost*(cost6)
  
  # V[['car1']] =  ev * (fuel1 == "electric") + large * (type1  == "sportuv" | type1  == "truck" | type1 == "van") + size * (size1 * 0.1) + sportcar * (type1 == "sportcar") + suv * (type1 == "sportuv") + truck * (type1 == "truck") + van * (type1 == "van") + priceLogInc * price1 + range * range1
  # V[['car2']] =  ev * (fuel2 == "electric") + large * (type2  == "sportuv" | type2  == "truck" | type2 == "van") + size * (size2 * 0.1) + sportcar * (type2 == "sportcar") + suv * (type2 == "sportuv") + truck * (type2 == "truck") + van * (type2 == "van") + priceLogInc * price2 + range * range2
  # V[['car3']] =  ev * (fuel3 == "electric") + large * (type3  == "sportuv" | type3  == "truck" | type3 == "van") + size * (size3 * 0.1) + sportcar * (type3 == "sportcar") + suv * (type3 == "sportuv") + truck * (type3 == "truck") + van * (type3 == "van") + priceLogInc * price3 + range * range3
  # V[['car4']] =  ev * (fuel4 == "electric") + large * (type4  == "sportuv" | type4  == "truck" | type4 == "van") + size * (size4 * 0.1) + sportcar * (type4 == "sportcar") + suv * (type4 == "sportuv") + truck * (type4 == "truck") + van * (type4 == "van") + priceLogInc * price4 + range * range4
  # V[['car5']] =  ev * (fuel5 == "electric") + large * (type5  == "sportuv" | type5  == "truck" | type5 == "van") + size * (size5 * 0.1) + sportcar * (type5 == "sportcar") + suv * (type5 == "sportuv") + truck * (type5 == "truck") + van * (type5 == "van") + priceLogInc * price5 + range * range5
  # V[['car6']] =  ev * (fuel6 == "electric") + large * (type6  == "sportuv" | type6  == "truck" | type6 == "van") + size * (size6 * 0.1) + sportcar * (type6 == "sportcar") + suv * (type6 == "sportuv") + truck * (type6 == "truck") + van * (type6 == "van") + priceLogInc * price6 + range * range6
  #V[['car7']] =  ev * (fuel7 == "electric") + large * (type7  == "sportuv" | type7  == "truck" | type7 == "van") + size * (size7 * 0.1) + sportcar * (type7 == "sportcar") + suv * (type7 == "sportuv") + truck * (type7 == "truck") + van * (type7 == "van") + priceLogInc * price7 + range * range7

  
  
  
  ### Define settings for MNL model component
  mnl_settings    = list(
      alternatives  = c(car1=1, car2=2, car3=3, car4=4, car5=5, car6=6),
      avail         = 1,
      choiceVar     = Choice,
      V             = V
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  #P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across intra-individual draws
  #P = apollo_avgIntraDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}



# ########################################################################## #
#### Calculate the model fit to the given parameter values
# ########################################################################## #
apollo_llCalc(apollo_beta,
              apollo_probabilities,
              apollo_inputs)

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #


model = apollo_estimate(apollo_beta, apollo_fixed,
                        apollo_probabilities, apollo_inputs, 
                        estimate_settings=list(hessianRoutine="maxLik"))

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #
summary(model)
apollo_modelOutput(model)

#save.image("C:/Lambert/thesis/Source/model.RData")
# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# ----------------------------------------------------------------- #

apollo_saveOutput(model)
model.estimates <- read_csv("car_purchase_mxl_estimates.csv")
xtable(model.estimates, digits=4)


# ---------------------------------------------------------------- #
#---                  Model Predictions                         ----
# ---------------------------------------------------------------- #
model = apollo_loadModel("car_purchase_mnl")

# Base prediction
predictions_base = apollo_prediction(model, apollo_probabilities, apollo_inputs)

##########################
##### Read New dataset####
##########################
#prediction_data<- read_csv("predict_dataset.csv")
#prediction_data <- as.data.frame(prediction_data)
#database <- prediction_data


#####################################################
##### Decrease the cost of all EV by 10% ############
#####################################################
mydata <- filter(database, fuel3=="electric")
database$cost1[which(database$fuel3 =="electric")] <- mydata$cost1 - (mydata$cost1 * 0.1)
database$cost2[which(database$fuel3 =="electric")] <- mydata$cost2 - (mydata$cost2 * 0.1)
database$cost3[which(database$fuel3 =="electric")] <- mydata$cost3 - (mydata$cost3 * 0.1)
database$cost4[which(database$fuel3 =="electric")] <- mydata$cost4 - (mydata$cost4 * 0.1)
database$cost5[which(database$fuel3 =="electric")] <- mydata$cost5 - (mydata$cost5 * 0.1)
database$cost6[which(database$fuel3 =="electric")] <- mydata$cost6 - (mydata$cost6 * 0.1)

mydata <- filter(database, fuel4=="electric")
database$cost1[which(database$fuel4 =="electric")] <- mydata$cost1 - (mydata$cost1 * 0.1)
database$cost2[which(database$fuel4 =="electric")] <- mydata$cost2 - (mydata$cost2 * 0.1)
database$cost3[which(database$fuel4 =="electric")] <- mydata$cost3 - (mydata$cost3 * 0.1)
database$cost4[which(database$fuel4 =="electric")] <- mydata$cost4 - (mydata$cost4 * 0.1)
database$cost5[which(database$fuel4 =="electric")] <- mydata$cost5 - (mydata$cost5 * 0.1)
database$cost6[which(database$fuel4 =="electric")] <- mydata$cost6 - (mydata$cost6 * 0.1)

mydata <- filter(database, fuel5=="electric")
database$cost1[which(database$fuel5 =="electric")] <- mydata$cost1 - (mydata$cost1 * 0.1)
database$cost2[which(database$fuel5 =="electric")] <- mydata$cost2 - (mydata$cost2 * 0.1)
database$cost3[which(database$fuel5 =="electric")] <- mydata$cost3 - (mydata$cost3 * 0.1)
database$cost4[which(database$fuel5 =="electric")] <- mydata$cost4 - (mydata$cost4 * 0.1)
database$cost5[which(database$fuel5 =="electric")] <- mydata$cost5 - (mydata$cost5 * 0.1)
database$cost6[which(database$fuel5 =="electric")] <- mydata$cost6 - (mydata$cost6 * 0.1)

mydata <- filter(database, fuel6=="electric")
database$cost1[which(database$fuel6 =="electric")] <- mydata$cost1 - (mydata$cost1 * 0.1)
database$cost2[which(database$fuel6 =="electric")] <- mydata$cost2 - (mydata$cost2 * 0.1)
database$cost3[which(database$fuel6 =="electric")] <- mydata$cost3 - (mydata$cost3 * 0.1)
database$cost4[which(database$fuel6 =="electric")] <- mydata$cost4 - (mydata$cost4 * 0.1)
database$cost5[which(database$fuel6 =="electric")] <- mydata$cost5 - (mydata$cost5 * 0.1)
database$cost6[which(database$fuel6 =="electric")] <- mydata$cost6 - (mydata$cost6 * 0.1)

##########################################################
# Run prediction with the discount on EV's cost  #########
##########################################################
prediction_new = apollo_prediction(model, apollo_probabilities, apollo_inputs)


#################################################
# Add alternative with attribute Cost 7 ########
################################################

data.list <- lapply(1, function(x) {
  nrep <- 1;
  cost7 <- c(replicate(nrep,(runif(4654,1,8))));
  data.frame(cost7);
});
database$cost7 <-  as.numeric(as.character(data.frame(data.list[[1]])[,1]))

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="prediction"){
  
  ### Function initialisation: do not change the following three commands
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  
  V[['car1']] = cost*(cost1) 
  V[['car2']] = cost*(cost2) 
  V[['car3']] = cost*(cost3)
  V[['car4']] = cost*(cost4)
  V[['car5']] = cost*(cost5)
  V[['car6']] = cost*(cost6)
  V[['car7']] = cost*(cost7)
  # V[['car1']] =  ev * (fuel1 == "electric") + large * (type1  == "sportuv" | type1  == "truck" | type1 == "van") + size * (size1 * 0.1) + sportcar * (type1 == "sportcar") + suv * (type1 == "sportuv") + truck * (type1 == "truck") + van * (type1 == "van") + priceLogInc * price1 + range * range1
  # V[['car2']] =  ev * (fuel2 == "electric") + large * (type2  == "sportuv" | type2  == "truck" | type2 == "van") + size * (size2 * 0.1) + sportcar * (type2 == "sportcar") + suv * (type2 == "sportuv") + truck * (type2 == "truck") + van * (type2 == "van") + priceLogInc * price2 + range * range2
  # V[['car3']] =  ev * (fuel3 == "electric") + large * (type3  == "sportuv" | type3  == "truck" | type3 == "van") + size * (size3 * 0.1) + sportcar * (type3 == "sportcar") + suv * (type3 == "sportuv") + truck * (type3 == "truck") + van * (type3 == "van") + priceLogInc * price3 + range * range3
  # V[['car4']] =  ev * (fuel4 == "electric") + large * (type4  == "sportuv" | type4  == "truck" | type4 == "van") + size * (size4 * 0.1) + sportcar * (type4 == "sportcar") + suv * (type4 == "sportuv") + truck * (type4 == "truck") + van * (type4 == "van") + priceLogInc * price4 + range * range4
  # V[['car5']] =  ev * (fuel5 == "electric") + large * (type5  == "sportuv" | type5  == "truck" | type5 == "van") + size * (size5 * 0.1) + sportcar * (type5 == "sportcar") + suv * (type5 == "sportuv") + truck * (type5 == "truck") + van * (type5 == "van") + priceLogInc * price5 + range * range5
  # V[['car6']] =  ev * (fuel6 == "electric") + large * (type6  == "sportuv" | type6  == "truck" | type6 == "van") + size * (size6 * 0.1) + sportcar * (type6 == "sportcar") + suv * (type6 == "sportuv") + truck * (type6 == "truck") + van * (type6 == "van") + priceLogInc * price6 + range * range6
  #V[['car7']] =  ev * (fuel7 == "electric") + large * (type7  == "sportuv" | type7  == "truck" | type7 == "van") + size * (size7 * 0.1) + sportcar * (type7 == "sportcar") + suv * (type7 == "sportuv") + truck * (type7 == "truck") + van * (type7 == "van") + priceLogInc * price7 + range * range7
  
  
  
  
  ### Define settings for MNL model component
  mnl_settings    = list(
    alternatives  = c(car1=1, car2=2, car3=3, car4=4, car5=5, car6=6, car7=7),
    avail         = 1,
    choiceVar     = Choice,
    V             = V
  )
  
  ### Compute probabilities using MNL model
  P[['model2']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
 # P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across intra-individual draws
  #P = apollo_avgIntraDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

apollo_inputs   = apollo_validateInputs()

## prediction of market share with new alternative
predictions_new2 = apollo_prediction(model, 
                                     apollo_probabilities, 
                                     apollo_inputs, prediction_settings = "model2")

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, predicted result)               ----
# ----------------------------------------------------------------- #

final <- cbind(date_time=format(Sys.time(), format="%Y/%m/%d %H:%M"),
               predictions = prediction_new)

write.csv(final,"Predictions_basicmodel.csv", row.names = FALSE)


