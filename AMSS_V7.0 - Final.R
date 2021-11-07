#This script has been produced as part of my professional thesis focused on comparing Linear Regression and XGBoost in the context of marketing mix modeling (check the python notebook for the analysis).
#Below you will find the code used to generate the dataset used in the research.
#Please note that the generation process includes an element of randomness.
#This element will not allow you to generate the exact same dataset that is used in the research.
#Best regards, Alexander Ninua, 2021.


library(amss)
library(ggplot2)


n.years <- 4
time.n <- n.years * 52



# States: Unaware, exploratory, purchase

activity.transition <- matrix(
  c(0.7, 0.25, 0.05,  # migration originating from inactive state
    0.45, 0.40, 0.15,  # exploratory state
    0.25, 0.50, 0.25),  # purchase state
  nrow = length(kActivityStates), byrow = TRUE)


favorability.transition <- matrix(
  c(0.85, 0.00, 0.07, 0.05, 0.02,  # migration from the unaware state
    0.00, 0.85, 0.15, 0.00, 0.00,  # negative state
    0.20, 0.00, 0.70, 0.10, 0.00,  # neutral state
    0.00, 0.00, 0.30, 0.60, 0.10,  # somewhat favorable state
    0.00, 0.00, 0.10, 0.20, 0.70),  # favorable state
  nrow = length(kFavorabilityStates), byrow = TRUE)

market.rate.nonoise <-
  SimulateSinusoidal(n.years * 52, 52,
                     vert.trans = 0.6, amplitude = 0.22)
# with some added noise
market.rate.seas <- pmax(
  0, pmin(1,
          market.rate.nonoise *
            SimulateAR1(length(market.rate.nonoise), 1, 0.1, 0.3)))

nat.mig.params <- list(
  population = 6.5e7,
  market.rate.trend = 0.70,
  market.rate.seas = market.rate.seas,
  # default activity status of a newly arrived customer
  prop.activity = c(0.3, 0.5, 0.2),
  # default brand favorability of a newly arrived customer
  prop.favorability = c(0.03, 0.05, 0.82, 0.05, 0.05),
  # Customer's inclination to switch brands, stay loyal to our brand or stay loyal to the competitor's brand
  prop.loyalty = c(0.95, 0.025, 0.025),
  transition.matrices = list(
    activity = activity.transition,
    favorability = favorability.transition))


budget.index <- rep(1:n.years, each = 52)

tv.flighting <-
  pmax(0,
       market.rate.seas +
         SimulateAR1(length(market.rate.seas), -1.1, 0.5, -0.8))
tv.flighting <- tv.flighting[c(6:length(tv.flighting), 1:5)]


tv.ar.data <- data.frame(flighting = tv.flighting, index = 1:208)

ggplot(data = tv.ar.data, aes(x = index, y = flighting))+
  geom_line()



tv.activity.trans.mat <- matrix(
  c(0.05, 0.70, 0.25,  # migration originating from the inactive state
    0.05, 0.35, 0.60,  # exploratory state
    0.00, 0.10, 0.90),  # purchase state
  nrow = length(kActivityStates), byrow = TRUE)
tv.favorability.trans.mat <- matrix(
  c(0.05,  0.00,  0.25, 0.50, 0.20,  # migration from the unaware state
    0.0,  0.6,  0.2, 0.15, 0.05,  # negative state
    0.0,  0.0,  0.1, 0.55, 0.35,  # neutral state
    0.0,  0.0,  0.0, 0.2, 0.80,  # somewhat favorable state
    0.0,  0.0,  0.0, 0.10, 0.90),  # favorable state
  nrow = length(kFavorabilityStates), byrow = TRUE)



params.tv <- list(
  audience.membership = list(activity = rep(0.4, 3)),
  budget = rep(c(21e5, 24e5, 25e5, 23e5), length = n.years),
  budget.index = budget.index,
  flighting = tv.flighting,
  unit.cost = 0.006,
  hill.ec = 1.56,
  hill.slope = 1,
  transition.matrices = list(
    activity = tv.activity.trans.mat,
    favorability = tv.favorability.trans.mat))


#---- RADIO

radio.flighting <-
  pmax(0,
       market.rate.seas +
         SimulateAR1(length(market.rate.seas), -0.59, 0.2, -0.7))
radio.flighting <- radio.flighting[c(6:length(radio.flighting), 1:5)]



radio.ar.data <- data.frame(flighting = radio.flighting, index = 1:208)

ggplot(data = radio.ar.data, aes(x = index, y = flighting))+
  geom_line()


radio.activity.trans.mat <- matrix(
  c(0.8, 0.15, 0.05,  # migration originating from the inactive state
    0.20, 0.65, 0.15,  # exploratory state
    0.15, 0.65, 0.20),  # purchase state
  nrow = length(kActivityStates), byrow = TRUE)
radio.favorability.trans.mat <- matrix(
  c(0.35,  0.05,  0.50, 0.10, 0.00,  # migration from the unaware state
    0.0,  0.90,  0.10, 0.00, 0.00,  # negative state
    0.0,  0.05,  0.90, 0.05, 0.00,  # neutral state
    0.0,  0.0,  0.40, 0.50, 0.10,  # somewhat favorable state
    0.0,  0.1,  0.50, 0.03, 0.10),  # favorable state
  nrow = length(kFavorabilityStates), byrow = TRUE)

params.radio <- list(
  audience.membership = list(activity = rep(0.4, 3)),
  budget = rep(c(6e5, 4e5, 3e5, 5.5e5), length = n.years),
  budget.index = budget.index,
  flighting = radio.flighting,
  unit.cost = 0.005,
  hill.ec = 1.56,
  hill.slope = 1,
  transition.matrices = list(
    activity = radio.activity.trans.mat,
    favorability = radio.favorability.trans.mat))

#PRESS -------

press.flighting <-
  pmax(0,
       market.rate.seas +
         SimulateAR1(length(market.rate.seas), -0.3, 0.1, -0.9))
press.flighting <- press.flighting[c(6:length(press.flighting), 1:5)]

press.flighting

press.ar.data <- data.frame(flighting = press.flighting, index = 1:208)

ggplot(data = press.ar.data, aes(x = index, y = flighting))+
  geom_line()



press.activity.trans.mat <- matrix(
  c(0.3, 0.4, 0.2,  # migration originating from the inactive state
    0.25, 0.45, 0.30,  # exploratory state
    0.05, 0.65, 0.30),  # purchase state
  nrow = length(kActivityStates), byrow = TRUE)
press.favorability.trans.mat <- matrix(
  c(0.25,  0.05,  0.30, 0.25, 0.15,  # migration from the unaware state
    0.0,  0.8,  0.1, 0.05, 0.05,  # negative state
    0.0,  0.05,  0.35, 0.50, 0.10,  # neutral state
    0.0,  0.0,  0.05, 0.60, 0.35,  # somewhat favorable state
    0.0,  0.00,  0.00, 0.15, 0.85),  # favorable state
  nrow = length(kFavorabilityStates), byrow = TRUE)

params.press <- list(
  audience.membership = list(activity = rep(0.4, 3)),
  budget = rep(c(11e5, 9e5, 12e5, 10.5e5), length = n.years),
  budget.index = budget.index,
  flighting = press.flighting,
  unit.cost = 0.0033,
  hill.ec = 1.56,
  hill.slope = 1,
  transition.matrices = list(
    activity = press.activity.trans.mat,
    favorability = press.favorability.trans.mat))


#-------

sales.params <- list(
  competitor.demand.max = list(loyalty = c(0.8, 0, 0.8)),
  advertiser.demand.slope = list(favorability = rep(0, 5)),
  advertiser.demand.intercept = list(
    favorability = c(0.014, 0, 0.2, 0.3, 0.9)),
  price = 8)


sim.data <- SimulateAMSS(
  time.n = time.n,
  nat.mig.params = nat.mig.params,
  media.names = c("tv", "radio", 'press'),
  media.modules = c(
    `DefaultTraditionalMediaModule`,
    `DefaultTraditionalMediaModule`,
    `DefaultTraditionalMediaModule`),
  media.params = list(params.tv, params.radio, params.press),
  sales.params = sales.params)


burn.in.length <- 52
final.year.end <- n.years * 52
final.year.start <- final.year.end - 51
observed.data <- sim.data$data[(burn.in.length + 1):final.year.end, ]



dirname(rstudioapi::getSourceEditorContext()$path)
fName = paste0(dirname(rstudioapi::getSourceEditorContext()$path),paste('/V.7.0.AMSS_Data ', toString(Sys.Date()), '.csv', sep=""))

typeof(observed.data)

write.csv(observed.data, file = fName)


         