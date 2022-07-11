# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

loadModel <- function(model.name){
  f = system.file("extdata", model.name, package = "climaticSystemsPredictions")
  return(readRDS(f))
}

loadMeans <- function(file.name){
  f = system.file("extdata", file.name, package = "climaticSystemsPredictions")
  return(read.csv(f))
}

#' Random forest prediction
#'
#' Prediction of reaching the target temperature inside the greenhouse, taking into account the different parameters, when the 3 fans, the recirculators and the cooling are on at a given time.
#'
#' @param innerTemp0 Numerical vector containing the temperature (in Celsius) inside the greenhouse at the moment of switching on the climatic systems.
#' @param innerTempF Numerical vector containing the temperature (in Celsius) inside the greenhouse at the moment of switching off the climatic systems.
#' @param innerHum0 Numerical vector containing the relative humidity (percentage) inside the greenhouse at the moment of switching on the climatic systems.
#' @param extTemp0 Numerical vector containing the temperature (in Celsius) outside the greenhouse at the moment of switching on the climatic systems.
#' @param extTempF Numerical vector containing the temperature (in Celsius) outside the greenhouse at the moment of switching off the climatic systems.
#' @param extHum0 Numerical vector containing the relative humidity (percentage) outside the greenhouse at the moment of switching on the climatic systems.
#' @param extHumF Numerical vector containing the relative humidity (percentage) outside the greenhouse at the moment of switching off the climatic systems.
#' @param extRad0 Numerical vector containing the radiation (in W/m^2) outside the greenhouse at the moment of switching on the climatic systems.
#' @param extRadF Numerical vector containing the radiation (in W/m^2) outside the greenhouse at the moment of switching off the climatic systems.
#' @param secondsON Numerical vector containing the switch-on times of the climatic systems in seconds.
#' @return Logical: TRUE if the target temperature is reached and FALSE if not, Numeric: indicating the final humidity inside the greenhouse and Numeric: indicating the energy consumption in watts
#' @author Jose Mendoza Bernal
#' @examples
#'getPred.rf.3FansRecirculatorsCooling(27.4, 25.2, 70.5, 22.2, 21.7, 100, 98, 414, 477, 1980)
#'
#' @import randomForest
#' @import stats
#' @export
getPred.rf.3FansRecirculatorsCooling <- function(innerTemp0, innerTempF, innerHum0, extTemp0,
                                                 extTempF, extHum0, extHumF, extRad0, extRadF, secondsON) {

  daySchedule = factor(rep(1, length(innerTemp0)))

  # Create the data frame with the data
  df <- data.frame(innerTemp0, innerTempF, innerHum0, extTemp0,
                   extTempF, extRad0, extRadF, daySchedule, secondsON)

  # Load the trained model
  rf.fit <- loadModel("3fans+recirculators+cooling-rf-augmented-scaled.rds")

  # Load the train means file
  means <- loadMeans("3fans+recirculators+cooling-means.csv")

  # Center and scale predicted data
  df$innerTemp0 <- (df$innerTemp0 - means$innerTemp0[1])/means$innerTemp0[2]
  df$innerTempF <- (df$innerTempF - means$innerTempF[1])/means$innerTempF[2]
  df$innerHum0 <- (df$innerHum0 - means$innerHum0[1])/means$innerHum0[2]
  df$extTemp0 <- (df$extTemp0 - means$extTemp0[1])/means$extTemp0[2]
  df$extTempF <- (df$extTempF - means$extTempF[1])/means$extTempF[2]
  df$extRad0 <- (df$extRad0 - means$extRad0[1])/means$extRad0[2]
  df$extRadF <- (df$extRadF - means$extRadF[1])/means$extRadF[2]
  df$secondsON <- (df$secondsON - means$secondsON[1])/means$secondsON[2]

  # Predict the new data
  out <- predict(rf.fit, newdata = df, type = "raw")

  # Final humidity estimation
  climaticSystem = rep(3, length(innerTemp0))
  humidity <- getPred.rf.finalHumidity(climaticSystem, innerTemp0, innerTempF, innerHum0, extTemp0,
                                       extTempF, extHum0, extHumF, extRad0, extRadF, secondsON)

  # Consumption in watts
  consumption <- round(740*secondsON/3600 + (1470+375+375)*secondsON/3600 + 450*secondsON/3600, digits = 1)

  output <- data.frame("Target" = as.logical(as.integer(out$pred)-1), "Humidity" = humidity, "Consumption" = consumption)

  # Return predictions
  return(output)
}

#' Random forest prediction
#'
#' Prediction of reaching the target temperature inside the greenhouse, taking into account the different parameters, when the 3 fans and the recirculators are on at a given time.
#'
#' @param innerTemp0 Numerical vector containing the temperature (in Celsius) inside the greenhouse at the moment of switching on the climatic systems.
#' @param innerTempF Numerical vector containing the temperature (in Celsius) inside the greenhouse at the moment of switching off the climatic systems.
#' @param innerHum0 Numerical vector containing the relative humidity (percentage) inside the greenhouse at the moment of switching on the climatic systems.
#' @param extTemp0 Numerical vector containing the temperature (in Celsius) outside the greenhouse at the moment of switching on the climatic systems.
#' @param extTempF Numerical vector containing the temperature (in Celsius) outside the greenhouse at the moment of switching off the climatic systems.
#' @param extHum0 Numerical vector containing the relative humidity (percentage) outside the greenhouse at the moment of switching on the climatic systems.
#' @param extHumF Numerical vector containing the relative humidity (percentage) outside the greenhouse at the moment of switching off the climatic systems.
#' @param extRad0 Numerical vector containing the radiation (in W/m^2) outside the greenhouse at the moment of switching on the climatic systems.
#' @param extRadF Numerical vector containing the radiation (in W/m^2) outside the greenhouse at the moment of switching off the climatic systems.
#' @param secondsON Numerical vector containing the switch-on times of the climatic systems in seconds.
#' @return Logical: TRUE if the target temperature is reached and FALSE if not, Numeric: indicating the final humidity inside the greenhouse and Numeric: indicating the energy consumption in watts
#' @author Jose Mendoza Bernal
#' @examples
#'getPred.rf.3FansRecirculators(27.0, 26.6, 64.5, 21.4, 21.6, 60, 60, 924, 925, 120)
#'
#' @import randomForest
#' @import stats
#' @export
getPred.rf.3FansRecirculators <- function(innerTemp0, innerTempF, innerHum0, extTemp0,
                                          extTempF, extHum0, extHumF, extRad0, extRadF, secondsON) {

  daySchedule = factor(rep(1, length(innerTemp0)))

  # Create the data frame with the data
  df <- data.frame(innerTemp0, innerTempF, innerHum0, extTemp0,
                   extTempF, extRad0, extRadF, daySchedule, secondsON)

  # Load the trained model
  rf.fit <- loadModel("3fans+recirculators-rf-augmented-scaled.rds")

  # Load the train means file
  means <- loadMeans("3fans+recirculators-means.csv")

  # Center and scale predicted data
  df$innerTemp0 <- (df$innerTemp0 - means$innerTemp0[1])/means$innerTemp0[2]
  df$innerTempF <- (df$innerTempF - means$innerTempF[1])/means$innerTempF[2]
  df$innerHum0 <- (df$innerHum0 - means$innerHum0[1])/means$innerHum0[2]
  df$extTemp0 <- (df$extTemp0 - means$extTemp0[1])/means$extTemp0[2]
  df$extTempF <- (df$extTempF - means$extTempF[1])/means$extTempF[2]
  df$extRad0 <- (df$extRad0 - means$extRad0[1])/means$extRad0[2]
  df$extRadF <- (df$extRadF - means$extRadF[1])/means$extRadF[2]
  df$secondsON <- (df$secondsON - means$secondsON[1])/means$secondsON[2]

  # Predict the new data
  out <- predict(rf.fit, newdata = df, type = "raw")

  # Final humidity estimation
  climaticSystem = rep(2, length(innerTemp0))
  humidity <- getPred.rf.finalHumidity(climaticSystem, innerTemp0, innerTempF, innerHum0, extTemp0,
                                       extTempF, extHum0, extHumF, extRad0, extRadF, secondsON)

  # Consumption in watts
  consumption <- round(740*secondsON/3600 + (1470+375+375)*secondsON/3600, digits = 1)

  output <- data.frame("Target" = as.logical(as.integer(out$pred)-1), "Humidity" = humidity, "Consumption" = consumption)

  # Return predictions
  return(output)
}

#' Random forest prediction
#'
#' Prediction of reaching the target temperature inside the greenhouse, taking into account the different parameters, when the 2 fans and the recirculators are on at a given time.
#'
#' @param innerTemp0 Numerical vector containing the temperature (in Celsius) inside the greenhouse at the moment of switching on the climatic systems.
#' @param innerTempF Numerical vector containing the temperature (in Celsius) inside the greenhouse at the moment of switching off the climatic systems.
#' @param innerHum0 Numerical vector containing the relative humidity (percentage) inside the greenhouse at the moment of switching on the climatic systems.
#' @param extTemp0 Numerical vector containing the temperature (in Celsius) outside the greenhouse at the moment of switching on the climatic systems.
#' @param extTempF Numerical vector containing the temperature (in Celsius) outside the greenhouse at the moment of switching off the climatic systems.
#' @param extHum0 Numerical vector containing the relative humidity (percentage) outside the greenhouse at the moment of switching on the climatic systems.
#' @param extHumF Numerical vector containing the relative humidity (percentage) outside the greenhouse at the moment of switching off the climatic systems.
#' @param extRad0 Numerical vector containing the radiation (in W/m^2) outside the greenhouse at the moment of switching on the climatic systems.
#' @param extRadF Numerical vector containing the radiation (in W/m^2) outside the greenhouse at the moment of switching off the climatic systems.
#' @param secondsON Numerical vector containing the switch-on times of the climatic systems in seconds.
#' @return Logical: TRUE if the target temperature is reached and FALSE if not, Numeric: indicating the final humidity inside the greenhouse and Numeric: indicating the energy consumption in watts
#' @author Jose Mendoza Bernal
#' @examples
#'getPred.rf.2FansRecirculators(26.7, 26.3, 76.4, 21.6, 21.4, 70, 70, 80, 80, 50)
#'
#' @import randomForest
#' @import stats
#' @export
getPred.rf.2FansRecirculators <- function(innerTemp0, innerTempF, innerHum0, extTemp0,
                                          extTempF, extHum0, extHumF, extRad0, extRadF, secondsON) {

  daySchedule = factor(rep(1, length(innerTemp0)))

  # Create the data frame with the data
  df <- data.frame(innerTemp0, innerTempF, innerHum0, extTemp0,
                   extTempF, extRad0, extRadF, daySchedule, secondsON)

  # Load the trained model
  rf.fit <- loadModel("2fans+recirculators-rf-augmented-scaled.rds")

  # Load the train means file
  means <- loadMeans("2fans+recirculators-means.csv")

  # Center and scale predicted data
  df$innerTemp0 <- (df$innerTemp0 - means$innerTemp0[1])/means$innerTemp0[2]
  df$innerTempF <- (df$innerTempF - means$innerTempF[1])/means$innerTempF[2]
  df$innerHum0 <- (df$innerHum0 - means$innerHum0[1])/means$innerHum0[2]
  df$extTemp0 <- (df$extTemp0 - means$extTemp0[1])/means$extTemp0[2]
  df$extTempF <- (df$extTempF - means$extTempF[1])/means$extTempF[2]
  df$extRad0 <- (df$extRad0 - means$extRad0[1])/means$extRad0[2]
  df$extRadF <- (df$extRadF - means$extRadF[1])/means$extRadF[2]
  df$secondsON <- (df$secondsON - means$secondsON[1])/means$secondsON[2]

  # Predict the new data
  out <- predict(rf.fit, newdata = df, type = "raw")

  # Final humidity estimation
  climaticSystem = rep(1, length(innerTemp0))
  humidity <- getPred.rf.finalHumidity(climaticSystem, innerTemp0, innerTempF, innerHum0, extTemp0,
                                       extTempF, extHum0, extHumF, extRad0, extRadF, secondsON)

  # Consumption in watts
  consumption <- round(740*secondsON/3600 + (1470+375)*secondsON/3600, digits = 1)

  output <- data.frame("Target" = as.logical(as.integer(out$pred)-1), "Humidity" = humidity, "Consumption" = consumption)

  # Return predictions
  return(output)
}


#' Random forest prediction
#'
#' Prediction of the final humidity inside the greenhouse, taking into account the different parameters.
#'
#' @param climaticSystem Numerical vector containing number 1 when the two fans and the recirculators are on, 2 when the three fans and the recirculators are on or 3 when the three fans, the recirculators and Cooling are on.
#' @param innerTemp0 Numerical vector containing the temperature (in Celsius) inside the greenhouse at the moment of switching on the climatic systems.
#' @param innerTempF Numerical vector containing the temperature (in Celsius) inside the greenhouse at the moment of switching off the climatic systems.
#' @param innerHum0 Numerical vector containing the relative humidity (percentage) inside the greenhouse at the moment of switching on the climatic systems.
#' @param extTemp0 Numerical vector containing the temperature (in Celsius) outside the greenhouse at the moment of switching on the climatic systems.
#' @param extTempF Numerical vector containing the temperature (in Celsius) outside the greenhouse at the moment of switching off the climatic systems.
#' @param extRad0 Numerical vector containing the radiation (in W/m^2) outside the greenhouse at the moment of switching on the climatic systems.
#' @param extRadF Numerical vector containing the radiation (in W/m^2) outside the greenhouse at the moment of switching off the climatic systems.
#' @param extHum0 Numerical vector containing the relative humidity (percentage) outside the greenhouse at the moment of switching on the climatic systems.
#' @param extHumF Numerical vector containing the relative humidity (percentage) outside the greenhouse at the moment of switching off the climatic systems.
#' @param secondsON Numerical vector containing the switch-on times of the climatic systems in seconds.
#' @return Numeric: indicating the final humidity inside the greenhouse.
#' @author Jose Mendoza Bernal
#' @examples
#'getPred.rf.finalHumidity(3, 27.4, 25.2, 70.5, 22.2, 21.7, 100, 98, 414, 477, 1980)
#'
#' @import stats
#' @import utils
#' @import randomForest
#' @export
getPred.rf.finalHumidity <- function(climaticSystem, innerTemp0, innerTempF, innerHum0, extTemp0,
                                     extTempF, extHum0, extHumF, extRad0, extRadF, secondsON) {

  climaticSystem = replace(climaticSystem, climaticSystem==2, 5)
  climaticSystem = replace(climaticSystem, climaticSystem==3, 6)
  climaticSystem = replace(climaticSystem, climaticSystem==1, 2)

  # Create the data frame for humidity prediction
  df <- data.frame(climaticSystem, innerTemp0, innerTempF, innerHum0, extTemp0,
                   extTempF, extHum0, extHumF, extRad0, extRadF, secondsON)

  # Load the trained model for estimating the final humidity
  rf.hum <- loadModel("innerHumidityFinal-rf.rds")

  # Load the train means file
  means <- loadMeans("innerHumidityFinal-means.csv")

  # Center and scale predicted data
  df$innerTemp0 <- (df$innerTemp0 - means$innerTemp0[1])/means$innerTemp0[2]
  df$innerTempF <- (df$innerTempF - means$innerTempF[1])/means$innerTempF[2]
  df$innerHum0 <- (df$innerHum0 - means$innerHum0[1])/means$innerHum0[2]
  df$extTemp0 <- (df$extTemp0 - means$extTemp0[1])/means$extTemp0[2]
  df$extTempF <- (df$extTempF - means$extTempF[1])/means$extTempF[2]
  df$extHum0 <- (df$extHum0 - means$extHum0[1])/means$extHum0[2]
  df$extHumF <- (df$extHumF - means$extHumF[1])/means$extHumF[2]
  df$extRad0 <- (df$extRad0 - means$extRad0[1])/means$extRad0[2]
  df$extRadF <- (df$extRadF - means$extRadF[1])/means$extRadF[2]
  df$secondsON <- (df$secondsON - means$secondsON[1])/means$secondsON[2]

  # Predict the new data
  out <- predict(rf.hum, newdata = df, type = "raw")
  out <- out * means$innerHumF[2] + means$innerHumF[1]

  # Return predictions
  return(round(out, 1))
}

