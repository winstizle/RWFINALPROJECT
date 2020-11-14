#' A 10 Day RVA weather forecaster
#'
#' This function allows you to predict the weather ten days into the future
#' @param Initial What is the intial weather. E.G."Sunny"
#' @keywords Weather
#' @export
#' @examples
#' RVAForecast("Sunny")

RVAForecast <- function(Initial) {
  ProbabilityMatrix <- matrix(c(0.85, 0.15, 0.65, 0.35), 2, 2, T)
  Start <- Initial
  ifelse(Start == "Sunny", InitialVector <- c(1, 0), InitialVector <- c(0, 1))
  Weatherdata <- data.frame("Day" = 1:10, "Prediction" = rep(0, 10), "Rainfall" = rep(0, 10))
  for (i in 1:10) {
    FollowingDay <- InitialVector %*% ProbabilityMatrix
    NextDaySim <- rbern(1, p = FollowingDay[1])
    ifelse(NextDaySim == 1, Weatherdata$Prediction[i] <- "Sunny", Weatherdata$Prediction[i] <- "Rainy")
    ifelse(NextDaySim == 0, Weatherdata$Rainfall[i] <- rexp(1, 2), Weatherdata$Rainfall[i] <- round(0, 1))
    FollowingDay <- FollowingDay %*% ProbabilityMatrix
  }

  Weatherdata %>% mutate("Sunny Days" = cumsum(Prediction == "Sunny"), "Total Rainfall in Inches" = cumsum(Rainfall))
}
