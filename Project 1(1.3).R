setwd("C:\\Users\\Nikant Yadav\\Desktop\\Internship Preparation\\ECO project\\Project 1")
co2data <- read.csv("co2data3.csv", na.strings = "***")
tempdata <- read.csv("NH.Ts+dSST.csv", skip = 1, na.strings = "***")

#part 1.3 (3)
par(mfrow = c(1, 1))
plot(co2data$Year[23:length(co2data$Year)], co2data$Interpolated[23:length(co2data$Year)], type="l",
     xlab="Year", ylab="CO2(ppm)", col="blue", main="Trend and Interpolated monthly mean CO2")

lines(co2data$Year[23:length(co2data$Year)], co2data$Trend[23:length(co2data$Year)], type="l",
      xlab="Year", ylab="CO2(ppm)", col = "red", lwd="2")

legend("bottomright", legend = c("Interpolated", "Trend"), col = c("blue", "red"), lty = 1)



# part 1.3 (4a)
trend <- unlist(co2data[5])
month <- unlist(co2data[2])
jan <- c()
n = length(co2data$Year)
for(i in 1:n) {
  if(month[i] == 1) {
    jan <- append(jan,trend[i])
  } 
}

jan <- unname(jan)
zero1 <- rep(0,79)
zero2 <- rep(0,7)
jan <- append(zero1,jan)
jan <- append(jan, zero2)
tempdata$co2trendjan <- jan

filtered_data <- tempdata[tempdata$co2trendjan != 0, ]

plot(filtered_data$Jan, filtered_data$co2trendjan, xlab= "Anomaly Temperature", ylab="CO2(ppm) Trend",
     pch = 16, col = "blue", main = "Scatterplot for CO2 emmissions and temperature anomalies")

#part 1.3 (4b)

correlation = cor(filtered_data$Jan, filtered_data$co2trendjan,
                  method = "pearson")





