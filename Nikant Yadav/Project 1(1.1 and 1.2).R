setwd("C:\\Users\\Nikant Yadav\\Desktop\\Internship Preparation\\ECO project\\Project 1\\Nikant Yadav")
tempdata <- read.csv("NH.Ts+dSST.csv", skip = 1, na.strings = "***")


#part 1.1 (1,2)
plot( tempdata$Year, tempdata$Jan,
      xlab="Year", ylab="Anomaly", 
      main="January Anomaly variation over years", type="l")

abline(h = 0, col = "darkorange2", lwd=2)
text(2000, -0.1, "1951-1980 average")

#part 1.1 (3)
column = c("Jan", "Feb", "Mar", "Apr","May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec","JD","DN" ,"DJF", "MAM","JJA","SON")

par(mfrow = c(2, 2))

for(i in 15:length(column)){
  yaxis = column[i]
  plot(tempdata$Year, tempdata[[yaxis]],  
       xlab="Year", ylab="Average Anomaly", 
       main=paste(column[i], "Anomaly variation over years"), type="l")
}

par(mfrow = c(1,1))

#part 1.1 (4)

plot(tempdata$Year, tempdata$J.D,  
     xlab="Year", ylab="Average Annual Anomaly", 
     main= "Average Annual Anomaly variation over years", type="l")


#part 1.2 (1)
#my method 1
rangeof_anomaly <- seq(-0.3, 1.05, 0.05)

data51_80 = array(tempdata[71:101, c("Jun", "Jul", "Aug")])
data81_20 = array(tempdata[102:131, c("Jun","Jul","Aug")])
vector_data51 <- unlist(data51_80)
vector_data81 <- unlist(data81_20)
freq1 <- table(cut(vector_data51, breaks=rangeof_anomaly))
freq2 <- table(cut(vector_data81, breaks=rangeof_anomaly))

par(mfrow = c(1, 2))
hist1 <- hist(vector_data51, breaks=rangeof_anomaly, plot=TRUE, xlab="Anomaly", main="1951-1980")
hist2 <- hist(vector_data81, breaks=rangeof_anomaly, plot=TRUE, xlab="Anomaly", main="1981-2020")

#book method
tempdata$Period <- factor(NA, levels = c("1921-1950", "1951-1980", "1981-2020"), ordered = TRUE)

tempdata$Period[(tempdata$Year > 1920) & (tempdata$Year < 1951)] <- "1921-1950"
tempdata$Period[(tempdata$Year > 1950) & (tempdata$Year < 1981)] <- "1951-1980"
tempdata$Period[(tempdata$Year > 1980) & (tempdata$Year < 2021)] <- "1981-2020"
temp_summer <- c(tempdata$Jun, tempdata$Jul, tempdata$Aug)
temp_Period <- c(tempdata$Period, tempdata$Period, tempdata$Period)
temp_Period <- factor(temp_Period, levels = levels(tempdata$Period))

hist1 = hist(temp_summer[temp_Period == "1951-1980"], main = "1951-1980", xlab = "Anomaly", ylab = "Frequency")
hist2 = hist(temp_summer[temp_Period == "1981-2020"], main = "1981-2020", xlab = "Anomaly", ylab = "Frequency")


#part 1.2 (3)
#my method

temp <-
temp_all_5180 <- subset(tempdata, (Year>=1951 & Year <= 1980))
temp51_80 <- unname(unlist(temp_all_5180[,2:13]))
season51_80 <- temp_all_5180[ , 16:19]
DJF51_80 <-  unname(unlist(season51_80[1]))
MAM51_80 <-  unname(unlist(season51_80[2]))
JJA51_80 <-  unname(unlist(season51_80[3]))
SON51_80 <-  unname(unlist(season51_80[4]))

n <- length(temp51_80)
for (j in 1:(n - 1)) {
  for (i in 1:(n - j)) {
    if (temp51_80[i + 1] < temp51_80[i]) {
      temp <- temp51_80[i]
      temp51_80[i] <- temp51_80[i + 1]
      temp51_80[i + 1] <- temp
    }
  }
}

decile <- c()
for (i in 1:10) {
  decile[i] = i*(n+1)/10
}

dec3 <- temp51_80[floor(decile[3])] + (decile[3] - floor(decile[3])) * (temp51_80[ceiling(decile[3])] - temp51_80[floor(decile[3])])

dec7 <- temp51_80[floor(decile[7])] + (decile[7] - floor(decile[7])) * (temp51_80[ceiling(decile[7])] - temp51_80[floor(decile[7])])


#book method
perc <- quantile(temp51_80, c(0.3, 0.7))
p30 <- perc[1]
p70 <- perc[2]




#part 1.2 (4)
#my method
temp_all_8110 <- subset(tempdata, (Year>=1981 & Year <= 2010))
temp81_10 <- unname(unlist(temp_all_8110[,2:13]))
season81_10 <- temp_all_8110[ , 16:19]
DJF81_10 <-  unname(unlist(season81_10[1]))
MAM81_10 <-  unname(unlist(season81_10[2]))
JJA81_10 <-  unname(unlist(season81_10[3]))
SON81_10 <-  unname(unlist(season81_10[4]))

#sorting the vector
n <- length(temp81_10)
for (j in 1:(n - 1)) {
  for (i in 1:(n - j)) {
    if (temp81_10[i + 1] < temp81_10[i]) {
      temp <- temp81_10[i]
      temp81_10[i] <- temp81_10[i + 1]
      temp81_10[i + 1] <- temp
    }
  }
}


count <- 0

for (i in 1:n){
  if(temp81_10[i]>p70) {
    count <- count + 1
  }
}
percentage <- (count/n)*100



#part 1.2 (5)
#my method
temp_all_2150 <- subset(tempdata, (Year>=1921 & Year <= 1950))
season21_50 <- temp_all_2150[ , 16:19]
DJF21_50 <- unname(unlist(season21_50[1]))
MAM21_50 <-  unname(unlist(season21_50[2]))
JJA21_50 <-  unname(unlist(season21_50[3]))
SON21_50 <-  unname(unlist(season21_50[4]))

mean(DJF21_50)
var(DJF21_50)

#using this method, means and variances can be calculated like this for all different seasons
#of different years

#book method
library(mosaic)
mean_DJF <- mean(~DJF|Period,data = tempdata)
mean_MAM <- mean(~MAM|Period,data = tempdata)
mean_JJA <- mean(~JJA|Period,data = tempdata)
mean_SON <- mean(~SON|Period,data = tempdata)
var_DJF <- var(~DJF|Period,data = tempdata)
var_MAM <- var(~MAM|Period,data = tempdata)
var_JJA <- var(~JJA|Period,data = tempdata)
var_SON <- var(~SON|Period,data = tempdata)




