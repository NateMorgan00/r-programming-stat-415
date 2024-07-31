library(readxl)
weather <- read_xlsx(params$weather)

Exercise 1:

myfun <- function(y){
  x <- sample(y,20)
  s <- vector()
  d <- vector()
  m = 1
  n = 1
  for (i in x) {
    if (i <= 100 & i%%3 == 0){
      s[[m]] <- i
      m = m + 1
    }else if (i > 100 & i%%4 == 0){
      d[[n]] <- i
      n = n + 1
    }
  }
  l <- c(s,d)
  return(l)
}
my_vec <- c(1:200)
myfun(my_vec)



Exercise 2:

x <- seq(-4, 45, length=500)                                                        # x values
dist.x <- dnorm(x, mean = 20, sd = 5)                                               # Returns CDF of normal distribution with mean 20 and sd 5.

degf <- c(4, 10, 20)                                                                # Set up the three degree of freedoms
colors <- c("green", "red", "blue", "black")                                        # Set up the four colors 
labels <- c("df=4", "df=10", "df=20",  "normal")                                    # Label the plots

plot(x, dist.x, type="l", lty=2, lwd=2, xlab= "x value",
     ylab= "Density", main= "Comparison of Normal and Chi-square Distributions")    # Plot normal density curve

for (i in 1:3){
lines(x, dchisq(x, degf[i]), lty=1, lwd=2, col=colors[i])                           # Plot chi-square distribution curves
  # dchisq() returns CDF of chi-square distribution
  # col= set up colors for the chi-square distributions
}

legend("topright", inset=0.01, title="Distributions",                               # Set up the title of legend
       labels, lwd=2, lty=c(1, 1, 1, 2), col=colors)                                # Set up line types appearing in the colors



Exercise 3

library(ggplot2)
library(ggridges)
weather$Month <- factor(weather$Month,
                        levels = c("January", "February", "March", "April", "May", "June", "July", 
                                   "August", "September","October", "November", "December"))
head(weather)
ggplot(weather, aes(x = Mean_Temp, y = Month, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "Temp. [F]", option = "C") +
  labs(title = 'Temperatures in Brookings, SD in 2019') +
  xlab("Mean Temperature [F]") +
  ylab("Month") +
  theme(plot.title = element_text(hjust = 0.5))



Exercise 4

library(ggplot2)
p <- ggplot(ggplot(weather, aes(x = precip, y = Month, fill = Month))) +
  geom_boxplot() +
  labs(title = 'Precipitation in Brookings, SD in 2019') +
  xlab("Precipitation (inches)") +
  ylab("Month") +
  theme(plot.title = element_text(hjust = 0.5)) +
  legend("topright", inset=0.01, title="Months", labels, col = fill)
p



Exercise 5

library(ggplot2)
library(plotly)
p <- ggplot(ggplot(weather, aes(x = precip, y = Month, fill = Month))) +
  geom_boxplot() +
  labs(title = 'Precipitation in Brookings, SD in 2019') +
  xlab("Precipitation (inches)") +
  ylab("Month") +
  theme(plot.title = element_text(hjust = 0.5))
    legend("topright", inset=0.01, title="Months", labels, col = fill)
ggplotly(p)



Exercise 6

frostdays <- subset(weather, weather$Low <= 32)
frostdays
warmdays <- subset(weather, weather$High >= 80)
warmdays



Exercise 7

Summer <- subset(weather, weather$Month == "June" | weather$Month == "July" | weather$Month == "August")
Winter <- subset(weather, weather$Month == "January" | weather$Month == "February" | weather$Month == "December")
head(Summer)
head(Winter)
dim(Summer)
dim(Winter)



Exercise 8

mean_summer <- mean(Summer$Precip.)
mean_winter <- mean(Winter$Precip.)

mean_vect <- c(mean_summer,mean_winter)
names(mean_vect) <- c('Average daily precipitation in Summer', 'Average daily preciption in Winter')
mean_vect



Exercise 9

for (i in 1:92){
    Summer$Difference[i] <- Summer$High[i]-Summer$Low[i]
}
Summer$Difference <- Summer$Difference
Summer$Difference <- sort(Summer$Difference, decreasing = TRUE)
Summer$Difference[1:10]
