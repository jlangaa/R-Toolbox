## wait-treat plot with thresholds and marginal densities

library(tidyr)
library(ggplot2)
library(ggExtra)

## make some fake data
N.each <- 200
rho <- .4
M1.low <- 25
M1.high<- 35
M2.low <- 28
M2.high<- 38

SD<- 4

d1 <- tibble(
  x1 = rnorm(N.each,M1.low,SD),
  x2 = (rho*x1) + sqrt(1 - rho*rho) * rnorm(n=N.each, M2.low,SD),
  dx = "dx1"
)

d2 <- tibble(
  x1 = rnorm(N.each,M1.high,SD),
  x2 = (rho*x1) + sqrt(1 - rho*rho) * rnorm(n=N.each, M2.high,SD),
  dx = "dx2"
)

d3 <- tibble(
  x1 = rnorm(N.each,M1.low,SD),
  x2 = (rho*x1) + sqrt(1 - rho*rho) * rnorm(n=N.each, M2.high,SD),
  dx = "dx3"
)

dat <- bind_rows(d1,d2,d3)
# cor(dat)

## Set some cutoff values for the lines
# Overwrite these as needed
thresholds <- tibble(
  x1 = c(M1.low+SD),
  x2 = c(M2.low+SD)
)

## Create the plot
p1 <- ggplot(dat, aes(x=x1, y = x2, color = dx, shape=dx)) +
  geom_point() +
  geom_hline(yintercept=thresholds$x2) +
  geom_vline(xintercept=thresholds$x1) +
  theme_bw() +
  labs(title = "Title goes here",subtitle = "Subtitle here",
       x="X axis label",y="Y axis label")

## add marginal densities
p.final <- ggMarginal(p1, groupColour = TRUE, groupFill=TRUE)
p.final
