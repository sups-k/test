library(dslabs)
library(dplyr)
library(ggplot2)

murders |> ggplot()
# this is the equivalent of ggplot(data = murders)

p <- ggplot(data = murders)
# p <- murders |> ggplot()

p + geom_point(aes(x = population/10^6, y = total)) +
# Non-standard evaluation - population is not a variable but there is no error because it knows that population is murders$population
geom_text(aes(population/10^6, total, label = abb))

# Global aesthetic mappings - put aes in ggplot
murders |> ggplot(aes(population/10^6, total, label = abb)) +
  geom_point(aes(color = region), size = 3) +
  geom_text()
# will get warning because geom_point does not use "label"
# aes is for the data
# You can keep saving each layer as another p
# library(themepark) on git
# library(gridExtra) helps put graphs in a grid
# Choose log 10 or log 2 for plotting not ln
# geom_roster()