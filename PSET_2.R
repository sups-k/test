library(dslabs)
library(tidyverse)
library(ggplot2)

avg <- us_contagious_diseases %>%
  filter(disease == "Measles" & !state %in% c("Alaska", "Hawaii") &
           weeks_reporting > 0) %>%
  group_by(year) %>%
  summarize(rate = sum(count/weeks_reporting*52, na.rm = TRUE)/sum(population)*10000)


avg |> 
  ggplot(aes(year, rate)) +
  geom_line() +
  labs(x = "Year", y = "Rate", title = "Incidence of Measles in The US") +
  theme_bw() +
  geom_vline(aes(xintercept = 1963, color = "pink"), show.legend = FALSE)




us_contagious_diseases %>%
  filter(disease == "Measles" & !state %in% c("Alaska", "Hawaii") &
           weeks_reporting > 0) %>%
  mutate(us_rate = count / population * 10000 * 52 / weeks_reporting) %>%
  ggplot() +
  geom_line(aes(year, us_rate, group = state),  color = "grey50", 
            show.legend = FALSE, alpha = 0.2, linewidth = 1) +
  geom_line(mapping = aes(year, rate),  data = avg, linewidth = 1) +
  scale_y_continuous(trans = "sqrt", breaks = c(5, 25, 125, 300)) + 
  ggtitle("Incidence of Measles in The US") + 
  xlab("Year") + ylab("Cases per 10,000 by state") +
  geom_text(data = data.frame(x = 1955, y = 50), 
            mapping = aes(x, y, label = "US average"), 
            color = "black") + 
  geom_vline(aes(xintercept = 1963, color = "pink"), show.legend = FALSE)



## use this color pallete
reds <- RColorBrewer::brewer.pal(9, "Reds")

us_contagious_diseases %>%
  filter(disease == "Measles" & weeks_reporting > 0) %>%
  mutate(us_rate = count / population * 10000 * 52 / weeks_reporting) %>%
  mutate(state = reorder(state, ifelse(year <= 1963, us_rate, NA), 
                         median, na.rm = TRUE)) %>%
  ggplot(aes(year, state, fill = us_rate)) +
  geom_tile(color = "grey50") +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_gradientn(colors = reds, trans = "sqrt") +
  geom_vline(xintercept = 1963, col = "blue") +
  theme_minimal() +  
  theme(panel.grid = element_blank(), 
        legend.position = "bottom", 
        text = element_text(size = 8)) +
  labs(title = "Incidence of Measles in The US", x = "Year", y = "State")
