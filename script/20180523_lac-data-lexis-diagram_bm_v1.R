
# data lexis grid for LAC ADRN 2018 ---------------------------------------
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(here)
library(Cairo)

# data range df

year <- rep(1991:2016, each = 20)
year <- as.data.frame(year)
age <- rep(0:19, 26)
age <- as.data.frame(age)
z <- rep(0:19, 26)
z <- as.data.frame(z)
df <- bind_cols(year, age, z) %>% 
  rename(Year = year,
         Age = age)

rm(age, year)


lines <- seq(-1950, -2020 , -5)

lines <- 
  as.tibble(lines) %>% 
  rename(intercept = value) %>% 
  mutate(slope = 1)


# example placement histories
points <- 
  tribble(~child, ~pointx, ~pointy, ~segx, ~segy, ~segxend, ~omitted,
          "one",   1995,    0,    2000,     5,     2012,    FALSE,
          "two",   1997,    0,    2000,     3,     2007,    TRUE,
          "three", 2008,    0,    2008,     0,     2015.5,  FALSE,
          "four",  2012,    0,    2012,     0,     2015.5,  FALSE)

points <- 
  points %>% 
  mutate(segyend = segy + segxend - segx)



# plot axes ---------------------------------------------------------------


axes <- 
  df %>% 
  ggplot(aes(x = Year, y = Age)) +
  coord_fixed(ratio = 1, xlim = c(1990, 2016), ylim = c(0, 20)) +
  geom_abline(data = lines, aes(intercept = intercept, slope = slope), colour = "grey90") +
  geom_hline(yintercept = seq(0, 18, 5), colour = "grey50") +
  geom_vline(xintercept = seq(1990, 2016, 5), colour = "grey50") +
  theme_bw()

ggsave(filename = here("figures", "axes.png"), plot = axes, type = "cairo",
       height = 10, width = 8, device = NULL)
