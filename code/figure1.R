#FIGURE 1: Islamic win margin and Islamic vote share in 1994

library(tidyverse)
library(haven)

#Load the data
df <- read_dta("regdata0.dta")
nrow(df)   #3204 municipalities in the data

#Create the results folder if it does not exist
dir.create("results", showWarnings = FALSE)

#Find the names of the vote share and win margin variables
names(df)[grepl("islam|iwm", names(df))]

summary(df$vshr_islam1994)   #Islamic vote share in 1994
summary(df$iwm94)            #Islamic win margin in 1994 = running variable

#Both variables are shares (0-1), so 2 p.p. = 0.02
#544 municipalities have no 1994 election result, so 2660 can be used

#Keep municipalities with a vote share and a win margin
fig1 <- df |>
  filter(!is.na(iwm94), !is.na(vshr_islam1994)) |>
  mutate(close = abs(iwm94) < 0.02)   #Mark close elections (within 2 p.p. of the cutoff)

nrow(fig1)          #2660 municipalities
table(fig1$close)   #101 close elections and 2559 not close

#Plot the win margin against the vote share
fig1_plot <- ggplot(fig1, aes(x = vshr_islam1994, y = iwm94, colour = close)) +
  geom_point(size = 1) +                                                   
  geom_hline(yintercept = 0, colour = "grey60") +                          #Cutoff line at 0
  geom_abline(intercept = -1, slope = 2, colour = "grey60") +              #Two-party line: margin = 2v - 1
  scale_colour_manual(values = c("FALSE" = "grey60", "TRUE" = "black")) +  
  labs(x = "Islamic vote share",
       y = "Islamic win margin") +
  theme_classic() +                                                        
  theme(legend.position = "none")                                          #Remove the legend
fig1_plot

#Save Figure 1 to the results folder
ggsave("results/fig1.png", fig1_plot, width = 7, height = 5, dpi = 300)
