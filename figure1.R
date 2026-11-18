# Packages
library(tidyverse) #data tools (mutate, filter) + ggplot for plots
library(haven)     #reads .dta files

#Loading the data
df <- read_dta("regdata0.dta")

#Searching for right variables names 
names(df)[grepl("islam|iwm", names(df))]

summary(df$vshr_islam1994) #Islamic vote share in 1994
summary(df$iwm94)          #Islamic win margin in 1994 = running variable

# Both variables are (0-1), so 2 p.p = 0.02
#544 NA => 544 missing value in both => 2660 municipalities can be used

#Making a new dataset with non-missing values
fig1 <- df |>
  filter(!is.na(iwm94), !is.na(vshr_islam1994)) |>
  mutate(close=abs(iwm94) < 0.02) #marking close elections with cutoff=0.02

table(fig1$close) #results => 101 close elections and 2559 non close

#Creating scatter plot with x-axis=vote share and y-axis=win margin
fig1_plot <- ggplot(fig1, aes(x=vshr_islam1994, y=iwm94, colour=close)) + 
  geom_point(size=1) +                                                 #drawing points (one dot per municipality)
  geom_hline(yintercept = 0, colour='grey60') +                        #cutoff line at 0
  geom_abline(intercept = -1, slope = 2, colour = "grey60") +          #two part line: margin = 2v-1
  scale_colour_manual(values = c( "FALSE"="grey60", "TRUE"="black")) + #changing colours close=black and non-close=grey
  labs(x="Islamic vote share", 
       y="Islamic win margin") +
  theme_classic() +                                                    #changing to white background and no grid
  theme(legend.position = "none")                                      #removing the legend
fig1_plot
