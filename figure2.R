#Figure 2: Density test of Islamic win margin (McCrary 2008 test)

#Packages
library(tidyverse) #data tools (mutate, filter) + ggplot for plots
library(haven)     #reads .dta files
library(rddensity) #density test
library(rdd)       #DCdensity = original McCrary(2008) test -> the same one as in the paper

#Loading the data
df <- read_dta('regdata0.dta')

#Keeping only municipalities with non-missing win margin
fig2 <- df |>
  filter(!is.na(iwm94))

nrow(fig2) #2660 municipalities as in Fig1

#Figure 2 (a): shows histogram of Islamic win margin for full range in 2 p.p wide (0.02)
fig2a <- ggplot(fig2, aes(x=iwm94, y = after_stat(count/sum(count)*100))) +       # y = share of municipalities in each bin
  geom_histogram(binwidth = 0.02, boundary = 0,                                   #boundary is 0 -> a bin edges lies exactly at the cutoff
                 fill ='grey70', colour = 'black', linewidth = 0.2) + 
  geom_vline(xintercept = 0, linetype = 'dashed') +                               #cutoff at 0
  labs(x='Islamic win margin 1994', y = 'Percent')+
  theme_classic()

fig2a

#Figure 2 (b): shows the McCrary (2008) test of whether there is a discontinuity in the density of the Islamic win margin
fig2b <- DCdensity(fig2$iwm94, cutpoint = 0, ext.out = TRUE)    #DCdensity draws a base R-plot (only using running variable)
title(xlab='Islamic win margin 1994', ylab='Density', line = 2) #x and y-axis titles
abline(v=0, lty=2, col='grey50')                                # cutoff at 0 

fig2b$theta   #size of the jump (log difference in density)
fig2b$se      #standard error
fig2b$z       #z-statistic(theta/se)
fig2b$p       #p-value
fig2b$bw      #chosen bandwidth
fig2b$binsize #width of small bins

#Result: theta = -0.095, se = 0.147, z = -0.65, p = 0.52 (bw = 0.165, bin = 0.0086)
#Paper:  theta = -0.049, se = 0.161 -> same conclusion (no manipulation),
#the difference could be due to R vs Stata bandwidth/bin choices

mtext(paste0('Discontinuity est. = ', round(fig2b$theta, 3),', s.e. = ', round(fig2b$se, 3)),
      side = 1, line = 3, adj = 0, cex = 0.6) #discontinuity small note to the left under the plot (rounded to 3 decimal)
