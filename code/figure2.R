#FIGURE 2: Density of the Islamic win margin (McCrary 2008 test)

library(tidyverse)   
library(haven)       
library(rdd)         

#Load the data
df <- read_dta("regdata0.dta")
nrow(df)   #3204 municipalities in the data

#Create the results folder if it does not exist
dir.create("results", showWarnings = FALSE)

#Keep only municipalities with a win margin
fig2 <- df |>
  filter(!is.na(iwm94))

nrow(fig2)   #2660 municipalities, as in Figure 1

#Figure 2(a): histogram of the Islamic win margin, 2 p.p. bins (0.02)
fig2a <- ggplot(fig2, aes(x = iwm94, y = after_stat(count / sum(count) * 100))) +   #y = percent of municipalities in each bin
  geom_histogram(binwidth = 0.02, boundary = 0,                                     #boundary = 0, so a bin edge lies exactly at the cutoff
                 fill = "grey70", colour = "black", linewidth = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +                                 #cutoff at 0
  labs(x = "Islamic win margin 1994", y = "Percent") +
  theme_classic()
fig2a

#Save Figure 2(a) 
ggsave("results/fig2a.png", fig2a, width = 7, height = 5, dpi = 300)

#Figure 2(b): McCrary (2008) test of a discontinuity in the density of the win margin
#DCdensity draws a base-R plot (not ggplot), so ggsave does not work here
png("results/fig2b.png", width = 7, height = 5, units = "in", res = 300)   #open the png file
fig2b <- DCdensity(fig2$iwm94, cutpoint = 0, ext.out = TRUE)               #run the test and draw the plot (only uses the running variable)
title(xlab = "Islamic win margin 1994", ylab = "Density", line = 2)        
abline(v = 0, lty = 2, col = "grey50")                                     #cutoff at 0
mtext(paste0("Discontinuity est. = ", round(fig2b$theta, 3), ", s.e. = ", round(fig2b$se, 3)),
      side = 1, line = 3, adj = 0, cex = 0.6)                              
dev.off()                                                                  #close and save the file

#Test results
fig2b$theta     #size of the jump (log difference in density)
fig2b$se        #standard error
fig2b$z         #z-statistic (theta/se)
fig2b$p         #p-value
fig2b$bw        #chosen bandwidth
fig2b$binsize   #width of the small bins

#Result: theta = -0.095, se = 0.147, z = -0.65, p = 0.52 (bw = 0.165, bin = 0.0086)
#Paper:  theta = -0.049, se = 0.161 -> same conclusion (no manipulation)
#The difference is likely due to R vs Stata bandwidth and bin choices

