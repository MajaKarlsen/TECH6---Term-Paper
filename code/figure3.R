library(tidyverse)
library(haven)
library(fixest)
library(patchwork)

# Loading variables and helper functions
source("data/variables.R")
source("code/helpers.R")

# panels per covariate, 8% bins
fig3_panels <- imap(fig3_vars, \(label, y)
                    rd_panel(df, y, label, width = 0.08, h_smooth = h_smooth))

# Combining the panels into one figure
fig3 <- wrap_plots(fig3_panels, ncol = 2) +
  plot_annotation(
    title   = "Figure 3: Balanced covariate checks",
    caption = str_wrap(width = 110, paste(
      "Dots: unconditional means in 8% bins of the Islamic win margin in the",
      "1994 mayoral elections. Solid line: local linear smoother on raw data,",
      "each side of zero. Grey lines: 95% CI.")))
fig3

ggsave("results/figure3_covariates.png", fig3, width = 7, height = 9.5, dpi = 300)


