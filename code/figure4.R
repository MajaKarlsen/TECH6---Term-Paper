library(tidyverse)
library(haven)
library(fixest)
library(patchwork)

source("data/variables.R")
source("code/helpers.R")

fig4_panels <- imap(fig4_vars, \(label, y)
                    rd_panel(df, y, ylab = "Share with high school",
                             width = 0.04, h_smooth = h_smooth, title = label))

fig4 <- wrap_plots(fig4_panels, ncol = 2) +
  plot_annotation(
    title   = "Figure 4: High school attainment and the Islamic win margin",
    caption = str_wrap(width = 110, paste(
      "Dots: unconditional means in 4% bins of the Islamic win margin in the",
      "1994 mayoral elections. Solid line: local linear smoother on raw data,",
      "each side of zero. Grey lines: 95% CI.")))
fig4
ggsave("results/figure4_outcomes.png", fig4, width = 7, height = 6, dpi = 300)

rd_jump <- function(y, h, with_controls = TRUE) {
  rhs <- "islamic + margin + margin:islamic"
  if (with_controls) rhs <- paste(rhs, "+", paste(setdiff(controls, y), collapse = " + "))
  fit <- feols(as.formula(paste(y, "~", rhs, "| pcode")),
               data = filter(df, abs(margin) < h), cluster = ~pcode,
               notes = FALSE)
  tibble(variable = y, h = h, jump = coef(fit)["islamic"],
         se = se(fit)["islamic"], p = pvalue(fit)["islamic"], n = nobs(fit))
}

bind_rows(rd_jump("hischshr1520f",    0.240),
          rd_jump("hischshr1520m",    0.323),
          rd_jump("c90hischshr1520f", 0.240),
          rd_jump("c90hischshr1520m", 0.323))

map_dfr(names(fig3_vars), \(y) rd_jump(y, 0.240, with_controls = FALSE))

