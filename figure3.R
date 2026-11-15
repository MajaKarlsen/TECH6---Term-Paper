library(tidyverse)
library(haven)
library(fixest)
library(patchwork)

source("data/variables.R")

raw <- read_dta("regdata0.dta")

controls <- c("vshr_islam1994", "partycount", "lpop1994", "ageshr19",
              "ageshr60", "sexr", "merkezi", "merkezp", "subbuyuk", "buyuk")

df <- raw |>
  filter(!is.na(iwm94), !is.na(hischshr1520f), !is.na(hischshr1520m)) |>
  drop_na(all_of(controls)) |>
  mutate(margin  = iwm94,
         islamic = as.numeric(margin > 0))

nrow(df) 
sum(df$islamic)
bin_means <- function(data, y, width) {
  data |>
    filter(!is.na(.data[[y]])) |>
    mutate(bin = floor(margin / width) * width + width / 2) |>
    group_by(bin) |>
    summarise(mean = mean(.data[[y]]), n = n(), .groups = "drop")
}
local_linear <- function(data, y, h_smooth, n_grid = 60) {
  one_side <- function(side_data) {
    grid <- seq(min(side_data$margin), max(side_data$margin), length.out = n_grid)
    map_dfr(grid, function(x0) {
      d <- side_data |>
        mutate(xc = margin - x0,                 
               u  = xc / h_smooth,
               w  = 0.75 * (1 - u^2)) |>        
        filter(abs(u) < 1)
      if (nrow(d) < 10) return(NULL)           
      m <- feols(as.formula(paste(y, "~ xc")), data = d,
                 weights = ~w, vcov = "hetero", notes = FALSE)
      b <- coef(m)[["(Intercept)"]]
      s <- se(m)[["(Intercept)"]]
      tibble(margin = x0, fit = b, lo = b - 1.96 * s, hi = b + 1.96 * s)
    })
  }
  data <- data |> filter(!is.na(.data[[y]]))
  bind_rows(one_side(filter(data, margin <  0)) |> mutate(side = "left"),
            one_side(filter(data, margin >= 0)) |> mutate(side = "right"))
}

rd_panel <- function(data, y, ylab, width, h_smooth, window = 0.32,
                     title = NULL) {
  data <- filter(data, abs(margin) < window)
  dots <- bin_means(data, y, width)
  line <- local_linear(data, y, h_smooth)
  ggplot() +
    geom_line(data = line, aes(margin, lo, group = side), colour = "grey55",
              linewidth = 0.3) +
    geom_line(data = line, aes(margin, hi, group = side), colour = "grey55",
              linewidth = 0.3) +
    geom_line(data = line, aes(margin, fit, group = side), linewidth = 0.6) +
    geom_point(data = dots, aes(bin, mean), colour = "grey45", size = 1.6) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
    scale_x_continuous(breaks = c(-0.25, 0, 0.25)) +
    labs(title = title, x = "Islamic win margin", y = ylab) +
    theme_classic(base_size = 10) +
    theme(plot.title = element_text(size = 10, hjust = 0.5))
}

h_smooth <- 0.15

fig3_panels <- imap(fig3_vars, \(label, y)
                    rd_panel(df, y, label, width = 0.08, h_smooth = h_smooth))

fig3 <- wrap_plots(fig3_panels, ncol = 2) +
  plot_annotation(
    title   = "Figure 3: Balanced covariate checks",
    caption = str_wrap(width = 110, paste(
      "Dots: unconditional means in 8% bins of the Islamic win margin in the",
      "1994 mayoral elections. Solid line: local linear smoother on raw data,",
      "each side of zero. Grey lines: 95% CI.")))
fig3
ggsave("figure3_covariates.png", fig3, width = 7, height = 9.5, dpi = 300)


