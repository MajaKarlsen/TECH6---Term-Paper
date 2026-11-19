#EXTENSION 2: Was the gap already there before 1994?
#Change in women and men's high school completion from 1990 and 2000 at the cutoff

library(tidyverse)
library(haven)
library(fixest)

#Load controls
source("data/variables.R")

#Create the results folder if it does not exist
dir.create("results", showWarnings = FALSE)

#Sample and change in high school share 1990-2000
df <- read_dta("regdata0.dta") |>
  filter(!is.na(iwm94), !is.na(hischshr1520f), !is.na(hischshr1520m)) |>
  drop_na(all_of(controls)) |>
  mutate(margin = iwm94, islamic = i94, has90 = as.numeric(!is.na(c90hischshr1520f)),
         change_f = hischshr1520f - c90hischshr1520f, change_m = hischshr1520m - c90hischshr1520m)

nrow(df)        #2629
sum(df$has90)   #1609 with 1990 data

#Keep municipalities with 1990 data
df90 <- filter(df, has90 == 1)

#RD regression
rd <- function(y, h, data, with_controls = TRUE) {
  rhs <- "islamic + margin + margin:islamic"
  if (with_controls) rhs <- paste(rhs, "+", paste(controls, collapse = " + "))
  fit <- feols(as.formula(paste(y, "~", rhs, "| pcode")),
               data = filter(data, abs(margin) < h),
               cluster = ~pcode, fixef.rm = "none", notes = FALSE)
  tibble(outcome = y, h = h, controls = with_controls,
         estimate = coef(fit)[["islamic"]], se = se(fit)[["islamic"]],
         p = pvalue(fit)[["islamic"]], n = nobs(fit))
}

#Set bandwidths for women and men
h_f <- 0.24
h_m <- 0.323

#Levels in 2000 and 1990, and the change
main_result <- bind_rows(
  rd("hischshr1520f", h_f, df90),
  rd("c90hischshr1520f", h_f, df90),
  rd("change_f", h_f, df90),
  rd("hischshr1520m", h_m, df90),
  rd("c90hischshr1520m", h_m, df90),
  rd("change_m", h_m, df90))
main_result

#Change at half and double bandwidth, and without controls
checks <- bind_rows(
  rd("change_f", h_f / 2, df90),
  rd("change_f", h_f * 2, df90),
  rd("change_f", h_f, df90, with_controls = FALSE))

checks

#For comparison, main result on the full sample
rd("hischshr1520f", h_f, df)

#Compare the municipalities with and without 1990 data
df |> group_by(has90) |> summarise(n = n(), log_pop = mean(lpop1994),
                                   women_hs_2000 = mean(hischshr1520f),
                                   district_centre = mean(merkezi))

#Check if having 1990 data jumps at the cutoff
selection <- bind_rows(rd("has90", h_f, df),
                       rd("has90", h_f, df, with_controls = FALSE))

selection

#Plot
plot_data <- df90 |>
  filter(abs(margin) < h_f) |>
  mutate(bin = floor(margin / 0.04) * 0.04 + 0.02)

#Mean change in 4% bins
bins <- plot_data |> group_by(bin) |>
  summarise(change_f = mean(change_f), .groups = "drop")

figure <- ggplot(plot_data, aes(margin, change_f)) +
  geom_smooth(aes(group = islamic), method = "lm", colour = "black", linewidth = 0.6) +
  geom_point(data = bins, aes(bin, change_f), colour = "grey", size = 1.8) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
  labs(title = "Change in women's high school completion, 1990-2000",
       x = "Islamic win margin 1994",
       y = "High school share, 2000 minus 1990") +
  theme_classic(base_size = 10)

figure

#Save the figure to the results folder
ggsave("results/ext2_change.png", figure, width = 6, height = 6, dpi = 300)

#Save results to csv
bind_rows(
  main_result = main_result,
  checks = checks,
  selection = selection, .id = "block"
) |> mutate(across(c(estimate, se, p), \(x) round(x, 4))) |>
  write_csv("results/ext2_results.csv")
