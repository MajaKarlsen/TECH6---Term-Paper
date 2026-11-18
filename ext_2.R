library(tidyverse)
library(haven)
library(fixest)

controls <- c("vshr_islam1994", "partycount", "lpop1994", 
              "ageshr19", "ageshr60", "sexr", "merkezi", "merkezp", "subbuyuk", "buyuk")

df <- read_dta("regdata0.dta") |>
  filter(!is.na(iwm94), !is.na(hischshr1520f), !is.na(hischshr1520m)) |>
  drop_na(all_of(controls)) |>
  mutate(margin = iwm94, islamic = i94, has90 = as.numeric(!is.na(c90hischshr1520f)),
         change_f = hischshr1520f - c90hischshr1520f, change_m = hischshr1520m - c90hischshr1520m)

nrow(df)
sum(df$has90)
df90 <- filter(df, has90 ==1)

# RD regression

rd <- function(y, h, data, with_controls = TRUE) {
  rhs <- "islamic + margin + margin:islamic"
  if(with_controls) rhs <- paste(rhs, "+", paste(controls, collapse = " + "))
  fit <- feols(as.formula(paste(y, "~", rhs, "| pcode")),
               data = filter(data, abs(margin) < h),
               cluster = ~pcode, fixef.rm = "none", notes = FALSE)
  tibble(outcome = y, h = h, controls = with_controls,
         estimate = coef(fit)[["islamic"]], se = se(fit)[["islamic"]],
         p = pvalue(fit)[["islamic"]], n = nobs(fit))
}

h_f <- 0.24
h_m <- 0.323

main_result <- bind_rows(
  rd("hischshr1520f", h_f, df90),
  rd("c90hischshr1520f",h_f, df90),
  rd("change_f", h_f, df90),
  rd("hischshr1520m", h_m, df90),
  rd("c90hischshr1520m", h_m, df90),
  rd("change_m", h_m, df90))
main_result

checks <- bind_rows(
  rd("change_f", h_f/2, df90),
  rd("change_f", h_f*2, df90),
  rd("change_f", h_f, df90, with_controls=FALSE))

checks

rd("hischshr1520f", h_f, df)

df |> group_by(has90) |> summarise(n = n(), log_pop = mean(lpop1994),
                                   women_hs_2000 = mean(hischshr1520f),
                                   district_centre = mean(merkezi))

selection <- bind_rows(rd("has90", h_f, df),
                       rd("has90", h_f, df, with_controls=FALSE))

selection

# Plot

plot_data <- df90 |>
  filter(abs(margin) < h_f) |>
  mutate(bin=floor(margin/0.04) * 0.04 + 0.02)

bins <- plot_data |> group_by(bin) |>
  summarise(change_f = mean(change_f), .groups = "drop")

figure <- ggplot(plot_data, aes(margin, change_f)) +
  geom_smooth(aes(group = islamic), method = "lm", colour = "black", linewidth = 0.6) +
  geom_point(data = bins, aes(bin, change_f), colour = "gray", size = 1.8) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "gray40") +
  labs(title = "Change in women's high school completion, 1990-2000",
       x = "Islamic win margin 1994",
       y = "High school chare, 2000 minus 1990") +
  theme_classic(base_size = 10)

figure
ggsave("ext2_change.png", figure, width=6, height=6, dpi=300)  

bind_rows(
  main_result = main_result,
  checks = checks,
  selection = selection, .id = "block"
) |> mutate(across(c(estimate, se, p), \(x) round(x, 4))) |>
  write_csv("ext2_results.csv")
