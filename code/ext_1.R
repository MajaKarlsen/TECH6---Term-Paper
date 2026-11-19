#EXTENSION 1: Did more schooling result in more jobs?
#Same specification as in table2.R

library(tidyverse)
library(haven)
library(fixest)
library(flextable)

#Load the data
df <- read_dta("regdata0.dta")
nrow(df)   #3204 municipalities in the data

#Create the results folder if it does not exist
dir.create("results", showWarnings = FALSE)

#Use the same controls as table2.R
controls <- c(
  "vshr_islam1994", "partycount",
  "shhs", "lpop1994", "ageshr19",
  "ageshr60", "sexr", "merkezi",
  "merkezp", "subbuyuk", "buyuk"
)

#Check the new outcomes (0-1 shares?, how many are missing?)
summary(df$nonagshr1530f)
summary(df$hwfshr1530f)
summary(df$c90empf)       #not used: mostly zeros, 1332 missing

#Use a bandwidth of 0.240 so the result is comparable with the main education estimate
h <- 0.240

rd_sample <- df |>
  filter(abs(iwm94) < h)  #Keep only municipalities narrowly won or lost by less than 24 p.p.

nrow(rd_sample)   #1046 municipalities within the bandwidth

#First RDD: did Islamic rule change women's non-agricultural work?
#nonagshr1530f - share of women 15-30 working outside of agriculture
m_nonag_f <- feols(
  nonagshr1530f ~ i94 + vi1 + vi1_i +                          #i94 = RD effect; vi1, vi1_i = margin slope on each side
    vshr_islam1994 + partycount + shhs + lpop1994 + ageshr19 +
    ageshr60 + sexr + merkezi + merkezp + subbuyuk + buyuk |
    province_post,
  data = rd_sample,
  cluster = ~pcode,
  fixef.rm = "none"                                            #keep provinces with one municipality, as in table2.R
)

summary(m_nonag_f)

#Second RDD: did Islamic rule change men's non-agricultural work?
#Same RDD for men, same sample and bandwidth
m_nonag_m <- feols(
  nonagshr1530m ~ i94 + vi1 + vi1_i +                         #outcome: men 15-30 working outside agriculture
    vshr_islam1994 + partycount + shhs + lpop1994 + ageshr19 +
    ageshr60 + sexr + merkezi + merkezp + subbuyuk + buyuk |
    province_post,
  data = rd_sample,
  cluster = ~pcode,
  fixef.rm = "none"
)

#Show women and men side by side
etable(m_nonag_f, m_nonag_m, keep = "i94")

#Means of the outcomes in the RD sample (to judge the size of the effects)
mean(rd_sample$nonagshr1530f, na.rm = TRUE)
mean(rd_sample$nonagshr1530m, na.rm = TRUE)

#Result (bw = 0.240):
#women: i94 =  0.0025 (se 0.0037), not significant, mean = 0.076
#men:   i94 = -0.0243 (se 0.0113), significant at 5%, mean = 0.365
#=> no effect for women and negative effect for men (needs a robustness check)

#Robustness check: testing if the results hold at bandwidth 0.12 (half), 0.24 (main) and 0.48 (double)

#Use a function so the same specification is used for every outcome and bandwidth
run_rd <- function(outcome, h) {
  #Build the formula as text: outcome ~ i94 + vi1 + vi1_i + controls| province FE
  formula_text <- paste(outcome, "~ i94 + vi1 + vi1_i +",
                        paste(controls, collapse = " + "), "| province_post")
  feols(
    as.formula(formula_text),
    data = df |> filter(abs(iwm94) < h),
    cluster = ~pcode,
    fixef.rm = "none")
}

#Women: half, main and double bandwidth
etable(run_rd("nonagshr1530f", 0.12),
       run_rd("nonagshr1530f", 0.24),
       run_rd("nonagshr1530f", 0.48),
       keep = "i94")

#Men: half, main and double bandwidth
etable(run_rd("nonagshr1530m", 0.12),
       run_rd("nonagshr1530m", 0.24),
       run_rd("nonagshr1530m", 0.48),
       keep = "i94")

#Result: women ~0 at all bandwidths (robust null).
#Men: negative at all bandwidths (-0.013, -0.024**, -0.020**), but not significant at half the bandwidth

#Check where the change for women happened: students, housewives and employed (ages 15-30)
#Table II (col. 9) and Table S.IV report these separately at one bandwidth; here we compare them

#Students
etable(run_rd("studshr1530f", 0.12),
       run_rd("studshr1530f", 0.24),
       run_rd("studshr1530f", 0.48),
       keep = "i94")

#Housewives
etable(run_rd("hwfshr1530f", 0.12),
       run_rd("hwfshr1530f", 0.24),
       run_rd("hwfshr1530f", 0.48),
       keep = "i94")

#Result: student = robust increase, housewife = not robust (only significant at 0.24 bw), employed = robust zero
#=> women moved into school, but not (yet) into work

#Build one results table
bandwidths <- c(0.12, 0.24, 0.48)   #half, main and double bandwidth

#Stars follow the paper's footnote (* 10%, ** 5%, *** 1%)
get_stars <- function(p) {
  if (p < 0.01) "***"
  else if (p < 0.05) "**"
  else if (p < 0.10) "*"
  else ""
}

#Format the i94 result as "estimate*** (s.e.)"
format_result <- function(model) {
  result <- coeftable(model)
  sprintf("%.3f%s (%.3f)",
          result["i94", 1], get_stars(result["i94", 4]), result["i94", 2])
}

#The rows of one panel for one model: mean, estimate, bandwidth, R2, N
panel_rows <- function(model, h) {
  c(sprintf("%.3f", mean(fitted(model))),   #outcome mean
    format_result(model),
    sprintf("%.3f", h),
    sprintf("%.3f", r2(model, "r2")),
    as.character(nobs(model)))
}

#The five outcomes in the table: panel title = variable name
outcomes <- c(
  "Panel A: Women, students"                    = "studshr1530f",
  "Panel B: Women, housewives"                  = "hwfshr1530f",
  "Panel C: Women, non-agricultural employment" = "nonagshr1530f",
  "Panel D: Men, students"                      = "studshr1530m",
  "Panel E: Men, non-agricultural employment"   = "nonagshr1530m"
)

row_labels <- c("Outcome mean", "Islamic mayor in 1994",
                "Bandwidth", "R²", "Observations")

#First column: for each panel -> title, the 5 row labels, an empty row
ext_final <- data.frame(
  Statistic = unlist(lapply(names(outcomes), function(title) c(title, row_labels, ""))),
  stringsAsFactors = FALSE
)

#One column per bandwidth: (1) half, (2) main, (3) double
for (i in 1:3) {
  ext_final[[paste0("(", i, ")")]] <- unlist(lapply(outcomes, function(y) {
    model <- run_rd(y, bandwidths[i])
    c("", panel_rows(model, bandwidths[i]), "")         #title row empty, 5 results, empty row
  }))
}

ext_final   #check in the Console: 35 rows, 4 columns

#Format the table for Word
ft <- flextable(ext_final)
ft <- add_header_lines(ft,
                       values = "TABLE E.1 — ISLAMIC MAYORS AND THE ACTIVITIES OF WOMEN AND MEN AGED 15-30, 2000")
ft <- theme_booktabs(ft)
ft <- bold(ft, part = "header")
ft <- fontsize(ft, size = 8, part = "all")
ft <- bold(ft, i = c(1, 8, 15, 22, 29), bold = TRUE, part = "body")
ft <- align(ft, j = 2:4, align = "center", part = "all")
ft <- align(ft, j = 1, align = "left", part = "all")
ft <- add_footer_lines(ft, values = c(
  "Columns (1)-(3): half, main and double bandwidth (0.12, 0.24, 0.48).",
  "Local linear RD with the controls of Table II and province fixed effects.",
  "Standard errors clustered by province in parentheses.",
  "* p < 0.10, ** p < 0.05, *** p < 0.01."))
ft <- fontsize(ft, size = 8, part = "footer")
ft <- padding(ft, padding.top = 1, padding.bottom = 1, part = "all")
ft <- line_spacing(ft, space = 1, part = "all")
ft <- autofit(ft)

#Export the results
save_as_docx(ft, path = "results/ext1_formatted.docx")
write.csv(ext_final, "results/ext1.csv", row.names = FALSE)
