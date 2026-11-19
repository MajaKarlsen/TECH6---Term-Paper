#TABLE 7: RD treatment effects of Islamic rule on education in 2011

library(tidyverse)
library(haven)
library(fixest)
library(flextable)

#Load the data
df <- read_dta("regdata0.dta")
nrow(df)                      #3204 municipalities in the data
dir.create("results", showWarnings = FALSE)

#Each outcome becomes one column in Table 7
#The first four are high school completion shares and the last two are unmarried shares
outcomes <- c(
  "ahischshr15_19f",
  "ahischshr15_19m",
  "ahischshr15_29f",
  "ahischshr15_29m",
  "nevm15_19f",
  "nevm15_19m"
)

#Controls used in every model with covariates
controls <- c(
  "vshr_islam1994", "partycount",
  "shhs", "lpop1994", "ageshr19",
  "ageshr60", "sexr", "merkezi",
  "merkezp", "subbuyuk", "buyuk"
)

province_vars <- grep("^pd_", names(df), value = TRUE)

#PANEL A: global OLS on the full sample, without and with controls
table7_panelA <- list()

for (outcome in outcomes) {
  #Choose the municipalities separately for each outcome
  #Use the same municipalities for the models with and without controls
  ols_vars <- c(outcome, "i94", controls, province_vars, "pcode")
  ols_sample <- df[complete.cases(df[, ols_vars]), ]
  print(paste(outcome, "OLS sample:", nrow(ols_sample)))   #2338 for each 2011 outcome
  
  #Model 1: no controls
  model_unconditional <- feols(
    as.formula(paste(outcome, "~ i94")),
    data = ols_sample,
    cluster = ~pcode
  )
  #Model 2: controls and province fixed effects
  formula_conditional <- as.formula(
    paste(
      outcome, "~ i94 +",
      paste(controls, collapse = " + "),
      "| province_post"
    )
  )
  model_conditional <- feols(
    formula_conditional,
    data = ols_sample,
    cluster = ~pcode,
    fixef.rm = "none"
  )
  
  table7_panelA[[paste0(outcome, "_unconditional")]] <-
    model_unconditional
  
  table7_panelA[[paste0(outcome, "_conditional")]] <-
    model_conditional
}

#The bandwidths printed in the paper for each outcome
optimal_bandwidths <- c(
  ahischshr15_19f = 0.239,
  ahischshr15_19m = 0.199,
  ahischshr15_29f = 0.263,
  ahischshr15_29m = 0.227,
  nevm15_19f = 0.280,
  nevm15_19m = 0.271
)
#Panel B: local linear RD at different bandwidth
table7_panelB <- list()

for (outcome in outcomes) {
  
  h <- optimal_bandwidths[[outcome]]
  
  rd_vars <- c(outcome, "i94", "iwm94", "vi1", "vi1_i",
               controls, province_vars, "pcode")
  rd_sample <- df[complete.cases(df[, rd_vars]), ]
  print(paste(outcome, "RD sample before the bandwidth:", nrow(rd_sample)))   #2338, the same as the OLS sample
  
  bandwidth_options <- c(
    Optimal = h,
    Half = h / 2,
    Double = h * 2
  )
  
  #Run the same local linear model three times for this outcome
  #once at the main bandwidth, once at half and once at double
  for (spec in names(bandwidth_options)) {
    
    bw <- bandwidth_options[[spec]]
    
    sample <- rd_sample |>
      filter(abs(iwm94) < bw)
    
    #vi1 is the election margin.
    #vi1_i lets the slope differ on each side of zero.
    formula_rd <- as.formula(
      paste(
        outcome, "~ i94 + vi1 + vi1_i +",
        paste(controls, collapse = " + "),
        "| province_post"
      )
    )
    
    model <- feols(
      formula_rd,
      data = sample,
      cluster = ~pcode,
      fixef.rm = "none"
    )
    
    model_name <- paste(outcome, spec, sep = "_")
    table7_panelB[[model_name]] <- model
  }
}

get_stars <- function(p) {
  if (p < 0.01) "***"
  else if (p < 0.05) "**"
  else if (p < 0.10) "*"
  else ""
}
#Returns the i94 estimate as "estimate*** (s.e.)"
format_result <- function(model) {
  
  result <- coeftable(model)
  
  estimate <- result["i94", 1]
  se <- result["i94", 2]
  p <- result["i94", 4]
  
  sprintf("%.3f%s (%.3f)",
          estimate, get_stars(p), se)
}
#Build the first column with the row labels
table7_final <- data.frame(
  Statistic = c(
    "Panel A: Global OLS",
    "Outcome mean",
    "Model 1. Unconditional estimates",
    "Islamic mayor in 1994",
    "Model 2. Conditional estimates",
    "Islamic mayor in 1994",
    "Bandwidth",
    "Observations",
    "",
    "Panel B: Conditional local linear RD",
    "Outcome mean",
    "Model 1. Optimal bandwidth",
    "Islamic mayor in 1994",
    "Bandwidth",
    "Observations",
    "",
    "Model 2. Half bandwidth",
    "Islamic mayor in 1994",
    "Bandwidth",
    "Observations",
    "",
    "Model 3. Double bandwidth",
    "Islamic mayor in 1994",
    "Bandwidth",
    "Observations"
  ),
  stringsAsFactors = FALSE
)
#Column names, in the same order as outcome
outcome_labels <- c(
  "Women high school 15-19",
  "Men high school 15-19",
  "Women high school 15-29",
  "Men high school 15-29",
  "Women unmarried 15-19",
  "Men unmarried 15-19"
)

for (i in seq_along(outcomes)) {
  
  outcome <- outcomes[i]
  
  model_uncond <- table7_panelA[[
    paste0(outcome, "_unconditional")
  ]]
  
  model_cond <- table7_panelA[[
    paste0(outcome, "_conditional")
  ]]
  
  model_opt <- table7_panelB[[
    paste(outcome, "Optimal", sep = "_")
  ]]
  
  model_half <- table7_panelB[[
    paste(outcome, "Half", sep = "_")
  ]]
  
  model_double <- table7_panelB[[
    paste(outcome, "Double", sep = "_")
  ]]
  
  h <- optimal_bandwidths[[outcome]]
  
  outcome_mean <- mean(fitted(model_cond))
  
  table7_final[[outcome_labels[i]]] <- c(
    "",
    sprintf("%.3f", outcome_mean),
    "",
    format_result(model_uncond),
    "",
    format_result(model_cond),
    "1.00",
    as.character(nobs(model_cond)),
    "",
    
    "",
    sprintf("%.3f", mean(fitted(model_opt))),
    "",
    format_result(model_opt),
    sprintf("%.3f", h),
    as.character(nobs(model_opt)),
    "",
    
    "",
    format_result(model_half),
    sprintf("%.3f", h / 2),
    as.character(nobs(model_half)),
    "",
    
    "",
    format_result(model_double),
    sprintf("%.3f", h * 2),
    as.character(nobs(model_double))
  )
}

#Format the table
ft <- flextable(table7_final)

ft <- add_header_lines(
  ft,
  values = "TABLE VII — RD TREATMENT EFFECTS OF ISLAMIC RULE ON EDUCATION IN 2011"
)

ft <- theme_booktabs(ft)

ft <- bold(ft, part = "header")

ft <- fontsize(ft, size = 8, part = "all")

ft <- bold(
  ft,
  i = c(1, 10),
  bold = TRUE,
  part = "body"
)

ft <- align(
  ft,
  j = 2:7,
  align = "center",
  part = "all"
)

ft <- align(
  ft,
  j = 1,
  align = "left",
  part = "all"
)

ft <- add_footer_lines(
  ft,
  values = paste(
    "* p < 0.10, ** p < 0.05, *** p < 0.01."
  )
)

ft <- fontsize(ft, size = 8, part = "footer")

ft <- autofit(ft)

#Export the results
save_as_docx(
  ft,
  path = "results/table7_formatted.docx"
)

write.csv(
  table7_final,
  "results/table7.csv",
  row.names = FALSE
)