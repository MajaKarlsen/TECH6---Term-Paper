#TABLE 2
 
library(tidyverse)
library(haven)
library(fixest)
library(flextable)
 
#Load the data
df <- read_dta("regdata0.dta")
 
dir.create("results", showWarnings = FALSE)
 
#Select variables for table 2
table2_vars <- c(
"hischshr1520f", "hischshr1520m",
"i94", "iwm94",
"vshr_islam1994", "partycount",
"shhs", "lpop1994", "ageshr19",
"ageshr60", "sexr", "merkezi",
"merkezp", "subbuyuk", "buyuk"
)
 
province_vars <- grep("^pd_", names(df), value = TRUE)
 
#Starting with muncipalities that have data for both women and men all the controls
df_table2 <- df[complete.cases(df[ , c(table2_vars,province_vars)]),]
 
# Controls used in every model with covariates
controls <- c(
    "vshr_islam1994", "partycount",
    "shhs", "lpop1994", "ageshr19",
    "ageshr60", "sexr", "merkezi",
    "merkezp", "subbuyuk", "buyuk"
)
 
#vi1 is the election margin. vi1_i lets the slope differ in the
#different sides of the cutoff
linear    <- c("vi1", "vi1_i")
quadratic <- c(linear, "vi2", "vi2_i")
cubic     <- c(quadratic, "vi3", "vi3_i")
 
#Use the bandwidth printet in the paper
bw <- c(
    f = 0.240,          # Women (15-20)
    m = 0.323,          # Men (15-20)
    enroll_f = 0.205,   # Women (15-30)
    enroll_m = 0.230    # Men (15-30)
)
 
run_model <- function(outcome, h, rd_terms = NULL, covariates = TRUE) {
 
    sample <- df_table2 |>
        filter(abs(iwm94) < h)
 
    rhs <- c("i94", rd_terms)
    if (covariates) {
        rhs <- c(rhs, controls)
    }
 
    formula_text <- paste(outcome, "~", paste(rhs, collapse = " + "))
    if (covariates) {
        formula_text <- paste(formula_text, "| province_post")
    }
 
    feols(
        as.formula(formula_text),
        data = sample,
        cluster = ~pcode,
        fixef.rm = "none"
    )
}
 
#Panel A: High school completion for women
female_models <- list(
    #Column 1 compares two mayor groups
    run_model("hischshr1520f", Inf, covariates = FALSE),
 
    #Column 2 adds controls (covariates) and province fixed effects
    run_model("hischshr1520f", Inf),
 
    run_model("hischshr1520f", bw["f"], linear, covariates = FALSE),
    run_model("hischshr1520f", bw["f"], linear),
 
    #Repeat the model with half the bandwidth
    run_model("hischshr1520f", bw["f"] / 2, linear),
 
    #Repeat the model with double the bandwidth
    run_model("hischshr1520f", bw["f"] * 2, linear),
 
    #Keep the original bandwidth, but fit quadratic curve
    run_model("hischshr1520f", bw["f"], quadratic),
 
    #Keep the original bandwidth, but fit cubic curve
    run_model("hischshr1520f", bw["f"], cubic),
 
    #Column 9 switches to school enrollment for ages 15-30,
    #and uses its own bandwidth
    run_model("studshr1530f", bw["enroll_f"], linear)
)
 
#Panel B: Repeat the same nine models for men,
#using the men's bandwidth
male_models <- list(
    run_model("hischshr1520m", Inf, covariates = FALSE),
    run_model("hischshr1520m", Inf),
    run_model("hischshr1520m", bw["m"], linear, covariates = FALSE),
    run_model("hischshr1520m", bw["m"], linear),
    run_model("hischshr1520m", bw["m"] / 2, linear),
    run_model("hischshr1520m", bw["m"] * 2, linear),
    run_model("hischshr1520m", bw["m"], quadratic),
    run_model("hischshr1520m", bw["m"], cubic),
    run_model("studshr1530m", bw["enroll_m"], linear)
)
 
#Panel C: Stack the women's and men's outcomes in one dataset.
#Male is 0 for women's outcomes, and 1 for men's outcomes.
#Each gender keeps its own bandwidth, and every term is interacted
#with male, so women and men get their own slopes, controls and
#fixed effects
run_gender_test <- function(outcome_f, outcome_m, h_f, h_m,
                            rd_terms = NULL, covariates = TRUE) {
 
    women <- df_table2 |> filter(abs(iwm94) < h_f)
    women$outcome <- as.numeric(women[[outcome_f]])
    women$male <- 0
 
    men <- df_table2 |> filter(abs(iwm94) < h_m)
    men$outcome <- as.numeric(men[[outcome_m]])
    men$male <- 1
 
    df_gender <- bind_rows(women, men)
 
    rhs <- rd_terms
    if (covariates) {
        rhs <- c(rhs, controls)
    }
 
    formula_text <- "outcome ~ i94 * male"
    if (length(rhs) > 0) {
        formula_text <- paste0(
            formula_text,
            " + (", paste(rhs, collapse = " + "), ") * male"
        )
    }
    if (covariates) {
        formula_text <- paste(formula_text, "| province_post^male")
    }
 
    feols(
        as.formula(formula_text),
        data = df_gender,
        cluster = ~pcode,
        fixef.rm = "none"
    )
}
 
hs_f <- "hischshr1520f"
hs_m <- "hischshr1520m"
 
panel_c_models <- list(
    run_gender_test(hs_f, hs_m, Inf, Inf, covariates = FALSE),
    run_gender_test(hs_f, hs_m, Inf, Inf),
    run_gender_test(hs_f, hs_m, bw["f"], bw["m"], linear, covariates = FALSE),
    run_gender_test(hs_f, hs_m, bw["f"], bw["m"], linear),
    run_gender_test(hs_f, hs_m, bw["f"] / 2, bw["m"] / 2, linear),
    run_gender_test(hs_f, hs_m, bw["f"] * 2, bw["m"] * 2, linear),
    run_gender_test(hs_f, hs_m, bw["f"], bw["m"], quadratic),
    run_gender_test(hs_f, hs_m, bw["f"], bw["m"], cubic),
    run_gender_test("studshr1530f", "studshr1530m",
                    bw["enroll_f"], bw["enroll_m"], linear)
)
 
get_stars <- function(p) {
    if (p < 0.01) "***"
    else if (p < 0.05) "**"
    else if (p < 0.10) "*"
    else ""
}
 
format_result <- function(model) {
 
    result <- coeftable(model)
 
    estimate <- result["i94", 1]
    se <- result["i94", 2]
    p <- result["i94", 4]
 
    sprintf("%.3f%s (%.3f)",
            estimate, get_stars(p), se)
}
 
#Gets the outcome mean for each column using the muncipalities in that model
#In these OLS models, the average fitted value equals the average observed outcome.
female_means <- sapply(female_models, function(model) mean(fitted(model)))
male_means   <- sapply(male_models, function(model) mean(fitted(model)))
 
female_results <- sapply(female_models, format_result)
male_results <- sapply(male_models, format_result)
 
female_n <- sapply(female_models, nobs)
male_n <- sapply(male_models, nobs)
 
female_r2 <- sapply(female_models, function(m) r2(m, "r2"))
male_r2 <- sapply(male_models, function(m) r2(m, "r2"))
 
# Bandwidths shown in the table (columns 1-2 use the full range, shown as 1)
female_bandwidths <- c(1, 1, bw["f"], bw["f"], bw["f"] / 2, bw["f"] * 2,
                       bw["f"], bw["f"], bw["enroll_f"])
male_bandwidths <- c(1, 1, bw["m"], bw["m"], bw["m"] / 2, bw["m"] * 2,
                     bw["m"], bw["m"], bw["enroll_m"])
 
#i94:male gives the men's Islamic mayor coefficient minus the women's.
#Its p-value tests whether the two coefficients are equal
gender_p <- sapply(
    panel_c_models,
    function(model) coeftable(model)["i94:male", 4]
)
 
table2_final <- data.frame(
    Statistic = c(
        "Panel A: Women",
        "Outcome mean",
        "Islamic mayor in 1994",
        "Bandwidth",
        "R²",
        "Observations",
        "",
        "Panel B: Men",
        "Outcome mean",
        "Islamic mayor in 1994",
        "Bandwidth",
        "R²",
        "Observations",
        "",
        "Panel C: Test of coefficient equality between women and men",
        "p-value"
    ),
    stringsAsFactors = FALSE
)
 
for (i in 1:9) {
    table2_final[[paste0("(", i, ")")]] <- c(
        "",
        sprintf("%.3f", female_means[i]),
        female_results[i],
        sprintf("%.3f", female_bandwidths[i]),
        sprintf("%.3f", female_r2[i]),
        as.character(female_n[i]),
        "",
        "",
        sprintf("%.3f", male_means[i]),
        male_results[i],
        sprintf("%.3f", male_bandwidths[i]),
        sprintf("%.3f", male_r2[i]),
        as.character(male_n[i]),
        "",
        "",
        sprintf("%.3f", gender_p[i])
    )
}
 
#Formatting the table
ft <- flextable(table2_final)
 
ft <- add_header_lines(
    ft,
    values = "TABLE II — ISLAMIC MAYORS AND EDUCATIONAL OUTCOMES"
)
 
ft <- theme_booktabs(ft)
ft <- bold(ft, part = "header")
ft <- fontsize(ft, size = 8, part = "all")
 
# Panel titles in bold
ft <- bold(
    ft,
    i = c(1, 8, 15),
    bold = TRUE,
    part = "body"
)
 
ft <- align(
    ft,
    j = 2:10,
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
    path = "results/table2_formatted.docx"
)
 
write.csv(
    table2_final,
    "results/table2.csv",
    row.names = FALSE
)
