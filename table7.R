library(haven)
library(dplyr)
library(fixest)
library(flextable)

df <- read_dta("regdata0.dta")

outcomes <- c(
    "ahischshr15_19f",
    "ahischshr15_19m",
    "ahischshr15_29f",
    "ahischshr15_29m",
    "nevm15_19f",
    "nevm15_19m"
)

controls <- c(
    "vshr_islam1994", "partycount",
    "shhs", "lpop1994", "ageshr19",
    "ageshr60", "sexr", "merkezi",
    "merkezp", "subbuyuk", "buyuk"
)

province_vars <- grep("^pd_", names(df), value = TRUE)

table7_vars <- c(
    outcomes, "i94", "iwm94",
    "vi1", "vi1_i", controls,
    province_vars
)

df_table7 <- df[
    complete.cases(df[, table7_vars]),
]


model_t7_1_unconditional <- feols(
    ahischshr15_19f ~ i94,
    data = df_table7,
    cluster = ~pcode
)

model_t7_1_conditional <- feols(
    ahischshr15_19f ~ i94 +
        vshr_islam1994 + partycount +
        shhs + lpop1994 + ageshr19 + ageshr60 +
        sexr + merkezi + merkezp + subbuyuk + buyuk |
        province_post,
    data = df_table7,
    cluster = ~pcode,
    fixef.rm = "none"
)


table7_panelA <- list()

for (outcome in outcomes) {

    model_unconditional <- feols(
        as.formula(paste(outcome, "~ i94")),
        data = df_table7,
        cluster = ~pcode
    )

    formula_conditional <- as.formula(
        paste(
            outcome, "~ i94 +",
            paste(controls, collapse = " + "),
            "| province_post"
        )
    )

    model_conditional <- feols(
        formula_conditional,
        data = df_table7,
        cluster = ~pcode,
        fixef.rm = "none"
    )

    table7_panelA[[paste0(outcome, "_unconditional")]] <-
        model_unconditional

    table7_panelA[[paste0(outcome, "_conditional")]] <-
        model_conditional
}

panelA_results <- data.frame()

for (outcome in outcomes) {
    for (spec in c("unconditional", "conditional")) {

        model <- table7_panelA[[paste0(outcome, "_", spec)]]
        result <- coeftable(model)

        panelA_results <- rbind(
            panelA_results,
            data.frame(
                Outcome = outcome,
                Specification = spec,
                Coefficient = result["i94", 1],
                SE = result["i94", 2],
                Observations = nobs(model)
            )
        )
    }
}



optimal_bandwidths <- c(
    ahischshr15_19f = 0.239,
    ahischshr15_19m = 0.199,
    ahischshr15_29f = 0.263,
    ahischshr15_29m = 0.227,
    nevm15_19f = 0.280,
    nevm15_19m = 0.271
)

table7_panelB <- list()

panelB_results <- data.frame()

for (outcome in outcomes) {

    h <- optimal_bandwidths[[outcome]]

   
    bandwidth_options <- c(
        Optimal = h,
        Half = h / 2,
        Double = h * 2
    )

    for (spec in names(bandwidth_options)) {

        bw <- bandwidth_options[[spec]]

        sample <- df_table7 |>
            filter(abs(iwm94) < bw)

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

        result <- coeftable(model)

        panelB_results <- rbind(
            panelB_results,
            data.frame(
                Outcome = outcome,
                Specification = spec,
                Bandwidth = bw,
                Coefficient = result["i94", 1],
                SE = result["i94", 2],
                P_value = result["i94", 4],
                Observations = nobs(model)
            )
        )
    }
}


# --------------------------------------------------
# TABLE VII: FORMAT AND EXPORT RESULTS
# --------------------------------------------------

# Significance levels used in Meyersson (2014)
get_stars <- function(p) {
    if (p < 0.01) "***"
    else if (p < 0.05) "**"
    else if (p < 0.10) "*"
    else ""
}

# Extract coefficient, standard error and significance
format_result <- function(model) {

    result <- coeftable(model)

    estimate <- result["i94", 1]
    se <- result["i94", 2]
    p <- result["i94", 4]

    sprintf("%.3f%s (%.3f)",
            estimate, get_stars(p), se)
}


# Create Table VII
table7_final <- data.frame(
    Statistic = c(
        "Panel A: Global OLS",
        "Outcome mean",
        "Unconditional OLS",
        "Conditional OLS",
        "R² (conditional)",
        "Observations",
        "",
        "Panel B: Local linear RD",
        "Optimal bandwidth",
        "Islamic mayor (optimal)",
        "R² (optimal)",
        "Observations (optimal)",
        "",
        "Half bandwidth",
        "Islamic mayor (half)",
        "Observations (half)",
        "",
        "Double bandwidth",
        "Islamic mayor (double)",
        "Observations (double)"
    ),
    stringsAsFactors = FALSE
)


# Labels for the six outcome columns
outcome_labels <- c(
    "Women HS 15-19",
    "Men HS 15-19",
    "Women HS 15-29",
    "Men HS 15-29",
    "Women unmarried 15-19",
    "Men unmarried 15-19"
)


# Fill the table with results
for (i in seq_along(outcomes)) {

    outcome <- outcomes[i]

    # Panel A models
    model_uncond <- table7_panelA[[
        paste0(outcome, "_unconditional")
    ]]

    model_cond <- table7_panelA[[
        paste0(outcome, "_conditional")
    ]]

    # Panel B models
    model_opt <- table7_panelB[[
        paste(outcome, "Optimal", sep = "_")
    ]]

    model_half <- table7_panelB[[
        paste(outcome, "Half", sep = "_")
    ]]

    model_double <- table7_panelB[[
        paste(outcome, "Double", sep = "_")
    ]]

    # Bandwidths
    h <- optimal_bandwidths[[outcome]]

    # Outcome mean in the full sample
    outcome_mean <- mean(
        df_table7[[outcome]],
        na.rm = TRUE
    )

    # Add the results to the corresponding column
    table7_final[[outcome_labels[i]]] <- c(
        "",
        sprintf("%.3f", outcome_mean),
        format_result(model_uncond),
        format_result(model_cond),
        sprintf("%.3f", as.numeric(fitstat(model_cond, "r2")$r2)),
        as.character(nobs(model_cond)),
        "",
        "",
        sprintf("%.3f", h),
        format_result(model_opt),
        sprintf("%.3f", as.numeric(fitstat(model_opt, "r2")$r2)),
        as.character(nobs(model_opt)),
        "",
        sprintf("%.4f", h / 2),
        format_result(model_half),
        as.character(nobs(model_half)),
        "",
        sprintf("%.3f", h * 2),
        format_result(model_double),
        as.character(nobs(model_double))
    )
}


# Create results folder
dir.create("results", showWarnings = FALSE)

# Export CSV
write.csv(
    table7_final,
    "results/table7.csv",
    row.names = FALSE
)

ft <- flextable(table7_final)

ft <- add_header_lines(
    ft,
    values = "RD TREATMENT EFFECTS OF ISLAMIC RULE ON EDUCATION IN 2011"
)

ft <- theme_booktabs(ft)

ft <- bold(ft, part = "header")

ft <- fontsize(ft, size = 8, part = "all")

ft <- bold(
    ft,
    i = c(1, 8),
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
        "Panel A reports unconditional and conditional OLS results.",
        "Panel B reports local linear RD estimates using the optimal,",
        "half and double bandwidths.",
        "All conditional regressions include municipal controls",
        "and province fixed effects.",
        "Standard errors are clustered by province.",
        "Bandwidths use the rounded published values;",
        "this may cause small differences in observation counts.",
        "R² is the unadjusted R-squared.",
        "* p < 0.10, ** p < 0.05, *** p < 0.01."
    )
)

ft <- fontsize(ft, size = 8, part = "footer")

ft <- autofit(ft)

save_as_docx(
    ft,
    path = "results/table7_formatted.docx"
)
