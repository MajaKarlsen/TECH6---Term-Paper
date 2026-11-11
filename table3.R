library(tidyverse)
library(haven)
library(fixest)
library(flextable)

#Load data
df <- read_dta("regdata0.dta")
dir.create("results", showWarnings = FALSE)

table3_vars <- c(
    "hischshr1520f", "hischshr1520m",
    "i94", "iwm94",
    "vshr_islam1994", "partycount",
    "shhs", "lpop1994", "ageshr19",
    "ageshr60", "sexr", "merkezi",
    "merkezp", "subbuyuk", "buyuk"
)

province_vars <- grep("^pd_", names(df), value = TRUE)

df_table3 <- df[
    complete.cases(df[, c(table3_vars, province_vars)]),
]

bandwidths <- c(1, 0.5, 0.25, 0.1, 0.05)

controls <- c(
    "vshr_islam1994", "partycount", "shhs",
    "lpop1994", "ageshr19", "ageshr60",
    "sexr", "merkezi", "merkezp",
    "subbuyuk", "buyuk"
)

polynomials <- c(
    "None" = "",
    "Linear" = "vi1 + vi1_i",
    "Quadratic" = "vi1 + vi1_i + vi2 + vi2_i",
    "Cubic" = "vi1 + vi1_i + vi2 + vi2_i + vi3 + vi3_i",
    "Quartic" = "vi1 + vi1_i + vi2 + vi2_i + vi3 + vi3_i + vi4 + vi4_i"
)

table3_models <- list()

#Run the models for women (Panel A) and men (Panel B)
#Each bandwidth is a column and each polynomial order is a row
for (gender in c("f", "m")) {

    outcome <- paste0("hischshr1520", gender)

    for (bw in bandwidths) {

        #Keep muncipalities where the election margin is within the bandwidth
        sample <- df_table3 |>
            filter(abs(iwm94) < bw)

        #Rescale each power of the election margin and its interaction by bw^k
        #This is to keep the higher powers from getting very small in narrow windows
        for (k in 1:4) {
            sample[[paste0("vi", k)]] <-
                sample[[paste0("vi", k)]] / bw^k

            sample[[paste0("vi", k, "_i")]] <-
                sample[[paste0("vi", k, "_i")]] / bw^k
        }

        for (poly in names(polynomials)) {

            terms <- c(
                "i94",
                polynomials[[poly]],
                controls
            )

            terms <- terms[terms != ""]

            formula_text <- paste(
                outcome, "~",
                paste(terms, collapse = " + "),
                "| province_post"
            )

            model <- feols(
                as.formula(formula_text),
                data = sample,
                cluster = ~pcode,
                fixef.rm = "none"
            )

            model_name <- paste(gender, bw, poly, sep = "_")

            #Stop if the regression dropped a variable because of collinearity.
            #This is so we can check that the full polynomial was fitted.
            stopifnot(is.null(model$collin.var))
            table3_models[[model_name]] <- model
        }
    }
}

get_stars <- function(p) {
    if (p < 0.01) "***"
    else if (p < 0.05) "**"
    else if (p < 0.10) "*"
    else ""
}

#Each cell shows the i94 estimate with stars and its standrad error in parentheses
format_result <- function(model) {

    result <- coeftable(model)

    estimate <- result["i94", 1]
    se <- result["i94", 2]
    p <- result["i94", 4]

    sprintf("%.3f%s (%.3f)",
            estimate, get_stars(p), se)
}

table3_final <- data.frame(
    Specification = c(
        "Panel A: Women",
        "None",
        "Linear",
        "Quadratic",
        "Cubic",
        "Quartic",
        "Observations",
        "",
        "Panel B: Men",
        "None",
        "Linear",
        "Quadratic",
        "Cubic",
        "Quartic",
        "Observations"
    ),
    stringsAsFactors = FALSE
)

for (bw in bandwidths) {

    column_values <- rep("", nrow(table3_final))

    for (gender in c("f", "m")) {

        row_start <- if (gender == "f") 2 else 10

        for (i in seq_along(polynomials)) {

            poly <- names(polynomials)[i]

            model_name <- paste(gender, bw, poly, sep = "_")
            model <- table3_models[[model_name]]

            column_values[row_start + i - 1] <- format_result(model)
        }

        sample_model <- table3_models[[
            paste(gender, bw, "None", sep = "_")
        ]]

        obs_row <- if (gender == "f") 7 else 15

        column_values[obs_row] <- as.character(nobs(sample_model))
    }

    column_name <- sprintf("BW %.2f", bw)
    table3_final[[column_name]] <- column_values
}

#Formatting the table
ft <- flextable(table3_final)

ft <- add_header_lines(
    ft,
    values = "TABLE III — ALTERNATIVE RD SPECIFICATIONS"
)

ft <- theme_booktabs(ft)

ft <- bold(ft, part = "header")
ft <- fontsize(ft, size = 8, part = "all")

ft <- bold(
    ft,
    i = c(1, 9),
    bold = TRUE,
    part = "body"
)

ft <- align(
    ft,
    j = 2:6,
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
    path = "results/table3_formatted.docx"
)

write.csv(
    table3_final,
    "results/table3.csv",
    row.names = FALSE
)