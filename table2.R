library(haven)
library(dplyr)
library(fixest)
library(flextable)

#Load the data
df <- read_dta("regdata0.dta")

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

df_table2 <- df[complete.cases(df[ , c(table2_vars,province_vars)]),]


model_f1 <- feols(
    hischshr1520f ~ i94,
    data = df_table2,
    cluster = ~pcode
)

model_f2 <- feols(
    hischshr1520f ~ i94 + vshr_islam1994 + partycount + 
        shhs + lpop1994 + ageshr19 + ageshr60 + 
        sexr + merkezi + merkezp + subbuyuk + buyuk | province_post,
    data = df_table2,
    cluster = ~pcode
)

bandwidth_f <- 0.240

df_rd_f <- df_table2 |>
    filter(abs(iwm94) < bandwidth_f)


model_f3 <- feols(
    hischshr1520f ~ i94 + vi1 + vi1_i,
    data = df_rd_f,
    cluster = ~pcode
)

model_f4 <- feols(
    hischshr1520f ~ i94 + vi1 + vi1_i +
        vshr_islam1994 + partycount +
        shhs + lpop1994 + ageshr19 + ageshr60 +
        sexr + merkezi + merkezp + subbuyuk + buyuk |
        province_post,
    data = df_rd_f,
    cluster = ~pcode,
    fixef.rm = "none"
)

bandwidth_half <- bandwidth_f / 2

df_rd_half <- df_table2 |>
    filter(abs(iwm94) < bandwidth_half)

model_f5 <- feols(
    hischshr1520f ~ i94 + vi1 + vi1_i +
        vshr_islam1994 + partycount +
        shhs + lpop1994 + ageshr19 + ageshr60 +
        sexr + merkezi + merkezp + subbuyuk + buyuk |
        province_post,
    data = df_rd_half,
    cluster = ~pcode,
    fixef.rm = "none"
)

bandwidth_double <- bandwidth_f * 2

df_rd_double <- df_table2 |>
    filter(abs(iwm94) < bandwidth_double)

model_f6 <- feols(
    hischshr1520f ~ i94 + vi1 + vi1_i +
        vshr_islam1994 + partycount +
        shhs + lpop1994 + ageshr19 + ageshr60 +
        sexr + merkezi + merkezp + subbuyuk + buyuk |
        province_post,
    data = df_rd_double,
    cluster = ~pcode,
    fixef.rm = "none"
)

model_f7 <- feols(
    hischshr1520f ~ i94 + vi1 + vi1_i + vi2 + vi2_i +
        vshr_islam1994 + partycount +
        shhs + lpop1994 + ageshr19 + ageshr60 +
        sexr + merkezi + merkezp + subbuyuk + buyuk |
        province_post,
    data = df_rd_f,
    cluster = ~pcode,
    fixef.rm = "none"
)

model_f8 <- feols(
    hischshr1520f ~ i94 + vi1 + vi1_i +
        vi2 + vi2_i + vi3 + vi3_i +
        vshr_islam1994 + partycount +
        shhs + lpop1994 + ageshr19 + ageshr60 +
        sexr + merkezi + merkezp + subbuyuk + buyuk |
        province_post,
    data = df_rd_f,
    cluster = ~pcode,
    fixef.rm = "none"
)

bandwidth_enroll_f <- 0.205

df_enroll_f <- df_table2 |>
    filter(abs(iwm94) < bandwidth_enroll_f)

model_f9 <- feols(
    studshr1530f ~ i94 + vi1 + vi1_i +
        vshr_islam1994 + partycount +
        shhs + lpop1994 + ageshr19 + ageshr60 +
        sexr + merkezi + merkezp + subbuyuk + buyuk |
        province_post,
    data = df_enroll_f,
    cluster = ~pcode,
    fixef.rm = "none"
)

model_m1 <- feols(
    hischshr1520m ~ i94,
    data = df_table2,
    cluster = ~pcode
)

model_m2 <- feols(
    hischshr1520m ~ i94 + vshr_islam1994 + partycount + 
        shhs + lpop1994 + ageshr19 + ageshr60 + 
        sexr + merkezi + merkezp + subbuyuk + buyuk | province_post,
    data = df_table2,
    cluster = ~pcode
)

bandwidth_m <- 0.323

df_rd_m <- df_table2 |>
    filter(abs(iwm94) < bandwidth_m)


model_m3 <- feols(
    hischshr1520m ~ i94 + vi1 + vi1_i,
    data = df_rd_m,
    cluster = ~pcode
)

model_m4 <- feols(
    hischshr1520m ~ i94 + vi1 + vi1_i +
        vshr_islam1994 + partycount +
        shhs + lpop1994 + ageshr19 + ageshr60 +
        sexr + merkezi + merkezp + subbuyuk + buyuk |
        province_post,
    data = df_rd_m,
    cluster = ~pcode,
    fixef.rm = "none"
)

bandwidth_half_m <- bandwidth_m / 2

df_rd_half_m <- df_table2 |>
    filter(abs(iwm94) < bandwidth_half_m)

model_m5 <- feols(
    hischshr1520m ~ i94 + vi1 + vi1_i +
        vshr_islam1994 + partycount +
        shhs + lpop1994 + ageshr19 + ageshr60 +
        sexr + merkezi + merkezp + subbuyuk + buyuk |
        province_post,
    data = df_rd_half_m,
    cluster = ~pcode,
    fixef.rm = "none"
)

bandwidth_double_m <- bandwidth_m * 2

df_rd_double_m <- df_table2 |>
    filter(abs(iwm94) < bandwidth_double_m)

model_m6 <- feols(
    hischshr1520m ~ i94 + vi1 + vi1_i +
        vshr_islam1994 + partycount +
        shhs + lpop1994 + ageshr19 + ageshr60 +
        sexr + merkezi + merkezp + subbuyuk + buyuk |
        province_post,
    data = df_rd_double_m,
    cluster = ~pcode,
    fixef.rm = "none"
)

model_m7 <- feols(
    hischshr1520m ~ i94 + vi1 + vi1_i + vi2 + vi2_i +
        vshr_islam1994 + partycount +
        shhs + lpop1994 + ageshr19 + ageshr60 +
        sexr + merkezi + merkezp + subbuyuk + buyuk |
        province_post,
    data = df_rd_m,
    cluster = ~pcode,
    fixef.rm = "none"
)

model_m8 <- feols(
    hischshr1520m ~ i94 + vi1 + vi1_i +
        vi2 + vi2_i + vi3 + vi3_i +
        vshr_islam1994 + partycount +
        shhs + lpop1994 + ageshr19 + ageshr60 +
        sexr + merkezi + merkezp + subbuyuk + buyuk |
        province_post,
    data = df_rd_m,
    cluster = ~pcode,
    fixef.rm = "none"
)

bandwidth_enroll_m <- 0.230

df_enroll_m <- df_table2 |>
    filter(abs(iwm94) < bandwidth_enroll_m)

model_m9 <- feols(
    studshr1530m ~ i94 + vi1 + vi1_i +
        vshr_islam1994 + partycount +
        shhs + lpop1994 + ageshr19 + ageshr60 +
        sexr + merkezi + merkezp + subbuyuk + buyuk |
        province_post,
    data = df_enroll_m,
    cluster = ~pcode,
    fixef.rm = "none"
)

df_gender <- bind_rows(
    df_table2 |>
        mutate(
            outcome = as.numeric(hischshr1520f),
            male = 0,
            i94 = i94,
            pcode = pcode
        ),

    df_table2 |>
        mutate(
            outcome = as.numeric(hischshr1520m),
            male = 1,
            i94 = i94,
            pcode = pcode
        )
)

df_gender_rd <- bind_rows(
    df_rd_f |>
        mutate(
            outcome = as.numeric(hischshr1520f),
            male = 0
        ),

    df_rd_m |>
        mutate(
            outcome = as.numeric(hischshr1520m),
            male = 1
        )
)

df_gender_half <- bind_rows(
    df_rd_half |>
        mutate(outcome = as.numeric(hischshr1520f), male = 0),
    df_rd_half_m |>
        mutate(outcome = as.numeric(hischshr1520m), male = 1)
)

df_gender_double <- bind_rows(
    df_rd_double |>
        mutate(outcome = as.numeric(hischshr1520f), male = 0),
    df_rd_double_m |>
        mutate(outcome = as.numeric(hischshr1520m), male = 1)
)

df_gender_enroll <- bind_rows(
    df_enroll_f |>
        mutate(outcome = as.numeric(studshr1530f), male = 0),
    df_enroll_m |>
        mutate(outcome = as.numeric(studshr1530m), male = 1)
)

model_c1 <- feols(
    outcome ~ i94*male,
    data = df_gender,
    cluster = ~pcode
)


model_c2 <- feols(
    outcome ~ i94 * male +
        (vshr_islam1994 + partycount +
        shhs + lpop1994 + ageshr19 + ageshr60 +
        sexr + merkezi + merkezp + subbuyuk + buyuk) * male |
        province_post^male,
    data = df_gender,
    cluster = ~pcode,
    fixef.rm = "none"
)

model_c3 <- feols(
    outcome ~ i94 * male +
        (vi1 + vi1_i) * male,
    data = df_gender_rd,
    cluster = ~pcode
)

model_c4 <- feols(
    outcome ~ i94 * male +
        (vi1 + vi1_i +
        vshr_islam1994 + partycount +
        shhs + lpop1994 + ageshr19 + ageshr60 +
        sexr + merkezi + merkezp + subbuyuk + buyuk) * male |
        province_post^male,
    data = df_gender_rd,
    cluster = ~pcode,
    fixef.rm = "none"
)

model_c5 <- feols(
    outcome ~ i94 * male +
        (vi1 + vi1_i +
        vshr_islam1994 + partycount +
        shhs + lpop1994 + ageshr19 + ageshr60 +
        sexr + merkezi + merkezp + subbuyuk + buyuk) * male |
        province_post^male,
    data = df_gender_half,
    cluster = ~pcode,
    fixef.rm = "none"
)

model_c6 <- feols(
    outcome ~ i94 * male +
        (vi1 + vi1_i +
        vshr_islam1994 + partycount +
        shhs + lpop1994 + ageshr19 + ageshr60 +
        sexr + merkezi + merkezp + subbuyuk + buyuk) * male |
        province_post^male,
    data = df_gender_double,
    cluster = ~pcode,
    fixef.rm = "none"
)

model_c7 <- feols(
    outcome ~ i94 * male +
        (vi1 + vi1_i + vi2 + vi2_i +
        vshr_islam1994 + partycount +
        shhs + lpop1994 + ageshr19 + ageshr60 +
        sexr + merkezi + merkezp + subbuyuk + buyuk) * male |
        province_post^male,
    data = df_gender_rd,
    cluster = ~pcode,
    fixef.rm = "none"
)

model_c8 <- feols(
    outcome ~ i94 * male +
        (vi1 + vi1_i + vi2 + vi2_i + vi3 + vi3_i +
        vshr_islam1994 + partycount +
        shhs + lpop1994 + ageshr19 + ageshr60 +
        sexr + merkezi + merkezp + subbuyuk + buyuk) * male |
        province_post^male,
    data = df_gender_rd,
    cluster = ~pcode,
    fixef.rm = "none"
)

model_c9 <- feols(
    outcome ~ i94 * male +
        (vi1 + vi1_i +
        vshr_islam1994 + partycount +
        shhs + lpop1994 + ageshr19 + ageshr60 +
        sexr + merkezi + merkezp + subbuyuk + buyuk) * male |
        province_post^male,
    data = df_gender_enroll,
    cluster = ~pcode,
    fixef.rm = "none"
)

get_stars <- function(p) {
    if (p < 0.01) "***"
    else if (p < 0.05) "**"
    else if (p < 0.10) "*"
    else ""
}

format_coefficient <- function(model) {

    result <- coeftable(model)

    estimate <- result["i94", 1]
    se <- result["i94", 2]
    p <- result["i94", 4]

    sprintf("%.3f%s (%.3f)",
            estimate, get_stars(p), se)
}

female_models <- list(
    model_f1, model_f2, model_f3,
    model_f4, model_f5, model_f6,
    model_f7, model_f8, model_f9
)

male_models <- list(
    model_m1, model_m2, model_m3,
    model_m4, model_m5, model_m6,
    model_m7, model_m8, model_m9
)

female_means <- sapply(female_models, function(model) mean(fitted(model)))
male_means   <- sapply(male_models, function(model) mean(fitted(model)))

female_results <- sapply(female_models, format_coefficient)
male_results <- sapply(male_models, format_coefficient)

female_n <- sapply(female_models, nobs)
male_n <- sapply(male_models, nobs)

panel_c_models <- list(
    model_c1, model_c2, model_c3,
    model_c4, model_c5, model_c6,
    model_c7, model_c8, model_c9
)

gender_p <- sapply(
    panel_c_models,
    function(model) coeftable(model)["i94:male", 4]
)

female_r2 <- sapply(
    female_models,
    function(m) as.numeric(fitstat(m, "r2")$r2)
)

male_r2 <- sapply(
    male_models,
    function(m) as.numeric(fitstat(m, "r2")$r2)
)

female_bandwidths <- c(
    NA, NA,
    bandwidth_f, bandwidth_f,
    bandwidth_half, bandwidth_double,
    bandwidth_f, bandwidth_f,
    bandwidth_enroll_f
)

male_bandwidths <- c(
    NA, NA,
    bandwidth_m, bandwidth_m,
    bandwidth_half_m, bandwidth_double_m,
    bandwidth_m, bandwidth_m,
    bandwidth_enroll_m
)

format_bandwidth <- function(x) {
    if (is.na(x)) {
        return("1")
    }
    format(x, trim = TRUE, scientific = FALSE)
}

table2_final <- data.frame(
    Statistic = c(
        "Panel A: Women",
        "Outcome mean",
        "Islamic mayor (SE)",
        "Bandwidth",
        "R²",
        "Observations",
        "",
        "Panel B: Men",
        "Outcome mean",
        "Islamic mayor (SE)",
        "Bandwidth",
        "R²",
        "Observations",
        "",
        "Panel C: Gender difference",
        "p-value"
    ),
    stringsAsFactors = FALSE
)

for (i in 1:9) {
    table2_final[[paste0("(", i, ")")]] <- c(
        "",                                  
        sprintf("%.3f", female_means[i]),
        female_results[i],
        format_bandwidth(female_bandwidths[i]),
        sprintf("%.3f", female_r2[i]),
        as.character(female_n[i]),
        "",                                  
        "",                                  
        sprintf("%.3f", male_means[i]),
        male_results[i],
        format_bandwidth(male_bandwidths[i]),
        sprintf("%.3f", male_r2[i]),
        as.character(male_n[i]),
        "",                                  
        "",                                  
        sprintf("%.3f", gender_p[i])
    )
}

dir.create("results", showWarnings = FALSE)

write.csv(
    table2_final,
    "results/table2.csv",
    row.names = FALSE
)

ft <- flextable(table2_final)

ft <- add_header_lines(
    ft,
    values = "TABLE II — ISLAMIC MAYORS AND EDUCATIONAL OUTCOMES"
)

ft <- theme_booktabs(ft)

ft <- bold(ft, part = "header")

ft <- fontsize(ft, size = 8, part = "all")

panel_rows <- c(1, 8, 15)

ft <- bold(
    ft,
    i = panel_rows,
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
        "Columns 1–2 use OLS and columns 3–9 use RD. Column 9 measures school enrollment.",
        "Standard errors are clustered by province and shown in parentheses.",
        "RD bandwidths use the rounded values from the paper.",
        "Panel C tests whether the effects differ by gender using a stacked regression. The paper uses Stata's suest test.",
        "* p < 0.10, ** p < 0.05, *** p < 0.01."
    )
)

ft <- autofit(ft)

save_as_docx(
    ft,
    path = "results/table2_formatted.docx"
)

