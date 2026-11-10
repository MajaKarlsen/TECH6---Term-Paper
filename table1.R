library(haven)
library(dplyr)
library(flextable)

#Load the data
df <- read_dta("regdata0.dta")

#Select variables for table 1
table1_vars <- c(
"iwm94", "hischshr1520f", "hischshr1520m", 
"i94", "vshr_islam1994", "partycount",
"lpop1994", "ageshr19", "ageshr60",
"sexr", "merkezi", "merkezp",
"subbuyuk", "buyuk"
)

df_table1 <- df[complete.cases(df[, table1_vars]), ]


df_table1 <- df_table1 |>
    mutate(I94 = 1 - i94)


table1_labels <- c(
    hischshr1520f = "Share Women aged 15-20 with High School education",
    hischshr1520m = "Share Men aged 15-20 with High School education",
    i94 = "Islamic mayor in 1994",
    vshr_islam1994 = "Islamic vote share 1994",
    partycount = "Number of parties receiving votes 1994",
    lpop1994 = "Log population 1994",
    ageshr19 = "Population share below 19 in 2000",
    ageshr60 = "Population share above 60 in 2000",
    sexr = "Gender ratio in 2000",
    shhs = "Household size in 2000",
    merkezi = "District center",
    merkezp = "Province center",
    subbuyuk = "Sub-metro center"
)

table1_final <- data.frame(
    Variable = character(),
    All = character(),
    Islamic = character(),
    Secular = character(),
    Difference = character()
)

# Loop through each variable and calculate means, standard deviations, and differences
for (variable in names(table1_labels)) {

    x <- df_table1[[variable]]

    islamic <- x[df_table1$i94 == 1]
    secular <- x[df_table1$i94 == 0]

    difference <- mean(islamic) - mean(secular)

    if (variable == "i94") {
        se <- 0
        stars <- ""
    } else {
        test <- t.test(x ~ df_table1$I94, var.equal = TRUE)

        se <- test$stderr
        p <- test$p.value

        stars <- if (p < 0.001) "***"
                 else if (p < 0.01) "**"
                 else if (p < 0.05) "*"
                 else ""
    }

    table1_final <- rbind(
        table1_final,
        data.frame(
            Variable = table1_labels[[variable]],
            All = sprintf("%.3f (%.3f)", mean(x), sd(x)),
            Islamic = sprintf("%.3f (%.3f)", mean(islamic), sd(islamic)),
            Secular = sprintf("%.3f (%.3f)", mean(secular), sd(secular)),
            Difference = sprintf("%.3f%s (%.3f)", difference, stars, se)
        )
    )
}

table1_final <- rbind(
    table1_final,
    data.frame(
        Variable = "Observations",
        All = as.character(nrow(df_table1)),
        Islamic = as.character(sum(df_table1$i94 == 1)),
        Secular = as.character(sum(df_table1$i94 == 0)),
        Difference = as.character(nrow(df_table1))
    )
)

dir.create("results", showWarnings = FALSE)

#Formatting the table
ft <- flextable(table1_final)

ft <- add_header_lines(
    ft,
    values = "TABLE I — SUMMARY STATISTICS"
)

ft <- theme_booktabs(ft)
ft <- bold(ft, part = "header")
ft <- fontsize(ft, size = 9, part = "all")


ft <- align(
    ft,
    j = c("All", "Islamic", "Secular", "Difference"),
    align = "center",
    part = "all"
)

ft <- add_footer_lines(
    ft,
    values = paste(
        "Columns 1-3 report means (SD). Column 4 reports",
        "differences (SE).",
        "* p < 0.05, ** p < 0.01, *** p < 0.001."
    )
)

ft <- autofit(ft)

#Export the results
save_as_docx(
    ft,
    path = "results/table1_formatted.docx"
)

write.csv(
    table1_final,
    "results/table1.csv",
    row.names = FALSE
)