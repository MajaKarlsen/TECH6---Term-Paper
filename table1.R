#TABLE 1: Summary statistics

library(haven)
library(tidyverse)
library(flextable)

#Load the data
df <- read_dta("regdata0.dta")
nrow(df)                      #3204 municipalities in the data

#Create the results folder if it does not exist
dir.create("results", showWarnings = FALSE)

#Select variables for table 1 and the labels shown in the table
table1_labels <- c(
  hischshr1520f = "Share Women aged 15-20 with high school education",
  hischshr1520m = "Share Men aged 15-20 with high school education",
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

table1_vars <- names(table1_labels)

#Keep only municipalities with no missing values in the table variables
df_table1 <- df[complete.cases(df[, table1_vars]), ]
nrow(df_table1) #2629 municipalities (544 have no 1994 election result and 31 miss an outcome or)

#Stars follow the published Table I (0.001 / 0.01 / 0.05), not the 1/5/10 % given in the paper's footnote
get_stars <- function(p) {
  if (p < 0.001) "***"
  else if (p < 0.01) "**"
  else if (p < 0.05) "*"
  else ""
}

#Returns one row of Table I (means, s.d. and difference) for one variable
make_row <- function(variable) {
  x <- df_table1[[variable]]
  
  islamic <- x[df_table1$i94 == 1]
  secular <- x[df_table1$i94 == 0]
  
  difference <- mean(islamic) - mean(secular)
  
  #i94 is always 1 in the Islamic group and 0 for the secular
  if (variable == "i94") {
    se <- 0
    stars <- ""
  } else {
    #Test the difference between the two means with a t-test
    test <- t.test(islamic, secular, var.equal = TRUE)
    
    se <- test$stderr
    p <- test$p.value
    
    stars <- get_stars(p)
  }
  data.frame(
    Variable = table1_labels[[variable]],
    All = sprintf("%.3f (%.3f)", mean(x), sd(x)),
    Islamic = sprintf("%.3f (%.3f)", mean(islamic), sd(islamic)),
    Secular = sprintf("%.3f (%.3f)", mean(secular), sd(secular)),
    Difference = sprintf("%.3f%s (%.3f)", difference, stars, se)
  )
}

#Build one row per variable and combine them into one table
table1_rows <- lapply(names(table1_labels), make_row)
table1_final <- bind_rows(table1_rows)

table1_final <- bind_rows(
  table1_final,
  data.frame(
    Variable = "Observations",
    All = as.character(nrow(df_table1)),
    Islamic = as.character(sum(df_table1$i94 == 1)),
    Secular = as.character(sum(df_table1$i94 == 0)),
    Difference = as.character(nrow(df_table1))
  )
)

#Format the table
ft <- flextable(table1_final)

ft <- add_header_lines(
  ft,
  values = "TABLE I — SUMMARY STATISTICS"
)

ft <- theme_booktabs(ft)
ft <- bold(ft, part = "header")
ft <- fontsize(ft, size = 8, part = "all")

ft <- align(
  ft,
  j = c("All", "Islamic", "Secular", "Difference"),
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
    "Stars follow the entries displayed in the published table:",
    "* p < 0.05, ** p < 0.01, *** p < 0.001."
  )
)

ft <- fontsize(ft, size = 8, part = "footer")

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