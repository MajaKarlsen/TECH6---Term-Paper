#RUN ALL: regenerates every table and figure in results/ from regdata0.dta

dir.create("results", showWarnings = FALSE)

scripts <- c(
  "code/table1.R",
  "code/table2.R",
  "code/table3.R",
  "code/table7.R",
  "code/figure1.R",
  "code/figure2.R",
  "code/figure3.R",
  "code/figure4.R",
  "code/ext_1.R",
  "code/ext_2.R"
)

for (s in scripts) {
  message("Running script: ", s)
  source(s, local = new.env())
}

message("All scripts executed successfully. Please see results/")
