#VARIABLES: panel labels for Figures 3 and 4, and controls for ext_2.R

fig3_vars <- c(
  vshr_islam1994 = "Islamic vote share",
  ageshr60       = "Age 60+",
  ageshr19       = "Age 19-",
  lpop1994       = "Log Population",
  sexr           = "Gender",
  anyc           = "Any center municipality",
  partycount     = "Number of parties",
  shhs           = "Household size"
)

fig4_vars <- c(
  hischshr1520f    = "(a) Women 15-20 with high school, 2000",
  hischshr1520m    = "(b) Men 15-20 with high school, 2000",
  c90hischshr1520f = "(c) Women 15-20 with high school, 1990",
  c90hischshr1520m = "(d) Men 15-20 with high school, 1990"
)

#For ext2
controls <- c("vshr_islam1994", "partycount", "lpop1994",
              "ageshr19", "ageshr60", "sexr", "merkezi", "merkezp", "subbuyuk", "buyuk")
