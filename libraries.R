

# installation for dmetar
if (!require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("MathiasHarrer/dmetar")

# Packages
library(readxl)
library(dplyr)
library(janitor)
library(naniar)
library(meta)
library(metafor)
library(dmetar)
library(ggplot2)

