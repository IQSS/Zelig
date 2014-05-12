library(knitr)

setwd("source/")

knit("1_introduction.Rrst")
purl("1_introduction.Rrst")

knit("2_install.Rrst")
purl("2_install.Rrst")

knit("3_data_analysis.Rrst")
purl("3_data_analysis.Rrst")

knit("4_statistical_commands.Rrst")
purl("4_statistical_commands.Rrst")

setwd("..")
