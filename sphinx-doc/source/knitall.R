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

knit("5_graphing_commands.Rrst")
purl("5_graphing_commands.Rrst")

knit("6_R_objects.Rrst")
purl("6_R_objects.Rrst")

knit("7_programming_statements.Rrst")
purl("7_programming_statements.Rrst")

knit("8_writing_new_models.Rrst")
purl("8_writing_new_models.Rrst")

knit("9_adding_models_and_methods.Rrst")
purl("9_adding_models_and_methods.Rrst")

knit("10_faq.Rrst")
purl("10_faq.Rrst")

setwd("..")
