# Render all markdown reports into a folder

groupname <- "countryname"

# Create a folder for the reports
dir.create(paste0("outputs/", groupname), showWarnings = FALSE)

# Render all Rmd files in this folder - save as html to the groupname folder
rmarkdown::render("scripts/CAET analysis.Rmd", output_file = paste0("outputs/", groupname, "/CAET analysis.html"))
rmarkdown::render("scripts/CAET Score T-Tests.Rmd", output_file = paste0("outputs/", groupname, "/CAET Score T-Tests.html"))
rmarkdown::render("scripts/CAET vs Economic Indicators.Rmd", output_file = paste0("outputs/", groupname, "/CAET vs Economic Indicators.html"))
rmarkdown::render("scripts/CAET vs Environment Indicators.Rmd", output_file = paste0("outputs/", groupname, "/CAET vs Environment Indicators.html"))
rmarkdown::render("scripts/CAET vs Food and Nutrition.Rmd", output_file = paste0("outputs/", groupname, "/CAET vs Food and Nutrition.html"))
rmarkdown::render("scripts/CAET vs Pesticides.Rmd", output_file = paste0("outputs/", groupname, "/CAET vs Pesticides.html"))
rmarkdown::render("scripts/CAET vs Productivity.Rmd", output_file = paste0("outputs/", groupname, "/CAET vs Productivity.html"))
rmarkdown::render("scripts/CAET Vs Social Indicators.Rmd", output_file = paste0("outputs/", groupname, "/CAET Vs Social Indicators.html"))
rmarkdown::render("scripts/CAET vs Soil Health.Rmd", output_file = paste0("outputs/", groupname, "/CAET vs Soil Health.html"))
rmarkdown::render("scripts/Soil Sample Analysis.Rmd", output_file = paste0("outputs/", groupname, "/Soil Sample Analysis.html"))

