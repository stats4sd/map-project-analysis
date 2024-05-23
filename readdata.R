library(tidyverse)
library(openxlsx)
# PUT PATH TO DATAFILE HERE
### Place your Excel data file into this project folder. Paste in the name of the file to the data_file variable below.
### If you need to download the data file, please login at https://giz-map.stats4sd.org/. These scripts work with the Survey Data from the "Data and Indicator Export" table
data_file <- "Tape GIZ-data-export-2024-05-23 10_45_12 - Kenya.xlsx"


# read in performance data
Main_Survey <- read.xlsx(data_file,
    sheet = "Main Survey",
    check.names = TRUE
)

Indicators <- read.xlsx(data_file,
    sheet = "Calculated Indicators",
    check.names = TRUE
) %>%
    select(farm_id, dietary_diversity:youth_score_f) %>%
    select(-c(
        inc3, structure, compaction, depth, residues,
        color, water_ret, cover, erosion, invertebrates, microbio
    ))

soil_data <- read.xlsx(data_file,
                         sheet = "soil_lab_data",
                         check.names = TRUE) %>%
  select(farm_id, soil_sample_label:soil_textural_class)

data <- left_join(
    Main_Survey,
    Indicators,
) %>%
  left_join(soil_data)

# NOTE: This only works for a single country's data. If you are doing a multi-country analysis, be aware that each country's economic data uses the local currencies, so you cannot directly compare the numbers across countries.
currency <- data[["currency"]][1]


