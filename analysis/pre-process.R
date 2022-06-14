# Records 15672166/15672166
# Rscript analysis.R ./data/csv-files/
options(crayon.enabled=FALSE)
library(dplyr)
library(tidyverse) 
library(arrow)

merge_files <- function(FILE){
    read_csv(FILE, col_names=TRUE, col_types=cols(), progress=FALSE) 
}

# Output directory 
# args = commandArgs(trailingOnly=TRUE)
# Path for directory with data
args = "./../data/csv-files/"

# Get csv outputs
csv_files <- list.files(path = args, pattern = "*-darshanConnector_all_data_6_9.csv$", recursive = TRUE, all.files = TRUE, full.names = TRUE)

if(length(csv_files) != 0)
    lapply(csv_files, merge_files) %>%
        bind_rows -> df

# Write parquet file
write_parquet(df, "./../data/all_csv.parquet")
