# Records 15672166/15672166
# Rscript analysis.R ./data/csv-files/
options(crayon.enabled=FALSE)
library(dplyr)
library(tidyverse) 
library(arrow)

# Write parquet file
df <- read_parquet("./../data/all_csv.parquet")

df