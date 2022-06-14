# Records 15672166/15672166
# Rscript analysis.R ./data/csv-files/
options(crayon.enabled=FALSE)
library(dplyr)
library(tidyverse) 
library(arrow)

args = commandArgs(trailingOnly=TRUE)

# Read parquet file
df <- read_parquet(args)
df
df %>% colnames()

# df %>% distinct(`#uid`)
df %>% distinct(job_id) %>% arrange(job_id) %>% as.data.frame()
# df %>% distinct(op)
# df %>% distinct(rank) %>% arrange(rank) %>% as.data.frame()
# df %>% distinct(ProducerName)
# df %>% distinct(execution)

 df %>% group_by(job_id) %>%
    mutate(time = timestamp - min(timestamp)) -> df.new

write_parquet(df.new, "./../data/hacc_io_new.parquet")
 