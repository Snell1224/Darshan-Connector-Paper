# Separating files (per experiment) for local plots generation
options(crayon.enabled=FALSE)
library(dplyr)
library(tidyverse) 
library(arrow)

# Read parquet file and separate experiments in separate files
df <- read_parquet("./../data/all_csv.parquet")

df %>% mutate(execution = sub('.*\\/', '', exe)) %>%
    select(-exe) -> df.all

# Get execution names
df.all %>% distinct(execution) 

# Get data for each file separately
df.all %>% filter(execution == "hacc_io") -> df.hacc_io
write_parquet(df.hacc_io, "./../data/hacc_io.parquet")

df.all %>% filter(execution == "mpi_io_test_lC") -> df.mpi_io_test_lC
write_parquet(df.mpi_io_test_lC, "./../data/mpi_io_test_lC.parquet")

df.all %>% filter(execution == "mpi_io_test_l") -> df.mpi_io_test_l
write_parquet(df.mpi_io_test_l, "./../data/mpi_io_test_l.parquet")

# Ignore for now due to issues running this application
#df.all %>% filter(execution == "hmmbuild") -> df.hmmbuild
#write_parquet(df.hmmbuild, "./../data/hmmbuild.parquet")

df.all %>% filter(execution == "mpi_io_test_nC") -> df.mpi_io_test_nC
write_parquet(df.mpi_io_test_nC, "./../data/mpi_io_test_nC.parquet")

df.all %>% filter(execution == "mpi_io_test_n") -> df.mpi_io_test_n
write_parquet(df.mpi_io_test_n, "./../data/mpi_io_test_n.parquet")

df.all %>% filter(execution == "hacc_io") -> df.hacc_io
write_parquet(df.hacc_io, "./../data/hacc_io.parquet")

# For N/A execution file (not metadata)
df.all %>% filter(execution == "A") -> df.na
write_parquet(df.na, "./../data/na.parquet")

