# Separating files (per experiment) for local plots generation
options(crayon.enabled=FALSE)
library(dplyr)
library(tidyverse) 
library(arrow)

# Read parquet file to separate experiments in separate files, and
# rename exe column values to easier to read than full dir path
df <- read_parquet("./../data/all_csv.parquet") %>%
    mutate(execution = sub('.*\\/', '', exe))

# Applications Job_IDs - From Excel file
sw4 = c(255724,255726,255728,255729,255723)

hacc_lustre_10 = c(255515,255673,255674,255675,255669)
hacc_lustre_5 = c(255668,255670,255671,255672,255667)

hacc_nfs_10 = c(255686,255687,255688,255689,255685)
hacc_nfs_5 = c(255677,255678,255681,255684,255676)

mpi_io_luster_withcollective = c(255648,255649,255650,255651,255647)
mpi_io_luster_withoutcollective = c(255653,255654,255655,255656,255652)

mpi_io_nfs_withcollective = c(255658,255659,255661,255662,255657)
mpi_io_nfs_withoutcollective = c(255663,255664,255665,255666,255660)

hmmer_luster = c(256021,256022,256023,256024,256020)
hmmer_nfs = c(256016,256017,256018,256019,256015)

# Print the distinct values for the execution column
df %>% distinct(execution) 
df %>% distinct(exe) 

# Generate one data file for each experiment separately
df %>% 
    filter(job_id %in% sw4) -> df.sw4
    write_csv(df.sw4, "./../data/files-per-app/sw4.csv")
    write_parquet(df.sw4, "./../data/files-per-app/sw4.parquet")

df %>% 
    filter(job_id %in% hacc_lustre_10) -> df.hacc_lustre_10
    write_csv(df.hacc_lustre_10, "./../data/files-per-app/hacc_lustre_10.csv")
    write_parquet(df.hacc_lustre_10, "./../data/files-per-app/hacc_lustre_10.parquet")

df %>% 
    filter(job_id %in% hacc_lustre_5) -> df.hacc_lustre_5
    write_csv(df.hacc_lustre_5, "./../data/files-per-app/hacc_lustre_5.csv")
    write_parquet(df.hacc_lustre_5, "./../data/files-per-app/hacc_lustre_5.parquet")

df %>% 
    filter(job_id %in% hacc_nfs_10) -> df.hacc_nfs_10
    write_csv(df.hacc_nfs_10, "./../data/files-per-app/hacc_nfs_10.csv")
    write_parquet(df.hacc_nfs_10, "./../data/files-per-app/hacc_nfs_10.parquet")

df %>% 
    filter(job_id %in% hacc_nfs_5) -> df.hacc_nfs_5
    write_csv(df.hacc_nfs_5, "./../data/files-per-app/hacc_nfs_5.csv")
    write_parquet(df.hacc_nfs_5, "./../data/files-per-app/hacc_nfs_5.parquet")

df %>% 
    filter(job_id %in% mpi_io_luster_withcollective) -> df.mpi_io_luster_withcollective
    write_csv(df.mpi_io_luster_withcollective, "./../data/files-per-app/mpi_io_luster_withcollective.csv")
    write_parquet(df.mpi_io_luster_withcollective, "./../data/files-per-app/mpi_io_luster_withcollective.parquet")

df %>% 
    filter(job_id %in% mpi_io_luster_withoutcollective) -> df.mpi_io_luster_withoutcollective
    write_csv(df.mpi_io_luster_withoutcollective, "./../data/files-per-app/mpi_io_luster_withoutcollective.csv")
    write_parquet(df.mpi_io_luster_withoutcollective, "./../data/files-per-app/mpi_io_luster_withoutcollective.parquet")

df %>% 
    filter(job_id %in% mpi_io_nfs_withcollective) -> df.mpi_io_nfs_withcollective
    write_csv(df.mpi_io_nfs_withcollective, "./../data/files-per-app/mpi_io_nfs_withcollective.csv")
    write_parquet(df.mpi_io_nfs_withcollective, "./../data/files-per-app/mpi_io_nfs_withcollective.parquet")

df %>% 
    filter(job_id %in% mpi_io_nfs_withoutcollective) -> df.mpi_io_nfs_withoutcollective
    write_csv(df.mpi_io_nfs_withoutcollective, "./../data/files-per-app/mpi_io_nfs_withoutcollective.csv")
    write_parquet(df.mpi_io_nfs_withoutcollective, "./../data/files-per-app/mpi_io_nfs_withoutcollective.parquet")

df %>% 
    filter(job_id %in% hmmer_luster) -> df.hmmer_luster
    write_csv(df.hmmer_luster, "./../data/files-per-app/hmmer_luster.csv")
    write_parquet(df.hmmer_luster, "./../data/files-per-app/hmmer_luster.parquet")

df %>% 
    filter(job_id %in% hmmer_nfs) -> df.hmmer_nfs
    write_csv(df.hmmer_nfs, "./../data/files-per-app/hmmer_nfs.csv")
    write_parquet(df.hmmer_nfs, "./../data/files-per-app/hmmer_nfs.parquet")

