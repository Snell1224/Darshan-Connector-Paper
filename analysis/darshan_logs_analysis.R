###########################################################################
# IMPORTING R libraries for processing data and generating visualizations
# Install R packages as:
#  install.packages(c("dplyr", "tidyverse", "patchwork", "magick", "viridis"))
# Run as $ Rscript generating_plots_swfft.R INPUT_FILENAME
###########################################################################
options(crayon.enabled=FALSE)
library(dplyr)
library(tidyverse)  
library(arrow)
options(scipen = 999)

###########################################################################
# READ INFO ABOUT HACC IO
df.hacc <- read_csv("./logs_csv_output/hacc-io/all_darshan_ldms_hacc.csv")

# Getting number of operations and bytes processed
df.hacc %>% 
    # Not getting close operations because Darshan does not return it
    filter(op != "close") %>%
    group_by(job_id, op, `#module`) %>%
    summarize(n=n(), 
        mintime=min(`seg:timestamp`), 
        maxtime=max(`seg:timestamp`),
        tbytes=sum(`seg:len`),
        tmaxbytes=max(max_byte)) %>%
    mutate(total_dur = maxtime - mintime) %>%
    select(job_id, op, n, `#module`, total_dur, tbytes, tmaxbytes) %>%
    arrange(job_id, `#module`) %>%
    as.data.frame()

# Getting total execution time
df.hacc %>% 
    group_by(job_id, `#module`) %>%
    summarize(n=n(), 
        mintime=min(`seg:timestamp`), 
        maxtime=max(`seg:timestamp`)) %>%
    mutate(total_dur = maxtime - mintime) %>%
    select(job_id, `#module`, total_dur) %>%
    arrange(job_id) %>%
    as.data.frame()

df.hacc %>% 
    filter(`#module` == "POSIX") %>%
    filter(job_id == "256542") %>%
    group_by(job_id, rank) %>%
    summarize(mintime=min(`seg:timestamp`), 
        tbytes=sum(`seg:len`),
        maxtime=max(`seg:timestamp`)) %>%
    arrange(maxtime) %>%
    as.data.frame()

df.hacc %>% 
    filter(`#module` == "POSIX") %>%
    filter(job_id == "256576") %>%
    group_by(job_id, rank) %>%
    summarize(mintime=min(`seg:timestamp`), 
        tbytes=sum(`seg:len`),
        maxtime=max(`seg:timestamp`)) %>%
    arrange(maxtime) %>%
    as.data.frame()

# The Darshan-LDMS connector has the same number of POSIX operations as Darshan
df.hacc %>%
    filter(`#module` == "POSIX") %>%
    filter(op %in% c("read", "write")) %>%
    group_by(job_id, ProducerName, rank, op) %>%
    summarize(occurences=n()) 

# # # The extra 440 operations are STDIO operations from node 20 and rank
df.hacc %>%
    filter(`#module` == "STDIO") %>%
    filter(op %in% c("read", "write")) 

###########################################################################
# READ INFO ABOUT MPIIO
df.mpiio <- read_csv("./logs_csv_output/mpi-io/all_darshan_ldms_mpi.csv")

# # Getting number of operations and bytes processed
df.mpiio %>% 
    filter(op != "close") %>%
    group_by(job_id, op, `#module`) %>%
    summarize(n=n(), 
        mintime=min(`seg:timestamp`), 
        maxtime=max(`seg:timestamp`),
        tbytes=sum(`seg:len`),
        tmaxbytes=max(max_byte)) %>%
    mutate(total_dur = maxtime - mintime) %>%
    select(job_id, op, n, `#module`, total_dur, tbytes, tmaxbytes) %>%
    arrange(job_id, `#module`) %>%
    as.data.frame()

# # Getting total execution time
df.mpiio %>% 
    group_by(job_id, `#module`) %>%
    summarize(n=n(), 
        mintime=min(`seg:timestamp`), 
        maxtime=max(`seg:timestamp`)) %>%
    mutate(total_dur = maxtime - mintime) %>%
    select(job_id, `#module`, total_dur) %>%
    arrange(job_id) %>%
    as.data.frame()

# # Getting fastest and slowest rank per job
# # just for the jobs analysed
df.mpiio %>% 
    filter(job_id == "256511") %>%
    group_by(job_id, rank, `#module`) %>%
    summarize(n=n(), 
        mintime=min(`seg:timestamp`), 
        maxtime=max(`seg:timestamp`)) %>%
    mutate(total_dur = maxtime - mintime) %>%
    arrange(job_id, total_dur) %>%
    head(5) %>%
    as.data.frame()

df.mpiio %>% 
    filter(`#module` == "MPIIO") %>%
    filter(job_id == "256511") %>%
    group_by(job_id, rank) %>%
    summarize(mintime=min(`seg:timestamp`), 
        tbytes=sum(`seg:len`),
        maxtime=max(`seg:timestamp`)) %>%
    arrange(maxtime) %>%
    as.data.frame()
    
df.mpiio %>% 
    filter(`#module` == "MPIIO") %>%
    filter(job_id == "256522") %>%
    group_by(job_id, rank) %>%
    summarize(mintime=min(`seg:timestamp`), 
        tbytes=sum(`seg:len`),
        maxtime=max(`seg:timestamp`)) %>%
    arrange(maxtime) %>%
    as.data.frame()



