###########################################################################
# IMPORTING R libraries for processing data and generating visualizations
# Install R packages as:
#  install.packages(c("dplyr", "tidyverse", "patchwork", "magick", "viridis"))
# Run as $ Rscript generating_plots_swfft.R INPUT_FILENAME
###########################################################################
options(crayon.enabled=FALSE)
library(dplyr)
library(tidyverse)  
options(scipen = 999)

###########################################################################
# READ INFO ABOUT HACC IO
df.hacc <- read_csv("./logs_csv_output/hacc-io/all_darshan_ldms_hacc.csv")

my_theme <- function() {                                                                                                                    
      theme_bw(base_size=14) +                                                                                                                   
      theme(panel.background = element_blank(),                                                                                                  
            legend.box.margin = margin(0,0,0,0),                                                                                                 
            legend.spacing = unit(0, "pt"),                                                                                                      
            legend.position = "top",                                                                                                             
            legend.text = element_text(color = "black", size = 14),              
            strip.text.y = element_text(size = 16),            
            panel.spacing.x = unit(0.2, "lines"),
            panel.spacing.y = unit(0.8, "lines"),
            strip.background = element_rect(fill="#F5F5F5", color="black", size=0.1, linetype="solid"),                                                                                                                                                               
            legend.box.spacing = unit(0, "pt"))                                                                                                         
}


# Getting total execution time
# df.hacc %>% 
#     group_by(job_id, `#module`) %>%
#     summarize(mintime=min(`seg:timestamp`), maxtime=max(`seg:timestamp`)) %>%
#     mutate(total_dur = maxtime - mintime) %>%
#     select(job_id, mintime, maxtime, total_dur, `#module`) %>%
#     arrange(job_id) %>%
#     as.data.frame()

# Getting number of operations and bytes processed
# df.hacc %>% 
#     # Not getting close operations because Darshan does not return it
#     filter(op != "close") %>%
#     filter(`#module` == "POSIX") %>%
#     group_by(job_id, op, `#module`) %>%
#     summarize(n=n(), 
#         mintime=min(`seg:timestamp`), 
#         maxtime=max(`seg:timestamp`),
#         tbytes=sum(abs(`seg:len`)),
#         tmaxbytes=max(max_byte)) %>%
#     mutate(total_dur = maxtime - mintime) %>%
#     select(job_id, op, n, `#module`, total_dur, tbytes, tmaxbytes) %>%
#     arrange(job_id, `#module`) %>%
#     as.data.frame()

# Getting total execution time
# df.hacc %>% 
#     group_by(job_id, `#module`) %>%
#     summarize(n=n(), 
#         mintime=min(`seg:timestamp`), 
#         maxtime=max(`seg:timestamp`)) %>%
#     mutate(total_dur = maxtime - mintime) %>%
#     select(job_id, `#module`, total_dur) %>%
#     arrange(job_id) %>%
#     as.data.frame()

# # Getting fastest and slowest rank per job
# # just for the jobs analysed
# df.hacc %>% 
#     filter(`#module` == "POSIX") %>%
#     filter(job_id == "256542") %>%
#     group_by(job_id, rank) %>%
#     summarize(mintime=min(`seg:timestamp`), 
#         tbytes=sum(`seg:len`),
#         maxtime=max(`seg:timestamp`)) %>%
#     arrange(maxtime) %>%
#     as.data.frame()

# df.hacc %>% 
#     filter(`#module` == "POSIX") %>%
#     filter(job_id == "256576") %>%
#     group_by(job_id, rank) %>%
#     summarize(mintime=min(`seg:timestamp`), 
#         tbytes=sum(`seg:len`),
#         maxtime=max(`seg:timestamp`)) %>%
#     arrange(maxtime) %>%
#     as.data.frame()

# The Darshan-LDMS connector has the same number of POSIX operations as Darshan
# df.hacc %>%
#     filter(`#module` == "POSIX") %>%
#     filter(op %in% c("read", "write")) %>%
#     group_by(job_id, ProducerName, rank, op) %>%
#     summarize(occurences=n()) 

# # # The extra 440 operations are STDIO operations from node 20 and rank
# df.hacc %>%
#     filter(`#module` == "STDIO") %>%
#     filter(op %in% c("read", "write")) 

###########################################################################
# READ INFO ABOUT MPIIO
df.mpiio <- read_csv("./logs_csv_output/mpi-io/all_darshan_ldms_mpi.csv")

# # Getting number of operations and bytes processed
# df.mpiio %>% 
#     filter(op != "close") %>%
#     group_by(job_id, op, `#module`) %>%
#     summarize(n=n(), 
#         mintime=min(`seg:timestamp`), 
#         maxtime=max(`seg:timestamp`),
#         tbytes=sum(`seg:len`),
#         tmaxbytes=max(max_byte)) %>%
#     mutate(total_dur = maxtime - mintime) %>%
#     select(job_id, op, n, `#module`, total_dur, tbytes, tmaxbytes) %>%
#     arrange(job_id, `#module`) %>%
#     as.data.frame()

# # Getting total execution time
# df.mpiio %>% 
#     group_by(job_id, `#module`) %>%
#     summarize(n=n(), 
#         mintime=min(`seg:timestamp`), 
#         maxtime=max(`seg:timestamp`)) %>%
#     mutate(total_dur = maxtime - mintime) %>%
#     select(job_id, `#module`, total_dur) %>%
#     arrange(job_id) %>%
#     as.data.frame()

# # Getting fastest and slowest rank per job
# # just for the jobs analysed
# df.mpiio %>% 
#     filter(job_id == "256511") %>%
#     group_by(job_id, rank, `#module`) %>%
#     summarize(n=n(), 
#         mintime=min(`seg:timestamp`), 
#         maxtime=max(`seg:timestamp`)) %>%
#     mutate(total_dur = maxtime - mintime) %>%
#     arrange(job_id, total_dur) %>%
#     head(5) %>%
#     as.data.frame()

# df.mpiio %>% 
#     filter(`#module` == "MPIIO") %>%
#     filter(job_id == "256511") %>%
#     group_by(job_id, rank) %>%
#     summarize(mintime=min(`seg:timestamp`), 
#         tbytes=sum(`seg:len`),
#         maxtime=max(`seg:timestamp`)) %>%
#     arrange(maxtime) %>%
#     as.data.frame()
    
# df.mpiio %>% 
#     filter(`#module` == "MPIIO") %>%
#     filter(job_id == "256522") %>%
#     group_by(job_id, rank) %>%
#     summarize(mintime=min(`seg:timestamp`), 
#         tbytes=sum(`seg:len`),
#         maxtime=max(`seg:timestamp`)) %>%
#     arrange(maxtime) %>%
#     as.data.frame()

# #################################################
# More information after meeting
# df.mpiio %>% distinct(job_id)
# # Getting number of operations and bytes processed
# df.mpiio %>%
#     # filter(op != "close") %>%
#     group_by(job_id, op, `#module`) %>%
#     summarize(n=n(),
#         mintime=min(`seg:timestamp`),
#         maxtime=max(`seg:timestamp`),
#         tbytes=sum(`seg:len`),
#         tmaxbytes=max(max_byte)) %>%
#     mutate(total_dur = maxtime - mintime) %>%
#     select(job_id, op, n, `#module`, total_dur, tbytes, tmaxbytes) %>%
#     arrange(`#module`, job_id) %>%
#     as.data.frame()
# # Getting total execution time
# df.mpiio %>%
#     filter(`#module` == "POSIX") %>%
#     group_by(job_id, `#module`, filesystem, collective) %>%
#     summarize(n=n(),
#         mintime=min(`seg:timestamp`),
#         maxtime=max(`seg:timestamp`)) %>%
#     mutate(total_dur = maxtime - mintime) %>%
#     select(filesystem, collective, job_id, `#module`, total_dur) %>%
#     arrange(job_id) %>%
#     as.data.frame()

# # Getting fastest and slowest rank per job
# # just for the jobs analysed
# df.mpiio %>%
#     filter(`#module` == "MPIIO") %>%
#     filter(job_id == "256511") %>%
#     group_by(rank) %>%
#     summarize(n=n(),
#         mintime=min(`seg:timestamp`),
#         maxtime=max(`seg:timestamp`)) %>%
#     mutate(total_dur = (maxtime - mintime)) %>%
#     arrange(total_dur) %>%
#     ggplot(aes(x=rank, y=total_dur)) +
#     geom_col() + 
#     scale_x_continuous("Rank", expand=c(0,0), breaks=seq(0,200000,50)) + 
#     coord_cartesian(ylim = c(234.3,234.5)) +
#     scale_y_continuous("Total execution time (sec)", expand=c(0,0), breaks=seq(0,2000,0.01)) + 
#     my_theme() -> p

#     ggsave(filename=paste("./figs/", "openmmpi_fastest_slowest_ranks_totaltime.png", sep=""),
#     plot = p,
#     height=4,
#     width=10)

# df.mpiio %>%
#     filter(`#module` == "MPIIO") %>%
#     filter(job_id == "256511") %>%
#     group_by(rank) %>%
#     summarize(total_dur=sum(`seg:dur`),
#         tbytes=sum(`seg:len`)) %>%
#     arrange(total_dur) %>%
#     as.data.frame() %>%
#     print() %>%
#     ggplot(aes(x=rank, y=total_dur)) +
#     geom_col() + 
#     scale_x_continuous("Rank", expand=c(0,0), breaks=seq(0,200000,50)) + 
#     coord_cartesian(ylim = c(700,NA)) +
#     scale_y_continuous("Total operations duration (sec)", expand=c(0,0), breaks=seq(0,2000,100)) + 
#     my_theme() -> p

#     ggsave(filename=paste("./figs/", "openmmpi_fastest_slowest_ranks_sum_256511.png", sep=""),
#     plot = p,
#     height=4,
#     width=10)

# df.mpiio %>%
#     filter(`#module` == "MPIIO") %>%
#     filter(job_id == "256522") %>%
#     group_by(rank) %>%
#     summarize(total_dur=sum(`seg:dur`),
#         tbytes=sum(`seg:len`)) %>%
#     arrange(total_dur) %>%
#     as.data.frame() %>%
#     print() %>%
#     ggplot(aes(x=rank, y=total_dur)) +
#     geom_col() + 
#     scale_x_continuous("Rank", expand=c(0,0), breaks=seq(0,200000,50)) + 
#     coord_cartesian(ylim = c(700,NA)) +
#     scale_y_continuous("Total operations duration (sec)", expand=c(0,0), breaks=seq(0,10000,500)) + 
#     my_theme() -> p

#     ggsave(filename=paste("./figs/", "openmmpi_fastest_slowest_ranks_sum_256522.png", sep=""),
#     plot = p,
#     height=4,
#     width=10)

# df.mpiio %>%
#     filter(`#module` == "MPIIO") %>%
#     filter(job_id == "256522") %>%
#     group_by(rank) %>%
#     summarize(total_dur=sum(`seg:dur`)) %>%
#     as.data.frame() %>%
#     print() 
    
    # %>%
    # ggplot(aes(x=rank, y=total_dur)) +
    # geom_col() + 
    # scale_x_continuous("Rank", expand=c(0,0), breaks=seq(0,200000,50)) + 
    # coord_cartesian(ylim = c(700,NA)) +
    # scale_y_continuous("Total execution time (sec)", expand=c(0,0), breaks=seq(0,2000,100)) + 
    # my_theme() -> p

    # ggsave(filename=paste("./figs/", "openmmpi_fastest_slowest_ranks_sum_256522.png", sep=""),
    # plot = p,
    # height=4,
    # width=10)

# df.mpiio %>%
#     filter(`#module` == "MPIIO") %>%
#     filter(job_id == "256511") %>%
#     group_by(job_id, rank) %>%
#     summarize(mintime=min(`seg:timestamp`),
#         tbytes=sum(`seg:len`),
#         maxtime=max(`seg:timestamp`)) %>%
#     arrange(maxtime) %>%
#     as.data.frame()
# df.mpiio %>%
#     filter(`#module` == "MPIIO") %>%
#     filter(job_id == "256522") %>%
#     group_by(job_id, rank) %>%
#     summarize(mintime=min(`seg:timestamp`),
#         tbytes=sum(`seg:len`),
#         maxtime=max(`seg:timestamp`)) %>%
#     arrange(maxtime) %>%
#     as.da

# df.mpiio %>%
#     filter(`#module` == "MPIIO") %>%
#     filter(job_id == "256511") %>%
#     filter(op %in% c("read", "write")) %>%
#     group_by(rank) %>%
#     summarize(total_dur=sum(`seg:dur`),
#         tbytes=sum(`seg:len`)) %>%
#     arrange(total_dur) %>%
#     as.data.frame() 

# df.mpiio %>%
#     filter(`#module` == "MPIIO") %>%
#     filter(job_id == "256522") %>%
#     filter(op %in% c("read", "write")) %>%
#     group_by(rank) %>%
#     summarize(total_dur=sum(`seg:dur`),
#         tbytes=sum(`seg:len`)) %>%
#     arrange(total_dur) %>%
#     as.data.frame() 

df.mpiio %>%
    filter(`#module` == "MPIIO") %>%
    filter(job_id == "256511") %>%
    # filter(op %in% c("read", "write")) %>%
    group_by(rank) %>%
    summarize(total_dur=sum(`seg:dur`),
        maxtime = max(`seg:timestamp`),
        tbytes=sum(`seg:len`)) %>%
    arrange(maxtime) %>%
    as.data.frame() %>% head(10)

df.mpiio %>%
    filter(`#module` == "MPIIO") %>%
    filter(job_id == "256511") %>%
    # filter(op %in% c("read", "write")) %>%
    group_by(rank) %>%
    summarize(total_dur=sum(`seg:dur`),
        maxtime = max(`seg:timestamp`),
        tbytes=sum(`seg:len`)) %>%
    arrange(maxtime) %>%
    as.data.frame() %>% tail(10)

df.mpiio %>%
    filter(`#module` == "MPIIO") %>%
    filter(job_id == "256522") %>%
    # filter(op %in% c("read", "write")) %>%
    group_by(rank) %>%
    summarize(total_dur=sum(`seg:dur`),
        maxtime = max(`seg:timestamp`),
        tbytes=sum(`seg:len`)) %>%
    arrange(maxtime) %>%
    as.data.frame() %>% head(10)

df.mpiio %>%
    filter(`#module` == "MPIIO") %>%
    filter(job_id == "256522") %>%
    # filter(op %in% c("read", "write")) %>%
    group_by(rank) %>%
    summarize(total_dur=sum(`seg:dur`),
        maxtime = max(`seg:timestamp`),
        tbytes=sum(`seg:len`)) %>%
    arrange(maxtime) %>%
    as.data.frame() %>% tail(10)

# df.haccio <- read_csv("./logs_csv_output/hacc-io/all_darshan_ldms_hacc.csv")

# df.haccio %>%
#     filter(`#module` == "POSIX") %>%
#     filter(job_id == "256542") %>%
#     filter(op %in% c("read", "write")) %>%
#     group_by(rank) %>%
#     summarize(total_dur=sum(`seg:dur`),
#         maxtime = max(`seg:timestamp`),
#         tbytes=sum(`seg:len`)) %>%
#     arrange(maxtime) %>%
#     as.data.frame() %>% head(10)

# df.haccio %>%
#     filter(`#module` == "POSIX") %>%
#     filter(job_id == "256542") %>%
#     filter(op %in% c("read", "write")) %>%
#     group_by(rank) %>%
#     summarize(total_dur=sum(`seg:dur`),
#         maxtime = max(`seg:timestamp`),
#         tbytes=sum(`seg:len`)) %>%
#     arrange(maxtime) %>%
#     as.data.frame() %>% tail(10)

# df.haccio %>%
#     filter(`#module` == "POSIX") %>%
#     filter(job_id == "256576") %>%
#     filter(op %in% c("read", "write")) %>%
#     group_by(rank) %>%
#     summarize(total_dur=sum(`seg:dur`),
#         maxtime = max(`seg:timestamp`),
#         tbytes=sum(`seg:len`)) %>%
#     arrange(maxtime) %>%
#     as.data.frame() %>% head(10)

# df.haccio %>%
#     filter(`#module` == "POSIX") %>%
#     filter(job_id == "256576") %>%
#     filter(op %in% c("read", "write")) %>%
#     group_by(rank) %>%
#     summarize(total_dur=sum(`seg:dur`),
#         maxtime = max(`seg:timestamp`),
#         tbytes=sum(`seg:len`)) %>%
#     arrange(maxtime) %>%
#     as.data.frame() %>% tail(10)