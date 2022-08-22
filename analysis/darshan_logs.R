###########################################################################
# IMPORTING R libraries for processing data and generating visualizations
# Install R packages as:
#  install.packages(c("dplyr", "tidyverse", "patchwork", "magick", "viridis"))
# Run as $ Rscript generating_plots_swfft.R INPUT_FILENAME
###########################################################################
options(crayon.enabled=FALSE)
library(dplyr)
library(tidyverse)  
library(magick)
library(arrow)
library(patchwork)
library(viridis)
options(scipen = 999)

magick_zize <- function(TARGET_FILE, plot_object, w=4, h=4) {
    ggsave(TARGET_FILE,
           plot=plot_object,
           width=w,
           height=h,
           units = "in",
           dpi = 100,
           limit = FALSE)
    m_png <- image_trim(image_read(TARGET_FILE))
    image_write(m_png, TARGET_FILE)
    print(paste("See:", TARGET_FILE))
}

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

###########################################################################
# Write all HACC CSV to same file

# read_logs <- function(FILE){
#     read_csv(FILE, col_types=cols()) %>%
#     mutate(filesystem = ifelse(grepl("nfs", FILE), "nfs", "lustre"))
# }

# csv_files <- list.files(path = "./logs_csv_output/hacc-io/csv/", pattern = "*.csv$", recursive = TRUE, all.files = TRUE, full.names = TRUE)
# if(length(csv_files) != 0) {    
#         lapply(csv_files, read_logs) %>%
#         bind_rows -> df.hacc
# }

# particles_5=c(256542, 256543, 256544, 256545, 256546, 256576, 256577, 256578, 256579, 256580)
# particles_10=c(256547, 256548, 256549, 256550, 256551, 256583, 256584, 256585, 256586, 256587)

# df.hacc.5 <- df.hacc %>% subset(job_id %in% particles_5) %>% mutate(particles=5)
# df.hacc.10 <- df.hacc %>% subset(job_id %in% particles_10) %>% mutate(particles=10)

# rbind(df.hacc.5, df.hacc.10) -> df.hacc

# write_csv(df.hacc, "./logs_csv_output/hacc-io/all_darshan_ldms_hacc.csv")

###########################################################################
# Write all MPIIO CSV to same file

# collec = c(256511, 256512, 256513, 256514, 256515, 
#     256522, 256523, 256524, 256525, 256534)
# nfs = c(256535, 256536, 256537, 256538, 256539,
#     256522, 256523, 256524, 256525, 256534)

# read_logs <- function(FILE){
#     read_csv(FILE, col_types=cols()) 
# }

# csv_files <- list.files(path = "./logs_csv_output/mpi-io/csv", pattern = "*.csv$", recursive = TRUE, all.files = TRUE, full.names = TRUE)
# csv_files
# if(length(csv_files) != 0) {    
#         lapply(csv_files, read_logs) %>%
#         bind_rows -> df.mpi
# }

# df.mpi %>%
#     mutate(collective = ifelse((job_id %in% collec), "collective", "nocollective")) %>%
#     mutate(filesystem = ifelse((job_id %in% nfs), "nfs", "lustre")) %>%
#     # Removing this unfinished job 
#     filter(job_id != 256526) -> df

# write_csv(df, "./logs_csv_output/mpi-io/all_darshan_ldms_mpi.csv")

###########################################################################
# READ INFO ABOUT HACC IO
# df.hacc <- read_csv("./logs_csv_output/hacc-io/all_darshan_ldms_hacc.csv")

# df.hacc
# df.hacc %>% distinct(ProducerName) %>% arrange(ProducerName)
# df.hacc %>% distinct(rank) %>% arrange(rank)
# df.hacc %>% distinct(`#module`)

# # The Darshan-LDMS connector has the same number of POSIX operations as Darshan
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
# df.mpiio <- read_csv("./logs_csv_output/mpi-io/all_darshan_ldms_mpi.csv")

# df.mpiio

# df.mpiio %>% distinct(ProducerName) %>% arrange(ProducerName)
# df.mpiio %>% distinct(rank) %>% arrange(rank)
# df.mpiio %>% distinct(`#module`)

# # # The Darshan-LDMS connector has the same number of MPIIO operations as Darshan
# df.mpiio %>%
#     filter(`#module` == "MPIIO") %>%
#     filter(op %in% c("read", "write")) %>%
#     group_by(job_id, ProducerName, rank, op) %>%
#     summarize(occurences=n()) %>% ungroup() %>% distinct(occurences)

# # There are 120 STDIO operations captured
# df.mpiio %>%
#     filter(`#module` == "STDIO") %>%
#     filter(op %in% c("read", "write")) 

# # There are 3,520,000 POSIX operations captured   
# df.mpiio %>%
#     filter(`#module` == "POSIX") %>%
#     filter(op %in% c("read", "write")) 

# ###########################################################################
# # PLOTS ABOUT HACC IO

# # 1. Number of operations
# df.hacc %>% 
#     filter(`#module` == "POSIX") %>%
#     mutate(application = paste(filesystem, particles, sep="")) %>%
#     group_by(op, job_id, application) %>%
#     summarize(n=n(),
#         std = sd(n),
#         variance = var(n)) %>% 
#     mutate(error=2*std/sqrt(n)) %>% #95%confidence
#     ggplot(aes(x=as.factor(application), y=n, fill=op)) +
#     geom_bar(stat="identity", color="black", size=0.3, position=position_dodge()) + 
#     geom_errorbar(aes(x=as.factor(application), ymin=n-error, ymax=n+error), width=0.2, 
#     position=position_dodge(.9), colour="red", alpha=0.9, size=0.5) +
#     my_theme() +
#     scale_y_continuous("Op. per execution", expand=c(0,0), lim=c(0,NA), breaks=seq(0,200000,1000)) +
#     scale_x_discrete("Application") +
#     ggtitle("HACC benchmark - POSIX operations") +
#     theme(axis.text.x = element_text(angle = 15, vjust=0.9),
#         axis.title.x=element_blank(), legend.title=element_blank()) +
#     scale_fill_viridis(discrete=TRUE, option="plasma") -> p

#     ggsave(filename=paste("./figs/", "new_operations_hacc.png", sep=""),
#     plot = p,
#     height=3.5,
#     width=5)

# # Reads and writes is: 4609
# # Closes and opens: 1026

# # df.hacc %>% 
#     filter(`#module` == "STDIO") %>%
#     mutate(application = paste(filesystem, particles, sep="")) %>%
#     group_by(op, job_id, application) %>%
#     summarize(n=n(),
#         std = sd(n),
#         variance = var(n)) %>% 
#     mutate(error=2*std/sqrt(n)) %>% #95%confidence
#     ggplot(aes(x=as.factor(application), y=n, fill=op)) +
#     geom_bar(stat="identity", color="black", size=0.3, position=position_dodge()) + 
#     geom_errorbar(aes(x=as.factor(application), ymin=n-error, ymax=n+error), width=0.2, 
#         position=position_dodge(.9), colour="red", alpha=0.9, size=0.5) +
#     my_theme() +
#     scale_y_continuous("Op. per execution", expand=c(0,0), lim=c(0,NA), breaks=seq(0,20000,500)) +
#     scale_x_discrete("Application") +
#     ggtitle("HACC benchmark - STDIO operations") +
#     theme(axis.text.x = element_text(angle = 15, vjust=0.9),
#         axis.title.x=element_blank(), legend.title=element_blank()) +
#     scale_fill_viridis(discrete=TRUE, option="plasma") -> p

#     ggsave(filename=paste("./figs/", "new_operations_hacc2.png", sep=""),
#     plot = p,
#     height=3.5,
#     width=5)

# # Opens: 1536
# # Writes: 22

# # 2. Difference in duration for each job operations
# df.hacc %>% 
#     filter(`#module` == "POSIX") %>%
#     mutate(application = paste(filesystem, particles, sep="")) %>%
#     filter(filesystem == "nfs") %>%
#     filter(op %in% c("read", "write")) %>%
#     group_by(job_id) %>%
#     mutate(node_id = as.integer(substr(ProducerName,6,8))) %>%
#     mutate(node = as.integer(factor(node_id))) %>%
#     ungroup() %>%
#     ggplot(aes(y=`seg:dur`, x=rank, colour=as.factor(application))) +
#     geom_point(size=0.5, alpha=0.8) + my_theme() + 
#     theme(panel.grid.major.x=element_blank(), legend.position = "none") +
#     scale_y_continuous("Duration (s)", expand=c(0,0), lim=c(0,NA), breaks=seq(0,10000,100)) +   
#     scale_x_continuous("Rank", expand=c(0,0), lim=c(0,NA), breaks=seq(0,1000,250)) + 
#     facet_grid(op~job_id, scales="free") +
#     ggtitle("HACC benchmark - POSIX operations for 5 and 10 million particles - NFS") +
#     labs(colour = "Job") + scale_colour_viridis(discrete=TRUE) -> p

#     ggsave(filename=paste("./figs/", "new_operations_hacc3_nfs.png", sep=""),
#     plot = p,
#     height=4,
#     width=12)

# # df.hacc %>% 
#     filter(`#module` == "POSIX") %>%
#     mutate(application = paste(filesystem, particles, sep="")) %>%
#     filter(filesystem == "lustre") %>%
#     filter(op %in% c("read", "write")) %>%
#     group_by(job_id) %>%
#     mutate(node_id = as.integer(substr(ProducerName,6,8))) %>%
#     mutate(node = as.integer(factor(node_id))) %>%
#     ungroup() %>%
#     ggplot(aes(y=`seg:dur`, x=rank, colour=as.factor(application))) +
#     geom_point(size=0.5, alpha=0.8) + my_theme() + 
#     theme(panel.grid.major.x=element_blank(), legend.position = "none") +
#     scale_y_continuous("Duration (s)", expand=c(0,0), lim=c(0,NA), breaks=seq(0,10000,100)) +   
#     scale_x_continuous("Rank", expand=c(0,0), lim=c(0,NA), breaks=seq(0,1000,250)) + 
#     facet_grid(op~job_id, scales="free") +
#     ggtitle("HACC benchmark - POSIX operations for 5 and 10 million particles - Lustre") +
#     labs(colour = "Job") + scale_colour_viridis(discrete=TRUE) -> p

#     ggsave(filename=paste("./figs/", "new_operations_hacc3_lustre.png", sep=""),
#     plot = p,
#     height=4,
#     width=12)

# # Weird behavior for 2 jobs using Lustre...

# # df.hacc %>% 
#     filter(`#module` == "POSIX") %>%
#     filter(filesystem == "lustre") %>%
#     filter(op %in% c("read", "write")) %>%
#     mutate(application = paste(filesystem, particles, sep="")) %>%
#     group_by(op, job_id, application) %>%
#     summarize(mean_dur = mean(`seg:dur`), n=n())

# # They have the same number of operations but very different durations
# #    op    job_id application mean_dur     n
# #    <chr>  <dbl> <chr>          <dbl> <int>
# #  1 read  256542 lustre5     116.      4609
# #  2 read  256543 lustre5       0.0844  4609
# #  3 read  256544 lustre5       0.0845  4609
# #  4 read  256545 lustre5     106.      4609

# # 3. Analyze temporally the weird behavior
# df.hacc %>% 
#     filter(`#module` == "POSIX") %>%
#     mutate(application = paste(filesystem, particles, sep="")) %>%
#     filter(application == "lustre5") %>%
#     filter(op %in% c("read", "write")) %>%
#     group_by(job_id) %>%
#     mutate(start = `seg:timestamp` - min(`seg:timestamp`)) %>%
#     mutate(end = start + `seg:dur`) %>% 
#     ggplot(aes(xmin=start, xmax=end, ymin=rank-0.2, ymax=rank+0.2, group=op)) +
#     geom_rect(alpha=0.8, size=0.2, color="#FDE725FF") + 
#     my_theme() + theme(panel.grid.major.x=element_blank(), 
#         legend.position="none",
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank()) +
#     scale_x_continuous("Execution Time (s)", expand=c(0,0), lim=c(0, NA), breaks=seq(0,5000,100)) + 
#     scale_y_continuous("Rank", expand=c(0,0), lim=c(0,NA), breaks=seq(0,700,150)) +
#     scale_size(range = c(0.2, 5)) + 
#     facet_grid(job_id~op) -> p

#     ggsave(filename=paste("./figs/", "new_operations_hacc3_lustre2.png", sep=""),
#     plot = p,
#     height=7,
#     width=7)

# No reason yet why this happened. It reflets the read durations captured in the Darshan logs,
# so it might be a Darshan problem

# ###########################################################################
# # PLOTS ABOUT MPI IO

# # 1. Number of operations
# df.mpiio %>% 
#     filter(`#module` == "MPIIO") %>%
#     group_by(op, job_id, collective) %>%
#     summarize(n=n(),
#         std = sd(n),
#         variance = var(n)) %>% 
#     mutate(error=2*std/sqrt(n)) %>% #95%confidence
#     ggplot(aes(x=as.factor(collective), y=n, fill=op)) +
#     geom_bar(stat="identity", color="black", size=0.3, position=position_dodge()) + 
#     geom_errorbar(aes(x=as.factor(collective), ymin=n-error, ymax=n+error), width=0.2, 
#     position=position_dodge(.9), colour="red", alpha=0.9, size=0.5) +
#     my_theme() +
#     scale_y_continuous("Op. per execution", expand=c(0,0), lim=c(0,NA), breaks=seq(0,200000,1000)) +
#     scale_x_discrete("Collective") +
#     ggtitle("MPIIO benchmark - MPIIO operations") +
#     theme(axis.text.x = element_text(angle = 15, vjust=0.9),
#         axis.title.x=element_blank(), legend.title=element_blank()) +
#     scale_fill_viridis(discrete=TRUE, option="plasma") -> p

#     ggsave(filename=paste("./figs/", "new_operations_mpiio.png", sep=""),
#     plot = p,
#     height=3.5,
#     width=5)

# df.mpiio %>% 
#     filter(`#module` == "STDIO") %>%
#     group_by(op, job_id, collective) %>%
#     summarize(n=n(),
#         std = sd(n),
#         variance = var(n)) %>% 
#     mutate(error=2*std/sqrt(n)) %>% #95%confidence
#     ggplot(aes(x=as.factor(collective), y=n, fill=op)) +
#     geom_bar(stat="identity", color="black", size=0.3, position=position_dodge()) + 
#     geom_errorbar(aes(x=as.factor(collective), ymin=n-error, ymax=n+error), width=0.2, 
#     position=position_dodge(.9), colour="red", alpha=0.9, size=0.5) +
#     my_theme() +
#     scale_y_continuous("Op. per execution", expand=c(0,0), lim=c(0,NA), breaks=seq(0,200000,1000)) +
#     scale_x_discrete("Collective") +
#     ggtitle("MPIIO benchmark - STDIO operations") +
#     theme(axis.text.x = element_text(angle = 15, vjust=0.9),
#         axis.title.x=element_blank(), legend.title=element_blank()) +
#     scale_fill_viridis(discrete=TRUE, option="plasma") -> p

#     ggsave(filename=paste("./figs/", "new_operations_mpiio2.png", sep=""),
#     plot = p,
#     height=3.5,
#     width=5)

# df.mpiio %>% 
#     filter(`#module` == "POSIX") %>%
#     group_by(op, collective) %>%
#     summarize(n=n(),
#         std = sd(n),
#         variance = var(n)) %>% 
#     mutate(error=2*std/sqrt(n)) %>% #95%confidence
#     ggplot(aes(x=as.factor(collective), y=n, fill=op)) +
#     geom_bar(stat="identity", color="black", size=0.3, position=position_dodge()) + 
#     geom_errorbar(aes(x=as.factor(collective), ymin=n-error, ymax=n+error), width=0.2, 
#     position=position_dodge(.9), colour="red", alpha=0.9, size=0.5) +
#     my_theme() +
#     scale_y_continuous("Op. per execution", expand=c(0,0), lim=c(0,NA), breaks=seq(0,10000000,200000)) +
#     scale_x_discrete("Collective") +
#     ggtitle("MPIIO benchmark - POSIX operations") +
#     theme(axis.text.x = element_text(angle = 15, vjust=0.9),
#         axis.title.x=element_blank(), legend.title=element_blank()) +
#     scale_fill_viridis(discrete=TRUE, option="plasma") -> p

#     ggsave(filename=paste("./figs/", "new_operations_mpiio3.png", sep=""),
#     plot = p,
#     height=3.5,
#     width=5)

# # 2. Difference in duration for each job operations
# df.mpiio %>% 
#     filter(`#module` == "MPIIO") %>%
#     filter(collective == "nocollective") %>%
#     filter(op %in% c("read", "write")) %>%
#     ggplot(aes(y=`seg:dur`, x=rank, colour=as.factor(filesystem))) +
#     geom_point(size=0.5, alpha=0.8) + my_theme() + 
#     theme(panel.grid.major.x=element_blank(), legend.position = "none") +
#     scale_y_continuous("Duration (s)", expand=c(0,0), lim=c(0,NA), breaks=seq(0,10000,100)) +   
#     scale_x_continuous("Rank", expand=c(0,0), lim=c(0,NA), breaks=seq(0,1000,250)) + 
#     facet_grid(op~job_id, scales="free") +
#     ggtitle("MPIIO operations for lustre and nfs filesystems") +
#     labs(colour = "Job") + scale_colour_viridis(discrete=TRUE) -> p

#     ggsave(filename=paste("./figs/", "new_operations_mpiio3_noncollective.png", sep=""),
#     plot = p,
#     height=4,
#     width=12)

# # 3. Analyze temporally the weird behavior

# df.mpiio %>% 
#     filter(`#module` == "MPIIO") %>%
#     filter(filesystem == "lustre") %>%
#     filter(collective == "nocollective") %>%
#     filter(op %in% c("read", "write")) %>%
#     group_by(job_id) %>%
#     mutate(start = `seg:timestamp` - min(`seg:timestamp`)) %>%
#     mutate(end = start + `seg:dur`) %>% 
#     ggplot(aes(xmin=start, xmax=end, ymin=rank-0.2, ymax=rank+0.2, group=op)) +
#     geom_rect(alpha=0.8, size=0.2, color="#541352FF") + 
#     my_theme() + theme(panel.grid.major.x=element_blank(), 
#         legend.position="none",
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank()) +
#     scale_x_continuous("Execution Time (s)", expand=c(0,0), lim=c(0, NA), breaks=seq(0,5000,1000)) + 
#     scale_y_continuous("Rank", expand=c(0,0), lim=c(0,NA), breaks=seq(0,700,150)) +
#     scale_size(range = c(0.2, 5)) + 
#     facet_grid(job_id~op) -> p

#     ggsave(filename=paste("./figs/", "new_operations_mpiio_lustre_nocollective.png", sep=""),
#     plot = p,
#     height=7,
#     width=7)

#######################################3
# Comparting Darshan profiler with manual data processing
# of data collected by LDMS

###########################################################################
# Read HACC 
df.hacc <- read_csv("./logs_csv_output/hacc-io/all_darshan_ldms_hacc.csv")

# Execution time of each job
# df.hacc %>% 
#     group_by(job_id, particles, `#module`, filesystem) %>%
#     summarize(n=n(), 
#         mintime=min(`seg:timestamp` - `seg:dur`), 
#         maxtime=max(`seg:timestamp`)) %>%
#     mutate(total_dur = maxtime - mintime) %>%
#     select(filesystem, particles, job_id, `#module`, total_dur) %>%
#     arrange(job_id) %>%
#     as.data.frame()

# Operations captured by Collector
# df.hacc %>% 
#     distinct(op)

# Operations per rank
# df.hacc %>% 
#     filter(op %in% c("read", "write")) %>%
#     group_by(job_id, particles, `#module`, filesystem, rank, op) %>%
#     summarize(n=n()) %>%
#     arrange(job_id, -n) %>%
#     filter(n!=9) %>%
#     as.data.frame()
# All have the same number for POSIX operations, for STDIO no, 
# but Darshan does not report STDIO info

# Fastest/Slowest for POSIX op - the intuitive approach
# df.hacc %>% 
#     filter(`#module` == "POSIX") %>%
#     filter(job_id == "256542") %>%
#     group_by(job_id, rank, `#module`) %>%
#     summarize(n=n(), 
#         mintime=min(`seg:timestamp` - `seg:dur`), 
#         maxtime=max(`seg:timestamp`)) %>%
#     mutate(total_dur = maxtime - mintime) %>%
#     arrange(job_id, maxtime) %>%
#     select(job_id, rank, total_dur) %>%
#     as.data.frame()

# Fastest/Slowest for POSIX op - cumulative read, write, and meta times
# The metadata we have are times with opens, but even considering it 
# still shows different results
# df.hacc %>% 
#     filter(type == "MET") %>%
#     filter(job_id == "256542") %>%
#     distinct(`#module`) 

# df.hacc %>% 
#     filter(type == "MET") %>%
#     filter(job_id == "256542") %>%
#     distinct(op) 

# df.hacc %>% 
#     filter(`#module` == "POSIX") %>%
#     filter(job_id == "256542") %>%
#     filter(op %in% c("read", "write")) %>%
#     group_by(job_id, rank) %>%
#     summarize(n=n(), 
#         u_ops = toString(unique(op)),
#         distinct_operations = paste(u_ops, collapse = ', '),
#         total_dur = sum(`seg:dur`),
#         total_bytes = sum(`seg:len`)) %>%
#     arrange(job_id, total_dur) %>%
#     select(job_id, rank, distinct_operations, total_dur, total_bytes) %>%
#     as.data.frame()

# # For all, as reported is calculated to Darshan
# df.hacc %>% 
#     filter(`#module` == "POSIX") %>%
#     filter(job_id == "256542") %>%
#     group_by(job_id, rank, `#module`) %>%
#     summarize(n=n(), 
#         u_ops = toString(unique(op)),
#         distinct_operations = paste(u_ops, collapse = ', '),
#         total_dur = sum(`seg:dur`),
#         total_bytes = sum(`seg:len`)) %>%
#     arrange(job_id, total_dur) %>%
#     select(job_id, `#module`, rank, distinct_operations, total_dur, total_bytes) %>%
#     tail(6) %>%
#     as.data.frame() 

# # For all, as reported is calculated to Darshan
# df.hacc %>% 
#     filter(`#module` == "POSIX") %>%
#     filter(job_id == "256542") %>%
#     group_by(job_id, `#module`, rank) %>%
#     summarize(n=n(), 
#         u_ops = toString(unique(op)),
#         distinct_operations = paste(u_ops, collapse = ', '),
#         total_dur = sum(`seg:dur`),
#         total_bytes = sum(`seg:len`)) %>%
#     arrange(job_id, total_dur) %>%
#     select(job_id, `#module`, rank, distinct_operations, total_dur, total_bytes) %>%
#     head(6) %>%
#     as.data.frame() 

# # For all, as reported is calculated to Darshan
# df.hacc %>% 
#     filter(`#module` == "POSIX") %>%
#     filter(job_id == "256576") %>%
#     group_by(job_id, rank, `#module`) %>%
#     summarize(n=n(), 
#         u_ops = toString(unique(op)),
#         distinct_operations = paste(u_ops, collapse = ', '),
#         total_dur = sum(`seg:dur`),
#         total_bytes = sum(`seg:len`)) %>%
#     arrange(job_id, total_dur) %>%
#     select(job_id, `#module`, rank, distinct_operations, total_dur, total_bytes) %>%
#     tail(6) %>%
#     as.data.frame() 

# # For all, as reported is calculated to Darshan
# df.hacc %>% 
#     filter(`#module` == "POSIX") %>%
#     filter(job_id == "256576") %>%
#     group_by(job_id, `#module`, rank) %>%
#     summarize(n=n(), 
#         u_ops = toString(unique(op)),
#         distinct_operations = paste(u_ops, collapse = ', '),
#         total_dur = sum(`seg:dur`),
#         total_bytes = sum(`seg:len`)) %>%
#     arrange(job_id, total_dur) %>%
#     select(job_id, `#module`, rank, distinct_operations, total_dur, total_bytes) %>%
#     head(6) %>%
#     as.data.frame() 

# Cumulative time spent read/writing at the POSIX level
# df.hacc %>% 
#     filter(`#module` == "POSIX") %>%
#     filter(job_id == "256542") %>%
#     group_by(job_id, op, ProducerName) %>%
#     summarize(sum_duration = sum(`seg:dur`)) %>%
#     as.data.frame() 

# df.hacc %>% 
#     filter(`#module` == "POSIX") %>%
#     filter(job_id == "256542") %>%
#     filter(type == "MET") %>%
#     group_by(job_id, op) %>%
#     summarize(sum_duration = sum(`seg:dur`)) %>%
#     as.data.frame() 

# Duration of the slowest/fastest individual POSIX read/write
# df.hacc %>% 
#     filter(`#module` == "POSIX") %>%
#     filter(job_id == "256542") %>%
#     filter(op == "write") %>%
#     arrange(-`seg:dur`) %>%
#     select(op, `seg:dur`)

# # Duration of the slowest/fastest individual POSIX read/write
# df.hacc %>% 
#     filter(`#module` == "POSIX") %>%
#     filter(job_id == "256542") %>%
#     filter(op != "close") %>%
#     group_by(rank) %>%
#     summarize(total_dur = sum(`seg:dur`)) %>%
#     arrange(total_dur)

# total_STDIO_F_SLOWEST_RANK_TIME:
# df.hacc %>%
#     filter(`#module` == "STDIO") %>%
#     filter(job_id == "256542") %>%
#     filter(op == "write") %>%
#     arrange(-`seg:dur`) %>%
#     select(rank, op, type) %>%
#     as.data.frame()

# df.hacc %>%
#     filter(job_id == "256542") %>%
#     mutate(Start = `seg:timestamp` - `seg:dur`) %>%
#     mutate(End = `seg:timestamp`) %>%
#     group_by(job_id) %>%
#     mutate(End = End - min(Start),
#         Start = Start - min(Start)) %>%
#     ungroup() %>%
#     filter(op=="open") %>%
#     filter(`#module` == "POSIX") %>%
#     head(1) %>%
#     as.data.frame()

# Get consecutive POSIX operations (read/write)
# df.hacc %>%
#     filter(job_id == "256542") %>%
#     filter(`#module` == "POSIX") %>%
#     select(op, rank, `seg:off`, ProducerName) %>%
#     group_by(gr = cumsum(op != lag(op, 
#         default = first(op)))) %>%
#     mutate(count = n()) %>%
#     as.data.frame()

# Timestamp of the first operation execution
# df.hacc %>%
#     filter(job_id == "256542") %>%
#     filter(`seg:timestamp` != 0) %>%
#     select(`seg:timestamp`) %>%
#     head(1)

# df.hacc %>%
#     filter(job_id == "256542") %>%
#     mutate(Start = `seg:timestamp` - `seg:dur`) %>%
#     mutate(End = `seg:timestamp`) %>%
#     mutate(End = End - min(Start),
#          Start = Start - min(Start)) %>%
#     mutate(Duration = End - Start) %>%
#     select(`#module`, op, Start, End, Duration) %>%
#     filter(`#module` == "POSIX") %>%
#     arrange(Duration)

################################# START
# total_POSIX_F_WRITE_START_TIMESTAMP
# Using their start timestamp as zero
# df.hacc %>%
#     filter(job_id == "256542") %>%
#     mutate(end = `seg:timestamp` - 1658512320) %>%
#     mutate(start =  end - `seg:dur`) %>%
#     select(`#module`, rank, op, job_id, start, `seg:timestamp`) %>%
#     filter(op == "write") %>%
#     filter(`#module` == "POSIX") %>%
#     head(1) %>%
#     as.data.frame() 

# # total_POSIX_F_READ_START_TIMESTAMP
# df.hacc %>%
#     filter(job_id == "256542") %>%
#     mutate(end = `seg:timestamp` - 1658512320) %>%
#     mutate(start =  end - `seg:dur`) %>%
#     select(`#module`, rank, op, job_id, start, `seg:timestamp`) %>%
#     filter(op == "read") %>%
#     filter(`#module` == "POSIX") %>%
#     head(1) %>%
#     as.data.frame() 

# # total_POSIX_F_OPEN_START_TIMESTAMP
# df.hacc %>%
#     filter(job_id == "256542") %>%
#     mutate(end = `seg:timestamp` - 1658512320) %>%
#     mutate(start =  end - `seg:dur`) %>%
#     select(`#module`, rank, op, job_id, start, `seg:timestamp`) %>%
#     filter(op == "open") %>%
#     filter(`#module` == "POSIX") %>%
#     head(1) %>%
#     as.data.frame() 

# # total_POSIX_F_CLOSE_START_TIMESTAMP
# df.hacc %>%
#     filter(job_id == "256542") %>%
#     mutate(end = `seg:timestamp` - 1658512320) %>%
#     mutate(start =  end - `seg:dur`) %>%
#     select(`#module`, rank, op, job_id, start, `seg:timestamp`) %>%
#     filter(op == "close") %>%
#     filter(`#module` == "POSIX") %>%
#     head(1) %>%
#     as.data.frame() 

# ################################# END
# # total_POSIX_F_WRITE_END_TIMESTAMP
# df.hacc %>%
#     filter(job_id == "256542") %>%
#     mutate(end = `seg:timestamp` - 1658512320) %>%
#     select(`#module`, rank, op, job_id, end, `seg:timestamp`) %>%
#     filter(op == "write") %>%
#     filter(`#module` == "POSIX") %>%
#     arrange(-end) %>%
#     head(1) %>%
#     as.data.frame() 

# # total_STDIO_F_READ_START_TIMESTAMP
# df.hacc %>%
#     filter(job_id == "256542") %>%
#     mutate(end = `seg:timestamp` - 1658512320) %>%
#     select(`#module`, rank, op, job_id, end, `seg:timestamp`) %>%
#     filter(op == "read") %>%
#     arrange(-end) %>%
#     filter(`#module` == "POSIX") %>%
#     head(1) %>%
#     as.data.frame() 

# # total_STDIO_F_OPEN_END_TIMESTAMP
# df.hacc %>%
#     filter(job_id == "256542") %>%
#     filter(`seg:timestamp` != 0) %>%
#     mutate(end = `seg:timestamp` - 1658512320) %>%
#     select(`#module`, rank, op, job_id, end, `seg:timestamp`) %>%
#     filter(op == "open") %>%
#     filter(`#module` == "POSIX") %>%
#     arrange(-end) %>%
#     head(1) %>%
#     as.data.frame() 

# # total_STDIO_F_CLOSE_END_TIMESTAMP
# df.hacc %>%
#     filter(job_id == "256542") %>%
#     mutate(end = `seg:timestamp` - 1658512320) %>%
#     select(`#module`, rank, op, job_id, end, `seg:timestamp`) %>%
#     filter(op == "close") %>%
#     filter(`#module` == "POSIX") %>%
#     arrange(-end) %>%
#     head(1) %>%
#     as.data.frame() 

    # ggplot(aes(ymin=rank-0.5, ymax=rank+0, xmin=start, xmax=end+0.5)) +
    # geom_rect(color="black", size=0.03, alpha=1) + my_theme() + 
    # theme(panel.grid.major.x=element_blank()) +
    # ggtitle("POSIX write operations for HACC-IO - Lustre 256542") +
    # scale_x_continuous("Execution time", expand=c(0,0), breaks=seq(0,1000,25)) + 
    # scale_y_continuous("Ranks", expand=c(0,0)) -> p

    # ggsave(filename=paste("./figs/", "haccio_ganttchart.png", sep=""),
    # plot = p,
    # height=4,
    # width=10)

################################# END
# total_POSIX_F_WRITE_END_TIMESTAMP
# df.hacc %>%
#     filter(job_id == "256542") %>%
#     filter(`#module` == "POSIX") %>% 
#     group_by(job_id, `#module`, op) %>%
#     summarize(total_duration = sum(`seg:dur`)) %>%
#     as.data.frame() 

################################# total_POSIX_F_MAX_READ_TIME
df.hacc %>%
    filter(job_id == "256542") %>%
    filter(`#module` == "POSIX") %>% 
    filter(op == "read") %>%
    arrange(-`seg:dur`) %>%
    select(`#module`, op, ProducerName, rank, max_byte, rank, type, `seg:len`, `seg:dur`) %>%
    head(3) %>%
    as.data.frame() 

df.hacc %>%
    filter(job_id == "256542") %>%
    filter(`#module` == "POSIX") %>% 
    filter(op == "write") %>%
    select(`#module`, op, ProducerName, rank, max_byte, rank, type, `seg:len`, `seg:dur`) %>%
    arrange(-`seg:dur`) %>%
    head(3) %>%
    as.data.frame() 