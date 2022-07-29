###########################################################################
# IMPORTING R libraries for processing data and generating visualizations
# Install R packages as:
#  install.packages(c("dplyr", "tidyverse", "patchwork", "magick", "arrow", "viridis"))
###########################################################################
options(crayon.enabled=FALSE)
library(dplyr)
library(tidyverse)  
library(magick)
library(arrow)
library(viridis)

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
      theme_bw(base_size=16) +                                                                                                                   
      theme(panel.background = element_blank(),                                                                                                  
            legend.box.margin = margin(0,0,0,0),                                                                                                 
            legend.spacing = unit(0, "pt"),                                                                                                      
            legend.position = "top",                                                                                                             
            legend.text = element_text(color = "black", size = 14),                                                                              
            # panel.grid.minor.x = element_blank(),                    
            strip.text.y = element_text(size = 16),            
            panel.spacing.x = unit(0.2, "lines"),
            panel.spacing.y = unit(0.8, "lines"),
            strip.background = element_rect(fill="#F5F5F5", color="black", size=0.1, linetype="solid"),                                                                                                                                                               
            legend.box.spacing = unit(0, "pt"))                                                                                                         
}

####################################################################
# Write all HACC files
# FILEPATH="./../data/files-per-app/"
# read_csv(paste(FILEPATH,"mpi_io_luster_withcollective.csv",sep="", collapse=NULL)) %>%
#     mutate(application = "mpi_io_luster_withcollective") -> mpi.l.c
# read_csv(paste(FILEPATH,"mpi_io_luster_withoutcollective.csv",sep="", collapse=NULL)) %>%
#     mutate(application = "mpi_io_luster_withoutcollective") -> mpi.l.nc
# read_csv(paste(FILEPATH,"mpi_io_nfs_withcollective.csv",sep="", collapse=NULL)) %>%
#     mutate(application = "mpi_io_nfs_withcollective") -> mpi.nfs.c
# read_csv(paste(FILEPATH,"mpi_io_nfs_withoutcollective.csv",sep="", collapse=NULL)) %>%
#     mutate(application = "mpi_io_nfs_withoutcollective") -> mpi.nfs.nc

# mpi.l.c
# mpi.l.nc
# mpi.nfs.c
# mpi.nfs.nc

# rbind(mpi.l.c,mpi.l.nc,mpi.nfs.c,mpi.nfs.nc) -> df

# write_csv(df, paste(FILEPATH,"mpiio_all.csv",sep="", collapse=NULL))

####################################################################
# Read all HACC files
FILEPATH="./../data/files-per-app/"
read_csv(paste(FILEPATH,"mpiio_all.csv",sep="", collapse=NULL)) -> df

###########################################################################
###########################################################################
# Plots to show different number of operations in each repetition of the same program 
# and also between nodes

df %>% 
    filter(module == "MPIIO") %>%
    filter(application=="mpi_io_luster_withoutcollective") %>%
    group_by(op, job_id) %>%
    summarize(n=n()) %>% 
    ggplot(aes(x=as.factor(job_id), y=n, fill=op)) +
    geom_bar(stat="identity", color="black", size=0.3, position=position_dodge()) + 
    my_theme() +
    scale_y_continuous("I/O requests", expand=c(0,0), lim=c(0,NA), breaks=seq(0,200000,500)) +
    scale_x_discrete("Application") +
    ggtitle("MPIIO benchmark - MPIIO operations") +
    theme(axis.text.x = element_text(angle = 15, vjust=0.9),
        axis.title.x=element_blank(), legend.title=element_blank()) +
    scale_fill_viridis(discrete=TRUE, option="plasma") -> p

    ggsave(filename=paste("./../figs/", "/checking-number-op/mpi_io_luster_withoutcollective.png", sep=""),
    plot = p,
    height=4.5,
    width=6)


df %>%
    filter(module=="MPIIO") %>%
    filter(job_id == "255653") %>%
    mutate(node_id = as.integer(substr(ProducerName,6,8))) %>%
    mutate(node = as.integer(factor(node_id))) %>%
    subset(op %in% c("read", "write")) %>%
    ggplot(aes(y=dur, x=rank, colour=node)) +
    geom_point(size=0.3, alpha=0.8) + my_theme() + theme(panel.grid.major.x=element_blank()) +
    scale_y_continuous("Duration (s)", expand=c(0,0), lim=c(0,NA), breaks=seq(0,10000,50)) +   
    scale_x_continuous("Rank", expand=c(0,0), lim=c(0,NA), breaks=seq(0,1000,200)) + 
    facet_wrap(~op, nrow=1) +
    labs(colour = "Node") + scale_colour_viridis() -> p

    ggsave(filename=paste("./../figs/", "/checking-number-op/255653_mpi_io_luster_no_coll_duration.pdf", sep=""),
    plot = p,
    height=3,
    width=6)


###########################################################################
# job_id_names <- c("job_id 1", "job_id 2", "job_id 3", "job_id 4", "job_id 5")
# names(job_id_names) <- c("255652","255653", "255654", "255655", "255656")

# op_names <- c("Read", "Write")
# names(op_names) <- c("read","write")

# Write CSV file
# df1 %>%
#     filter(job_id == "255653") %>%
#     subset(op %in% c("read", "write")) %>%
#     ggplot(aes(y=dur, x=rank, colour=node)) +
#     geom_point(size=0.3, alpha=0.5) + my_theme() + theme(panel.grid.major.x=element_blank()) +
#     # ggtitle("MPI I/O benchmark - opens duration without collective operations") +
#     scale_y_continuous("Duration (s)", expand=c(0,0), lim=c(0,NA), breaks=seq(0,10000,50)) +   
#     scale_x_continuous("Rank", expand=c(0,0), lim=c(0,NA), breaks=seq(0,1000,200)) + 
#     facet_wrap(~op, labeller = labeller(job_id = job_id_names),nrow=1) +
#     labs(colour = "Node") + scale_colour_viridis() -> p

#     ggsave(filename=paste("./../figs/", "255653_mpi_io_luster_no_coll_duration.pdf", sep=""),
#     plot = p,
#     height=3,
#     width=6)

# df1 %>%
#     subset(op %in% c("read", "write")) %>%
#     ggplot(aes(y=dur, x=rank, colour=node)) +
#     geom_point(size=0.8, alpha=0.5) + my_theme() + theme(panel.grid.major.x=element_blank()) +
#     # ggtitle("MPI I/O benchmark - opens duration without collective operations") +
#     scale_y_continuous("Duration (s)", expand=c(0,0), lim=c(0,NA), breaks=seq(0,10000,100)) +   
#     scale_x_continuous("Rank", expand=c(0,0), lim=c(0,NA), breaks=seq(0,1000,250)) + 
#     facet_grid(op~job_id,labeller = labeller(job_id = job_id_names, op=op_names)) +
#     labs(colour = "Node") + scale_colour_viridis() -> p

#     ggsave(filename=paste("./../figs/", "mpi_io_luster_no_coll_duration_allexperiments.pdf", sep=""),
#     plot = p,
#     height=5,
#     width=6)

# df1 %>%
#     subset(op %in% c("read", "write")) %>%
#     ggplot(aes(y=dur, x=node, colour=node)) +
#     geom_point(size=0.8, alpha=0.5) + my_theme() + theme(panel.grid.major.x=element_blank()) +
#     # ggtitle("MPI I/O benchmark - opens duration without collective operations") +
#     scale_y_continuous("Duration (s)", expand=c(0,0), lim=c(0,NA), breaks=seq(0,10000,100)) +   
#     scale_x_continuous("Node", expand=c(0,0), lim=c(0,NA), breaks=seq(0,1000,250)) + 
#     facet_grid(op~job_id,labeller = labeller(job_id = job_id_names, op=op_names)) +
#     labs(colour = "Node") + scale_colour_viridis() -> p

#     ggsave(filename=paste("./../figs/", "mpi_io_luster_no_coll_duration_allexperiments_node.pdf", sep=""),
#     plot = p,
#     height=5,
#     width=7)

# # Write CSV file no color
# df1 %>%
#     filter(job_id == "255653") %>%
#     subset(op %in% c("read", "write")) %>%
#     ggplot(aes(y=dur, x=rank, colour=job_id)) +
#     geom_point(size=0.3, alpha=0.5) + my_theme() + theme(panel.grid.major.x=element_blank()) +
#     # ggtitle("MPI I/O benchmark - opens duration without collective operations") +
#     scale_y_continuous("Duration (s)", expand=c(0,0), lim=c(0,NA), breaks=seq(0,10000,50)) +   
#     scale_x_continuous("Rank", expand=c(0,0), lim=c(0,NA), breaks=seq(0,1000,200)) + 
#     facet_wrap(~op, labeller = labeller(job_id = job_id_names),nrow=1) + theme(legend.position="none") +
#     labs(colour = "Node") + scale_colour_viridis() -> p

#     ggsave(filename=paste("./../figs/", "255653_mpi_io_luster_no_coll_duration_nocolor.pdf", sep=""),
#     plot = p,
#     height=2.3,
#     width=6)

# df1 %>%
#     filter(job_id == "255653") %>%
#     subset(op %in% c("read", "write")) %>%
#     mutate(start = timestamp - min(timestamp)) %>%
#     mutate(end = start + dur) %>% 
#     ggplot(aes(ymin=rank-0.45, ymax=rank+0.35, xmin=start, xmax=end, fill=as.factor(job_id))) +
#     geom_rect() + my_theme() + theme(panel.grid.major.x=element_blank()) +
#     # ggtitle("MPI I/O benchmark - opens without collective operations") +
#     scale_x_continuous("Execution Time (s)", expand=c(0,0), lim=c(0, NA), breaks=seq(0,500,100)) + 
#     scale_y_continuous("Rank", expand=c(0,0), lim=c(0,NA), breaks=seq(0,600,200)) +
#     facet_wrap(~op, nrow=1) +
#     theme(legend.position = "none") +
#     scale_fill_viridis(discrete=TRUE) -> p

#     ggsave(filename=paste("./../figs/", "255653_mpi_io_luster_no_coll_execution.pdf", sep=""),
#     plot = p,
#     height=4,
#     width=5.5)

# df1 %>%
#     filter(job_id == "255653") %>%
#     rename(duration = dur) %>%
#     subset(op %in% c("read", "write")) %>% 
#     mutate(start = timestamp - min(timestamp)) %>%
#     mutate(end = start + duration) %>%
#     select(job_id, node, op, rank, timestamp, start, end, duration) %>%
#     arrange(-duration) %>%
#     group_by(node, rank, op) %>%
#     summarize(n=n()) %>% 
#     as.data.frame() %>%
#     filter(node==1) %>%
#     arrange(op, rank)

# df1 %>%
#     filter(job_id == "255653") %>%
#     filter(rank == 16) %>%
#     filter(node==1) %>%
#     rename(duration = dur) %>%
#     subset(op %in% c("write")) %>% 
#     mutate(start = timestamp - min(timestamp)) %>%
#     mutate(end = start + duration) %>%
#     select(job_id, node, op, record_id, rank, timestamp, start, end, duration) %>%
#     arrange(-duration)

# df1 %>%
#     filter(job_id == "255653") %>%
#     filter(node == 1) %>%
#     mutate(start = timestamp - min(timestamp)) %>%
#     mutate(end = start + dur) %>% 
#     filter(start > 50) %>%
#     filter(op=="write") %>%
#     ggplot(aes(xmin=start, xmax=end, ymin=rank-0.45, ymax=rank+0.45, group=op, fill=node)) +
#     geom_rect(alpha=0.1, size=0.1, aes(colour=node)) + 
#     my_theme() + theme(panel.grid.major.x=element_blank()) +
#     scale_x_continuous("Execution Time (s)", expand=c(0,0), lim=c(60, 270), breaks=seq(0,500,50)) + 
#     scale_y_continuous("Rank", expand=c(0,0), lim=c(0,NA), breaks=seq(0,600,1)) +
#     scale_size(range = c(0.2, 5)) +
#     facet_wrap(~op, nrow=2, labeller = labeller(op=op_names), strip.position="right") +
#     theme(legend.position="none") -> p

#     ggsave(filename=paste("./../figs/", "255653_mpi_io_luster_no_coll_execution2.png", sep=""),
#     plot = p,
#     height=5.5,
#     width=5)

# df1 %>%
#     filter(job_id == "255653") %>%
#     filter(node == 1) %>%
#     mutate(start = timestamp - min(timestamp)) %>%
#     mutate(end = start + dur) %>% 
#     filter(op=="write") %>%
#     ggplot(aes(xmin=start, xmax=end, ymin=rank-0.45, ymax=rank+0.45, group=op, fill=node)) +
#     geom_rect(alpha=0.1, size=0.1, aes(colour=node)) + 
#     my_theme() + theme(panel.grid.major.x=element_blank()) +
#     scale_x_continuous("Execution Time (s)", expand=c(0,0), lim=c(0, NA), breaks=seq(0,500,50)) + 
#     scale_y_continuous("Rank", expand=c(0,0), lim=c(0,NA), breaks=seq(0,600,1)) +
#     scale_size(range = c(0.2, 5)) +
#     facet_wrap(~op, nrow=2, labeller = labeller(op=op_names), strip.position="right") +
#     theme(legend.position="none") -> p

#     ggsave(filename=paste("./../figs/", "255653_mpi_io_luster_no_coll_execution2_all.png", sep=""),
#     plot = p,
#     height=5.5,
#     width=5)

# df1 %>%
#     filter(job_id == "255653") %>%
#     mutate(start = timestamp - min(timestamp)) %>%
#     mutate(end = start + dur) %>% 
#     subset(op %in% c("read", "write")) %>% 
#     ggplot(aes(xmin=start, xmax=end, ymin=rank-0.45, ymax=rank+0.45, group=op, fill=node)) +
#     geom_rect(alpha=0.1, size=0.1, aes(colour=node)) + 
#     my_theme() + theme(panel.grid.major.x=element_blank()) +
#     scale_x_continuous("Execution Time (s)", expand=c(0,0), lim=c(0, NA), breaks=seq(0,600,100)) + 
#     scale_y_continuous("Rank", expand=c(0,0), lim=c(0,NA), breaks=seq(0,700,100)) +
#     scale_size(range = c(0.2, 5)) +
#     facet_wrap(~op, nrow=2, labeller = labeller(op=op_names), strip.position="right") -> p

#     ggsave(filename=paste("./../figs/", "255653_mpi_io_luster_no_coll_execution_all.png", sep=""),
#     plot = p,
#     height=5.5,
#     width=5)