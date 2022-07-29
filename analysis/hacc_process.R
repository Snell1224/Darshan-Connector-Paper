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
# read_csv(paste(FILEPATH,"hacc_lustre_10.csv",sep="", collapse=NULL)) %>%
#     mutate(application = "hacc_lustre10") -> hacc.l.10
# read_csv(paste(FILEPATH,"hacc_lustre_5.csv",sep="", collapse=NULL)) %>%
#     mutate(application = "hacc_lustre5") -> hacc.l.5
# read_csv(paste(FILEPATH,"hacc_nfs_10.csv",sep="", collapse=NULL)) %>%
#     mutate(application = "hacc_nfs10") -> hacc.nfs.10
# read_csv(paste(FILEPATH,"hacc_nfs_5.csv",sep="", collapse=NULL)) %>%
#     mutate(application = "hacc_nfs5") -> hacc.nfs.5

# hacc.l.10
# hacc.l.5
# hacc.nfs.10
# hacc.nfs.5

# rbind(hacc.l.10,hacc.l.5,hacc.nfs.10,hacc.nfs.5) -> df

# write_csv(df, paste(FILEPATH,"hacc_all.csv",sep="", collapse=NULL))

####################################################################
# Read all HACC files
FILEPATH="./../data/files-per-app/"
read_csv(paste(FILEPATH,"hacc_all.csv",sep="", collapse=NULL)) -> df

###########################################################################
###########################################################################
# Plots to show different number of operations in each reptition of the same program 
# and also between nodes

df %>% 
    filter(module == "POSIX") %>%
    filter(application=="hacc_lustre10") %>%
    group_by(op, job_id) %>%
    summarize(n=n()) %>% 
    ggplot(aes(x=as.factor(job_id), y=n, fill=op)) +
    geom_bar(stat="identity", color="black", size=0.3, position=position_dodge()) + 
    my_theme() +
    scale_y_continuous("I/O requests", expand=c(0,0), lim=c(0,NA), breaks=seq(0,200000,200)) +
    scale_x_discrete("Application") +
    ggtitle("HACC benchmark - POSIX operations") +
    theme(axis.text.x = element_text(angle = 15, vjust=0.9),
        axis.title.x=element_blank(), legend.title=element_blank()) +
    scale_fill_viridis(discrete=TRUE, option="plasma") -> p

    ggsave(filename=paste("./../figs/", "/checking-number-op/hacc_lustre10.png", sep=""),
    plot = p,
    height=4.5,
    width=6)


df %>% 
    filter(module == "POSIX") %>%
    filter(application=="hacc_lustre10") %>%
    filter(job_id == "255515") %>%
    group_by(op, job_id, ProducerName) %>%
    summarize(n=n(),
        std = sd(n),
        variance = var(n)) %>% 
    mutate(error=2*std/sqrt(n)) %>% #95%confidence
    ggplot(aes(x=as.factor(ProducerName), y=n, fill=op)) +
    geom_bar(stat="identity", color="black", size=0.3, position=position_dodge()) + 
    geom_errorbar(aes(x=as.factor(ProducerName), ymin=n-error, ymax=n+error), width=0.5, 
        position=position_dodge(.9), colour="red", alpha=0.9, size=0.5) +
    my_theme() +
    scale_y_continuous("I/O requests", expand=c(0,0), lim=c(0,NA), breaks=seq(0,200000,20)) +
    scale_x_discrete("Application") +
    ggtitle("HACC benchmark - Lustre - 10 million particles") +
    theme(axis.text.x = element_text(angle = 15, vjust=0.9),
        axis.title.x=element_blank(), legend.title=element_blank()) +
    scale_fill_viridis(discrete=TRUE, option="plasma") -> p

    ggsave(filename=paste("./../figs/", "/checking-number-op/hacc_lustre10_255515.png", sep=""),
    plot = p,
    height=4,
    width=8)

# Plot made for the paper
# Rename jobs to shorter name
job_id_names <- c("job1", "job2", "job3", "job4", "job5")
names(job_id_names) <- c(255686,255687,255688,255689,255685)

df %>% 
    group_by(op, application) %>%
    mutate(duration = mean(dur),
        std = sd(dur),
        variance = var(dur),
        n = n()) %>% 
    mutate(error=2*std/sqrt(n)) %>% #95%confidence
    ggplot(aes(x=as.factor(application), y=duration, fill=op)) +
    geom_bar(stat="identity", color="black", size=0.3, position=position_dodge()) + 
    geom_errorbar(aes(x=as.factor(application), ymin=duration-error, ymax=duration+error), width=0.2, 
        position=position_dodge(.9), colour="red", alpha=0.9, size=0.5) +
    my_theme() +
    scale_y_continuous("Mean duration (s)", expand=c(0,0), lim=c(0,900), breaks=seq(0,1000,200)) +
    scale_x_discrete("Application") +
    theme(axis.text.x = element_text(angle = 15, vjust=0.8),
        legend.title=element_blank()) +
    scale_fill_viridis(labels=c('Close', 'Open', 'Read', 'Write'), discrete=TRUE) -> p

    ggsave(filename=paste("./../figs/", "/checking-number-op/operations_hacc_duration.pdf", sep=""),
    plot = p,
    height=3,
    width=5)