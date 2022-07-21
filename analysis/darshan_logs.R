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

###########################################################################
# Read CSV file
read_csv("./logs_csv_output/hacc_io_all_7_20.csv")-> hacc

# Got start and end time from the Darshan file
# Get the traces related to this execution

# HACC_luster_5.256460.darshan.txt
# hacc %>% 
#     filter(`seg:timestamp` >= "1658339316") %>% 
#     filter(`seg:timestamp` <= "1658339737") %>% 
#     mutate(date = as.POSIXct(`seg:timestamp`, origin = "1970-01-01")) %>%
#     arrange(date) %>% 
#     select(date, ProducerName, rank, file, everything()) -> df

# df %>% 
#     filter(op %in% c("read", "write")) %>%
#     group_by(rank, op) %>%
#     summarize(n=n()) %>% as.data.frame()


# HACC_luster_10.256251.darshan
# hacc %>% 
#     filter(`seg:timestamp` >= "1658341358") %>% 
#     filter(`seg:timestamp` <= "1658341852") %>% 
#     mutate(date = as.POSIXct(`seg:timestamp`, origin = "1970-01-01")) %>%
#     arrange(date) %>% 
#     select(date, ProducerName, rank, file, everything()) -> df

# df %>% 
#     filter(op %in% c("read", "write")) %>%
#     group_by(rank, op) %>%
#     summarize(n=n()) %>% 
#     filter(n!=9) %>% as.data.frame() 

# HACC_nfs_5.256461.darshan.txt
hacc %>% 
    filter(`seg:timestamp` >= "1658340546") %>% 
    filter(`seg:timestamp` <= "1658341133") %>% 
    select(ProducerName, rank, file, everything()) -> df
df %>% 
    filter(op %in% c("read", "write")) %>%
    group_by(rank, op) %>%
    summarize(n=n()) %>% 
    filter(n!=9) %>% as.data.frame() 
df %>% distinct(rank)

# HACC_nfs_10.256463.darshan.txt
hacc %>% 
    filter(`seg:timestamp` >= "1658342139") %>% 
    filter(`seg:timestamp` <= "1658343312") %>% 
    # mutate(date = as.POSIXct(`seg:timestamp`, origin = "1970-01-01")) %>%
    # arrange(date) %>% 
    select(ProducerName, rank, file, everything()) -> df

df %>% 
    filter(op %in% c("read", "write")) %>%
    group_by(rank, op) %>%
    summarize(n=n()) %>% 
    filter(n!=9) %>% as.data.frame() 

df %>% distinct(rank)