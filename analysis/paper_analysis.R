###########################################################################
# IMPORTING R libraries for processing data and generating visualizations
# Install R packages as:
#  install.packages(c("dplyr", "tidyverse", "patchwork", "magick", "viridis"))
# Run as $ Rscript generating_plots_swfft.R INPUT_FILENAME
###########################################################################
options(crayon.enabled=FALSE)
library(dplyr)
library(tidyverse)  
library(patchwork)
library(magick)
library(viridis)
library(arrow)

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
            legend.text = element_text(color = "black", size = 12),                                                                              
            panel.grid.minor = element_blank(),                                                                                                  
            strip.text.x = element_text(size = 12),                                                                                              
            strip.text.y = element_text(size = 12),                                                                                              
            legend.box.spacing = unit(0, "pt"))                                                                                                  
}

# Read parquet file
args = commandArgs(trailingOnly=TRUE)
df <- read_parquet(args)

df %>%
    group_by(job_id) %>%
    mutate(endtime = as.POSIXct(timestamp, origin="1970-01-01")) %>%
    mutate(starttime = endtime-dur) %>% select(-timestamp) %>%
    mutate(s = starttime - min(starttime)) %>%
    mutate(e = endtime - min(starttime)) %>%
    ungroup() -> df.time

df.time %>%
    rename(`I/O Operation Size [bytes]` = len) %>%
    ggplot(aes(ymin=rank-0.1, ymax=rank+0.1, xmin=s, xmax=e, fill=`I/O Operation Size [bytes]`)) +
    geom_rect(color="black", size=0.03, alpha=1) + my_theme() + theme(panel.grid.major.x=element_blank()) +
    ggtitle("Open operations for mpi_io_test_lC.parquet") +
    scale_x_continuous("Execution time", expand=c(0,0)) + 
    scale_y_continuous("Ranks", expand=c(0,0)) +
    facet_wrap(~job_id, ncol=1) +
    scale_fill_gradientn(colours=mako(20)) -> p0

    TARGET_FILE <- paste("./../figs/", "SWFFT_ganttchart.png", sep="")
    magick_zize(TARGET_FILE,
              p0, h=8, w=10)

 df %>% group_by(job_id) %>%
    mutate(time = timestamp - min(timestamp)) -> df.new

# write_parquet(df.new, "./../data/hacc_io_new.parquet")
 