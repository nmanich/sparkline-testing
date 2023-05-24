# Sparkline code for BBS trends
# Includes highlighted atlas windows
# Nicholas M. Anich 5/24/23
# A nod to B.D. Smith's sparkline code https://gist.github.com/bds/759838
# And Gabriel Foley for help with looping and random bugs

library(extrafont) # Adding Gill Sans based on its effectiveness at small font sizes
loadfonts(device="win")
font_import(pattern = "GIL", prompt = FALSE)  # Import Gill family
fonts()  # See what fonts are available
library(tidyverse)
library(cowplot) #for theme nothing

# this csv file has the first column YEAR starting at 1966 
# second column is called barpct and has 100 for atlas years and blank for non-atlas years
# all subsequent columns have a four letter code followed by the estimated yearly relative abundance from BBS

#pivot so individual species columns are combined into a
#longer column and relative abundance is listed in a single column beside each
#species name
df <- read.csv("2023TEST.csv") %>%
  pivot_longer(!c(YEAR, barpct), names_to = "species", values_to = "rel_abun")  
  
#list of species
list <- unique(df$species)
for (i in list) {
  df_bird <- df %>%
    filter(species == i)
  
#calculating highest and lowest for geom point
highest <- subset(df_bird, rel_abun == max(rel_abun))
lowest <- subset(df_bird, rel_abun == min(rel_abun))
  
#Extracting numbers for labels and rounding to nearest tenth 
highest2 <- data.frame(highest$rel_abun) %>% 
  mutate(highest.rel_abun = round(highest.rel_abun, 1))
lowest2 <- data.frame(lowest$rel_abun) %>% 
  mutate(lowest.rel_abun = round(lowest.rel_abun, 1))
  
# replace my placeholders 100 marking atlas years with the max rel abun
# to make the bar height the same as the rel abun height
maxrelabun <- as.character(highest$rel_abun)
df_bird$barpct <- as.character(df_bird$barpct) 
df_bird <- df_bird %>% 
mutate(barpct = str_replace(barpct, "100", maxrelabun))
df_bird$barpct <- as.numeric(as.character(df_bird$barpct))
maxrelabun <- as.numeric(highest$rel_abun)
 
file_name <- paste("plot_", i, ".eps", sep="")
  
# ggplot, prints the plot
# geom col is the bar chart
# geom line is the BBS trend
# points are the red and blue max and min
# text are the point labels
# theme nothing makes the plot axis etc vanish
t1 <- ggplot() + geom_col(data=df_bird, aes(x=YEAR, y=barpct), fill="pink", width = 1) +
geom_line(data = df_bird, aes(x=YEAR, y=rel_abun), size = 0.5) +
geom_point(data = lowest, aes(x=YEAR, y=rel_abun), size = 0.2, color = "red") + 
geom_point(data = highest, aes(x=YEAR, y=rel_abun), size = 0.2, color = "blue") +
geom_text(data = highest, aes(x=YEAR, y=rel_abun), label=highest2,  family = "Gill Sans MT") +
geom_text(data = lowest, aes(x=YEAR, y=rel_abun), label=lowest2, family = "Gill Sans MT") +
  coord_cartesian(
    xlim = c(1966, 2019),
    ylim = c(0, (maxrelabun)),
    expand = TRUE,
    default = FALSE,
    clip = "on" ) +
#theme_nothing() #blank
theme_half_open() # for testing

#old try at limits
#lims(x = c(1966,2019), y = c(0, (maxrelabun + 5))) +
   
# PREVIOUS POINT LABELS, CONTINUE TO EXPERIMENT HERE
#   geom_text(data = highest, aes(x=YEAR, y=rel_abun), label=highest2, position = position_nudge(y = 12), size  = 22, vjust="inward", hjust="inward", family = "Gill Sans MT") +
#   geom_text(data = lowest, aes(x=YEAR, y=rel_abun), label=lowest2, position = position_nudge(y = -12), size = 22, vjust="inward", hjust="inward", family = "Gill Sans MT") +
    
# for testing plot extent 
# Pull x axis limits for plot named t1 
# ggplot_build(t1)$layout$panel_scales_x[[1]]$range$range
   
# Pull y axis limits for plot named t1
# ggplot_build(t1)$layout$panel_scales_y[[1]]$range$range
   
# NEED TO FURTHER TWEAK PRINT DIMENSIONS, which can be done with width and height here
# the cairo_ps setting allows postscript to actually print Gill Sans
# print to file
ggplot2::ggsave(filename = paste0(i, "_bbs_spark.eps"),
                  plot = t1, 
                  device = cairo_ps, 
                  dpi = 1200, 
                  width = 9,
                  height = 6, 
                  units = "in")
  
   print(i)
}

warnings()
# this warning message is okay:
# Removed 43 rows containing missing values (position_stack). 
# It's removing blank values from the bar plot where no atlas was run
# Other warning messages may indicate problems
