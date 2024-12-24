# Sparkline code for BBS trends
# Includes highlighted atlas windows
# Nicholas M. Anich 5/24/23
# A nod to B.D. Smith's sparkline code https://gist.github.com/bds/759838
# And this dude Brian https://stackoverflow.com/questions/46200293/program-labels-on-line-graph-to-avoid-line-ggplot2
# And Gabriel Foley for help with looping and random bugs

# As of 12/2024 this is abandoned because it got shot down by the publisher

# It did occur to me, the best way to resize these would be to calculate the needed height scale, could be done outside R
# or at least as a separate process by looking at the range of the height needed
# and then pull in a matching table with the recommended height

library(extrafont) # Adding Gill Sans based on its effectiveness at small font sizes
loadfonts(device="win")
font_import(pattern = "GIL", prompt = FALSE)  # Import Gill family
fonts()  # See what fonts are available
library(tidyverse)
library(cowplot) #for theme nothing
library(ggrepel)#for label overlap
library(dplyr)# for adjust away from line

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
  
  
  # this is a function to keep labels off lines
  adjust_away_from_line <- function(df, x, y, vextend = 0.5) {
    if(!is.data.frame(df)) {return(df)}
    
    x <- enquo(x)
    y <- enquo(y)
    
    if(!(quo_name(x) %in% names(df))) {
      warning(paste0("Column '", quo_name(x), "' not found in data."))
      return(df)
    }
    
    if(!(quo_name(y) %in% names(df))) {
      warning(paste0("Column '", quo_name(y), "' not found in data."))
      return(df)
    }
    
    
    df %>% arrange(!!x) %>% 
      mutate(nb.slope = case_when(
        is.na(lead(!!y)) ~ (    (!!y) - lag(!!y))/(    (!!x) - lag(!!x)),
        is.na(lag(!!y))  ~ (lead(!!y) -    (!!y))/(lead(!!x) -    (!!x)),
        TRUE             ~ (lead(!!y) - lag(!!y))/(lead(!!x) - lag(!!x))  
      ),
      nb.pos = case_when(
        is.na(lead(!!y))                              ~ -sign(nb.slope),
        is.na(lag(!!y))                               ~ -sign(nb.slope),
        (lead(!!y) >= (!!y)) &  (lag(!!y) >= (!!y))  ~  1.1,
        !(lead(!!y) >= (!!y)) & !(lag(!!y) >= (!!y))  ~ -1.1,
        TRUE                                          ~ -1
      ),
      hjust = case_when(
        nb.pos   >  1 ~ 0.5,
        nb.pos   < -1 ~ 0.5,
        nb.slope >  0 ~ 1,
        nb.slope <  0 ~ 0,
        TRUE          ~ 0.5
      ),
      vjust = scales::rescale(round(nb.pos), to = c(0-vextend, 1+vextend))) %>% 
      select(-nb.slope, -nb.pos)
    
    
  }
  
  # ggplot, prints the plot
  # geom col is the bar chart
  # geom line is the BBS trend
  # points are the red and blue max and min
  # text are the point labels
  # theme nothing makes the plot axis etc vanish
  # scale_y_continuous allows for different y axes with different bird abundances
  # geom_text_repel keeps labels off points
  t1 <- df_bird %>%
    adjust_away_from_line(YEAR, rel_abun) %>%
  ggplot() + geom_col(data=df_bird, aes(x=YEAR, y=barpct), fill="pink", width = 1) +
    geom_line(data = df_bird, aes(x=YEAR, y=rel_abun), linewidth = 0.5) +
    geom_point(data = lowest, aes(x=YEAR, y=rel_abun), size = 0.2, color = "red") + 
    geom_point(data = highest, aes(x=YEAR, y=rel_abun), size = 0.2, color = "blue") +
    geom_text_repel(data = highest, aes(x=YEAR, y=rel_abun), label=highest2, segment.color = NA, family = "Gill Sans MT") + #nudge_y = -0.1 can help move label if still needed
    geom_text_repel(data = lowest, aes(x=YEAR, y=rel_abun), label=lowest2, segment.color = NA, family = "Gill Sans MT") +
    coord_cartesian(
      xlim = c(1966, 2019),
      ylim = c(0, (maxrelabun)),
      expand = TRUE,
      default = FALSE,
      clip = "on" ) +
    scale_y_continuous(expand=c(0.15,0)) + #scaling for differing y axes
    theme_nothing() #blank for spark
    #theme_half_open() # for testing
  
  # NEED TO FURTHER TWEAK PRINT DIMENSIONS, which can be done with width and height here
  # the cairo_ps setting allows postscript to actually print Gill Sans
  # print to file
  ggplot2::ggsave(filename = paste0(i, "_bbs_sparkTEST15.eps"),
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
