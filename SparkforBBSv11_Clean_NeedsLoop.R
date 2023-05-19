# Sparkline code for BBS trends
# Includes highlighted atlas windows
# Nicholas M. Anich 5/18/23
# A nod to B.D. Smith's sparkline code https://gist.github.com/bds/759838
# And Gabriel Foley for help with looping

library(extrafont) # Adding Gill Sans based on its effectiveness at small font sizes
loadfonts(device="win")
font_import(pattern = "GIL", prompt = FALSE)  # Import Gill family
fonts()  # See what fonts are available
library(tidyverse) # for ggplot2 and dplyr
library(cowplot) # for theme nothing

# this csv file has the first column YEAR starting at 1966 
# second column is called barpct and has 100 for atlas years and blank for non-atlas years
# all subsequent columns have a four letter code followed by the estimated yearly relative abundance from BBS
df <- read.csv("2023TEST.csv")

# round all but the first 2 columns
df <- df %>% mutate_at(vars(-barpct, -YEAR), funs(round(., 1)))

#calculating highest and lowest for geom point
highest <- subset(df, REVI == max(REVI))
lowest <- subset(df, REVI == min(REVI))

#Extracting numbers for labels
highest2 <- data.frame(highest$REVI)
lowest2 <- data.frame(lowest$REVI)

# ggplot, prints the plot
# geom col is the bar chart
# geom line is the BBS trend
# points are the red and blue max and min
# text are the point labels
# theme nothing makes the plot axis etc vanish
t1 <- ggplot() + geom_col(data=df, aes(x=YEAR, y=barpct), fill="pink", width = 1) +
                 geom_line(data = df, aes(x=YEAR, y=REVI), size = 0.5) +
                 geom_point(data = lowest, aes(x=YEAR, y=REVI), size = 0.2, color = "red") + 
                 geom_point(data = highest, aes(x=YEAR, y=REVI), size = 0.2, color = "blue") +
                 geom_text(data = highest, aes(x=YEAR, y=REVI), label=highest2, position = position_nudge(y = 12), size  = 2, vjust="inward", hjust="inward", family = "Gill Sans MT") +
                 geom_text(data = lowest, aes(x=YEAR, y=REVI), label=lowest2, position = position_nudge(y = -12), size = 2, vjust="inward", hjust="inward", family = "Gill Sans MT") +
                 theme_nothing()

# this will generate an error message about removing 43 rows, but those are the NA values for non-atlas years
# for the bar chart so it's expected

# NEED TO FURTHER TWEAK DIMENSIONS, which can be done with width and height here
# the cairo_ps setting allows postscript to actually print Gill Sans
# print to file
ggplot2::ggsave(filename = "tPlot.eps", 
                plot = t1, 
                device = cairo_ps, 
                dpi = 1200, 
                width = 3,
                height = 2, 
                units = "cm")






