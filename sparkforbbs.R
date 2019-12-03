library("sparkTable")

#make a spreadsheet with 3 column headers: time, variable, and value - these correspond to year, species, and relative abundance
# get numbers from USGS Hierarchical model here: https://www.mbr-pwrc.usgs.gov/bbs/trend/tf15.shtml

df <- read.csv(file="REVIFORSPARK3.csv", header=TRUE, sep=",", row.names=NULL)

# this sets up the sparkline
pop_ges <- df $ value [ df$ variable =="REVI"]
sline <- newSparkLine (value = pop_ges)

#sets various size and dimension parameters
#it does okay without these in height and width with the exception of apparently cutting off the dot edges
# and it looks kind of thick and having trouble with the padding, the padding doesn't seem to increase as it should?
sline <- setParameter(sline, type='lineWidth', value=0.1)
sline <- setParameter(sline, type='width', value=1.5)
sline <- setParameter(sline, type='pointWidth', value=.3)
sline <- setParameter(sline, type='height', value=.3)
sline <- setParameter(sline, type='padding', value=c(35,35,35,35))

# was playing around with some way to highlight WBBA I era points but it doesn't look like you can customize point color of just any point
# could fix this for all graphs by making the background of the graph have a horizontal bar through these years (and WBBA II era points)

# this exports to eps or png or pdf
export(sline, outputType="eps", filename="sparklineREVI")
