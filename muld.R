# Töökausta määramine
setwd('C:/Users/lillemets/Dropbox/work/mullakaart')
setwd('/home/jrl/work/mullakaart')

# Pakettide laadimine
library('rgdal');library('foreign');library('broom');library('dplyr')

# Read data
soil <- readOGR(dsn = 'ESDB', layer = 'SGDB_PTR')

# Spatial vector to data frame
soil <- tidy(soil, region = 'AGLIM1')

# Add values to AGLIM1
codes <- read.csv(textConnection(
'id, value
0, No information
1, No limitation to agricultural use
2, Gravelly
3, Stony
4, Lithic
5, Concretionary
6, Petrocalcic
7, Saline
8, Sodic
9, Glaciers and snow-caps
10, Soils disturbed by man
11, Fragipans
12, Excessively drained
13, Almost always flooded
14, Eroded phase
15, Phreatic phase
16, Duripan
17, Petroferric horizon
18, Permafrost'))
soil$aglim <- codes$value[match(soil$id, codes$id)]

# Save
save(soil, file = 'soil.Rda')