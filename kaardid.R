# Töökausta määramine
setwd('C:/Users/lillemets/Dropbox/work/mullakaart')
setwd('/home/jrl/work/mullakaart')

# Pakettide laadimine
library('dplyr');library('extrafont');library('ggplot2');library('raster');library('mapproj');library('openxlsx');library('gridExtra')

# Tõmmise laadimine
load('mullakaart.Rda')

# Andmete laadimine ja kohandamine ----------

## Objektid
load('C:/Users/lillemets/Dropbox/data/objects/maps/world.Rda') # Kaart
world.df <- readRDS('/home/jrl/data/objects/maps/world.Rds') # Kaart

## Prantsusmaa kood parandada
levels(world.df$ISO_A2) <- c(levels(world.df$ISO_A2), 'FR')
world.df$ISO_A2[world.df$SOVEREIGNT == 'France'] <- 'FR'

## Otsetoetused
pay <- readWorkbook('OT_mullakonverentsil.xlsx')
pay <- pay[, c(1, 4)]
names(pay) <- c('cnt', 'payments')

world.df$payments <- cut(pay$payments[match(world.df$ISO_A2, pay$cnt)],
                      breaks = seq(100, 500, 50),
                      labels = c('100 - 150', '151 - 200', '201 - 250', '251 - 300', '301 - 350', 
                                 '351 - 400', '401 - 450', '451 - 500'))

## Saagikus
#download.file('https://ec.europa.eu/agriculture/sites/agriculture/files/market-observatory/crops/doc/cereals-production_en.xlsx', dest = 'cereals-production_en.xlsx')
yld <- readWorkbook('cereals-production_en.xlsx', sheet = 2)
yld <- yld[yld$Type == 'Yield' & yld$Product == 'Total cereals', ]
yld$yield <- rowMeans(yld[, c('2014', '2015', '2016')])
yld <- yld[, c('Member.State', 'yield')]
yld$yield <- yld$yield * 1e3
names(yld)[names(yld) == 'Member.State'] <- 'cnt'

world.df$yield <- cut(yld$yield[match(world.df$SOVEREIGNT, yld$cnt)],
                      breaks = seq(1e3, 1e4, 1e3),
                      labels = c('1000 - 2000', '2000 - 3000', '3001 - 4000', '4001 - 5000', 
                                 '5001 - 6000', '6000 - 7000', '7001 - 8000', '8001 - 9000', 
                                 '9001 - 10000'))

## Muld
load('soil.Rda')
soil$id <- as.numeric(soil$id)
soil$aglim <- as.character(soil$aglim)
soil$aglim[soil$id == 0] <- NA
soil$aglim[soil$id == 1] <- 'Not limited'
soil$aglim[soil$id > 1] <- 'Limited'
world.df$na <- NA # Joonisel puuduvate riikide värvimiseks

## Salvestamine
save.image('mullakaart.Rda')


# Jooniste ilmutamine ----------

## Põhi
baseplot <- 
  ggplot(world.df) + aes(long, lat, group = group) + 
  coord_map(53, 9, projection = 'lambert', 
            xlim = c(-10, 35), ylim = c(34, 66)) + 
  theme(text = element_text(family = 'Roboto Condensed', size = 10), 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        legend.background = element_rect(fill = 'white', color = 'gray60', size = .1),
        legend.margin = margin(4,4,4,4, 'pt'), 
        legend.key = element_rect(color = 'gray60', size = .1), 
        legend.key.size = unit(10, 'pt'), 
        legend.text = element_text(size = 8), 
        legend.title = element_text(),
        legend.position = c(.01, .99),
        legend.justification = c(0, 1),
        panel.background = element_rect(fill = '#e6f2ff'),
        panel.border = element_rect(colour = 'gray60', fill = NA, size = .1),
        panel.grid = element_blank(),
        plot.background = element_blank(),
        plot.title = element_text(size = 14), 
        plot.caption = element_text(size = 10), 
        plot.margin = margin(2,2,2,2, 'pt'))

## Otsetoetused
otsetoetused <- baseplot + 
  geom_polygon(aes(fill = payments)) + 
  geom_path(color = 'grey40', size = .1) + 
  scale_fill_brewer(name = "€/ha", 
                    palette = 'Reds', na.value = 'grey80') +
  labs(title = "Direct payments per hectare of UAA (2016)", 
       #subtitle = paste("EU28:", format(pay[pay$cnt == 'EL28', 'payments'], digits = 0), "€/ha"), 
       caption = "Source: Commission Delegated Regulation (EU) No 1378/2014") + 
  annotate(geom = "text", x = 30, y = 3,
           label = paste("EU28:", format(pay[pay$cnt == 'EL28', 'payments'], digits = 0), "€/ha"),
           hjust = 0)

## Saagikus
saagikus <- baseplot + 
  geom_polygon(aes(fill = yield)) + 
  geom_path(color = 'grey40', size = .1) + 
  scale_fill_brewer(name = "kg/ha", 
                    palette = 'Greens', na.value = 'grey80') +
  labs(title = "Cereal yield (2014 - 2016 average)", 
       #subtitle = paste("EU28:", format(yld[yld$cnt == 'EU-28', 'yield'], digits = 0), "kg/ha"), 
       caption = "Source: DG Agriculture and Rural Development")

## Muld
muld <- baseplot + 
    geom_polygon(aes(fill = na)) + 
    geom_polygon(data = soil, aes(long, lat, group = group, fill = aglim), 
                 inherit.aes = F, size = .1) +
    geom_path(color = 'black', size = .2) + 
    scale_fill_brewer(name = "Agricultural use", 
                      palette = 'Paired', direction = -1, na.value = 'grey80') + 
    labs(title = "Limitations to agricultural use of land (2001)", 
         caption = "Source: European Soil Data Centre (ESDC)")


# Salvestamine ----------
ggsave('kaardid.png', arrangeGrob(otsetoetused, saagikus, muld, nrow = 1), 
       scale = 1.6, width = 9, height = 3)

wb <- createWorkbook()
addWorksheet(wb, "Payments per hectare (€/ha)")
writeDataTable(wb, sheet = 1, pay, colNames = F)
addWorksheet(wb, "Cereal yield per UAA (kg/ha)")
writeDataTable(wb, sheet = 2, yld, colNames = F)
saveWorkbook(wb, file = 'Andmed kaartidel.xlsx', overwrite = T)
