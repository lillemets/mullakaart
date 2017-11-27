# Töökausta määramine
setwd('C:/Users/lillemets/Dropbox/work/mullakaart')
setwd('/home/jrl/work/mullakaart')

# Pakettide laadimine
library('dplyr');library('extrafont');library('ggplot2');library('raster');library('mapproj');
library('openxlsx');library('gridExtra')

# Tõmmise laadimine
load('mullakaart.Rda')

# Andmete laadimine ja kohandamine ----------

## Objektid
load('C:/Users/lillemets/Dropbox/data/objects/maps/world.Rda') # Kaart
load('/home/jrl/data/objects/maps/world.Rda') # Kaart

## Saagikus
yld <- read.table('yield.txt', header = T)
yld$yield <- yld$yield * 1e3
yld <- yld %>% group_by(cnt) %>% summarise(yield = mean(yield)) %>% data.frame

world.df$yield <- cut(yld$yield[match(world.df$ISO_A2, yld$cnt)],
                      breaks = seq(2e3, 1e4, 2e3),
                      labels = c('2000 - 4000', '4001 - 6000', '6001 - 8000', '8001 - 10000'))

## Viljakus
download.file('http://www.fao.org/fileadmin/user_upload/soils/docs/HWSD/Soil_Quality_data/sq1.asc', 
              dest = 'sq1.asc')
soil <- raster('sq1.asc')
soil <- data.frame(rasterToPoints(soil))
names(soil) <- c('long', 'lat', 'nut')
soil <- soil[soil$nut != 0 & soil$nut != 7, ] # Eemaldada vesi

## Otsetoetused
pay <- readWorkbook('OT_mullakonverentsil.xlsx')
pay <- pay[, c(1, 4)]
names(pay) <- c('cnt', 'payments')

world.df$payments <- cut(pay$payments[match(world.df$ISO_A2, pay$cnt)],
                      breaks = seq(100, 500, 100),
                      labels = c('100 - 200', '201 - 300', '301 - 400', '401 - 500'))

## Salvestamine
save.image('mullakaart.Rda')


# Andmede kohandamine ----------

## Prantsusmaa kood parandada
levels(world.df$ISO_A2) <- c(levels(world.df$ISO_A2), 'FR')
world.df$ISO_A2[world.df$SOVEREIGNT == 'France'] <- 'FR'


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
        plot.margin = margin(0,0,0,0, 'pt'))
#baseplot + geom_path()

## Saagikus (Estonian Ministry of Rural Affairs)
saagikus <- baseplot + 
  geom_polygon(aes(fill = yield)) + 
  geom_path(color = 'grey40', size = .1) + 
  scale_fill_brewer(name = "kg/ha", 
                    palette = 'Greens', na.value = 'grey80') +
  labs(title = "Cereal yield (average of 2014 - 2016)", 
       caption = "Source: Estonian Ministry of Rural Affairs")
    
## Viljakus (Soil texture, soil organic carbon, soil pH, total exchangeable bases) (Food and Agriculture Organizaton of the United Nations, 2008)
#soil <- soil[sample(nrow(soil), nrow(soil) * .1), ]
viljakus <- baseplot + 
    geom_point(data = soil, aes(long, lat, color = as.factor(nut)), inherit.aes = F, size = .1) +
    geom_path(color = 'grey10', size = .1) + 
    scale_color_brewer(name = "Nutrient availability", 
                       labels = c("No or slight limitations", "Moderate limitations", 
                                  "Severe limitations", "Very severe limitations", 
                                  "Mainly non-soil", "Permafrost area"), 
                       palette = 'Oranges', direction = -1) + 
    labs(title = "Nutrient availability (2008)", 
         caption = "Source: Food and Agriculture Organizaton of the United Nations") + 
    guides(colour = guide_legend(override.aes = list(size = 2)))

## Otsetoetused
otsetoetused <- baseplot + 
  geom_polygon(aes(fill = payments)) + 
  geom_path(color = 'grey40', size = .1) + 
  scale_fill_brewer(name = "€/ha", 
                    palette = 'Blues', na.value = 'grey80') +
  labs(title = "Direct payments per hectare (2016)", 
       caption = "Source: Estonian Ministry of Rural Affairs")

# Save ----------
ggsave('kaardid.png', arrangeGrob(otsetoetused, saagikus, viljakus, nrow = 1), 
       width = 9, height = 3)