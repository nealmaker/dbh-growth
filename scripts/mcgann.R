load("../big-rdas/dbh-growth-model-op.rda")
load("../cruise/rda/mcgann-south-cruise-2019.rda")
library("maps")
library("tidyverse")
library("caret")
library("extrafont")

weird <- trees[50,] %>% 
  select(spp, dbh, cr, bal, ht, ba, forest_type, site_class, lat, lon) %>% 
  mutate(elev = 1200)

# Replicate across geographic grid 
weird <- expand.grid(spp = weird$spp, 
                     dbh = weird$dbh, 
                     cr = weird$cr, 
                     bal = weird$bal, 
                     ht = weird$ht, 
                     ba = weird$ba, 
                     forest_type = weird$forest_type, 
                     site_class = weird$site_class, 
                     elev = weird$elev,
                     lat = seq(43.9, 46.8, length.out = 30),
                     lon = seq(-75.2, -67.7, length.out = 30))

# Get growth rates for different stocking levels
weird_10 <- mutate(weird, bal = 0, ba = 10)

weird_40 <- mutate(weird, bal = 0, ba = 40)

weird_90 <- mutate(weird, bal = 0, ba = 90)

weird_130 <- mutate(weird, bal = 0, ba = 130)

weird <- weird %>% 
  mutate(dbh_rate = predict(dbh_growth_model_op, newdata = weird),
         dbh_rate_10 = predict(dbh_growth_model_op, newdata = weird_10),
         dbh_rate_40 = predict(dbh_growth_model_op, newdata = weird_40),
         dbh_rate_90 = predict(dbh_growth_model_op, newdata = weird_90),
         dbh_rate_130 = predict(dbh_growth_model_op, newdata = weird_130),
         diff = dbh_rate_10 - dbh_rate_90)

# Map differences
weird %>% ggplot(aes(lon, lat, color = diff)) + 
  geom_point(size = 3) +
  geom_polygon(data = map_data("state"),
               aes(x = long, y = lat, group = group),
               fill = NA, col = "gray 25") +
  scale_color_distiller(type = "div",
                        direction = 1,
                        palette = 5,
                        name = "growth increase \nfrom release") +
  coord_fixed(xlim = c(-76.8, -67), ylim = c(42.4, 47.5), ratio = 1.3) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        text = element_text(family = "Perpetua"))
