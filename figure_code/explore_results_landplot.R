#####################################################################################
### Quick plots to look at the landscape orientation of urban area and tree cover ###
#####################################################################################

ggforest <- (R0.home.s.gg.t.f %>% filter(
#  disease == "Yellow Fever"
  disease == "Dengue"
#, dens    == unique(R0.home.s.gg.t.f$dens)
, dens    == 1
, land    == 1
#, Host    == "Human"
, Host    == "Human"
, starting_host == "human"
  )) %>% {
  ggplot(., aes(X, Y, z = forest_mean)) + 
  scale_fill_distiller(palette = "Spectral") +
  geom_raster(aes(fill = forest_mean)) +
  xlab("Km") + 
  ylab("Km") + 
  guides(fill = guide_colorbar(title = "Forest")) +
  facet_wrap(~land) +
 # scale_x_continuous(breaks = c(0, 3, 6, 9 , 12)) +
 # scale_y_continuous(breaks = c(0, 3, 6, 9 , 12)) +
  theme(
    legend.key.size = unit(.45, "cm")
  , legend.title = element_text(size = 12)
  , legend.text = element_text(size = 14)
  , legend.background = element_blank()
  , axis.text.x = element_text(size = 12) 
  , axis.text.y = element_text(size = 12) 
  )
  }

gghumans <- (R0.home.s.gg.t.f %>% filter(
#  disease == "Yellow Fever"
  disease == "Dengue"
#, dens    == unique(R0.home.s.gg.t.f$dens)
, dens    == 1
, land    == 1
#, Host    == "Human"
, Host    == "Human"
, starting_host == "human"
  )) %>% {
  ggplot(., aes(X, rev(Y), z = urban)) + 
  scale_fill_distiller(palette = "Spectral") +
  geom_raster(aes(fill = log(urban))) +
  xlab("Km") + 
  ylab("Km") + 
  guides(fill = guide_colorbar(title = "Log Urban")) +
  facet_wrap(~land) +
 # scale_x_continuous(breaks = c(0, 3, 6, 9 , 12)) +
 # scale_y_continuous(breaks = c(0, 3, 6, 9 , 12)) +
  theme(
    legend.key.size = unit(.45, "cm")
  , legend.title = element_text(size = 12)
  , legend.text = element_text(size = 14)
  , legend.background = element_blank()
  , axis.text.x = element_text(size = 12) 
  , axis.text.y = element_text(size = 12) 
  )
  }

