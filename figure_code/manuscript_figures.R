############################################
### Main text figures for the manuscript ###
############################################

## Code for figures (but not saved Rds files to create the code -- see explore_results_kernel.R for details)

################################
## Methods: Conceptual Figure ##
################################

## Many of these panels are made from within the simulation script
## 1) run top_level_script.R up through run_sim_kernel.R with landscape_counter <- 7, disease_counter   <- 1
## 2) Use the saved landscape list and the code in run_sim_kernel.R to make the following panels
## Panel B: 
 ## Plot the whole landscape
## Panel C:
 ## Plot just 20:30, 30:40
## Panel D: 
 ## Run the model for i = 6, j = 6
 ## Use h_m_mat to plot the infected mosquitoes
## Panel E: 
 ## Run the model for i = 6, j = 6
 ## Use m_h_mat to plot the infected humans and others

## Panel B: 
landB.urban  <- melt(landscape[[landscape_counter]][,,"urban"])
landB.forest <- melt(landscape[[landscape_counter]][,,"forest"])

names(landB.urban)  <- c("X", "Y", "Urban_Intensity")
names(landB.forest) <- c("X", "Y", "Tree_Cover")

landB.urban  <- landB.urban %>% mutate(X = X * 200 / 1000, Y = Y * 200 / 1000)
landB.forest <- landB.forest %>% mutate(X = X * 200 / 1000, Y = Y * 200 / 1000)

## For Panel C need X from 16:26 and Y from 34:44, which equates to 4:6 and 6:8
landB.urban   <- landB.urban  %>% filter(X >= 3.2, X <= 5.2, Y >= 6.8, Y <= 8.8)
landB.forest  <- landB.forest %>% filter(X >= 3.2, X <= 5.2, Y >= 6.8, Y <= 8.8)

rect.plot <- data.frame(
  x1 = 4
, x2 = 6
, y1 = 6
, y2 = 8
, Urban_Intensity = 0
)

  ggplot(
#    landB.urban
   landB.forest
    , aes(X, Y
#   , z = Urban_Intensity
    , z = Tree_Cover
    )) + 
  scale_fill_gradient(
#  low = "white", high = "tan4"
   low = "white", high = "forestgreen"  
  ) +
  geom_raster(aes(
#   fill = Urban_Intensity
    fill = Tree_Cover
    )) +
  xlab("Km") + 
  ylab("Km") + 
#  guides(fill = guide_colorbar(
#  title = "Tree 
#Cover")) +
  guides(fill = guide_colorbar(
  title = "Urban
Intensity")) +
#  scale_x_continuous(breaks = c(0, 3, 6, 9, 12)) +
# scale_y_continuous(breaks = c(0, 3, 6, 9, 12)) +
  scale_x_continuous(breaks = c(3, 4, 5)) +
  scale_y_continuous(breaks = c(7, 8, 9)) +
  theme(
    legend.key.size = unit(.65, "cm")
  , legend.title = element_text(size = 16)
  , legend.text = element_text(size = 14)
  , legend.background = element_blank()
  , axis.text.x = element_text(size = 22) 
  , axis.text.y = element_text(size = 22) 
  , axis.title.x = element_text(size = 22) 
  , axis.title.y = element_text(size = 22)
 # , panel.border = element_blank()
 # , panel.background = element_blank()
   , legend.position = "none"
  )
  
ggsave(filename = "/Users/Morgan/Desktop/panel.pdf", plot = last_plot(), width = 3.52,  height = 3.7, units = c("in"))

## Panel D: 
panelD         <- melt(h_m_mat[,,2])
names(panelD)  <- c("X", "Y", "Infected_Mosquitoes")
panelD         <- panelD %>% mutate(X = X * 200 / 1000, Y = Y * 200 / 1000)
panelD         <- panelD %>% filter(X >= 3.2, X <= 5.2, Y >= 6.8, Y <= 8.8)

  ggplot(
   panelD
    , aes(X, Y
    , z = Infected_Mosquitoes
    )) + 
  scale_fill_gradient(
  low = "white", high = "firebrick3"
  ) +
  geom_raster(aes(
   fill = Infected_Mosquitoes
    )) +
  xlab("Km") + 
  ylab("Km") + 
  guides(fill = guide_colorbar(
  title = "Infected
Aedes
albopictus")) +
#  guides(fill = guide_colorbar(
#  title = "Urban
#Intensity")) +
  scale_x_continuous(breaks = c(3, 4, 5)) +
  scale_y_continuous(breaks = c(7, 8, 9)) +
  theme(
    legend.key.size = unit(.65, "cm")
  , legend.title = element_text(size = 16)
  , legend.text = element_text(size = 14)
  , legend.background = element_blank()
  , axis.text.x = element_text(size = 22) 
  , axis.text.y = element_text(size = 22) 
  , axis.title.x = element_text(size = 22) 
  , axis.title.y = element_text(size = 22)
 # , panel.border = element_blank()
 # , panel.background = element_blank()
   , legend.position = "none"
  )
  
ggsave(filename = "/Users/Morgan/Desktop/panel.pdf", plot = last_plot(), width = 3.52,  height = 3.7, units = c("in"))

## Panel E: 
panelE         <- melt(m_h_mat.s[,,2])
names(panelE)  <- c("X", "Y", "Infected_Hosts")
panelE         <- panelE %>% mutate(X = X * 200 / 1000, Y = Y * 200 / 1000)
panelE         <- panelE %>% filter(X >= 3.2, X <= 5.2, Y >= 6.8, Y <= 8.8)

  ggplot(
   panelE
    , aes(X, Y
    , z = Infected_Hosts
    )) + 
  scale_fill_gradient(
  low = "white", high = "dodgerblue4"
  ) +
  geom_raster(aes(
   fill = Infected_Hosts
    )) +
  xlab("Km") + 
  ylab("Km") + 
  guides(fill = guide_colorbar(
  title = "Exposed
Non-
Humans")) +
#  guides(fill = guide_colorbar(
#  title = "Urban
#Intensity")) +
  scale_x_continuous(breaks = c(3, 4, 5)) +
  scale_y_continuous(breaks = c(7, 8, 9)) +
  theme(
    legend.key.size = unit(.65, "cm")
  , legend.title = element_text(size = 16)
  , legend.text = element_text(size = 14)
  , legend.background = element_blank()
  , axis.text.x = element_text(size = 22) 
  , axis.text.y = element_text(size = 22) 
  , axis.title.x = element_text(size = 22) 
  , axis.title.y = element_text(size = 22)
 # , panel.border = element_blank()
 # , panel.background = element_blank()
  # , legend.position = "none"
  )
  
ggsave(filename = "/Users/Morgan/Desktop/panel.pdf", plot = last_plot(), width = 3.52,  height = 3.7, units = c("in"))

#################################################
## Results Section One -- Simulated Landscapes ##
#################################################

#########################
## Figure 1: Summary of average FOI on a given landscape for each disease across landscape features 
#########################

# FOI_on.s.f <- readRDS("Figure_Rds/disease_summary.Rds"); FOI_on.s.f <- FOI_on.s.f[[3]]

FOI_on.s.f.s <- FOI_on.s.f %>%
  group_by(disease, land, dens
    , starting_host
    #, X, Y
    ) %>%
#  summarize(
#   R0 = sum(R0)
#  ) %>% 
#  ungroup(X, Y) %>% 
  summarize(
    mean_R0 = mean(FOI_on_H)
  , var_R0  = var(FOI_on_H)
  , lwr_R0  = quantile(FOI_on_H, 0.025)
  , mid_R0  = quantile(FOI_on_H, 0.500)
  , upr_R0  = quantile(FOI_on_H, 0.975)
  )

FOI_on.s.f.s <- FOI_on.s.f.s %>%
  pivot_longer(cols = ends_with("R0"), names_to = "Statistic", values_to = "Estimate")

FOI_on.s.f.s$dens <- as.factor(FOI_on.s.f.s$dens)  
cc                <- scales::seq_gradient_pal("dodgerblue", "black", "Lab")(seq(0,1,length.out=6))
FOI_on.s.f.s.w    <- FOI_on.s.f.s %>% pivot_wider(names_from = Statistic, values_from = Estimate)

FOI_on.s.f$land <- round(FOI_on.s.f$land, 2)
  
## Landscape-level summary across landscape heterogeneity with lwr and upr ranges
{ggplot(FOI_on.s.f.s.w, aes(land, mid_R0)) + 
 # geom_ribbon(aes(ymin = lwr_R0, ymax = upr_R0, fill = dens), alpha = 0.1) +
  geom_line(aes(colour = dens, linetype = starting_host), lwd = 1) +
#  scale_colour_manual(values = cc, name = "Average human
#population density
#(people / sq km)") +
  scale_colour_brewer(palette = "PuOr", name = "Average human
population density
(people / sq km)") +
  scale_linetype_manual(values = c("solid", "dotted"), name = "Infection Source", labels = c("Human", "Primate")) +
  facet_wrap(~disease) + 
  xlab("Clustering of landscape features") +
  ylab("Average Force of Infection on Humans") +
  theme(
    legend.key.size = unit(.45, "cm")
  , legend.title = element_text(size = 12)
  , legend.text = element_text(size = 14)
  , legend.position = c(0.57, 0.66)
  , legend.background = element_blank()
  , axis.text.x = element_text(size = 12) 
  )}

#########################
## Figure 2: Summary of FOI across human density on the X for DENGUE, with three panels, the first being
##           Aedes aegypti, the second being Aedes albopictus, and the third being cobined
#########################

# FOI_on.s.f <- readRDS("Figure_Rds/yf_dens_explore.Rds")

(
  FOI_on.s.f %>% group_by(Mosquito, dens) %>% 
    filter() %>% 
      filter(
       X >= 1
     , X <= 11
     , Y >= 1
     , Y <= 11
    ) %>% 
    summarize(
      FOI_on_H = mean(FOI_on_H)
        ) %>% mutate(
          Mosquito = mapvalues(Mosquito
            , from = c("1", "2", "all")
            , to   = c("Aedes aegypti", "Aedes albopictus", "Both"))
        )
) %>% {
  ggplot(., aes(dens, FOI_on_H)) + 
    geom_line(aes(linetype = Mosquito)) +
    scale_linetype_manual(values = c("solid", "dotted", "dotdash")) +
    xlab("Human population density") +
    ylab("Average Force of 
Infection on Humans") + 
 theme(
    legend.key.size = unit(.35, "cm")
  , legend.title = element_text(size = 12)
  , legend.text = element_text(size = 11)
  , legend.background = element_blank()
  , axis.text.x = element_text(size = 15) 
 # , axis.title.y = element_text(size = 12)
  , axis.text.y = element_text(size = 15) 
  , legend.position = c(0.75, 0.45)
  )
}

#########################
## Figure 3: Summary of FOI across aedes aegypti exponent on the X for DENGUE, with three panels, the first being
##           aedes aegypti, the second being aedes albopictus, and the third being combined
#########################

(
  FOI_on.s.f %>% group_by(Mosquito, exponent, dens) %>% 
    summarize(
      FOI_on_H = mean(FOI_on_H)
        ) %>% mutate(
          Mosquito = mapvalues(Mosquito
            , from = c("1", "2", "all")
            , to   = c("Aedes aegypti", "Aedes albopictus", "Both"))
        )
) %>% {
  ggplot(., aes(exponent, FOI_on_H)) + 
    geom_line(aes(linetype = Mosquito)) +
    scale_linetype_manual(values = c("solid", "dotted", "dotdash")) +
    xlab("Aedes aegypti density-dependence exponent") +
    ylab("Average Force of 
Infection on Humans") + 
    facet_wrap(~dens, scales = "free") +
    scale_y_continuous(trans = "pseudo_log"
      , breaks = c(
        0
    #  , 0.5
      , 1
    #  , 1.5
    #  , 2.0
      , 10
      , 20
      , 40
      , 100
      , 400
        )) +
 theme(
    legend.key.size = unit(.35, "cm")
  , legend.title = element_text(size = 12)
  , legend.text = element_text(size = 11)
  , legend.background = element_blank()
  , axis.text.x = element_text(size = 12) 
 #, axis.title.y = element_text(size = 12)
  , axis.text.y = element_text(size = 12) 
  , legend.position = c(0.45, 0.75)
  )
}

#########################
## Figure 4: Exploration of patterns of Malaria across landscape features (explaining the pattern of malaria across land heterogeneity)
#########################

## Bottom panel, scatter plot to show relationship between forest, urbanization, and malaria

(FOI_on.s.f %>% filter(
  disease == "Malaria"
, dens    == 250) %>% 
    dplyr::select(
        X, Y
      , FOI_on_H
      , disease, land, dens, starting_host
      , forest, urban, forest_var) %>% 
    filter(
       X >= 1
     , X <= 11
     , Y >= 1
     , Y <= 11
    ) %>% 
    filter(
      land == 0.18 | land == 0.58 | land == 0.98
    )
) %>% {
   ggplot(., aes(
    forest
  , FOI_on_H
  )
  ) + 
 geom_point(aes(
 #  colour = forest
 #   colour = forest_var
    colour = urban
   ), alpha = 0.8) +
# scale_colour_gradient(low = "white", high = "darkgreen"
#  , name = "Tree cover"
 # , name = "Variance in
#Tree Cover" 
#  ) + 
 scale_colour_gradient(low = "white", high = "tan4"
   , name = "Urban Intensity"
   ) + 
# xlab("Urban Intensity") + 
 xlab("Tree cover") + 
 ylab("Average Force of 
Infection on Humans") + 
 facet_wrap(~land
   , scales = 'free'
   , nrow = 1
   ) +
 theme(
    legend.key.size = unit(.35, "cm")
  , legend.title = element_text(size = 12)
  , legend.text = element_text(size = 11)
  , legend.background = element_blank()
  , axis.text.x = element_text(size = 12) 
  , axis.text.y = element_text(size = 12) 
  , legend.position = c(0.24, 0.75)
  )
}

## Top few panels are maps across the simulated landscaeps to show the locations of the FOI for Malaria

(FOI_on.s.f %>% filter(
  disease == "Malaria"
, dens    == 250) %>% 
    dplyr::select(
        X, Y
      , FOI_on_H
      , disease, land, dens, starting_host
      , forest, urban) %>% 
    filter(
       X >= 1
     , X <= 11
     , Y >= 1
     , Y <= 11
    ) %>% 
    filter(
      land == 0.18 | land == 0.58 | land == 0.98
    )
) %>% {
  ggplot(., aes(X, Y
     #  , z = FOI_on_H
        , z = forest
     #  , z = urban
      )) + 
  # scale_fill_distiller(palette = "Spectral") +
  scale_fill_gradient(
  #  low = "white", high = "tan4"
   low = "white", high = "forestgreen"  
  ) +
  geom_raster(aes(
    # fill = FOI_on_H
     fill = forest
    # fill = urban
    )) +
  xlab("Km") + 
  ylab("Km") + 
#  guides(fill = guide_colorbar(
#  title = "Second 
#generation
#infections")) +
  guides(fill = guide_colorbar(
  title = "Tree 
 Cover")) +
#  guides(fill = guide_colorbar(
#  title = "Urban
#Intensity")) +
  facet_wrap(~land) +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12)) +
  scale_y_continuous(breaks = c(0, 3, 6, 9, 12)) +
  theme(
    legend.key.size = unit(.45, "cm")
  , legend.title = element_text(size = 12)
  , legend.text = element_text(size = 14)
  , legend.background = element_blank()
  , axis.text.x = element_text(size = 12) 
  , axis.text.y = element_text(size = 12) 
 # , panel.border = element_blank()
 # , panel.background = element_blank()
 # , legend.position = "none"
  )
}

#########################
## Figure 5: Correlation figure across landscape features
#########################

if (file.exists("Figure_Rds/check_all_cor.Rds")) {
  check_all_cor <- readRDS("Figure_Rds/check_all_cor.Rds")
} else {
  
check_all_cor <- expand.grid(
  land.out   = (1 - seq(0, 0.92, by = 0.02))[seq(2, 47, length = 10)]
, dens.out   = c(5, 20, 80, 320)
, adj_forest = c(0, 1, 2)
, adj_urban  = c(0, 1, 2)
)

check_all_cor <- check_all_cor %>% 
  filter(
    !(adj_forest != 0 & adj_urban != 0)
  )

check_all_cor1 <- check_all_cor %>%
  mutate(
  dis1 = "Dengue"
, dis2 = "Malaria"
  )

check_all_cor2 <- check_all_cor %>%
  mutate(
  dis1 = "Dengue"
, dis2 = "Yellow Fever"
  )

check_all_cor3 <- check_all_cor %>%
  mutate(
  dis1 = "Yellow Fever"
, dis2 = "Malaria"
  )

check_all_cor <- rbind(check_all_cor1, check_all_cor2, check_all_cor3)

check_all_cor <- check_all_cor %>% mutate(
  corr = 0
)

check_all_cor$land.out <- round(check_all_cor$land.out, 2)

FOI_on.s.f.for_corr <- FOI_on.s.f %>%
    dplyr::select(X, Y
      , FOI_on_H
      , disease, land, dens, starting_host
      , forest, urban) %>%
    group_by(X, Y, disease, land, dens) %>% 
    summarize(
       FOI_on_H = sum(FOI_on_H)
    ,  forest   = mean(forest)
    ,  urban    = mean(urban)
      ) %>% 
    filter(
       X >= 1
     , X <= 11
     , Y >= 1
     , Y <= 11
    )

FOI_on.s.f.for_corr$land <- round(FOI_on.s.f.for_corr$land, 2)

for (i in 1:nrow(check_all_cor)) {
  
  if (is.na(check_all_cor[i, "corr"])) {
  
vals1 <- FOI_on.s.f.for_corr %>% filter(
        land      == check_all_cor[i, "land.out"]
      , disease   == check_all_cor[i, "dis1"]) 
vals2 <- FOI_on.s.f.for_corr %>% filter(
        land      == check_all_cor[i, "land.out"]
      , disease   == check_all_cor[i, "dis2"]) 

if (check_all_cor[i, "adj_urban"] == 1) {
  
vals1 <- vals1 %>% filter(urban < 0.50)
vals2 <- vals2 %>% filter(urban < 0.50)
  
} else if (check_all_cor[i, "adj_urban"] == 2) {
  
vals1 <- vals1 %>% filter(urban > 0.50)
vals2 <- vals2 %>% filter(urban > 0.50)
  
} else {
  
}

if (check_all_cor[i, "adj_forest"] == 1) {
  
vals1 <- vals1 %>% filter(forest < 0.50)
vals2 <- vals2 %>% filter(forest < 0.50)
  
} else if (check_all_cor[i, "adj_forest"] == 2) {
  
vals1 <- vals1 %>% filter(forest > 0.50)
vals2 <- vals2 %>% filter(forest > 0.50)
  
} else {
  
}

check_all_cor[i, "corr"] <- cor(vals1$FOI_on_H, vals2$FOI_on_H)

}

print(i)

}

}

check_all_cor <- check_all_cor %>% mutate(Disease = interaction(dis1, dis2))
check_all_cor <- check_all_cor %>% mutate(dens.out = dens.out * 25 / 2)
check_all_cor <- check_all_cor %>% mutate(
  adj_forest = mapvalues(adj_forest, from = c(0, 1, 2)
    , to = c("All", "Less than 0.50", "Greater than 0.50"))
, adj_urban = mapvalues(adj_urban, from = c(0, 1, 2)
    , to = c("All", "Less than 0.50", "Greater than 0.50"))
)

check_all_cor          <- check_all_cor %>% mutate(sub_region = interaction(adj_forest, adj_urban))
check_all_cor$dens.out <- as.factor(check_all_cor$dens.out)

check_all_cor$Disease <- factor(check_all_cor$Disease
  , levels = c(
      "Dengue.Malaria"
    , "Dengue.Yellow Fever"
    , "Yellow Fever.Malaria"
  ))

ggplot(
  (check_all_cor %>% filter(dens.out == 250) %>% filter(adj_forest == "All"))
  , aes(land.out, corr)) + 
  geom_line(aes(colour = sub_region)) +
  xlab("Landscape feature spatial autocorrelation") +
  ylab("Correlation") +
  scale_colour_manual(
   values = c("black", "forestgreen", "springgreen2")
  # values = c("black", "brown", "tan2") 
  , name = ""
    ) +
  facet_wrap(~Disease) +
  geom_hline(aes(yintercept = 0), linetype = "dotted") + 
  theme(
    legend.key.size = unit(.65, "cm")
  , legend.title = element_text(size = 12)
  , legend.text = element_text(size = 14)
  , legend.background = element_blank()
  , axis.text.x = element_text(size = 12) 
  , axis.text.y = element_text(size = 12) 
  , legend.position = c(0.80, 0.85)
  )
  
#########################
## Figure 6: Stacked map to show tradeoffs
#########################

library(rayshader)

FOI_on.s.f$land <- round(FOI_on.s.f$land, 2)

#####
## Disease FOI
#####
ggmap <- (FOI_on.s.f %>%
    dplyr::select(X, Y
      , FOI_on_H
      , disease, land, dens, starting_host) %>%
    group_by(X, Y, disease, land, dens) %>% 
    summarize(
       FOI_on_H = sum(FOI_on_H)
      ) %>%
    filter(
       disease    == "Malaria"
    ,  land      == 0.58
     , dens       == 250
  ) %>% 
    filter(
       X >= 1
     , X <= 11
     , Y >= 1
     , Y <= 11
    ) %>% 
    group_by(
      X, Y, land, dens
    ) %>% summarize(
      FOI_on_H = sum(FOI_on_H)
    )
  ) %>% {
  ggplot(
    .
    , aes(X, Y
     #   , z = FOI_on_H
        , z = FOI_on_H / max(FOI_on_H)
      )) + 
  scale_fill_distiller(palette = "Spectral") +
  geom_raster(aes(
  #  fill = FOI_on_H
   fill = FOI_on_H / max(FOI_on_H)
    )) +
  xlab("Km") + 
  ylab("Km") + 
  guides(fill = guide_colorbar(
  title = "Second 
generation
infections")) +
  scale_x_continuous(breaks = c(0, 3, 6, 9 , 12)) +
  scale_y_continuous(breaks = c(0, 3, 6, 9 , 12)) +
  theme(
    legend.key.size = unit(.75, "cm")
  , legend.title = element_text(size = 14)
  , legend.text = element_text(size = 14)
  , legend.background = element_blank()
  , axis.text.x = element_text(size = 12) 
  , axis.text.y = element_text(size = 12) 
  , panel.border = element_blank()
  , panel.background = element_blank()
 # , legend.position = "none"
  )
    }
ggmap

plot_gg(ggmap, zoom = 1.25, phi = 30, sunangle = 0, shadow_intensity = 0, raytrace = FALSE)
render_camera(fov = 30, zoom = 1.00, theta = 345, phi = 35)
render_snapshot(clear=TRUE)

## Landscape Features
ggland <- (FOI_on.s.f %>%
    dplyr::select(X, Y
    # , forest 
      , urban
      , disease, land, dens, starting_host) %>%
    group_by(X, Y, disease, land, dens) %>% 
    summarize(
    # forest = mean(forest)
      urban = mean(urban)
      ) %>%
    filter(
       disease   == "Dengue"
     , land      == 0.58
     , dens      == 250
  ) %>% 
    filter(
       X >= 1
     , X <= 11
     , Y >= 1
     , Y <= 11
    )) %>% {
  ggplot(
    .
    , aes(X, Y
      , z = urban
 #     , z = forest
      )) + 
  scale_fill_gradient(
    low = "white", high = "tan4"
  # low = "white", high = "forestgreen"  
    ) +
  geom_raster(aes(
  fill = urban
#   fill = forest
    )) +
  xlab("Km") + 
  ylab("Km") + 
  guides(fill = guide_colorbar(
  title = "Urban
Intensity")) +
#  title = "Tree
#Cover")) +
  scale_x_continuous(breaks = c(0, 3, 6, 9 , 12)) +
  scale_y_continuous(breaks = c(0, 3, 6, 9 , 12)) +
  theme(
    legend.key.size = unit(.45, "cm")
  , legend.title = element_text(size = 12)
  , legend.text = element_text(size = 14)
  , legend.background = element_blank()
  , axis.text.x = element_text(size = 12) 
  , axis.text.y = element_text(size = 12) 
  , panel.border = element_blank()
  , panel.background = element_blank()
  , legend.position = "none"
  )
  }
ggland

plot_gg(ggland, zoom = 1.25, phi = 30, sunangle = 0, shadow_intensity = 0, raytrace = FALSE)
render_camera(fov = 30, zoom = 1.00, theta = 345, phi = 35)
render_snapshot(clear=TRUE)

#################################################
## Results Section Two -- Empirical Landscapes ##
#################################################

#########################
## Figure 1: Primary result figure showing reforestation strategies
#########################

####
## Top panel: Mean + sd in FOI on humans from the various stages of reforestation
####

FOI_on.s.f.s <- FOI_on.s.f %>%
  group_by(
      disease
    , which_landscape 
    , X, Y
    ) %>%
    summarize(
     FOI_on_H = sum(FOI_on_H) 
    ) %>% 
  ungroup(X, Y) %>% summarize(
    mean_R0 = mean(FOI_on_H)
  , var_R0  = var(FOI_on_H)
  , lwr_R0    = quantile(FOI_on_H, 0.250)
  , lwr_lw_R0 = quantile(FOI_on_H, 0.025)
  , mid_R0    = quantile(FOI_on_H, 0.500)
  , upr_R0    = quantile(FOI_on_H, 0.750)
  , upr_up_R0 = quantile(FOI_on_H, 0.975)
  )

FOI_on.s.f.s <- FOI_on.s.f.s %>%
  pivot_longer(cols = ends_with("R0"), names_to = "Statistic", values_to = "Estimate")

cc                <- scales::seq_gradient_pal("dodgerblue1", "firebrick3", "Lab")(seq(0,1,length.out=4))
FOI_on.s.f.s.w    <- FOI_on.s.f.s %>% pivot_wider(names_from = Statistic, values_from = Estimate)

FOI_on.s.f.s.w <- FOI_on.s.f.s.w %>% mutate(
  which_landscape = mapvalues(which_landscape
    , from = c("land1_base", "land1_contiguous", "land1_patchy", "land1_flat")
    , to   = c("Baseline", "Contiguous", "Patchy", "Flat")
    )
, disease         = mapvalues(disease
  , from = c("dengue", "malaria", "yellow_fever")
  , to   = c("Dengue", "Malaria", "Yellow Fever")))

FOI_on.s.f.s.w$which_landscape <- factor(FOI_on.s.f.s.w$which_landscape
  , levels = c("Baseline", "Flat", "Contiguous", "Patchy"))

## Landscape-level summary across landscape heterogeneity with lwr and upr ranges
{ggplot(FOI_on.s.f.s.w, aes(which_landscape, mid_R0)) + 
 # geom_ribbon(aes(ymin = lwr_R0, ymax = upr_R0, fill = dens), alpha = 0.1) +
  geom_point(lwd = 2) +
  geom_errorbar(aes(which_landscape, ymin = lwr_R0, ymax = upr_R0), width = 0.3) + 
  geom_point(aes(which_landscape, upr_up_R0), shape = 24, lwd = 2) + 
  geom_point(aes(which_landscape, lwr_lw_R0), shape = 25, lwd = 2) + 
  scale_colour_manual(values = cc, name = "Average human
population density
(people / sq km)") +
  scale_fill_manual(values = cc, name = "Average human
population density
(people / sq km)") +
  scale_linetype_manual(values = c("solid", "dotted"), name = "Infection Source", labels = c("Human", "Primate")) +
  facet_wrap(~disease) + 
  xlab("Landscape feature spatial autocorrelation") +
  ylab("Average Force of 
Infection on Humans") +
  theme(
    legend.key.size = unit(.45, "cm")
  , legend.title = element_text(size = 12)
  , legend.text = element_text(size = 14)
  , legend.position = c(0.50, 0.76)
  , legend.background = element_blank()
  , axis.text.x = element_text(size = 12, angle = 330, hjust = 0.1) 
  )}

####
## Second set of panels are maps of risk across the 4 scenarios
####

FOI_on.s.f <- FOI_on.s.f %>% mutate(
  which_landscape = mapvalues(which_landscape
    , from = c("land1_base", "land1_contiguous", "land1_patchy", "land1_flat")
    , to   = c("Baseline", "Contiguous", "Patchy", "Flat")
    )
, disease         = mapvalues(disease
  , from = c("dengue", "malaria", "yellow_fever")
  , to   = c("Dengue", "Malaria", "Yellow Fever")))

FOI_on.s.f$which_landscape <- factor(FOI_on.s.f$which_landscape
 , levels = c("Baseline", "Flat", "Contiguous", "Patchy"))

(FOI_on.s.f %>%
    dplyr::select(
        X, Y
      , FOI_on_H
      , disease, which_landscape) %>%
    group_by(X, Y, disease, which_landscape) %>% 
    summarize(
       FOI_on_H = sum(FOI_on_H)
      ) %>%
    filter(
       disease   == "Yellow Fever"
    # , land      == 0.58
   #  , dens      == 250
  ) #%>% 
  #  filter(
  #     X >= 1
  #   , X <= 22
  #   , Y >= 1
  #   , Y <= 17.4
  #  ) 
  ) %>% {
  ggplot(
    .
    , aes(X, rev(Y)
       , z = FOI_on_H
      )) + 
  scale_fill_distiller(palette = "Spectral") +
  geom_raster(aes(
     fill = FOI_on_H
    )) +
  xlab("Km") + 
  ylab("Km") + 
  guides(fill = guide_colorbar(
  title = "Second 
generation
infections")) +
  facet_wrap(~which_landscape, ncol = 1) +
  scale_x_continuous(breaks = c(0, 3, 6, 9 , 12, 15, 18, 21)) +
  scale_y_continuous(breaks = c(0, 3, 6, 9 , 12, 15, 18)) +
  theme(
    legend.key.size = unit(.45, "cm")
  , legend.title = element_text(size = 12)
  , legend.text = element_text(size = 14)
  , legend.background = element_blank()
  , axis.text.x = element_text(size = 12) 
  , axis.text.y = element_text(size = 12) 
 # , panel.border = element_blank()
 # , panel.background = element_blank()
 # , legend.position = "none"
  )
  }

### check correlations for main text numbers
FOI_on.s.f.one.m <- FOI_on.s.f.one %>%
    dplyr::select(
        X, Y
      , FOI_on_H
      , disease, which_landscape) %>%
    group_by(X, Y, disease, which_landscape) %>% 
    summarize(
       FOI_on_H = sum(FOI_on_H)
      ) %>%
    filter(
       disease   == "Dengue"
)

FOI_on.s.f.m <- FOI_on.s.f %>%
    dplyr::select(
        X, Y
      , FOI_on_H
      , disease, which_landscape) %>%
    group_by(X, Y, disease, which_landscape) %>% 
    summarize(
       FOI_on_H = sum(FOI_on_H)
      ) %>%
    filter(
       disease   == "Dengue"
)

plot(FOI_on.s.f.m$FOI_on_H, FOI_on.s.f.one.m$FOI_on_H)
cor(FOI_on.s.f.m$FOI_on_H, FOI_on.s.f.one.m$FOI_on_H)
