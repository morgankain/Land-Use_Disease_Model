#######################################
### Supp figures for the manuscript ###
#######################################

## Code for figures (but not saved Rds files to create the code -- see manuscript_figures.R and explore_results_kernel.R for details)

#########################
## Figure S1: Host pathogen response profiles
#########################

####
## Host Titer raw data with fits
####

host_titer <- read.csv("disease_parameters/land_disease_extracted_data/host_titer.csv") %>% 
  mutate(dose = log10(Dose)) %>%
  filter(Titer_mean > 0)

## First predictions
host_titer.pred.gg.D <- exp(predict(
   host_titer.mod.D
 , newdata = data.frame(
   Day = seq(-3, 7, by = 0.1)), type = "response"))
host_titer.pred.gg.D <- data.frame(
  Virus     = "Dengue"
, Day = seq(-3, 7, by = 0.1)
, viral_load = host_titer.pred.gg.D)

host_titer.pred.gg.YF <- exp(predict(
   host_titer.mod.YF
 , newdata = data.frame(
   Day = seq(0, 7, by = 0.1)), type = "response"))
host_titer.pred.gg.YF <- data.frame(
  Virus     = "Yellow Fever"
, Day = seq(0, 7, by = 0.1)
, viral_load = host_titer.pred.gg.YF)
host_titer.pred.gg <- rbind(host_titer.pred.gg.D, host_titer.pred.gg.YF)

gg_titer.1 <- ggplot(host_titer[host_titer$Virus == "Dengue", ]
  , aes(Day, Titer_mean)) + 
  geom_point(aes(colour = Host), lwd = 2) + 
  scale_colour_manual(values = c("royalblue3")) +
  xlab("Days from symptom onset") +
  ylab(expression(log[10]("Viral load"))) +
  geom_line(data = host_titer.pred.gg[host_titer.pred.gg$Virus == "Dengue", ]
    , aes(Day, viral_load)) +
  theme(
    legend.key.size = unit(.45, "cm")
  , legend.title = element_text(size = 16)
  , legend.text = element_text(size = 14)
  , legend.position = c(0.9, 0.9)
  , legend.background = element_blank()
    ) 

gg_titer.2 <- ggplot(host_titer[host_titer$Virus == "Yellow Fever", ]
  , aes(Day, Titer_mean)) + 
  geom_point(aes(colour = Host), lwd = 2) + 
  scale_colour_manual(values = c("darkorange3", "magenta3")) +
  xlab("Days from exposure") +
  ylab(expression(log[10]("Viral load"))) +
  geom_line(data = host_titer.pred.gg[host_titer.pred.gg$Virus == "Yellow Fever", ]
    , aes(Day, viral_load)) +
  theme(
    legend.key.size = unit(.45, "cm")
  , legend.title = element_text(size = 16)
  , legend.text = element_text(size = 14)
  , legend.position = c(0.8, 0.9)
  , legend.background = element_blank()
    ) 

host_titer.pred.gg.M <- 10^(predict(
   host_titer.mod.M
 , newdata = data.frame(
   Day = seq(0, 20, by = 0.1)), type = "response"))

host_titer.pred.gg.M <- data.frame(
  Day = seq(0, 20, by = 0.1)
, viral_load = host_titer.pred.gg.M
, Host = "Human"
)

gg_titer.3 <- ggplot(host_titer.pred.gg.M, aes(Day, viral_load, linetype = Host)) + 
  geom_hline(aes(yintercept = 425.6), lwd = 0.5, linetype = "dashed") +
  geom_hline(aes(yintercept = 5096), lwd = 0.5, linetype = "dotted") +
  xlab("Days from exposure") + 
  ylab("Malaria gametocytes") +
  geom_line() +
  theme(
    legend.key.size = unit(.45, "cm")
  , legend.title = element_text(size = 16)
  , legend.text = element_text(size = 14)
  , legend.position = c(0.9, 0.9)
  , legend.background = element_blank()
    )

####
## Host titer -predictions-
####

## Go to adj_models.R and do these individually and then stitch them together

host_titer.gg        <- melt(host_titer)
names(host_titer.gg) <- c("Day", "Host", "Titer")
host_titer.gg        <- host_titer.gg %>% mutate(pathogen = "Dengue")

host_titer.gg2        <- melt(host_titer)
names(host_titer.gg2) <- c("Day", "Host", "Titer")
host_titer.gg2        <- host_titer.gg2 %>% mutate(pathogen = "Yellow Fever")

host_titer.gg3        <- melt(host_titer)
names(host_titer.gg3) <- c("Day", "Host", "Titer")
host_titer.gg3        <- host_titer.gg3 %>% mutate(pathogen = "Malaria")

host_titer.gg.s <- rbind(host_titer.gg, host_titer.gg2, host_titer.gg3)

host_titer.gg.s <- host_titer.gg.s %>% mutate(
  Host = mapvalues(Host, from = c("other", "human"), to = c("Others", "Humans"))
)

gg_titer.4 <- ggplot(host_titer.gg.s[host_titer.gg.s$pathogen == "Dengue", ]
  , aes(Day, Titer)) + 
  geom_line(aes(linetype = Host)) +
  ylab(expression(log[10]("Viral load"))) +
  xlab("Day") +
  theme(
    legend.key.size = unit(.45, "cm")
  , legend.title = element_text(size = 16)
  , legend.text = element_text(size = 14)
  , legend.position = c(0.9, 0.9)
  , legend.background = element_blank()
    )

gg_titer.5 <- ggplot(host_titer.gg.s[host_titer.gg.s$pathogen == "Yellow Fever", ]
  , aes(Day, Titer)) + 
  geom_line(aes(linetype = Host)) +
  xlab("Day") + 
  ylab(expression(log[10]("Viral load"))) +
  theme(
    legend.key.size = unit(.45, "cm")
  , legend.title = element_text(size = 16)
  , legend.text = element_text(size = 14)
  , legend.position = c(0.9, 0.9)
  , legend.background = element_blank()
    )
  
gg_titer.6 <- ggplot(host_titer.gg.s[host_titer.gg.s$pathogen == "Malaria", ]
  , aes(Day, Titer)) + 
  geom_line(aes(linetype = Host)) +
  xlab("Day") + 
  ylab("Malaria gametocytes") +
  scale_y_log10() +
  theme(
    legend.key.size = unit(.45, "cm")
  , legend.title = element_text(size = 16)
  , legend.text = element_text(size = 14)
  , legend.position = c(0.9, 0.9)
  , legend.background = element_blank()
    )

gridExtra::grid.arrange(
  gg_titer.1, gg_titer.2, gg_titer.3
, gg_titer.4, gg_titer.5, gg_titer.6
, nrow = 2)

#########################
## Figure S2: Mosquito infection probability
#########################

####
## Mosquito infection probability, raw data with fits
####

## Host to mosquito transmission

## First predictions
h_to_m.pred.gg.DYF <- predict(
   h_to_m_trans.mod.DYF
 , newdata = expand.grid(
  Titer    = seq(2.5, 12.5, by = 0.1)
, Species = c("aegypti", "albopictus")
, Virus   = c("Dengue", "Yellow Fever")
, Day     = 14)
, re.form = NA
  , type = "response")

h_to_m.pred.gg.DYF <- expand.grid(
  Titer    = seq(2.5, 12.5, by = 0.1)
, Species = c("aegypti", "albopictus")
, Virus   = c("Dengue", "Yellow Fever")
, Day     = 14) %>% mutate(
  Infection = h_to_m.pred.gg.DYF
)

gg_hm.1 <- ggplot(h_to_m[h_to_m$Virus != "Malaria", ]
  , aes(Titer, N_Infected/N_Tested)) + 
  geom_point(aes(colour = Species)) + 
  geom_line(data = h_to_m.pred.gg.DYF
  , aes(Titer, Infection, colour = Species)) +
  xlab(expression(log[10]("Viral load"))) +
  ylab("Infection Probability") + 
  scale_colour_brewer(
    palette = "Dark2"
  , labels = c("Aedes aegypti", "Aedes albopictus")) + 
  facet_wrap(~Virus) + 
  theme(
    legend.key.size = unit(.45, "cm")
  , legend.title = element_text(size = 16)
  , legend.text = element_text(size = 14)
  , legend.position = c(0.88, 0.2)
  , legend.background = element_blank()
    )

## ``Artificially'' pull down the intercept a bit by adding a 0 infection probability at 0 Malaria
h_to_m.M <- rbind(h_to_m.M
  , data.frame(
      Day     = rep(14, 3), Titer = rep(0, 3), N_Tested = rep(1000, 3), N_Infected = rep(0, 3), Genus = rep("Anopheles", 3)
    , Species = c("gambiae", "dirus", "dalingi"), Mosquito_Strain = rep(NA, 3), Method = rep(NA, 3)
    , Virus = rep("Malaria", 3), Serotype = rep(NA, 3), Virus_Strain = rep(NA, 3)
    , Citation = rep(NA, 3), Figure_Table = rep(NA, 3), Notes = rep(NA, 3), log10titer = rep(-4, 3))
)

## Model with all three species and a random effect for citation
h_to_m_trans.mod.M <- glmer(
    cbind(N_Infected, N_Tested - N_Infected) ~ log10titer +
  + (1 + log10titer | Citation)
  , data    = h_to_m.M
  , family  = "binomial")

h_to_m.pred.gg.M <- predict(
   h_to_m_trans.mod.M
 , newdata = data.frame(
  log10titer = log10(seq(1, 5000, by = 1))
, Virus      = "Malaria"
, Day        = 14)
, re.form    = NA
  , type = "response")

h_to_m.pred.gg.M <- data.frame(
  Titer      = seq(1, 5000, by = 1)
, Virus      = "Malaria"
, Day        = 14
, Model      = "all") %>% mutate(
  Infection = h_to_m.pred.gg.M
)

## Fit by species within the same model
h_to_m_trans.mod.M <- glm(
    cbind(N_Infected, N_Tested - N_Infected) ~ log10titer*Species
  , data    = h_to_m.M
  , family  = "binomial")

h_to_m.pred.gg.M2 <- predict(
   h_to_m_trans.mod.M
 , newdata = expand.grid(
  log10titer = log10(seq(1, 5000, by = 1))
, Species    = unique(h_to_m.M$Species)
, Virus      = "Malaria"
, Day        = 14)
  , type = "response")

h_to_m.pred.gg.M2 <- expand.grid(
  Titer     = seq(1, 5000, by = 1)
, Species    = unique(h_to_m.M$Species)
, Virus      = "Malaria"
, Day        = 14) %>% mutate(
  Infection = h_to_m.pred.gg.M2
)

gg_hm.2 <- ggplot(h_to_m[h_to_m$Virus == "Malaria", ]
  , aes(Titer, N_Infected/N_Tested)) + 
  geom_point(aes(colour = Species)) + 
  geom_line(data = h_to_m.pred.gg.M2, aes(Titer, Infection, colour = Species)) +
 # geom_line(data = h_to_m.pred.gg.M, aes(Titer, Infection)) +
  scale_x_log10() +
  xlab("Malaria gametocytes") +
  ylab("Infection Probability") + 
  scale_colour_brewer(
    palette = "Set1"
  , labels = c("Anopheles darlingi", "Anopheles dirus", "Anopheles gambiae")) + 
  theme(
    legend.key.size = unit(.45, "cm")
  , legend.title = element_text(size = 16)
  , legend.text = element_text(size = 14)
  , legend.position = c(0.8, 0.2)
  , legend.background = element_blank()
    ) + facet_wrap(~Virus)

####
## Mosquito infection probability -predictions-
####

## Go to adj_models.R and do these individually and then stitch them together

hm.gg        <- melt(h_to_m_trans)
names(hm.gg) <- c("Day", "Host", "Mosquito", "Infection_Probability")
hm.gg        <- hm.gg %>% mutate(pathogen = "Dengue")

hm.gg2        <- melt(h_to_m_trans)
names(hm.gg2) <- c("Day", "Host", "Mosquito", "Infection_Probability")
hm.gg2        <- hm.gg2 %>% mutate(pathogen = "Yellow Fever")

hm.gg3        <- melt(h_to_m_trans)
names(hm.gg3) <- c("Day", "Host", "Mosquito", "Infection_Probability")
hm.gg3        <- hm.gg3 %>% mutate(pathogen = "Malaria")

hm.gg.s <- rbind(hm.gg, hm.gg2, hm.gg3)

hm.gg.s <- hm.gg.s %>% mutate(
  Host = mapvalues(Host, from = c("other", "human"), to = c("Others", "Humans"))
)

gg_hm.3 <- ggplot(hm.gg.s[hm.gg.s$pathogen == "Dengue", ]
  , aes(Day, Infection_Probability)) + 
  geom_line(aes(linetype = Host, colour = Mosquito)) +
  scale_colour_brewer(
    palette = "Dark2"
  , labels = c("Aedes aegypti", "Aedes albopictus")) + 
  scale_linetype_manual(
    values = c("solid", "dashed")
  , labels = c("Humans", "Others")
  ) +
  xlab("Host Infection Day") +
  ylab("Mosquito Infection Probability | Bite") +
  theme(
    legend.key.size = unit(.45, "cm")
  , legend.title = element_text(size = 16)
  , legend.text = element_text(size = 14)
  , legend.position = c(0.80, 0.80)
  , legend.background = element_blank()
    )

gg_hm.4 <- ggplot(hm.gg.s[hm.gg.s$pathogen == "Yellow Fever", ]
  , aes(Day, Infection_Probability)) + 
  geom_line(aes(linetype = Host, colour = Mosquito)) +
  scale_colour_brewer(
    palette = "Dark2"
  , labels = c("Aedes aegypti", "Aedes albopictus")) + 
  scale_linetype_manual(
    values = c("solid", "dashed")
  , labels = c("Humans", "Others")
  ) +
  xlab("Host Infection Day") +
  ylab("") +
  theme(
    legend.key.size = unit(.45, "cm")
  , legend.title = element_text(size = 16)
  , legend.text = element_text(size = 14)
  , legend.position = c(0.80, 0.80)
  , legend.background = element_blank()
    )
  
gg_hm.5 <- ggplot(hm.gg.s[hm.gg.s$pathogen == "Malaria", ]
  , aes(Day, Infection_Probability)) + 
  geom_line(aes(linetype = Host, colour = Mosquito)) +
  scale_colour_brewer(
    palette = "Dark2"
  , labels = c("Anopheles darlingi")) + 
  scale_linetype_manual(
    values = c("solid", "dashed")
  , labels = c("Humans", "Others")
  ) +
  xlab("Host Infection Day") +
  ylab("") +
  theme(
    legend.key.size = unit(.45, "cm")
  , legend.title = element_text(size = 16)
  , legend.text = element_text(size = 14)
  , legend.position = c(0.55, 0.55)
  , legend.background = element_blank()
    )

mosq_supp_plot <- gridExtra::arrangeGrob(
  gg_hm.1, gg_hm.2, gg_hm.3, gg_hm.4, gg_hm.5
, layout_matrix = 
matrix(
     c(
1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2,
1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2,
1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2,
1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2,
1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2,
1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2,
3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 
3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5,
3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5,
3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5
       )
    , byrow  = T, nrow = 10) 
)

 ggsave("Figure1.pdf",
         mosq_supp_plot,
         device = "pdf",
         width = 12*2,
         height = 10*1.5)

#########################
## Figure S3: Mosquito transmission probability
#########################

## Mosquito to Host transmission. Could consider adding "made up" data at day 0 (probability = 0) to fix transmission at 0 for that day, 
 ## which may help constrain the fit
m_to_h_trans.mod  <- glmer(
    cbind(N_Transmitting, N_Tested - N_Transmitting) ~ Day*Species + Titer.cen +
  + (1 | Citation)
  , data    = m_to_h
  , family  = "binomial")
 
 ## First predictions
m_to_h.pred.gg <- predict(
   m_to_h_trans.mod
 , newdata = expand.grid(
  Day       = seq(1, 40, by = 0.1)
, Species   = c("aegypti", "albopictus")
, Virus     = c("Dengue", "Yellow Fever")
, Titer.cen = 0.5)
, re.form = NA
, type = "response")

m_to_h.pred.gg <- expand.grid(
  Day       = seq(1, 40, by = 0.1)
, Species   = c("aegypti", "albopictus")
, Virus     = c("Dengue", "Yellow Fever")
, Titer.cen = 0.5) %>%
  mutate(
    Transmission = m_to_h.pred.gg
  )

m_to_h.pred.M.gg <- predict(m_to_h_trans.mod.M
  , newdata = expand.grid(
    Temperature = c(20, 22, 24, 26)
  , Day         = seq(1, 25, by = 1)
  )
, type = "response"
  )

m_to_h.pred.M.gg <- expand.grid(
    Temperature = c(20, 22, 24, 26)
  , Day         = seq(1, 25, by = 1)
  ) %>% mutate(
    Transmission = m_to_h.pred.M.gg
  )

m_to_h.pred.H.gg <- predict(m_to_h_trans.mod.YF.H
  , newdata = expand.grid(
   day         = seq(1, 30, by = 1)
  )
, type = "response"
  )

m_to_h.pred.H.gg <- expand.grid(
    day         = seq(1, 30, by = 1)
  ) %>% mutate(
    prop = m_to_h.pred.H.gg
  )

(gg_mh.1 <- ggplot(m_to_h[m_to_h$Genus != "Anopheles", ]
  , aes(Day, N_Transmitting/N_Tested)) + 
  geom_jitter(aes(colour = Titer, shape = Species)) + 
  geom_line(data = m_to_h.pred.gg
  , aes(Day, Transmission, linetype = Species)) +
  xlab("Day") +
  ylab("Transmission Probability") + 
  scale_shape_manual(
    values = c(2, 16)
  , labels = c("Aedes aegypti", "Aedes albopictus")) + 
  scale_linetype_manual(
    values = c("solid", "dashed")
  , labels = c("Aedes aegypti", "Aedes albopictus")) + 
  theme(
    legend.key.size = unit(.85, "cm")
  , legend.title = element_text(size = 16)
  , legend.text = element_text(size = 14)
  , legend.position = c(0.84, 0.40)
  , legend.background = element_blank()
    ))

(gg_mh.1b <- ggplot(m_to_h[m_to_h$Genus == "Anopheles", ]
  , aes(Day, N_Transmitting/N_Tested)) + 
  geom_point(aes(colour = as.factor(Temperature)), lwd = 2.5) + 
  geom_line(data = m_to_h.pred.M.gg
  , aes(Day, Transmission, colour = as.factor(Temperature))) +
  xlab("Day") +
  ylab("Transmission Probability") + 
  scale_colour_brewer(
    palette = "Dark2"
  , name   = "Temperature"
  , labels = c("20", "22", "24", "26")) + 
  theme(
    legend.key.size = unit(.85, "cm")
  , legend.title = element_text(size = 16)
  , legend.text = element_text(size = 14)
  , legend.position = c(0.14, 0.80)
  , legend.background = element_blank()
    ))

(gg_mh.1c <- ggplot(haem_trans.s.s
  , aes(day, prop)) + 
  geom_line(data = m_to_h.pred.H.gg) +
  geom_point(aes(size = total)) +
  xlab("Day") +
  ylab("Transmission Probability") + 
    scale_size_continuous(name = "Number of
Mosquitoes") +
  theme(
    legend.key.size = unit(.85, "cm")
  , legend.title = element_text(size = 16)
  , legend.text = element_text(size = 14)
  , legend.position = c(0.14, 0.80)
  , legend.background = element_blank()
    ))

####
## Mosquito to host transmission -predictions-
####

## Go to adj_models.R and do these individually and then stitch them together 
 ## That is, set disease == yellow fever and then disease == malaria get the predictions for both mosquitoes, then 
  ## create a single data frame to plot all of them

mh.gg        <- melt(m_to_h_trans)
names(mh.gg) <- c("Day", "Mosquito", "Transmission_Probability")
mh.gg        <- mh.gg %>% mutate(
  Mosquito = mapvalues(Mosquito
    , from = c("aedes aegypti", "aedes albopictus", "haemagogus sp")
    , to = c("Aedes aegypti", "Aedes albopictus", "Haemagogus")
    )
)

mosq_surv.gg        <- melt(mosq_surv)
names(mosq_surv.gg) <- c("Day", "Mosquito", "Survival")
mosq_surv.gg        <- mosq_surv.gg %>% 
  mutate(Mosquito = mapvalues(Mosquito
    , from = c(1, 2, 3)
    , to = c("Aedes aegypti", "Aedes albopictus", "Haemagogus"))
    )
mh.gg        <- left_join(mh.gg, mosq_surv.gg)

mh.gg$wt <- with(mh.gg, Transmission_Probability * Survival)

mh.gg2        <- melt(m_to_h_trans)
names(mh.gg2) <- c("Day", "Mosquito", "Transmission_Probability")
mh.gg2        <- mh.gg2 %>% mutate(
  Mosquito = mapvalues(Mosquito, from = c("anopheles"), to = c("Anopheles"))
)

mosq_surv.gg        <- melt(mosq_surv)
names(mosq_surv.gg) <- c("Day", "Mosquito", "Survival")
mosq_surv.gg        <- mosq_surv.gg %>% 
  mutate(Mosquito = mapvalues(Mosquito, from = c(1), to = c("Anopheles")))
mh.gg2        <- left_join(mh.gg2, mosq_surv.gg)

mh.gg2$wt <- with(mh.gg2, Transmission_Probability * Survival)

mh.gg.s <- rbind(mh.gg, mh.gg2)

(gg_mh.2 <- ggplot(mh.gg.s[mh.gg.s$Day <=40, ]
  , aes(Day, Transmission_Probability)) + 
  geom_line(aes(colour = Mosquito)) +
  xlab("Day") +
  ylab("") + 
  scale_colour_brewer(
    palette = "Dark2"
  , labels = c("Aedes aegypti", "Aedes albopictus", "Anopheles", "Haemagogus")) + 
  theme(
    legend.key.size = unit(.85, "cm")
  , legend.title = element_text(size = 16)
  , legend.text = element_text(size = 14)
  , legend.position = c(0.30, 0.80)
  , legend.background = element_blank()
    ))

(gg_mh.3 <- ggplot(mh.gg.s[mh.gg.s$Day <= 40, ]
  , aes(Day, Survival)) + 
  geom_line(aes(colour = Mosquito)) +
  xlab("Day") +
  ylab("Survival Probability") + 
  scale_colour_brewer(
    palette = "Dark2"
  , labels = c("Aedes aegypti", "Aedes albopictus", "Anopheles", "Haemagogus")) + 
  theme(
    legend.key.size = unit(.85, "cm")
  , legend.title = element_text(size = 16)
  , legend.text = element_text(size = 14)
  , legend.position = "None"
  , legend.background = element_blank()
    ))

(gg_mh.4 <- ggplot(mh.gg.s[mh.gg.s$Day <= 40, ]
  , aes(Day, wt)) + 
  geom_line(aes(colour = Mosquito)) +
  xlab("Day") +
  ylab("Survival-Weighted Transmission Probability") + 
  scale_colour_brewer(
    palette = "Dark2"
  , labels = c("Aedes aegypti", "Aedes albopictus", "Anopheles", "Haemagogus")) + 
  theme(
    legend.key.size = unit(.85, "cm")
  , legend.title = element_text(size = 16)
  , legend.text = element_text(size = 14)
  , legend.position = "None"
  , legend.background = element_blank()
    )) 

gridExtra::grid.arrange(
  gg_mh.1, gg_mh.1b, gg_mh.1c,gg_mh.2, gg_mh.3, gg_mh.4
  , nrow = 2)

mosq_supp_plot_mh <- gridExtra::arrangeGrob(
  gg_mh.1, gg_mh.1b, gg_mh.2, gg_mh.3, gg_mh.4
, layout_matrix = 
matrix(
     c(
1, 1, 1, 2, 2, 2, 3, 3, 3,
1, 1, 1, 2, 2, 2, 3, 3, 3,
1, 1, 1, 2, 2, 2, 3, 3, 3,
4, 4, 4, 4, 5, 5, 5, 5, 5, 
4, 4, 4, 4, 5, 5, 5, 5, 5, 
4, 4, 4, 4, 5, 5, 5, 5, 5, 
4, 4, 4, 4, 5, 5, 5, 5, 5,  
4, 4, 4, 4, 5, 5, 5, 5, 5
       )
    , byrow  = T, nrow = 8) 
)

 ggsave("Figure2.pdf",
         mosq_supp_plot_mh,
         device = "pdf",
         width = 12*2,
         height = 10*1.5)

#########################
## Figure S4-S7: Mosquito distributions
#########################

####
## NOTE: Run top_level_script.R up through pop_dens_kernel with the desired landscape configuration and disease to get the desired mosquito species
####

## Predicted mosquito abundance plotted against individual predictors
mosqplot        <- melt(landscape[[landscape_counter]])
names(mosqplot) <- c("X", "Y", "feature", "est")
mosqplot        <- mosqplot %>% pivot_wider(names_from = feature, values_from = est)

if (disease_counter == 3) {
names(mosqplot)[7] <- c("mosq")
forestvar <- melt(temp_anoph_landscape[,, l, "forest_var"])
names(forestvar) <- c("X", "Y", "forest_var")
mosqplot <- left_join(mosqplot, forestvar)
} else if (disease_counter == 1) {
names(mosqplot)[c(7, 8)] <- c("aegypti", "albopictus")
} else {
names(mosqplot)[c(7, 8, 9)] <- c("aegypti", "albopictus", "haemagogus")  
}

## For Dengue or Yellow Fever pick which of the three mosquitoes to do a plot for
names(mosqplot)[9] <- c("mosq")

mosqplot <- mosqplot %>% mutate(
  human = human * ((1000 * 1000) / (200*200))
, mosq  = mosq * ((1000 * 1000) / (200*200))
  )

gg.m.1 <- ggplot(mosqplot, aes(human, mosq)) + geom_point(lwd  = 0.5, alpha = 0.5) + xlab("Humans / sq km") + ylab("Mosquitoes / sq km") 
gg.m.2 <- ggplot(mosqplot, aes(forest, mosq)) + geom_point(lwd  = 0.5, alpha = 0.5) + xlab("Mean Tree Cover") + ylab("Mosquitoes / sq kme")
if (disease_counter == 3) {
gg.m.3 <- ggplot(mosqplot, aes(forest_var, mosq)) + geom_point(lwd  = 0.5, alpha = 0.5) + xlab("Variance in Tree Cover") + ylab("Mosquitoes / sq km")
} else {
gg.m.3 <- ggplot(mosqplot, aes(other, mosq)) + geom_point(lwd  = 0.5, alpha = 0.5) + xlab("Other Host Density") + ylab("Mosquitoes / sq km") 
}

### Forest cover
landscape.b.gg        <- melt(landscape[[landscape_counter]][,,3])
names(landscape.b.gg) <- c("X.coord", "Y.coord", "Out")
landscape.b.gg        <- landscape.b.gg %>% 
  mutate(
    X.coord = X.coord * 200 / 1000
  , Y.coord = Y.coord * 200 / 1000
  )

gg.m.4 <- ggplot(landscape.b.gg, aes(X.coord, Y.coord, z = Out)) +
  scale_fill_distiller(palette = "Greens", direction = 1) +
   geom_raster(aes(fill = Out)) +
  scale_x_continuous(breaks = c(2.5, 5.0, 7.5, 10)) +
  scale_y_continuous(breaks = c(2.5, 5.0, 7.5, 10)) +
  xlab("Km") + 
  ylab("Km") + 
 guides(fill = guide_colorbar(title = "Tree Cover")) +
  theme(legend.key.size = unit(.45, "cm"))

### Human population density
landscape.b.gg        <- melt(landscape[[landscape_counter]][,,1])
names(landscape.b.gg) <- c("X.coord", "Y.coord", "Out")
landscape.b.gg        <- landscape.b.gg %>% 
  mutate(
    X.coord = X.coord * 200 / 1000
  , Y.coord = Y.coord * 200 / 1000
  )

mosqplot <- mosqplot %>% 
  mutate(
    X = X * 200 / 1000
  , Y = Y * 200 / 1000
  )

gg.m.5 <- ggplot(mosqplot, aes(X, Y, z = human)) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
   geom_raster(aes(fill = human)) +
  scale_x_continuous(breaks = c(2.5, 5.0, 7.5, 10)) +
  scale_y_continuous(breaks = c(2.5, 5.0, 7.5, 10)) +
  xlab("Km") + 
  ylab("Km") + 
 guides(fill = guide_colorbar(title = "Humans /   
sq km")) +
  theme(legend.key.size = unit(.45, "cm"))

### Mosquito abundance
gg.m.6 <- ggplot(mosqplot, aes(X, Y, z = mosq)) +
  scale_fill_distiller(palette = "Spectral") +
   geom_raster(aes(fill = mosq)) +
  scale_x_continuous(breaks = c(2.5, 5.0, 7.5, 10)) +
  scale_y_continuous(breaks = c(2.5, 5.0, 7.5, 10)) +
  xlab("Km") + 
  ylab("Km") + 
 guides(fill = guide_colorbar(title = "Mosquitoes / 
sq km")) +
  theme(legend.key.size = unit(.45, "cm"))

mosq_supp_plot <- gridExtra::arrangeGrob(
  gg.m.4, gg.m.6, gg.m.1, gg.m.2, gg.m.3, gg.m.5
, layout_matrix = 
matrix(
     c(
       1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
       1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
       1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
       1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
       1, 1, 1, 1, 1, 1, 1, 1, 4, 4, 4, 4, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
       1, 1, 1, 1, 1, 1, 1, 1, 4, 4, 4, 4, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
       6, 6, 6, 6, 6, 6, 6, 6, 4, 4, 4, 4, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
       6, 6, 6, 6, 6, 6, 6, 6, 4, 4, 4, 4, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
       6, 6, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
       6, 6, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
       6, 6, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
       6, 6, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
    , byrow  = T, nrow = 12) 
)

 ggsave("Figure1.pdf",
         mosq_supp_plot,
         device = "pdf",
         width = 24,
         height = 12)

####
## Extra stuff / manual scratch
####
 
mosq_abund <- data.frame(
  mosq         = "Anopheles"
, human_dens   = 100
, land_feature = seq(0.01, 0.99, by = 0.01)
, mosq_dens    = mosq_dens(
    land              = qlogis(seq(0.01, 0.99, by = 0.01))
  , pop               = 100
  , ratio             = mosq_host_ratio["Anopheles sp"]
  , interc            = mosq_hab_params[["Anopheles sp"]]["interc"]
  , linear_scaling    = mosq_hab_params[["Anopheles sp"]]["linear_scaling"]
  , quadratic_scaling = mosq_hab_params[["Anopheles sp"]]["quadratic_scaling"]
  , pop_exp           = mosq_hab_params[["Anopheles sp"]]["pop_exp"]
  , oth_scaling       = mosq_hab_params[["Anopheles sp"]]["oth_predictor"] / anoph_scale_1
  , oth_predictor     = log(seq(0.01, 0.99, by = 0.01) * anoph_scale_2)
    ) 
)

ggplot(mosq_abund, aes(land_feature, mosq_dens)) + geom_line() +
  xlab("Tree Cover") +
  ylab("Mosquito Abundance")

tempdens <- mosq_dens(
    land              = temp_anoph_landscape[,, l, "forest_mean"]
  , pop               = landscape[[landscape_counter]][,,"human"] * mosq_bite_pref.weighting["human", "Anopheles sp"] + landscape[[landscape_counter]][,,"other"] * mosq_bite_pref.weighting["other", "Anopheles sp"]
  , ratio             = mosq_host_ratio["Anopheles sp"]
  , interc            = mosq_hab_params[["Anopheles sp"]]["interc"]
  , linear_scaling    = mosq_hab_params[["Anopheles sp"]]["linear_scaling"]
  , quadratic_scaling = mosq_hab_params[["Anopheles sp"]]["quadratic_scaling"]
  , pop_exp           = mosq_hab_params[["Anopheles sp"]]["pop_exp"]
  , oth_scaling       = mosq_hab_params[["Anopheles sp"]]["oth_predictor"] / anoph_scale_1
  , oth_predictor     = log(temp_anoph_landscape[,, l, "forest_var"] * anoph_scale_2)
    )

mosq_abund <- data.frame(
  mosq          = "Anopheles"
, human_dens    = c(landscape[[landscape_counter]][,,"human"])
, land_feature1 = c(temp_anoph_landscape[,, l, "forest_mean"])
, land_feature2 = c(temp_anoph_landscape[,, l, "forest_var"])
, mosq_dens     = c(tempdens)
)

ggplot(mosq_abund, aes(land_feature2, mosq_dens)) + geom_point() +
  xlab("Tree Cover") +
  ylab("Mosquito Abundance")

#########################
## Figure S8: Simulated landscapes
#########################

## Can be run with basically any output

R0.home.s.gg.t.f$land <- round(R0.home.s.gg.t.f$land, 2)

(R0.home.s.gg.t.f %>% filter(land == 0.48)) %>% {
  ggplot(., aes(X, Y, z = urban_mean)) + 
  geom_raster(aes(fill = urban_mean)) +
  xlab("Km") + 
  ylab("Km") + 
 scale_fill_gradient(
     low = "white", high = "tan4"
   , name = "Urban Intensity"
#     low = "white", high = "forestgreen"
#   , name = "Tree Cover"
   , breaks = c(0, 0.25, 0.50, 0.75, 1.00)
   , limits = c(0, 1)
   ) + 
  facet_wrap(~land, nrow = 1) +
  scale_x_continuous(breaks = c(2.5, 5.0, 7.5, 10)) +
  scale_y_continuous(breaks = c(2.5, 5.0, 7.5, 10)) +
  theme(
    legend.key.size = unit(.35, "cm")
  , legend.title = element_text(size = 12)
  , legend.text = element_text(size = 12)
  , legend.background = element_blank()
  , axis.text.x = element_text(size = 14) 
  , axis.text.y = element_text(size = 14) 
  )
  }

(R0.home.s.gg.t.f %>% 
    filter(
      land == 0.48
    , X    >= 0.6
    , X    <= 2.2
    , Y    >= 5.4
    , Y    <= 7.0)) %>% {
  ggplot(., aes(X, Y, z = urban_mean)) + 
  geom_raster(aes(fill = urban_mean)) +
  xlab("Km") + 
  ylab("Km") + 
 scale_fill_gradient(
     low = "white", high = "tan4"
   , name = "Urban Intensity"
#     low = "white", high = "forestgreen"
#   , name = "Tree Cover"
#  ,  breaks = c(0, 0.20, 0.40, 0.60, 0.80)
#   , limits = c(0, 1)
   ) + 
  facet_wrap(~land, nrow = 1) +
  scale_x_continuous(breaks = c(0.6, 1.0, 1.4, 1.8, 2.2)) +
  scale_y_continuous(breaks = c(5.4, 5.8, 6.2, 6.6, 7.0)) +
  theme(
    legend.key.size = unit(.35, "cm")
  , legend.title = element_text(size = 12)
  , legend.text = element_text(size = 12)
  , legend.background = element_blank()
  , axis.text.x = element_text(size = 14) 
  , axis.text.y = element_text(size = 14) 
  )
    }

#########################
## Figure S9: Empirical landscape
#########################

## Simply a snapshot from google earth, with a box drawn over the relevant region

#########################
## Figure S10: YF patterns across human density (same as main text dengue figure)
#########################

(
  FOI_on.s.f %>% 
    dplyr::select(X, Y
      , FOI_on_H
      , disease, land, dens, starting_host
      , forest, urban, Mosquito) %>%
    group_by(X, Y, disease, land, dens, Mosquito) %>% 
    filter(starting_host == "primate") %>% 
#    summarize(
#       FOI_on_H = sum(FOI_on_H)
#    ,  forest   = mean(forest)
#    ,  urban    = mean(urban)
#      ) %>% 
    group_by(Mosquito, dens) %>% 
      filter(
       X >= 1
     , X <= 11
     , Y >= 1
     , Y <= 11
    ) %>% 
    summarize(
      FOI_on_H = mean(FOI_on_H)
        ) %>% 
    mutate(
          Mosquito = mapvalues(Mosquito
            , from = c("1", "2", "3", "all")
            , to   = c("Aedes aegypti", "Aedes albopictus", "Haemagogus sp.", "All"))
        )
) %>% {
  ggplot(., aes(dens, FOI_on_H)) + 
    geom_line(aes(linetype = Mosquito)) +
    scale_linetype_manual(values = c("solid", "dotted", "dashed", "dotdash")) +
    xlab("Human population density") +
    ylab("Average Force of 
Infection on Humans") + 
 theme(
    legend.key.size = unit(.50, "cm")
  , legend.title = element_text(size = 14)
  , legend.text = element_text(size = 12)
  , legend.background = element_blank()
  , axis.text.x = element_text(size = 15) 
 # , axis.title.y = element_text(size = 12)
  , axis.text.y = element_text(size = 15) 
  , legend.position = c(0.80, 0.75)
  )
}

(
  FOI_on.s.f %>% 
    group_by(Mosquito, dens) %>% 
      filter(
       X >= 1
     , X <= 11
     , Y >= 1
     , Y <= 11
    ) %>% 
    filter(starting_host == "human") %>% 
    summarize(
      FOI_on_H = mean(FOI_on_H)
        ) %>% mutate(
          Mosquito = mapvalues(Mosquito
            , from = c("1", "2", "3", "all")
            , to   = c("Aedes aegypti", "Aedes albopictus", "Haemagogus sp.", "All"))
        )
) %>% {
  ggplot(., aes(dens, FOI_on_H)) + 
    geom_line(aes(linetype = Mosquito)) +
    scale_linetype_manual(values = c("solid", "dotted", "dashed", "dotdash")) +
    xlab("Human population density") +
    ylab("Average Force of 
Infection on Humans") + 
 theme(
    legend.key.size = unit(.50, "cm")
  , legend.title = element_text(size = 14)
  , legend.text = element_text(size = 12)
  , legend.background = element_blank()
  , axis.text.x = element_text(size = 15) 
 # , axis.title.y = element_text(size = 12)
  , axis.text.y = element_text(size = 15) 
  , legend.position = c(0.80, 0.75)
  )
}

#########################
## Figure S11: Exploration of patterns of Yellow Fever across landscapes
#########################

## Top panels are maps 

FOI_on.s.f$land <- round(FOI_on.s.f$land, 2)

(FOI_on.s.f %>% filter(
  disease == "Dengue"
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
      land == 0.38 |
      land == 0.58 | 
      land == 0.78
    , starting_host == "human"
    )
) %>% {
  ggplot(., aes(X, Y
       , z = FOI_on_H
    #   , z = forest
    #   , z = urban
      )) + 
   scale_fill_distiller(palette = "Spectral") +
#  scale_fill_gradient(
# low = "white", high = "tan4"
#  low = "white", high = "forestgreen"  
#  ) +
  geom_raster(aes(
      fill = FOI_on_H
  #   fill = forest
  #   fill = urban
    )) +
  xlab("Km") + 
  ylab("Km") + 
  guides(fill = guide_colorbar(
  title = "Second 
generation
infections")) +
#  guides(fill = guide_colorbar(
#  title = "Tree 
# Cover")) +
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

## Bottom panels are scatterplots

(FOI_on.s.f %>% filter(
  disease == "Yellow Fever"
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
    , starting_host == "primate"
    )
) %>% {
   ggplot(., aes(
    forest
  , FOI_on_H
  )
  ) + 
 geom_point(aes(
  # colour = forest
 #  colour = forest_var
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
 # , axis.title.y = element_text(size = 12)
  , axis.text.y = element_text(size = 12) 
  , legend.position = c(0.54, 0.35)
  )
}

#########################
## Figures S12: Tradeoffs between the diseases and descriptions of FOI as a function of landscape characteristics 
#########################

#####
## First portion of the figure is a view of the tradeoffs between diseases
#####

FOI_on.s.f.w <- FOI_on.s.f %>% 
  dplyr::select(FOI_on_H, disease, land, starting_host, dens, X, Y) %>% 
  group_by(disease, land, dens, X, Y) %>% summarize(FOI_on_H = sum(FOI_on_H)) %>% 
  pivot_wider(names_from = "disease", values_from = FOI_on_H)

FOI_on.s.f.w$land <- round(FOI_on.s.f.w$land, 2)

ggplot(
  (FOI_on.s.f.w %>% filter(
    dens == 250
# , (
#    land == unique(FOI_on.s.f.w$land)[2] | 
#    land == unique(FOI_on.s.f.w$land)[6] | 
#    land == unique(FOI_on.s.f.w$land)[10] 
#   )
    ))
  , aes(Dengue, Malaria)) + 
 geom_point(aes(colour = `Yellow Fever`), alpha = 0.5) +
 scale_colour_gradient(low = "darkgoldenrod1", high = "deepskyblue3", name = "Yellow Fever
Force of Infection") + 
 xlab("Dengue Force of Infection") + 
 ylab("Malaria Force of Infection") + 
 facet_wrap(~land) +
 theme(
    legend.key.size = unit(.45, "cm")
  , legend.title = element_text(size = 12)
  , legend.text = element_text(size = 14)
  , legend.background = element_blank()
  , axis.text.x = element_text(size = 12) 
  , axis.text.y = element_text(size = 12) 
  )

#####
## Second portion of the figure is summing the three foi across the diseases
#####

ggplot(
(FOI_on.s.f %>%
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
  #    disease   == "Malaria"
     #  land    == 0.18
      dens      == 250
  ) %>% 
    filter(
       X >= 1
     , X <= 11
     , Y >= 1
     , Y <= 11
    ) %>%
    group_by(
      X, Y, land
    ) %>% 
    summarize(
      FOI_on_H = sum(FOI_on_H)
    , forest   = mean(forest)
    , urban    = mean(urban)
    )
)
  , aes(
    urban
  , FOI_on_H
  )
  ) + 
 geom_point(aes(
     colour = forest
 #   colour = urban
   ), alpha = 0.8) +
 scale_colour_gradient(low = "white", high = "darkgreen"
  , name = "Tree cover"
  ) + 
# scale_colour_gradient(low = "white", high = "tan4"
#   , name = "Urbanization"
#   ) + 
  scale_x_continuous(breaks = c(0.10, 0.30, 0.50, 0.70, 0.90)) +
  xlab("Urbanization") + 
# xlab("Tree cover") + 
 ylab("Average Force of 
Infection on Humans") + 
 facet_wrap(~land
  # , scales = 'free'
   , nrow = 1
   ) +
 theme(
    legend.key.size = unit(.35, "cm")
  , legend.title = element_text(size = 12)
  , legend.text = element_text(size = 11)
  , legend.background = element_blank()
  , axis.text.x = element_text(size = 12) 
  , axis.text.y = element_text(size = 12) 
  , legend.position = "none" #c(0.27, 0.57)
  )

#####
## Third portion of the figure is breaking down the patterns across landscape features
#####

ggplot(
(FOI_on.s.f %>%
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
  #    disease   == "Malaria"
     #  land      == 0.18
      dens      == 250
  ) %>% 
    filter(
       X >= 1
     , X <= 11
     , Y >= 1
     , Y <= 11
    )# %>% group_by(
      #X, Y, land, dens
    #) %>% summarize(
    #   FOI_on_H = sum(FOI_on_H)
    #,  forest   = mean(forest)
    #,  urban    = mean(urban)
    #)
)
  , aes(
    urban
  , FOI_on_H
  )
  ) + 
 geom_point(aes(
     colour = forest
 #   colour = urban
   ), alpha = 0.8) +
 scale_colour_gradient(low = "white", high = "darkgreen"
  , name = "Tree cover"
  ) + 
# scale_colour_gradient(low = "white", high = "tan4"
#   , name = "Urbanization"
#   ) + 
  xlab("Urbanization") + 
# xlab("Tree cover") + 
 ylab("Average Force of Infection on Humans") + 
 facet_wrap(~disease
   , scales = 'free'
   , nrow = 3
   ) +
 theme(
    legend.key.size = unit(.35, "cm")
  , legend.title = element_text(size = 12)
  , legend.text = element_text(size = 11)
  , legend.background = element_blank()
  , axis.text.x = element_text(size = 12) 
  , axis.text.y = element_text(size = 12) 
  , legend.position = "none" #c(0.27, 0.57)
  )

#########################
## Figures S13: Stacked FOI (absolute instead of relative as is shown in the main text)
#########################

## See main text Figure 6

#########################
## Figures S14: Supplemental version of relative risk with a single infection arising on the landscape
#########################

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
       disease   == "Malaria"
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
       , z = FOI_on_H / max(FOI_on_H)
      )) + 
  scale_fill_distiller(palette = "Spectral") +
  geom_raster(aes(
     fill = FOI_on_H / max(FOI_on_H)
    )) +
  xlab("Km") + 
  ylab("Km") + 
  guides(fill = guide_colorbar(
  title = "Relative
Risk of 
Infection")) +
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
