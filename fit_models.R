#####################################################################
### Fit models to physiological responses of hosts and mosquitoes ###
#####################################################################

## Models currently focused on Dengue, Yellow Fever, and Malaria, but set up to use data 
 ## for w/e disease. A few models have convergence issues but still give estimates with reasonable CI

## Host titer profiles
host_titer <- read.csv("disease_parameters/host_titer.csv") %>% 
  mutate(dose = log10(Dose)) %>%
  filter(Titer_mean > 0)

## Host titer will me modeled separately for each disease so separate. Yes, column is named Virus despite malaria being here as well
host_titer.D  <- host_titer %>% filter(Virus == "Dengue")
host_titer.YF <- host_titer %>% filter(Virus == "Yellow Fever")
host_titer.M  <- host_titer %>% filter(Virus == "Malaria")

## Add days after the acute phase to be the same as the pre-phase
host_titer.M  <- rbind(host_titer.M, host_titer.M[6:1, ])
host_titer.M  <- host_titer.M %>% mutate(Day = seq(1, n()))

## Probability that a mosquito gets infected when feeding on a host with a specific titer 
h_to_m     <- read.csv("disease_parameters/host_to_mosquito_transmission.csv")

## Combining all anopheles into one genus (necessary because of a lack of data, but definitely unfortunate)
h_to_m     <- h_to_m %>%
  filter((Genus == "Aedes" & Species == "aegypti") |
         (Genus == "Aedes" & Species == "albopictus") |
          Genus == "Anopheles")

## For now fit one model for the two viruses and another for Malaria which will behave quite differently given massively different density in the host
h_to_m.DYF <- h_to_m %>% filter(Virus != "Malaria")
h_to_m.M   <- h_to_m %>% filter(Virus == "Malaria" & (Method == "Membrane" | Method == "Skin")) %>%
  mutate(log10titer = log10(Titer))

## Transmission of the disease from infected mosquitoes to susceptible hosts. 
m_to_h     <- read.csv("disease_parameters/mosquito_to_host_transmission.csv")
m_to_h     <- m_to_h %>%
  filter((Genus == "Aedes" & Species == "aegypti") |
         (Genus == "Aedes" & Species == "albopictus") |
          Genus == "Anopheles")

## Transmission of Haemagogus relies on a different data set because of methodological differences due to very old papers
haem_trans <- read.csv("disease_parameters/haem_trans.csv")

## For all titer responses assume quadratic, but could consider a non-symmetric curve as well such as a Ricker function x*e^(x) (these
 ## functions are set up in functions.R but not actually used here)

## Titer. Have to do these separately because hosts and virus has no overlap
host_titer.mod.D  <- lm(
    log(Titer_mean) ~ poly(Day, 2)
  , data    = host_titer.D
  )

host_titer.mod.YF  <- lm(
    log(Titer_mean) ~ poly(Day, 2)
  , data    = host_titer.YF
  )

## Doesn't fit the data very well, but the data is so bad so I don't expect much better
host_titer.mod.M  <- lm(
    log10(Titer_mean) ~ poly(Day, 2)
  , data    = host_titer.M
  )

## Percent from plot extraction leads to fractional infected, so need to round for Binomial. 
 ## Rounding up and down should be random so won't bias answer 
h_to_m.M <- h_to_m.M %>% mutate(N_Infected = round(N_Infected))

## ``Artificially'' pull down the intercept a bit by adding a 0 infection probability at 0 malaria parasites in order to 
 ## avoid crazy answers at low parasite density
h_to_m.M <- rbind(h_to_m.M
  , data.frame(
      Day     = rep(14, 3), Titer = rep(0, 3), N_Tested = rep(1000, 3), N_Infected = rep(0, 3), Genus = rep("Anopheles", 3)
    , Species = c("gambiae", "dirus", "darlingi"), Mosquito_Strain = rep(NA, 3), Method = rep(NA, 3)
    , Virus = rep("Malaria", 3), Serotype = rep(NA, 3), Virus_Strain = rep(NA, 3)
    , Citation = rep(NA, 3), Figure_Table = rep(NA, 3), Notes = rep(NA, 3), log10titer = rep(-4, 3)))

## Host to Mosquito transmission
h_to_m_trans.mod.DYF <- glmer(
    cbind(N_Infected, N_Tested - N_Infected) ~ Day + Titer*Species*Virus +
  + (1 + Titer | Citation)
  , data    = h_to_m.DYF
  , family  = "binomial")

h_to_m_trans.mod.M <- glm(
    cbind(N_Infected, N_Tested - N_Infected) ~ log10titer*Species
  , data    = h_to_m.M
  , family  = "binomial")

## Variation in titer here is just a nuisance and don't care about 0 titer, so center titer for this model
m_to_h <- m_to_h %>% mutate(Titer.cen = (Titer - mean(Titer)) / sd(Titer))

## Given poor data from very old papers (1930s-1970s) on Haemagogus transmission of yellow fever, we rely on a different model for
 ## the transmission of yellow fever by Haemagogus
## First, these experiments did not use virus load in salivary glands because of old methods but instead let (usually) pools of mosquitoes
 ## feed on an infected host and then feed on a susceptible host. The issue with these data are that there is no way to tell how many of the mosquitoes
  ## got infected, just that _at least one_ mosquito was infected and then able to transmit. These data do help give a general picture of extrinsic incubation
   ## period, but do not provide the granularity that we need here (proportion of mosquitoes transmitting over time). 
    ## These data are summarized here for a plot, but we do not actually use these data in any model.

Haem_pooled_inf <- ggplot(
  (haem_trans %>% dplyr::filter(style == "pool"))
  , aes(day, trans)) +  geom_point(aes(size = count)) +
  xlab("Days post infection") +
  ylab("Transmission Occurred") +
  scale_size_continuous(name = "Number of
Mosquitoes")

## However, a small subset of these experiments do track individual mosquitoes. We restrict our models to using these data because it allows us to capture
 ## the actual proportion of mosquitoes on day X post infection that are infectious, which corresponds to the other mosquito transmission models used here.
  ## One issue with these data however is incomplete information on dose, strain and other information that would help to resolve the patterns. Thus, they
   ## look very noisy. They are the best we have though so we continue with them
    ## Some summary needed first to get the data into proportions:

haem_trans.s.n <- haem_trans %>% filter(style == "single", trans == 0) %>% dplyr::select(-trans, -style, -mosquito_species)
names(haem_trans.s.n)[3] <- "count.n"
haem_trans.s.y <- haem_trans %>% filter(style == "single", trans == 1) %>% dplyr::select(-trans, -style, -mosquito_species)
names(haem_trans.s.y)[3] <- "count.y"
haem_trans.s   <- left_join(haem_trans.s.n, haem_trans.s.y, by = c("record", "day"))

haem_trans.s.s <- haem_trans.s %>% mutate(total = count.n + count.y)
haem_trans.s.s[is.na(haem_trans.s.s$total), ]$total <- haem_trans.s.s[is.na(haem_trans.s.s$total), ]$count.n
haem_trans.s.s <- haem_trans.s.s %>% mutate(prop = 1 - (count.n / total))

## Mosquito to Host transmission
m_to_h_trans.mod  <- glmer(
    cbind(N_Transmitting, N_Tested - N_Transmitting) ~ Day*Species + Titer.cen +
  + (1 | Citation)
  , data    = m_to_h[m_to_h$Genus != "Anopheles", ]
  , family  = "binomial")

m_to_h_trans.mod.YF.H <- glm(
  prop ~ day, family = "binomial", weights = total, data = haem_trans.s.s
)

## ``Artificially'' pull down the intercept a bit by adding a 0 infection probability at 0 Malaria, again in order to 
 ## avoid crazy answers at low parasite density
m_to_h <- rbind(m_to_h
  , data.frame(
      Day     = rep(0, 4), Titer = rep(5, 4), N_Tested = rep(1000, 4), N_Transmitting = rep(0, 4), Genus = rep("Anopheles", 4)
    , Species = rep("yoelii", 4), Mosquito_Strain = rep(NA, 4), Virus = rep("Malaria", 4), Serotype = rep(NA, 4), Virus_Strain = rep(NA, 4)
    , Temperature = c(20, 26, 24, 22), Citation = rep(NA, 4), Figure_Table = rep(NA, 4), Notes = rep(NA, 4), Titer.cen = rep(-1.007498, 4))
)

## Quadratic temperature term given that both 20 and 26 degrees have lower incubation rates than the intermediate temperatures
m_to_h_trans.mod.M  <- glm(
    cbind(N_Transmitting, N_Tested - N_Transmitting) ~ Day*poly(Temperature,2)
  , data    = m_to_h[m_to_h$Genus == "Anopheles", ]
  , family  = "binomial")

####
## Model thoughts/updates/desires
####

## 1) I would like to fit a random effect for mosquito strain or Day | Citation, but too many citations
 ## measured for only a single day, so not possible currently. If they are all pegged at 0 this *could*[?] be possible (but likely would have convergence issues)
## 2) Would also like more fixed effect interactions (Mosquito_Species*Day) but this is basically impossible with the data
## 3) There is so little data for Dengue, that I just fit the model without Virus as a fixed effect (just assume the strong simplifying assumption of the same mosquito to host 
 ## transmission for Dengue and Yellow Fever)

####
## No model, but bring in biting preference raw numbers --- see parameter.R for a bit more detail about biting
####

bite.data    <- read.csv("disease_parameters/feeding_preference.csv")
bite.data.an <- bite.data %>% filter(Genus == "Anopheles")
bite.data.ae <- bite.data %>% filter(Genus == "Aedes")
bite.data.ha <- bite.data %>% filter(Genus == "Haemagogus")

## Use Feeding_Ratio where possible (which considers host population)
 ## and convert HBI (human biting index, the proportion of blood from a human) to the odds a bite will come
  ## on a human when not possible: HBI / 1 - HBI to calculate biting of humans relative to others 
   ## Note that this is quite a sub-optimal way to get feeding _preference_ because it does not take into consideration the 
    ## abundance of each host, but that is impossible heregiven the lack of info about the host communities in these publications

## Aedes
bite.data.ae.fr   <- bite.data.ae %>% filter(!is.na(Feeding_Ratio)) %>% dplyr::select(Feeding_Ratio, Species)
bite.data.ae.fr   <- bite.data.ae.fr %>% mutate(HBI = 1 - (1 / (Feeding_Ratio + 1))) %>% dplyr::select(-Feeding_Ratio)
bite.data.ae.hbi  <- bite.data.ae %>% filter(!is.na(HBI)) %>% dplyr::select(HBI, Species)
bite.data.ae.bite <- rbind(bite.data.ae.fr, bite.data.ae.hbi)
bite.data.ae.bite <- bite.data.ae.bite %>% group_by(Species) %>% summarize(FR = median(HBI))

## Anopheles. Focus on darlini here to hone the model to South America
bite.data.an.hbi  <- bite.data.an %>% filter(!is.na(HBI)) %>% filter(Species == "darlingi") %>% dplyr::select(HBI, Genus) 
bite.data.an.bite <- bite.data.an.hbi %>% group_by(Genus) %>% summarize(FR = median(HBI))

## Haemagogus
bite.data.ha.hbi  <- bite.data.ha %>% filter(!is.na(HBI)) %>% dplyr::select(HBI, Genus)
bite.data.ha.bite <- bite.data.ha.hbi %>% group_by(Genus) %>% summarize(FR = median(HBI))

####
## No model, but bring in biting rate numbers for Haemagogus using gonotrophic cycle length
####

cl.data  <- read.csv("disease_parameters/gonotrophic_cycle_length.csv")
cl.data  <- cl.data  %>% summarize(cycle_length = mean(Cycle_Length)) %>% mutate(cycle_length = 1 / cycle_length)
