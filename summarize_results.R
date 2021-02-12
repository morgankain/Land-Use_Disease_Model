####################################################################################
### Take calculated R0 and summarize for overall R0, direction of infection flow ###
####################################################################################

## Not really the cleanest way to do this... but do need some different summarizing based on the disease
if (disease[disease_counter] == "dengue") {

## Total mosquitoes infected 
h_m_mat.home.aegypti[i, j]    <- sum(h_m_mat[,,1])
h_m_mat.home.albopictus[i, j] <- sum(h_m_mat[,,2])
  
h_m_mat.s             <- apply(h_m_mat, 1:2, sum)
h_m_mat.home.s[i, j]  <- sum(h_m_mat.s)

## R0 for this infection is a combination of the number of humans and the number of others infected
m_h_mat.s            <- apply(m_h_mat, c(1, 2, 3), sum)
R0.home.s.h[i, j]    <- sum(m_h_mat.s[,,1])
R0.home.s.o[i, j]    <- sum(m_h_mat.s[,,2])
R0.home.s[i, j]      <- sum(m_h_mat.s)

## Fill in the FOI_all which tracks the force of infection into END cells from the current loop iteration [i, j]
 ## which is for STARTING infection
FOI_all.h[,,1] <- FOI_all.h[,,1] + m_h_mat[,,1,1]
FOI_all.h[,,2] <- FOI_all.h[,,2] + m_h_mat[,,1,2]
FOI_all.o[,,1] <- FOI_all.o[,,1] + m_h_mat[,,2,1]
FOI_all.o[,,2] <- FOI_all.o[,,2] + m_h_mat[,,2,2]

## Calculate the major direction of flow of infection from host to host after one generation
 ## (or just host to mosquito): recall, m_h_mat has dimensions landscape rows, landscape columns, hosts, mosquitoes

## This naming convention is definitely not intuitive....
 ## flow.one.aegypti means where the next generation of Humans (.one) get infected because
  ## of all of the aegypti mosquiotes (.aegypti) that were infected from the host that 
   ## was the origin of infection in [i, j]

## Extremely rare event, but it can happen that there are two cells with identical values...
 ## Should probably store both, but for now just store the first
flow.one.aegypti     <- which(m_h_mat[,,1,1] == max(m_h_mat[,,1,1]), arr.ind = T)[1, ]
flow.one.albopictus  <- which(m_h_mat[,,1,2] == max(m_h_mat[,,1,2]), arr.ind = T)[1, ]
flow.one.total       <- apply(m_h_mat[,,1,], 1:2, sum)
flow.one.total       <- which(flow.one.total == max(flow.one.total), arr.ind = T)[1, ]

flow.two.aegypti     <- which(m_h_mat[,,2,1] == max(m_h_mat[,,2,1]), arr.ind = T)[1, ]
flow.two.albopictus  <- which(m_h_mat[,,2,2] == max(m_h_mat[,,2,2]), arr.ind = T)[1, ] 
flow.two.total       <- apply(m_h_mat[,,2,], 1:2, sum)
flow.two.total       <- which(flow.two.total == max(flow.two.total), arr.ind = T)[1, ]
 
flow.both.aegypti    <- apply(m_h_mat[,,,1], 1:2, sum)
flow.both.aegypti    <- which(flow.both.aegypti == max(flow.both.aegypti), arr.ind = T)[1, ]
flow.both.albopictus <- apply(m_h_mat[,,,2], 1:2, sum)
flow.both.albopictus <- which(flow.both.albopictus == max(flow.both.albopictus), arr.ind = T)[1, ]
flow.both.total      <- apply(m_h_mat, 1:2, sum)
flow.both.total      <- which(flow.both.total == max(flow.both.total), arr.ind = T)[1, ]

flow.h.ae[,,flow.t] <- rbind(c(i, j), flow.one.aegypti)
flow.h.al[,,flow.t] <- rbind(c(i, j), flow.one.albopictus)
flow.h.bo[,,flow.t] <- rbind(c(i, j), flow.one.total)
flow.o.ae[,,flow.t] <- rbind(c(i, j), flow.two.aegypti)
flow.o.al[,,flow.t] <- rbind(c(i, j), flow.two.albopictus)
flow.o.bo[,,flow.t] <- rbind(c(i, j), flow.two.total)
flow.b.ae[,,flow.t] <- rbind(c(i, j), flow.both.aegypti)
flow.b.al[,,flow.t] <- rbind(c(i, j), flow.both.albopictus)
flow.b.bo[,,flow.t] <- rbind(c(i, j), flow.both.total)

## Just the first step (host to mosquito)
flow.ae.i_temp <- which(h_m_mat[,,1] == max(h_m_mat[,,1]), arr.ind = T)[1, ]
flow.ae.i      <- rbind(c(i, j), flow.ae.i_temp)
flow.al.i_temp <- which(h_m_mat[,,2] == max(h_m_mat[,,2]), arr.ind = T)[1, ]
flow.al.i      <- rbind(c(i, j), flow.al.i_temp)
flow.bo.i_temp <- which(h_m_mat.s == max(h_m_mat.s), arr.ind = T)[1, ]
flow.bo.i      <- rbind(c(i, j), flow.bo.i_temp)

landscape.out.temp <- list(
## Number of infected mosquitoes
  num_inf_mosquito.aegypti       = h_m_mat.home.aegypti         
, num_inf_mosquito.albopictus    = h_m_mat.home.albopictus      
, all_mosquitos                  = h_m_mat.home.s 
## Locations of the highest density of infected mosquitoes
, flow_to_mosquitoes.aegypti     = flow.ae.i
, flow_to_mosquitoes.albopictus  = flow.al.i
, flow_to_mosquitoes.both        = flow.bo.i
## R0 for the pixel
, R0                             = R0.home.s   
, R0.h                           = R0.home.s.h 
, R0.o                           = R0.home.s.o
## FOI into each focal cell from infections starting in all cells on the landscape
, FOI_on.h                       = FOI_all.h
, FOI_on.o                       = FOI_all.o
## Infectious flow for the whole life-cycle for one generation of host to host
, flow_to_humans.aegypti         = flow.h.ae       ## Infectious flow to humans (cell with most infections)
, flow_to_humans.albopictus      = flow.h.al       ## Infectious flow to humans (cell with most infections)
, flow_to_humans.both            = flow.h.bo       ## Infectious flow to humans (cell with most infections)
, flow_to_other.aegypti          = flow.o.ae       ## Infectious flow to "other" (cell with most infections)
, flow_to_other.albopictus       = flow.o.al       ## Infectious flow to "other" (cell with most infections)
, flow_to_other.both             = flow.o.bo       ## Infectious flow to "other" (cell with most infections)
, flow_to_hosts.aegypti          = flow.b.ae       ## Sum of infectious flow
, flow_to_hosts.albopictus       = flow.b.al       ## Sum of infectious flow
, flow_to_hosts.both             = flow.b.bo       ## Sum of infectious flow
)

} else if (disease[disease_counter] == "yellow_fever") {
  
## Total mosquitoes infected 
h_m_mat.home.aegypti[i, j]    <- sum(h_m_mat[,,1])
h_m_mat.home.albopictus[i, j] <- sum(h_m_mat[,,2])
h_m_mat.home.haemagogus[i, j] <- sum(h_m_mat[,,3])
  
h_m_mat.s             <- apply(h_m_mat, 1:2, sum)
h_m_mat.home.s[i, j]  <- sum(h_m_mat.s)

## R0 for this infection is a combination of the number of humans and the number of others infected
m_h_mat.s            <- apply(m_h_mat, c(1, 2, 3), sum)
R0.home.s.h[i, j]    <- sum(m_h_mat.s[,,1])
R0.home.s.p[i, j]    <- sum(m_h_mat.s[,,2])
R0.home.s.o[i, j]    <- sum(m_h_mat.s[,,3])
R0.home.s[i, j]      <- sum(m_h_mat.s)

## Fill in the FOI_all which tracks the force of infection into END cells from the current loop iteration [i, j]
 ## which is for STARTING infection. Add over i, j instead of storing all routes because of a lack of memory
FOI_all.h[,,1] <- FOI_all.h[,,1] + m_h_mat[,,1,1]
FOI_all.h[,,2] <- FOI_all.h[,,2] + m_h_mat[,,1,2]
FOI_all.h[,,3] <- FOI_all.h[,,3] + m_h_mat[,,1,3]
FOI_all.p[,,1] <- FOI_all.p[,,1] + m_h_mat[,,2,1]
FOI_all.p[,,2] <- FOI_all.p[,,2] + m_h_mat[,,2,2]
FOI_all.p[,,3] <- FOI_all.p[,,3] + m_h_mat[,,2,3]
FOI_all.o[,,1] <- FOI_all.o[,,1] + m_h_mat[,,3,1]
FOI_all.o[,,2] <- FOI_all.o[,,2] + m_h_mat[,,3,2]
FOI_all.o[,,3] <- FOI_all.o[,,3] + m_h_mat[,,3,3]

## Calculate the major direction of flow of infection from host to host after one generation
 ## (or just host to mosquito): recall, m_h_mat has dimensions landscape rows, landscape columns, hosts, mosquitoes

## This naming convention is a bit confusing...
 ## flow.one.aegypti means where the next generation of Humans (.one) get infected because
  ## of all of the aegypti mosquiotes (.aegypti) that were infected from the host that 
   ## was the origin of infection in [i, j]

## Extremely rare event, but it can happen that there are two cells with identical values...
 ## Should probably store both, but for now just store the first
flow.h.aegypti     <- which(m_h_mat[,,1,1] == max(m_h_mat[,,1,1]), arr.ind = T)[1, ]
flow.h.albopictus  <- which(m_h_mat[,,1,2] == max(m_h_mat[,,1,2]), arr.ind = T)[1, ]
flow.h.haemagogus  <- which(m_h_mat[,,1,3] == max(m_h_mat[,,1,3]), arr.ind = T)[1, ]
flow.h.total       <- apply(m_h_mat[,,1,], 1:2, sum)
flow.h.total       <- which(flow.h.total == max(flow.h.total), arr.ind = T)[1, ]

flow.p.aegypti     <- which(m_h_mat[,,2,1] == max(m_h_mat[,,2,1]), arr.ind = T)[1, ]
flow.p.albopictus  <- which(m_h_mat[,,2,2] == max(m_h_mat[,,2,2]), arr.ind = T)[1, ] 
flow.p.haemagogus  <- which(m_h_mat[,,2,3] == max(m_h_mat[,,2,3]), arr.ind = T)[1, ]
flow.p.total       <- apply(m_h_mat[,,2,], 1:2, sum)
flow.p.total       <- which(flow.p.total == max(flow.p.total), arr.ind = T)[1, ]

flow.o.aegypti     <- which(m_h_mat[,,3,1] == max(m_h_mat[,,3,1]), arr.ind = T)[1, ]
flow.o.albopictus  <- which(m_h_mat[,,3,2] == max(m_h_mat[,,3,2]), arr.ind = T)[1, ] 
flow.o.haemagogus  <- which(m_h_mat[,,3,3] == max(m_h_mat[,,3,3]), arr.ind = T)[1, ]
flow.o.total       <- apply(m_h_mat[,,3,], 1:2, sum)
flow.o.total       <- which(flow.o.total == max(flow.o.total), arr.ind = T)[1, ]
 
flow.both.aegypti    <- apply(m_h_mat[,,,1], 1:2, sum)
flow.both.aegypti    <- which(flow.both.aegypti == max(flow.both.aegypti), arr.ind = T)[1, ]
flow.both.albopictus <- apply(m_h_mat[,,,2], 1:2, sum)
flow.both.albopictus <- which(flow.both.albopictus == max(flow.both.albopictus), arr.ind = T)[1, ]
flow.both.haemagogus <- apply(m_h_mat[,,,3], 1:2, sum)
flow.both.haemagogus <- which(flow.both.haemagogus == max(flow.both.haemagogus), arr.ind = T)[1, ]
flow.both.total      <- apply(m_h_mat, 1:2, sum)
flow.both.total      <- which(flow.both.total == max(flow.both.total), arr.ind = T)[1, ]

flow.h.ae[,,flow.t] <- rbind(c(i, j), flow.h.aegypti)
flow.h.al[,,flow.t] <- rbind(c(i, j), flow.h.albopictus)
flow.h.ha[,,flow.t] <- rbind(c(i, j), flow.h.haemagogus)
flow.h.bo[,,flow.t] <- rbind(c(i, j), flow.h.total)

flow.p.ae[,,flow.t] <- rbind(c(i, j), flow.p.aegypti)
flow.p.al[,,flow.t] <- rbind(c(i, j), flow.p.albopictus)
flow.p.ha[,,flow.t] <- rbind(c(i, j), flow.p.haemagogus)
flow.p.bo[,,flow.t] <- rbind(c(i, j), flow.p.total)

flow.o.ae[,,flow.t] <- rbind(c(i, j), flow.o.aegypti)
flow.o.al[,,flow.t] <- rbind(c(i, j), flow.o.albopictus)
flow.o.ha[,,flow.t] <- rbind(c(i, j), flow.o.haemagogus)
flow.o.bo[,,flow.t] <- rbind(c(i, j), flow.o.total)

flow.b.ae[,,flow.t] <- rbind(c(i, j), flow.both.aegypti)
flow.b.al[,,flow.t] <- rbind(c(i, j), flow.both.albopictus)
flow.b.ha[,,flow.t] <- rbind(c(i, j), flow.both.haemagogus)
flow.b.bo[,,flow.t] <- rbind(c(i, j), flow.both.total)

## Just the first step (host to mosquito)
flow.ae.i_temp <- which(h_m_mat[,,1] == max(h_m_mat[,,1]), arr.ind = T)[1, ]
flow.ae.i      <- rbind(c(i, j), flow.ae.i_temp)
flow.al.i_temp <- which(h_m_mat[,,2] == max(h_m_mat[,,2]), arr.ind = T)[1, ]
flow.al.i      <- rbind(c(i, j), flow.al.i_temp)
flow.ha.i_temp <- which(h_m_mat[,,3] == max(h_m_mat[,,3]), arr.ind = T)[1, ]
flow.ha.i      <- rbind(c(i, j), flow.ha.i_temp)
flow.bo.i_temp <- which(h_m_mat.s == max(h_m_mat.s), arr.ind = T)[1, ]
flow.bo.i      <- rbind(c(i, j), flow.bo.i_temp)

landscape.out.temp <- list(
## Number of infected mosquitoes
  num_inf_mosquito.aegypti       = h_m_mat.home.aegypti         
, num_inf_mosquito.albopictus    = h_m_mat.home.albopictus
, num_inf_mosquito.haemagogus    = h_m_mat.home.haemagogus
, all_mosquitos                  = h_m_mat.home.s 
## Locations of the highest density of infected mosquitoes
, flow_to_mosquitoes.aegypti     = flow.ae.i
, flow_to_mosquitoes.albopictus  = flow.al.i
, flow_to_mosquitoes.albopictus  = flow.ha.i
, flow_to_mosquitoes.both        = flow.bo.i
## R0 for the pixel
, R0                             = R0.home.s   
, R0.h                           = R0.home.s.h
, R0.p                           = R0.home.s.p
, R0.o                           = R0.home.s.o
## FOI into each focal cell from infections starting in all cells on the landscape
, FOI_on.h                       = FOI_all.h
, FOI_on.p                       = FOI_all.p
, FOI_on.o                       = FOI_all.o
## Infectious flow for the whole life-cycle for one generation of host to host
, flow_to_humans.aegypti         = flow.h.ae       ## Infectious flow to humans (cell with most infections)
, flow_to_humans.albopictus      = flow.h.al       ## Infectious flow to humans (cell with most infections)
, flow_to_humans.haemagogus      = flow.h.ha       ## Infectious flow to humans (cell with most infections)
, flow_to_humans.both            = flow.h.bo       ## Infectious flow to humans (cell with most infections)
, flow_to_primates.aegypti       = flow.p.ae       ## Infectious flow to humans (cell with most infections)
, flow_to_primates.albopictus    = flow.p.al       ## Infectious flow to humans (cell with most infections)
, flow_to_primates.haemagogus    = flow.p.ha       ## Infectious flow to humans (cell with most infections)
, flow_to_primates.both          = flow.p.bo       ## Infectious flow to humans (cell with most infections)  
, flow_to_other.aegypti          = flow.o.ae       ## Infectious flow to "other" (cell with most infections)
, flow_to_other.albopictus       = flow.o.al       ## Infectious flow to "other" (cell with most infections)
, flow_to_other.haemagogus       = flow.o.ha       ## Infectious flow to "other" (cell with most infections)
, flow_to_other.both             = flow.o.bo       ## Infectious flow to "other" (cell with most infections)
, flow_to_hosts.aegypti          = flow.b.ae       ## Sum of infectious flow
, flow_to_hosts.albopictus       = flow.b.al       ## Sum of infectious flow
, flow_to_hosts.haemagogus       = flow.b.ha       ## Sum of infectious flow
, flow_to_hosts.both             = flow.b.bo       ## Sum of infectious flow
)

} else {
  
## Total mosquitoes infected 
h_m_mat.s             <- h_m_mat[,,1]
h_m_mat.home.s[i, j]  <- sum(h_m_mat.s)

## R0 for this infection
m_h_mat.s            <- apply(m_h_mat, c(1, 2, 3), sum)
R0.home.s.h[i, j]    <- sum(m_h_mat.s[,,1])
R0.home.s.o[i, j]    <- sum(m_h_mat.s[,,2])
R0.home.s[i, j]      <- sum(m_h_mat.s)

## Fill in the FOI_all which tracks the force of infection into END cells from the current loop iteration [i, j]
 ## which is for STARTING infection
FOI_all.h[,,1] <- FOI_all.h[,,1] + m_h_mat[,,1,1]
FOI_all.o[,,1] <- FOI_all.o[,,1] + m_h_mat[,,2,1]

## Calculate the major direction of flow of infection from host to host after one generation
 ## (or just host to mosquito): recall, m_h_mat has dimensions landscape rows, landscape columns, hosts, mosquitoes

## This naming convention is a bit confusing...
 ## flow.one.aegypti means where the next generation of Humans (.one) get infected because
  ## of all of the aegypti mosquiotes (.aegypti) that were infected from the host that 
   ## was the origin of infection in [i, j]

## Extremely rare event, but it can happen that there are two cells with identical values...
 ## Should probably store both, but for now just store the first
flow.one.total  <- apply(m_h_mat[,,1,], 1:2, sum)
flow.one.total  <- which(flow.one.total == max(flow.one.total), arr.ind = T)[1, ]

flow.two.total  <- apply(m_h_mat[,,2,], 1:2, sum)
flow.two.total  <- which(flow.two.total == max(flow.two.total), arr.ind = T)[1, ]
 
flow.both.total <- apply(m_h_mat, 1:2, sum)
flow.both.total <- which(flow.both.total == max(flow.both.total), arr.ind = T)[1, ]

flow.h.bo[,,flow.t] <- rbind(c(i, j), flow.one.total)
flow.o.bo[,,flow.t] <- rbind(c(i, j), flow.two.total)
flow.b.bo[,,flow.t] <- rbind(c(i, j), flow.both.total)

## Just the first step (host to mosquito)
flow.bo.i_temp      <- which(h_m_mat.s == max(h_m_mat.s), arr.ind = T)[1, ]
flow.bo.i[,,flow.t] <- rbind(c(i, j), flow.bo.i_temp)

## Shortcutting to keep the names the same as for the other diseases. Both means the total to num_mosq, which 
 ## for malaria is just the one mosquito
landscape.out.temp <- list(
## Number of infected mosquitoes
  all_mosquitos                  = h_m_mat.home.s 
## Locations of the highest density of infected mosquitoes
, flow_to_mosquitoes.both        = flow.bo.i
## R0 for the pixel
, R0                             = R0.home.s      
, R0.h                           = R0.home.s.h 
, R0.o                           = R0.home.s.o
## FOI into each focal cell from infections starting in all cells on the landscape
, FOI_on.h                       = FOI_all.h
, FOI_on.o                       = FOI_all.o
## Infectious flow for the whole life-cycle for one generation of host to host
, flow_to_humans.both            = flow.h.bo       ## Infectious flow to humans (cell with most infections)
, flow_to_other.both             = flow.o.bo       ## Infectious flow to "other" (cell with most infections)
, flow_to_hosts.both             = flow.b.bo       ## Sum of infectious flow
)
  
}

landscape.out.temp <- c(landscape.out.temp, list(landscape[[landscape_counter]]))
names(landscape.out.temp)[length(landscape.out.temp)] <- "landscape"

if (!sim_landscape) {
  landscape.out.temp <- c(landscape.out.temp, list(names(sim_land)[disease_counter]))
  names(landscape.out.temp)[length(landscape.out.temp)] <- "land_type"
}

## Continually overright with the next piece of data, saving from time to time for larger runs
landscape.out.top[[
  paste(disease[disease_counter], landscape_counter, starting_host[starting_host_counter], sep = "_")
  ]] <- landscape.out.temp

