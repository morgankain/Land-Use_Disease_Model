#########################
### Result containers ###
#########################

## Number of infected mosquitoes and flow of infection to these mosquitoes 
h_m_mat.home.s          <- matrix(data = 0, nrow = dim(landscape[[landscape_counter]])[1], ncol = dim(landscape[[landscape_counter]])[2])
h_m_mat.home.aegypti    <- matrix(data = 0, nrow = dim(landscape[[landscape_counter]])[1], ncol = dim(landscape[[landscape_counter]])[2])
h_m_mat.home.albopictus <- matrix(data = 0, nrow = dim(landscape[[landscape_counter]])[1], ncol = dim(landscape[[landscape_counter]])[2])
h_m_mat.home.haemagogus <- matrix(data = 0, nrow = dim(landscape[[landscape_counter]])[1], ncol = dim(landscape[[landscape_counter]])[2])

flow.ae.i      <- array(data  = 0, dim = c(2, 2, dim(landscape[[landscape_counter]])[1] * dim(landscape[[landscape_counter]])[2]))
flow.al.i      <- array(data  = 0, dim = c(2, 2, dim(landscape[[landscape_counter]])[1] * dim(landscape[[landscape_counter]])[2]))
flow.ha.i      <- array(data  = 0, dim = c(2, 2, dim(landscape[[landscape_counter]])[1] * dim(landscape[[landscape_counter]])[2]))
flow.bo.i      <- array(data  = 0, dim = c(2, 2, dim(landscape[[landscape_counter]])[1] * dim(landscape[[landscape_counter]])[2]))

## Flow of infection from infected host to new generation of infected hosts
flow.h.ae      <- array(data  = 0, dim = c(2, 2, dim(landscape[[landscape_counter]])[1] * dim(landscape[[landscape_counter]])[2]))
flow.h.al      <- array(data  = 0, dim = c(2, 2, dim(landscape[[landscape_counter]])[1] * dim(landscape[[landscape_counter]])[2]))
flow.h.ha      <- array(data  = 0, dim = c(2, 2, dim(landscape[[landscape_counter]])[1] * dim(landscape[[landscape_counter]])[2]))
flow.h.bo      <- array(data  = 0, dim = c(2, 2, dim(landscape[[landscape_counter]])[1] * dim(landscape[[landscape_counter]])[2]))
flow.o.ae      <- array(data  = 0, dim = c(2, 2, dim(landscape[[landscape_counter]])[1] * dim(landscape[[landscape_counter]])[2]))
flow.o.al      <- array(data  = 0, dim = c(2, 2, dim(landscape[[landscape_counter]])[1] * dim(landscape[[landscape_counter]])[2]))
flow.o.ha      <- array(data  = 0, dim = c(2, 2, dim(landscape[[landscape_counter]])[1] * dim(landscape[[landscape_counter]])[2]))
flow.o.bo      <- array(data  = 0, dim = c(2, 2, dim(landscape[[landscape_counter]])[1] * dim(landscape[[landscape_counter]])[2]))
flow.b.ae      <- array(data  = 0, dim = c(2, 2, dim(landscape[[landscape_counter]])[1] * dim(landscape[[landscape_counter]])[2]))
flow.b.al      <- array(data  = 0, dim = c(2, 2, dim(landscape[[landscape_counter]])[1] * dim(landscape[[landscape_counter]])[2]))
flow.b.ha      <- array(data  = 0, dim = c(2, 2, dim(landscape[[landscape_counter]])[1] * dim(landscape[[landscape_counter]])[2]))
flow.b.bo      <- array(data  = 0, dim = c(2, 2, dim(landscape[[landscape_counter]])[1] * dim(landscape[[landscape_counter]])[2]))

## R0
R0.home.s      <- matrix(data = 0, nrow = dim(landscape[[landscape_counter]])[1], ncol = dim(landscape[[landscape_counter]])[2])
R0.home.s.h    <- matrix(data = 0, nrow = dim(landscape[[landscape_counter]])[1], ncol = dim(landscape[[landscape_counter]])[2])
R0.home.s.o    <- matrix(data = 0, nrow = dim(landscape[[landscape_counter]])[1], ncol = dim(landscape[[landscape_counter]])[2])

## FOI breakdown on humans by mosquito type

## FOI on a given cell from starting infection in all surrounding cells. Would be best to be able to summarize this, but given that 
 ## I do not know yet the best way to weight by starting probability (something about human population size?), store an array
  ## that provides for a given cell, the FOI of infection into that cell from all possible starting infections
## Here each dim [ , , i] provides the END cell and for [, , i], [x, y, i] gives the FOI from a starting infection in that cell 
 ## e.j. a given matrix that you see when you print(FOI_all.h) shows all of the FOI values from all of the cells surrounding the focal cell's
  ## matrix to that focal cell that you are viewing

FOI_all.h        <- array(
  data = 0 
, dim = c(
  dim(landscape[[landscape_counter]])[1]
, dim(landscape[[landscape_counter]])[2] 
, num_mosq
  )
)

FOI_all.o        <- array(
  data = 0 
, dim = c(
  dim(landscape[[landscape_counter]])[1]
, dim(landscape[[landscape_counter]])[2] 
, num_mosq
  )
)

 ## only used for yellow fever
if (disease[disease_counter] == "yellow_fever") {
flow.p.ae      <- array(data  = 0, dim  = c(2, 2, dim(landscape[[landscape_counter]])[1] * dim(landscape[[landscape_counter]])[2]))
flow.p.al      <- array(data  = 0, dim  = c(2, 2, dim(landscape[[landscape_counter]])[1] * dim(landscape[[landscape_counter]])[2]))
flow.p.ha      <- array(data  = 0, dim  = c(2, 2, dim(landscape[[landscape_counter]])[1] * dim(landscape[[landscape_counter]])[2]))
flow.p.bo      <- array(data  = 0, dim  = c(2, 2, dim(landscape[[landscape_counter]])[1] * dim(landscape[[landscape_counter]])[2]))
R0.home.s.p    <- matrix(data = 0, nrow = dim(landscape[[landscape_counter]])[1], ncol   = dim(landscape[[landscape_counter]])[2])

#FOI_all.p        <- array(data = 0, dim = c(
#  dim(landscape[[landscape_counter]])[1]
#, dim(landscape[[landscape_counter]])[2]
#, dim(landscape[[landscape_counter]])[1] * dim(landscape[[landscape_counter]])[2]
#))

FOI_all.p        <- array(
  data = 0 
, dim = c(
  dim(landscape[[landscape_counter]])[1]
, dim(landscape[[landscape_counter]])[2] 
, num_mosq
  )
)

}

