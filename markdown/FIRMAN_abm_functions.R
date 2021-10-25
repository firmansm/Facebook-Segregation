## Functions
### Agent Population

#This function generates a population with the following parameters. 

#Input:
#n       = number of agents
#si      = number of social identity
#w_si    = weight for each social identity
#p_mino  = parameter for minority for each social identity; Bernoulli distribution with regard to w_si
#p_to    = parameter for tie outreachability; binomial distribution with trial equal to max of possible value of w_si
#p_tc    = parameter for tie capacity
#m_tc = mean, normal distribution
#d_tc = standard deviation, normal distribution

#Output:
#  dataframe with
#SI = social identity 1 through n
#TO = tie outreachability
#TC = tie capacity
#AG = agent


agents <- function(n=100, si=2, w_si=c(1,2), p_mino=c(.5, .5), p_to=.5, p_tc=c(m_tc=3, sd_tc=1)) {
  if(length(p_mino)<si){
    p_mino = rep(p_mino,si) #by default p_mino are the same for all social identities
  }
  if(length(w_si)<si){
    w_si = rep(w_si,si)#by default w_si are the same for all social identity weights
  }
  population <- matrix(NA, nrow=n, ncol=si)
  for (i in 1:si){
    population[,i] <- w_si[i]*rbinom(n, 1, p_mino[i]) #generating weighted social identity
  }
  population <- as.data.frame(population)
  colnames(population) <- paste0("SI", 1:si)
  population$TO <- rbinom(nrow(population), sum(w_si[1:si]), p_to) #generating tie outreachability
  population$TC <- abs(round(rnorm(nrow(population), p_tc[[1]], p_tc[[2]]),0)) #generating tie capacity
  population$AG <- 1:n #generating agent's names
  return(population)
}





### Friendship Development

#This function takes input from agent population then generates dyadic friendships.
#It always reaches an equlibrium, in which, given the tie outreachabiity and tie capacity parameters, no additional friendship can be further made.

#Input:
 # dataset from agents

#Output:
 # dataframe with 
#$friends_yes = developed friendship
#AG1, AG2        = agents in the dyads
#DIST            = social identity distance, calculated using Manhattan distance
#AG1_TO, AG2_TO  = agents' tie outreachability
 # $friends_no  = undeveloped friendship
   # AG1, AG2        = agents in the dyads
    #DIST            = social identity distance, calculated using Manhattan distance
   # AG1_TO, AG2_TO  = agents' tie outreachability
#WHY_NOT         = reason why two agents can be friends
#0         = one or both agents can only generate zero-length of ties
#99        = one or both agents have reached their maximum tie capacity

#This the improved version, which is faster from the previous version.


friendships <- function(data=must_from_agents) {
  
  potential_friends <- t(combn(1:nrow(data), 2)) #all possible friendships
  random_potential_friends <- potential_friends[sample(nrow(potential_friends)),] #randomized order of potential friends
  
  
  #### filter based on tie outreachability ####
  
  distance_friends <- as.matrix(dist(data[,1:(ncol(data)-3)], upper=T, diag=T, method="manhattan")) #social identity distance
  random_potential_friends <- cbind(random_potential_friends,
                                    sapply(1:nrow(random_potential_friends), function(i) distance_friends[random_potential_friends[i,][1], random_potential_friends[i,][2]]) #assign social identity disstance to dyads
  )
  
  random_potential_friends <- cbind(random_potential_friends,
                                    sapply(random_potential_friends[,1], function(i) data$TO[data$AG==i]) #assign TO for agent1
  )
  random_potential_friends <- cbind(random_potential_friends,
                                    sapply(random_potential_friends[,2], function(i) data$TO[data$AG==i]) #assign TO for agent2
  )
  
  random_potential_friends <- cbind(random_potential_friends,
                                    sapply(1:nrow(random_potential_friends), 
                                           function(i) ifelse(random_potential_friends[i,][3] <= min(random_potential_friends[i,][4], random_potential_friends[i,][5]), 
                                                              1, 0)) #1: yes reciprocated, 0: no reciprocated
  )
  
  random_potential_friends_reciprocated <- random_potential_friends[random_potential_friends[,6] == 1,] #filter only those reciprocated
  
  #### matrix for friends yes and friends no####
  
  potential_friends_filtered <- random_potential_friends_reciprocated[,1:5]
  friends_yes <- matrix(NA, nrow(potential_friends_filtered), 5)
  friends_no <- matrix(NA, nrow(potential_friends_filtered), 5)
  
  
  #### filter based on tie capacity ####
  
  for(i in 1:nrow(potential_friends_filtered)){ 
    if (
      isTRUE(length(which(friends_yes[,1:2] == potential_friends_filtered[i,][1])) < data$TC[potential_friends_filtered[i,][1]])
      &
      isTRUE(length(which(friends_yes[,1:2] == potential_friends_filtered[i,][2])) < data$TC[potential_friends_filtered[i,][2]])
    ) { friends_yes[i,] <- potential_friends_filtered[i,] } 
    else {
      friends_no[i,] <- potential_friends_filtered[i,]
    }
  }
  
  
  #### why not friends
  
  random_potential_friends_unreciprocated <- random_potential_friends[random_potential_friends[,6] == 0,]
  random_potential_friends_unavailable <- cbind(friends_no, rep(99,nrow(friends_no)))
  friends_no <- rbind(random_potential_friends_unreciprocated, random_potential_friends_unavailable) 
  
  #### results ####
  
  friends_yes <- as.data.frame(na.omit(friends_yes))
  colnames(friends_yes) <- c("AG1", "AG2", "DIST", "AG1_TO", "AG2_TO")
  friends_no <- as.data.frame(na.omit(friends_no))
  colnames(friends_no) <- c("AG1", "AG2", "DIST", "AG1_TO", "AG2_TO", "WHY_NOT")
  friends <- list("friends_yes" = friends_yes, "friends_no" = friends_no)
  
  
  return(friends)
  
}
