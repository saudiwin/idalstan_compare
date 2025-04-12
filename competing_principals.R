# JAGS code for the competing principals model

library(rjags)

###########################################################################
# Competing principals model
###########################################################################

# This is the model with beta-distributed priors on the deltas
compete="model {
  for (i in 1:n.legislators) {
    for (j in 1:n.item) {
      probit(CJR[i,j]) <- beta[j]*theta[i] - alpha[j];
    }
  }
  for (k in 1:2) {
    for (j in 1:n.item) {
      lead[k,j] ~ dbern (0.5)
    }
  }
  for (i in 1:n.partisans) {
    for (j in 1:n.item) {
      Y[i,j] ~ dcat(Q[i,j,1:3]);         # code Nay as 1, NA as 2, Aye as 3
      Q[i,j,1] <- pow(probVoteDisagree*(1-CJR[i,j]), p[i,j]) * pow(probVoteAgree*(1-CJR[i,j]), (1 - p[i,j]));
      Q[i,j,2] <- 1 - Q[i,j,1] - Q[i,j,3];
      Q[i,j,3] <- pow(probVoteAgree*CJR[i,j], p[i,j]) * pow(probVoteDisagree*CJR[i,j], (1 - p[i,j]));
      p[i,j] <- lead[party.membership[i],j]; 
    }
  }
  for (i in (n.partisans+1):n.legislators) {
    for (j in 1:n.item) {
      Y[i,j] ~ dbern(CJR[i,j]);
    }
  }
# priors
#   delta[1] ~ dbeta(1,5); # This prior puts a lot of mass on values close to 0
#   delta[2] ~ dbeta(5,1); # This prior puts a lot of mass on values close to 1
delta[1] ~ dbeta(1,1); 
delta[2] ~ dbeta(1,1); 
probVoteDisagree <- delta[1];
probVoteAgree    <- delta[2];
for(j in 1:n.item) { alpha[j] ~ dnorm(0, 0.25); }
  beta[fix.bill[1]] <-  4
  for(j in (fix.bill[1]+1):(fix.bill[2]-1)) { beta[j]  ~ dnorm(0, 0.25); }
  beta[fix.bill[2]] <- -4
  for(j in (fix.bill[2]+1):n.item) { beta[j]  ~ dnorm(0, 0.25); }
for(i in 1:n.legislators)  { theta[i] ~ dnorm(0, 1); }
}"

party.membership <- ifelse (rc.exp$US.client==1, 1, ifelse(rc.exp$Soviet.client==1, 2, 3)) 
new.ord <- order (party.membership)
party.membership <- party.membership[new.ord]
RC <- rc.exp[new.ord,3:70]
rownames (RC) <- rc.exp$country[new.ord]
new.RC <- RC #  RC[-c(40,50),]  # Use this if USSR (50) and US (40) are omitted

#Quick checks
party.membership[rownames(new.RC)=="UNITED.STATES"]  # should be 1
party.membership[rownames(new.RC)=="U.S.S.R."]       # should be 2

lead.SOV <- RC["U.S.S.R.",]
lead.USA <- RC["UNITED.STATES",]
lead <- rbind(lead.USA, lead.SOV)
# code NA as 2, Nay as 1, Aye as 3 (40 and 50 are rows for US and USSR in the re-arranged roll-call matrix)
y <- ifelse(is.na(new.RC)==TRUE, 2, ifelse(new.RC==0,1,3))    
n.legislators <- nrow (y)

# Use the next line if want to omit US and USSR as "whips"
# party.membership.no.whips <- party.membership[-c(19,52)]
party.membership.no.whips <- party.membership
n.partisans <- length (party.membership.no.whips[party.membership.no.whips<3])
n.item <- ncol (y)
Y1 <- y[1:n.partisans,]
Y2 <- new.RC[(n.partisans+1):n.legislators,]
Y  <- rbind (Y1, Y2)
fix.bill <- c(1,57)

compete.parameters = c("theta", "alpha", "beta", "probVoteAgree", "lead", "probVoteDisagree", "deviance")

compete.inits <- function() {
  dump.format(
    list(
      theta = rnorm(n.legislators)
      , alpha = rnorm(n.item)
      , beta = c(NA, rnorm(fix.bill[2]-fix.bill[1]-1), NA, rnorm(n.item-fix.bill[2]))
      , delta = sort(runif (2)) 
      ,.RNG.name="base::Wichmann-Hill"
      ,.RNG.seed=randomNumbers(n = 1, min = 1, max = 1e+04,col=1))
  )
}

compete.data <- dump.format(list(Y=as.matrix(Y), lead=as.matrix(lead), party.membership=party.membership.no.whips[1:n.partisans], n.partisans=n.partisans, n.legislators=n.legislators, n.item=n.item, fix.bill=fix.bill))

#  This next step already done, simply import the RData file named "UNCompeteRun.RData", which has object "out2"
system.time(
  out2 <- mclapply(1:2, function(x) {
    jags.compete <- try(run.jags(
      model=compete,
      monitor=compete.parameters,
      n.chains=2,
      data=compete.data,
      inits=compete.inits(),
      thin=100, burnin=50000, sample=100000,
#       thin=5, burnin=200, sample=200,
      check.conv=FALSE, plots=FALSE
    ))
    if(inherits(jags.compete,"try-error")) {return()}
    return(jags.compete)
  }, mc.cores=2 )
)
# 
# save (out2, file="UNCompeteRun.RData")
load ("UNCompeteRun.RData")