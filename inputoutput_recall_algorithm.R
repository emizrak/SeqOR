
#### sample size 5, requires 150 trials

# first step: use 120 possible trials, each once 
ntrials<- 180

mmat <- matrix(NA_integer_, nrow = 720, ncol = 6)
i <- 1

el <- 1:6

for (l1 in el) {
  for (l2 in el[-l1]) {
    for (l3 in el[-c(l1, l2)]) {
      for (l4 in el[-c(l1, l2, l3)]) {
        for (l5 in el[-c(l1, l2, l3, l4)]) {
          for (l6 in el[-c(l1, l2, l3, l4,l5)]) {
           mmat[i, ] <- c(l1, l2, l3, l4, l5,l6)
           i <- i + 1
        }
      }
    }
  }
  }
}
head(mmat)

ml <- reshape2::melt(mmat)
with(ml, table(Var2, value)) 

outm <- mmat[sample(1:nrow(mmat), ntrials),]

mlout <- reshape2::melt(outm)
test<- with(mlout, table(Var2, value)) 

#take one number from 1 to nrow(mmat)  put it in outm and put this number in the first list position 
travpos <- vector("list",ntrials)
ctr <- 1
newoutm <- matrix(NA_integer_,nrow=ntrials,ncol=6)
allrows <- 1:nrow(mmat)
nint<- 30

while(!all(test == nint)) {
  
  for(i in (nint+1):nrow(newoutm)) {
    ml <- reshape2::melt(newoutm[1:i,])
    test <- with(ml, table(Var2, value))
    if (any(test > nint)) {
      ctr <- (i)
      travpos <- c(travpos[1:(i)], vector("list", ntrials - (i)))
      #print(paste("reset:",i))
      break
    }
    #if (any(table(travpos[[i]]) > 1)) browser()
  }
  # for (i in seq_len(ntrials)) {
  #   if (length(c(sapply( travpos[seq_len(i-1)], tail, n = 1  ), 
  #                 travpos[[i]] )) >= nrow(mmat)) {
  #     ctr <- (i-1)
  #     travpos <- c(travpos[1:(i-1)], vector("list", ntrials - (i -1)))
  #     #print(c(i, ", aha"))
  #     break
  #   }
  # }
  
  if (ctr == ntrials+1) {
    ctr <- ctr - 1
  }
  
  if (ctr == 1) {
    curit<- sample(allrows[ !(allrows %in% travpos[[ctr]])  ],1)
    travpos[[ctr]] <- c(travpos[[ctr]], curit)
    newoutm[ctr,] <- mmat[curit,]
    ctr <- ctr+1
  }
  while(ctr <= ntrials  ) {
    pick_from <- allrows[ !(allrows %in% c(sapply( travpos[seq_len(ctr - 1)], tail, n = 1  ), travpos[[ctr]] ))  ]
    if (length(pick_from) == 0) {
      ctr <- ctr - 1
      travpos <- c(travpos[1:ctr], vector("list", ntrials - ctr))
      next
    } else if (length(pick_from) == 1) {
      curit <- pick_from
    } else {
      curit <- sample(pick_from, 1)
    }
    travpos[[ctr]] <- c(travpos[[ctr]], curit)
    newoutm[ctr,] <- mmat[curit,]
    ctr <- ctr+1
  }
  ml <- reshape2::melt(newoutm)
  test <- with(ml, table(Var2, value))
  
}
print("done")

out <- rbind(mmat, newoutm)
ml <- reshape2::melt(out)
with(ml, table(Var2, value))

#write.csv(newoutm, "newoutm30.csv")
#write.csv(out, "allout150.csv")


# ## 6 trials per cell remain
# 
# ml <- reshape2::melt(mmat[seq(1, nrow(mmat), by = 2 ),  ])
# with(ml, table(Var2, value)) 
# 
# ml <- reshape2::melt(mmat[1:(nrow(mmat)/2),  ])
# with(ml, table(Var2, value)) 
# 
# ## 6 trials per cell remain
# 
# 
# ### select remaining 30 from all possible trials randomly without replacement 
# mmat2 <- mmat
# 
# ## first, select 6 trials at random:
# 
# new1 <- sample(seq_len(nrow(mmat2)), 6)
# mnew <- mmat2[new1,]
# mmat2 <- mmat2[-new1,]
# 
# mnl <- reshape2::melt(mnew)
# with(mnl, table(Var2, value)) 
# 
# #######
# 
# sample(1:5)
# 
# ############
# mat <- t(replicate(150, sample(1:5)))
# ml <- reshape2::melt(mat)
# test <- with(ml, table(Var2, value))
# 
# 
# mat <- t(replicate(30, sample(1:5)))
# ml <- reshape2::melt(mat)
# test <- with(ml, table(Var2, value))
# 
# 
# # ml2<- reshape2::melt(full_trialsample_4)
# # test2 <- with(ml2, table(Var2, value))
# # 
# # ml3<- reshape2::melt(xx)
# # test3 <- with(ml3, table(Var2, value))
# 
# while(!all(test == 30)) {
#   mat <- t(replicate(150, sample(1:5)))
#   ml <- reshape2::melt(mat)
#   test <- with(ml, table(Var2, value))
#   #print(test)
# }
# 
# 
# while(!all(test == 6)) {
#   mat <- t(replicate(30, sample(1:5)))
#   ml <- reshape2::melt(mat)
#   test <- with(ml, table(Var2, value))
#   #print(test)
# }
# 
# ######
# 
# mmat <- matrix(NA_integer_, nrow = 120, ncol = 5)
# i <- 1
# 
# el <- 1:5
# 
# for (l1 in el) {
#   for (l2 in el[-l1]) {
#     for (l3 in el[-c(l1, l2)]) {
#       for (l4 in el[-c(l1, l2, l3)]) {
#         for (l5 in el[-c(l1, l2, l3, l4)]) {
#           mmat[i, ] <- c(l1, l2, l3, l4, l5)
#           i <- i + 1
#         }
#       }
#     }
#   }
# }
# 
# 
# ml <- reshape2::melt(mmat)
# 
# with(ml, table(Var2, value))
