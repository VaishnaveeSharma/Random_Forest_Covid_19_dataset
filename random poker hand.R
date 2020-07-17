## Building a deck set
deck=matrix(c(rep( c(2:10,"Jack","Queen","King","Ace"),4),rep(c("Club","Diamond","Heart","Spade"),rep(13,4))), ncol=2,dimnames=list(NULL,c("rank","suit")))

## Dealing card to hand (select random 5 cards out of 52)
deal_hand = function()
{
  return( deck[sample(1:52,5,replace=F),])
}

## Figure out what kind of card combination a hand has
what_hand = function(hand)
{    
  ranks_acehigh = c(2:10,"Jack","Queen","King","Ace")
  ranks_acelow = c("Ace",2:10,"Jack","Queen","King")
  
  rank_i_ah = sort( sapply(hand[,"rank"],function(x) which(x == ranks_acehigh)) ) # Check if Ace is count as highest value
  rank_i_al = sort( sapply(hand[,"rank"],function(x) which(x == ranks_acelow)) ) # Check if Ace is count as lowest value
  
  is_straight = all( rank_i_ah-min(rank_i_ah)+1 == 1:5 ) | all( rank_i_al-min(rank_i_al)+1 == 1:5 )
  is_flush = length(unique(hand[,"suit"])) == 1
  
  if (is_straight && is_flush) {
    if (all(c("King","Ace") %in% hand[,"rank"])) return( "Royal flush" )
    else return( "Straight flush" )
  }
  
  if (is_straight) return( "Straight" )
  if (is_flush) return( "Flush" )
  
  tab = sort( table(hand[,"rank"]) )
  if (length(tab)==2) {
    if (all(tab == c(1,4))) return( "Four of a kind" )
    if (all(tab == c(2,3))) return( "Full house")
  }
  if (length(tab)==3) {
    if (all(tab == c(1,1,3))) return( "Three of a kind")
    if (all(tab == c(1,2,2))) return( "Two pair" )
  }
  if (length(tab)==4) {
    return( "Pair" )
  }
  
  return( "No pair" )
}
## Simulating number of deals
simulate = function(N=10000) 
{
  hands = c("Royal flush", "Straight flush", "Four of a kind",
            "Full house", "Flush", "Straight",
            "Three of a kind", "Two pair", "Pair", "No pair")
  
  res = matrix(rep(0,length(hands)),nrow=length(hands),ncol=2)
  rownames(res) = hands
  colnames(res) = c("Counts","Estimated Probability")
  
  pb = txtProgressBar(min = 0, max = N, style = 3)
  for(i in 1:N) {
    hand = what_hand(deal_hand())
    res[hand,1] = res[hand,1]+1
    setTxtProgressBar(pb, i)
  }
  res[,2]<-res[,1]/N
  
  return(res)
}
## Testing Section
# Running in Serial
system.time(result1<-lapply(c(2598960),simulate))
# Running in parallel
system.time({
library(parallel)
nc <- detectCores()
cl <- makeCluster(rep("localhost", nc))
clusterExport(cl, list("deal_hand","what_hand","deck"))

result2<-parLapply(cl,c(2598960),simulate)

stopCluster(cl)
})
## Display result
result1
result2
