play <- function(k, t) {
  buildDeck(t)
  # list of scores - a player's score is found by scores[player+1] (dealer is index 1)
  scores <<- rep(list(0), k+1)
  
  initialDraw(0)
  for(i in seq(from=1, to=k, by=1)){
    initialDraw(i)
    repeat{
      if (scores[i+1] >= 21) {
        break
      }
      nextMove <- readline(prompt=paste0("Player ", i, ": Do you want to hit or stand? "))
      if(identical("stand", nextMove)) {
        print("You chose stand")
        break
      }
      if(identical("hit", nextMove)) {
        print("You chose hit")
        scores[i+1] <<- drawCard(i)
      }
      
    }
  }
  
  print("Dealer's turn ")
  while (as.numeric(scores[1]) < 17){
    scores[1] <<- drawCard(0)
  }
  
  print(paste0("Dealer had a ", cardD1, " and a ", cardD2, " after the initial draw. Dealer's final score is: ", scores[1])) 
  
  # print out all scores once all players are done
  for(i in seq(from=1, to=k, by=1)){
    print(paste0("Player ", i, " final score: ", scores[i+1])) 
  }
  
  if (scores[1] == 21) {
    return("Dealer wins")
  }
  else {
    ## Keeping a vector of scores == 21
    equals21 <<- c()
    for(i in seq(from=1, to=k, by=1)){
      if (scores[i+1] == 21) {
        equals21 <<- c(equals21, i)
      }
    }
    ## One player has a score of 21
    if (length(equals21) == 1) {
      return(paste0("Player ", equals21[1], " wins"))
    }
    ## More than one player has a score of 21
    else if (length(equals21) > 1) {
      winners <- "It's a tie between"
      for(i in seq(from=1, to=length(equals21)-1, by=1)){
        winners <- paste0(winners, " player ", equals21[i], ", ")
      }
      winners <- paste0(winners, " and player ", equals21[length(equals21)])
      return(winners)
    }
    # No one got 21
    else {
      currentWinner <<- list(22, -1) #[1]: difference from 21 [2-onward]: player(s) with that difference 
      for(i in seq(from=1, to=k+1, by=1)){
        if (scores[i] < 21) {
          difference <- 21-as.numeric(scores[i])
          # has lower difference (closer to 21)
          if (difference < currentWinner[1]) {
            # reset list
            currentWinner <<- list(difference, i-1)
          }
          # has same score
          else if (difference == currentWinner[1]){
            # add another player
            currentWinner <<- c(currentWinner, i-1)
          }
        }
      } 
    }
  }
  
  # means winners list did not get result from the default
  if (currentWinner[2] == -1) {
    return("All players and dealer got over 21. No one wins")
  }
  winnersList <<- "It's a tie between"
  if (length(currentWinner) == 2){
    if (currentWinner[2] == 0) {
      return("Dealer wins")
    }
    return(paste0("Player ", as.numeric(currentWinner[2]), " wins"))
  }
  else{
    for(i in seq(from=2, to=length(currentWinner)-1, by=1)){
      if (as.numeric(currentWinner[i]) == 0) {
        winnersList <<- paste0(winnersList, " dealer, ")
      }
      else {
        winnersList <<- paste0(winnersList, " player ", as.numeric(currentWinner[i]), ",")  
      }
    }
    return(paste0(winnersList, " and player ", as.numeric(currentWinner[length(currentWinner)])))
  }
}

# Draws a card for a player and prints out the card, and new score
drawCard <- function(player) {
  card <- getCard()
  scores[player+1] <<- newScore(player, card)
  if (player != 0) {
    print(paste0("Player ", player, ", you drew a ", card, ". Your score is now: ", scores[player+1])) 
  }
  else{
    print(paste0("Dealer drew a ", card, ". Dealer score is now: ", scores[player+1]))
  }
  return(scores[player+1])
}

## Initially draws 2 cards for a given player.
initialDraw <- function(player){
  
  if (player == 0) {
    cardD1 <<- getCard()
    scores[1] <<- newScore(0, cardD1)
    cardD2 <<- getCard()
    scores[1] <<- newScore(0, cardD2)
    print(paste0("Dealer is showing ", cardD1))
  }
  else {
    card1 <- getCard()
    scores[player+1] <<- newScore(player, card1)
    card2 <- getCard()
    scores[player+1] <<- newScore(player, card2)
    print(paste0("[INITIAL DEALING]: Player ", player, ", you drew a ", card1, " and ", card2, ". Your score is: ", scores[player+1]))
  }
}

buildDeck <- function(t){
  deck <<- c()
  oneDeck <- list("A", "A", "A", "A",
                  2, 2, 2, 2,
                  3, 3, 3, 3,
                  4, 4, 4, 4,
                  5, 5, 5, 5,
                  6, 6, 6, 6,
                  7, 7, 7, 7,
                  8, 8, 8, 8,
                  9, 9, 9, 9,
                  10, 10, 10, 10,
                  "J", "J", "J", "J",
                  "Q", "Q", "Q", "Q",
                  "K", "K", "K", "K")
  for (i in 1:t){
    deck <<- c(deck, oneDeck)
  }
  return(deck)
}

# gets a random card from the deck
getCard <- function(){
  # check if there are cards in the deck
  if (length(deck) == 0) {
    # if not, add another deck (equivalent to reshuffling)
    print("There are no cards left. Reshuffling the deck")
    buildDeck(1) 
  }
  # randomly gets a card from the deck
  random <- sample(1:length(deck), 1)
  # save the card to return, so you can remove it
  card <- deck[random]
  # removes the card from the deck
  deck <<- deck[-random]
  return(card)
}

# returns the new score given a player and the card 
newScore <- function(player, add){
  currPlayerScore <- as.numeric(scores[player+1])
  if (add == "A") {
    # an ace is = 1 if the score will go over 21
    if ((currPlayerScore + 11) > 21) {
      currPlayerScore <- currPlayerScore + 1
    }
    else {
      currPlayerScore <- currPlayerScore + 11 
    }
  }
  else if (add == "J" || add == "K" || add == "Q") {
    currPlayerScore <- currPlayerScore + 10
  }
  else {
    currPlayerScore <- currPlayerScore + as.numeric(add)
  }
  return(currPlayerScore)
}