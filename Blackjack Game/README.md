This is a blackjack game I coded in R.

-To play, call play(k,t) where k is number of players and t is number of decks    
-The play method first deals cards initially to the dealer (prints out one card the dealer is “showing”)   
-Then it does an initial dealing of 2 cards for next players; asks whether to hit or stand. Continues until score is >= 21 OR player types stand.   
-Dealer is on automatic decision rule, where will continue to hit as long as score is less than 17   
-If runs out of cards before game is over, one deck is added   
-After all players play, prints out score for each player   
-If dealer gets 21, dealer wins.  
-If multiple players (but not dealer) get 21, prints out the winners that it is a tie between.   
-If every player scores > 21, dealer wins by default   
-If nobody gets 21 but < 21, the player closest to 21 wins. (If multiple same scores, prints out the winners that tied)   
   
The code is posted within this folder, and you can also find a HTML Doc showing a sample play.   
