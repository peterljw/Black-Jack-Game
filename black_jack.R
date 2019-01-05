#Create a full deck with 52 cards (no jokers)
full_deck <- c()
for(i in 1:9){
  full_deck <- c(full_deck, rep(i,4))
}
full_deck <- c(full_deck, rep(10,16))
playing_deck <- full_deck

#A function that randomly picks 2 cards from a given deck and deletes the two cards from the playing deck, returns the two cards as a list
random_two <- function(playing_deck){
  cards_dealt <- sample(playing_deck, 2, replace=F)
  index_a <- match(cards_dealt[1],playing_deck)[1]
  index_b <- match(cards_dealt[2],playing_deck)[1]
  playing_deck <<- playing_deck[-c(index_a, index_b)]
  return(cards_dealt)
}

#A function that compliles all possible sums from a given deck
all_sums <- function(cards){
  if(1 %in% cards){ #If there are aces in the cards
    possible_sums <- c(sum(cards)) #regular sum
    cards_table <- table(cards)
    ace_number <- cards_table[names(cards_table)==1]
    for(i in 1:ace_number){
      possible_sums <- c(possible_sums, sum(cards)+(i*10))
    }
    return(possible_sums)
  }else{
    cards_sum <- sum(cards)
    return(cards_sum)
  }
}

#A function that checks "bust", returning T if busted, F otherwise
bust_check <- function(cards){
  bool_list <- all_sums(cards) > 21
  return(all(bool_list))
}

#A function that checks "blackjack", returning T if yes, F otherwise
bj_check <- function(cards){
  bool_list <- all_sums(cards) == 21
  if(T %in% bool_list){
    return(T)
  }else{return(F)}
}

#A function that determines if the dealer should hit(T) or stand(F) based on the player's score
dealer_move <- function(dealer_cards, player_score){
  dealer_sums <- all_sums(dealer_cards)
  if(T %in% (dealer_sums > player_score)){
    return(F)
  }else if(T %in% (dealer_sums == player_score & player_score >=15)){
    return(F)
  }else{
    return(T)
  }
}

#A function that plays a single game of blackjack
play_blackjack <- function(playing_deck){
  
  print("----------Welcome to Weigas, it is game time!----------")
  
  #Stage 1: Deal cards and check blackjacks, then show cards if nothing happens
  dealer_cards <- random_two(playing_deck)
  player_cards <- random_two(playing_deck)
  if(bj_check(dealer_cards)==T & bj_check(player_cards)==T){
    print("Wow! It's a push with two blackjacks!")
    return(NA)
  }else if(bj_check(dealer_cards)==T){
    print("Oops! The dealer has won with a blackjack!")
    return(F)
  }else if(bj_check(player_cards)==T){
    print("Winner! Winner! Chicken Dinner! You have won with a blackjack!")
    return(T)
  }
  cat("Your cards: ", player_cards)
  cat("\nDealer's shown card: ", dealer_cards[1])
  
  #Stage 2: Player's move and calculate player's final score
  cat("\n----------Your Move!----------")
  player_move <- readline(prompt="Enter H to hit or S to stand!")
  while(player_move == "H"){
    #Deal a card
    card <- sample(playing_deck, 1)
    index <- match(card, playing_deck)
    playing_deck <<- playing_deck[-index]
    player_cards <- c(player_cards, card)
    cat("Card dealt: ", card)
    cat("\nYour cards: ", player_cards)
    #Check blackjack or bust
    if(bj_check(player_cards)==T){
      print("Winner! Winner! Chicken Dinner! You have won with a blackjack!")
      return(T)
    }
    if(bust_check(player_cards)==T){
      print("Ahh...Busted!")
      return(F)
    }
    player_move <- readline(prompt="Enter H to hit or S to stand!")
  }
  player_outcomes <- all_sums(player_cards)
  player_score <- max(player_outcomes[player_outcomes<=21])
  cat("Your final score: ", player_score)
  
  #Stage 3: Dealer's move and print the outcome of the game
  cat("\n----------Dealer's Move!----------")
  move <- dealer_move(dealer_cards = dealer_cards, player_score = player_score)
  while(move == T){
    card <- sample(playing_deck, 1)
    index <- match(card, playing_deck)
    playing_deck <- playing_deck[-index]
    dealer_cards <- c(dealer_cards, card)
    cat("\nCard dealt: ", card)
    cat("\nDealer's cards: ", dealer_cards)
    #Check blackjack or bust
    if(bj_check(dealer_cards)==T){
      cat("\nOops! The dealer has won with a blackjack!")
      return(F)
    }
    if(bust_check(dealer_cards)==T){
      cat("\nYou have won! The dealer has busted!")
      return(T)
    }
    move <- dealer_move(dealer_cards = dealer_cards, player_score = player_score)
  }
  dealer_outcomes <- all_sums(dealer_cards)
  dealer_score <- max(dealer_outcomes[dealer_outcomes<=21])
  cat("\nYour final score: ", player_score)
  cat("\nDealer's final score: ", dealer_score)
  if(dealer_score == player_score){
    cat("\nPush! Try again!")
    return(NA)
  }else if(dealer_score > player_score){
    cat("\nOops, you have lost :(")
    return(F)
  }else{
    cat("\nYou have won!")
    return(T)
  }
}

play_alotof_blackjack <- function(){
  play <- "P"
  match_history <- c()
  while(play == "P"){
    playing_deck <- full_deck
    outcome <- play_blackjack(playing_deck = playing_deck)
    match_history <- c(match_history, outcome)
    if(is.na(outcome)){
      play <- readline(prompt = "Ties are unaccpeted! Now enter P to try again! (or Press Q to quit but you know you won't!")
    }else if(outcome == T){
      play <- readline(prompt = "Nicely done! Now enter P to play again! (or Press Q to quit but you know you won't :)")
    }else{
      play <- readline(prompt = "It's OK! Now enter P to win it back! (or Press Q to quit but you know you won't!")
    }
  }
  win_rate <- sum(match_history, na.rm = T)/length(match_history)
  cat("\nThank you for playing! Your win rate is: ", win_rate)
  return(match_history)
}

#--------------------------------------------------------------------------------

#Game time babe!
record <- play_alotof_blackjack()
#Match Summary
table(record)