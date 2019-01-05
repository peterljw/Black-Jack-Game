#Import libraries
import numpy as np

#Create a full deck with 52 cards (no jokers)
full_deck = []
for i in range(1,10):
    add = [i] * 4
    full_deck = full_deck + add
full_deck = full_deck + [10]*16
playing_deck = np.array(full_deck)

#A function that randomly picks 2 cards from a given deck and deletes the two cards from the playing deck, returns the two cards as a list
def draw_two(deck):
    cards_dealt = np.random.choice(deck, 2, replace=False)
    return(cards_dealt)

#A function that removes cards from a given deck and returns the new deck
def remove_cards(deck, cards):
    index = []
    for card in cards:
        itemindex = np.where(deck == card)
        index.append(itemindex[0][0])
    new_deck = np.delete(deck, index)
    return(new_deck)

#A function that compiles all possible sums from a given hand cards
def all_sums(cards):
    if np.any(cards == 1):
        possible_sums = [np.sum(cards)]
        ace_number = np.sum(cards==1)
        for i in range(1,ace_number+1):
            possible_sums.append(np.sum(cards)+i*10)
        return(np.array(possible_sums))
    else:
        cards_sum = np.sum(cards)
        return(cards_sum)

#A function that checks "bust", returning T if busted, F otherwise
def bust_check(cards):
    bool_list = all_sums(cards) > 21
    return(not(np.any(bool_list == False)))

#A function that checks "blackjack", returning T if yes, F otherwise
def bj_check(cards):
    bool_list = all_sums(cards) == 21
    return(np.any(bool_list == True))

#A function that determines if the dealer should hit(T) or stand(F) based on the player's score
def dealer_move(dealer_cards, player_score):
    dealer_sums = all_sums(dealer_cards)
    if np.any(dealer_sums > player_score):
        return(False)
    elif np.any(dealer_sums == player_score) and player_score >= 15:
        return(False)
    else:
        return(True)

#A function that plays a single game of blackjack
def play_blackjack(playing_deck):

    print("----------Welcome to Weigas, it is game time!----------")

    # Stage 1: Deal cards and check blackjacks, then show cards if nothing happens
    dealer_cards = draw_two(playing_deck)
    playing_deck = remove_cards(playing_deck, dealer_cards)
    player_cards = draw_two(playing_deck)
    playing_deck = remove_cards(playing_deck, player_cards)
    if bj_check(dealer_cards)==True and bj_check(player_cards)==True:
        print("Wow! It's a push with two blackjacks!")
        return (None)
    elif bj_check(dealer_cards)==True:
        print("Oops! The dealer has won with a blackjack!")
        return (False)
    elif bj_check(player_cards)==True:
        print("Winner! Winner! Chicken Dinner! You have won with a blackjack!")
        return (True)
    print("Your cards: ", player_cards)
    print("Dealer's shown card: ", dealer_cards[0])

    # Stage 2: Player's move and calculate player's final score
    print("----------Your Move!----------")
    player_move = input("Enter H to hit or S to stand!")
    while player_move == "H":
        card = np.random.choice(playing_deck, 1)
        playing_deck = remove_cards(playing_deck, card)
        player_cards = np.array(list(player_cards) + list(card))
        print("Card dealt: ", card)
        print("Your cards: ", player_cards)
        if bj_check(player_cards)==True:
            print("Winner! Winner! Chicken Dinner! You have won with a blackjack!")
            return (True)
        if bust_check(player_cards)==True:
            print("Ahh...Busted!")
            return (False)
        player_move = input("Enter H to hit or S to stand!")
    player_outcomes = all_sums(player_cards)
    player_score = np.amax(player_outcomes[player_outcomes<=21])
    print("Your final score: ", player_score)

    # Stage 3: Dealer's move and print the outcome of the game
    print("----------Dealer's Move!----------")
    move = dealer_move(dealer_cards, player_score)
    while move==True:
        card = np.random.choice(playing_deck, 1)
        playing_deck = remove_cards(playing_deck, card)
        dealer_cards = np.array(list(dealer_cards) + list(card))
        print("Card dealt: ", card)
        print("Dealer's cards: ", dealer_cards)
        if bj_check(dealer_cards)==True:
            print("Oops! The dealer has won with a blackjack!")
            return(False)
        if bust_check(dealer_cards)==True:
            print("You have won! The dealer has busted!")
            return(True)
        move = dealer_move(dealer_cards, player_score)
    dealer_outcomes = all_sums(dealer_cards)
    dealer_score = np.amax(dealer_outcomes[dealer_outcomes<=21])
    print("Your final score: ", player_score)
    print("Dealer's final score: ", dealer_score)
    if dealer_score == player_score:
        print("Push! Try again!")
        return(None)
    elif dealer_score > player_score:
        print("Oops, you have lost :(")
        return(False)
    else:
        print("You have won!")
        return(True)

#Game function
def play_alotof_blackjack():
    play = "P"
    match_history = []
    while play == "P":
        playing_deck = np.array(full_deck)
        outcome = play_blackjack(playing_deck)
        match_history.append(outcome)
        if outcome == None:
            play = input("Ties are unaccpeted! Now enter P to try again! (or Press Q to quit but you know you won't!")
        elif outcome == True:
            play = input("Nicely done! Now enter P to play again! (or Press Q to quit but you know you won't :)")
        else:
            play = input("It's OK! Now enter P to win it back! (or Press Q to quit but you know you won't!")
    win_rate = sum(np.array(match_history)==True)/len(match_history)
    print("Thank you for playing! Your win rate is: ", win_rate)
    return(match_history)

record = play_alotof_blackjack()
print(record)

