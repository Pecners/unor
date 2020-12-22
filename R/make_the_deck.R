library(tidyverse)

# Make the Deck

colors <- c("Red", "Green", "Yellow", "Blue")

num_cards <- map_df(colors, function(x) {
  
  nums <- c(0, rep(1:9, 2))
  specials <- rep(c("Draw Two", "Skip", "Reverse"), 2)
  wilds <- c("Wild", "Draw Four")
  
  t <- tibble(color = x, number = as.character(nums))
  d <- tibble(color = x, number = specials)
  w <- tibble(color = "Wild", number = wilds)
  
  bind_rows(t, d, w)

})

# Deal Cards

shuffled_deck <- num_cards[sample(1:nrow(num_cards), replace = FALSE),]
stock <- shuffled_deck

deal <- function(players = 4) {
  
  stock <<- shuffled_deck[(players * 7 + 1):nrow(shuffled_deck),]
  
  
  map(1:players, function(x) {
    
    shuffled_deck[seq(from = x, by = players, length.out = 7), ]
    
  })
  
}

hands <- deal()

# Play a hand

play_hand <- function() {
  
}
