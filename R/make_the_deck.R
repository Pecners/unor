library(tidyverse)


play_uno <- function() {
  
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

  deal <- function(players = 4) {
    
    stock <- shuffled_deck[-(1:(players * 7)),]
    discard <- stock[1,]
    stock <- stock[-1,]
    
    hands <- map(1:players, function(x) {
      
      shuffled_deck[seq(from = x, by = players, length.out = 7), ]
      
    })
    
    list(hands = hands, 
         stock = stock, 
         discard = discard,
         no_winner = TRUE,
         o = 1,
         card_is_active = FALSE,
         c = 1,
         wild_color = NULL,
         p = 1)
    
  }
  
  all_cards <- deal()
  
  
  draw <- function(game, number_to_draw = 1) {
    
    if (nrow(game$stock) < number_to_draw) {
      
      # Print numbers before reshuffle
      
      print(paste("Stock:", nrow(game$stock)))
      print(paste("Discard:", nrow(game$discard)))
      
      # Put stock cards into hand
      
      game$hands[[all_cards$p]] <- bind_rows(game$hands[[all_cards$p]], game$stock)
      
      still_need <- number_to_draw - nrow(game$stock)
      
      # Reshuffle
      
      print("Reshuffle!")
    
      game$stock <- game$discard[sample(1:(nrow(game$discard) - 1), replace = FALSE),]
      game$discard <- game$discard[nrow(game$discard),]
      
      # Draw rest of cards
      
      game$hands[[all_cards$p]] <- bind_rows(game$hands[[all_cards$p]], game$stock[(1:still_need),])
      game$stock <- game$stock[-(1:still_need),]
      
      # Print numbers after
      
      print(paste("Stock:", nrow(game$stock)))
      print(paste("Discard:", nrow(game$discard)))
      
    } else {
      game$hands[[all_cards$p]] <- bind_rows(game$hands[[all_cards$p]], game$stock[(1:number_to_draw),])
      game$stock <- game$stock[-(1:number_to_draw),]
    }
    
    return(game)
  }
  
  next_player <- function(player, order = all_cards$o) {
    
    if (order == 1) {
      
      if (player == 4) {
        
        all_cards$p <- 1
        
      } else {
        
        all_cards$p <- player + 1
        
      }
    } else {
      
      if (player == 1) {
        
        all_cards$p <- 4
        
      } else {
        
        all_cards$p <- player - 1
        
      }
    }
    
    return(all_cards$p)
  }
  

  
  
  play_hand <- function() {
    
    # Check for Wild, if yes choose color
    
    if (all_cards$discard[[1]][nrow(all_cards$discard)] == "Wild") {
      
      color_in_play <- all_cards$wild_color
      
    } else {
      
      color_in_play <- all_cards$discard[[1]][nrow(all_cards$discard)]
      
    }
    
    number_in_play <- as.character(all_cards$discard[[2]][nrow(all_cards$discard)])
    
    # Can't play a card
    
    no_turn <- c("Draw Two",
                 "Draw Four",
                 "Skip",
                 "Reverse")
    
    if (number_in_play %in% no_turn & all_cards$card_is_active) {
      
      if (number_in_play == "Draw Two") {
        
        all_cards <- draw(game = all_cards, number_to_draw = 2)
        print(paste("Player", all_cards$p, "drew two cards!"))
        all_cards$p <- next_player(player = all_cards$p, order = all_cards$o)
        all_cards$c <- all_cards$c + 1
        
      } else if (number_in_play == "Draw Four") {
        
        all_cards <- draw(game = all_cards, number_to_draw = 4)
        print(paste("Player", all_cards$p, "drew four cards!"))
        all_cards$p <- next_player(player = all_cards$p, order = all_cards$o)
        all_cards$c <- all_cards$c + 1
        
      } else if (number_in_play == "Skip") {
        
        print("You were skipped!")
        all_cards$p <- next_player(player = all_cards$p, order = all_cards$o)
        all_cards$c <- all_cards$c + 1
        
      } else if (number_in_play == "Reverse") {
        
        print("Reverse!")
        all_cards$p <- next_player(player = all_cards$p, order = all_cards$o)
        all_cards$c <- all_cards$c + 1
        
      }
      
      all_cards$card_is_active <- FALSE
      
      return(all_cards)
      
    } else {
      
      # Play hand
      
      t <- all_cards$hands[[all_cards$p]]
      
      playable <- t$color %in% c(color_in_play, "Wild") | as.character(t$number) == number_in_play
      
      if (sum(playable) > 0) {
        
        # Play a card
        
        s <- sample(which(playable), size = 1)
        play_card <- t[s,]
        
        if (play_card$color == "Wild") {
          
          all_cards$wild_color <- sample(colors, 1)
          print(all_cards$wild_color)
          
        }
        
        
        all_cards$discard <- bind_rows(all_cards$discard, play_card)
        all_cards$hands[[all_cards$p]] <- all_cards$hands[[all_cards$p]][-s, ]
        all_cards$card_is_active <- TRUE
        print(paste(play_card$color, play_card$number, "was played!"))
        
        if (nrow(all_cards$hands[[all_cards$p]]) == 0) {
          
          all_cards$no_winner <- FALSE
          print(paste("Player", all_cards$p, "wins!"))
          
          return(all_cards)
          
        } else if (play_card$number == "Reverse") {
          
          all_cards$o <- all_cards$o * -1
          
          return(all_cards)
          
        } else {
          
          all_cards$p <- next_player(player = all_cards$p, order = all_cards$o)
          all_cards$c <- all_cards$c + 1
          
          return(all_cards)
        }
        
      } else {
        
        # Can't play, draw
        
        all_cards <- draw(game = all_cards)
        paste("Player", all_cards$p, "drew a card!")
        all_cards$c <- all_cards$c + 1
        
        return(all_cards)
      }
    }
  }
  
  while (all_cards$no_winner) {
    
    all_cards <- play_hand()
    
    # Monitor cards in play for bugs
    
    cards_in_hands <- sum(map_int(1:length(all_cards$hands), function(x) {
      nrow(all_cards$hands[[x]])
    })
    )
    
    total_cards <- sum(cards_in_hands, nrow(all_cards$stock), nrow(all_cards$discard))
    
    if (total_cards != 108) {
      
      for (i in 1:length(all_cards$hands)) {
        print(paste("Hand", i, "has", nrow(all_cards$hands[[i]]), "cards." ))
      }
      
      stop(paste("Bad card math!", total_cards))
      
    }
    
  } 
  

  map_df(1:length(all_cards$hands), function(x) {
    all_cards$hands[[x]] %>%
      mutate(hand = x)
  })
  
}

h <- map_df(1:10000, function(x) {
  play_uno() %>%
    mutate(game = x)
})


