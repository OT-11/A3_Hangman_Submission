#Assignment 3: Hangman
# Overal function to run game straight through
source("A3 Helper Functions.R") #Loading helper functions to check and validate user input
play_hangman <- function(){
  hangman_list_path <- ("Hangman_Word_List_Cities.txt") #Word list to read words into game
  # In the context of reading a word list from a text file for a Hangman game, readLines() is a more suitable choice than read.delim(). This is because a word list is typically stored as a simple text file with one word per line, without any structured columns or separators. The readLines() function allows you to read each line (word) as a separate element in a vector, which can be easily manipulated and used in the Hangman game logic.
  # On the other hand, read.delim() is designed for more structured data with multiple columns and specific delimiters. If your word list file had additional information associated with each word in tabular format, then read.delim() could be more appropriate.
  city_list <- readLines(hangman_list_path)
  city_list <- toupper(city_list)
  word_selection <- sample(city_list, 1, replace = FALSE, prob = NULL) #Picking a secret word for the game
  # Informing the user on the length of the secret word. 
  word_length <- nchar(word_selection)
  word_length_check <- cat("The word is", word_length, "letters long.")
  # Creation of hangman figures
  hangman_figures <- c("
   +---+
       |
       |
       |
      ===",
                       "
   +---+
   O   |
       |
       |
      ===",
                       "
   +---+
   O   |
   |   |
       |
      ===",
                       "
   +---+
   O   |
  /|   |
       |
      ===",
                       "
   +---+
   O   |
  /|\\  |
       |
      ===",
                       "
   +---+
   O   |
  /|\\  |
  /    |
      ===",
                       "
   +---+
   O   |
  /|\\  |
  / \\  |
      ===")
  
  # Informing the user about the number of wrong guesses/tries allowed. 
  max_wrong_guesses <- 7 # This is so it matches up with the number of created hangman images
  wrong_guess_check <- cat("You have", max_wrong_guesses, "wrong guesses allowed.")
  # Updating of wrong guesses
  num_wrong_guesses <- 0
  # Generation of visual word count clue
  visual_clue <- rep("_", word_length)
  split_word <- strsplit(word_selection, split = "", fixed = TRUE)[[1]]
  
  # Game loops
  while (num_wrong_guesses < max_wrong_guesses) { # This loop will run as long as the user's number of wrong guesses is less than the max number allowed.
    user_input <- validate_input(word_selection) # calls the validate_input() function, which prompts the user for input and checks that the input is valid (either a single letter or a full word guess that matches the word to be guessed, word_selection). The validated input is then assigned to the user_input variable.
    result <- check_input2(user_input, split_word, word_selection) # The check_input2() function checks if the user's input matches a letter in the secret word or if it matches the entire secret word. The function will return one of three strings based on the check: "correct", "win", or "incorrect". The return value is then assigned to the result variable.
    
    if (result == "correct") { # If result is "correct", it means the user's letter guess is in the secret word. It then prints the message "Correct guess!\n". A for loop is used to iterate over each letter in the split_word (which is the secret word split into individual letters), and if a letter matches the user's guess, the corresponding position in visual_clue (which is the player's current guess for the word) is replaced with the guessed letter. The updated visual_clue is then printed.
      cat("Correct guess!\n")
      for(i in 1:length(split_word)) {
        if (split_word[i] == toupper(user_input)) {
          visual_clue[i] <- toupper(user_input)
        }
      }
      cat(visual_clue, "\n")
    }
    else if (result == "win") { # else if (result == "win") {: This condition checks if the result is "win", which means the player's guess (as a full word) matches the secret word. If it's true, it means that the player has won the game
      cat("You guessed the right word! You win!\n") # This line prints out a message to the player, letting them know they have won the game by guessing the right word.
      break #breaking while loop because player one
    } else { # If result was neither "correct" nor "win", it must have been "incorrect". This means the user's guess was wrong. This else block executes if the player's guess is incorrect.
      cat("Incorrect guess. You have", max_wrong_guesses - num_wrong_guesses, "guesses remaining.\n\n") # prints out a message telling the player that their guess was incorrect and how many guesses they have remaining, which is the difference between the maximum number of allowed wrong guesses and the number of wrong guesses the player has already made.
      # Print the current hangman figure based on the number of wrong guesses
      cat(hangman_figures[num_wrong_guesses + 1], "\n") # This line prints the hangman figure corresponding to the current number of wrong guesses. As the array of figures is 0-indexed and num_wrong_guesses starts from 0, num_wrong_guesses + 1 is used to get the right figure.
      num_wrong_guesses <- num_wrong_guesses + 1 # This line increases the number of wrong guesses by 1.
    }
  }
  
  
 


  
  if (num_wrong_guesses >= max_wrong_guesses) { # checks if the number of wrong guesses made by the user is greater than or equal to the maximum number of wrong guesses allowed in the game. If this condition is true, then it means the player has exhausted all their chances without correctly guessing the word, so the game is over.
    cat("Game over! You've reached the maximum number of wrong guesses.") #If the above condition is true, this line prints a message to the player to let them know that the game is over and that they've reached the maximum number of wrong guesses.
    cat("The secret word was '", word_selection, "'.") #This line reveals the secret word to the user. It concatenates the phrase "The secret word was '", the secret word itself (word_selection), and another "'" to create a complete sentence and then prints it out.
  }
}

play_hangman() #This function will run all of the code for the game in one function

