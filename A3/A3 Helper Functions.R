# Helper functions for Hangman_A3
# Repeat
validate_input <- function(word_selection) {
  repeat { #This loop will continuously ask for user input until a valid input is provided.
    user_input <- toupper(readline("Guess one letter or the whole word: ")) #prompts the user to enter their guess and converts it to uppercase with toupper(). The input is then stored in the variable user_input.
    if ((nchar(user_input) == 1) && is.character(user_input)) { #checks if the user's input is a single character. nchar(user_input) == 1 checks that the input has a length of 1 (i.e., is a single character), and that the input is indeed a character type. If both conditions are met, the function returns the user's input.
      return(user_input)
    } else if (nchar(user_input) == nchar(word_selection) && toupper(user_input) == toupper(word_selection)) { #checks if the user's input is a guess for the whole word. It checks if the length of the user's input matches the length of the secret word, and then checks if the user's input matches the secret word itself (both comparisons are case-insensitive due to the use of toupper()). If both conditions are met, the function returns the user's input.
      return(user_input)
    } else {
      cat("Invalid guess. Please enter only one character or the whole word.") #executed if the user's input does not pass the previous two checks (it is not a single character and is not a correct guess for the entire word)
    }
  }
}

# Check to see if the user input is in the secret word or is the secret word
check_input <- function(user_input, word_selection) {
  if (user_input %in% strsplit(word_selection, "")[[1]]) {
    return("correct")
  } else if (user_input == word_selection) {
    return("win")
  } else {
    return("incorrect")
  }
}




# Check to see if the user input is in the secret word or is the secret word
check_input2 <- function(user_input, split_word, word_selection) { 
  if (nchar(user_input) == 1) { #This if statement checks if the user's input is a single character.
   if (user_input %in% split_word) {#  checks whether the user's single character guess is in the secret word (split_word). If the character is in the secret word, the function returns "correct".
     return("correct")
   } else {
     return("incorrect")
   }
  }
  else if (user_input == word_selection) { #executed if the user's input is not a single character (i.e., it's a word). It checks whether the user's word guess matches the secret word exactly. If it does, the function returns "win".
    return("win")
  }
  else 
    return("incorrect")
}
















