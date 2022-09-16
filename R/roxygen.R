#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#'   The player select his first door
#' 
#' @description
#'   We choose a random door as a first selection
#'     
#' @details
#'   We use sample function to return a picked door 
#'   
#' @param 
#'   No parameters used
#'   
#' @return 
#'   Return the first picked door 
#'   
#' @examples
#'   select_door()
#'   
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'   The host open the first door
#'   
#' @description
#'   The host open a door different from the picked door & the car door
#'   
#' @details
#'   If the picked door is "car", the host can open any of the other two. 
#'   If the picked door is "goat" the host has only one option to open
#'   
#' @param 
#'   The random set of the 3 doors created at crest_game() & 
#'   The first selection by player
#'   
#' @return 
#'   The "goat" door number that the host will open
#'   
#' @examples
#'   open_game_door(game, a.pick) --> 2
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#'   two conditions for the player
#'   
#' @description
#'   The player will get to decide if they will switch door or stay
#'   
#' @details
#'   Depending on the player choice, the function will return either 
#'   their first door selction or their new pick
#'   
#' @param 
#'   The first door picked by played, the opened door by host, and a boolean option
#'   
#' @return 
#'   The final picked door by player
#'   
#' @examples
#'   change_door(stay=F, opened.door, a.pick)
#'   
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title
#'   Determine if player win or lose
#'   
#' @description
#'   depends on the player final selection
#'   
#' @details
#'   If the final selection is the car --> "win"
#'   if final selection goat --> "lose"
#'   
#' @param 
#'   the first set created by the first function creat_game()
#'   
#' @return
#'    return win or lose
#'    
#' @examples
#'   determine_winner(1, (car, goat, goat)) --> win
#'   determine_winner(2, (car, goat, goat)) --> lose
#'   
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#'   play the game from the start 
#'   
#' @description
#'   call all the previous functions
#'   
#' @details
#'   creating the game, selecting the first door, open the first door, 
#'   choosing to stay or to switch door, finding if win or lose
#'   
#' @param 
#'   no parameters
#'   
#' @return 
#'   The strategy & the result 
#'   
#' @examples
#'   play_game()
#'   
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#'   play multiple times
#'   
#' @description
#'   to compare the results of stating option & switching the door option
#'   
#' @details
#'   The game played n times, and return the results of each strategy 
#'   
#' @param 
#'   The number og games played
#'   
#' @return 
#'   return a table with the numbers of winning & losing for each strategy 
#'   
#' @examples
#'   play_n_games(n=1000)
#'   
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}