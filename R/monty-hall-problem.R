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
#'    Select one of the three doors.
#'    
#' @description
#'    `select_door()` generates a randome selection of an intial
#'    door number of one through three. 
#'     
#' @details
#'     As part of the original game, "Let's Make a Deal" contestants
#'     pick an initial door that they think/hope the car is behind. 
#'     Contestants pick one door number, 1 through 3. 
#'     
#' @param ... no arguments are used by the function.
#' 
#' @return 
#'    The function returns a numeric vector 1-3.
#'    
#' @examples
#'     select_door()
#' 
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'     Open one of the Goat Doors.
#'     
#' @description
#'      `open_goat_door()` will return a numeric vector of 
#'      one of the door numbers, the door that is opened. 
#' 
#' @details
#'      In the original "Let's Make a Deal" game, after
#'      the contestant selects one of the three doors as 
#'      their intial pick, one of the remaining doors is
#'      opened to reveal one of the two goats. In this
#'      game, the if the contestant selected door one, 
#'      either door 2 or 3 would be opened to reveal one 
#'      of the goats behind one of the doors. This will 
#'      bring the contestant to the point of having to 
#'      decide between staying or switching doors. This 
#'      function  will randomly select one of 
#'      two doors that the contestant did not select. The 
#'      randomization is set for both scenarios in which 
#'      the contestant's intial pick was the car or one of
#'      the goats. 
#'      
#' @param ... no arguments are used by the function.
#' 
#' @return 
#'     The function returns a numeric vector 1-3.
#'     
#' @examples
#'      open_goat_door()
#'      
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
#'     Stay with Initial Door or Switch?
#' 
#' @description
#'     `change_door()` return a numeric vector in regards to 
#'     one of the two remaining doors.   
#' 
#' @details
#'     In the original "Let's Make a Deal" game, after one of
#'     the goat doors was revealed, contestants have the option
#'     to stay with the door that was initially picked, or to 
#'     switch to the other remaining door. As mentioned above, 
#'     this is the part of the debate about whether it was 
#'     optimal to stay or switch when given the option to switch.
#' 
#' @param  ... no arguments are used by the function.
#' 
#' @return 
#'    The function returns a numeric vector 1-3.
#' 
#' @examples
#'     change_door()
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
#'     Did you win the car, or lose and get a goat? 
#'     
#' @description
#'     `determine_winner()` returns a character vector of
#'     WIN or LOSE depending on if the final door was the car or goat. 
#'     
#' @details
#'     Based on the final door that was picked, the contestant
#'     is deemed to WIN or LOSE based on if the final door that
#'     was selected was the car or one of the goats. 
#' 
#' @param ... no arguments are used by the function.
#' 
#' @return 
#'     The function returns a character vector of WIN or LOSE
#'     depending on if the goat or car door is the final door 
#'     pick. 
#' 
#' @examples
#'     determine_winner()
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
#'    Play the Entire Monty Hall Game with One Function 
#'    
#' @description
#'     `play_game` runs the entire game simulation of the 
#'     functions in this package. 
#'     
#' @details
#'     This funtion will run all the Monty Hall Game functions
#'     as a loop. It will play thorugh an entire game of the 
#'     "Let's Make a Deal" process. 
#'     
#' @param ... no arguments are used by the function. 
#' 
#' @return 
#'     play game will return a data frame stating if
#'     STAY or SWITCH was the winning or losing option.
#' 
#' @examples
#'     play_game()
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
#'     Run Mulitple Simulations of the Monty Hall Game
#'     
#' @description
#'     `play_n_games()` will play the Monty Hall Game several 
#'     times based on the input of n for number of games. 
#'     
#' @details
#'      This funtion will run all the Monty Hall Game functions
#'      as a loop. It will play thorugh an entire game of the 
#'      "Let's Make a Deal" process. The loop will run based on 
#'      the number of games indicated by n. 
#' 
#' @param ... no arguments are used by the function. 
#' 
#' @return 
#'     This function will return a data frame with the strategy
#'     and percentage chance of winning or losing for each 
#'     strategy. Strategies include staying or switching. 
#'     
#' @examples
#'     play_n_games()
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
