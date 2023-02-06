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
#'   Select a random door between 1 and 3
#'
#' @description
#'    `select_door()` generates 3 doors and randomly selects one
#'
#' @details
#'   The selection serves as the initial door choice for the contestant
#'   in the Monty Hall game.
#'
#' @param ... no arguments are used by the function.
#'
#' @return  The function returns a number between 1 and 3
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
#'   Open the Goat Door
#'
#' @description
#'    `open_goat_door()` selects the Goat Door to be opened
#'
#' @details
#'   If the contestant's initial door selection is the car, the function
#'   opens either remaining goat door at random. If the contestant's initial
#'   door selection is a goat, then the other goat door is opened.
#'
#' @param game game A length 3 character vector indicating the positions of
#' goats and the car
#' @param a.pick A number between 1 and 3
#'
#' @return  The function returns a number between 1 and 3
#'
#' @examples
#'   open_goat_door()
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
#'   Select the whether the contestant stays or switches doors
#'
#' @description
#'    `change_door()` selects the final door for the contestant
#'
#' @details
#'   The function produces the final door selection based on the initial
#'   door choice and whether the contestant stays with that choice or
#'   switches to the other remaining door.
#'
#' @param Stay=T A logical vector
#' @param opened.door A number between 1 and 3
#' @param a.pick A number between 1 and 3
#'
#' @return  The function returns a number between 1 and 3
#'
#' @examples
#'   change_door()
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
#'   Determine whether the contestant wins or loses
#'
#' @description
#'    `determine_winner()` evaluates whether the contestants wins or loses
#'
#'
#' @details
#'   The function determines whether the contestant's final pick is the
#'   winning car door, or the losing goat door.
#'
#' @param final.pick A number between 1 and 3
#' @param game A length 3 character vector indicating the positions of
#' goats and the car.
#'
#' @return  The function returns a character vector of either "WIN" or "LOSE"
#'
#' @examples
#'   change_door()
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
#'   Play the Monty Hall game
#'
#' @description
#'    `play_game()` plays a single Monty Hall game and displays the outcome
#'
#'
#' @details
#'   The function plays a 3-door Monty Hall game with 2 goats and 1 car.
#'   The outcome is displayed for whether the contestant chose to stay
#'   with their initial door selection or switch to the other door.
#'
#' @param ... no arguments are used by the function.
#'
#' @return  The function returns a data frame of the game results
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
#'   Play the Monty Hall game n times and display results
#'
#' @description
#'    `play_n_games()` plays a Monty Hall game n times displays the outcome
#'
#'
#' @details
#'   The function plays a 3-door Monty Hall game with 2 goats and 1 car.The
#'   function loops for a specified number of times. The outcomes arr displayed
#'   for the number of times the contestant wins or loses with each strategy,
#'   either staying or switching
#'
#' @param n=100 A number to determine how many times to iterate the function
#'
#' @return  The function returns a table of results and displays the proportion
#' of wins and losses for each strategy
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
