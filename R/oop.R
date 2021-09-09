
#' Random bot
#'
#' @param name name of player
#' @param state list of boards
#' @export

random_bot <- function(name,state){
  stopifnot(name == "S" | name == "N")
  bname <- paste0("B_",name)
  move <- unname(mancalaR::choose_random_move(state[[bname]]))
  return(move)
}


#' Human player
#'
#' take move from human player and parse it to vector
#'
#' @param name name of the player "N" for north and "S" for south
#' @param state state of the game
#'
#' @return move
#' @export

human_player <- function(name, state){
  bname <- paste0("B_",name)
  allowed_moves <- mancalaR::find_posssible_moves(state[[bname]])

  print(state$B_N)
  print(state$B_S)

  move <- readline(paste0(name," move: "))
  move <- as.numeric(regmatches(move,
                                gregexpr("[0-9.]+", move))[[1]])

  is_allowed <- any((allowed_moves[,1] == move[1]) & (allowed_moves[,2] == move[2]))
  print(move)
  while(!is_allowed){
    print(paste0("Cannot move from ",move[1],' ',move[2],". Pick another move!"))

    move <- readline(paste0(name," move: "))
    move <- as.numeric(regmatches(move,
                                  gregexpr("[0-9.]+", move))[[1]])

    is_allowed <- any((allowed_moves[,1] == move[1]) & (allowed_moves[,2] == move[2]))

  }

  return(move)

}

#' init state
#'
#' @return initialize HusBao state (boards)
#' @export

init_state <- function(){

  state <- list(B_N = NA, B_S = NA)
  state$B_N <- matrix(c(2, 2, 2, 2, 2, 2, 2, 2,
                        2, 2, 2, 2, 0, 0, 0, 0),
                      nrow = 2,
                      byrow = TRUE)

  state$B_S <- matrix(c(0, 0, 0, 0, 2, 2, 2, 2,
                        2, 2, 2, 2, 2, 2, 2, 2),
                      nrow = 2,
                      byrow = TRUE)
  return(state)

}

#' An R6 class to represent a mancala game
#'
#' @field players a list of functions taking the state of the game and
#' returning the move.
#' @field state contains the boards of both players
#' @field stats_df is a dataframe containing statistics of the played game
#' @field active_player the player currently taking the turn
#' @field check_game_end Checker of whether game should end. Function taking state of the game as an input and returning a logical.
#' @field make_move Mechanics of move. Function that takes the state of the game, the player's identifier and a their move and returns state of the game
#' @field init_state Initial state of the game. Function that returns the initial state of the game.
#' @export

Mancala <- R6::R6Class("Mancala",
                   public = list(
                    # fields
                    players = list(N=NA,S=NA),
                    state    = NA,
                    stats_df = NA,
                    active_player = NA,
                    ## game mechanics to be provided
                    check_game_end = NA,
                    make_move      = NA,
                    init_state     = NA,

                    # methods
                    ## technical


#' @param player_N Mechanics of player N. Function that takes the state of the game as an input and returns a move as a numeric vector.
#' @param player_S Mechanics of player S. Function that takes the state of the game as an input and returns a move as a numeric vector
#' @param check_game_end Checker of whether game should end. Function taking state of the game as an input and returning a logical.
#' @param make_move Mechanics of move. Function that takes the state of the game, the player's identifier and a their move and returns state of the game
#' @param init_state Initial state of the game. Function that returns the initial state of the game.
#' @param starting_state optional. Starting state. A state from which we want to start the game instead of the initial state.
#'


                    initialize =  function(player_N = mancalaR::human_player,
                                           player_S = mancalaR::human_player,
                                           check_game_end = mancalaR::check_if_end_game,
                                           make_move = mancalaR::check_and_move,
                                           init_state = mancalaR::init_state,
                                           starting_state = NA){
                      # check args
                      stopifnot(class(player_N)=="function"&
                                class(player_S)=="function"&
                                class(check_game_end)=="function"&
                                class(make_move)=="function" &
                                class(init_state)=="function"
                                )

                      # assign fields their values
                      self$players$N <- player_N
                      self$players$S <- player_S
                      self$check_game_end <-check_game_end
                      self$make_move <- make_move
                      self$init_state <- init_state

                      if(is.na(starting_state)){
                        self$state <- self$init_state()
                      }else if(is.list(starting_state)){
                        self$state <- starting_state
                      }

                    },
#' reset state
#'
#' @return assigns initial state to the state field

                    reset_state = function(){
                      self$state <- self$init_state()
                    },

                    ## game mechanics that are universal

#' @param print_int logical. choose whether to show verbose debugging stuff
                    play_game = function(print_int = F){

                      self$active_player <- sample(c("S","N"),1)
                      self$stats_df <- mancalaR::init_stats_df()
                      move_no <- 0

                      #loop until someone looses (cannot move)
                      while(!self$check_game_end(self$state$B_N,self$state$B_S,print_int = print_int)){

                        # get move from player N
                        move <- self$players[[self$active_player]](self$active_player,self$state)
                        print(paste("==== move player",self$active_player,":",move[1],move[2],move[3]))
                        # make move
                        move_result <- self$make_move(self$active_player,self$state$B_N,self$state$B_S,move[1],move[2],move[3],
                                                 print_int = print_int)

                        self$state$B_S <- move_result$S
                        self$state$B_N <- move_result$N

                        move_no = move_no + 1
                        if(self$active_player == "N"){
                          move_to_add <- c(move,0,0,0)
                        }else{
                          move_to_add <- c(0,0,0,move)
                        }
                        self$stats_df <-  mancalaR::add_move_stats(move_no,self$state$B_N,self$state$B_S,
                                                    move_to_add[1],move_to_add[2],move_to_add[3],
                                                    move_to_add[4],move_to_add[5],move_to_add[6],
                                                    move_result$move_length,
                                                    stats_df = self$stats_df)

                        if(self$active_player == "S"){
                          self$active_player <- "N"
                        }else if(self$active_player == "N"){
                          self$active_player <- "S"
                        }else{
                          stop("Shouldn't be here!")
                        }


                        }

                      return(self$stats_df)

                      }



                   )
                   )
