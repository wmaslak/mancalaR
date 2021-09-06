#' make_move
#'
#' @param player
#' @param B_N
#' @param B_S
#' @param pit_r
#' @param pit_c
#' @param direction
#' @param can_capture
#' @param print_int
#' @param should_end
#' @param move_length
#'
#' @return

make_move <- function(player,
                      B_N,
                      B_S,
                      pit_r,
                      pit_c,
                      direction,
                      should_end = FALSE,
                      can_capture = FALSE,
                      print_int = FALSE,
                      move_length = 0) {


    move_length = move_length + 1

    after_sow_state <- single_sow(player,
                                  B_N,
                                  B_S,
                                  pit_r,
                                  pit_c,
                                  direction
                                  )

    if(print_int) print(after_sow_state)

    should_end <- after_sow_state$should_end

    after_capture_state <- capture_rule(player,
                                   after_sow_state$N,
                                   after_sow_state$S,
                                   after_sow_state$end_pit_r,
                                   after_sow_state$end_pit_c
                                   )

    if(print_int) print(after_capture_state)

    can_capture <- ifelse(is.logical(after_capture_state),FALSE,TRUE)

    if(print_int) print(paste("should_end: ", should_end,
                "can_capture: ", can_capture))

    if (can_capture == FALSE & should_end == FALSE) {# no capture, keep sowing
      # cat(paste("Gonna run myself again with such arguments: ",
      #             player, "\n",
      #             #after_sow_state$N,"\n",
      #             #after_sow_state$S,"\n",
      #             after_sow_state$end_pit_r,"\n",
      #             after_sow_state$end_pit_c,"\n",
      #             direction,"\n",
      #             should_end,"\n",
      #             can_capture)
      #       )
      make_move(player,
                after_sow_state$N,
                after_sow_state$S,
                after_sow_state$end_pit_r,
                after_sow_state$end_pit_c,
                direction,
                should_end,
                can_capture,
                print_int = print_int,
                move_length = move_length)

    }else if(can_capture == TRUE){ # capture, keep sowing

      # cat(paste("Gonna run myself again with such arguments: ",
      #             player,"\n",
      #             #after_capture_state$N,"\n",
      #             #after_capture_state$S,"\n",
      #             after_sow_state$end_pit_r,"\n",
      #             after_sow_state$end_pit_c,"\n",
      #             direction,"\n",
      #             should_end,"\n",
      #             can_capture))

          make_move(player,
                    after_capture_state$N,
                    after_capture_state$S,
                    after_sow_state$end_pit_r,
                    after_sow_state$end_pit_c,
                    direction,
                    should_end,
                    can_capture,
                    print_int = print_int,
                    move_length = move_length)

    }else if (should_end & !can_capture){# no capture, and landed in empty pit => end move

      return(list(N = after_sow_state$N , S = after_sow_state$S, move_length = move_length))
    }


}



#' Check if starting pit is allowed and perform move.
#'
#' @param player
#' @param B_N
#' @param B_S
#' @param pit_r
#' @param pit_c
#' @param direction
#' @param print_int
#'
#' @export

check_and_move <- function(player,
                           B_N,
                           B_S,
                           pit_r,
                           pit_c,
                           direction,
                           print_int = F){


  # check if the chosen pit is allowed as a starting pit
  if(player == "S"){
    move_allowed <- check_if_move_possible(pit_r = pit_r,pit_c = pit_c, board = B_S)
    if(!move_allowed){
      print(paste("Move from pit",pit_r ,pit_c ,"not allowed!"))
      return(-1)
    }else{
      boards <- make_move(player,B_N,B_S,pit_r,pit_c,direction, print_int = print_int)

      return(list(N = boards$N, S = boards$S, move_length = boards$move_length))
    }

  }else if(player == "N"){
    move_allowed <- check_if_move_possible(pit_r = pit_r,pit_c = pit_c, board = B_N)
    if(!move_allowed){
      print(paste("Move from pit",pit_r ,pit_c ,"not allowed!"))
      return(-1)
    }else{
      boards <- make_move(player,B_N,B_S,pit_r,pit_c,direction, print_int = print_int)

      return(list(N = boards$N, S = boards$S, move_length = boards$move_length))

    }

  }else{
    print(paste0("Wrong player! No such player as ",player))
    return(-1)
  }

}

#' Check if game should end or continue given boards of players.
#'
#' @param B_N
#' @param B_S
#' @param print_int
#'
#' @examples
#' @export
check_if_end_game <- function(B_N,B_S, print_int = FALSE){
  # check if any of the players cannot move
  if( !any(B_N>1) ){

    if(print_int) print("Player S wins!")

    # TODO add returning info about who won
    return(T)

  }else if( !any(B_S > 1) ){

    if(print_int) print("Player N wins!")

    # TODO add returning info about who won
    return(T)

  }else if( any(B_S>1) & any(B_N>1) ){

    return(F)

  }else if( !any(B_S>1) & !any(B_N>1) ){

    stop("Opss... Both players cannot move, this should not happen!")

  }
}


#' Initialize df with game statistics
#'
#' @return
#'
#' @examples
#' @export
init_stats_df <- function(){

  #initialize df with statistics and helpers
  stats_df <- data.frame(move = integer(),
                      move_N = character(),
                      move_S = character(),
                      stones_N = integer(),
                      stones_S = integer(),
                      depth = integer(),
                      B_N = character(),
                      B_S = character(),
                      stringsAsFactors=FALSE)


  return(stats_df)

}

#' Parse move to easily handeled information
#'
#' @param r row of pit
#' @param c col of pit
#' @param d direction
#'
#' @return string in the form of "rcd"
#'
#'
#' @examples
parse_move_str <- function(r,c,d){

  return(as.character(paste0(r,c,d)))

}


#' Add move to stats df
#'
#' @param B_N board n
#' @param B_S board s
#' @param r_N
#' @param c_N
#' @param d_N
#' @param r_S
#' @param c_S
#' @param d_S
#' @param stones_N
#' @param stones_S
#' @param depth
#' @param stats_df
#'
#' @return
#' @export

add_move_stats <- function(move,B_N,B_S,
                      r_N,c_N,d_N,
                      r_S,c_S,d_S,
                      depth,
                      stats_df = stats_df){

  move_N <- parse_move_str(r_N,c_N,d_N)
  move_S <- parse_move_str(r_S,c_S,d_S)

  stones_N <- sum(B_N)
  stones_S <- sum(B_S)

  board_s <- encode_board(B_N)
  board_n <- encode_board(B_S)

  new_row <- list(as.integer(move),
                move_N,move_S,
                as.integer(stones_N),as.integer(stones_S),
                as.integer(depth),
                board_s,
                board_n
                )

  names(new_row) <- colnames(stats_df)


  ret_df <- rbind(stats_df,new_row)

  return(ret_df)

}



#' Two player console mode
#'
#' @param print_int
#'
#' @return
#'
#' @examples
#'
#'
#'
two_player_mode <- function(B_N,B_S,print_int = FALSE) {

  stats_df <- init_stats_df()
  move_no <- 0

  if (print_int)
    print("Board of player N:")
  print(B_N)
  if (print_int)
    print("Board of player S:")
  print(B_S)

  if (print_int) {
    print("N moves first!")
    print(
      "Provide the moves as a 3-ples: [row of starting pit] [row of starting pit] [direction: 1 for CW, 0 for CCW]"
    )
  }

  # loop until someone looses (cannot move)
  while(!check_if_end_game(B_N,B_S,print_int = print_int)){

    # get move from player N
    move_N <- readline("N move: ")
    move_N <- as.numeric(regmatches(move_N,
                         gregexpr("[0-9.]+", move_N))[[1]])
    if(move_N[1]>2 |
       move_N[2] > 8 |
       move_N[3] > 1 |
       move_N[1] < 1 |
       move_N[2] < 1 |
       move_N[3] < 0
    ){
      print("That move is invalid!")
      next
    }
    # make move
    boards <- check_and_move("N",B_N,B_S,move_N[1],move_N[2],move_N[3],
                             print_int = print_int)


    if(is.numeric(boards)){

      #Move is illegal
      next

    }else{
      B_S <- boards$S
      B_N <- boards$N

      if(check_if_end_game(B_N,B_S,print_int = print_int)) break

      move_no = move_no + 1
      stats_df <-  add_move_stats(move_no,B_N,B_S,
                     move_N[1],move_N[2],move_N[3],
                     0,0,0,
                     boards$move_length,
                     stats_df = stats_df)

      # print boards
      print(B_N)
      print(B_S)

      # get move from player S
      move_S <- readline("S move: ")
      move_S <- as.numeric(regmatches(move_S,
                           gregexpr("[0-9.]+", move_S))[[1]])

      while (move_S[1] > 2 |
             move_S[2] > 8 |
             move_S[3] > 1 |
             move_S[1] < 1 |
             move_S[2] < 1 |
             move_S[3] < 0) {

        print("That move is invalid!")
        move_S <- readline("S move: ")
        move_S <- as.numeric(regmatches(move_S,
                             gregexpr("[0-9.]+", move_S))[[1]])
      }

      # make move
      boards <-
        check_and_move("S", B_N, B_S, move_S[1], move_S[2], move_S[3],
                       print_int = print_int)

      # if the move is ok we will not enter the loop, otherwise we won't leave
      # it until it's correct

      while(is.numeric(boards)){

        #keep asking for move
        move_S <- readline("S move: ")
        move_S <- as.numeric(regmatches(move_S,
                             gregexpr("[0-9.]+", move_S))[[1]])
        # make move
        boards <- check_and_move("S",B_N,B_S,move_S[1],move_S[2],move_S[3],
                                 print_int = print_int)


      }

      B_S <- boards$S
      B_N <- boards$N

      move_no = move_no + 1
      stats_df <- add_move_stats(move_no,B_N,B_S,
                     0,0,0,
                     move_S[1],move_S[2],move_S[3],
                     boards$move_length,
                     stats_df = stats_df)

      # print boards
      print(B_N)
      print(B_S)

    }
  }
  return(stats_df)
}

#' Choose random but possible move.
#'
#' @param board
#'
#' @return
#'
#' @examples
#' @export
choose_random_move <- function(board){

  poss <- find_posssible_moves(board)
  stopifnot(nrow(poss)>0)
  # choose row and direction randomly
  row <- sample.int(nrow(poss),1)
  direction <- sample(c(0,1),1)

  #return(list(r = poss[row,1], c = poss[row,2], direction = direction))
  return(c(poss[row,1],poss[row,2],direction))
}



#' Two bots
#'
#' Each chooses random move out of possible moves until the game ends.
#'
#' @param B_N
#' @param B_S
#' @param print_int
#'
#' @return
#'
#' @examples
random_vs_random_mode <- function(B_N, B_S, print_int = FALSE){

  stats_df <- init_stats_df()
  move_no <- 0

  # loop until someone looses (cannot move)
  while(!check_if_end_game(B_N,B_S,print_int = print_int)){

    # get move from player N
    move_N <- unname(choose_random_move(B_N))
    print(paste("==== move_N:",move_N[1],move_N[2],move_N[3]))
    # make move
    boards <- check_and_move("N",B_N,B_S,move_N[1],move_N[2],move_N[3],
                             print_int = print_int)

    B_S <- boards$S
    B_N <- boards$N

    move_no = move_no + 1
    stats_df <-  add_move_stats(move_no,B_N,B_S,
                                move_N[1],move_N[2],move_N[3],
                                0,0,0,
                                boards$move_length,
                                stats_df = stats_df)



    if(check_if_end_game(B_N,B_S,print_int = print_int)) break
    # print boards
    #print(B_N)
    #print(B_S)

    # get move from player S
    move_S <- unname(choose_random_move(B_S))
    print(paste("---- move_S:",move_S[1],move_S[2],move_S[3]))
    # make move
    boards <-
      check_and_move("S", B_N, B_S, move_S[1],move_S[2],move_S[3],
                     print_int = print_int)

    # if the move is ok we will not enter the loop, otherwise we won't leave
    # it until it's correct

    B_S <- boards$S
    B_N <- boards$N

    move_no = move_no + 1
    stats_df <- add_move_stats(move_no,B_N,B_S,
                               0,0,0,
                               move_S[1],move_S[2],move_S[3],
                               boards$move_length,
                               stats_df = stats_df)

    # print boards
    #print(B_N)
    #print(B_S)



  }

  return(stats_df)

}

#' Play Mancala in selected mode
#'
#' @param mode modes available: \code{two_player} - two players play on the
#' same machine
#' @param print_int
#'
#' @return
#'
#'
#' @examples
play_game <- function(mode="two_player",print_int=FALSE){


  # initialize boards

  B_N <-  matrix(c(2, 2, 2, 2, 2, 2, 2, 2,
                   2, 2, 2, 2, 0, 0, 0, 0),
                 nrow = 2,
                 byrow = TRUE)
  B_S <- matrix(c(0, 0, 0, 0, 2, 2, 2, 2,
                  2, 2, 2, 2, 2, 2, 2, 2),
                nrow = 2,
                byrow = TRUE)

  if(mode == "two_player"){

    two_player_mode(B_N,B_S,print_int = print_int)

  }else if(mode == "random_vs_random"){

    random_vs_random_mode(B_N,B_S,print_int = print_int)

  }
}



simulate_mancala <- function(mode = "random_vs_random",gameplay=play_game,n=20){

  games_df <- data.frame()

  for (i in 1:n) {
    print(paste0("Playing game ",i))
    new_game <- gameplay(mode=mode)

    if(tail(new_game,1)$stones_S > tail(new_game,1)$stones_N){

      winner <- "S"

    }else if(tail(new_game,1)$stones_S < tail(new_game,1)$stones_N){

      winner <- "N"

    }else{
      stop("Something is wrong, players cannot end with equal stones")
    }

    new_game$game_no <- i
    new_game$winner <- winner


    games_df <- rbind(games_df,new_game)

  }

  return(games_df)

}
