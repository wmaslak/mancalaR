#' single_sow
#'
#' Make a single sowing move, without "compound" sowing
#'
#' @param player Player making the move, \code{"S"} for south or \code{"N"} for north
#' @param B_N Board of player N
#' @param B_S Board of player S
#' @param pit_r Row of the starting pit
#' @param pit_c Column of the starting pit
#' @param direction Direction of sowing, either 1 for clockwise of 0 for
#'   counter-clockwise
#' @return \code{list(N, S,end_pit_r, end_pit_c, should_end)} Boards of player N
#'   and S, ending pit coordinates and boolean indicating whether we've ended in
#'   an empty pit


single_sow<- function(player,B_N, B_S, pit_r, pit_c, direction) {
  #TODO add argument validation?
  if(player == "S"){
    stone_count <- B_S[pit_r,pit_c]
    B_S[pit_r,pit_c] <- 0

  }else{
    stone_count <- B_N[pit_r,pit_c]
    B_N[pit_r,pit_c] <- 0
  }

  curr_pit_r <- pit_r
  curr_pit_c <- pit_c
  should_end <- FALSE
  if(direction == 1){

    move_length = 0
    captured = 0

    while (stone_count>0) {

      if(identical(curr_pit_r,1) & identical(curr_pit_c,8)){
        next_pit_r <- 2
        next_pit_c <- curr_pit_c
      }else if(identical(curr_pit_r,2) & identical(curr_pit_c,1)){
        next_pit_r <- 1
        next_pit_c <- curr_pit_c
      }else if(identical(curr_pit_r,1)){
        next_pit_r <- curr_pit_r
        next_pit_c <- curr_pit_c+1
      }else{
        next_pit_r <- curr_pit_r
        next_pit_c <- curr_pit_c-1
      }

      if(player == "S"){
        B_S[next_pit_r,next_pit_c] <- B_S[next_pit_r,next_pit_c] + 1

      }else{
        B_N[next_pit_r,next_pit_c] <- B_N[next_pit_r,next_pit_c] + 1

      }
      stone_count <- stone_count - 1
      curr_pit_c <- next_pit_c
      curr_pit_r <- next_pit_r
    }
    #check if move should end (last pit has one stone only)
    if(player == "S"){
      if(B_S[curr_pit_r,curr_pit_c] == 1) should_end <- TRUE
    }else{
      if(B_N[curr_pit_r,curr_pit_c] == 1) should_end <- TRUE
    }
  }else{

    move_length = 0
    captured = 0

    while (stone_count>0) {

      if(identical(curr_pit_r,1) & identical(curr_pit_c,1)){
        next_pit_r <- 2
        next_pit_c <- curr_pit_c
      }else if(identical(curr_pit_r,2) & identical(curr_pit_c,8)){
        next_pit_r <- 1
        next_pit_c <- curr_pit_c
      }else if(identical(curr_pit_r,1)){
        next_pit_r <- curr_pit_r
        next_pit_c <- curr_pit_c-1
      }else{
        next_pit_r <- curr_pit_r
        next_pit_c <- curr_pit_c+1
      }

      if(player == "S"){
        B_S[next_pit_r,next_pit_c] <- B_S[next_pit_r,next_pit_c] + 1

      }else{
        B_N[next_pit_r,next_pit_c] <- B_N[next_pit_r,next_pit_c] + 1

      }
      stone_count <- stone_count - 1
      curr_pit_c <- next_pit_c
      curr_pit_r <- next_pit_r

    }
    #check if move should end (last pit has one stone only)
    if(player == "S"){
      if(B_S[curr_pit_r,curr_pit_c] == 1) should_end <- TRUE
    }else{
      if(B_N[curr_pit_r,curr_pit_c] == 1) should_end <- TRUE
    }
  }
#print(c(curr_pit_r,curr_pit_c))
return(list(N = B_N, S = B_S,
            end_pit_r = curr_pit_r, end_pit_c = curr_pit_c,
            should_end = should_end))
  }


#' capture_rule
#'
#' Decide if capture is activated at the end of the move
#'
#' @param player Player making the move, \code{"S"} for south or \code{"N"} for north
#' @param B_N Board of player N
#' @param B_S Board of player S
#' @param end_pit_r,end_pit_c Row and column of the pit where the sowing ended
#' @return \code{list(captured = cap, N = B_N, S= B_S, stones_won = st_won)} Boolean indicating if capture took place, boards of player N and S
#'   after the capture, number of stones captured. If capture did not take place
#'   it returns \code{FALSE}
#' @export

capture_rule <- function(player, B_N, B_S, end_pit_r, end_pit_c){

  cap <- FALSE
  # check if capture will happen
  if (player == "S") {
    if((end_pit_r == 1) & (B_N[2,end_pit_c] > 0)){
      cap <- TRUE
    }
  }else{
    if ((end_pit_r == 2) & (B_S[1,end_pit_c] > 0)){
      cap <- TRUE
    }
  }


  if (!cap) { #if capture condition not fulfilled
    return(cap)
  }else{
    if (player == "S") {
      st_won <- sum(B_N[1:2,end_pit_c])
      B_N[1:2,end_pit_c] <- c(0,0)
      B_S[end_pit_r,end_pit_c] <- B_S[end_pit_r,end_pit_c] + st_won
    }else{
      st_won <- sum(B_S[1:2,end_pit_c])
      B_S[1:2,end_pit_c] <- c(0,0)
      B_N[end_pit_r,end_pit_c] <- B_N[end_pit_r,end_pit_c] + st_won
    }
    return(list(captured = cap, N = B_N, S= B_S, stones_won = st_won))
  }




}

#' find_possible_moves
#'
#' Find coordinates of possible starting pits.
#' You can move from a pit if ic contains at least 2 stones.
#'
#'
#' @param board board which we want to find moves from
#'
#' @return Named matrix which contains coordinates of possible starting pits.
#' @export
find_posssible_moves <- function(board){
  poss <- which(board > 1,T)

  return(poss)
}

#' Check if provided move is possible
#'
#' @param pit_r row of the chosen pit
#' @param pit_c column of the chosen pit
#' @param board board of the player making the move
#'

check_if_move_possible <- function(pit_r,pit_c,board){

  poss_moves <- find_posssible_moves(board)
  # check if the intended pit is on the list of possible moves
  is_poss <- any(poss_moves[,1] == pit_r & poss_moves[,2] == pit_c)

  return(is_poss)
}

#' Encode board from matrix to string
#'
#' @param board a matrix representing a HusBao board
#'
#' @return a string representing the matrix
encode_board <- function(board){
  return(paste0(as.numeric(board),collapse = ''))
}

#' Decode board from string to matrix
#'
#' @param board_encoded a string of digits representing the HusBao board
#'
#' @return a matrix corresponding to the string

decode_board <- function(board_encoded){
  board_v <- as.integer(unlist(strsplit(board_encoded, "")))
   board <-  matrix(board_v,nrow=2,ncol=8)
  return(board)
}
