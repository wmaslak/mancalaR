#' single_sow
#'
#' Make a single sowing move, without "compound" sowing
#'
#' @param player Player making the move, S for south or N for north
#' @param B_N Board of player N
#' @param B_S Board of player S
#' @param pit_r Row of the starting pit
#' @param pit_c Column of the starting pit
#' @param direction Direction of sowing, either 1 for clockwise of 0 for
#'   counter-clockwise
#' @return Boards of player N and S, ending pit coordinates and boolean
#'   indicating whether we've ended in an empty pit


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



