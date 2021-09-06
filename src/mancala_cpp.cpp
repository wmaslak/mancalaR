#include <Rcpp.h>
using namespace Rcpp;

//' Faster version of single sow
//' @param
// [[Rcpp::export]]
List single_sow_cpp(String player,
                    NumericMatrix B_N,
                    NumericMatrix B_S,
                    int pit_r,
                    int pit_c,
                    int direction) {

  // declare types
  int stone_count;
  int curr_pit_r;
  int curr_pit_c;
  int next_pit_r;
  int next_pit_c;
  bool should_end;

  //NumericMatrix B_N = clone(B_N_arg);
  //NumericMatrix B_S = clone(B_S_arg);

  // actual body

  pit_r --;
  pit_c --;

  if(player == "S"){
    stone_count = B_S(pit_r,pit_c);
    B_S(pit_r,pit_c) = 0;
  }else{
    stone_count = B_N(pit_r,pit_c);
    B_N(pit_r,pit_c) = 0;
  }

  curr_pit_r = pit_r;
  curr_pit_c = pit_c;
  should_end = false;

  if(direction == 1){
    while(stone_count>0){

      if(curr_pit_r == 0 & curr_pit_c == 7){
        next_pit_r  = 1;
        next_pit_c  = curr_pit_c;
      }else if( curr_pit_r==1 &  curr_pit_c==0){
        next_pit_r  = 0;
        next_pit_c  = curr_pit_c;
      }else if( curr_pit_r==0){
        next_pit_r  = curr_pit_r;
        next_pit_c  = curr_pit_c+1;
      }else{
        next_pit_r  = curr_pit_r;
        next_pit_c  = curr_pit_c-1;
      }

      if(player == "S"){

        B_S (next_pit_r,next_pit_c)  = B_S (next_pit_r,next_pit_c) + 1;

      }else{
        B_N (next_pit_r,next_pit_c)  = B_N (next_pit_r,next_pit_c) + 1;

      }

      stone_count--;
      curr_pit_c  = next_pit_c;
      curr_pit_r  = next_pit_r;

      if(player == "S"){

        if(B_S(curr_pit_r,curr_pit_c) == 1) should_end = true;

      }else{

        if(B_N(curr_pit_r,curr_pit_c) == 1) should_end = true;

      }

    }

  }else{

    while (stone_count>0) {

      if( curr_pit_r ==0  &  curr_pit_c ==0){
        next_pit_r = 1;
        next_pit_c = curr_pit_c;
      }else if( curr_pit_r ==1  &  curr_pit_c ==7){
        next_pit_r = 0;
        next_pit_c = curr_pit_c;
      }else if( curr_pit_r == 0){
        next_pit_r = curr_pit_r;
        next_pit_c = curr_pit_c-1;
      }else{
        next_pit_r = curr_pit_r;
        next_pit_c = curr_pit_c+1;
      }

      if(player == "S"){
        B_S(next_pit_r,next_pit_c) = B_S(next_pit_r,next_pit_c) + 1;

      }else{
        B_N(next_pit_r,next_pit_c) = B_N(next_pit_r,next_pit_c) + 1;

      }

      stone_count = stone_count - 1;
      curr_pit_c = next_pit_c;
      curr_pit_r = next_pit_r;

      if(player == "S"){

        if(B_S(curr_pit_r,curr_pit_c) == 1) should_end = true;

      }else{

        if(B_N(curr_pit_r,curr_pit_c) == 1) should_end = true;

      }

    }




  }
  List state;
  state["B_N"] = B_N;
  state["B_S"] = B_S;
  state["end_pit_r"] = curr_pit_r;
  state["end_pit_c"] = curr_pit_c;
  state["should_end"] = should_end;

  return(state);
}





