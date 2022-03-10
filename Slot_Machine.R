get_symbols <- function(){
  wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
  sample(wheel, size = 3, replace = TRUE,
         prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52))
}


score <- function(symbols){

    # identify case
    
    #This tests if all symbols are exactly the same
    same <- ((symbols[1] == symbols[2]) & (symbols[2] == symbols[3]))
    
    #This line tests if all the symbols are of type bar
    bars <- (symbols %in% c("B", "BBB", "BB"))
    
    # get prize
    if(same){
      payouts <- c("DD" = 100, "7" = 80, "BBB" = 40, "BB" = 25,
                   "B" = 10, "C" = 10, "0" = 0)
      prize <- unname(payouts[symbols[1]])
    } else if(all(bars)){
      prize <- 5
    }else{
      cherries <- sum(symbols == "C")
      prize <- c(0, 2, 5)[cherries + 1]
    }
    
    #adjust for diamonds
    
    diamonds <- sum(symbols == "DD")
    #multiply by 2 to the power of diamonds
    prize * 2^diamonds
}

play <- function(){
  symbols <- get_symbols()
  print(symbols)
  score(symbols)
}