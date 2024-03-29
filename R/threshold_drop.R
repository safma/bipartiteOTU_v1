#' @title Threshold drop
#'
#' @description Funkce threshold_drop redukuje OTU tabulku na základě nastaveného práhu threshold. Pozorování nižší než nastavený práh jsou
#' z tabulky odstraněna.
#'
#' @param OTUtab maticový zápis OTU tabulky
#' @param threshold číselná proměnná. Prahová hodnota se kterou jsou pozorování porovnávána.
#'
#' @export
#' @return maticový zápis redukované OTU tabulky
#'
#'

drop_threshold <- function(OTUtab, threshold){

  # odstran?n? OTU jejich? pozorovan? ?etnost se pohybuje pod pr?hem threshold
  OTUtab[OTUtab<threshold] <- 0
  k <- which(rowSums(OTUtab)==0)
  if (length(k)>0){
      OTUtab <- OTUtab[-k,]
  }

  return(OTUtab)
}
