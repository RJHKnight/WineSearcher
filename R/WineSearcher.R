
#' Find wine prices online
#'
#' @param name The name of the wine
#' @param vintage A vintage to search for
#' @param currency The currency to return prices in
#' @return A data frame containing the raw prices plus the following fields:
#' \describe{
#'   \item{Title}{The parsed name of the wine you are searching for}
#'   \item{Details}{The merchant description of the product}
#'   \item{Price}{The raw price of the deal}
#'   \item{NumBottles}{The volume of wine in the deal, expressed as a number of standard bottles (eg. a magnum would be 2)}
#'   \item{Merchange Name}{The name of the merchant}
#'   \item{Country}{The coutry of the merchange}
#'   \item{Town}{The town or state of the merchant}
#' }
#' @examples
#' findWine("st henri", 2014, "AUD")
#' @export
findWine <- function(name, vintage, currency) {

  .loadWineSearcher(name, vintage, currency = "AUD")
}
