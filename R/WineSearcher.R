
#' Find wine prices online
#'
#' @param name The name of the wine
#' @param vintage A vintage to search for
#' @param currency The currency to return prices in
#' @param aggregate Return a summary of prices by deal type
#' @return A data frame containing the raw prices plus the following fields:
#' \describe{
#'   \item{Title}{The parsed name of the wine you are searching for}
#'   \item{Details}{The merchant description of the product}
#'   \item{Price}{The raw price of the deal}
#'   \item{Type}{The type of deal - single bottle, magnum, case etc}
#'   \item{NumBottles}{The volume of wine in the deal, expressed as a number of standard bottles (eg. a magnum would be 2)}
#'   \item{Merchange Name}{The name of the merchant}
#'   \item{Country}{The coutry of the merchange}
#'   \item{Town}{The town or state of the merchant}
#' }
#' @examples
#' findWine("st henri", 2014, "AUD")
#' @export
findWine <- function(name, vintage, currency, aggregate = FALSE) {

  res <- .loadWineSearcher(name, vintage, currency = "AUD")

  if (!aggregate) {
    return (res)
  }

  return (aggregateByType(res))
}

# TODO: Aggregate by:
# 1) Type (do we need to infer a generic type)
# 2) Country
# 3) None (i.e. simple average)
aggregateByType <- function(res) {

  res %>%
    group_by(Type) %>%
    summarise(
      NumDeals              = n(),
      AveragePricePerBottle = mean(PricePerBottle),
      AveragePricePerDeal   = mean(Price)
    ) %>%
    arrange(-NumDeals)
}
