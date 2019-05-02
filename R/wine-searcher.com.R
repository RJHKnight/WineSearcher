baseURL <- "https://www.wine-searcher.com/find/"
noAuctionBit <- "/-/-/u"
ccyBit <- "?Xsavecurrency=Y&Xcurrencycode="
EXCLUDE_STRING <- "Search results prioritize sponsor merchants."

# Patterns
HALF_BOTTLE <- "Half Bottle"
BOTTLE <- "Bottle"
MAGNUM <- "Magnum"
SINGLE_CASE <- "Case"
CASE_BOTTLE <- "Case.*Btls"
CASE_MAGNUM <- "Case.*Mags"

# Internal function to pull data from www.wine-searcher.com
.loadWineSearcher <- function(name, vintage, currency = "AUD") {

  name <- escapeWhiteSpace(name)

  thisURL <- paste0(
    baseURL,
    name, "/",
    vintage,
    noAuctionBit,
    ccyBit,
    currency
  )

  thisPage <- read_html(thisURL)
  Title <- extractTitle(thisPage)
  thisPrices <- getPrices(thisPage)

  return (cbind(Title, thisPrices))
}

escapeWhiteSpace <- function(name) {
  str_replace_all(name, " ", "+")
}

# The individual prices
getPrices <- function(thisPage) {

  table <- thisPage %>%
    html_node(xpath = '//*[@id="wine_list"]') %>%
    html_table(header = TRUE, fill = FALSE)

  sellerData <- table %>%
    select(`Seller Information`) %>%
    separate(`Seller Information`, c("MerchantName", "Country", "Town"), sep = "\n{1,}", extra = "drop") %>%
    mutate(Country = str_replace(Country, ":.*", "")) %>%
    mutate(Town = case_when(
      Country == "Hong Kong" ~ "",
      TRUE            ~ Town))

  otherData <- table %>%
    select(`Offer Description`) %>%
    separate(`Offer Description`, c("Details", "Price", "Type", "IncludesTax", "Link"), sep = "\n{1,}", fill = "right" ) %>%
    mutate(Details = str_trim(Details, side = "right")) %>%
    mutate(Price = as.numeric(str_replace(str_extract(Price, "[\\d|,]+\\.*\\d*"), ",", ""))) %>%
    mutate(NumBottles = case_when(
      Type == HALF_BOTTLE               ~ 0.5,
      Type == BOTTLE                    ~ 1.0,
      Type == MAGNUM                    ~ 2.0,
      Type == SINGLE_CASE               ~ 6.0,
      str_detect(Type, CASE_BOTTLE)     ~ as.numeric(str_extract(Type, "\\d+\\.*\\d*")),
      str_detect(Type, CASE_MAGNUM)     ~ 2 * as.numeric(str_extract(Type, "\\d+\\.*\\d*")),
      TRUE                       ~ -1
    )) %>%
    mutate(PricePerBottle = Price / NumBottles) %>%
    select(-IncludesTax, -Link) %>%
    bind_cols(sellerData) %>%
    filter(Details != EXCLUDE_STRING)

  return (otherData)
}

# The parsed name for the wine we are searching for...
extractTitle <- function(thisPage) {

  thisPage %>%
    html_nodes("title") %>%
    html_text() %>%
    str_replace(":.*", "")
}
