###
# scraping international ikea websites
# 2303
###

wdir <- "~/work/Teaching/DSIER23/03-web-scraping-api/"
dir.create(file.path(wdir, "temp"), showWarnings = FALSE)
dir.create(file.path(wdir, "output"), showWarnings = FALSE)

setwd(wdir)

# libraries ----
pacman::p_load(rvest)     # Web scraping and parsing HTML/XML content
pacman::p_load(stringr)   # String manipulation tools
pacman::p_load(magrittr)  # Pipe operator for chaining commands
pacman::p_load(data.table) # Fast data manipulation and processing
pacman::p_load(jsonlite)  # Read and write JSON data
pacman::p_load(tidyr)     # Reshape and tidy data
pacman::p_load(countrycode) # Convert country names/codes to various standard formats
pacman::p_load(ggplot2)   # Data visualization
pacman::p_load(ggrepel)   # Prevent overlapping labels in ggplot2 plots

# custom functions ----
possibly_fromJSON = purrr::possibly(fromJSON, otherwise = NA) # function so that if it encounters an error, it returns a default value instead of throwing an error
possibly_html_element = purrr::possibly(html_element, otherwise=NA)

# structure of ikea links ----
# https://www.ikea.com/us/en/p/billy-bookcase-white-00263850/
# https://www.ikea.com/de/de/p/billy-buecherregal-weiss-00263850/
# https://www.ikea.com/fr/fr/p/billy-bibliotheque-blanc-00263850/
# https://www.ikea.com/ca/en/p/billy-bookcase-white-00263850/

# https://www.ikea.com/de/de/search/products/?q=00263850
# https://sik.search.blue.cdtapps.com/us/en/search-result-page
# https://sik.search.blue.cdtapps.com/us/en/search-result-page?max-num-filters=8&q=00263850
# https://sik.search.blue.cdtapps.com/us/en/search-result-page?max-num-filters=8&q=00263850

# download regions ----
regions = readLines("https://www.ikea.com/global/en/shared-data/regions.js") #read the content of the page into a vector of character strings
regions = str_remove(regions, "window\\['regions\\-10bpdvv'\\] \\= ") 
regions = fromJSON(regions)
names(regions) # Displays the column names
regions = regions %>% unnest(cols = "localizedSites")
setDT(regions)
regions = regions[, .(code, isoCode3, language, languageCode, url)]

# subset to the "easy" ones (rest for practice at home) ----
regions = regions[!(isoCode3 %in% c("RUS") | isoCode3 %in% "UKR") & languageCode!="RU"] 
regions = regions[!isoCode3 %in% c("THA", "GRC", "ISL",
                                   "TUR", "CYP", "NZL",
                                   "CHL", "SGP", "BGR", "COL")]

i = regions[isoCode3 == "USA", which = TRUE] 

# iterate over localized sites ----
# for (i in 1:nrow(regions)) {

cat(i, "-", regions[i]$url, "\n")
  
# set country, language and file path
country = regions[i]$code %>% str_to_lower()
language = regions[i]$languageCode %>% str_to_lower()

# in many cases we need to fetch the information from a json (that is then rendered locally if viewed in browser)
page_json = possibly_fromJSON(str_c("https://sik.search.blue.cdtapps.com/", country, "/", language, "/", "search-result-page?max-num-filters=1&q=00263850"))
  
if (!is.na(page_json[1])) {
    
# subset to price data.frame
page_json = page_json$searchResultPage$products$main$items$product$salesPrice[1,]
    
# in some countries billies might have a different product number, so we just search for "billy" and pick first entry. probably error-prone
# https://www.ikea.com/ph/en/search/products/?q=00263850
# https://www.ikea.com/ph/en/search/products/?q=billy
  
  if (length(page_json) == 0) {
      page_json = possibly_fromJSON(str_c("https://sik.search.blue.cdtapps.com/", country, "/", language, "/", "search-result-page?max-num-filters=1&q=billy"))
      page_json = page_json$searchResultPage$products$main$items$product$salesPrice$numeral[1]
    
    # extract price
    page_price = page_json$numeral %>%
      str_remove_all(" ") %>%
      as.numeric()
    
    # extract currency from prefix or suffix
    if (page_json$currencyCode != "") {
      page_currency = page_json$currencyCode %>% str_trim()
    } else {
      page_currency = page_json$currencyCode %>% str_trim()
    }
  }
  # save price and currency to regions data.frame
cat("price: ", page_price, "- currency: ", page_currency, "\n")
regions[i, price := page_price]
regions[i, currency := page_currency]
  
# USE HTML
i = regions[isoCode3 == "DOM" & languageCode == "EN", which = TRUE]

# iterate over localized sites ----
# for (i in 1:nrow(regions)) {

cat(i, "-", regions[i]$url, "\n")

# set country, language and file path
country = regions[i]$code %>% str_to_lower()
language = regions[i]$languageCode %>% str_to_lower()

possibly_fromJSON(str_c("https://sik.search.blue.cdtapps.com/", country, "/", language, "/", "search-result-page?max-num-filters=1&q=00263850"))
possibly_fromJSON(str_c("https://sik.search.blue.cdtapps.com/", country, "/", language, "/", "search-result-page?max-num-filters=1&q=billy"))
# set country, language and file path
country = regions[i]$code %>% str_to_lower()
language = regions[i]$languageCode %>% str_to_lower()
file_path = str_c("temp/", country, "-", language, ".html")

# download html if necessary, then read
if (!file.exists(file_path)) {
  download.file(str_c(regions[i]$url, "search/products/?q=00263850"), destfile = file_path)
}
page_search = read_html(file_path)

# extract html element with price and currency info
page_element = page_search %>%
  possibly_html_element("#art_00263850 > div > div.card-body > div > div.itemPriceBox > div > p > span")
if (!is.na(page_element)) {
  
  page_price = page_element %>% html_attr("data-price") %>% as.numeric()
  page_currency = page_element %>% html_children() %>% html_text()
  
}




# get exchange rates ----
data_xr = read_html("https://www.ecb.europa.eu/stats/policy_and_exchange_rates/euro_reference_exchange_rates/html/index.en.html")
data_xr = data_xr %>%
  html_node("#main-wrapper > main > div.jumbo-box > div.lower > div > div > table") %>%
  html_table()
setDT(data_xr)

# first two digits of currency code are equal to iso3 codes, good for merging
data_xr = data_xr[, .(currency=Currency, spot = Spot)]

# Recode suffix into currency code
regions[currency == "$", currency := "USD"]
regions[currency == "RD$", currency := "DOP"]

# merge data ----
data = merge(regions,
             data_xr,
             by = "currency",
             all.x = T)

# within euro exchange rate is 1
data[currency == "EUR", spot := 1]

# compute relative prices to USD
data[, price := as.numeric(price)]
data[, price_rel := price / data[url == "https://www.ikea.com/de/de/", price]]

# plots ----

## scatter plot xr vs price ----
plot_data = data[!is.na(price_rel) & !is.na(spot), .(code, price_rel, spot)] %>% unique()
plot_data[, country := countrycode(code, "iso2c", "country.name")]
plot_data[, ratio := price_rel / spot]

plot = ggplot(plot_data) +
  theme_minimal() +
  geom_point(aes(x = price_rel, y = spot, color = ratio)) +
  geom_text_repel(aes(x = price_rel, y = spot, label = country)) +
  geom_abline(slope = 1, intercept = 0) +
  scale_color_viridis_c() +
  scale_x_log10("Relative price") +
  scale_y_log10("Exchange rate")

ggsave(plot,
       filename = str_c(wdir, "output/relative_billy_prices.png"),
       width = 20,
       height = 20,
       units = "cm")


dev.off()


## histogram ----
plot = ggplot(plot_data) +
  theme_minimal() +
  geom_bar(aes(x = reorder(country, ratio), y = ratio, fill = ratio),
           stat = "identity", position = "dodge") +
  scale_fill_viridis_c() +
  xlab("Country") +
  scale_y_continuous("Ratio") +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(plot,
       filename = str_c(wdir, "output/ratio_prices.png"),
       width = 20,
       height = 20,
       units = "cm")
