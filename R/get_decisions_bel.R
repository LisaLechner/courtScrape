#' Return decisions from the Belgium Constitutional Court
#'
#' The function extracts decisions from the Belgium Constitutional Court archive \url{https://www.const-court.be}
#'
#' @param date_start an object of class \code{character} or \code{date} with the format yyyy-mm-dd indicating from
#' which date on data should be retrieved.
#' @param date_end an object of class \code{character} or \code{date} with the format yyyy-mm-dd indicating the date until
#' data should be retrieved.
#' @param metadata a logical object indicating whether metadata such as case ID, decision date, etc. should be retrieved or not. The default is \code{TRUE}.
#' @param textdata a logical object indicating whether the fulltexts of the court decisions should be retrieved or not. The default is \code{TRUE}.
#'
#' @return an object of class \code{data.frame} which contains the downloaded decisions and
#' if required the \code{metadata} and \code{textdata}
#'
#' @importFrom dplyr full_join
#' @importFrom readtext
#' @importFrom methods as
#' @importFrom stringi stri_extract
#' @importFrom rvest html_nodes read_html html_attr html_text
#'
#' @keywords constitutional court, scraping
#'
#' @examples
#' out <- get_decisions_bel(date_start = "2019-06-01",
#' date_end = Sys.Date())
#'
#' @export

get_decisions_bel <- function (date_start = "2019-06-01",
                           date_end=Sys.Date(),
                           metadata = TRUE,
                           textdata = TRUE) {

  year_start <- stri_extract(as.character(date_start), regex = "[[:digit:]]{4}")
  year_end <- stri_extract(as.character(date_end), regex = "[[:digit:]]{4}")

  if(year_start != year_end) {
    year_interval <- (as.numeric(year_start)):year_end
  } else {
    year_interval <- year_start
  }

# GERMAN
  year_interval_d <- year_interval[ year_interval > 1988 & year_interval != 1990 ]

  if (length(year_interval_d != 0)) {
    urls <- lapply(1:length(year_interval_d), function(x){
      url_html <- read_html(paste0("https://www.const-court.be/public/d/", year_interval_d[x]))
      node <- html_nodes(url_html, css = "a")
      url <- html_attr(node, "href")
      url <- url[2:length(url)]
      url <- paste0("https://www.const-court.be/public/d/", year_interval_d[x], "/", url)
    }) %>% unlist()

    text <- lapply(1:length(urls), function(u) {
      urlpdf <- urls[u]
      text <- readtext(urlpdf)  %>% unlist()
      text <- as(text[2], "character")
    }) %>% unlist()

    date <- stri_extract(text, regex = "vom [[:digit:]]{1,2}[[:punct:]]{0,1} [[:alpha:]]{3,20} [[:digit:]]{4}[[:graph:]]{0,20}\r\n") %>% gsub("decembre", "d?cembre", .) %>% gsub("[[:punct:]]{1}", "", .) %>% gsub("vom ", "", .) %>% gsub("\r\n", "", .)
    Sys.setlocale("LC_ALL","German")
    date <- as.Date(date, format = "%d %B %Y")
    case <- stri_extract(urls, regex = "[[:digit:]]{1,3}d.pdf") %>% gsub("d.pdf", "", .)
    case <- paste0("Urteil Nr. ", case, "/", format(date, format = "%y"))
    doclang <- "DE"
    doclang[1:length(case)] <- "DE"
    country <- "BEL"
    country[1:length(case)] <- "BEL"
    meta_d <- data.frame(case = case, date = date, text = text, doclang = doclang, country = country, url = urls)
  }

#FRENCH
  year_interval_f <- year_interval[ year_interval > 1984 ]

  if (length(year_interval_f != 0)) {
    urls <- lapply(1:length(year_interval_f), function(x){
      url_html <- read_html(paste0("https://www.const-court.be/public/f/", year_interval_f[x]))
      node <- html_nodes(url_html, css = "a")
      url <- html_attr(node, "href")
      url <- url[2:length(url)]
      url <- paste0("https://www.const-court.be/public/f/", year_interval_f[x], "/", url)
    }) %>% unlist()

    text <- lapply(1:length(urls), function(u) {
      urlpdf <- urls[u]
      text <- readtext(urlpdf)  %>% unlist()
      text <- as(text[2], "character")
    }) %>% unlist()

    date <- stri_extract(text, regex = "du [[:digit:]]{1,2} [[:alpha:]]{3,20} [[:digit:]]{4}[[:graph:]]{0,20}\r\n") %>% gsub("decembre", "d?cembre", .) %>% gsub("[[:punct:]]{1}", "", .) %>% gsub("du ", "", .) %>% gsub("\r\n", "", .)
    Sys.setlocale("LC_TIME", "French")
    date <- as.Date(date, format = "%d %B %Y")
    case <- stri_extract(urls, regex = "[[:digit:]]{1,3}f.pdf") %>% gsub("f.pdf", "", .)
    case <- paste0("Arr?t n? ", case, "/", format(date, format = "%y"))
    doclang <- "FR"
    doclang[1:length(case)] <- "FR"
    country <- "BEL"
    country[1:length(case)] <- "BEL"
    meta_f <- data.frame(case = case, date = date, text = text, doclang = doclang, country = country, url = urls)
  }

#DUTCH
  year_interval_n <- year_interval[ year_interval > 1984 ]

  if (length(year_interval_n != 0)) {
    urls <- lapply(1:length(year_interval_n), function(x){
      url_html <- read_html(paste0("https://www.const-court.be/public/n/", year_interval_n[x]))
      node <- html_nodes(url_html, css = "a")
      url <- html_attr(node, "href")
      url <- url[2:length(url)]
      url <- paste0("https://www.const-court.be/public/n/", year_interval_n[x], "/", url)
    }) %>% unlist()

    text <- lapply(1:length(urls), function(u) {
      urlpdf <- urls[u]
      text <- readtext(urlpdf)  %>% unlist()
      text <- as(text[2], "character")
    }) %>% unlist()

    date <- stri_extract(text, regex = "van [[:digit:]]{1,2} [[:alpha:]]{3,20} [[:digit:]]{4}[[:graph:]]{0,20}\r\n") %>% gsub("decembre", "d?cembre", .) %>% gsub("[[:punct:]]{1}", "", .) %>% gsub("van ", "", .) %>% gsub("\r\n", "", .)
    Sys.setlocale("LC_TIME", "Dutch")
    date <- as.Date(date, format = "%d %B %Y")
    case <- stri_extract(urls, regex = "[[:digit:]]{1,3}n.pdf") %>% gsub("n.pdf", "", .)
    case <- paste0("Arrest nr. ", case, "/", format(date, format = "%y"))
    doclang <- "NL"
    doclang[1:length(case)] <- "NL"
    country <- "BEL"
    country[1:length(case)] <- "BEL"
    meta_n <- data.frame(case = case, date = date, text = text, doclang = doclang, country = country, url = urls)
  }

  if ((exists('meta_d') && is.data.frame(get('meta_d'))) == TRUE) {
    data <- full_join(meta_f, meta_n)
    data <- full_join(data, meta_d)
  } else if ((exists('meta_n') && is.data.frame(get('meta_n'))) == TRUE) {
    data <- full_join(meta_f, meta_n)
  } else {
    data <- meta_f
  }
  data <- subset(data, date > as.Date(date_start) & date < as.Date(date_end))
  data$case <- as.character(data$case)
  data$text <- as.character(data$text)
  data$doclang <- as.character(data$doclang)
  data$country <- as.character(data$country)
  data$url <- as.character(data$url)
  return(data)
}
