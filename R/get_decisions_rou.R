#' Return decisions from the Romanian Constitutional Court
#'
#' The function extracts decisions from the Romanian Constitutional Court archive \url{https://www.ccr.ro}
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
#' @importFrom dplyr
#' @importFrom plyr ldply
#' @importFrom readtext
#' @importFrom stringi stri_extract
#' @importFrom rvest read_html html_nodes html_attr
#' @importFrom utils download.file
#'
#' @keywords constitutional court, scraping
#'
#' @examples
#' out <- get_decisions_rou(date_start = "2019-06-01",
#' date_end = Sys.Date())
#' @export


get_decisions_rou <- function(date_start=(Sys.Date()-1),
                          date_end=Sys.Date(),
                          textdata = TRUE,
                          metadata = TRUE){

  year_start <- stri_extract(date_start, regex = "[[:digit:]]{4}")
  year_end <- stri_extract(date_end, regex = "[[:digit:]]{4}")

  if(year_start != year_end) {
    year_interval <- (as.numeric(year_start)):year_end
  } else {
  year_interval <- year_start
  }

  urls <- lapply(1:length(year_interval), function(x){
    url <- paste0("https://www.ccr.ro/jurisprudenta-decizii-de-admitere/?anul_postului=", year_interval[x])
    urls <- lapply(1:15, function(p) {
      url_html <- read_html(paste0(url, "&_page=", p))
      node <- html_nodes(url_html, '#post-40 .cvplbd')
      urls <- unique(html_attr(node, "href")) %>% gsub("#new_tab", "", .)
      Sys.sleep(0.5)
      return(urls)
    }) %>% unlist()
  }) %>% unlist()

  urls <- gsub("#newtab", "", urls)

  if (metadata == TRUE) {
    meta <- lapply(1:length(urls), function(m){
      url_active <- urls[m]
      download.file(url_active, destfile = "decision.pdf", quiet = T, mode = "wb")
      text <- readtext("decision.pdf")
      text <- text$text[1]
      file.remove("decision.pdf")
      Sys.sleep(1)
      case <- stri_extract(text, regex = 'DECIZIA Nr.[[:digit:]]{1,4}')
      date <- stri_extract(text, regex = '[[:digit:]]{1,2} [[:alpha:]]{2,12} [[:digit:]]{4}')
      Sys.setlocale("LC_TIME", "Romanian")
      date <- as.Date(date, format = "%d %B %Y")
      doclang <- "RO"
      country <- "ROU"
      meta <- data.frame(case = case, date = date, doclang = doclang, country = country)
    })
    meta <- ldply(meta, data.frame)
  }


  if (textdata == TRUE) {
    texts <- lapply(1:length(urls), function(t) {
      url_active <- urls[t]
      download.file(url_active, destfile = "decision.pdf", quiet = T, mode = "wb")
      text <- readtext("decision.pdf")
      text <- text$text[1]
      file.remove("decision.pdf")
      Sys.sleep(1)
      return(text)
    }) %>% unlist
  }

  if(textdata==FALSE & metadata==TRUE){
    temp <- data.frame(meta, url = urls)
  }

  if(textdata==TRUE & metadata==TRUE){
    temp <- data.frame(meta, text = texts, url = urls)
  }

  if(textdata==FALSE & metadata==FALSE){
    temp <- data.frame(url = urls)
  }
  if(textdata==TRUE & metadata==FALSE){
    temp <- data.frame(text = texts, url = urls)
  }
  return(temp)

  temp <- subset(temp, date > as.Date(date_start) & date < as.Date(date_end))

} %>% ldply
