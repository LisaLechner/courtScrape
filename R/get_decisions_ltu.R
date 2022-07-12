#' Return decisions from the Lithuanian Constitutional Court
#'
#' The function extracts decisions from the Lithuanian Constitutional Court archive \url{https://www.lrkt.lt}
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
#' @importFrom rvest read_html html_text html_nodes html_attr
#' @importFrom lubridate
#'
#' @keywords constitutional court, scraping
#'
#' @examples
#' out <- get_decisions_ltu(date_start = "2019-06-01",
#' date_end = Sys.Date())
#' @export


get_decisions_ltu <- function(date_start=(Sys.Date()-1),
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

  if(year_end == "2020") {
    year_interval <- gsub("2020", "2020/p0", as.character(year_interval))
    year_interval <- c(year_interval, "2020/p100")
  }

  urls <- lapply(1:length(year_interval), function(x){
    url_html <- read_html(paste0("https://www.lrkt.lt/lt/teismo-aktai/nutarimai-isvados-ir-sprendimai/138/y", year_interval[x]))
    node <- html_nodes(url_html, css = '.second_title a')
    urls <- html_attr(node, "href")
  }) %>% unlist()
  urls <- paste0("https://www.lrkt.lt", urls)

  if (metadata == TRUE) {
    meta <- lapply(1:length(year_interval), function(m){
      url_active <- read_html(paste0("https://www.lrkt.lt/lt/teismo-aktai/nutarimai-isvados-ir-sprendimai/138/y", year_interval[m]))
      case <- html_text(html_nodes(url_active, css = '.title')) %>% gsub("[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2} sprendimas, bylos[[:blank:]]{1,2}", "", .) %>% gsub("[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2} nutarimas, bylos", "", .)
      date <- as.Date(html_text(html_nodes(url_active, css = '.date_title')) %>% gsub(" sprendimas", "", .) %>% gsub(" nutarimas", "", .))
      doclang <- "LT"
      country <- "LTU"
      Sys.sleep(0.1)
      meta <- data.frame(case = case, date = date, doclang = doclang, country = country)
    })
    meta <- ldply(meta, data.frame)
  }


  if (textdata == TRUE) {
    texts <- lapply(1:length(urls), function(u) {
      url_active <- read_html(urls[u])
      text <- html_text(html_nodes(url_active, css = '.teismoaktai_content')) %>% gsub("\r\n", "", .) %>% gsub("\n", "", .)
      Sys.sleep(0.1)
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


