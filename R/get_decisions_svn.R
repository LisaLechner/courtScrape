#' Return decisions from the Slovenian Constitutional Court
#'
#' The function extracts decisions from the Slovenian Constitutional Court archive \url{https://www.us-rs.si}
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
#' @importFrom plyr ldply
#' @importFrom stringi stri_extract
#' @importFrom rvest read_html html_text html_nodes html_attr
#'
#' @keywords constitutional court, scraping
#'
#' @examples
#' out <- get_decisions_svn(date_start = "2019-06-01",
#' date_end = Sys.Date())
#' @export

get_decisions_svn <- function(date_start="2019-06-01",
                          date_end=Sys.Date(),
                          textdata = TRUE,
                          metadata = TRUE){

  date_start <- format(strptime(date_start,"%Y-%m-%d"),"%d.%m.%Y")
  date_end <- format(strptime(date_end,"%Y-%m-%d"),"%d.%m.%Y")

  url <- paste0('https://www.us-rs.si/odlocitve/?q=&df=', date_start, '&dt=', date_end, '&af=&at=&vd=&vo=&vv=&vs=&ui=&va=&page=1&sort=date&order=asc#show-decision-results')
  ht <- read_html(url)
  node <- html_nodes(ht, '.mb-md-0+ p')
  page_nr <- ceiling(as.numeric(gsub('.*od ', '', html_text(node)))/20)
  urls <- lapply(1:page_nr, function(p){
    url_html <- read_html(paste0('https://www.us-rs.si/odlocitve/?q=&df=', date_start, '&dt=', date_end, '&af=&at=&vd=&vo=&vv=&vs=&ui=&va=&page=', p, '&sort=date&order=asc#show-decision-results'))
    node <- html_nodes(url_html, '.case-id .single-decision-link')
    urls <- html_attr(node, "href")
    urls <- paste0('https://www.us-rs.si/odlocitev/?', stri_extract(urls, regex = 'id=[[:digit:]]{5,6}'))
  }) %>% unlist

  if (metadata == TRUE) {
    meta <- lapply(seq_along(urls),function(a){
      url_active <- read_html(urls[a])
      case <- html_text(html_nodes(url_active, css = 'tr:nth-child(1) td+ td'))
      case <- case[1]
      if (length(case) == 0) { case <- NA }
      ecli_id <- html_text(html_nodes(url_active, css = 'tr:nth-child(3) td+ td'))
      ecli_id <- ecli_id[1]
      if (length(ecli_id) == 0) { ecli_id <- NA }
      date <- as.Date(stri_extract(html_text(html_nodes(url_active, css = 'tr:nth-child(2) td+ td')), regex = "[[:digit:]]{2}.[[:digit:]]{2}.[[:digit:]]{4}"), format = "%d.%m.%Y")
      date <- date[1]
      if (length(date) == 0) { date <- NA }
      doclang <- "SI"
      country <- "SVN"
      Sys.sleep(0.1)
      meta = data.frame(case = case, ecli_id = ecli_id, date = date, doclang = doclang, country = country)
    })
    meta = ldply(meta, data.frame)
  }

  if (textdata == TRUE) {
    texts <- lapply(seq_along(urls),function(u){
      url_active <- read_html(urls[u])
      text <- html_text(html_nodes(url_active, css = 'tr:nth-child(11) div'))
      if (length(text) == 0) { text <- NA }
      if (is.na(text) == T|nchar(text) < 200|is.na(stri_extract(text, regex = ".pdf")) == F) { text <- html_text(html_nodes(url_active, css = 'tr:nth-child(10) div')) }
      if (length(text) == 0) { text <- NA }
      if (is.na(text) == T|nchar(text) < 200|is.na(stri_extract(text, regex = ".pdf")) == F) { text <- html_text(html_nodes(url_active, css = 'tr:nth-child(8) div')) }
      if (length(text) == 0) { text <- NA }
      if (is.na(text) == T|nchar(text) < 200|is.na(stri_extract(text, regex = ".pdf")) == F) { text <- html_text(html_nodes(url_active, css = 'tr:nth-child(10) td+ td')) }
      if (length(text) == 0) { text <- NA }
      if (is.na(text) == T|nchar(text) < 200|is.na(stri_extract(text, regex = ".pdf")) == F) { text <- html_text(html_nodes(url_active, css = 'tr:nth-child(11) td+ td')) }
      if (length(text) == 0) { text <- NA }
      if (is.na(text) == T|nchar(text) < 200|is.na(stri_extract(text, regex = ".pdf")) == F) { text <- html_text(html_nodes(url_active, css = 'td > div')) }
      if (length(text) == 0) { text <- NA }

      text <- do.call(paste, c(as.list(text), sep = " "))

      if (
        is.na(stri_extract(text, regex = '.pdf                                            \r\n                                            \r\n                                                \r\n                                                Prenesi')) == F
      ) {
        text <- html_text(html_nodes(url_active, css = 'tr:nth-child(10) td+ td'))
        text <- do.call(paste, c(as.list(text), sep = " "))
      }

      if (length(text) == 0|nchar(text) < 200) { text <- NA }

      Sys.sleep(0.1)
      return(text)
    }) %>% unlist
  }


  if (textdata == FALSE & metadata == TRUE) {
    temp <- data.frame(meta, url = urls)
  }

  if (textdata == TRUE & metadata == FALSE) {
    temp <- data.frame(text = texts, url = urls)
  }

  if (textdata == TRUE & metadata == TRUE) {
    temp <- data.frame(meta, text = texts, url = urls)
  }

  if (textdata == FALSE & metadata == FALSE) {
    temp <- data.frame(url = urls)
  }

  return(temp)

}
