#' Return decisions from the German Constitutional Court
#'
#' The function extracts decisions from the German Constitutional Court archive \url{https://www.bundesverfassungsgericht.de}
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
#' @importFrom stringi stri_extract
#' @importFrom rvest read_html html_node html_attr html_nodes html_text
#'
#' @keywords constitutional court, scraping
#'
#' @examples
#' out <- get_decisions_deu(date_start = "2019-06-01",
#' date_end = Sys.Date())
#' @export

get_decisions_ger <- function (date_start = "2019-01-01",
                           date_end=Sys.Date(),
                           metadata = TRUE,
                           textdata = TRUE) {

  date_start <- format(as.Date(date_start), "%d.%m.%Y")
  date_end <- format(as.Date(date_end), "%d.%m.%Y")

  url <- paste0("https://www.bundesverfassungsgericht.de/SiteGlobals/Forms/Suche/Entscheidungensuche_Formular.html?input_=5399828&gtp=5403124_list%253D1&facettedEntscheidungstyp.GROUP=1&submit=Senden&resourceId=5399864&facettedVerfahrensart.GROUP=1&dateAfter=", date_start, "&dateBefore=", date_end, "&pageLocale=de")
  ht <- read_html(url)
  page_nr <- html_node(ht,'.singleview~ h2+ p') %>% html_text() %>% gsub("[[:punct:]]","",.)
  page_nr <- as.numeric(stri_extract(page_nr, regex = "[[:digit:]]{1,}$")) / as.numeric(gsub("bis ","",stri_extract(page_nr, regex = "bis [[:digit:]]{1,}")))
  page_nr <- ceiling(page_nr)
  urls <- lapply(1:page_nr, function(p){
    url_html <- read_html(paste0("https://www.bundesverfassungsgericht.de/SiteGlobals/Forms/Suche/Entscheidungensuche_Formular.html?input_=5399828&gtp=5403124_list%253D", p, "&facettedEntscheidungstyp.GROUP=1&submit=Senden&resourceId=5399864&facettedVerfahrensart.GROUP=1&dateAfter=", date_start, "&dateBefore=", date_end, "&pageLocale=de"))
    node <- html_nodes(url_html, '.relevance100+ a')
    urls <- html_attr(node, "href")
  }) %>% unlist
  urls <- paste0("https://www.bundesverfassungsgericht.de/",urls)

  if (textdata == TRUE) {
    texts <- lapply(seq_along(urls),function(u){
      url_active <- read_html(urls[u])
      xpath <- '//*[(@id = "wrapperContent")]'
      node <- html_nodes(url_active, xpath = xpath)
      text <- html_text(node)
      Sys.sleep(0.1)
      text <- gsub("\n", "", text)
      return(text)
    }) %>% unlist
  }

  if (metadata == TRUE) {
    meta <- lapply(1:length(urls),function(m){
      url_active <- read_html(urls[m])
      xpath <- '//*[(@id = "navBreadcrumbs")]//strong'
      node <- html_nodes(url_active, xpath = xpath)
      meta_text <- html_text(node)
      #case
      case <- stri_extract(meta_text, regex = "[[:digit:]]{1} [[:alpha:]]{3} [[:digit:]]{1,5}/[[:digit:]]{2}")
      if (length(case) == 0) { case <- NA }
      #ecli
      xpath <- '//*[contains(concat( " ", @class, " " ), concat( " ", "ecli", " " ))]'
      node <- html_nodes(url_active, xpath = xpath)
      ecli_id <- html_text(node)
      if (length(ecli_id) == 0) { ecli_id <- NA }
      #date
      date <- stri_extract(meta_text, regex = "[[:digit:]]{2}. [[:alpha:]]{3,30} [[:digit:]]{4}")
      if (length(date) == 0) {
        date <- NA
      } else {
        if (is.na(date) == TRUE) {
         date <- stri_extract(meta_text, regex = "[[:digit:]]{2} [[:alpha:]]{3,30} [[:digit:]]{4}")
        }
        day <- stri_extract(date, regex = "[[:digit:]]{2}")
        month <- stri_extract(date, regex = "[[:alpha:]]{3,20}")
        if (month == "Januar" | month == "January" | month == "janvier" | month == "enero") {
          month <- "01"
        } else if (month == "Februar" | month == "February" | month == "f?vrier" | month == "febrero") {
          month <- "02"
        } else if (month == "M?rz" | month == "March" | month == "mars" | month == "marzo") {
          month <- "03"
        } else if (month == "April" | month == "avril" | month == "abril") {
          month <- "04"
        } else if (month == "Mai" | month == "May" | month == "mai" | month == "mayo") {
          month <- "05"
        } else if (month == "Juni" | month == "June" | month == "juin" | month == "junio") {
          month <- "06"
        } else if (month == "Juli" | month == "July" | month == "juillet" | month == "julio") {
          month <- "07"
        } else if (month == "August" | month == "ao?t" | month == "agosto") {
          month <- "08"
        } else if (month == "September" | month == "septembre" | month == "septiembre") {
          month <- "09"
        } else if (month == "Oktober" | month == "October" | month == "octobre" | month == "octubre") {
          month <- "10"
        } else if (month == "November" | month == "novembre" | month == "noviembre") {
          month <- "11"
        } else if (month == "Dezember" | month == "December" | month == "d?cembre" | month == "diciembre") {
          month <- "12"
        }
        year <- stri_extract(date, regex = "[[:digit:]]{4}")
        date <- paste0(year, "-", month, "-", day)
      }
      #doclang
      if (is.na(stri_extract(meta_text, regex = " of ")) == FALSE) {
        doclang <- "EN"
      } else if (is.na(stri_extract(meta_text, regex = "janvier|f?vrier|fevrier|mars|avril|mai|juin|juillet|ao?t|aout|septembre|octobre|novembre|d?cembre|decembre")) == FALSE) {
        doclang <- "FR"
      } else if (is.na(stri_extract(meta_text, regex = "enero|febrero|marzo|abril|mayo|junio|julio|agosto|septiembre|octubre|noviembre|diciembre")) == FALSE) {
        doclang <- "ES"
      } else {
        doclang <- "DE"
      }
      country <- "GER"
      meta = data.frame(case = case, ecli_id = ecli_id, date = date, doclang = doclang, country = country)
    })
    meta = ldply(meta, data.frame)
  }

  if (textdata == FALSE & metadata == TRUE) {
    output <- data.frame(meta, url <- urls)
  }

  if (textdata == TRUE & metadata == FALSE) {
    output <- data.frame(text <- texts, url <- urls)
  }

  if (textdata == TRUE & metadata == TRUE) {
    output <- data.frame(meta,text <- texts, url <- urls)
  }

  if (textdata == FALSE & metadata == FALSE) {
    output <- data.frame(url <- urls)
  }

  return(output)
}
