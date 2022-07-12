#' Return decisions from the Czech Constitutional Court
#'
#' The function extracts decisions from the Czech Constitutional Court archive \url{http://nalus.usoud.cz}
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
#' @importFrom methods as
#' @importFrom stringi stri_extract stri_extract_all
#' @importfrom RSelenium rsDriver
#' @importFrom XML htmlParse
#'
#' @keywords constitutional court, scraping
#'
#' @examples
#' out <- get_decisions_cze(date_start = "2019-06-01",
#' date_end = Sys.Date())
#' @export

get_decisions_cze <- function(date_start=(Sys.Date()-1),
                          date_end=Sys.Date(),
                          textdata = TRUE,
                          metadata = TRUE,
                          browser = "chrome",
                          chromever = "83.0.4103.39"){

  date_start <- format(as.Date(date_start), "%d.%m.%Y")
  date_end <- format(as.Date(date_end), "%d.%m.%Y")

  rD <- rsDriver(port = 9517L, browser = c(browser),chromever=chromever)
  remDr <- rD[["client"]]

  remDr$navigate("http://nalus.usoud.cz/Search/Search.aspx")
  remDr$findElement(using = "css", "#ctl00_MainContent_decidedFrom")$sendKeysToElement(list(date_start))
  remDr$findElement(using = "css", "#ctl00_MainContent_decidedTo")$sendKeysToElement(list(date_end))
  remDr$findElement(using = "css", "#ctl00_MainContent_resultsPageSize")$clickElement()
  remDr$findElement(using = "css", "#ctl00_MainContent_resultsPageSize")$sendKeysToElement(list("80"))
  remDr$findElement(using = "css", "#ctl00_MainContent_but_search")$clickElement()
  Sys.sleep(2)

  number_pages <- stri_extract(stri_extract(stri_extract(as(htmlParse(remDr$getPageSource()[[1]]), "character"), regex = '[[:print:]]{50}Dal'), regex = '>[[:digit:]]{1,5}<'), regex = '[[:digit:]]{1,5}') %>% unlist()
  if(is.na(number_pages) == TRUE) { number_pages <- 1 }

  meta <- lapply(1:number_pages, function(p) {
    remDr$navigate(paste0("http://nalus.usoud.cz/Search/Results.aspx?page=", p-1))
    Sys.sleep(2)
    urls <- unlist(remDr$getPageSource()[[1]])
    ecli_id <- stri_extract_all(urls, regex = "ECLI:[[:graph:]]{1,25}") %>% unlist() %>% gsub("<br", "", .) %>% gsub(">", "", .)
    id <- stri_extract_all(stri_extract_all(urls, regex = 'Detail.aspx[[:punct:]]{1}id=[[:digit:]]{2,6}'), regex = '[[:digit:]]{2,6}') %>% unlist()
    case <- stri_extract_all(urls, regex = "typ=result\" class=\"resultData[[:digit:]]\">(.*?)</a>") %>%
      unlist () %>%
      gsub("typ=result\" class=\"resultData[[:digit:]]\">", "", .) %>%
      gsub("</a>", "", .)
    date <- as.Date(unlist(stri_extract_all(stri_extract_all(urls, regex = "rowspan=\"2\"><b>[[:digit:]]{1,2}. [[:digit:]]{1,2}. [[:digit:]]{4}"), regex = "[[:digit:]]{1,2}. [[:digit:]]{1,2}. [[:digit:]]{4}")), format = "%d. %m. %Y")
    citation <- unlist(stri_extract_all(urls, regex = "[[:digit:]]{1,2}. [[:digit:]]{1,2}. [[:digit:]]{4}</td>\n<td class=\"resultData[[:digit:]]{1}\" rowspan=\"2\">(.*?)</td>")) %>%
      gsub(".*rowspan=\"2\">", "", .) %>%
      gsub("</td>.*", "", .) %>%
      gsub("<br>", " ", .) %>%
      gsub("\n", "", .)
    c <- length(id)
    doclang <- "CZ"
    doclang[1:c] <- "CZ"
    country <- "CZE"
    country[1:c] <- "CZE"
    meta <- data.frame(case = case, id = id, ecli_id = ecli_id, date = date, citation = citation, doclang = doclang, country = country)
  })
  meta <- ldply(meta, data.frame)


  if (textdata == TRUE) {
    texts <- lapply(1:length(meta$id), function(m){
      remDr$navigate(paste0("https://nalus.usoud.cz/Search/Word.aspx?print=0&id=", meta$id[m]))
      Sys.sleep(4)
      remDr$navigate(paste0("https://nalus.usoud.cz/Search/Word.aspx?print=0&id=", meta$id[m]))
      text <- as(unlist(remDr$getPageSource()[[1]]), "character")
      text <- gsub(".*DocCZHeader\">", "", text) %>%
        gsub("><script type.*", "", .) %>%
        gsub("<(.*?)>", "", .) %>%
        gsub("&nbsp;", "", .) %>%
        gsub("\n", "", .)
    }) %>% unlist()
  }

  remDr$close()

  if(textdata==FALSE & metadata==TRUE){
    temp <- data.frame(meta, url = urls)
  }

  if(textdata==TRUE & metadata==TRUE){
    temp <- data.frame(meta, text = texts, url = paste0("https://nalus.usoud.cz/Search/Word.aspx?print=0&id=", meta$id))
  }

  if(textdata==FALSE & metadata==FALSE){
    temp <- data.frame(url = urls)
  }
  if(textdata==TRUE & metadata==FALSE){
    temp <- data.frame(text = texts, url = urls)
  }
  return(temp)

} %>% ldply


