#' Return decisions from the Croatian Constitutional Court
#'
#' The function extracts decisions from the Croatian Constitutional Court archive \url{https://sljeme.usud.hr}
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
#' @importFrom stringi stri_extract stri_extract_all
#' @importFrom rvest read_html html_text html_nodes
#' @importFrom RSelenium rsDriver
#' @importFrom xml2 read_html
#'
#' @keywords constitutional court, scraping
#'
#' @examples
#' out <- get_decisions_hrv(date_start = "2019-06-01",
#' date_end = Sys.Date())
#' @export

get_decisions_hrv <- function(date_start=(Sys.Date()-1),
                          date_end=Sys.Date(),
                          textdata = TRUE,
                          metadata = TRUE,
                          browser = "chrome",
                          chromever = "83.0.4103.39"){

  date_start <- format(as.Date(date_start), "%d.%m.%Y")
  date_end <- format(as.Date(date_end), "%d.%m.%Y")

  rD <- rsDriver(port = 9517L, browser = c(browser),chromever=chromever)
  remDr <- rD[["client"]]

  remDr$navigate("https://sljeme.usud.hr/usud/praksaw.nsf/fSearchNew.xsp")
  remDr$findElement(using = "css", '#view\\:_id1\\:_id2\\:callbackContent\\:ebDatumOdlOd')$sendKeysToElement(list(date_start))
  remDr$findElement(using = "css", '#view\\:_id1\\:_id2\\:callbackContent\\:ebDatumOdlDo')$sendKeysToElement(list(date_end))
  remDr$findElement(using = "css", "#view\\:_id1\\:_id2\\:callbackContent\\:btnPretrazi")$clickElement()
  Sys.sleep(10)
  if (is.na(stri_extract(unlist(remDr$getPageSource()), regex = 'XSP.attachPartial[[:graph:]]{10,500}NextImage')) == FALSE) {
    remDr$findElement(using = "css", '#view\\:_id1\\:_id2\\:callbackContent\\:pager1__LastImage__img img')$clickElement()
    Sys.sleep(5)
    number_pages <- stri_extract(stri_extract(unlist(remDr$getPageSource()[[1]]), regex = "xspCurrentItem\">[[:digit:]]{1,4}</span>"), regex = "[[:digit:]]{1,4}")
    remDr$findElement(using = "css", '#view\\:_id1\\:_id2\\:callbackContent\\:pager1__FirstImage__img img')$clickElement()
    Sys.sleep(2)
  } else {
    number_pages <- 1
  }

  if (number_pages == 1) {
    url <- unlist(remDr$getPageSource())
    urls <- stri_extract_all(url, regex = 'link1\" href=\"[[:graph:]]{1,500}\"') %>% unlist() %>% gsub('link1\" href=\"', '', .) %>% gsub('\"', '', .)
    Sys.sleep(0.5)
  } else {
    urls <- lapply(1:(as.numeric(number_pages)-1), function(p) {
      url <- unlist(remDr$getPageSource())
      url <- stri_extract_all(url, regex = 'link1\" href=\"[[:graph:]]{1,500}\"') %>% unlist() %>% gsub('link1\" href=\"', '', .) %>% gsub('\"', '', .)
      if (length(remDr$findElements(using = "css", '#view\\:_id1\\:_id2\\:callbackContent\\:pager1__NextImage__img img'))!=0) {
        remDr$findElement(using = "css", '#view\\:_id1\\:_id2\\:callbackContent\\:pager1__NextImage__img img')$clickElement()
      }
      Sys.sleep(3)
      urls2 <- data.frame(url = url)
    }) %>% unlist()
    urls <- as.character(urls)
    url <- unlist(remDr$getPageSource())
    url <- stri_extract_all(url, regex = 'link1\" href=\"[[:graph:]]{1,500}\"') %>% unlist() %>% gsub('link1\" href=\"', '', .) %>% gsub('\"', '', .)
    urls <- c(urls, url)
  }
  urls <- as.character(urls) %>% gsub("http", "https", .) %>% gsub("&amp;", "&", .)
  remDr$close()

  if (metadata == TRUE) {
    meta <- lapply(1:length(urls), function(m){
      url_active <- read_html(urls[m])
      case <- html_text(html_nodes(url_active, css = '#view\\:_id1\\:_id2\\:callbackContent\\:cfSignatura'))
      date <- as.Date(html_text(html_nodes(url_active, css = '#view\\:_id1\\:_id2\\:callbackContent\\:cfDatum')), format = "%d. %m. %Y")
      doclang <- "HR"
      country <- "HRV"
      meta <- data.frame(case = case, date = date, doclang = doclang, country = country)
    })
    meta <- ldply(meta, data.frame)
  }

  if (textdata == TRUE) {
    texts <- lapply(seq_along(urls),function(u){
      url_active <- read_html(urls[u])
      text <- html_text(html_nodes(url_active, css = '.xspInputFieldRichText')) %>% gsub("\t\t", " ", .)
      if (length(text) == 0) {
        url_active <- do.call(paste, c(as.list(readLines(urls[u])), sep = " "))
        urlpdf <- stri_extract(url_active, regex = '"[[:graph:]]{1,500}.pdf"')  %>% gsub('"', '', .)
        Sys.sleep(0.1)
        text <- readtext(urlpdf) %>% unlist() %>% gsub("\r\n", "", .) %>% gsub('\n', ' ', .)
        text <- as(text[2], "character")
      }
      if (length(text) == 0) { text <- NA }
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

} %>% ldply



