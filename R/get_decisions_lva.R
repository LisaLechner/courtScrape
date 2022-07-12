#' Return decisions from the Latvian Constitutional Court
#'
#' The function extracts decisions from the Latvian Constitutional Court archive \url{http://www.satv.tiesa.gov.lv}
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
#' @importFrom stringi stri_extract stri_detect
#' @importFrom rvest read_html html_text html_nodes html_attr
#' @importFrom RSelenium rsDriver
#'
#' @keywords constitutional court, scraping
#'
#' @examples
#' out <- get_decisions_lva(date_start = "2019-06-01",
#' date_end = Sys.Date())
#' @export

get_decisions_lva <- function (date_start = "2019-01-01",
                           date_end=Sys.Date(),
                           metadata = TRUE,
                           textdata = TRUE) {

  year_start <- stri_extract(date_start, regex = "[[:digit:]]{4}")
  year_end <- stri_extract(date_end, regex = "[[:digit:]]{4}")

  if(year_start != year_end) {
    year_interval <- (as.numeric(year_start) + 1):year_end
    year_interval <- paste0(",", year_interval)
    url_brick <- paste0(year_start, paste(year_interval, collapse = ""))
  } else {
    url_brick <- year_start
  }

  url <- paste0("http://www.satv.tiesa.gov.lv/cases/?case-filter-years=[", url_brick, "]&case-filter-status=[42]&case-filter-types=&case-filter-result=&searchtext=&page=1")
  ht <- read_html(url)
  xpath_page <- "//li[(((count(preceding-sibling::*) + 1) = 8) and parent::*)]//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'page-numbers', ' ' ))]"
  page_nr <- ceiling(as.numeric(html_nodes(ht,'.found-results') %>% html_text() %>% stri_extract(., regex = "[[:digit:].]{1,}")) / 20)
  urls <- lapply(1:page_nr,function(p){
    urls <- paste0("http://www.satv.tiesa.gov.lv/cases/?case-filter-years=[", url_brick, "]&case-filter-status=[42]&case-filter-types=&case-filter-result=&searchtext=&page=",p)
    ht2 <- read_html(urls)
    urls2 <- lapply(1:20,function(x){
      xpath <- paste0('//*[@id="search-results"]/div[2]/div/div[',x,']/div[4]/div[1]/a')
      nodes <- html_nodes(ht2,xpath=xpath)
      urls3 <- html_attr(nodes,"href")
    }) %>% unlist
  }) %>% unlist
  urls <- paste0("http://www.satv.tiesa.gov.lv", urls)

  if (metadata == TRUE) {
    url_meta <- paste0(url, "&print=1")
    ht <- read_html(url_meta)
    # case
    xpath <- '//*[contains(concat( " ", @class, " " ), concat( " ", "head", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "first", " " ))]'
    node <- html_nodes(ht, xpath = xpath)
    case <- unique(html_text(node))
    #date
    xpath <- '//*[contains(concat( " ", @class, " " ), concat( " ", "block", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "second", " " ))]'
    node <- html_nodes(ht, xpath = xpath)

    node <- html_nodes(ht, '.third date')
    date <- html_text(node)
    date <- gsub("\\.$","",date)
    date <- gsub("^-$","",date)
    #date <- stri_extract(date, regex = "[[:digit:].]{2,}.[[:digit:].]{2,}.[[:digit:]]{4,}")
   # date <- paste0(stri_extract(date, regex = "[[:digit:]]{4,}"), "-", stri_extract(stri_extract(date, regex = ".[[:digit:]]{2,}."), regex = "[[:digit:]]{2,}"), "-", stri_extract(date, regex = "[[:digit:]]{2,}"))
    #ecli ???
    #doclang
    doclang <- lapply(1:length(case),function(l){
      doclang <- "LV"
    }) %>% unlist ()
    #country
    country <- lapply(1:length(case),function(c){
      country <- "LV"
    }) %>% unlist ()

    meta <- data.frame(case = case, date = date, doclang = doclang, country = country)
    meta <- ldply(meta, data.frame)
  }

  if (textdata == TRUE) {

    eCaps <- list(
      chromeOptions =
        list(prefs = list(
          "profile.default_content_settings.popups" = 0L,
          "download.prompt_for_download" = FALSE,
          "download.default_directory" = tempdir()
        )))


    texts <- lapply(seq_along(urls),function(i){

      rD <- rsDriver(port = 9517L, browser = c("chrome"),extraCapabilities = eCaps,chromever="78.0.3904.70")
      remDr <- rD[["client"]]


      url <- urls[i]
      remDr$navigate(url)
      Sys.sleep(3)
      txt <- remDr$findElement(using='css selector','#download')
      txt$clickElement()
      Sys.sleep(1)
      rm(txt)

      if(!stri_detect(regex="\\.pdf",list.files(tempdir())[1]) ){
        text <- data.frame(doc_id=NA,text=NA)
      }

      if(stri_detect(regex="\\.pdf",list.files(tempdir())[1]) ){
        text <- readtext::readtext(paste0(tempdir(),"/",list.files(tempdir())[1]))
        unlink(list.files(tempdir(),full.names = TRUE)[1])
      }

      rm(rD)
      rm(remDr)
      gc()

      return(text)

    }) %>% ldply()
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
