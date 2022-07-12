#' Return decisions from the Polish Constitutional Court
#'
#' The function extracts decisions from the Polish Constitutional Court archive \url{http://otkzu.trybunal.gov.pl}
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
#' @importFrom readtext
#' @importFrom methods as
#' @importFrom stringi stri_extract_all stri_extract
#' @importFrom rvest read_html html_text html_nodes html_attr
#' @importFrom RSelenium rsDriver
#' @importFrom utils download.file
#' @importFrom XML htmlParse
#'
#' @keywords constitutional court, scraping
#'
#' @examples
#' out <- get_decisions_pol(date_start = "2019-06-01",
#' date_end = Sys.Date())
#' @export

get_decisions_pol <- function(date_start=(Sys.Date()-1),
                          date_end=Sys.Date(),
                          browser = "firefox",
                          chromever = "89.0.4389.23",
                          textdata = TRUE,
                          metadata = TRUE){

  year_start <- stri_extract(as.character(date_start), regex = "[[:digit:]]{4}")
  year_end <- stri_extract(as.character(date_end), regex = "[[:digit:]]{4}")

  if(year_start != year_end) {
    year_interval <- (as.numeric(year_start)):year_end
  } else {
    year_interval <- as.numeric(year_start)
  }

  rD <- rsDriver(port = 9517L, browser = c(browser),chromever=chromever)
  remDr <- rD[["client"]]
  remDr$navigate("http://otkzu.trybunal.gov.pl/")
  urls <- lapply(seq_along(year_interval), function(y) {
    css_brick <- as.numeric(format(Sys.Date(), "%Y")) - year_interval[y]
    css <- paste0('#katalog\\:j_idt28\\:', css_brick, ' .ui-corner-all')
    remDr$findElement("css", css)$clickElement()
    Sys.sleep(3)
    page_nr <- ceiling(as.numeric(stri_extract(stri_extract(unlist(remDr$getPageSource()[[1]]), regex = "<div class=\"ui-datatable-header ui-widget-header ui-corner-top\">(.*?)</div>"), regex = "[[:digit:]]{1,4}")) / 25)

    if(page_nr > 1) {
      urls <- lapply(1:page_nr, function(p) {
        remDr$findElement("css", '.ui-icon-seek-next')$clickElement()
        url <- stri_extract_all(as(htmlParse(remDr$getPageSource()[[1]]), "character"), regex = 'href="(.*?)"><span class="sygnatura"') %>% unlist() %>% gsub("href=\"", "", .) %>% gsub("\"><span class=\"sygnatura\"", "", .) %>% gsub("amp;", "", .)
        url <- paste0("http://otkzu.trybunal.gov.pl", url)
        Sys.sleep(3)
        return(url)
      }) %>% unlist()
    } else {
      url <- stri_extract_all(as(htmlParse(remDr$getPageSource()[[1]]), "character"), regex = 'href="(.*?)"><span class="sygnatura"') %>% unlist() %>% gsub("href=\"", "", .) %>% gsub("\"><span class=\"sygnatura\"", "", .)
      url <- paste0("http://otkzu.trybunal.gov.pl", url)
      Sys.sleep(3)
      return(url)
    }

    if(page_nr > 1) {
        remDr$findElement("css", '.ui-icon-seek-first')$clickElement()
    }
    return(urls)
  }) %>% unlist()
  remDr$close()

  if (metadata == TRUE) {
    meta <- lapply(1:length(urls), function(m){
      url_active <- urls[m]
      download.file(url_active, destfile = "scrapedpage.html", quiet=TRUE)
      url_active <- read_html("scrapedpage.html")
      case <- html_text(html_nodes(url_active, css = '.sygnatura'))
      date <- html_text(html_nodes(url_active, css = '#otk\\:j_idt64_content .prop:nth-child(5) .value')) %>% gsub("\n", "", .)
      Sys.setlocale("LC_TIME", "Polish")
      date <- as.Date(date, format = "%d %B %Y")
      doclang <- "PL"
      country <- "POL"
      Sys.sleep(1)
      meta <- data.frame(case = case, date = date, doclang = doclang, country = country)
    })
    meta <- ldply(meta, data.frame)
    file.remove("scrapedpage.html")
  }

  if (textdata == TRUE) {
    texts <- lapply(1:length(urls), function(u) {
      Sys.setlocale("LC_ALL", "Polish")
      Sys.setlocale("LC_CTYPE", "Polish")

      url_active <- urls[u]
      url_file <- paste0("http://otkzu.trybunal.gov.pl", stri_extract_all(as(htmlParse(url_active), "character"), regex = 'href="(.*?)"><img') %>% unlist() %>% gsub("href=\"", "", .) %>% gsub("\"><img", "", .))
      file_type <- stri_extract(url_file, regex = "pdf|Doc|dok")
      if (is.na(file_type) == T) {
        if(year_start >= 2013 && url_file != "http://otkzu.trybunal.gov.plNA") {
          file_type <- "pdf"
        } else {
          file_type <- "unknown"
        }
      }
      if (file_type == "pdf") {
        download.file(url_file, destfile = "decision.pdf", quiet = T, mode = "wb")
        text <- readtext("decision.pdf")
        text <- text$text[1]
        file.remove("decision.pdf")
      } else if (file_type == "Doc" | file_type == "dok") {
        download.file(url_file, destfile = "decision.doc", quiet = T, mode = "wb", encoding = "UTF-32")
        text <- readtext("decision.doc", encoding = "UTF-8") %>% unlist()
        text <- text[2]
        text <- gsub('\\?[[:alpha:]]', 's', text) %>% gsub('\\? [[:lower:]]', 's', .)
      } else {
        text <- NA
      }
      if (is.na(text) == T) {
        url_isap <- stri_extract_all(as(htmlParse(url_active), "character"), regex = 'href="(.*?)" target="_blank">ISAP') %>% unlist() %>% gsub('href="', '', .) %>% gsub('" target="_blank">ISAP', '', .)
        if (is.na(url_isap) == F) {
          url_isap_pdf <- paste0('http://isap.sejm.gov.pl', html_attr(html_nodes(read_html(url_isap), css = 'h2+ .row .doc-link'), 'href'))
          download.file(url_isap_pdf, destfile = "decision.pdf", quiet = T, mode = "wb")
          text <- readtext("decision.pdf")
          text <- paste0("<ISAP> ", text$text[1])
          text <- gsub('\\?[[:alpha:]]', 's', text) %>% gsub('\\? [[:lower:]]', 's', .)
          file.remove("decision.pdf")
        } else {
          text <- NA
        }
      }
      if (is.na(text) == T) {
        url_ipo <- stri_extract_all(as(htmlParse(url_active), "character"), regex = 'href="(.*?)" target="_blank"> link do IPO') %>% unlist() %>% gsub('href="', '', .) %>% gsub('\" target=\"_blank\"> link do IPO', '', .) %>% gsub("&amp;", "&", .)
        if (is.na(url_ipo) == F) {
          text <- html_text(html_nodes(read_html(url_ipo), '.ui-corner-bottom'))
          text <- text[2]
        } else {
          text <- NA
        }
      }
      return(text)
    }) %>% unlist()
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

  if(is.na(date_start) == F && is.na(date_end) == F) {
    temp <- subset(temp, date > as.Date(date_start) & date < as.Date(date_end))
  }

} %>% ldply
