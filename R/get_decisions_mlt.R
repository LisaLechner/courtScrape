#' Return decisions from the Maltese Constitutional Court
#'
#' The function extracts decisions from the Maltese Constitutional Court archive \url{https://ecourts.gov.mt}
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
#' @importFrom assertthat is.error
#' @importFrom readtext
#' @importFrom stringi stri_extract
#' @importFrom rvest read_html html_text html_nodes html_attr
#' @importFrom utils download.file
#' @importFrom RSelenium rsDriver
#' @importFrom tesseract ocr
#'
#' @keywords constitutional court, scraping
#'
#' @examples
#' out <- get_decisions_mlt(date_start = "2019-06-01",
#' date_end = Sys.Date())
#' @export

get_decisions_mlt <- function(date_start=(Sys.Date()-1),
                          date_end=Sys.Date(),
                          textdata = TRUE,
                          metadata = TRUE,
                          browser = "firefox",
                          chromever = "89.0.4389.23"){

  date_start <- format(as.Date(date_start), "%d/%m/%Y")
  date_end <- format(as.Date(date_end), "%d/%m/%Y")

  rD <- rsDriver(port = 9517L, browser = c(browser),chromever=chromever)
  remDr <- rD[["client"]]
  remDr$navigate("https://ecourts.gov.mt/onlineservices/Judgements")
  Sys.sleep(5)
  remDr$findElement(using = "css", "#divCookiePolicyBar .btn-primary")$clickElement()
  Sys.sleep(5)
  remDr$findElement(using = "css", "li:nth-child(8) a")$clickElement()
  Sys.sleep(5)
  remDr$findElement(using = "css", "#judgementdateOption3")$clickElement()
  remDr$findElement(using = "css", '#judgementDateFrom')$sendKeysToElement(list(date_start))
  remDr$findElement(using = "css", '#judgementDateTo')$sendKeysToElement(list(date_end))
  remDr$findElement("id", "CourtId")$clickElement()
  CourtId <- remDr$findElements("css", "#CourtId option")
  CourtId[[4]]$clickElement()
  Sys.sleep(3)
  remDr$findElement(using = "css", "#searchJudgement")$clickElement()
  Sys.sleep(3)
  number_decisions <- remDr$findElement(using = "css", "#informationResult span")$getElementText() %>% unlist() %>% as.numeric()
  number_pages <- ceiling(number_decisions/100)

  if (number_pages == 1) {
    urls <- lapply(1:(number_decisions), function(u) {
      if((u %% 2) == 0) {
        brick <- "even"
      } else {
        brick <- "odd"
      }
      css <- paste0(".", brick, ":nth-child(", u, ") .btn-default")
      remDr$findElement(using = "css", css)$clickElement()
      Sys.sleep(4)
      url <- remDr$getCurrentUrl()
      if(url == "https://ecourts.gov.mt/onlineservices/Judgements/Search") {
        remDr$findElement(using = "css", css)$clickElement()
      }
      Sys.sleep(4)
      remDr$goBack()
      Sys.sleep(4)
      return(url)
    })%>% unlist()
  } else {
    urls <- lapply(1:number_pages, function(p) {
      case_from <- remDr$findElement(using = "css", "#JudgementTable_info")$getElementText() %>% stri_extract(., regex = "[[:digit:]]{1,10}")
      case_to <- remDr$findElement(using = "css", "#JudgementTable_info")$getElementText() %>% stri_extract(., regex = "to [[:digit:]]{1,10}") %>% gsub("to ", "", .)
      page_seq <- (as.numeric(case_to) - as.numeric(case_from) + 1)
      lapply(1:(page_seq), function(u) {
        if((u %% 2) == 0) {
          brick <- "even"
        } else {
          brick <- "odd"
        }
        css <- paste0(".", brick, ":nth-child(", u, ") .btn-default")
        remDr$findElement(using = "css", css)$clickElement()
        Sys.sleep(4)
        url <- remDr$getCurrentUrl()
        if(url == "https://ecourts.gov.mt/onlineservices/Judgements/Search") {
          remDr$findElement(using = "css", css)$clickElement()
        }
        Sys.sleep(4)
        remDr$goBack()
        Sys.sleep(4)
        return(url)
      })
      if(p != number_pages) {
        button_next <- remDr$findElement(using = "css", "#JudgementTable_next.paginate_button.next")
        button_next$clickElement()
      }
    })%>% unlist()
  }

  remDr$close()
  rm(rD, remDr)

  if (metadata == TRUE) {
    meta <- lapply(1:length(urls), function(m){
      url_active <- read_html(urls[m])
      case <- html_text(html_nodes(url_active, css = '#redmark')) %>% gsub("Reference: ", "", .) %>% gsub("Referenza: ", "", .)
      date <- as.Date(html_text(html_nodes(url_active, css = 'br+ .col-lg-12 .col-md-6')) %>% gsub("\r\n", "", .) %>% gsub(" ", "", .), format = "%d/%m/%Y")
      date <- date[1]
      ecli <- html_text(html_nodes(url_active, css = '.col-lg-12:nth-child(8) .col-md-6 span')) %>% gsub("\r\n", "", .) %>% gsub(" ", "", .)
      doclang <- "MT"
      country <- "MLT"
      Sys.sleep(1)
      meta <- data.frame(case = case, date = date, ecli = ecli, doclang = doclang, country = country)
    })
    meta <- ldply(meta, data.frame)
  }

  if (textdata == TRUE) {
      texts <- lapply(seq_along(urls),function(u){
        id <- stri_extract(regex = "[[:digit:]]{3,7}", urls[u])
        url_active <- read_html(urls[u])
        urlpdf <- paste0('https://ecourts.gov.mt', html_attr((html_nodes(url_active, css = '.mobappicon-mobappicon-download')), 'href'))
        #urlpdf <- paste0("https://ecourts.gov.mt/onlineservices/Judgements/PrintPdf?CaseJudgementId=", id, "&JudgementId=0")
        download.file(urlpdf, destfile = "download.pdf", mode = "wb", quiet = T, timeout = 1000, method = "curl")
        if (is.error(readtext("download.pdf")) == F) {
          text <- readtext("download.pdf") %>% unlist()
          text <- text[2] %>% gsub("\r\n", "", .)
          if (nchar(text) < 100) {
            eng <- tesseract("eng")
            text <- tesseract::ocr("download.pdf", engine = eng)
            text <- paste(text, collapse = " ")
          }
        } else {
          text <- NA
        }
        file.remove("download.pdf")
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



