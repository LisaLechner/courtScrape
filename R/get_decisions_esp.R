#' Return decisions from the Spanish Constitutional Court
#'
#' The function extracts decisions from the Spanish Constitutional Court archive \url{http://hj.tribunalconstitucional.es}
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
#' @importFrom readtext
#' @importFrom stringi stri_extract_all stri_extract
#' @importFrom rvest read_html html_nodes html_text
#' @importFrom RSelenium rsDriver
#' @importFrom XML htmlParse
#' @importFrom utils download.file
#'
#' @keywords constitutional court, scraping
#'
#' @examples
#' out <- get_decisions_esp(date_start = "2019-06-01",
#' date_end = Sys.Date())
#' @export

get_decisions_esp <- function(date_start=(Sys.Date()-1),
                          date_end=Sys.Date(),
                          textdata = TRUE,
                          metadata = TRUE,
                          browser = "chrome",
                          chromever = "89.0.4389.23"){

  date_start <- format(as.Date(date_start), "%d/%m/%Y")
  date_end <- format(as.Date(date_end), "%d/%m/%Y")

  rD <- rsDriver(port = 9517L, browser = c(browser),chromever=chromever)
  remDr <- rD[["client"]]

  remDr$navigate("http://hj.tribunalconstitucional.es/en/Busqueda/Index")
  remDr$findElement(using = "xpath", "//*[(@id = 'date-from')]")$sendKeysToElement(list(date_start))
  remDr$findElement(using = "xpath", "//*[(@id = 'date-to')]")$sendKeysToElement(list(date_end))
  remDr$findElement(using = "xpath", "//*[(@id = 'buttonSearch')]")$clickElement()
  Sys.sleep(2)
  remDr$navigate("http://hj.tribunalconstitucional.es/en/Resolucion/List?resultsPerPage=50")

  number_pages <- ceiling(unlist(as.numeric(remDr$findElement(using = "xpath", "//b")$getElementText())) / 50)

  urls <- lapply(1:number_pages, function(p) {
    remDr$navigate(paste0("http://hj.tribunalconstitucional.es/HJ/en/Resolucion/List?sortOrder=desc&resultsPerPage=50&page=", p))
    Sys.sleep(2)
    urls <- htmlParse(remDr$getPageSource()[[1]])
    urls <- stri_extract_all(as(urls, "character"), regex = 'a class=\"resolucion-item\" href=\"[[:graph:]]{1,100}"') %>% unlist() %>% gsub('a class=\"resolucion-item\" href=\"', '', .) %>% gsub('"', '', .)
    urls <- paste0("http://hj.tribunalconstitucional.es", urls)
  }) %>% unlist()
  remDr$close()

  if (metadata == TRUE) {
    meta <- lapply(1:length(urls), function(m){
      url_active <- urls[m]
      download.file(url_active, destfile = "scrapedpage.html", quiet=TRUE)
      url_active <- read_html("scrapedpage.html")
      node <- html_nodes(url_active, css = '#resolucion-identifier h4')
      case <- stri_extract(html_text(node), regex = "[[:print:]]{1,100},") %>% gsub(",", "", .)
      date <- stri_extract(html_text(node), regex = "/[[:print:]]{1,100}") %>% gsub("/", "", .)
      year <- stri_extract(date, regex = "[[:digit:]]{4}")
      month <- stri_extract(date, regex = "[[:alpha:]]{3,15}")
      if (is.na(month) == FALSE) {
        if (month == "Januar" | month == "January" | month == "Enero" | month == "enero") {
          month <- "01"
        } else if (month == "Februar" | month == "February" | month == "Febrero" | month == "febrero") {
          month <- "02"
        } else if (month == "M?rz" | month == "March" | month == "Marzo" | month == "marzo") {
          month <- "03"
        } else if (month == "April" | month == "Abril" | month == "abril") {
          month <- "04"
        } else if (month == "Mai" | month == "May" | month == "Mayo" | month == "mayo") {
          month <- "05"
        } else if (month == "Juni" | month == "June" | month == "Junio" | month == "junio") {
          month <- "06"
        } else if (month == "Juli" | month == "July" | month == "Julio" | month == "julio") {
          month <- "07"
        } else if (month == "August" | month == "Agosto" | month == "agosto") {
          month <- "08"
        } else if (month == "September" | month == "Septiembre" | month == "septiembre") {
          month <- "09"
        } else if (month == "Oktober" | month == "October" | month == "Octubre" | month == "octubre") {
          month <- "10"
        } else if (month == "November" | month == "Noviembre" | month == "noviembre") {
          month <- "11"
        } else if (month == "Dezember" | month == "December" | month == "Diciembre" | month == "diciembre") {
          month <- "12"
        }
      }
      day <- stri_extract(stri_extract(html_text(node), regex = "de [[:print:]]{1,100}"), regex = "[[:digit:]]{1,2}")
      if (is.na(day) == TRUE) { day <- stri_extract(stri_extract(html_text(node), regex = ", [[:print:]]{1,100}"), regex = "[[:digit:]]{1,2}") }
      if (is.na(day) == TRUE) {
        if (is.na(stri_extract(date, regex = "ll")) == FALSE) { day <- 11 }
      }
      if (is.na(day) == TRUE | is.na(month) == TRUE) { date <- NA } else {
        if (as.numeric(day) < 10) {
          day <- paste0("0", day)
        }
        date <- paste0(year, "-", month, "-", day)
      }

      if (is.na(date) == TRUE) {
        url_active_date <- read_html(paste0(urls[m], '#ficha-tecnica'))
        node_date <- html_nodes(url_active_date, css = '#main')
        date <- stri_extract(html_text(node_date), regex = "[[:digit:]]{1,2}/[[:digit:]]{1,2}/[[:digit:]]{4}")
        date <- as.Date(date, "%d/%m/%Y")
      }

      node <- html_nodes(url_active, css = '.ecli')
      ecli_id <- html_text(node)
      doclang <- "ES"
      country <- "ESP"
      meta <- data.frame(case = case, ecli_id = ecli_id, date = date, doclang = doclang, country = country)
    })
    meta <- ldply(meta, data.frame)
  }

  file.remove("scrapedpage.html")

  if (textdata == TRUE) {
    texts <- lapply(seq_along(urls),function(u){
      url_active <- read_html(urls[u])
      node <- html_nodes(url_active, css = '.section')
      text <- html_text(node)
      Sys.sleep(0.1)
      text <- gsub("\r\n", "", text)
      text <- do.call(paste, c(as.list(text), sep = " "))
      #if (length(text) == 0) { text <- NA }
      ##start pdf
      if(length(text) == 0) {
        node <- html_nodes(url_active, css = '.pdf_link')
        if(length(node) > 0) {
          link_pdf <- paste0("http://hj.tribunalconstitucional.es", html_attr(node, "href"))
          #
          download.file(link_pdf, destfile = "decision.pdf", quiet = T, mode = "wb")
          text <- readtext("decision.pdf")
          text <- text$text[1]
          file.remove("decision.pdf")
        } else {
          text <- NA
        }
      }
      ##end pdf
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



