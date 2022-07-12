#' Return decisions from the Slovakian Constitutional Court
#'
#' The function extracts decisions from the Slovakian Constitutional Court archive \url{https://www.ustavnysud.sk}
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
#' @importFrom stringi
#' @importFrom rvest read_html html_text html_nodes html_attr
#' @importFrom RSelenium rsDriver
#' @importFrom XML
#' @importFrom utils download.file
#' @importFrom lubridate
#'
#' @keywords constitutional court, scraping
#'
#' @examples
#' out <- get_decisions_svk(date_start = "2019-06-01",
#' date_end = Sys.Date())
#' @export

get_decisions_svk <- function(date_start= "2021-12-07",
                          date_end  = Sys.Date(),
                          browser   = "chrome",
                          chromever = "96.0.4664.45",  #changed chrome Version
                          textdata  = TRUE,
                          metadata  = TRUE) {
  date_start <- format(as.Date(date_start), "%d.%m.%Y")
  date_end <- format(as.Date(date_end), "%d.%m.%Y")

  # Navigate to the page and Initialize search =================================
  # Random port number
  rand_port <- sample(1:9999,1)
  rD <- rsDriver(port = rand_port, browser = c(browser), chromever = chromever)
  #rD <- rsDriver(browser = c(browser), chromever = chromever)
  remDr <- rD[["client"]]
  #Sys.sleep(5)
  remDr$navigate("https://www.ustavnysud.sk/vyhladavanie-rozhodnuti#!DmsSearchView")
  #Sys.sleep(5)
  Sys.sleep(1)
  remDr$findElement(using = "css", '#gwt-uid-17')$sendKeysToElement(list(date_start))
  #Sys.sleep(5)
  remDr$findElement(using = "css", '#gwt-uid-19')$sendKeysToElement(list(date_end))
  Sys.sleep(1) #1
  remDr$findElement(using = "css", '#gwt-uid-19')$sendKeysToElement(list(date_end))
  Sys.sleep(1) #1
  remDr$findElement(using = "css", ".v-slot:nth-child(1) .v-button-wrap")$clickElement()
  Sys.sleep(2) #2
  results <- remDr$findElements(using = 'css', ".v-label-resultcount")
  results <- results[[1]]
  results <- results$getElementText() %>% unlist()
  results <- as.numeric(stri_extract(results, regex = "[[:digit:]]{1,5}"))
  pages <- ceiling(results/25)

  # Get URLS====================================================================
  Sys.sleep(3) #new
  urls <- lapply(1:pages, function(v){
    url_css <- remDr$findElements(using = 'css', '#p_p_id_USSRDmsSearch_WAR_ussrintranetportlet_ a')
    urls <- lapply(1:length(url_css), function(u){
      url <- url_css[[u]]
      url <- url$getElementAttribute('href') %>% unlist()
      return(url)
    }) %>% unlist()
    urls <- ldply(urls, data.frame)
    if(v != pages) {
      remDr$findElement(using = "css", ".v-button-pagedtable-next .v-button-caption")$clickElement()
    }
    #Sys.sleep(7)
    Sys.sleep(2)
    return(urls)
  })
  urls <- ldply(urls, data.frame)
  urls <- urls %>% rename(url = X..i..)
  if (pages > 1) {
    to_first_page <- remDr$findElement(using = "css", ".v-button-pagedtable-first .v-button-caption")
    to_first_page$clickElement()
  }
  #Sys.sleep(25)
  Sys.sleep(1)

  # Extract Metadata ===========================================================
  if (metadata == TRUE) {
    meta <- lapply(1:pages, function(p){
      if(p == pages) {
        decisions <- results - ((pages - 1)*25)
      } else {
        decisions <- 25
      }
      #Sys.sleep(2)
      meta <- lapply(1:decisions, function(x){
        #Sys.sleep(1)
        if((x %% 2) == 0) {
          brick <- "-odd"
        } else {
          brick <- ""
        }
        #Sys.sleep(2)
        #Scroll up to avoid hitting banner
        if(x==1) {
          webElem <- remDr$findElement("css", "body")
          webElem$sendKeysToElement(list(key = "home"))
          Sys.sleep(1)
        } #EDIT
        #css = paste0('.v-table-row', brick, ':nth-child(', x, ') .v-button-wrap') #Orginal
        css = paste0('.v-table-row', brick, ':nth-child(', x, ') .v-button-caption')
        show_metadata_x <- remDr$findElement(using = "css", css)
        show_metadata_x$clickElement()
        #Sys.sleep(2)
        Sys.sleep(0.5) #FAST 0.1 else 1
        css_case <- paste0('.v-table-row', brick, ':nth-child(', x, ') .v-slot-datalayout')
        case <- remDr$findElement(using = 'css', css_case)$getElementText() %>% unlist()
        case <- gsub("D?tum.*", "", case)
        ecli <- remDr$findElement(using = 'css', '.v-slot-property:nth-child(1) .v-slot-valuelbl')$getElementText() %>% unlist()
        if(ecli == "") { ecli <- NA }
        date <- remDr$findElement(using = 'css', '.v-slot-property:nth-child(4) .v-slot-valuelbl')$getElementText() %>% unlist()
        date <- as.Date(date, format = "%d. %m. %Y")
        country <- "SVK"
        doclang <- "SK"
        #Sys.sleep(1)
        #added as.characrer(ecli) because sometimes ecli == 1
        meta <- data.frame(case = case, ecli = as.character(ecli), date = date, doclang = doclang, country = country)
        #Sys.sleep(1)
        return(meta)
        #Sys.sleep(1)
      })
      #Sys.sleep(1)
      meta <- ldply(meta, data.frame)

      if(p != pages) {
        remDr$findElement(using = "css", ".v-button-pagedtable-next .v-button-caption")$clickElement()
        Sys.sleep(1) #new
      }
      #Sys.sleep(20)
      #Sys.sleep(2)
      return(meta)
    })
    meta <- ldply(meta, data.frame)
    if (pages > 1) {
      remDr$findElement(using = "css", ".v-button-pagedtable-first .v-button-caption")$clickElement()
    }
  }
  remDr$close()
  rD$server$stop()

  # Download Textdata ============================================================

  if (textdata == TRUE) {
    texts <- lapply(1:results, function(t){
      # url_file <- urls$url[t] Originalcode
      # if ERROR occurs in the download text is saved as NA
      tryCatch(
        {
          url_file <- levels(unlist(urls)[t])[t]  ###Solution to URL ERROR
          download.file(url_file, destfile = "decision.pdf", quiet = T, mode = "wb")
          text <- readtext("decision.pdf")
          text <- text$text[1]
          file.remove("decision.pdf")
          texts <- ldply(text, data.frame)
          texts$text <- texts$X..i..
          texts$X..i.. <- NULL
          #Sys.sleep(3)
          return(texts)
        },
        error = function(cond) {
          print(paste("DOWNLOAD ERROR! Decision number:", t))
          text <- NA
          file.remove("decision.pdf")
          texts <- ldply(text, data.frame)
          texts$text <- texts$X..i..
          texts$X..i.. <- NULL
          #Sys.sleep(3)
          return(texts)
        }
      )
    })
    #Sys.sleep(2)
    texts <- ldply(texts, data.frame)
  }

  # Merge Text and Metadata=====================================================

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

