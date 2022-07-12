#' Return decisions from the Austrian Constitutional Court
#'
#' The function extracts decisions from the Austrian Constitutional Court archive \url{https://www.ris.bka.gv.at}
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
#' @importFrom stringi stri_extract
#' @importFrom rvest html_nodes read_html html_attr html_text
#' @importFrom xml2 xml_attrs
#'
#' @keywords constitutional court, scraping
#'
#' @examples
#' out <- get_decisions_aut(date_start = "2019-06-01",
#' date_end = Sys.Date())
#'
#' @export

get_decisions_aut <- function (date_start = "2019-06-01",
                           date_end=Sys.Date(),
                           metadata = TRUE,
                           textdata = TRUE) {

      date_start <- format(as.Date(date_start), "%d.%m.%Y")
      date_end <- format(as.Date(date_end), "%d.%m.%Y")

      url <- paste0("https://www.ris.bka.gv.at/Ergebnis.wxe?Abfrage=Vfgh&Entscheidungsart=Undefined&Sammlungsnummer=&Index=&SucheNachRechtssatz=False&SucheNachText=True&GZ=&VonDatum=",date_start,"&BisDatum=",date_end,"&Norm=&ImRisSeitVonDatum=&ImRisSeitBisDatum=&ImRisSeit=Undefined&ResultPageSize=100&Suchworte=&Position=1")
      url_html <- read_html(url)

      xpath <- '//*[(@id = "TopMainTasks")]//*[contains(concat( " ", @class, " " ), concat( " ", "NumberOfDocuments", " " ))]'
      node <- html_nodes(url_html, xpath = xpath)
      page_nr <- ceiling(as.numeric(gsub("von", "", stri_extract(html_text(node), regex = "von.+[[:digit:]]{1,}"))) / 100)
      urls <- lapply(1:page_nr, function(p) {
        url_html <- read_html(paste0("https://www.ris.bka.gv.at/Ergebnis.wxe?Abfrage=Vfgh&Entscheidungsart=Undefined&Sammlungsnummer=&Index=&SucheNachRechtssatz=False&SucheNachText=True&GZ=&VonDatum=",date_start,"&BisDatum=",date_end,"&Norm=&ImRisSeitVonDatum=&ImRisSeitBisDatum=&ImRisSeit=Undefined&ResultPageSize=100&Suchworte=&Position=",p-1,"01"))
        xpath <- '//*[contains(concat( " ", @class, " " ), concat( " ", "nonWrappingCell", " " ))]'
        node <- html_nodes(url_html, xpath = xpath)
        urls2 <- html_attr(node, "href")
        Sys.sleep(1)
        return(urls2)
      }) %>% unlist
      urls <- unique(urls)
      urls <- paste0(paste0("https://www.ris.bka.gv.at", urls))

      if (metadata == TRUE) {
        meta <- lapply(1:length(urls), function(m){
          url_active <- read_html(urls[m])
          xpath <- '//*[contains(concat( " ", @class, " " ), concat( " ", "p", " " ))]'
          node <- html_nodes(url_active, xpath = xpath)
          meta_node <- html_text(node)
          case <- meta_node[3] %>%
            stri_extract(., regex = "\r\n[[:print:]]{1,50}/[[:print:]]{2,500}\r\n") %>%
            gsub("\r\n", "", .)
          sammlungsnummer <- meta_node[2] %>% stri_extract(., regex = "[[:digit:][:punct:]]{3,20}")
          if (length(sammlungsnummer) == 0) { sammlungsnummer <- NA }
          ecli_id <- meta_node[11] %>%
            gsub("\r\n", "", .) %>%
            stri_extract(., regex = "ECLI:AT:VFGH:+[[:digit:]]{4}:[[:graph:]]{1,25}") %>%
            gsub("\r\n", "", .)
          if (is.na(ecli_id) == TRUE) {
            ecli_id <- meta_node[12] %>%
              gsub("\r\n", "", .) %>%
              stri_extract(., regex = "ECLI:AT:VFGH:+[[:digit:]]{4}:[[:graph:]]{1,25}") %>%
              gsub("\r\n", "", .)
          }
          if (is.na(ecli_id) == TRUE) {
            ecli_id <- meta_node[9] %>%
              gsub("\r\n", "", .) %>%
              stri_extract(., regex = "ECLI:AT:VFGH:+[[:digit:]]{4}:[[:graph:]]{1,25}") %>%
              gsub("\r\n", "", .)
          }
          if (is.na(ecli_id) == TRUE) {
            ecli_id <- meta_node[10] %>%
              gsub("\r\n", "", .) %>%
              stri_extract(., regex = "ECLI:AT:VFGH:+[[:digit:]]{4}:[[:graph:]]{1,25}") %>%
              gsub("\r\n", "", .)
          }
          date <- meta_node[4] %>% stri_extract(., regex = "[[:digit:]]{2}.[[:digit:]]{2}.[[:digit:]]{4}")
          subject <- meta_node[10]
          subject_control <- stri_extract(subject, regex = "Schlagworte")
          if (is.na(subject_control) == TRUE) {
            subject <- meta_node[11]
          }
          subject_control <- stri_extract(subject, regex = "Schlagworte")
          if (is.na(subject_control) == TRUE) {
            subject <- meta_node[8]
          }
          subject_control <- stri_extract(subject, regex = "Schlagworte")
          if (is.na(subject_control) == TRUE) {
            subject <- meta_node[9]
          }
          subject <- gsub("\r\n      Schlagworte\r\n      ", "", subject)
          doclang <- "DE"
          country <- "AUT"

          Sys.sleep(0.1)

          meta <- data.frame(case = case, sammlungsnummer = sammlungsnummer, ecli_id = ecli_id, date = date, subject = subject, doclang = doclang, country = country)
          })
        meta <- ldply(meta, data.frame)
      }

      if (textdata == TRUE) {
        texts <- lapply(1:length(urls), function(u) {
              url_active <- read_html(urls[u])

              xpath <- '//*[contains(concat( " ", @class, " " ), concat( " ", "p", " " )) and (((count(preceding-sibling::*) + 1) = 5) and parent::*)]'
              node <- html_nodes(url_active, xpath = xpath)
              year <- html_text(node) %>% stri_extract(., regex = "[[:digit:]]{4}")

              if (year > 1979) {
                xpath <- '//*[contains(concat( " ", @class, " " ), concat( " ", "embeddedContent", " " ))]'
                node <- html_nodes(url_active, xpath = xpath)
                text <- html_text(node)
                if (is.na(text[1]) == FALSE) {
                  text <- paste0(text[1], text[2]) %>% gsub("\r\n", "", .)
                } else {
                  text <- NA
                }

              } else {
                xpath <- '//*[contains(concat( " ", @class, " " ), concat( " ", "documentDataItem", " " )) and (((count(preceding-sibling::*) + 1) = 3) and parent::*)]'
                node <- html_nodes(url_active, xpath = xpath)
                urlpdf <- xml_attrs(xml_child(node[[1]], 1))[["href"]]
                urlpdf <- paste0("https://www.ris.bka.gv.at", urlpdf)
                text <- readtext(urlpdf) %>% gsub("\r\n", "", .)
              }
              Sys.sleep(1)
              return(text)
        }) %>% unlist
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


