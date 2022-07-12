#' Return decisions from the French Constitutional Court
#'
#' The function extracts decisions from the French Constitutional Court archive \url{https://recherche.conseil-constitutionnel.fr}
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
#' @importFrom utils txtProgressBar
#' @importFrom rvest read_html html_nodes html_attr html_text
#' @importFrom R.cache loadCache saveCache
#'
#' @keywords constitutional court, scraping
#'
#' @examples
#' out <- get_decisions_fra(date_start = "2019-06-01",
#' date_end = Sys.Date())



get_decisions_fra <- function(date_start = "2019-01-01",
                           date_end=Sys.Date(),
                           metadata = TRUE,
                           textdata = TRUE){

  url <- paste0("https://recherche.conseil-constitutionnel.fr/?mid=a35262a4dccb2f69a36693ec74e69d26&filtres[]=type_doc%3AdossierComplet&offsetCooc=&offsetDisplay=0&nbResultDisplay=10&nbCoocDisplay=&UseCluster=&cluster=&showExtr=&sortBy=date&typeQuery=3&dateBefore=&dateAfter=&xtmc=&xtnp=p1&rech_ok=1&date-from=", date_start, "&date-to=", date_end)
  ht <- read_html(url)
  xpath <- '//*[contains(concat( " ", @class, " " ), concat( " ", "nbResFound", " " ))]'
  node <- html_nodes(ht, xpath = xpath)

  page_nr <- ceiling(as.numeric(stri_extract(html_text(node), regex = "[[:digit:]]{1,6}"))/10)
  pb <- txtProgressBar(min = 0, max = page_nr, style = 3)
  urls <- lapply(1:page_nr, function(p){
    url_html <- read_html(paste0("https://recherche.conseil-constitutionnel.fr/?mid=a35262a4dccb2f69a36693ec74e69d26&filtres[]=type_doc%3AdossierComplet&offsetCooc=&offsetDisplay=", (p-1)*10, "&nbResultDisplay=10&nbCoocDisplay=&UseCluster=&cluster=&showExtr=&sortBy=date&typeQuery=3&dateBefore=&dateAfter=&xtmc=&xtnp=p", p, "&rech_ok=1&date-from=", date_start, "&date-to=", date_end))
    xpath <- '//*[contains(concat( " ", @class, " " ), concat( " ", "title", " " ))]//a'
    node <- html_nodes(url_html, xpath = xpath)
    url <- html_attr(node, "href")
    Sys.sleep(2)
    setTxtProgressBar(pb, p)
    return(url)
  }) %>% unlist

  if(metadata == TRUE) {
    pb2 <- txtProgressBar(min = 0, max = length(urls), style = 3)
    meta <- lapply(seq_along(urls),function(m){
      # cache
      meta1 <- loadCache(key=list("meta",m,date_start,date_end))
      if(!is.null(meta1)){
        cat("Loaded cached data\n")
        return(meta1)
      }
      if(is.null(meta1)){
      url_active <- read_html(urls[m])
      xpath <- '//*[contains(concat( " ", @class, " " ), concat( " ", "right", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "title", " " ))]'
      node <- html_nodes(url_active, xpath = xpath)
      Sys.sleep(0.2)
      meta_text <- html_text(node)
      #case
      case <- stri_extract(meta_text, regex = "D?cision n? [[:digit:]]{2,4}-[[:graph:]]{1,50} [[:alpha:]]{1,5}")
      if (length(case) == 0) { case <- NA }
      #ecli
      xpath <- '//*[contains(concat( " ", @class, " " ), concat( " ", "wrapper-content", " " ))]'
      node <- html_nodes(url_active, xpath = xpath)
      ecli_id <- html_text(node)
      ecli_id <- stri_extract(ecli_id, regex = "ECLI(.*?)\n") %>% gsub("\n", "", .) %>% gsub(" : ", "", .)
      if (length(ecli_id) == 0) {
        xpath <- '//*[contains(concat( " ", @class, " " ), concat( " ", "content", " " ))]'
        node <- html_nodes(url_active, xpath = xpath)
        ecli_id <- html_text(node)
        ecli_id <- stri_extract(ecli_id, regex = "ECLI:FR:[[:graph:]]{1,25}:+[[:digit:]]{4}:[[:graph:]]{1,25}")
      }
      if (length(ecli_id) == 0) { ecli_id <- NA }
      date <- stri_extract(meta_text, regex = "[[:graph:]]{1,5} [[:alpha:]]{2,20} [[:digit:]]{4}")
      date <- paste0(stri_extract(date, regex = "[[:digit:]]{1,2}"), " ", gsub("[[:digit:]]{1,2}(.*?) ", "", date))
      Sys.setlocale("LC_TIME", "French")
      date <- as.Date(date, format = "%d %B %Y")
      doclang <- "FR"
      country <- "FRA"
      Sys.sleep(0.5)
      meta1 = data.frame(case = case, ecli_id = ecli_id, date = date, doclang = doclang, country = as.character(country))
      meta1$country <- as.character(meta1$country)
      saveCache(meta1,key=list("meta",m,date_start,date_end))}
      setTxtProgressBar(pb2, m)
      return(meta1)
    })
    meta = ldply(meta, data.frame)
    meta$case <- as.character(meta$case)
    meta$ecli_id <- as.character(meta$ecli_id)
    meta$doclang <- as.character(meta$doclang)

  }

  if (textdata == TRUE) {
    texts <- lapply(seq_along(urls),function(u){

      # cache
      text <- loadCache(key=list("text",u))
      if(!is.null(text)){
        cat("Loaded cached data\n")
        return(text)
      }
      if(is.null(text)){
      url_active <- read_html(urls[u])
      xpath <- '//*[contains(concat( " ", @class, " " ), concat( " ", "wrapper-content", " " ))]'
      node <- html_nodes(url_active, xpath = xpath)
      text <- html_text(node)
      Sys.sleep(0.5)
      text <- gsub("\n", "", text)
      text <- as.character(text)
      saveCache(text,key=list("text",u))
      }

      return(text)
    }) %>% unlist
  }

  if (textdata == FALSE & metadata == TRUE) {
    output <- data.frame(meta, url <- urls)
    output$url <- as.character(output$url)
    output$url....urls <- NULL
  }

  if (textdata == TRUE & metadata == FALSE) {
    output <- data.frame(text <- texts, url <- urls)
    output$text <- as.character(output$text)
    output$url <- as.character(output$url)
    output$text....texts <- NULL
    output$url....urls <- NULL
  }

  if (textdata == TRUE & metadata == TRUE) {
    output <- data.frame(meta,text <- texts, url <- urls)
    output$text <- as.character(output$text)
    output$url <- as.character(output$url)
    output$text....texts <- NULL
    output$url....urls <- NULL
  }

  if (textdata == FALSE & metadata == FALSE) {
    output <- data.frame(url <- urls)
    output$url <- as.character(output$url)
    output$url....urls <- NULL
  }

  return(output)
}

