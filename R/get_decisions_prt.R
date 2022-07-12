#' Return decisions from the Portuguese Constitutional Court
#'
#' The function extracts decisions from the Portuguese Constitutional Court archive \url{www.tribunalconstitucional.pt}
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
#' @importFrom dplyr select filter
#' @importFrom plyr ldply
#' @importFrom stringi stri_extract stri_pad stri_trim
#' @importFrom rvest read_html html_text html_nodes
#'
#' @keywords constitutional court, scraping
#'
#' @examples
#' out <- get_decisions_prt(date_start = "2019-06-01",
#' date_end = Sys.Date())
#' @export

get_decisions_prt <- function(date_start=(Sys.Date()-1),
                          date_end=Sys.Date(),
                          textdata = TRUE,
                          metadata = TRUE){

  date_start1 <- stri_extract(date_start, regex = "[[:digit:]]{4}")
  date_end1 <- stri_extract(date_end, regex = "[[:digit:]]{4}")

#getmeta

meta <- lapply(date_start1:date_end1,function(d){
    url <- paste0("http://www.tribunalconstitucional.pt/tc/acordaos/?acnum=&acano=",d,"&procnum=&procano=&seccao=&especie=&relator=&datadia=&datames=&dataano=&prepesquisa=&pesquisatipo=pesquisa&pesquisa=&pesquisanegativa=&datadia2=&datames2=&dataano2=&page=0")
    ht <- read_html(url)
    Sys.sleep(0.4)
    xpath_page <- "//small"
    page_nr <- html_nodes(ht,xpath=xpath_page) %>% html_text()

    n <- as.numeric(stri_extract(page_nr, regex = "[[:digit:]]{1,}"))
    page_nr <- as.numeric(gsub("/","",stri_extract(page_nr, regex = "/[[:digit:]]{1,}")))/n
    page_nr <- ceiling(page_nr)-1
    xpath <- '//*[contains(concat( " ", @class, " " ), concat( " ", "textoacordaolista", " " ))]//a'
    meta1 <- lapply(0:page_nr,function(p){
      url <- paste0("http://www.tribunalconstitucional.pt/tc/acordaos/?acnum=&acano=",d,"&procnum=&procano=&seccao=&especie=&relator=&datadia=&datames=&dataano=&prepesquisa=&pesquisatipo=pesquisa&pesquisa=&pesquisanegativa=&datadia2=&datames2=&dataano2=&page=",p)
      ht <- read_html(url)
      Sys.sleep(0.2)
      #id
      css <- '#acbigger a'
      node <- html_nodes(ht,css)
      id <- html_text(node) %>% stri_trim(.) %>% unique(.)
      #ProzessNr
      css <- '.processo'
      node <- html_nodes(ht,css)
      ProzessNr <- html_text(node) %>% stri_trim(.)
      ProzessNr <- ProzessNr[1:length(id)]
      #Berichterstatter
      css <- 'td.relator'
      node <- html_nodes(ht,css)
      Berichterstatter <- html_text(node) %>% stri_trim(.)
      #Art
      css <- '.especie'
      node <- html_nodes(ht,css)
      Art <- html_text(node) %>% stri_trim(.)
      Art <- Art[1:length(id)]
      #Formacao
      css <- '.seccao'
      node <- html_nodes(ht,css)
      Formacao <- html_text(node) %>% stri_trim(.)
      Formacao <- Formacao[1:length(id)]
      #dates
      css <- '.data'
      node <- html_nodes(ht,css)
      dates <- html_text(node) %>% stri_trim(.)
      dates <- as.Date(dates, "%d.%m.%Y")
      dates <- dates[1:length(id)]

      meta2 <- data.frame(id=id,ProzessNr=ProzessNr,dates=dates,Formacao=Formacao,Art=Art,Berichterstatter=Berichterstatter)
    }) %>% ldply %>% filter(between(dates, as.Date(date_start), as.Date(date_end)))
return(meta1)}) %>% ldply()



#get URL
id <- meta %>% select(id) %>% unlist
id <- stri_extract(id, regex = "[[:digit:]]{1,4}")
id <- stri_pad(id, 4, pad = "0")
d <- meta %>% select(dates) %>% unlist
d <- as.Date(d, "1970-01-01")
d <- stri_extract(d, regex = "[[:digit:]]{4}")
i <- paste0(d,id)
urls <- lapply(i, function(i){
  urls <- paste0("http://www.tribunalconstitucional.pt/tc/acordaos/",i,".html")
return(urls)
}) %>% unlist


#get texts
if(textdata == TRUE) {
 urls <- urls %>% unlist
 texts <- lapply(seq_along(urls),function(u){
   text <- read_html(urls[u])
   text <- html_nodes(text, xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "WordSection1", " " ))]')
   text <- html_text(text)
   if (length(text) == 0) {
     text <- read_html(urls[u])
     text <- html_nodes(text, xpath = '//*[(@id = "texto")]')
        text <- html_text(text)
        text <- gsub("\n", "", text)
        text <- gsub("\t", "", text)
        text <- gsub("\r", "", text)
        }
if (length(text) == 0) { text <- NA }
   Sys.sleep(0.4)
  return(text)
  texts <- unique(texts)
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


