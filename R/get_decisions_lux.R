#' Return decisions from the Luxembourgish Constitutional Court
#'
#' The function extracts decisions from the Luxembourgish Constitutional Court archive \url{https://justice.public.lu}
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
#' @importFrom dplyr full_join
#' @importFrom plyr ldply
#' @importFrom readtext
#' @importFrom stringi stri_extract stri_extract_all
#' @importFrom rvest read_html html_text html_nodes html_attr
#' @importFrom utils download.file
#'
#' @keywords constitutional court, scraping
#'
#' @examples
#' out <- get_decisions_lux(date_start = "2019-06-01",
#' date_end = Sys.Date())
#' @export


get_decisions_lux <- function(date_start=(Sys.Date()-50),
                          date_end=Sys.Date(),
                          textdata = TRUE,
                          metadata = TRUE){

  date_start1 <- stri_extract(date_start, regex = "[[:digit:]]{4}")
  date_end1 <- stri_extract(date_end, regex = "[[:digit:]]{4}")


date_start2 <- as.numeric(date_start1)
date_end2 <- as.numeric(date_end1)
  if(date_end2 - date_start2 == 0){
    url <- paste0("https://justice.public.lu/fr/jurisprudence/cour-constitutionnelle.html?r=f%2Faem_first_released%2F",date_start1,"&")}
  if(date_end2 - date_start2 >=1) {
    a <- date_end2 - date_start2
    a <- seq(1, a, by=1)
    url <- paste0("https://justice.public.lu/fr/jurisprudence/cour-constitutionnelle.html?r=f%2Faem_first_released%2F",date_end1,"&")
    url <- lapply(seq_along(a), function(a){
      url <- paste0("r=f%2Faem_first_released%2F",date_end2-a,"&")
    })
    url <- unlist(url)
    url <- paste0(url,collapse="")
    url <- paste0("https://justice.public.lu/fr/jurisprudence/cour-constitutionnelle.html?r=f%2Faem_first_released%2F",date_end1,"&",url)
    }

ht <- read_html(url)

#pagenr

css <- '.search-meta-count'
node <- html_nodes(ht,css)
pagenr <- html_text(node) %>% stri_extract(regex = "[[:digit:]]{1,}")
# css <- 'div span'
# ht <- read_html(url)
# node <- html_nodes(ht,css)
# pagenr <- html_text(node) %>% stri_trim(.) %>% unique(.)
# pagenr <- pagenr[39]
# pagenr <-  stri_extract(pagenr, regex = "[[:digit:]]{1,4}")
pagenr <- as.numeric(pagenr)
pagenr <- pagenr/20
pagenr <- ceiling(pagenr)


p <- pagenr * 20
p <- p - 20
url <- lapply(seq(0,p, by=20), function(p)
{paste0(url, "b=", p)})

#geturls
url <- unlist(url)
urls <- lapply(seq_along(url), function(u){
Sys.sleep(1)
url_html <- read_html(url[u])
css <- '.article-title a'
id <- html_nodes(url_html, css) %>% html_text()

css <- 'h2 a'
node <- html_nodes(url_html, css)
urls1 <- data.frame(id=id,url=html_attr(node, "href"))
urls1$url <- as.character(urls1$url)

for(t in seq_along(urls1$url)){
  url_html <- read_html(as.character(urls1$url[t]))
  css <- '#picto-download-pdf'
  xpath <- '//*[@id="linkManifPdf"]'
  node <- html_node(url_html, xpath = xpath)
  urls3 <- html_attr(node, "href")
  if(!is.null(urls3)){
  urls1$url[t] <- as.character(urls3) }else{
    urls1$url[t] <- NA}
}


return(urls1)
}) %>% ldply

#get meta
if(metadata == TRUE) {
meta <- lapply(seq_along(url), function(u){
  Sys.sleep(1)
  url_html <- read_html(url[u])

  # date
  css <- '.article-published'
  date <- html_nodes(url_html, css) %>% html_text()
  date <- as.Date(date,"%d/%m/%Y")
  # title numbering
  css <- '.article-title a'
  id <- html_nodes(url_html, css) %>% html_text()
  # title numbering
  css <- '.article-title a'
  id <- html_nodes(url_html, css) %>% html_text()
  # content
  css <- '.article-custom'
  cont <- html_nodes(url_html, css) %>% html_text()
  topic <- gsub("mati.re: ","",(stri_extract_all(cont,regex="mati.re:.+")))
  law <- gsub("loi: ","",(stri_extract_all(cont,regex="loi:.+")))
  const_law <- gsub("constitution: ","",(stri_extract_all(cont,regex="constitution:.+")))
  status <- gsub("conformit.: ","",(stri_extract_all(cont,regex="conformit.:.+")))

  meta1 <- data.frame(id=id,date=date,topic=topic,law=law,const_law=const_law,status=status)
  return(meta1)
}) %>% ldply
}



# #getmeta
# if(metadata == TRUE) {
# meta <- lapply(seq_along(urls), function(m){
#   url_html <- read_html(urls[m])
#   #id
#   css <- '.page-title'
#   node <- html_nodes(url_html, css)
#   id <- html_text(node, "href")
#   id <- gsub("Arr?t de la Cour constitutionnelle", "", id)
#   id <- gsub("Arr?t de la Cour Constitutionnelle", "", id)
#   id <- gsub("- ", "", id)
#   #type
#   css <- '.col-md-12:nth-child(1) p:nth-child(2)'
#   node <- html_nodes(url_html, css)
#   type <- html_text(node, "href")
#   type <- gsub("\n", "", type)
#   type <- gsub("\t", "", type)
#   type <- gsub("\r", "", type)
#   type <- gsub("Type :", "", type)
#   #signature
#   css <- '.col-md-12:nth-child(1) p:nth-child(3)'
#   node <- html_nodes(url_html, css)
#   signature <- html_text(node, "href")
#   signature <- gsub("\n", "", signature)
#   signature <- gsub("\t", "", signature)
#   signature <- gsub("\r", "", signature)
#   signature <- gsub("Signature : ", "", signature)
#
#   meta <- data.frame(id = id, type=type,signature=signature)
#   return(meta)
# }) %>% ldply
# }



# get texts (for this purpose we need Rselenium)
if(textdata == TRUE) {
texts <- lapply(seq_along(urls$url), function(t){
  download.file(urls$url[t],destfile = paste0(tempdir(),"/decision.pdf"))
  text <- data.frame(id=urls$id[t],text=readtext::readtext(paste0(tempdir(),"/decision.pdf")))
  file.remove(paste0(tempdir(),"/decision.pdf"))
  return(text)
  Sys.sleep(0.1)
}) %>% ldply

texts$text.doc_id <- NULL
}

#gettexts
# if(textdata == TRUE) {
#   texts <- lapply(seq_along(urls), function(t){
#     Sys.sleep(0.1)
#     url_html <- read_html(urls[t])
#     css <- '.rich-text'
#     node <- html_nodes(url_html, css)
#     text <- html_text(node, "href") %>% stri_trim(.)
#     text <- gsub("\n", "", text)
#     text <- gsub("\t", "", text)
#     if (length(text) == 0) { text <- NA }
#     return(text)
#   })
# }
# texts <- unlist(texts)

if(textdata==FALSE & metadata==TRUE){
  temp <- full_join(meta,urls)
}

if(textdata==TRUE & metadata==TRUE){
  temp <- full_join(meta,urls)
  temp <- full_join(temp,texts)
}

if(textdata==FALSE & metadata==FALSE){
  temp <- urls
}
if(textdata==TRUE & metadata==FALSE){
  temp <- full_join(texts,urls)
}
return(temp)

} %>% ldply

