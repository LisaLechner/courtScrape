#' Return decisions from the Hungarian Constitutional Court
#'
#' The function extracts decisions from the Hungarian Constitutional Court archive \url{https://alkotmanybirosag.hu}
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
#' @importFrom rvest read_html html_text html_nodes html_attr
#'
#' @keywords constitutional court, scraping
#'
#' @examples
#' out <- get_decisions_hun(date_start = "2019-06-01",
#' date_end = Sys.Date())
#' @export


get_decisions_hun <- function(date_start=(Sys.Date()-1),
                          date_end=Sys.Date(),
                          textdata = TRUE,
                          metadata = TRUE){

  year_start <- stri_extract(date_start, regex = "[[:digit:]]{4}")
  year_end <- stri_extract(date_end, regex = "[[:digit:]]{4}")

  if(year_start != year_end) {
    year_interval <- (as.numeric(year_start)):year_end
  } else {
  year_interval <- year_start
  }

  urls <- lapply(1:length(year_interval), function(x){
    url_html <- read_html(paste0("https://alkotmanybirosag.hu/ugykereso/talalatok?hatarozat_sorszam=&hatarozat_evszam=", year_interval[x], "&ugyszam_sorszam=&ugyszam_evszam=&dontes_szerv=&lezaras_modja=&befejezo_dontes_tartalma=&rendelkezo_resz=&indoklas=&velemenyek=&alkotmanybiro=&ugyszaki_jelleg=&inditvanyozo_tipusa=&eljaras_tipusa=&ugyallapot=&alkotpanasz_ugyall=&jogszabaly=&lenyeg=&feltetel1=2&feltetel2=2&befejezes_tipusa="))
    node <- html_nodes(url_html, css = '.result-link')
    urls <- html_attr(node, "href")
  }) %>% unlist()

  if (metadata == TRUE) {
    meta <- lapply(1:length(urls), function(m){
      url_active <- read_html(urls[m])
      case <- html_text(html_nodes(url_active, css = '#ugyszam2 font'))
      date <- html_text(html_nodes(url_active, css = '#keltezes font~ font'))
      date <- as.Date(date, format = "%m/%d/%Y")

      doclang <- "HU"
      country <- "HUN"

      Sys.sleep(0.1)

      meta <- data.frame(case = case, date = date, doclang = doclang, country = country)
    })
    meta <- ldply(meta, data.frame)
  }


  if (textdata == TRUE) {
    texts <- lapply(1:length(urls), function(u) {
      url_active <- read_html(urls[u])
      text <- html_text(html_nodes(url_active, css = '#hatarozat1')) %>% gsub("A hat?rozat sz?vege:\n", "", .)
      Sys.sleep(1)
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

  temp <- subset(temp, date > as.Date(date_start) & date < as.Date(date_end))

} %>% ldply


