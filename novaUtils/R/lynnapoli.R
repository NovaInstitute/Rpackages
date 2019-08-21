
#' @title lyneNaPoligone
#' @description Vat lyne getrek op QGIS en maak tot poligone
#' @return "sf" "data.frame"
#' @param fn Karakter. Naam van ESRI shapefile soos "erf_9382_subsections_blocks_20190821.shp"
#' @param druk Logies. Druk of nie.
#' @import tidyverse
#' @import sf
#' @export


lyneNaPoligone <- function(fn, druk = TRUE){
  ll <- st_read(fn) %>% 
    filter(!st_is_empty(geometry)) %>%
    st_simplify(dTolerance = 0.00005) 
  
  kortes <- which( sapply(ll$geometry,length) < 5)
  df <- as_tibble()
  
  ll <- ll[-kortes, ] %>% 
    st_cast("POLYGON") %>% 
    mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
           lat = map_dbl(geometry, ~st_centroid(.x)[[2]]))
  if (druk){
    ggplot(data = ll) + 
      geom_sf() + 
      geom_text(data = ll, mapping = aes(x = lon, y = lat, label = id))
  }
  ll
}

#' @title maakInvulNommers
#' @description Vat die produk van lyneNaPoligone en skep 'n 
#' karakterstring in die vorm id = "" wat jy kan gebruik om 'n 
#' benoemde vektor van id en teikenwaarde te maak
#' @param ssf sf.  produk van lyneNaPoligone
#' @export

maakInvulNommers <- function(ssf = ll){
  invisible(aa <- parse(text = dput(paste0("'",as.character(ll$id),"'" , "=", "''"))) %>% attributes())
  message("plak die uitvoer in 'n teks-leer tussen vnommer <- c( en ) en redigeer\n
          gebruik dan nuwePoliNommers om te ")
  return(aa$wholeSrcref)
}


#' @title nuwePoliNommers
#' @description Vat die resultaat van maakInvulNommers en lyneNaPoligone en ken nommers toe
#' @param vnommer karakter. vektor geskep en geredigeer met behulp van maakInvulNommers]
#' @param druk Logies
#' @return ssf
#' @export

nuwePoliNommers <- function(ssf, vnommer, druk = TRUE){
  dfv <- tibble(id = as.integer(names(vnommer)), number = vnommer)
  ll <- ssf %>% left_join(dfv) %>% filter(!is.na(number))
  
  if (druk){
    ggplot(data = ll) + 
    geom_sf() + 
    geom_text(data = ll, mapping = aes(x = lon, y = lat, label = id)) + 
    geom_text(data = ll %>% left_join(dfv), 
              mapping = aes(x = lon, y = lat+0.00008, label = number, color = I("Red")))
  }
  
  ll
  
}



