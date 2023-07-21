#' Convertir archivos Kml a shp
#'
#' @description
#' `csl_kml_to_shp` convierte los archivos kml de un directorio en shapefiles.
#'
#' @param crs Sistema de coordenadas de destino. Ej: 4326, 32718, 32719, etc.
#'
#' @return No retorna ni un archivo. Solo guarda shapefiles en el mismo directorio de los kml.
#' @export
#'
#' @importFrom sf read_sf write_sf st_transform st_zm
#' @importFrom stringr str_c
#' @importFrom magrittr `%>%`
#'
#' @examples
#' csl_kml_to_shp(crs = 32719)
#'
csl_kml_to_shp <- function(crs) {
  file.dir  <-  choose.dir()
  kml <- list.files(path = file.dir, pattern = '.kml$')
  shp <- kml %>% stringr::str_replace('.kml$', '.shp')
  for (i in seq_along(kml)) {
    shape <- sf::read_sf(stringr::str_c(file.dir, kml[i], sep = '\\')) %>%
      sf::st_transform(crs) %>%
      sf::st_zm(drop = T)
    sf::write_sf(shape, stringr::str_c(file.dir, shp[i], sep = '\\'))
  }
}
