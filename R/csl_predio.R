#' Capas prediales para conaf
#'
#' @param predio.shp Capa de un predio en formato sf y sistema de coordenadas UTM.
#' @param rol Rol de avalúo fiscal del predio
#' @param Nombre.predio Nombre del predio
#' @param save True si desea guardar la capa. FALSE default
#' @param NOMBREPREDIO Abreviación del nombre del predio que irá en el nombre del archivo al guardarse. Este debe ser todo con mayúsculas.
#' @param path Dirección donde desea guardar el documento. Directorio por defecto.
#'
#' @return Capa del predio como la solicita conaf
#'
#' @importFrom dplyr mutate select
#' @importFrom sf st_area write_sf
#' @importFrom units set_units drop_units
#'
#' @export
#'
#' @examples
#' csl_predio(predio.shp = Pica,
#' rol = "123-4",
#' Nombre.predio = "Fulano y Sutano",
#' save = TRUE,
#' NOMBREPREDIO = "FULANOYSUTANO",
#' path = tempdir())
#'
csl_predio <- function(predio.shp, rol, Nombre.predio, save = F, NOMBREPREDIO, path = getwd()){
  predio <- predio.shp |>
    dplyr::mutate(
      Nom_Predio = Nombre.predio,
      Rol = rol |> as.character(),
      Sup_ha = sf::st_area(geometry) |>
        units::set_units(hectare)) |>
    units::drop_units() |>
    dplyr::select(Nom_Predio, Rol, Sup_ha)
  if (save == T) {
    sf::write_sf(predio, file.path(path, paste0('Limite_Predial.', NOMBREPREDIO, '.shp')))
  }
  return(predio)
}
