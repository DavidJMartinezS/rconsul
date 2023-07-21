#' Listado Taxonómico
#'
#' Listado de especies junto a características taxonómicas, de distribución y protección legal.
#' Tener en cuenta que los nombres de las especies pueden no estar actualizadas, por lo que se recomienda revisar.
#'
#' @format ## `List_tax`
#' A tibble with 479rows and 14 columns:
#' \describe{
#'   \item{N°_ID}{Número identificador}
#'   \item{ESPECIE, NOMBRE.COMÚN, DIVISIÓN, CLASE, FAMILIA, GÉNERO, ORIGEN.FITOGEOGRÁFICO, HÁBITO.DE.CRECIMIENTO}{Campos taxonómicos}
#'   \item{DS.68/2009, ESTADO.DE.CONSERVACIÓN, REFERENCIA.O.DECRETO.CATEGORÍA.VIGENTE}{Campos legales de protección}
#'   \item{DISTRIBUCIÓN, RANGO.ALTITUDINAL}{Rangos de distribución regional y altitudinal}
#'   ...
#' }
#' @source Elaboración propia a partir de Rodriguez et al. (2018), INaturalist, etc.
"List_tax"
