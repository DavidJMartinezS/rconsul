#' Descripción Flora
#'
#' @description
#' `csl_Flora` arroja multiples resultados florísticos a partir del listado florístico y puntos de muestreo.
#' Es importante que los campos del listado taxonómico sean los mismos que se muestran en `Listado_taxonómico`.
#' Si utiliza el parámetro `save = T` entonces se creará una carpeta en el directorio que desee.
#'
#' @param list.tax Listado taxonómico
#' @param campañas Campañas con parcelas de muestreo
#' @param save Logical. TRUE si desea guardar. FALSE default
#' @param file Directorio donde desea crear carpeta con resultados. Directorio de trabajo default.
#'
#' @return lista de resultados florísticos
#' @export
#'
#' @importFrom dplyr count group_by summarise mutate tally n select n_distinct filter slice case_when rename
#' @importFrom stringr str_remove str_trim str_remove_all str_detect str_c str_to_lower
#' @importFrom tibble as_tibble rowid_to_column
#' @importFrom janitor adorn_totals
#' @importFrom magrittr `%>%`
#'
#' @examples
#' csl_Flora(Listado_taxonómico,campañas)
#' csl_Flota(Listado_taxonómico,campañas, save = T)
#'
csl_Flora <- function(list.tax, campañas, save = F, file = getwd()){
  if (names(list.tax)!=names(Listado_taxonómico)) {
    stop('Los campos del listado florísticos deben ser identicos a los del `Listado_taxonómico`')
  }

  N <- nrow(list.tax)
  # Riqueza floristica y nomenclatura
  R_1 <- list.tax %>%
    dplyr::count(DIVISIÓN, CLASE, FAMILIA, name = 'N° DE ESPECIES') %>%
    dplyr::mutate('PORCENTAJE (%)' = `N° DE ESPECIES` / N * 100) %>%
    tibble::as_tibble()

  R_2 <- list.tax %>%
    dplyr::group_by(CLASE, FAMILIA, ESPECIE, NOMBRE.COMÚN) %>%
    dplyr::tally() %>%
    dplyr::select(-n) %>%
    `names<-`(c('CLASE', 'FAMILIA', 'NOMBRE CIENTÍFICO', 'NOMBRE COMÚN'))

  R_3 <- list.tax %>%
    dplyr::group_by(DIVISIÓN, CLASE) %>%
    dplyr::summarise(
      'N° FAMILIAS' = dplyr::n_distinct(FAMILIA),
      'N° GÉNEROS' = dplyr::n_distinct(GÉNERO),
      'N° ESPECIES' = dplyr::n()
    ) %>%
    janitor::adorn_totals()

  # Abundancia
  symbol <- c('p', 'r', '+', '1', '2', '3', '4', '5')
  significado <-
    c(
      'Registro de especie fuera de la unidad de muestro, pero observada en la misma formación vegetal',
      'Muy pocos individuos, cobertura insignificante (<5%)',
      'Pocos individuos, cobertura insignificante (<5%)',
      'Numerosos individuos, cobertura insignificante (<5%)',
      '5-25%',
      '25-50%',
      '50-75%',
      '75-100%'
    )

  IAB <- campañas %>%
    dplyr::filter(!IAB == '-') %>%
    dplyr::mutate(
      Nombre.Científico = stringr::str_remove(Nombre.Científico, "\\(.*?\\)") %>% stringr::str_trim(),
      IAB = stringr::str_remove_all(IAB, "\\(|\\)") %>% stringr::str_trim() %>% stringr::str_to_lower()
    ) %>%
    dplyr::group_by(Nombre.Científico, IAB) %>%
    dplyr::tally() %>%
    dplyr::slice(which.max(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(IAB = factor(IAB, symbol)) %>%
    dplyr::mutate(
      Cobertura = dplyr::case_when(
        IAB == 'p' ~ significado[1],
        IAB == 'r' ~ significado[2],
        IAB == '+' ~ significado[3],
        IAB == '1' ~ significado[4],
        IAB == '2' ~ significado[5],
        IAB == '3' ~ significado[6],
        IAB == '4' ~ significado[7],
        IAB == '5' ~ significado[8]
      )
    ) %>%
    tibble::rowid_to_column('id') %>%
    dplyr::select(id, Nombre.Científico, Cobertura, IAB) %>%
    `names<-`(
      c(
        'N° ID',
        'ESPECIE/NOMBRE CIENTÍFICO',
        'COBERTURA (%)',
        'COBERTURA (BRAUN- BLANQUET, 1979)'
      )
    )

  # Tipos biológicos
  TB <- list.tax %>%
    dplyr::count(HÁBITO.DE.CRECIMIENTO) %>%
    dplyr::mutate(p=n/N*100) %>%
    `names<-`(c('TIPO BIOLÓGICO','N° DE ESPECIES','PORCENTAJE (%)')) %>%
    janitor::adorn_totals()

  # Origen fitogeografico
  OF <- list.tax %>%
    dplyr::count(ORIGEN.FITOGEOGRÁFICO) %>%
    dplyr::mutate(o=n/N*100) %>%
    `names<-`(c('ORIGEN GEOGRÁFICO','N° DE ESPECIES','PORCENTAJE (%)')) %>%
    janitor::adorn_totals()

  # Especies originarias
  EO <- list.tax %>%
    dplyr::filter(`DS.68/2009` %>% stringr::str_detect('S')) %>%
    dplyr::select(ESPECIE) %>%
    tibble::rowid_to_column('N° ID') %>%
    dplyr::rename('ESPECIE/NOMBRE CIENTÍFICO'=ESPECIE)

  # Especies categoría
  EC <- list.tax %>%
    dplyr::filter(!(ESTADO.DE.CONSERVACIÓN %in% c('-',NA))) %>%
    dplyr::select(ESPECIE,ESTADO.DE.CONSERVACIÓN,REFERENCIA.O.DECRETO.CATEGORÍA.VIGENTE) %>%
    `names<-`(c('ESPECIE/NOMBRE CIENTÍFICO','CATEGORÍA DE CONSERVACIÓN','REFERENCIA O DECRETO'))

  carpeta.flora <- file.path(file, 'Flora') %>% dir.create()
  list.flora <-
    list(
      'Riqueza_1' = R_1,
      'Riqueza_2' = R_2,
      'Riqueza_3' = R_3,
      'Abundancia' = IAB,
      'Tipos_Biológicos' = TB,
      'Origen_Fitogeográfico' = OF,
      'Especies_Originarias' = EO,
      'Especies_Categoría' = EC
    )
  list.names <- names(list.flora) %>%
    lapply(stringr::str_c,'.xlsx') %>%
    unlist()

  if (save) {
    for (i in seq_along(list.flora)) {
      openxlsx::write.xlsx(x = list.flora[[i]],file = file.path(carpeta.flora,list.names[1]))
    }
  }
  return(list.flora)
}
