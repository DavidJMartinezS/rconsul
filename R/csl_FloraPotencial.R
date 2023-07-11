#' FLora Potencial
#'
#' @param shape objeto sf.
#' @param link.simbio links de la o las páginas de los pisos vegetacionales correspondientes. Obtenerlos de <https://simbio.mma.gob.cl/Ecosistemas/>.
#' @param save Logical. Default False.
#' @param file Si save es TRUE. Ruta y nombre del archivo con terminal xlsx.
#'
#' @return flora potencial de acuerdo a Gajardo (1994) y Luebert y Pliscoff (2017)
#' @export
#'
#' @importFrom sf sf_use_s2 st_intersection st_transform st_zm
#' @importFrom dplyr filter group_by summarise mutate select tally
#' @importFrom tidyr unnest_legacy pivot_wider replace_na
#' @importFrom stringr str_extract_all str_split str_trim str_detect str_c str_remove str_to_lower word
#' @importFrom rvest read_html html_text2
#' @importFrom tibble as_tibble_col
#' @importFrom openxlsx write.xlsx
#' @importFrom magrittr `%>%`
#' @importFrom purrr map
#'
#' @examples
#' csl_FloraPotencial(shape = Pica, link.simbio = "https://simbio.mma.gob.cl/Ecosistemas/Details/51", save = T, file = "~/ruta/carpeta/file.xlsx")
csl_FloraPotencial <- function(shape, link.simbio, save = FALSE, file){
  sf::sf_use_s2(use_s2 = F)

  Gajardo_AI <- sf::st_intersection(Formaciones_Gajardo, shape %>% sf::st_transform(4326) %>% sf::st_zm(drop = T)) %>%
    dplyr::pull(Formacion) %>%
    unique()

  Gajardo <- Asociaciones_Gajardo %>%
    dplyr::filter(stringr::str_to_lower(FORMACIÓN) %in% stringr::str_to_lower(Gajardo_AI)) %>%
    dplyr::group_by(FORMACIÓN) %>%
    dplyr::summarise(asd = list(ASOCIACIÓN)) %>%
    dplyr::mutate(SP = purrr::map(asd,function(x){
      Especies_Gajardo %>%
        dplyr::filter(ASOCIACIÓN %in% x) %>%
        dplyr::group_by(ESPECIE) %>%
        dplyr::tally() %>%
        dplyr::mutate(Gajardo = "X") %>%
        dplyr::select(-n)})) %>%
    dplyr::select(FORMACIÓN, SP) %>%
    tidyr::unnest_legacy() %>%
    tidyr::pivot_wider(names_from = FORMACIÓN, values_from = Gajardo)

  LyP.1 <- rvest::read_html(link.simbio) %>%
    rvest::html_text2() %>%
    stringr::str_extract_all("(?<=Composición florística\r\n\r).*(?=\r\n\r\n\r\n\r\n\r\n\r)") %>%
    stringr::str_split(",") %>%
    .[[1]] %>%
    stringr::str_trim()

  LyP.2 <- rvest::read_html(link.simbio) %>%
    rvest::html_text2() %>%
    stringr::str_extract_all("(?<=Descripción de la vegetación nativa dominante\r\n\r).*(?=\r\n\r\n\r\n\r\n\r\n\r)") %>%
    stringr::str_extract_all("(?<!\\. )\\b[A-Z][a-z]+ [a-z]+\\b") %>%
    .[[1]] %>%
    stringr::str_trim()

  LyP <- c(LyP.1,LyP.2) %>%
    tibble::as_tibble_col('ESPECIE') %>%
    dplyr::mutate(`Luebert y Pliscoff (2017)`='X')

  for (i in 1:length(LyP)) {
    if (stringr::str_detect(LyP[i, 1], 'var.')) {
      LyP[i, 1] <- LyP[i, 1] %>%
        stringr::word(1, 2) %>%
        stringr::str_c(collapse = ' ')
    }
    if (stringr::str_detect(LyP[i, 1], '\\.$')) {
      LyP[i, 1] <- LyP[i, 1] %>%
        stringr::str_remove('\\.$')
    }
    if (stringr::word(LyP[i, 1], 1) %>% stringr::str_detect('\\.$')) {
      LyP[i, 1] <- paste(LyP[i - 1, 1] %>% stringr::word(1), LyP[i, 1] %>% stringr::word(2))
    }
  }

  if (save) {
    openxlsx::write.xlsx(file = file)
  }

  G_LyP <- merge(Gajardo, LyP, all = TRUE) %>%
    tidyr::replace_na(' ')

  warning("Tenga en cuenta que los nombres taxonómicos de las especies pueden ir cambiando en el tiempo.\nLos nombres de las especies que arroja esta función son las utilizadas en Los libro de Gajardo (1994) y Luebert y Pliscoff (2017), por lo que hay nombres de especies no se encuentran actualizados.\nPor favor no olvide revisar los nombres!!")
  return(G_LyP)
}
