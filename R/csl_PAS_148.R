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
csl_PAS_predio <- function(predio.shp, rol, Nombre.predio, save = F, NOMBREPREDIO, path = getwd()){
  predio <- predio.shp %>%
    dplyr::mutate(
      Nom_Predio = Nombre.predio,
      Rol = rol %>%  as.character(),
      Sup_ha = sf::st_area(geometry) %>%
        units::set_units(hectare)) %>%
    units::drop_units() %>%
    dplyr::select(Nom_Predio, Rol, Sup_ha)
  if (save == T) {
    sf::write_sf(predio, file.path(path, paste0('Limite_Predial.', NOMBREPREDIO, '.shp')))
  }
  return(predio)
}

cart_PAS_148 <- function(predios, BN.intervenir, formaciones, suelo=NULL, cut.x.buffer=5000, dem=NULL, step_curv_nivel=50, OSM=T, localidad, save = T, path=NULL){
  # cargar caminos
  caminos.mop <- sf::read_sf('~/TEBAL/DATA SIG/Red_Vial_Chile/Red_Vial_Chile_31_01_2023.gdb') %>%
    sf::st_transform(32719) %>%
    sf::st_zm(drop = T) %>%
    sf::st_set_geometry('geometry')
  caminos.osm <- osmdata::getbb(localidad) %>%
    osmdata::opq() %>%
    osmdata::add_osm_feature(key = "highway",
                             value = c("motorway", "primary","secondary", "tertiary","residential", "living_street", "unclassified","service", "footway")) %>%
    osmdata::osmdata_sf()
  # cargar DEM
  if (is.null(dem)) {
    dem_stars <- elevatr::get_elev_raster(sf::st_buffer(predios,100) %>% sf::st_transform(4326), source = "ALOS_PALSAR", z = 12) %>%
      raster::projectRaster(crs=32719) %>%
      sf::st_as_stars() %>%
      `names<-`("elev")
  } else {
    dem_stars <- stars::st_as_stars(dem) %>% `names<-`("elev") %>% .[sf::st_buffer(predio.shp,1000)]
  }
  # cargar comunas
  comunas <- read_sf('~/TEBAL/DATA SIG/DivisionPoliticoAdministrativa2020/COMUNA/COMUNAS_2020.shp') %>% sf::st_transform(32719) %>% sf::st_zm(drop = T)

  ### generar y guardar capas
  for (i in 1:nrow(predios)) {
    nombre.predio <- predios$Nom_Predio[i]
    rol <- predios$Rol[i]
    carpeta <-
      # Limite predial
      predio <- predios[i,] %>% dplyr::mutate(Sup_ha=st_area(geometry) %>% set_units(hectare)) %>% drop_units() %>% dplyr::select(Nom_Predio,Rol,Sup_ha)

    # Suelo
    if (is.null(suelo)) {
      suelos <- st_as_sf(st_sfc(),crs=32719)
    }
    suelos.chile <- suelo %>% st_transform(32719) %>% st_zm(drop = T)
    suelos <- sf::st_intersection(suelos.chile,predio) %>% dplyr::mutate(Clase_uso=TEXTCAUS,Sup_ha=sf::st_area(geometry) %>% units::set_units(hectare)) %>% units::drop_units() %>% dplyr::select(Nom_Predio,Clase_uso,Sup_ha)

    # Rodales
    rodal <- sf::st_intersection(BN.intervenir,suelo) %>%
      tibble::rowid_to_column('ID') %>%
      dplyr::mutate(N_rodal=paste0('A',ID),Sup_ha=sf::st_area(geometry) %>% units::set_units(hectare)) %>%
      dplyr::select(Nom_Predio,N_rodal,Tipo_Bos,Tipo_For,Sup_ha)

    # Uso actual
    uso <- sf::st_intersection(formaciones,predio) %>%
      dplyr::group_by(Uso_actual) %>% dplyr::tally() %>%
      dplyr::mutate(Nom_Predio=nombre.predio,Sup_ha=sf::st_area(geometry) %>% units::set_units(hectare)) %>%
      units::drop_units() %>%
      dplyr::select(Nom_Predio,Uso_actual,Sup_ha)

    # Caminos
    cami.mop <- caminos.mop[st_buffer(predio,cut.x.buffer),]
    cami.osm <- caminos.osm$osm_lines %>% st_transform(32719)
    cami.osm$incluido <- sapply(cami.osm$name, function(nombre) any(str_detect(cami.mop$NOMBRE_CAMINO, nombre)))
    cami.int <- cami.osm %>% mutate(dist=st_distance(geometry,cami.mop %>% st_union())) %>% units::drop_units() %>%
      filter(official_name %in% cami.mop$NOMBRE_CAMINO|(incluido==T & dist<1))
    cami.mop.2 <- cami.mop %>%
      mutate(Nom_Predio=predio$Nom_Predio[i],
             Tipo_Cam=if_else(str_detect(CLASIFICACION,'Internacional|Nacional|Regional Principal'),1,
                              if_else(str_detect(CLASIFICACION,'Regional Provincial|Regional Comunal'),2,
                                      if_else(str_detect(CLASIFICACION,'Acceso'),3,4)))) %>%
      dplyr::select(Nom_Predio,Tipo_Cam)
    cami.osm.2 <- cami.osm %>% dplyr::filter(!osm_id %in% cami.int$osm_id) %>%
      mutate(Nom_Predio=nombre.predio,Tipo_Cam=4) %>% dplyr::select(Nom_Predio,Tipo_Cam)cami.mop <- caminos.mop[st_buffer(predio.shp,cut.x.buffer),]
    cami.osm <- caminos.osm$osm_lines %>% st_transform(32719)
    cami.osm$incluido <- sapply(cami.osm$name, function(nombre) any(str_detect(cami.mop$NOMBRE_CAMINO, nombre)))
    cami.int <- cami.osm %>% mutate(dist=st_distance(geometry,cami.mop %>% st_union())) %>% units::drop_units() %>%
      filter(official_name %in% cami.mop$NOMBRE_CAMINO|(incluido==T & dist<1))
    cami.mop.2 <- cami.mop %>%
      mutate(Nom_Predio=nombre.predio,
             Tipo_Cam=if_else(str_detect(CLASIFICACION,'Internacional|Nacional|Regional Principal'),1,
                              if_else(str_detect(CLASIFICACION,'Regional Provincial|Regional Comunal'),2,
                                      if_else(str_detect(CLASIFICACION,'Acceso'),3,4)))) %>%
      dplyr::select(Nom_Predio,Tipo_Cam)
    cami.osm.2 <- cami.osm %>% dplyr::filter(!osm_id %in% cami.int$osm_id) %>%
      mutate(Nom_Predio=nombre.predio,Tipo_Cam=4) %>% dplyr::select(Nom_Predio,Tipo_Cam)
    if (is.null(OSM)) {
      caminos <- cami.mop.2 %>% st_intersection(st_buffer(predio,cut.x.buffer)) %>% group_by(Nom_Predio,Tipo_Cam) %>% tally() %>% dplyr::select(-n) %>% ungroup() %>% mutate_at('Tipo_Cam',as.character)
    } else {
      caminos <- bind_rows(cami.mop.2,cami.osm.2) %>% st_intersection(st_buffer(predio,cut.x.buffer)) %>% group_by(Nom_Predio,Tipo_Cam) %>% tally() %>% dplyr::select(-n) %>% ungroup() %>% mutate_at('Tipo_Cam',as.character)
    }

    # Hidrología
    red_hidr <- read_sf('~/TEBAL/DATA SIG/Hidrografia_V2/Hidrografia_V2 (1).shp') %>% st_transform(32719) %>% st_zm(drop = T)
    comuna <- comunas[st_buffer(predio,200),]
    red_hidro <- red_hidr %>% st_intersection(comuna) %>%
      st_intersection(st_buffer(predio,100)) %>%
      mutate(Nom_Predio=nombre.predio,
             Tipo_dren=if_else(TIPO =='Rio',1,
                               if_else(TIPO =='Estero',2,
                                       if_else(TIPO =='Arroyo',3,
                                               if_else(TIPO =='Quebrada',4,5)))),
             Tipo_Perma=2) %>%
      group_by(Nom_Predio,Tipo_dren,Tipo_Perma) %>% tally() %>% select(-n) %>%
      filter(!Tipo_dren==5) %>% ungroup() %>% mutate_at('Tipo_dren',as.character)

    # Curvas de nivel
    curv_niv <- st_contour(
      dem_stars,
      contour_lines = T,
      breaks = seq(
        plyr::round_any(min(dem_stars$elev, na.rm = T), step, ceiling),
        plyr::round_any(max(dem_stars$elev, na.rm = T), step, floor),
        step
      )
    ) %>% st_intersection(st_buffer(predio, 1000)) %>%
      dplyr::group_by(elev) %>% dplyr::tally() %>%
      dplyr::mutate(Nom_Predio = nombre.predio, Cot_curva = elev) %>%
      dplyr::select(Nom_Predio, Cot_curva)

    # Pendiente
    dem_stars <- dem_stars[st_buffer(predio,100)]
    dem_slope <- starsExtra::slope(dem_stars)
    dem_slope_per <- tan(dem_slope*pi/180)*100

    slope.reclass <- dem_slope
    slope.reclass[slope.reclass <= 3] <- 1
    slope.reclass[slope.reclass > 3 & slope.reclass <= 5] <- 2
    slope.reclass[slope.reclass > 5 & slope.reclass<= 10] <- 3
    slope.reclass[slope.reclass > 10 & slope.reclass<= 15] <- 4
    slope.reclass[slope.reclass > 15 & slope.reclass<= 30] <- 5
    slope.reclass[slope.reclass > 30 & slope.reclass<= 58] <- 6
    slope.reclass[slope.reclass > 58 & slope.reclass<= 100] <- 7
    slope.reclass[slope.reclass > 100] <- 8

    slope.sf <- sf::st_as_sf(slope.reclass) %>% drop_units()
    slope <-
      sf::st_intersection(slope.sf, predio) %>% dplyr::group_by(slope) %>% dplyr::tally() %>%
      dplyr::mutate(
        Nom_Predio = nombre.predio,
        Ran_Pend = dplyr::case_when(
          slope == 1 ~ '0-3%',
          slope == 2 ~ '3-5%',
          slope == 3 ~ '5-10%',
          slope == 4 ~ '10-15%',
          slope == 5 ~ '15-30%',
          slope == 6 ~ '30-58%',
          slope == 7 ~ '58-100%',
          slope == 8 ~ '>100'
        ),
        Sup_ha = sf::st_area(geometry) %>% units::set_units(hectare)
      ) %>% units::drop_units() %>%
      dplyr::select(Nom_Predio, Ran_Pend, Sup_ha)

    if (save==T) {
      carpeta <- file.path(path,'CARPETAS PREDIALES',paste('PREDIO',i,'ROL',predios$Rol[i])) %>% dir.create()
      NOMBREPREDIO <- predios$Nom_abrev[i]
      list.shp <- list(predio,suelo,rodal,uso,caminos,red_hidro,curv_niv,slope)
      list.names <- c('Limite_Predial','Suelos','Rodal','Uso_actual','Caminos','Hidrografia','Curvas_niv','Rangos_pend')
      for (j in seq_along(list.shp)) {
        if (nrow(list.shp[[j]])>0) {
          write_sf(list.shp[[j]],file.path(carpeta,paste0(list.names[j],NOMBREPREDIO,'.shp')))
        } else {
          print(cat("The shapefile",substitute(list.shp[[j]]),"is empty, so it was not saved"))
        }
      }
    } else {
      return(list.shp)
    }
  }
}
