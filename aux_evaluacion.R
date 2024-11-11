calcular_oferta <- function(
    equip_layer_path,
    nombre_equipamiento,
    radio,
    pesos_equipamientos
) {
  # Definir la ruta de salida
  output_path <- file.path(nombre_equipamiento, paste0("kernel_", nombre_equipamiento, ".tif"))
  
  limite_metropolitano_path <- "C:/Users/manue/IMEPLAN/Intraurbanos_Suficiencia/datos/Limite_metropolitano/Limite_metropolitano.shp"
  
  # Asegurarse de que el directorio de salida exista
  if (!dir.exists(nombre_equipamiento)) {
    dir.create(nombre_equipamiento, recursive = TRUE)
  }
  
  # Cargar y transformar la capa de entrada
  input_layer <- st_read(equip_layer_path, quiet = TRUE)
  input_layer <- st_transform(input_layer, crs = 32613)
  
  # Definir los parámetros para el algoritmo de QGIS
  params <- list(
    INPUT = input_layer,
    RADIUS = radio,
    RADIUS_FIELD = NULL,
    PIXEL_SIZE = 20,
    WEIGHT_FIELD = pesos_equipamientos,
    KERNEL = 0,           # Kernel Quartic (o 0 como valor predeterminado)
    DECAY = 0,
    OUTPUT_VALUE = 0,     # Cambiar a Density si es necesario
    OUTPUT = output_path  # Ruta de salida explícita
  )
  
  # Ejecutar el algoritmo de QGIS con manejo de errores
  tipo_geometria <- st_geometry_type(input_layer)[1]  # Toma el primer tipo de geometría
  
  # Usar if...else para asignar el ID del algoritmo según el tipo de geometría
  if (grepl("POINT", tipo_geometria)) {
    alg_id <- "qgis:heatmapkerneldensityestimation"
  } else if (grepl("LINE", tipo_geometria)) {
    alg_id <- "native:linedensity"
  } else {
    stop("Tipo de geometría no soportado. Solo se admiten geometrías de 'Point' o 'Line'.")
  }
  
  result <- tryCatch({
    do.call(qgis_run_algorithm, c(list(algorithm = alg_id), params))
  }, error = function(e) {
    message("Ocurrió un error:")
    message(e$message)
    NULL
  })
  
  # Verificar si el resultado es válido
  if (is.null(result)) {
    message("La ejecución del algoritmo falló.")
    return(NULL)
  }
  
  # Cargar el raster resultante y el shapefile de límite
  raster <- rast(output_path)
  shapefile <- st_read(limite_metropolitano_path, quiet = TRUE)
  
  # Asegurarse de que ambos tengan el mismo sistema de coordenadas
  shapefile <- st_transform(shapefile, crs(raster))
  
  # Obtener la extensión (bounding box) y extender el raster
  bbox <- ext(shapefile)
  extended_raster <- extend(raster, bbox)
  
  # Establecer el valor de NoData a -9999 y reemplazar con 0
  NAflag(extended_raster) <- -9999
  extended_raster[extended_raster == -9999] <- 0
  extended_raster[is.na(extended_raster)] <- 0
  
  # Guardar el raster resultante
  output_extended_path <- file.path(nombre_equipamiento, paste0("kernel_", nombre_equipamiento, "_limite_amg.tif"))
  writeRaster(extended_raster, output_extended_path, overwrite = TRUE)
  
  message("El procesamiento y guardado del raster se completaron con éxito.")
  
  return(output_extended_path)
}

calcular_demanda <- function(
    manzanas_layer_path,
    nombre_equipamiento,
    radio,
    pesos_manzanas
) {
  # Definir la ruta de salida
  output_path <- file.path(nombre_equipamiento, paste0("kernel_","manzanas_", nombre_equipamiento, ".tif"))
  
  limite_metropolitano_path <- "C:/Users/manue/IMEPLAN/Intraurbanos_Suficiencia/datos/Limite_metropolitano/Limite_metropolitano.shp"
  
  # Asegurarse de que el directorio de salida exista
  if (!dir.exists(nombre_equipamiento)) {
    dir.create(nombre_equipamiento, recursive = TRUE)
  }
  
  # Cargar y transformar la capa de entrada
  input_layer <- st_read(manzanas_layer_path, quiet = TRUE)
  input_layer <- st_transform(input_layer, crs = 32613)
  
  # Definir los parámetros para el algoritmo de QGIS
  params <- list(
    INPUT = input_layer,
    RADIUS = radio,
    RADIUS_FIELD = NULL,
    PIXEL_SIZE = 20,
    WEIGHT_FIELD = pesos_manzanas,
    KERNEL = 0,           # Kernel Quartic (o 0 como valor predeterminado)
    DECAY = 0,
    OUTPUT_VALUE = 0,     # Cambiar a Density si es necesario
    OUTPUT = output_path  # Ruta de salida explícita
  )
  
  # Ejecutar el algoritmo de QGIS con manejo de errores
  alg_id <- "qgis:heatmapkerneldensityestimation"
  result <- tryCatch({
    do.call(qgis_run_algorithm, c(list(algorithm = alg_id), params))
  }, error = function(e) {
    message("Ocurrió un error:")
    message(e$message)
    NULL
  })
  
  # Verificar si el resultado es válido
  if (is.null(result)) {
    message("La ejecución del algoritmo falló.")
    return(NULL)
  }
  
  # Cargar el raster resultante y el shapefile de límite
  raster <- rast(output_path)
  shapefile <- st_read(limite_metropolitano_path, quiet = TRUE)
  
  # Asegurarse de que ambos tengan el mismo sistema de coordenadas
  shapefile <- st_transform(shapefile, crs(raster))
  
  # Obtener la extensión (bounding box) y extender el raster
  bbox <- ext(shapefile)
  extended_raster <- extend(raster, bbox)
  
  # Establecer el valor de NoData a -9999 y reemplazar con 0
  NAflag(extended_raster) <- -9999
  extended_raster[extended_raster == -9999] <- 0
  extended_raster[is.na(extended_raster)] <- 0
  
  # Guardar el raster resultante
  output_extended_path <- file.path(nombre_equipamiento, paste0("kernel_","manzanas_", nombre_equipamiento, "_limite_amg.tif"))
  writeRaster(extended_raster, output_extended_path, overwrite = TRUE)
  
  message("El procesamiento y guardado del raster se completaron con éxito.")
  
  return(output_extended_path)
}

indice_norm <- function(
    nombre_equipamiento,
    constante_norm
) {
  # Cargar los rasters
  primarias <- rast(paste0(nombre_equipamiento,"/kernel_",nombre_equipamiento,"_limite_amg.tif"))
  manzanas <- rast(paste0(nombre_equipamiento,"/kernel_manzanas_", nombre_equipamiento,"_limite_amg.tif"))
  
  # Definir un raster base (primarias) como referencia para el alineamiento
  manzanas_aligned <- resample(manzanas, primarias, method = "bilinear")
  
  # Realizar la resta
  resultado <- primarias - manzanas_aligned
  
  writeRaster(resultado, paste0(nombre_equipamiento,"/resultado_resta.tif"), overwrite = TRUE)
  
  # Aplicar la normalización al raster
  indice_norm <- (2 / (1 + exp(constante_norm * resultado))) - 1
  
  # Guardar el raster normalizado
  writeRaster(indice_norm, paste0(nombre_equipamiento,"/resultado_normalizado.tif"), overwrite = TRUE)
}

results <- function(
    nombre_equipamiento,
    manzanas_layer_path
) {
  
  raster_normalizado <- rast(paste0(nombre_equipamiento,"/resultado_normalizado.tif"))
  
  input_layer <- st_read(manzanas_layer_path, quiet = TRUE)
  manzanas <- st_transform(input_layer, crs = 32613)
  
  # Asegurarse de que ambos tienen el mismo CRS
  # Cambiar el CRS de manzanas para que coincida con el del raster si es necesario
  if (!st_crs(manzanas) == crs(raster_normalizado)) {
    manzanas <- st_transform(manzanas, crs(raster_normalizado))
  }
  
  # Convertir el shapefile a puntos (puedes usar centroides o los puntos del contorno)
  manzanas_centroid <- st_centroid(manzanas)
  
  # Extraer los valores del raster en las ubicaciones de los puntos
  valores_extraidos <- terra::extract(raster_normalizado, vect(manzanas_centroid))
  
  # Asegurarse de que los valores extraídos se asignan correctamente
  # `valores_extraidos` es un data.frame con la estructura: ID del punto y valor del raster
  
  # Combinar los valores extraídos con el shapefile original
  manzanas$valor_intersectado <- valores_extraidos[, 2]
  
  # Crear la columna de categorías según los rangos proporcionados
  manzanas <- manzanas %>%
    mutate(categoria = case_when(
      valor_intersectado >= -1 & valor_intersectado < -0.60 ~ "Proximidad media con alto déficit de equipamiento",
      valor_intersectado >= -0.60 & valor_intersectado < -0.20 ~ "Proximidad alta con déficit de equipamiento",
      valor_intersectado >= -0.20 & valor_intersectado <= 0.20 ~ "Proximidad alta con balance de equipamiento",
      valor_intersectado > 0.20 & valor_intersectado <= 0.60 ~ "Proximidad alta con superávit de equipamientos",
      valor_intersectado > 0.60 & valor_intersectado <= 1 ~ "Proximidad muy alta con alto superávit de equipamiento",
      TRUE ~ "Sin categoría"
    ))
  
  manzanas <- manzanas %>%
    mutate(CV_Categoria = case_when(
      valor_intersectado >= -1 & valor_intersectado < -0.60 ~ 1,
      valor_intersectado >= -0.60 & valor_intersectado < -0.20 ~ 2,
      valor_intersectado >= -0.20 & valor_intersectado <= 0.20 ~ 3,
      valor_intersectado > 0.20 & valor_intersectado <= 0.60 ~ 4,
      valor_intersectado > 0.60 & valor_intersectado <= 1 ~ 5,
      TRUE ~ NA_real_  # Usamos NA_real_ para representar valores faltantes como numéricos
    ))
  
  
  # Guardar el shapefile resultante con la nueva columna
  st_write(manzanas, paste0(nombre_equipamiento,"/manzanas_resultados.shp"), delete_layer = TRUE)
}

results1 <- function(
    nombre_equipamiento,
    manzanas_layer_path,
    poligonos_layer_path
) {
  
  raster_normalizado <- rast(paste0(nombre_equipamiento, "/resultado_normalizado.tif"))
  
  # Leer las capas de puntos y polígonos
  input_layer <- st_read(manzanas_layer_path, quiet = TRUE)
  manzanas <- st_transform(input_layer, crs = 32613)
  
  poligonos <- st_read(poligonos_layer_path, quiet = TRUE)
  poligonos <- st_transform(poligonos, crs = st_crs(manzanas)) # Asegurarse de que ambos tienen el mismo CRS
  
  # Transformar puntos al mismo CRS que el raster
  if (!st_crs(manzanas) == crs(raster_normalizado)) {
    manzanas <- st_transform(manzanas, crs(raster_normalizado))
  }
  
  # Convertir el shapefile de manzanas a puntos y extraer valores del raster
  manzanas_centroid <- st_centroid(manzanas)
  valores_extraidos <- terra::extract(raster_normalizado, vect(manzanas_centroid))
  manzanas_centroid$valor_intersectado <- valores_extraidos[, 2]
  
  # Asignar categorías
  manzanas_centroid <- manzanas_centroid %>%
    mutate(
      categoria = case_when(
        valor_intersectado >= -1 & valor_intersectado < -0.60 ~ "Proximidad media con alto déficit de equipamiento",
        valor_intersectado >= -0.60 & valor_intersectado < -0.20 ~ "Proximidad alta con déficit de equipamiento",
        valor_intersectado >= -0.20 & valor_intersectado <= 0.20 ~ "Proximidad alta con balance de equipamiento",
        valor_intersectado > 0.20 & valor_intersectado <= 0.60 ~ "Proximidad alta con superávit de equipamientos",
        valor_intersectado > 0.60 & valor_intersectado <= 1 ~ "Proximidad muy alta con alto superávit de equipamiento",
        TRUE ~ "Sin categoría"
      ),
      CV_Categoria = case_when(
        valor_intersectado >= -1 & valor_intersectado < -0.60 ~ 1,
        valor_intersectado >= -0.60 & valor_intersectado < -0.20 ~ 2,
        valor_intersectado >= -0.20 & valor_intersectado <= 0.20 ~ 3,
        valor_intersectado > 0.20 & valor_intersectado <= 0.60 ~ 4,
        valor_intersectado > 0.60 & valor_intersectado <= 1 ~ 5,
        TRUE ~ NA_real_
      )
    )
  
  # Realizar intersección espacial y transferir los valores al shapefile de polígonos
  poligonos_con_valores <- st_join(poligonos, manzanas_centroid, left = TRUE)
  
  # Guardar el shapefile de polígonos con los nuevos valores
  st_write(poligonos_con_valores, paste0(nombre_equipamiento, "/poligonos_resultados.shp"), delete_layer = TRUE)
}