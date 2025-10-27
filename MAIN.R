# ================================================================
# 3D City Render with OSM + rayrender
# Area: Nairobi, Kenya
# Author: Alfrick Onyinkwa adaptation
# ================================================================

# ---- Libraries ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(osmdata, sf, dplyr, rayrender, curl)

# ---- AOI (Nairobi) ----
lon <- 36.8219
lat <- -1.2921

ctr_wgs <- sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326)

utm_epsg <- function(lon, lat) {
  stopifnot(is.finite(lon), is.finite(lat))
  if (lat >= 84) return(32661)
  if (lat <= -80) return(32761)
  zone <- floor((lon + 180) / 6) + 1
  if (lat >= 0) 32600 + zone else 32700 + zone
}

crs_m <- utm_epsg(lon, lat)
ctr_m <- sf::st_transform(ctr_wgs, crs_m)
radius <- 1000
aoi_m <- sf::st_buffer(ctr_m, radius)
aoi_wgs <- sf::st_transform(aoi_m, 4326)
bb <- sf::st_bbox(aoi_wgs)

# ---- Helper to safely query OSM ----
safe_osm_query <- function(qfun) {
  servers <- c(
    "https://overpass-api.de/api/interpreter",
    "https://overpass.kumi.systems/api/interpreter",
    "https://overpass.openstreetmap.fr/api/interpreter",
    "https://overpass-api.nextgis.com/api/interpreter"
  )
  for (srv in servers) {
    options(osmdata.overpass_server = srv)
    message("Trying Overpass server: ", srv)
    tryCatch({
      result <- qfun()
      if (!is.null(result)) return(result)
    },
    error = function(e) {
      message("Server failed: ", srv)
    })
  }
  stop("All Overpass servers unavailable or no internet connection.")
}

# ---- Get polygons / lines safely ----
get_polys <- function(q) {
  x <- tryCatch(osmdata::osmdata_sf(q)$osm_polygons, error = function(e) NULL)
  if (is.null(x)) {
    sf::st_sf(geometry = sf::st_sfc(crs = 4326))
  } else {
    sf::st_make_valid(x)
  }
}

get_lines <- function(q) {
  x <- tryCatch(osmdata::osmdata_sf(q)$osm_lines, error = function(e) NULL)
  if (is.null(x)) {
    sf::st_sf(geometry = sf::st_sfc(crs = 4326))
  } else {
    sf::st_make_valid(x)
  }
}

# ---- OSM Pulls ----
if (!curl::has_internet()) stop("❌ No internet connection detected.")

bld <- safe_osm_query(function() get_polys(osmdata::opq(bb) |>
                                            osmdata::add_osm_feature("building")))

landuse <- safe_osm_query(function() get_polys(osmdata::opq(bb) |>
                                                osmdata::add_osm_feature("landuse")))

green_cover <- subset(
  landuse, landuse %in% c("grass", "recreation_ground", "forest", "greenery")
)

parks <- dplyr::bind_rows(
  green_cover,
  safe_osm_query(function() get_polys(osmdata::opq(bb) |>
                                        osmdata::add_osm_feature("leisure", "park"))),
  safe_osm_query(function() get_polys(osmdata::opq(bb) |>
                                        osmdata::add_osm_feature("natural", c("wood", "scrub"))))
)

water <- dplyr::bind_rows(
  safe_osm_query(function() get_polys(osmdata::opq(bb) |>
                                        osmdata::add_osm_feature("natural", "water"))),
  safe_osm_query(function() get_polys(osmdata::opq(bb) |>
                                        osmdata::add_osm_feature("water", "river")))
)

roads <- safe_osm_query(function() get_lines(osmdata::opq(bb) |>
                                              osmdata::add_osm_feature("highway")))

rails <- safe_osm_query(function() get_lines(osmdata::opq(bb) |>
                                              osmdata::add_osm_feature("railway")))

# ---- Clip & project ----
clip_m <- function(x) {
  if (is.null(x) || nrow(x) == 0) {
    return(sf::st_sf(geometry = sf::st_sfc(crs = crs_m)))
  }
  x <- suppressWarnings(sf::st_intersection(x, aoi_wgs))
  if (nrow(x) == 0) {
    return(sf::st_sf(geometry = sf::st_sfc(crs = crs_m)))
  }
  sf::st_transform(x, crs_m)
}

bld <- clip_m(bld)
parks <- clip_m(parks)
landuse <- clip_m(landuse)
water <- clip_m(water)
roads <- clip_m(roads)
rails <- clip_m(rails)

# ---- Heights & buffers ----
if (nrow(bld) > 0) {
  h_raw <- suppressWarnings(as.numeric(gsub(",", ".", bld$height)))
  levraw <- suppressWarnings(as.numeric(gsub(",", ".", bld$`building:levels`)))
  bld$h <- ifelse(!is.na(h_raw), h_raw,
                  ifelse(!is.na(levraw), pmax(levraw, 1) * 3.2, 10))
  bld$h <- pmin(bld$h, 80)
}

roads_buf <- if (nrow(roads) > 0) sf::st_buffer(roads, 3) else sf::st_sf(geometry = sf::st_sfc(crs = crs_m))
rails_buf <- if (nrow(rails) > 0) sf::st_buffer(rails, 2) else sf::st_sf(geometry = sf::st_sfc(crs = crs_m))
roads_crown <- if (nrow(roads) > 0) sf::st_buffer(roads, 1.3) else sf::st_sf(geometry = sf::st_sfc(crs = crs_m))

# ---- Recenter ----
center_xy <- sf::st_coordinates(ctr_m)[1, 1:2]
recenter <- function(x) {
  if (is.null(x) || nrow(x) == 0) return(x)
  sf::st_geometry(x) <- sf::st_geometry(x) - center_xy
  x
}

bld <- recenter(bld)
landuse <- recenter(landuse)
parks <- recenter(parks)
water <- recenter(water)
roads_buf <- recenter(roads_buf)
rails_buf <- recenter(rails_buf)
roads_crown <- recenter(roads_crown)

# ---- Materials ----
col_bld_low <- "#c88a1e"
col_bld_mid <- "#b7720e"
col_bld_high <- "#98590a"
col_landuse <- "#b89c3a"
col_park <- "#66A61E"
col_road <- "#7e8792"
col_road_hi <- "#e6ebf1"
col_water <- "#3E8fe0"

mat_landuse <- rayrender::diffuse(col_landuse)
mat_park <- rayrender::diffuse(col_park)
mat_road <- rayrender::diffuse(col_road)
mat_road_hi <- rayrender::diffuse(col_road_hi)
mat_water <- rayrender::metal(color = col_water, fuzz = 0.5)

# ---- Scene objects ----
objs <- list()
add_obj <- function(objlist, geom, top, bottom, mat) {
  if (nrow(geom) > 0)
    append(objlist, list(rayrender::extruded_polygon(geom, top = top, bottom = bottom, material = mat)))
  else objlist
}

objs <- add_obj(objs, landuse, 0.5, 0.02, mat_landuse)
objs <- add_obj(objs, parks, 0.5, 0.02, mat_park)
objs <- add_obj(objs, water, 0.5, -1, mat_water)

rr <- dplyr::bind_rows(roads_buf, rails_buf)
objs <- add_obj(objs, rr, 1.0, 0.02, mat_road)
objs <- add_obj(objs, roads_crown, 1.12, 1.02, mat_road_hi)

# ---- Buildings ----
bin_buildings <- function(bld, use_quantiles = FALSE) {
  bld <- bld[!is.na(bld$h) & is.finite(bld$h) & bld$h > 0, ]
  if (nrow(bld) == 0) return(bld)
  if (use_quantiles) {
    qs <- stats::quantile(bld$h, c(1/3, 2/3), na.rm = TRUE)
    brks <- c(-Inf, qs, Inf)
  } else {
    brks <- c(-Inf, 10, 20, Inf)
  }
  bld$bin <- cut(bld$h, breaks = brks,
                 labels = c("low", "mid", "high"),
                 include.lowest = TRUE, right = TRUE)
  bld
}

bld <- bin_buildings(bld)
pal <- c(low = col_bld_low, mid = col_bld_mid, high = col_bld_high)
mat_map <- setNames(lapply(pal, rayrender::diffuse), names(pal))

add_buildings <- function(scene, b) {
  if (is.null(b) || nrow(b) == 0) return(scene)
  for (lev in levels(b$bin)) {
    sel <- b[b$bin == lev, ]
    if (nrow(sel))
      scene <- rayrender::add_object(
        scene,
        rayrender::extruded_polygon(sel, data_column_top = "h",
                                    scale_data = 1, material = mat_map[[lev]])
      )
  }
  scene
}

# ---- Assemble scene ----
scene <- objs[[1]]
if (length(objs) > 1) {
  for (i in 2:length(objs)) {
    scene <- rayrender::add_object(scene, objs[[i]])
  }
}
scene <- add_buildings(scene, bld)

# ---- Camera and lighting ----
lookfrom <- c(-2000, 2000, -2000)
lookat <- c(0, 10, 50)

scene <- rayrender::add_object(
  scene,
  rayrender::sphere(
    x = -1200, y = 2500, z = -1200, radius = 400,
    material = rayrender::light(intensity = 15)
  )
)

# ---- Render ----
rayrender::render_scene(
  scene = scene,
  lookfrom = lookfrom,
  lookat = lookat,
  fov = 35,
  width = 1800, height = 1800,
  samples = 100,
  sample_method = "sobol",
  aperture = 0,
  denoise = TRUE,
  ambient_light = TRUE,
  clamp_value = 1,
  min_variance = 1e-15,
  backgroundlow = "#FFFFFF",
  parallel = TRUE,
  interactive = FALSE,
  filename = "nairobi.png"
)
