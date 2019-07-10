# Source: https://data.anfr.fr/

dat <- read.csv(
  "~/RallyeData/transport/data/uc7_topo_reseau_mobile.csv",
  sep=";")

# https://stackoverflow.com/questions/7069076/split-column-at-delimiter-in-data-frame
# library(reshape)
# dat <- transform(
#   dat,
#   coord_split = colsplit(coordonnees, split = ", ", names = c('lat', 'lon')))

LatitudeRange  <- range(dat$latitude_2)
LongitudeRange <- range(dat$longitude_2)

rayon_Terre <- 6378137  # En mètres

rad_to_deg <- function(angle){
  angle * (180/pi)
}

deg_to_rad <- function(angle){
  angle * (pi/180)
}

geod_AB <- function(lat_A, lat_B, lon_A, lon_B) {
  # Calcul de distance géodésique, lat et lon en degrés
  # https://geodesie.ign.fr/contenu/fichiers/Distance_longitude_latitude.pdf
  delta_lon <- lon_B - lon_A

  lat_A_rad <- deg_to_rad(lat_A)
  lat_B_rad <- deg_to_rad(lat_B)
  delta_lon_rad <- deg_to_rad(delta_lon)

  S_AB <- acos(
    sin(lat_A_rad) * sin(lat_B_rad) + cos(lat_A_rad) * cos(lat_B_rad) * cos(delta_lon_rad)
  )

  rad_to_deg(S_AB) * rayon_Terre
}

# Angle équivalent pour 1000 km
ang_rad <- 1000000 / rayon_Terre
ang_deg <- rad_to_deg(ang_rad)

# Subdivision de l'espace en carrés de 10 km
n_lat      <- round(geod_AB(LatitudeRange[1], LatitudeRange[2], LongitudeRange[1], LongitudeRange[1]) / 10000)
n_lon_low  <- round(geod_AB(LatitudeRange[1], LatitudeRange[1], LongitudeRange[1], LongitudeRange[2]) / 10000)
n_lon_hi   <- round(geod_AB(LatitudeRange[2], LatitudeRange[2], LongitudeRange[1], LongitudeRange[2]) / 10000)
n_lon_mean <- round(mean(n_lon_hi, n_lon_low))

# Calcul de densité
# http://pages.stern.nyu.edu/~achinco/programming_examples/Example__PlotGeographicDensity.html
library(MASS)

AntennaDensityList <- kde2d(
  dat$longitude_2,
  dat$latitude_2,
  n = 4000,
  # n = c(n_lat, n_lon_mean),
  lims = c(LongitudeRange, LatitudeRange))

# Retrouver la densité pour chaque antenne
density_row <- function(value, data_ref){
  which(data_ref >= value)[1]
}

dat$density_lon_row <-
  sapply(dat$longitude_2, FUN = density_row, data_ref = AntennaDensityList$x)
dat$density_lat_row <-
  sapply(dat$latitude_2, FUN = density_row, data_ref = AntennaDensityList$y)

density_value <- function(data_ref){
  data_ref_lat <- as.numeric(unname(data_ref["density_lat_row"]))
  data_ref_lon <- as.numeric(unname(data_ref["density_lon_row"]))
  AntennaDensityList$z[data_ref_lat, data_ref_lon]
}

dat$density <- apply(X = dat, 1, FUN = density_value)

# Sauvegarde des donnees
write.csv(dat, file = "~/RallyeData/transport/data/uc7_topo_reseau_mobile_density.csv")

# Plot de la carte de densité
library(ggplot2)

# carte a partir du calcul de densite
ggplot(dat, aes(x = longitude_2, y = latitude_2)) +
  # annotation_map_tile(zoomin = -1) +
  geom_point(aes(colour = density), alpha = .3, show.legend = T) +
  scale_colour_gradient(low = "white", high = "blue") +
  lims(x = LongitudeRange, y = LatitudeRange)

# carte par méthode interne ggplot en fonction du nombre de points
ggplot(dat, aes(x = longitude_2, y = latitude_2)) +
  # annotation_map_tile(zoomin = -1) +
  stat_density_2d(data = dat,
                  aes(x = longitude_2, y = latitude_2, alpha = ..level.. ,  fill = ..level..),
                  contour = TRUE,
                  geom = "polygon") +
  scale_alpha_continuous(guide = "legend") +
  scale_fill_continuous(guide = "legend") +
  # coord_sf(crs = 4326) +
  lims(x = LongitudeRange, y = LatitudeRange)
