# Script to pull Seascape data for given polygon and plot a time series
# based on:
# - @JoryGriffith [bon-in-a-box-pipelines: ExampleWorkflow.R](https://github.com/GEO-BON/bon-in-a-box-pipelines/blob/59489f41e47cf09ae670ac65dcf7e19a66c9e5ad/scripts/ExampleWorkflow.R)
# - @bbest [seascapes-app: get_data.R](https://github.com/noaa-onms/seascapes-app/blob/c33518495d1f7d2a4fbb937cfb417ad4842cc5d1/get_data.R)

if (!require(librarian)){
  install.packages("librarian")
  library(librarian)
}
devtools::load_all("~/Github/marinebon/seascapeR")
devtools::load_all("~/Github/marinebon/extractr")
librarian::shelf(  # load libraries, installing first if needed
  dplyr,
  fs,
  glue,
  here,
  ropensci/mregions2,
  marinebon/seascapeR,
  sf,
  quiet = T)

# mrp_view("eez")

country <- "Belgium"
# country <- "United States"
type_nation <- gaz_rest_types() |> filter(type=="Nation") |> pull(typeID)  # 13
gid_country <- gaz_search(country, typeid = type_nation) |>
  pull(MRGID)  # Belgium: 14; United States: 2204
gid_eez <- gaz_relations(gid_country) |>
  filter(placeType == "EEZ") |>
  pull(MRGID)  # Belgium: 3293; United States: 8456
ply <- gaz_search(gid_eez) |>
  gaz_geometry()
# mapview::mapView(ply)
# ply |> st_drop_geometry() |> View()


ss_dataset <- "global_monthly"  # TODO: "global_8day"
ss_var     <- "CLASS"           # TODO: "P"

ss_info  <- get_ss_info(dataset = ss_dataset)
# ss_dates <- paste0(get_ss_dates(ss_info) |> as.character(), "T12:00:00Z")

ss_url  <- "https://cwcgom.aoml.noaa.gov/erddap/griddap/noaa_aoml_4729_9ee6_ab54.html"
ss_info <- ed_info(ss_url)
ss_var  <- "CLASS"           # TODO: "P"

# dims <- ed_dims(ss_info)
ss_times <- ed_dim(ss_info, "time")

# Reading inputs
input  <- biab_inputs()
r_tif  <- here(glue("userdata/seascapes/{country}.tif"))
z_csv  <- here(glue("userdata/seascapes/{country}.csv"))
dir_nc <- here(glue("userdata/seascapes/{country}_nc"))

# dir.create(dir_tif, showWarnings = F)

ed_success <- ed_extract(
  ed        = ss_info,
  var       = ss_var,
  sf_zones  = ply,
  fld_zones = "preferredGazetteerName",
  rast_tif  = r_tif,
  zonal_csv = z_csv,
  time_min  = min(ss_times),
  time_max  = max(ss_times),
  dir_nc    = dir_nc)

all(length(dims[dims_other])

ss_grids <- get_ss_grds(
  ss_info,
  ply,
  ss_var,
  # date_beg=ss_dates[1],
  # date_end=ss_dates[2],
  dir_tif = dir_tif) # grd_{ss_var}_{date}.tif

plot(ss_grids)

# Extract percentage of each polygon covered by each class
freq_table <- as.data.frame(freq(ss_grids))

# Total number of cells in the raster
total_cells <- ncell(ss_grids)

# Calculate the percentage of the bounding box that is each habitat category
freq_table$percentage_landcover <- (freq_table$count / total_cells) * 100

# Print the summary
print(freq_table)

#### Plot percentage of area with each habitat
# Merge with classification CSV (from: https://github.com/marinebon/seascapeR/blob/main/data-raw/ss_global_classes.txt)
classifications <- read.csv(
  "https://raw.githubusercontent.com/marinebon/seascapeR/refs/heads/main/data-raw/ss_global_classes.txt",
  sep="|") |>
  janitor::clean_names()
percent_class <- merge(classifications, freq_table, by.x="seascape_id_number", by.y="value")

#Plot percentages of each class in the polygon
ggplot(percent_class) +
  geom_col(mapping=aes(x=nominal_descriptor, y=percentage_landcover, fill=nominal_descriptor), position="dodge") +
  coord_flip() +
  labs(title="Seascape class percentage", y= "Percentage landcover") +
  theme_classic() +
  theme(axis.title.y=element_blank(), legend.position="none")

#### Pull data for species of interest and plot percentage of points in each class for each species

#### Pull OBIS data
polygon_wkt <- st_as_text(custom_ply)

species <- c("Stenella coeruleoalba","Astronesthes cyaneus") # user can choose one or more species

dat <- occurrence(scientificname = species, geometry=polygon_wkt)
# Some species to play around with
#Stenella coeruleoalba
#Astronesthes cyaneus
#Notolychnus valdiviae
#Pterotrachea hippocampus

ggplot() +
  geom_point(dat, mapping=aes(decimalLatitude, decimalLongitude, color=species))

# extract seascape values for each occurrence point
dat$seascape_class <- terra::extract(ss_grids, dat[,c("decimalLongitude", "decimalLatitude")])

# Plot percentage of occurrences that are in each class and percentage of total
dat_summary <- dat |>
  group_by(species) |>
  mutate(total_occ=n()) |>
  group_by(species, seascape_class, total_occ) |>
  summarise(
    total=n(), .groups = "drop") |>
  mutate(percentage_species=(total/total_occ*100))

# Merge with seascape categories
dat_summary_merged <- merge(classifications, dat_summary, by.x="seascape_id_number", by.y="seascape_class")

ggplot(dat_summary_merged) +
  geom_col(dat_summary_merged, mapping=aes(y=percentage_species, x=nominal_descriptor, fill=nominal_descriptor)) +
  coord_flip() +
  facet_wrap(~species) +
  labs(y= "Percentage of occurences in class") +
  theme_classic() +
  theme(axis.title.y=element_blank(), legend.position="none")

dat_summary_merged$percentage_species
