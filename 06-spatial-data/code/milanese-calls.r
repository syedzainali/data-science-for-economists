###
# spatial data
# 220601
###

setwd("/Users/syedzainali/Bielefeld/SS24/DSE....0/data-science-for-economists/06-spatial-data")

if (!require("pacman")) install.packages("pacman"); library("pacman")

library(pacman)
p_load(sf)
p_load(lubridate)
p_load(stringr)
p_load(ggplot2)
p_load(data.table)
p_load(ggthemes)
p_load(scales)

# load milan data ----
data = fread("input/milan_calls_hourly.csv.gz")
gc()

# plot time series
plot_data = data[, .(N = sum(N)), by = start_time]
plot = ggplot(plot_data) +
    geom_line(aes(x = start_time, y = N))
ggsave(plot, "output/time-series.png")


# plot time series for early december
plot_data = data[start_time >= ymd("2013-12-01") &
                 start_time < ymd("2013-12-08"),
                 .(N = sum(N)), by = start_time]
plot = ggplot(plot_data) +
    theme_minimal() +
    geom_line(aes(x = start_time, y = N)) +
    scale_x_datetime(NULL) +
    scale_y_continuous(name = "Number of calls",
                       labels = comma)
plot
# ggsave(plot, "output/time-series_week.png")

# plot weeks against each other, color
plot_data = data[, .(N = sum(N)), by = start_time]
plot_data = plot_data[year(start_time) == "2013"]
plot_data[, week := week(start_time)]
plot_data[, date := start_time - (week - min(week))*7*24*60*60]
plot_data = plot_data[week <= 52]
plot_data[, week := as.character(week)]

plot = ggplot(plot_data) +
    theme_minimal() +
    geom_line(aes(x = date, y = N, group = week, color = week)) +
    scale_x_datetime(name = NULL,
                     date_breaks = 'day',
                     date_labels = '%A') +
    scale_y_continuous(name = "Number of calls",
                       labels = comma) +
    scale_color_discrete()
plot

# heatmap ----
# grid
grid_sf <- st_read("input/milan_grid/")
# plot(grid_sf)

# Milan administrative shape file
milan_sf <- st_read("input/milan_borders/")

all.equal(st_crs(grid_sf),
          st_crs(milan_sf))

milan_sf = st_transform(milan_sf, crs = st_crs(grid_sf))

plot_data = data[, .(N = sum(N)), by = .(sq_1)]

setnames(plot_data, "sq_1", "cellId")
plot_data = merge(plot_data,
                  grid_sf,
                  by = "cellId")
plot_data = plot_data %>% st_as_sf()

plot = ggplot() + 
  geom_sf(data = plot_data, aes(color = N, fill = N)) +
  geom_sf(data = milan_sf, color = "white") +
  theme_void() +
  scale_fill_viridis_c() +
  scale_color_viridis_c()
plot

# on sundays at 11
data[, weekday := lubridate::wday(start_time, label = T)]
data[, hour := lubridate::hour(start_time)]
plot_data_sun = data[weekday == "Sun" & hour == 11, .(N = sum(N)), by = .(sq_1)]
gc()

setnames(plot_data_sun, "sq_1", "cellId")
plot_data_sun = merge(plot_data_sun,
                  grid_sf,
                  by = "cellId")
plot_data_sun = plot_data_sun %>% st_as_sf()

plot = ggplot() + 
  geom_sf(data = plot_data_sun, aes(color = N, fill = N)) +
  geom_sf(data = milan_sf, color = "white") +
  theme_void() +
  scale_fill_viridis_c() +
  scale_color_viridis_c()
plot

# on mondays at 11
plot_data_mon = data[weekday == "Mon" & hour == 11, .(N = sum(N)), by = .(sq_1)]
gc()

setnames(plot_data_mon, "sq_1", "cellId")
plot_data_mon = merge(plot_data_mon,
                  grid_sf,
                  by = "cellId")
plot_data_mon = plot_data_mon %>% st_as_sf()

plot = ggplot() + 
  geom_sf(data = plot_data_mon, aes(color = N, fill = N)) +
  geom_sf(data = milan_sf, color = "white") +
  theme_void() +
  scale_fill_viridis_c() +
  scale_color_viridis_c()
plot


# bilateral calls ----
data = fread("input/milan_2013-12-22_hourly.csv.gz")
gc()

head(data)
tail(data)

# density
plot_data = data[, .(n = sum(n)), by = .(cell_1, cell_2)]
plot_data = plot_data[, .(count_cells = .N), by = .(count_connections = n)]
# plot_data 

plot = ggplot(plot_data) +
    theme_minimal() +
    geom_point(aes(x = count_connections, y = count_cells)) +
    scale_x_log10() +
    scale_y_log10()
plot

# when are high-connections active?

# which cells have highest variance (businesses?)

# which are constantly high (hospital?)
