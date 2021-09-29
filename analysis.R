library(tidyverse)
library(hrbrthemes)
library(lubridate)
library(prophet)
library(highcharter)

# Read data
#https://www.epa.gov/outdoor-air-quality-data/air-data-multiyear-tile-plot
pm25 <- read_csv("ad_viz_tile_data_pm25.csv")

# Format data
pm25_cl <- pm25 %>% mutate(Date = as.Date(Date, format = "%m/%d/%Y"))

# Visualize data
pm25_cl %>%
  ggplot(aes(Date,`PM2.5 AQI Value`)) +
  #geom_point(alpha = .1) +
  annotate("rect", xmin=as.Date('2019-01-01'), xmax=as.Date('2021-09-27'), ymin=0, ymax=12, 
            fill = "green", alpha = .3) +
  annotate("rect", xmin=as.Date('2019-01-01'), xmax=as.Date('2021-09-27'), ymin=12, ymax=35, 
           fill = "yellow", alpha = .3) +
  annotate("rect", xmin=as.Date('2019-01-01'), xmax=as.Date('2021-09-27'), ymin=35, ymax=55, 
           fill = "orange", alpha = .3) +
  annotate("rect", xmin=as.Date('2019-01-01'), xmax=as.Date('2021-09-27'), ymin=55, ymax=150, 
           fill = "red", alpha = .3) +
  geom_vline(xintercept = as.Date("2020-03-13"), lty = 2) +
  geom_vline(xintercept = as.Date("2021-06-11"), lty = 2) +
  annotate("text", x = as.Date("2020-03-13"), y = 60, label = "Shutdown", hjust = -.1, fontface = "bold") +
  annotate("text", x = as.Date("2021-06-11"), y = 60, label = "Reopening", hjust = -.1, fontface = "bold") +
  annotate("text", x = as.Date("2019-01-01"), y = 60, label = "Unhealthy", hjust = 0, fontface = "bold", color = "red") +
  annotate("text", x = as.Date("2019-01-01"), y = 45, label = "Unhealthy for\nSensitive Groups", hjust = 0, fontface = "bold", color = "orange") +
  annotate("text", x = as.Date("2019-01-01"), y = 25, label = "Moderate", hjust = 0, fontface = "bold", color = "goldenrod") +
  geom_smooth(method = "gam") +
  coord_cartesian(ylim=c(25,65)) +
  scale_y_continuous(minor_breaks = NULL) +
  scale_x_date(labels = scales::date_format("%b %Y")) +
  labs(title = "Increasing Air Pollution with DC Reopening",
       subtitle = expression(paste("Smoothed Daily ",PM["2.5"]," Concentration (",μg/m^3,")")),
       x = NULL, y = NULL, 
       caption = "Source: U.S. EPA AirData, Sept. 27, 2021") +
  theme_ipsum_pub(base_size = 14, grid = "Y")

ggsave("pm25.png", device = "png", width = 12, height = 6, units = "in", dpi = 320)

ggsave("pm25-inline.png", device = "png", width = 8, height = 5, units = "in", dpi = 320)

# Model data
data <- pm25_cl %>%
  select(ds = Date, y = `PM2.5 AQI Value`)

holidays <- tibble(holiday = "July4th", 
                   ds = as.Date(c('2019-07-04', '2020-07-04', '2021-07-04','2022-07-04')),
                   lower_window = 1,
                   upper_window = 1)
  
mod <- prophet(data, growth = "linear", mcmc.samples = 1000,
               holidays = holidays, holidays.prior.scale = .5)  

future <- make_future_dataframe(mod, periods = 365)

forecast <- predict(mod, future)

# Plot model
plot(mod, forecast) +
  annotate("rect", xmin=as.POSIXct('2019-01-01'), xmax=as.POSIXct('2022-09-27'), ymin=55, ymax=150, 
           fill = "red", alpha = .3) +
  scale_y_continuous(minor_breaks = NULL) +
  labs(title = "Increasing Air Pollution with DC Reopening",
       subtitle = expression(paste("Smoothed Daily ",PM["2.5"]," Concentration (",μg/m^3,")")),
       x = NULL, y = NULL, 
       caption = "Source: U.S. EPA AirData, Sept. 27, 2021") +
  theme_ipsum_pub(base_size = 14, grid = "Y")

prophet_plot_components(mod, forecast)

#
forecast_data <- forecast %>%
  left_join(data, by = "ds") %>%
  select(ds, yhat, yhat_lower, yhat_upper, y) %>%
  mutate(ds = as.Date(ds),
         yhat = round(yhat, 1),
         yhat_lower = round(yhat_lower, 1),
         yhat_upper = round(yhat_upper, 1)) 


# Plot with highcharter
forecast_data %>% 
hchart(type = "line",hcaes(x = ds, y = yhat),
       name = "Modeled",
       id = "pred",
       lineWidth = 1.5,
       color = "#0080FF70",
       showInLegend = TRUE)%>%
  hc_add_series(
    forecast_data,
    type = "arearange",
    name = "Modeled Range",
    hcaes(x = ds, low = yhat_lower, high = yhat_upper),
    linkedTo = "pred", 
    showInLegend = FALSE,
    color = "#0080FF20",
    zIndex = -3 
  )%>%
  hc_add_series(
    forecast_data,
    type = "scatter",
    id = "actual",
    name = "Actual",
    hcaes(x = ds, y = y),
    linkedTo = "pred", 
    showInLegend = TRUE,
    marker = list(radius = 1.5),
    color = "black",
    tooltip = list(pointFormat = "{point.x:%B %d, %Y} <br> Actual: {point.y}")
  )%>% 
  hc_xAxis(
    title = FALSE, 
    type = "datetime"
  ) %>% 
  hc_yAxis(
    title = FALSE,
    max = 160,
    min = 0,
    plotBands = list(
      list(
        from = 55,
        to = 150,
        color = "rgba(255, 0, 0, 0.1)"
      )
    )
  ) %>%
  hc_tooltip(
    shared = TRUE,crosshairs = TRUE
  ) %>%
  hc_title(text = "Projected Increases in Air Pollution in DC") %>%
  hc_subtitle(text = "Daily PM2.5 Concentration (μg/m^3)") %>%
  hc_caption(text = "Source: U.S. EPA AirData, Sept. 27, 2021") %>%
  hc_add_theme(hc_theme_smpl())

# Save data
saveRDS(forecast_data, file="forecast_data.RDS")
#forecast_data <- readRDS(file="forecast_data.RDS")