# Authors Elisa Frank Buss, Carla Rodriguez, Yelvis Acevedo - 2021.
# Modified from Alba Germán's practical class for the MASTER IN SPACE INFORMATION APPLICATIONS - GULICH INSTITUTE - CONAE/UNC - Argentina
# First step: download data in csv format from the Appears web for a selected area and time.
# MOD13Q1 product was used.

# Upload libraries
library(magrittr)
library(xts)
library(scales)
library(ggplot2)
library(ggthemes)
library(dplyr)


# Upload files
NDVI_stats_nd = read.csv("C:/My_data/MOD13Q1-006-Statistics_no_deforested_2001_2019.csv", stringsAsFactors = FALSE)
NDVI_stats_d = read.csv("C:/My_data/MOD13Q1-006-Statistics_deforested_2001_2019.csv", stringsAsFactors = FALSE)

# Create new variables for year and month, and convert Date to Date format

NDVI_stats_nd$year = substring(NDVI_stats_nd$Date,1,4)
NDVI_stats_nd$month = substring(NDVI_stats_nd$Date,6,7)
NDVI_stats_nd$Date=as.Date(NDVI_stats_nd$Date)

NDVI_stats_d$year = substring(NDVI_stats_d$Date,1,4)
NDVI_stats_d$month = substring(NDVI_stats_d$Date,6,7)
NDVI_stats_d$Date = as.Date(NDVI_stats_d$Date)

NDVI_nd = NDVI_stats_nd[,]
NDVI_d = NDVI_stats_d[,]


# Plot the series
# No deforested
plot_x <- NDVI_nd %>%
  ggplot(aes(x = Date, y = Mean)) +
  geom_line(colour = 'red', size = 1) +
  geom_point(colour = 'red', size = 2) +
  labs(x = "Date",
       y = "NDVI",
       title = "NDVI MODIS 2010-2018: No deforested") +
  theme_classic()
plot_x

# Deforested
plot_y <- NDVI_d %>%
  ggplot(aes(x = Date, y = Mean)) +
  geom_line(colour = 'red', size = 1) +
  geom_point(colour = 'red', size = 2) +
  labs(x = "Date",
       y = "NDVI",
       title = "NDVI MODIS 2010-2018: Deforested") +
  theme_classic()
plot_y


# Plot time series plus LOESS model

ggplot(NDVI_nd, aes(Date,Mean)) +          
  geom_bar(stat="identity", na.rm = TRUE) +
  ggtitle("NDVI MODIS 2010-2018: No deforested") +
  xlab("Date") + ylab("Mean") +
  scale_x_date(labels=date_format ("%b %y"), breaks=date_breaks("1 year")) +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20)) +
  theme(text = element_text(size=18)) + stat_smooth(colour="green")

ggplot(NDVI_d, aes(Date,Mean)) +          
  geom_bar(stat="identity", na.rm = TRUE) +
  ggtitle("NDVI MODIS 2010-2018: Deforested") +
  xlab("Date") + ylab("Mean") +
  scale_x_date(labels=date_format ("%b %y"), breaks=date_breaks("1 year")) +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20)) +
  theme(text = element_text(size=18)) + stat_smooth(colour="green")


# Calculate NDVI yearly means
# No deforested
NDVI_yearly_mean_nd = NDVI_nd %>% group_by(year) %>% summarise(mvalue = mean(Mean))

# Deforested
NDVI_yearly_mean_d = NDVI_d %>% group_by(year) %>% summarise(mvalue = mean(Mean))


# See anomalies
# No deforested
mean_NDVI_nd = mean(NDVI_yearly_mean_nd$mvalue)
sd_NDVI_nd = sd(NDVI_yearly_mean_nd$mvalue)
anomalys_NDVI_nd = data.frame(year = NDVI_yearly_mean_nd$year, anomaly = (NDVI_yearly_mean_nd$mvalue-mean_NDVI_nd)/sd_NDVI_nd)
anomalys_NDVI_nd$sign = ifelse(anomalys_NDVI_nd$anomaly > 0, "pos", "neg")

ggplot(anomalys_NDVI_nd, aes(year, anomaly, fill = sign)) + 
  geom_bar(stat = "identity", show.legend = FALSE) + 
  labs(y = "NDVI anomaly No Deforested", x = "Year") +
  theme_hc()

# Deforested
mean_NDVI_d = mean(NDVI_yearly_mean_d$mvalue)
sd_NDVI_d = sd(NDVI_yearly_mean_d$mvalue)
anomalys_NDVI_d = data.frame(year = NDVI_yearly_mean_d$year, anomaly = (NDVI_yearly_mean_d$mvalue - mean_NDVI_d)/sd_NDVI_d)
anomalys_NDVI_d$sign = ifelse(anomalys_NDVI_d$anomaly > 0, "pos", "neg")

ggplot(anomalys_NDVI_d, aes(year, anomaly, fill = sign)) + 
  geom_bar(stat = "identity", show.legend = FALSE) + 
  labs(y = "NDVI anomaly Deforested", x = "Year") +
  theme_hc()

