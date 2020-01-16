
library(tidyr)
library(dplyr)

#import info about logger and plot


library(tidyverse)
library(readxl)


#import info about logger and plot
loggerID <- readxl::read_xlsx('Data/Loggers/LoggerID.xlsx', sheet = 2) 
PlotID <- readxl::read_xlsx('Data/Loggers/LoggerID.xlsx', sheet = 1) 

EM40839 <- readxl::read_xls('Data/Loggers/EM40839.xls') %>% 
  mutate(logger = 'EM40839',
         port_1 = as.numeric(port_1),
         port_2 = as.numeric(port_2),
         port_3 = as.numeric(port_3),
         port_4 = as.numeric(port_4),
         port_5 = as.numeric(port_5)) %>% 
  gather(key = "port", value = 'reading', 'port_1':'port_5', na.rm = TRUE) %>% 
  left_join(loggerID, by = c('logger', 'port')) %>% 
  left_join(PlotID, by = 'plot') %>% 
  group_by(plot, day, month, year, sensor, treatment, geography, phase) %>%
  summarise_at(.vars = 'reading', mean)

EM40894 <- readxl::read_xls('Data/Loggers/EM40894.xls') %>% 
  mutate(logger = 'EM40894',
         port_1 = as.numeric(port_1),
         port_2 = as.numeric(port_2),
         port_3 = as.numeric(port_3),
         port_4 = as.numeric(port_4),
         port_5 = as.numeric(port_5),
         port_6 = as.numeric(port_6)) %>% 
  gather(key = "port", value = 'reading', 'port_1':'port_6', na.rm = TRUE) %>% 
  left_join(loggerID, by = c('logger', 'port')) %>% 
  left_join(PlotID, by = 'plot') %>% 
  group_by(plot, day, month, year, sensor, treatment, geography, phase) %>%
  summarise_at(.vars = 'reading', mean)

EM40895 <- readxl::read_xlsx('Data/Loggers/EM40895.xlsx')%>% 
  mutate(logger = 'EM40895',
         port_1 = as.numeric(port_1),
         port_2 = as.numeric(port_2),
         port_3 = as.numeric(port_3),
         port_4 = as.numeric(port_4)) %>% 
  gather(key = "port", value = 'reading', 'port_1':'port_4', na.rm = TRUE) %>% 
  left_join(loggerID, by = c('logger', 'port')) %>% 
  left_join(PlotID, by = 'plot') %>% 
  group_by(plot, day, month, year, sensor, treatment, geography, phase) %>%
  summarise_at(.vars = 'reading', mean)

EM40896 <- readxl::read_xlsx('Data/Loggers/EM40896.xlsx') %>% 
  mutate(logger = 'EM40896',
         port_1 = as.numeric(port_1),
         port_2 = as.numeric(port_2),
         port_3 = as.numeric(port_3),
         port_4 = as.numeric(port_4),
         port_5 = as.numeric(port_5)) %>% 
  gather(key = "port", value = 'reading', 'port_1':'port_5', na.rm = TRUE) %>% 
  left_join(loggerID, by = c('logger', 'port')) %>% 
  left_join(PlotID, by = 'plot') %>% 
  group_by(plot, day, month, year, sensor, treatment, geography, phase) %>%
  summarise_at(.vars = 'reading', mean)

EM40897 <- readxl::read_xls('Data/Loggers/EM40897.xls') %>% 
  mutate(logger = 'EM40897',
         port_1 = as.numeric(port_1),
         port_2 = as.numeric(port_2),
         port_3 = as.numeric(port_3),
         port_4 = as.numeric(port_4)) %>% 
  gather(key = "port", value = 'reading', 'port_1':'port_4', na.rm = TRUE) %>% 
  left_join(loggerID, by = c('logger', 'port')) %>% 
  left_join(PlotID, by = 'plot') %>% 
  group_by(plot, day, month, year, sensor, treatment, geography, phase) %>%
  summarise_at(.vars = 'reading', mean)

EM40898 <- readxl::read_xls('Data/Loggers/EM40898.xls') %>% 
  mutate(logger = 'EM40898',
         port_1 = as.numeric(port_1),
         port_2 = as.numeric(port_2),
         port_3 = as.numeric(port_3)) %>% 
  gather(key = "port", value = 'reading', 'port_1':'port_3', na.rm = TRUE) %>% 
  left_join(loggerID, by = c('logger', 'port')) %>% 
  left_join(PlotID, by = 'plot') %>% 
  group_by(plot, day, month, year, sensor, treatment, geography, phase) %>%
  summarise_at(.vars = 'reading', mean)

EM40899 <- readxl::read_xls('Data/Loggers/EM40899.xls')%>% 
  mutate(logger = 'EM40899',
         port_1 = as.numeric(port_1),
         port_2 = as.numeric(port_2),
         port_3 = as.numeric(port_3),
         port_4 = as.numeric(port_4),
         port_5 = as.numeric(port_5)) %>% 
  gather(key = "port", value = 'reading', 'port_1':'port_5', na.rm = TRUE) %>% 
  left_join(loggerID, by = c('logger', 'port')) %>% 
  left_join(PlotID, by = 'plot') %>% 
  group_by(plot, day, month, year, sensor, treatment, geography, phase) %>%
  summarise_at(.vars = 'reading', mean)

EM40900 <- readxl::read_xlsx('Data/Loggers/EM40900.xlsx')%>% 
  mutate(logger = 'EM40900',
         port_1 = as.numeric(port_1),
         port_2 = as.numeric(port_2),
         port_3 = as.numeric(port_3),
         port_4 = as.numeric(port_4),
         port_5 = as.numeric(port_5)) %>% 
  gather(key = "port", value = 'reading', 'port_1':'port_5', na.rm = TRUE) %>% 
  left_join(loggerID, by = c('logger', 'port')) %>% 
  left_join(PlotID, by = 'plot') %>% 
  group_by(plot, day, month, year, sensor, treatment, geography, phase) %>%
  summarise_at(.vars = 'reading', mean)

EM40901 <- readxl::read_xls('Data/Loggers/EM40901.xls')%>% 
  mutate(logger = 'EM40901',
         port_1 = as.numeric(port_1),
         port_2 = as.numeric(port_2),
         port_3 = as.numeric(port_3),
         port_4 = as.numeric(port_4),
         port_5 = as.numeric(port_5)) %>% 
  gather(key = "port", value = 'reading', 'port_1':'port_5', na.rm = TRUE) %>% 
  left_join(loggerID, by = c('logger', 'port')) %>% 
  left_join(PlotID, by = 'plot') %>% 
  group_by(plot, day, month, year, sensor, treatment, geography, phase) %>%
  summarise_at(.vars = 'reading', mean)

EM40902 <- readxl::read_xls('Data/Loggers/EM40902.xls')%>% 
  mutate(logger = 'EM40902',
         port_1 = as.numeric(port_1),
         port_2 = as.numeric(port_2),
         port_3 = as.numeric(port_3),
         port_4 = as.numeric(port_4)) %>% 
  gather(key = "port", value = 'reading', 'port_1':'port_4', na.rm = TRUE) %>% 
  left_join(loggerID, by = c('logger', 'port')) %>% 
  left_join(PlotID, by = 'plot') %>% 
  group_by(plot, day, month, year, sensor, treatment, geography, phase) %>%
  summarise_at(.vars = 'reading', mean)

EM40903 <- readxl::read_xlsx('Data/Loggers/EM40903.xlsx')%>% 
  mutate(logger = 'EM40903',
         port_1 = as.numeric(port_1),
         port_2 = as.numeric(port_2),
         port_3 = as.numeric(port_3),
         port_4 = as.numeric(port_4),
         port_5 = as.numeric(port_5)) %>% 
  gather(key = "port", value = 'reading', 'port_1':'port_5', na.rm = TRUE) %>% 
  left_join(loggerID, by = c('logger', 'port')) %>% 
  left_join(PlotID, by = 'plot') %>% 
  group_by(plot, day, month, year, sensor, treatment, geography, phase) %>%
  summarise_at(.vars = 'reading', mean)

EM40904 <- readxl::read_xlsx('Data/Loggers/EM40904.xlsx')%>% 
  mutate(logger = 'EM40904',
         port_1 = as.numeric(port_1),
         port_2 = as.numeric(port_2),
         port_3 = as.numeric(port_3),
         port_4 = as.numeric(port_4)) %>% 
  gather(key = "port", value = 'reading', 'port_1':'port_4', na.rm = TRUE) %>% 
  left_join(loggerID, by = c('logger', 'port')) %>% 
  left_join(PlotID, by = 'plot') %>% 
  group_by(plot, day, month, year, sensor, treatment, geography, phase) %>%
  summarise_at(.vars = 'reading', mean)

EM40905 <- readxl::read_xlsx('Data/Loggers/EM40905.xlsx')%>% 
  mutate(logger = 'EM40905',
         port_1 = as.numeric(port_1),
         port_2 = as.numeric(port_2),
         port_3 = as.numeric(port_3),
         port_4 = as.numeric(port_4),
         port_5 = as.numeric(port_5)) %>% 
  gather(key = "port", value = 'reading', 'port_1':'port_5', na.rm = TRUE) %>% 
  left_join(loggerID, by = c('logger', 'port')) %>% 
  left_join(PlotID, by = 'plot') %>% 
  group_by(plot, day, month, year, sensor, treatment, geography, phase) %>%
  summarise_at(.vars = 'reading', mean)

EM40906 <- readxl::read_xlsx('Data/Loggers/EM40906.xlsx')%>% 
  mutate(logger = 'EM40906',
         port_1 = as.numeric(port_1),
         port_2 = as.numeric(port_2),
         port_3 = as.numeric(port_3),
         port_4 = as.numeric(port_4),
         port_5 = as.numeric(port_5)) %>% 
  gather(key = "port", value = 'reading', 'port_1':'port_5', na.rm = TRUE) %>% 
  left_join(loggerID, by = c('logger', 'port')) %>% 
  left_join(PlotID, by = 'plot') %>% 
  group_by(plot, day, month, year, sensor, treatment, geography, phase) %>%
  summarise_at(.vars = 'reading', mean)

EM40907 <- readxl::read_xls('Data/Loggers/EM40907.xls')%>% 
  mutate(logger = 'EM40907',
         port_1 = as.numeric(port_1),
         port_2 = as.numeric(port_2),
         port_3 = as.numeric(port_3),
         port_4 = as.numeric(port_4),
         port_5 = as.numeric(port_5)) %>% 
  gather(key = "port", value = 'reading', 'port_1':'port_5', na.rm = TRUE) %>% 
  left_join(loggerID, by = c('logger', 'port')) %>% 
  left_join(PlotID, by = 'plot') %>% 
  group_by(plot, day, month, year, sensor, treatment, geography, phase) %>%
  summarise_at(.vars = 'reading', mean)

EM40908 <- readxl::read_xls('Data/Loggers/EM40908.xls')%>% 
  mutate(logger = 'EM40908',
         port_1 = as.numeric(port_1),
         port_2 = as.numeric(port_2),
         port_3 = as.numeric(port_3),
         port_4 = as.numeric(port_4),
         port_5 = as.numeric(port_5)) %>% 
  gather(key = "port", value = 'reading', 'port_1':'port_5', na.rm = TRUE) %>% 
  left_join(loggerID, by = c('logger', 'port')) %>% 
  left_join(PlotID, by = 'plot') %>% 
  group_by(plot, day, month, year, sensor, treatment, geography, phase) %>%
  summarise_at(.vars = 'reading', mean)

EM40934 <- readxl::read_xlsx('Data/Loggers/EM40934.xlsx') %>% 
  mutate(logger = 'EM40934',
         port_1 = as.numeric(port_1),
         port_2 = as.numeric(port_2),
         port_3 = as.numeric(port_3),
         port_4 = as.numeric(port_4),
         port_5 = as.numeric(port_5)) %>% 
  gather(key = "port", value = 'reading', 'port_1':'port_5', na.rm = TRUE) %>% 
  left_join(loggerID, by = c('logger', 'port')) %>% 
  left_join(PlotID, by = 'plot') %>% 
  group_by(plot, day, month, year, sensor, treatment, geography, phase) %>%
  summarise_at(.vars = 'reading', mean)

EM40935 <- readxl::read_xlsx('Data/Loggers/EM40935.xlsx')%>% 
  mutate(logger = 'EM40935',
         port_1 = as.numeric(port_1),
         port_2 = as.numeric(port_2),
         port_3 = as.numeric(port_3),
         port_4 = as.numeric(port_4),
         port_5 = as.numeric(port_5)) %>% 
  gather(key = "port", value = 'reading', 'port_1':'port_5', na.rm = TRUE) %>% 
  left_join(loggerID, by = c('logger', 'port')) %>% 
  left_join(PlotID, by = 'plot') %>% 
  group_by(plot, day, month, year, sensor, treatment, geography, phase) %>%
  summarise_at(.vars = 'reading', mean)


# gather all observations

full.df <- EM40839 %>% 
  rbind(EM40894, 
        EM40895, 
        EM40896, 
        EM40897, 
        EM40897, 
        EM40898, 
        EM40899, 
        EM40900,
        EM40901,
        EM40902,
        EM40903,
        EM40904,
        EM40905,
        EM40906,
        EM40907,
        EM40908,
        EM40934,
        EM40935
        ) %>% 
  mutate(date = as.Date(paste(year, month, day, sep = '-'))) 


#produce monthly means
monthly <- full.df %>% 
  select(plot, day, month, year, sensor, treatment, geography, phase, reading) %>% 
  group_by(plot, month, year, sensor, treatment, geography, phase) %>%
  summarise_at(.vars = 'reading', mean)
  

#Look at data

#add season colour


#ggplot (full.df %>%                                     #Careful, this plot so many layers that everything will break down. should define seasons in main df and tidy up
          filter( sensor == 'soil.moist.15'), 
        aes (date, reading)) +
  geom_rect(aes(xmin = as.Date("2017-06-01"),
                xmax = as.Date("2017-08-31"),
                ymin = -Inf, ymax = Inf, fill = 'summer'), alpha = .2) +
  # geom_rect(aes(xmin = as.Date("2017-09-01"),
  #               xmax = as.Date("2017-11-30"),
  #               ymin = -Inf, ymax = Inf, fill = 'fall'), alpha = .2) +
  # geom_rect(aes(xmin = as.Date("2017-12-01"),
  #               xmax = as.Date("2018-02-28"),
  #               ymin = -Inf, ymax = Inf, fill = 'winter'), alpha = .2) +
  # geom_rect(aes(xmin = as.Date("2018-03-01"),
  #               xmax = as.Date("2018-05-31"),
  #               ymin = -Inf, ymax = Inf, fill = 'spring'), alpha = .2) +
  # geom_rect(aes(xmin = as.Date("2018-06-01"),
  #               xmax = as.Date("2018-08-31"),
  #               ymin = -Inf, ymax = Inf, fill = 'summer'), alpha = .2) +
  # geom_rect(aes(xmin = as.Date("2018-09-01"),
  #               xmax = as.Date("2018-11-30"),
  #               ymin = -Inf, ymax = Inf, fill = 'fall'), alpha = .2) +
  # geom_rect(aes(xmin = as.Date("2018-12-01"),
  #               xmax = as.Date("2019-02-28"),
  #               ymin = -Inf, ymax = Inf, fill = 'winter'), alpha = .2) +
  # geom_rect(aes(xmin = as.Date("2019-03-01"),
  #               xmax = as.Date("2019-05-31"),
  #               ymin = -Inf, ymax = Inf, fill = 'spring'), alpha = .2) +
  # geom_rect(aes(xmin = as.Date("2019-06-01"),
  #               xmax = as.Date("2019-08-31"),
  #               ymin = -Inf, ymax = Inf, fill = 'summer'), alpha = .2) +
  # geom_rect(aes(xmin = as.Date("2019-09-01"),
  #               xmax = as.Date("2019-11-30"),
  #               ymin = -Inf, ymax = Inf, fill = 'fall'), alpha = .2) +
  # geom_rect(aes(xmin = as.Date("2019-12-01"),
  #               xmax = as.Date("2020-02-29"),
  #               ymin = -Inf, ymax = Inf, fill = 'winter'), alpha = .2) +
  geom_smooth(aes(color=interaction(geography, phase), linetype=treatment), se=FALSE)  +
  theme_classic() +
  scale_fill_manual(values = c("summer" = "lightyellow", "fall" = "mistyrose", "winter" = "lightcyan", "spring" = "darkseagreen1"), name = "Season", labels = c("Summer", "Autumn", "Winter", "Spring"))







############################








