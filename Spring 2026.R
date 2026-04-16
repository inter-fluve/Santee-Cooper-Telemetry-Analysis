############ Load  Libraries  ###################

library(dplyr)
library(tidyverse)
library(lubridate)
library(readr)
library(ggplot2)
library(glatos)
library(tidyr)
library(officer)
library(janitor)

#install.packages('glatos', repos = c('https://ocean-tracking-network.r-universe.dev', 'https://cloud.r-project.org'))

setwd("I:/Shared drives/IFI/Projects_Active/SanteeCooper_FERC_19-04-09/IFI_TASKS/Task_Y_SturgUseofLock/Data")

colpal <- c("#6a8c88", "#bc5127", "#cdab40", "#656e44", "#6c545e", "#376863",
            "#9cac49", "#60544f", "#ba8056", "#5e7fb6", "#a84e63", "#a8a9ac", "#333333", "#014357")


############ Load  Data  ###################

## LOCK LOGS
llFall <- read.csv("LockOperatorLogs/2025LockLogsFall.csv")
llSpring <- read.csv("LockOperatorLogs/2026LockLogsSpring.csv")
ll <- full_join(llFall,llSpring)

# Tagged Sturgeon
Tags <- read_csv("SCDNR_tagged_sturgeon_lists_and_metadata/allanimals_Jan2026.csv")

# Shared Fathom workspace with Santee Cooper - detections
Dets_2025 <- read.csv("I:/Shared drives/IFI/Projects_Active/SanteeCooper_FERC_19-04-09/IFI_TASKS/Task_Y_SturgUseofLock/Data/DNRReceiverData/detections_2026-01-21_21-23-45.csv")
Dets_2026 <- read.csv("I:/Shared drives/IFI/Projects_Active/SanteeCooper_FERC_19-04-09/IFI_TASKS/Task_Y_SturgUseofLock/Data/Fathom_Downloads/detections_2026-04-14_13-33-14.csv")


Dets <- full_join(Dets_2025, Dets_2026)


#Fathom live detections
#Dets2 <- read.csv("I:/Shared drives/IFI/Projects_Active/SanteeCooper_FERC_19-04-09/IFI_TASKS/Task_Y_SturgUseofLock/Data/DNRReceiverData/Fathom_live_detections_aug25_to_jan26")
Dets2 <- read.csv("I:/Shared drives/IFI/Projects_Active/SanteeCooper_FERC_19-04-09/IFI_TASKS/Task_Y_SturgUseofLock/Data/Fathom_Downloads/Fathom_Live_July25-April26.csv")

############ Tidy Detections Data  ###################
Dets_clean <- Dets %>% 
  rename(detection_timestamp_utc = Time..UTC.,
    receiver_sn             = Receiver.Serial,
    transmitter_id          = Full.ID,
    sensor_type             = Sensor,
    sensor_value            = Sensor.Value) %>% 
  mutate(transmitter_id = str_remove(transmitter_id, "^A69-"),
         source = "fathom_central",
         detection_timestamp_utc = as.POSIXct(detection_timestamp_utc, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
         receiver_sn = as.character(receiver_sn)) %>% 
  select(where(~ !all(is.na(.)))) 

drop_animal_cols <- c(
  "Animal.Weight..kg.",
  "Animal.Length..m.",
  "Length.Type",
  "Animal.Length2..m.",
  "Length2.Type",
  "Animal.Sex",
  "Tagger",
  "Release.Location",
  "Release.Latitude..deg.",
  "Release.Longitude..deg.",
  "Release.Date.and.Time..UTC."
)


Dets2_clean <- Dets2 %>% 
  rename(detection_timestamp_utc = Date.and.Time..UTC.,
    receiver_sn             = Receiver,
    transmitter_id          = ID,
    sensor_type             = Sensor,
    sensor_value            = Sensor.Value..ADC.) %>% 
  mutate(source = "fathom_live",
         detection_timestamp_utc = as.POSIXct(detection_timestamp_utc, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
         receiver_sn = str_remove(receiver_sn, "^VRXM-")) %>% 
  dplyr::select(where(~ !all(is.na(.)))) %>% 
  dplyr::select(-any_of(drop_animal_cols))



DF_join <- bind_rows(Dets_clean, Dets2_clean) %>% 
  distinct(
    detection_timestamp_utc,
    receiver_sn,
    transmitter_id,
    .keep_all = TRUE
  )


Tags_clean <- Tags %>% 
  clean_names() %>% 
  dplyr::select(c("species_common_name", "tag_id", "tagging_time", "sex", "capture_time", "release_time", "tagging_notes")) %>% 
  rename(transmitter_id = tag_id) %>% 
  mutate(transmitter_id = str_remove(transmitter_id, "^A69-"))

DF <- left_join(DF_join, Tags_clean) %>% 
  filter(!is.na(species_common_name)) %>% 
  distinct(detection_timestamp_utc, transmitter_id, .keep_all = T) %>% 
  mutate(receiver_location = case_when(
    str_detect(receiver_sn, "111745") ~ "RKM 76",
    str_detect(receiver_sn, "138299") ~ "RKM 73",
    str_detect(receiver_sn, "489380") ~ "RKM 77",
    str_detect(receiver_sn, "489374") ~ "Lock Entrance Channel",
    str_detect(receiver_sn, "457073") ~ "Lock Receivers",
    str_detect(receiver_sn, "457074") ~ "Lock Receiver West",
    TRUE ~ receiver_sn ))  %>% 
  mutate(
    receiver_location2 = case_when(
      receiver_location %in% c("Lock Receivers", "Lock Receiver West") ~ "Lock Receivers",
      TRUE ~ receiver_location)) %>% 
  mutate(AFS = case_when(
    species_common_name == "Shortnose sturgeon" ~ "Shortnose Sturgeon",
    species_common_name %in% c("Atlantic sturgeon", "Atlantic Sturgeon") ~ "Atlantic Sturgeon",
    TRUE ~ species_common_name)) %>% 
  mutate(transmitter = case_when(
    AFS == "Shortnose Sturgeon" ~ paste0("SS-", transmitter_id),
    AFS == "Atlantic Sturgeon" ~ paste0("AS-", transmitter_id),
    TRUE ~ transmitter_id )) %>% 
  mutate(Year = year(detection_timestamp_utc),
         Month = month(detection_timestamp_utc),
         Season = case_when(
           Year == 2025 & Month == 12 ~ "Spring",   # December 2025 → Spring
           Year == 2025 ~ "Fall",                  # All other 2025 → Fall
           Year == 2026 ~ "Spring",                # All 2026 → Spring
           TRUE ~ NA_character_)) %>% 
  mutate(detection_timestamp_est = with_tz(detection_timestamp_utc, tzone = "America/New_York")) %>% 
  remove_empty(c("rows", "cols")) %>% 
  mutate(date = as.Date(detection_timestamp_est)) %>% 
  filter(detection_timestamp_utc > "2025-07-01") 
  



# Test Tags: Test Tags: 9001-65003/ 9001-59293/ 1602-1234/ 1602-2468


###############################################################################
# Create a clean list of unique tag IDs from detections
detection_tags <- DF_join %>%
  filter(detection_timestamp_utc > "2025-07-01 19:53:01") %>% 
  distinct(transmitter_id) %>%
  pull(transmitter_id)

# Create a clean list of unique tag IDs from metadata
animal_tags <- Tags_clean %>%
  distinct(transmitter_id) %>%
  pull(transmitter_id)

# Find the tags in the detections that are NOT in the animal metadata
missing_tags <- setdiff(detection_tags, animal_tags)

# Print a clear message to the console
if (length(missing_tags) > 0) {
  cat( "Number of missing tag IDs:", length(missing_tags), "\n",
       "The following tag IDs were detected but are not in the animal metadata:\n")
 # cat(missing_tags, sep = "\n")
} else {
  cat("All detected tag IDs are present in the animal metadata.\n")
}

shad_tags <- read.csv("SCDNR_tagged_sturgeon_lists_and_metadata/shad_tags.csv") %>% clean_names() %>% 
  mutate(transmitter_id = str_remove(transmitter_id, "^A69-"),
         species_common_name = "Shad",
         tag_date = ymd_hm(tag_date),
         tag_date = replace_na(tag_date, ymd_hm("2026-01-31 12:00"))) 

sturgeon_tags <- Tags %>% clean_names() %>%  
  mutate(transmitter_id = tag_id, tag_date = capture_time) %>% 
  dplyr::select(c(tag_date, transmitter_id, species_common_name)) %>% 
  mutate(transmitter_id = str_remove(transmitter_id, "^A69-"))

known_tags <- full_join(shad_tags, sturgeon_tags)

FULL_DF <- left_join(DF_join, known_tags)

FULL_DF %>%
  mutate(date = as.Date(detection_timestamp_utc)) %>%
  group_by(date, species_common_name) %>%
  summarise(frq = n_distinct(transmitter_id), .groups = "drop") %>%
  ggplot(aes( x = date,  y = frq, fill = species_common_name, color = species_common_name )) +
  geom_col() +
  theme_bw() +
  labs( x = "Date",  y = "Number of Distinct Transmitter IDs" )

############ Lock Logs  ###################

time_cols <- c(
  "Exact.Time.Lower.Miter.Gates.Closed",
  "Exact.Time.Upper.Miter.Gates.Opened",
  "Exact.Time.Upper.Miter.Gates.Closed",
  "Exact.Time.Lower.Tainter.Gates.Opened",
  "Exact.Time.Lower.Tainter.Gates.Closed",
  "Exact.Time.Lower.Miter.Gates.Opened"
)

ll_final <- ll %>%
  mutate(Date = mdy(Date)) %>%                  
  mutate(across(all_of(time_cols), ~ na_if(., "#VALUE!"))) %>%  
  mutate(
    LowerMiter.Opened   = ymd_hm(paste(Date, `Exact.Time.Lower.Miter.Gates.Opened`), tz = "America/New_York"),
    LowerMiter.Closed   = ymd_hm(paste(Date, `Exact.Time.Lower.Miter.Gates.Closed`), tz = "America/New_York"),
    UpperMiter.Opened   = ymd_hm(paste(Date, `Exact.Time.Upper.Miter.Gates.Opened`), tz = "America/New_York"),
    UpperMiter.Closed   = ymd_hm(paste(Date, `Exact.Time.Upper.Miter.Gates.Closed`), tz = "America/New_York"),
    LowerTainter.Opened = ymd_hm(paste(Date, `Exact.Time.Lower.Tainter.Gates.Opened`), tz = "America/New_York"),
    LowerTainter.Closed = ymd_hm(paste(Date, `Exact.Time.Lower.Tainter.Gates.Closed`), tz = "America/New_York")
  ) %>%
  select(
    Date, Operators.Initials,
    LowerMiter.Opened, LowerMiter.Closed,
    UpperMiter.Opened, UpperMiter.Closed,
    LowerTainter.Opened, LowerTainter.Closed
  )



# The hardcoded time intervals for gate status
# This is a key assumption from the original code
lock_closed_start_time <- hms::parse_hms("20:00:00")
lock_closed_end_time <- hms::parse_hms("06:30:00")


############ Summary Statistics   ##################
DF %>%
  group_by(AFS, transmitter_id, Season) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(AFS, transmitter_id) %>%
  summarise(n_seasons = n_distinct(Season), .groups = "drop")  %>% 
  filter(n_seasons > 1 & AFS != NA) %>%
  pull(transmitter_id) %>%
  unique()

DF %>%  group_by(transmitter_id, Season, AFS) %>% 
  distinct(transmitter_id, Season, AFS) %>% 
  summarise(count = n()) %>% 
  group_by(transmitter_id, AFS) %>% 
  summarise(n_seasons = n()) %>% print(n = 40)

# no tag is seen in fall 2024 AND spring 2025

############   Figures         ##################################### #######
path = "I:/Shared drives/IFI/Projects_Active/SanteeCooper_FERC_19-04-09/IFI_TASKS/Task_Y_SturgUseofLock/Data/Figures_Working/2025 - 2026 Report/"

DF %>% 
 # filter(transmitter_id == "9001-65003" & receiver_location == "Lock Receivers" & Season == "Fall") %>% 
  filter(transmitter_id == "9001-65003" & receiver_location2 %in% c("Lock Receivers") & Season == "Fall") %>% 
  # filter(transmitter_id == "9001-65003" & receiver_location == "Lock Receiver West" & Season == "Fall") %>% 
    ggplot(aes(x = detection_timestamp_est, fill = receiver_location2 )) +
  geom_histogram(binwidth = 3600*24, position = "identity") +  
  theme_bw() +
  theme(panel.grid = element_blank()) +
  labs(x = "Time", y = "Frequency", 
       title = "Fall 2025: Internal Test Tag Detections",
       fill = "Receiver Location") +
  scale_fill_manual(values=c( "#030303","#00dd00", "#00aaff","#9d4eec", "orange", "red"),
                    breaks=c('Lock Receivers', 'RKM 77', 'RKM 76', 'RKM 73', 'Lock Entrance Channel', 'Lock Receiver West'))

ggsave(path = path, filename = "a1_Internal_Test_tag_Fall.jpeg", plot = last_plot(), width = 8, height = 7, units = "in", dpi = 300)

# Fall 2025
DF %>% 
  # filter(transmitter_id == "9001-65003" & receiver_location == "Lock Receivers" & Season == "Fall") %>% 
  filter(transmitter_id %in% c("9001-65003", "9001-59293") & receiver_location %in% c("Lock Receivers", "Lock Receiver West") & Season == "Fall") %>% 
  # filter(transmitter_id == "9001-65003" & receiver_location == "Lock Receiver West" & Season == "Fall") %>% 
  ggplot(aes(x = detection_timestamp_est, fill = receiver_location2 )) +
  geom_histogram(binwidth = 3600*24, position = "identity", alpha = .7) +  
  theme_bw() +
  theme(panel.grid = element_blank()) +
  labs(x = "Time", y = "Frequency", 
       title = "Fall 2025:  Test Tag Detections",
       fill = "Receiver Location") +
  scale_fill_manual(values=c( "#030303","#00dd00", "#00aaff","#9d4eec", "orange", "red"),
                    breaks=c('Lock Receivers', 'RKM 77', 'RKM 76', 'RKM 73', 'Lock Entrance Channel', 'Lock Receiver West')) +
  facet_grid(~transmitter_id, labeller = labeller(transmitter_id = c("9001-59293" = "External Tag","9001-65003" = "Internal Tag" )))

DF %>% 
  filter(transmitter_id %in% c("9001-65003", "9001-59293"),
         #receiver_location %in% c("Lock Receivers", "RKM 76", "RKM 77", 
          #                        "Lock Entrance Channel", "RKM 73"),
         Season == "Fall") %>% 
  ggplot(aes(x = detection_timestamp_est, group = receiver_location2, fill = receiver_location2)) + 
  geom_histogram(binwidth = 3600*24, position = "stack") +
  facet_wrap(~ transmitter_id + receiver_location2, ncol = 4,
             labeller = labeller(transmitter_id = c("9001-59293" = "External Tag","9001-65003" = "Internal Tag"  )) ) +
  scale_fill_manual(values = c("#030303", "#00dd00", "#00aaff", "#9d4eec", "orange"),
                    breaks = c("Lock Receivers", "RKM 77", "RKM 76", "RKM 73", "Lock Entrance Channel") ) +
  xlab("Date") +
  ylab("Frequency of detections per day") +
  ggtitle("Test Tag Overview Fall 2025") +
  labs(fill = "Receiver Location") +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 10)) +
  theme_bw()+
  theme(panel.grid = element_blank())
ggsave(path = path, filename = "Test_tag_overview_Fall_FINAL.jpeg", plot = last_plot(), width = 8, height = 7, units = "in", dpi = 300)




# Spring 2026
DF %>% 
  # filter(transmitter_id == "9001-65003" & receiver_location == "Lock Receivers" & Season == "Fall") %>% 
  filter(transmitter_id %in% c("9001-65003", "9001-59293") & receiver_location %in% c("Lock Receivers", "Lock Receiver West") & Season == "Spring") %>% 
  # filter(transmitter_id == "9001-65003" & receiver_location == "Lock Receiver West" & Season == "Fall") %>% 
  ggplot(aes(x = detection_timestamp_est, fill = receiver_location2 )) +
  geom_histogram(binwidth = 3600*24, position = "identity", alpha = .7) +  
  theme_bw() +
  theme(panel.grid = element_blank()) +
  labs(x = "Time", y = "Frequency", 
       title = "Spring 2026:  Test Tag Detections",
       fill = "Receiver Location") +
  scale_fill_manual(values=c( "#030303","#00dd00", "#00aaff","#9d4eec", "orange", "red"),
                    breaks=c('Lock Receivers', 'RKM 77', 'RKM 76', 'RKM 73', 'Lock Entrance Channel', 'Lock Receiver West')) +
  facet_grid(~transmitter_id, labeller = labeller(transmitter_id = c("9001-59293" = "External Tag","9001-65003" = "Internal Tag" )))


# Fall 2025
DF %>% 
  # filter(transmitter_id == "9001-65003" & receiver_location == "Lock Receivers" & Season == "Fall") %>% 
  filter(transmitter_id %in% c("9001-65003", "9001-59293") & receiver_location %in% c("Lock Entrance Channel") & Season == "Fall") %>% 
  # filter(transmitter_id == "9001-65003" & receiver_location == "Lock Receiver West" & Season == "Fall") %>% 
  ggplot(aes(x = detection_timestamp_est, fill = receiver_location2 )) +
  geom_histogram(binwidth = 3600*24, position = "identity", alpha = .7) +  
  theme_bw() +
  theme(panel.grid = element_blank()) +
  labs(x = "Time", y = "Frequency", 
       title = "Fall 2025:  Test Tag Detections",
       fill = "Receiver Location") +
  scale_fill_manual(values=c( "#030303","#00dd00", "#00aaff","#9d4eec", "orange", "red"),
                    breaks=c('Lock Receivers', 'RKM 77', 'RKM 76', 'RKM 73', 'Lock Entrance Channel', 'Lock Receiver West')) +
  facet_grid(~transmitter_id, labeller = labeller(transmitter_id = c("9001-59293" = "External Tag","9001-65003" = "Internal Tag" )))





DF %>% 
  filter(transmitter_id == "9001-65003" ) %>% 
  distinct(detection_timestamp_est, receiver_location) %>% 
  ggplot(aes(x = detection_timestamp_est, fill = receiver_location)) +
  geom_histogram(binwidth = 3600*24, position = "stack") +  
  theme_bw() +
  theme(panel.grid = element_blank()) +
  labs(x = "Time", y = "Frequency", 
       title = "Spring 2026: Internal Test Tag Detections",
       fill = "Receiver ID")  +
  facet_grid(~receiver_location) +
  scale_fill_manual(values=c( "#030303","#00dd00", "#00aaff","#9d4eec", "orange"),
                    breaks=c('Lock Receivers', 'RKM 77', 'RKM 76', 'RKM 73', 'Lock Entrance Channel'))





# Take a closer look bc this should be swapped 
DF %>% 
 # filter(transmitter_id == "9001-59293" & receiver_location == "Lock Receivers") %>% 
  filter(transmitter_id == "9001-59293" & receiver_location %in% c("Lock Receivers", "Lock Receiver West")) %>% 
 # filter(Season == "Spring") %>% 
  mutate(time = hms::as_hms(detection_timestamp_est), 
         gate_status = ifelse(time >= "20:00:00" | time <= "06:30:00", "Closed", "Open")) %>% 
  group_by(date, receiver_location, gate_status) %>%
  ggplot(aes(x = detection_timestamp_est, group = receiver_location, fill = receiver_location), alpha = .5) + 
  geom_histogram(binwidth = 3600*24, position = "stack") +
  scale_fill_manual(values = c("#030303", "red")) +
#  facet_wrap(~ gate_status) +
  xlab("Date") +
  ylab("Frequency") +
  labs(fill = "Receiver Location") +
  ggtitle("Spring 2026 External Test Tag Detections by Gate Status") +
  theme(plot.title = element_text(size = 10))+
  theme_bw() +
  theme(panel.grid = element_blank())
ggsave(path = path, filename = "a5_External_Test_tag_spring.jpeg", plot = last_plot(), width = 8, height = 7, units = "in", dpi = 300)



# Take a closer look bc this should be swapped 
DF %>% 
  filter(transmitter_id == "9001-59293" & receiver_location == "Lock Receivers") %>% 
  filter(Season == "Fall") %>% 
  mutate(time = hms::as_hms(detection_timestamp_est), 
         gate_status = ifelse(time >= "20:00:00" | time <= "06:30:00", "Closed", "Open")) %>% 
  ggplot(aes(x = detection_timestamp_est, group = receiver_location, fill = receiver_location)) + 
  geom_histogram(binwidth = 3600*24, position = "stack") +
  scale_fill_manual(values = c("#030303")) +
  facet_wrap(~ gate_status) +
  xlab("Date") +
  ylab("Frequency") +
  labs(fill = "Receiver Location") +
  ggtitle("Fall 2025 External Test Tag Detections by Gate Status") +
  theme(plot.title = element_text(size = 10))+
  theme_bw() +
  theme(panel.grid = element_blank())
ggsave(path = path, filename = "a5_External_Test_tag_fall.jpeg", plot = last_plot(), width = 8, height = 7, units = "in", dpi = 300)



# Setting up plot for internal tag detections st rkm 77
DF %>% 
  filter(transmitter_id == "9001-65003" & receiver_location == "Lock Entrance Channel") %>% 
  filter(Season == "Spring") %>% 
  mutate(time = hms::as_hms(detection_timestamp_est),
         gate_status = ifelse(time >= "20:00:00" | time <= "06:30:00", "Closed", "Open")) %>% 
  ggplot(aes(x = detection_timestamp_est, group = receiver_location, fill = receiver_location)) + 
  geom_histogram(binwidth = 3600*24, position = "stack") +
  scale_fill_manual(values = c("orange")) +
  facet_wrap(~ gate_status) +
  xlab("Date") +
  ylab("Frequency") +
  labs(fill = "Receiver Location") +
  ggtitle("Spring 2026 Internal Test Tag Detections by Gate Status") +
  theme(plot.title = element_text(size = 10)) +
  theme_bw() +
  theme(panel.grid = element_blank())
ggsave(path = path, filename = "a6_Spring_Internal_Test_tag_lock_entrance.jpeg", plot = last_plot(), width = 8, height = 7, units = "in", dpi = 300)

DF %>% 
  filter(transmitter_id == "9001-65003" & receiver_location == "Lock Entrance Channel") %>% 
  filter(Season == "Fall") %>% 
  mutate(time = hms::as_hms(detection_timestamp_est),
         gate_status = ifelse(time >= "20:00:00" | time <= "06:30:00", "Closed", "Open")) %>% 
  ggplot(aes(x = detection_timestamp_est, group = receiver_location, fill = receiver_location)) + 
  geom_histogram(binwidth = 3600*24, position = "stack") +
  scale_fill_manual(values = c("orange"))  +
  facet_wrap(~ gate_status) +
  xlab("Date") +
  ylab("Frequency") +
  labs(fill = "Receiver Location") +
  ggtitle("Fall 2025 Internal Test Tag Detections by Gate Status") +
  theme(plot.title = element_text(size = 10)) +
  theme_bw() +
  theme(panel.grid = element_blank())
ggsave(path = path, filename = "a7_Fall_Internal_Test_tag_lock_entrance.jpeg", plot = last_plot(), width = 8, height = 7, units = "in", dpi = 300)

DF %>% 
  filter(transmitter_id %in% c("9001-65003", "9001-59293"),
         receiver_location %in% c("Lock Receivers", "RKM 76", "RKM 77", 
                             "Lock Entrance Channel", "RKM 73"),
         Season == "Spring") %>% 
  ggplot(aes(x = detection_timestamp_est, group = receiver_location, fill = receiver_location)) + 
  geom_histogram(binwidth = 3600*24, position = "stack") +
  facet_wrap(~ transmitter_id + receiver_location, ncol = 4,
             labeller = labeller(transmitter_id = c("9001-59293" = "External Tag","9001-65003" = "Internal Tag"  )) ) +
  scale_fill_manual(values = c("#030303", "#00dd00", "#00aaff", "#9d4eec", "orange"),
                    breaks = c("Lock Receivers", "RKM 77", "RKM 76", "RKM 73", "Lock Entrance Channel") ) +
  xlab("Date") +
  ylab("Frequency of detections per day") +
  ggtitle("Test Tag Overview Spring 2026") +
  labs(fill = "Receiver Location") +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 10)) +
  theme_bw()+
  theme(panel.grid = element_blank())
ggsave(path = path, filename = "a8_Test_tag_overview_Spring_FINAL.jpeg", plot = last_plot(), width = 8, height = 7, units = "in", dpi = 300)

DF %>% 
  filter(transmitter_id %in% c("9001-65003", "9001-59293"),
         receiver_location %in% c("Lock Receivers", "RKM 76", "RKM 77", 
                             "Lock Entrance Channel", "RKM 73"),
         Season == "Fall") %>% 
  group_by(date, transmitter_id, receiver_location) %>% 
  summarise(sum_day = n(), .groups = "drop") %>% 
  ggplot(aes(x = date, y = sum_day, fill = receiver_location)) +
  geom_col() +
  facet_wrap(~ transmitter_id + receiver_location, ncol = 4,
             labeller = labeller(transmitter_id = c("9001-59293" = "External Tag","9001-65003" = "Internal Tag"  )) ) +
  scale_fill_manual(values = c("#030303", "#00dd00", "#00aaff", "#9d4eec", "orange"),
                    breaks = c("Lock Receivers", "RKM 77", "RKM 76", "RKM 73", "Lock Entrance Channel") ) +
  xlab("Date") +
  ylab("Frequency of detections per hour") +
  ggtitle("Test Tag Overview Fall 2025") +
  labs(fill = "Receiver Location") +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 10)) +
  theme_bw()+
  theme(panel.grid = element_blank())
ggsave(path = path, filename = "a9_Test_tag_overview_Fall_FINAL.jpeg", plot = last_plot(), width = 8, height = 7, units = "in", dpi = 300)


### Figuring out why there are gaps in detectability from the receivers inside the lock:

DF %>% 
  filter(transmitter_id %in% c("9001-65003", "9001-59293"),
         receiver_location %in% c("Lock Receivers", "Lock Receiver West"),
         Season == "Fall") %>% 
  group_by(date, transmitter_id, receiver_location) %>% 
  summarise(sum_day = n(), .groups = "drop") %>% 
  ggplot(aes(x = date, y = sum_day, fill = receiver_location)) +
  geom_col() +
  facet_wrap(~ transmitter_id + receiver_location, ncol = 2,
             labeller = labeller(transmitter_id = c("9001-59293" = "External Tag","9001-65003" = "Internal Tag"  )) ) +
  scale_fill_manual(values = c("#030303", "#00dd00", "#00aaff", "#9d4eec", "orange", "red"),
                    breaks = c("Lock Receivers", "RKM 77", "RKM 76", "RKM 73", "Lock Entrance Channel", "Lock Receiver West") ) +
  xlab("Date") +
  ylab("Frequency of detections per hour") +
  ggtitle("Test Tag Overview Fall 2025") +
  labs(fill = "Receiver Location") +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 10)) +
  theme_bw()+
  theme(panel.grid = element_blank())
ggsave(path = path, filename = "a9_Test_tag_overview_Fall_FINAL.jpeg", plot = last_plot(), width = 8, height = 7, units = "in", dpi = 300)



DF %>% 
  filter(AFS %in% c("Shortnose Sturgeon", "Atlantic Sturgeon")) %>% 
  filter(Season == "Fall") %>% 
  group_by(date,receiver_location) %>%
  summarize(freq=n()) %>%
  ggplot(aes(x = date, y = freq, group=receiver_location,fill=receiver_location, label=freq)) + 
  geom_col() +
  #geom_text(size = 5, position = position_stack(vjust = 0.5)) +
  xlab("Date") +
  ylab("Frequency of detections per day") +
  labs(fill = "Receiver")+
  guides(fill=guide_legend(reverse = TRUE))+ 
  ggtitle("Fall 2025 Sturgeon Detections By Receiver")+
  theme(plot.title = element_text(size = 10))+
  scale_fill_manual(values = c( "#030303","#00dd00", "#00aaff","#9d4eec", "orange", "red"),
                    breaks=c('Lock Receivers', 'RKM 77', 'RKM 76', 'RKM 73', 'Lock Entrance Channel')) +
  theme_bw()+
  theme(panel.grid = element_blank()) 
  
ggsave(path = path, filename = "a10_Fall_Detections_by_receiver_non_facet_V2.jpeg", plot = last_plot(), width = 8, height = 7, units = "in", dpi = 300)


DF %>% 
  filter(AFS %in% c("Shortnose Sturgeon", "Atlantic Sturgeon")) %>% 
  filter(Season == "Fall") %>% 
  ggplot(aes(x = detection_timestamp_est, group=receiver_location2,fill=receiver_location2)) + 
  geom_histogram(binwidth = 3600*24, position = "stack") +
  #geom_text(size = 5, position = position_stack(vjust = 0.5)) +
  xlab("Date") +
  ylab("Frequency of detections per day") +
  labs(fill = "Receiver")+
  guides(fill=guide_legend(reverse = TRUE))+ 
  ggtitle("Fall 2025 Sturgeon Detections By Receiver")+
  theme(plot.title = element_text(size = 10))+
  scale_fill_manual(values = c( "#030303","#00dd00", "#00aaff","#9d4eec", "orange", "red"),
                    breaks=c('Lock Receivers', 'RKM 77', 'RKM 76', 'RKM 73', 'Lock Entrance Channel')) +
  theme_bw()+
  theme(panel.grid = element_blank()) 
ggsave(path = path, filename = "Fall_Detections_by_receiver_non_facet.jpeg", plot = last_plot(), width = 8, height = 7, units = "in", dpi = 300)




DF %>% 
  filter(AFS %in% c("Shortnose Sturgeon", "Atlantic Sturgeon")) %>% 
  filter(Season == "Spring") %>% 
  ggplot(aes(x = detection_timestamp_est, group=receiver_location2,fill=receiver_location2)) + 
  geom_histogram(binwidth = 3600*24, position = "stack") +
  #geom_text(size = 5, position = position_stack(vjust = 0.5)) +
  geom_vline(xintercept = as.POSIXct("2026-02-10"), linetype = "dashed") +
  geom_vline(xintercept = as.POSIXct("2026-04-10"), linetype = "dashed") +
  xlab("Date") +
  ylab("Frequency of detections per day") +
  labs(fill = "Receiver")+
  guides(fill=guide_legend(reverse = TRUE))+ 
  ggtitle("Spring 2026 Sturgeon Detections By Receiver")+
  theme(plot.title = element_text(size = 10))+
  scale_fill_manual(values = c( "#030303","#00dd00", "#00aaff","#9d4eec", "orange", "red"),
                    breaks=c('Lock Receivers', 'RKM 77', 'RKM 76', 'RKM 73', 'Lock Entrance Channel')) +
  theme_bw()+
  theme(panel.grid = element_blank()) 
ggsave(path = path, filename = "Spring_Detections_by_receiver_non_facet.jpeg", plot = last_plot(), width = 8, height = 7, units = "in", dpi = 300)









DF %>% 
  filter(AFS %in% c("Shortnose Sturgeon", "Atlantic Sturgeon")) %>% 
  filter(Season == "Spring") %>% 
  filter(!is.na(receiver_location)) %>% 
  group_by(date,receiver_location, transmitter_id) %>%
  summarize(freq=n()) %>%
  mutate(receiver_location = factor(receiver_location,
                               levels = c("Lock Receivers",
                                          "Lock Entrance Channel",
                                          "RKM 77",
                                          "RKM 76",
                                          "RKM 73"))) %>% 
  ggplot(aes(x = date, y = freq, group=receiver_location,fill=transmitter_id, label=freq)) + 
  geom_col() +
  #geom_text(size = 5, position = position_stack(vjust = 0.5)) +
  xlab("Date") +
  ylab("Frequency of detections per day") +
  labs(fill = "Sturgeon")+
  guides(fill=guide_legend(reverse = TRUE))+ 
  ggtitle("Spring 2026 Sturgeon Detections By Receiver")+
  theme(plot.title = element_text(size = 10))+
  theme_bw() +
  facet_wrap(~receiver_location)+
  theme(panel.grid = element_blank())
ggsave(path = path, filename = "a12_Spring_Detections_by_receiver.jpeg", plot = last_plot(), width = 8, height = 7, units = "in", dpi = 300)


DF %>% 
  filter(AFS %in% c("Shortnose Sturgeon", "Atlantic Sturgeon")) %>% 
  filter(Season == "Fall") %>% 
  filter(!is.na(receiver_location)) %>%
  mutate(receiver_location = factor(receiver_location,
                               levels = c("Lock Receivers",
                                          "Lock Entrance Channel",
                                          "RKM 77",
                                          "RKM 76",
                                          "RKM 73"))) %>% 
  ggplot(aes(x = detection_timestamp_est, group= receiver_location, fill=transmitter_id)) + 
  geom_histogram(binwidth = 3600*24, position = "stack") +
  #geom_text(size = 5, position = position_stack(vjust = 0.5)) +
  xlab("Date") +
  ylab("Frequency of detections per day") +
  labs(fill = "Sturgeon")+
  guides(fill=guide_legend(reverse = TRUE))+ 
  ggtitle("Fall 2025 Sturgeon Detections By Receiver")+
  theme(plot.title = element_text(size = 10))+
  theme_bw() +
  facet_wrap(~receiver_location, ncol = 2)+
  theme(panel.grid = element_blank())

ggsave(path = path, filename = "a13_Fall_Detections_by_receiver.jpeg", plot = last_plot(), width = 8, height = 7, units = "in", dpi = 300)


#####  Individual Day Histories Figures #####

ids <- unique(DF$transmitter_id)
seasons <- unique(DF$Season)
counter <- 1 

for (season in seasons) {
  for (id in ids) {
    df_hourly <- DF %>%
      filter(AFS %in% c("Shortnose Sturgeon", "Atlantic Sturgeon"),
             transmitter_id == id,
             Season == season) %>%
      mutate(hour_of_day = lubridate::hour(detection_timestamp_est)) %>% 
      group_by(date, hour_of_day, receiver_location2, AFS) %>%
      summarise(detections = n(), .groups = "drop") %>%
      filter(detections > 0)  # omit hours with zero detections
    
    if (nrow(df_hourly) == 0) next  # skip animals with no detections
    
    species_name <- unique(df_hourly$AFS)
    year_val <- unique(lubridate::year(df_hourly$date))
    
    p <- ggplot(df_hourly, aes(x = hour_of_day, y = detections, fill = receiver_location2)) +
      geom_col() +
      labs(
        title = paste(season, year_val, ": Hourly Detections for Transmitter", id, "-", species_name),
        x = "Hour of Day",
        y = "Detections per Hour",
        fill = "Receiver Location"
      ) +
      theme_bw() +
      facet_wrap(~date) +
      scale_fill_manual(values=c( "#030303","#00dd00", "#00aaff","#9d4eec", "orange", "red"),
                        breaks=c('Lock Receivers', 'RKM 77', 'RKM 76', 'RKM 73', 'Lock Entrance Channel', "Lock Receiver West"))+
      theme(panel.grid = element_blank())
    
    print(p)
    
    # dynamically scale width based on number of dates (facets)
    n_facets <- length(unique(df_hourly$date))
    plot_width  <- min(50, max(8, n_facets * 1)) 
    plot_height <- min(50, max(6, n_facets * 0.65))  
    
    
    # scale font size with plot dimensions (try tweaking the factor)
    base_font <- 10
    scale_factor <- (plot_width / 10)  # proportional to width
    font_size <- base_font * scale_factor
    
    p <- p +
      theme_bw(base_size = font_size) +   # global font scaling
      theme(
        panel.grid = element_blank(),
        strip.text = element_text(size = font_size * 0.8), # facet labels
        axis.text  = element_text(size = font_size * 0.6),
        axis.title = element_text(size = font_size * 0.8),
        legend.text = element_text(size = font_size * 0.6),
        legend.title = element_text(size = font_size * 0.8),
        plot.title  = element_text(size = font_size)
      )
    
    save_folder <- "I:/Shared drives/IFI/Projects_Active/SanteeCooper_FERC_19-04-09/IFI_TASKS/Task_Y_SturgUseofLock/Data/Figures_Working/2025 - 2026 Report/"
    
    file_name <- paste0(save_folder, "bb", counter, "_detections_", season, "_", year_val, "_", id, "_", gsub(" ", "_", species_name), ".jpeg")
    
    # ggsave(filename = file_name, plot = p, width = plot_width, height = plot_height, units = "in", dpi = 300, limitsize = FALSE)
    counter <- counter + 1 
  }
}



#####  Lock Receivers Timing with ISH #####
ids <- unique(DF$transmitter_id)
seasons <- unique(DF$Season)
save_folder <- "I:/Shared drives/IFI/Projects_Active/SanteeCooper_FERC_19-04-09/IFI_TASKS/Task_Y_SturgUseofLock/Data/Figures_Working/2025 - 2026 Report/"
counter <- 1

ll_rect <- ll_final %>%  
  mutate(date = as.Date(LowerTainter.Opened),
         xmin = hour(LowerTainter.Opened) + minute(LowerTainter.Opened)/60,
         xmax = hour(LowerTainter.Closed) + minute(LowerTainter.Closed)/60) %>%
  dplyr::select(date, xmin, xmax)


for (season in seasons) {
  for (id in ids) {
    df_hourly <- DF %>%
      filter(AFS %in% c("Shortnose Sturgeon", "Atlantic Sturgeon"),
             transmitter_id == id,
             Season == season) %>%
      mutate(hour_of_day = lubridate::hour(detection_timestamp_est)) %>%
      group_by(date, hour_of_day, receiver_location2, AFS) %>%
      summarise(detections = n(), .groups = "drop") %>%
      filter(detections > 0)
    
    if (nrow(df_hourly) == 0) next
    
    species_name <- unique(df_hourly$AFS)
    year_val <- unique(lubridate::year(df_hourly$date))
    
    # filter lock rectangles to dates for this fish
    ll_rect_sub <- ll_rect %>% filter(date %in% unique(df_hourly$date))
    
    p <- ggplot(df_hourly, aes(x = hour_of_day, y = detections, fill = receiver_location2)) +
      geom_col() +
      geom_rect(data = ll_rect_sub,
                aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
                fill = "grey30", alpha = 0.12, inherit.aes = FALSE) +
      facet_wrap(~date) +
      labs(
        title = paste(season, year_val, ": Hourly Detections for Transmitter", id, "-", species_name),
        x = "Hour of Day", y = "Detections per Hour", fill = "Receiver Location"
      ) +
      scale_fill_manual(values=c("#030303","#00dd00","#00aaff","#9d4eec","orange", "red"),
                        breaks=c('Lock Receivers','RKM 77','RKM 76','RKM 73','Lock Entrance Channel', "Lock Receiver West")) +
      theme_bw()+
      theme(panel.grid = element_blank())
    
    n_facets <- length(unique(df_hourly$date))
    plot_width  <- min(50, max(8, n_facets * 1)) 
    plot_height <- min(50, max(6, n_facets * 0.65))  
    
    # scale font size with plot dimensions (try tweaking the factor)
    base_font <- 10
    scale_factor <- (plot_width / 10)  # proportional to width
    font_size <- base_font * scale_factor
    
    p <- p +
      theme_bw(base_size = font_size) +   # global font scaling
      theme(
        panel.grid = element_blank(),
        strip.text = element_text(size = font_size * 0.8), # facet labels
        axis.text  = element_text(size = font_size * 0.6),
        axis.title = element_text(size = font_size * 0.8),
        legend.text = element_text(size = font_size * 0.6),
        legend.title = element_text(size = font_size * 0.8),
        plot.title  = element_text(size = font_size)
      )
    
    print(p)
    file_name <- paste0(save_folder, "NEW", counter, "_detections_with_locks_", season, "_", year_val, "_", id, "_", gsub(" ", "_", species_name), ".jpeg")
    ggsave(filename = file_name, plot = p, width = plot_width, height = plot_height, units = "in", dpi = 300, limitsize = FALSE)
    counter <- counter + 1 
  }
}


#####  Lock Receivers Timing with ISH - Zoomed In #####
ids <- unique(DF$transmitter_id)
seasons <- unique(DF$Season)
dates <- unique(DF$date)
save_folder <- "I:/Shared drives/IFI/Projects_Active/SanteeCooper_FERC_19-04-09/IFI_TASKS/Task_Y_SturgUseofLock/Data/Figures_Working/2025 - 2026 Report/"
counter <- 1

ll_rect <- ll_final %>%  
  mutate(date = as.Date(LowerTainter.Opened),
         xmin = hour(LowerTainter.Opened) + minute(LowerTainter.Opened)/60,
         xmax = hour(LowerTainter.Closed) + minute(LowerTainter.Closed)/60) %>%
  dplyr::select(date, xmin, xmax)



for (season in seasons) {
  for (id in ids) {
    valid_dates <- DF %>%
      filter(AFS %in% c("Shortnose Sturgeon", "Atlantic Sturgeon"),
             transmitter_id == id,
             Season == season,
             receiver_location == "Lock Receivers") %>%
      pull(date) %>%
      unique()
    
    # if no valid dates, skip this fish-season combo
    if (length(valid_dates) == 0) next
    
    for (date_val in valid_dates) {
      df_hourly <- DF %>%
        filter(AFS %in% c("Shortnose Sturgeon", "Atlantic Sturgeon"),
               transmitter_id == id,
               Season == season,
               date == date_val) %>%
        mutate(hour_of_day = lubridate::hour(detection_timestamp_est)) %>%
        group_by(date, hour_of_day, receiver_location2, AFS) %>%
        summarise(detections = n(), .groups = "drop") %>%
        filter(detections > 0)
      
      if (nrow(df_hourly) == 0) next
      
      if (nrow(df_hourly) == 0) next
      
      species_name <- unique(df_hourly$AFS)
      year_val <- unique(lubridate::year(df_hourly$date))
      
      # filter lock rectangles to dates for this fish
      ll_rect_sub <- ll_rect %>% filter(date %in% unique(df_hourly$date))
      
      p <- ggplot(df_hourly, aes(x = hour_of_day, y = detections, fill = receiver_location2)) +
        geom_col() +
        geom_rect(data = ll_rect_sub,
                  aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
                  fill = "grey30", alpha = 0.12, inherit.aes = FALSE) +
        facet_wrap(~date) +
        labs(
          title = paste(season, year_val, ": Hourly Detections for Transmitter", id, "-", species_name),
          x = "Hour of Day", y = "Detections per Hour", fill = "Receiver Location"
        ) +
        scale_fill_manual(values=c("#030303","#00dd00","#00aaff","#9d4eec","orange"),
                          breaks=c('Lock Receivers','RKM 77','RKM 76','RKM 73','Lock Entrance Channel')) +
        theme_bw()+
        theme(panel.grid = element_blank())
      
      n_facets <- length(unique(df_hourly$date))
      plot_width  <- min(50, max(8, n_facets * 1)) 
      plot_height <- min(50, max(6, n_facets * 0.65))  
      
      print(p)
      file_name <- paste0(save_folder, "d",counter, "_detections_with_locks_", season, "_", year_val, "_", id, "_", date_val, gsub(" ", "_", species_name), ".jpeg")
     ggsave(filename = file_name, plot = p, width = plot_width, height = plot_height, units = "in", dpi = 300, limitsize = FALSE)
      counter <- counter +1
    }
  }
}


#####  Saving to Powerpoint  ###########################

#doc <- read_pptx()

#files <- list.files(save_folder, pattern = "\\.jpeg$", full.names = TRUE)

#for (f in files) {
#  doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
#  doc <- ph_with(doc, value = basename(f), location = ph_location_type(type = "title"))
#  doc <- ph_with(doc, external_img(f, width = 9, height = 6),
#                 location = ph_location_type(type = "body"))
#}

#print(doc, target = "detections_review_V4.pptx")


#####  Sturgeon Tagged in 2025       #############



DF %>%  
  filter(transmitter_id == "9001-54543") %>% 
  filter(date < "2025-03-05") %>% 
  mutate(hour_of_day = lubridate::hour(detection_timestamp_est)) %>% 
  group_by(date, hour_of_day, receiver_location, AFS,transmitter_id) %>%
  summarise(detections = n(), .groups = "drop") %>% 
  ggplot(aes(x = hour_of_day, y = detections, fill = receiver_location)) +
  geom_col() +
  labs(title = "Spring 2025: Hourly Detections for Transmitter 9001-54543 - Shortnose Sturgeon",
       x = "Hour of Day",
       y = "Detections per Hour",
       fill = "Receiver Location") +
  theme_bw() +
  facet_wrap(~date) +
  theme(panel.grid = element_blank()) +
  scale_fill_manual(values=c( "#030303","#00dd00", "#00aaff","#9d4eec", "orange"),
                    breaks=c('Lock Receivers', 'RKM 77', 'RKM 76', 'RKM 73', 'Lock Entrance Channel')) 

ggsave(filename = "I:/Shared drives/IFI/Projects_Active/SanteeCooper_FERC_19-04-09/IFI_TASKS/Task_Y_SturgUseofLock/Data/Figures_Working/Spring2025/2025 - 2026 Report/e1_2025_Tagged_sturgeon.jpeg", plot = last_plot(), width = 8, 
       height = 6, units = "in", dpi = 300, limitsize = FALSE)


DF %>%  
  filter(transmitter_id == "9001-54544") %>% 
  filter(date < "2025-03-08") %>% 
  mutate(hour_of_day = lubridate::hour(detection_timestamp_est)) %>% 
  group_by(date, hour_of_day, receiver_location, AFS,transmitter_id) %>%
  summarise(detections = n(), .groups = "drop") %>% 
  ggplot(aes(x = hour_of_day, y = detections, fill = receiver_location)) +
  geom_col() +
  labs(title = "Spring 2025: Hourly Detections for Transmitter 9001-54544 - Shortnose Sturgeon",
       x = "Hour of Day",
       y = "Detections per Hour",
       fill = "Receiver Location") +
  theme_bw() +
  facet_wrap(~date) +
  theme(panel.grid = element_blank()) +
  scale_fill_manual(values=c( "#030303","#00dd00", "#00aaff","#9d4eec", "orange"),
                    breaks=c('Lock Receivers', 'RKM 77', 'RKM 76', 'RKM 73', 'Lock Entrance Channel')) 

ggsave(filename = "I:/Shared drives/IFI/Projects_Active/SanteeCooper_FERC_19-04-09/IFI_TASKS/Task_Y_SturgUseofLock/Data/Figures_Working/Spring2025/2025 - 2026 Report/e2_2025_Tagged_sturgeon.jpeg", 
       plot = last_plot(), width = 8, 
       height = 6, units = "in", dpi = 300, limitsize = FALSE)

DF %>%  
  filter(transmitter_id == "9001-54545") %>% 
  filter(date < "2025-03-18") %>% 
  mutate(hour_of_day = lubridate::hour(detection_timestamp_est)) %>% 
  group_by(date, hour_of_day, receiver_location, AFS,transmitter_id) %>%
  summarise(detections = n(), .groups = "drop") %>% 
  ggplot(aes(x = hour_of_day, y = detections, fill = receiver_location)) +
  geom_col() +
  labs(title = "Spring 2025: Hourly Detections for Transmitter 9001-54545 - Shortnose Sturgeon",
       x = "Hour of Day",
       y = "Detections per Hour",
       fill = "Receiver Location") +
  theme_bw() +
  facet_wrap(~date) +
  theme(panel.grid = element_blank()) +
  scale_fill_manual(values=c( "#030303","#00dd00", "#00aaff","#9d4eec", "orange"),
                    breaks=c('Lock Receivers', 'RKM 77', 'RKM 76', 'RKM 73', 'Lock Entrance Channel')) 

ggsave(filename = "I:/Shared drives/IFI/Projects_Active/SanteeCooper_FERC_19-04-09/IFI_TASKS/Task_Y_SturgUseofLock/Data/Figures_Working/Spring2025/2025 - 2026 Report/e3_2025_Tagged_sturgeon.jpeg", 
       plot = last_plot(), width = 8, 
       height = 6, units = "in", dpi = 300, limitsize = FALSE)

DF %>%  
  filter(transmitter_id == "9001-54546") %>% 
  filter(date < "2025-03-25") %>% 
  mutate(hour_of_day = lubridate::hour(detection_timestamp_est)) %>% 
  group_by(date, hour_of_day, receiver_location, AFS,transmitter_id) %>%
  summarise(detections = n(), .groups = "drop") %>% 
  ggplot(aes(x = hour_of_day, y = detections, fill = receiver_location)) +
  geom_col() +
  labs(title = "Spring 2025: Hourly Detections for Transmitter 9001-54546 - Shortnose Sturgeon",
       x = "Hour of Day",
       y = "Detections per Hour",
       fill = "Receiver Location") +
  theme_bw() +
  facet_wrap(~date) +
  theme(panel.grid = element_blank()) +
  scale_fill_manual(values=c( "#030303","#00dd00", "#00aaff","#9d4eec", "orange"),
                    breaks=c('Lock Receivers', 'RKM 77', 'RKM 76', 'RKM 73', 'Lock Entrance Channel')) 

ggsave(filename = "I:/Shared drives/IFI/Projects_Active/SanteeCooper_FERC_19-04-09/IFI_TASKS/Task_Y_SturgUseofLock/Data/Figures_Working/Spring2025/2025 - 2026 Report/e4_2025_Tagged_sturgeon.jpeg",
       plot = last_plot(), width = 8, 
       height = 6, units = "in", dpi = 300, limitsize = FALSE)

DF %>%  
  filter(transmitter_id == "9001-54547") %>% 
  filter(date < "2025-04-08") %>% 
  mutate(hour_of_day = lubridate::hour(detection_timestamp_est)) %>% 
  group_by(date, hour_of_day, receiver_location, AFS,transmitter_id) %>%
  summarise(detections = n(), .groups = "drop") %>% 
  ggplot(aes(x = hour_of_day, y = detections, fill = receiver_location)) +
  geom_col() +
  labs(title = "Spring 2025: Hourly Detections for Transmitter 9001-54547 - Shortnose Sturgeon",
       x = "Hour of Day",
       y = "Detections per Hour",
       fill = "Receiver Location") +
  theme_bw() +
  facet_wrap(~date) +
  theme(panel.grid = element_blank()) +
  scale_fill_manual(values=c( "#030303","#00dd00", "#00aaff","#9d4eec", "orange"),
                    breaks=c('Lock Receivers', 'RKM 77', 'RKM 76', 'RKM 73', 'Lock Entrance Channel')) 

ggsave(filename = "I:/Shared drives/IFI/Projects_Active/SanteeCooper_FERC_19-04-09/IFI_TASKS/Task_Y_SturgUseofLock/Data/Figures_Working/Spring2025/2025 - 2026 Report/e5_2025_Tagged_sturgeon.jpeg", 
       plot = last_plot(), width = 8, 
       height = 6, units = "in", dpi = 300, limitsize = FALSE)

##

DF %>%  
  filter(transmitter_id == "9001-54543") %>% 
  filter(date < "2025-03-10") %>% 
  ggplot(aes(x = timestamp, y = receiver_location, color = receiver_location)) +
  geom_point(size = 2) +
  labs(title = "Spring 2025: Hourly Detections for Transmitter 9001-54543 - Shortnose Sturgeon",
       x = "Date",
       y = "Receiver Location",
       color = "Receiver Location") +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  scale_color_manual(values=c( "#030303","#00dd00", "#00aaff","#9d4eec", "orange"),
                     breaks=c('Lock Receivers', 'RKM 77', 'RKM 76', 'RKM 73', 'Lock Entrance Channel')) 


DF %>%  
  filter(transmitter_id == "9001-54544") %>% 
  filter(date < "2025-03-15") %>% 
  ggplot(aes(x = timestamp, y = receiver_location, color = receiver_location)) +
  geom_point(size = 2) +
  labs(title = "Spring 2025: Hourly Detections for Transmitter 9001-54544 - Shortnose Sturgeon",
       x = "Date",
       y = "Receiver Location",
       color = "Receiver Location") +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  scale_color_manual(values=c( "#030303","#00dd00", "#00aaff","#9d4eec", "orange"),
                     breaks=c('Lock Receivers', 'RKM 77', 'RKM 76', 'RKM 73', 'Lock Entrance Channel')) 





#spring_tags <- DF %>% filter(Season == "Spring") %>%  distinct(transmitter_id, AFS, `Animal Common Name`, Animal_Number) %>% group_by(Animal_Number) %>%  print(n = 30)
#spring_tags2 <- left_join(spring_tags, tags_clean, by = "Animal_Number") %>%  dplyr::select(Animal_Number, AFS, transmitter_id.x, `Release Time`) %>%  distinct(.) %>%  filter(!is.na(AFS))




################################################################################


ids <- unique(DF$transmitter_id)

ll_rect2 <- ll_rect %>% 
  mutate(
    xmin = as.POSIXct(xmin, tz = "America/New_York"),
    xmax = as.POSIXct(xmax, tz = "America/New_York")
  )
for (id in ids) {
  Animal.Common.Name <- unique(DF$Animal.Common.Name)
  p <- DF %>% 
    filter(transmitter_id == id) %>% 
    mutate(receiver_location = factor(
        receiver_location, levels = c("RKM 73", "RKM 76", "RKM 77", "Lock Entrance Channel",
                                      "Lock Receiver West", "Lock Receivers"))) %>% 
    ggplot(aes(x = detection_timestamp_est, y = receiver_location, color = receiver_location)) +
    geom_point() +
    geom_rect(data = ll_final,
              aes(xmin = LowerTainter.Opened, xmax = LowerTainter.Closed, ymin = -Inf, ymax = Inf),
              fill = "grey30", alpha = 0.12, inherit.aes = FALSE) +
    labs(title = paste("Transmitter:", Animal.Common.Name, id)) +
    theme_bw()
  
  print(p)
}



###############################################################################


#####  Lock Receivers Timing with ISH #####
ids <- unique(DF$transmitter_id)
seasons <- unique(DF$Season)
save_folder <- "I:/Shared drives/IFI/Projects_Active/SanteeCooper_FERC_19-04-09/IFI_TASKS/Task_Y_SturgUseofLock/Data/Figures_Working/2025 - 2026 Report/"
counter <- 1

ll_rect <- ll_final %>%  
  mutate(date = as.Date(LowerTainter.Opened),
         xmin = hour(LowerTainter.Opened) + minute(LowerTainter.Opened)/60,
         xmax = hour(LowerTainter.Closed) + minute(LowerTainter.Closed)/60) %>%
  dplyr::select(date, xmin, xmax)


for (season in seasons) {
  for (id in ids) {
    df_pts <- DF %>%
      filter(AFS %in% c("Shortnose Sturgeon", "Atlantic Sturgeon"),
             transmitter_id == id,
             Season == season) %>%
      mutate(
        hour_of_day = lubridate::hour(detection_timestamp_est) +
          lubridate::minute(detection_timestamp_est)/60,
        receiver_location2 = factor(receiver_location2,
                                    levels = c("Lock Receivers",
                                               "Lock Entrance Channel",
                                               "RKM 77",
                                               "RKM 76",
                                               "RKM 73")))
    
    
    if (nrow(df_pts) == 0) next
    
    species_name <- unique(df_hourly$AFS)
    year_val <- unique(lubridate::year(df_hourly$date))
    
    # filter lock rectangles to dates for this fish
    ll_rect_sub <- ll_rect %>% filter(date %in% unique(df_hourly$date))
    
    p <- ggplot(df_pts, aes(x = hour_of_day, y = forcats::fct_rev(receiver_location2), color = receiver_location2)) +
      geom_point(alpha = 0.5, size = 2, position = position_jitter(height = 0.15 )) +
      geom_rect(data = ll_rect_sub,
                aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
                fill = "grey30", alpha = 0.12, inherit.aes = FALSE) +
      facet_wrap(~date) +
      labs(
        title = paste(season, year_val, ": Detections for Transmitter", id, "-", species_name),
        x = "Hour of Day",
        y = "Receiver Location",
        color = "Receiver Location"
      ) +
      theme_bw() +
      theme(panel.grid = element_blank()) +
      scale_color_manual(values=c( "#030303","#00dd00", "#00aaff","#9d4eec", "orange"),
                         breaks=c('Lock Receivers', 'RKM 77', 'RKM 76', 'RKM 73', 'Lock Entrance Channel')) 
    
    
    n_facets <- length(unique(df_hourly$date))
    plot_width  <- min(50, max(8, n_facets * 1)) 
    plot_height <- min(50, max(6, n_facets * 0.65))  
    
    # scale font size with plot dimensions (try tweaking the factor)
    base_font <- 10
    scale_factor <- (plot_width / 10)  # proportional to width
    font_size <- base_font * scale_factor
    
    p <- p +
      theme_bw(base_size = font_size) +   # global font scaling
      theme(
        panel.grid = element_blank(),
        strip.text = element_text(size = font_size * 0.8), # facet labels
        axis.text  = element_text(size = font_size * 0.6),
        axis.title = element_text(size = font_size * 0.8),
        legend.text = element_text(size = font_size * 0.6),
        legend.title = element_text(size = font_size * 0.8),
        plot.title  = element_text(size = font_size)
      )
    
    print(p)
    file_name <- paste0(save_folder, "Abacus", counter, "_detections_with_locks_", season, "_", year_val, "_", id, "_", gsub(" ", "_", species_name), ".jpeg")
    ggsave(filename = file_name, plot = p, width = plot_width, height = plot_height, units = "in", dpi = 300, limitsize = FALSE)
    counter <- counter + 1 
  }
}
