#install.packages("devtools")
devtools::install_github("plant-functional-trait-course/fluxible")

library(devtools)
library(fluxible)
library(tidyverse)
library(readxl)
library(dplyr)
library(writexl)
library(readr)
library(fluxible)
library(hms)
library(forcats)
library(ggplot2)
library(ggforce)
library(grid)
library(lme4)
library(lmerTest)
library(licoread)
library(purrr)

#naming where to find files locally on personal computer
file_path <- "C:/Users/Elias/Documents/master/koding/raw_data_licor"

#here importing those with ly in the name, aka just Lygra site measurements
all_files <- list.files(path = file_path,
                        pattern = "ly",
                        full.names = TRUE)

#combininng all of the measurements and adding a coloumn with the filename,
#this will be the unique ID for each measurement

combined_data <- map_dfr(all_files, function(file) {
  df <- read_table(file,
                   skip = 7,
                   col_names = FALSE,
                   col_types = cols(.default = col_character()))
  df$filename <- basename(file)
  df
})

# safer approach: drop unnamed columns
combined_data <- combined_data[, !is.na(colnames(combined_data))]
#renaming coloumns
colnames(combined_data) <- c(
                              "tag",                    
                              "seconds",                
                              "nanoseconds",            
                              "ndx",                    
                              "diag",                   
                              "remark",                 
                              "date",                   
                              "time",                   
                              "h2o_ppm",                
                              "co2_ppm",                
                              "ch4_ppb",                
                              "cavity_pressure_kPa",    
                              "cavity_temp_C",          
                              "laser_phase_p_kPa",      
                              "laser_temp_C",           
                              "residual",               
                              "ring_down_us",           
                              "thermal_enclosure_C",    
                              "phase_error",            
                              "laser_t_shift_C",        
                              "input_voltage_V",        
                              "chk",                    
                              "filename"              
                              )

#making a copy of the filename coloumn to later split one of them to extract the
#metadata info in the name
combined_data <- combined_data %>%
  mutate(plot_info = filename)
head(combined_data)
str(combined_data)
colnames(combined_data)
sample_n(combined_data, 10)

#cleaning up and naming the new coloumns with the matching plot meta data
#for each measurement
combined_data <- combined_data %>%
  separate(
    col= plot_info,
    into = c("site",
             "habitat",
             "species",
             "measuremen"),
    sep= "_",
    fill = "right",
    extra = "merge"
  ) %>%
  mutate(treatment=replace_na(treatment, "r"))

combined_data <- combined_data %>%
  mutate(uniqueID = paste(site,
                          habitat,
                          species,
                          replicat,
                          measurement,
                          treatment,
                          sep = "_"))%>%
  mutate(plotID = paste(site,
                          habitat,
                          species,
                          replicat,
                          treatment,
                          sep = "_"))


#import raw meta data locally, change for other computers
metadata_flux_measurements <- read_excel("C:/Users/Elias/Documents/master/koding/raw_data_licor/DURIN_4corners_diurnal_field_cflux_raw_2025.xlsx")

#slight clean-up as the time coloumns came with a weird date.
metadata_flux_measurements <- metadata_flux_measurements %>%
  separate(
    col = startTime,
    into = c("wrongdate",
             "start_time"),
    sep= " ",
    fill = "right",
    extra = "merge"
  ) %>%
  separate(
    col = stopTime,
    into = c("wrongdate",
             "stop_time"),
    sep= " ",
    fill = "right",
    extra = "merge"
  ) %>%
  select(-"wrongdate")

metadata_flux_measurements <- metadata_flux_measurements %>%
  mutate(site = recode(site,
                       "lygra" ="ly",
                       "sogndal" ="so"))%>%
  mutate(habitat = recode(habitat,
                          "forest" ="f",
                          "open" ="o"))%>%
  mutate(treatment = recode(treatment,
                            "removal" ="r",
                            "control" ="c"))%>%
  mutate(uniqueID = paste(site,
                          habitat,
                          plot,
                          replicate,
                          cover,
                          treatment,
                          sep = "_"))%>%
  mutate(start = paste(date,
                       start_time,
                          sep = " "))%>%
  mutate(stop = paste(date,
                       stop_time,
                       sep = " "))

metadata_flux_measurements$start <- as.POSIXct(metadata_flux_measurements$start,
                                                format = "%Y-%m-%d %H:%M:%S")
metadata_flux_measurements$stop <- as.POSIXct(metadata_flux_measurements$stop,
                                               format = "%Y-%m-%d %H:%M:%S")


#combine measurements

raw_combined_meta_logger <- combined_data %>%
  left_join(metadata_flux_measurements,
            by = "uniqueID",
            relationship = "many-to-many")

raw_combined_meta_logger <- raw_combined_meta_logger %>%
  mutate(datetime = start + relative_time)



columns_logger <- c("datetime", "conc", "relative_time", "temp", "pressure")

logger.df <- raw_combined_meta_logger[, columns_logger]
meta.df <- raw_combined_meta_logger[, !names(raw_combined_meta_logger) %in% columns_logger]
meta.df <- meta.df[, !names(meta.df) %in% c("aux_input", "co2_absorptance", "co2mmol/m3", "co2mg/m3",
                                            "h2o_absorptance", "h2ommol/m3", "h2og/m3",
                                            "h2ommol/mol", "h2o(celsius)", "cooler_voltage"
                                            )]

meta.df <- meta.df %>% distinct()




######################################


conc_lygra <- flux_match(
  raw_conc = logger.df,
  field_record = meta.df,
  f_datetime = datetime,
  start_col = start,
  measurement_length = 180,
  fixed_length = TRUE,
  time_diff = 0
)



slopes_lygra <- flux_fitting(
  conc_df = conc_lygra,
  f_conc = conc,
  f_datetime = datetime,
  f_start = f_start,
  f_end = f_end,
  f_fluxid = f_fluxid,
  fit_type = "exp_zhao18",
  start_cut = 30,
  end_cut = 60
)



flags_lygra <- flux_quality(
  slopes_df = slopes_lygra,
  f_conc = conc,
  ambient_conc = 445,
  error = 100,
  instr_error = 5
)

flags_lygra |>
  flux_plot(
    f_conc = conc,
    f_datetime = datetime,
    print_plot = FALSE,
    output = "pdfpages",
    f_plotname = "plots_lygra_evaluation_2",
    f_ylim_upper = 600,
    f_ylim_lower = 400
  )


fluxes_lygra <- flux_calc(
  slopes_df = flags_lygra,
  slope_col = f_slope_corr,
  f_datetime = datetime,
  temp_air_col = temp,
  conc_unit = "ppm",
  flux_unit = "mmol",
  temp_air_unit = "celsius",
  atm_pressure = 1,
  setup_volume = 250,
  plot_area = 0.25,
  cols_keep = c("filename",
                "site.x",
                "habitat.x",
                "species",
                "replicate",
                "measurement",
                "treatment.x",
                "plotID",
                "PAR1",
                "PAR2",
                "PAR3",
                "NDVI1",
                "NDVI2",
                "soilmoist1",
                "soilmoist2",
                "soilmoist3")
)

fluxes_lygra <- fluxes_lygra %>%
  rename(type=measurement
         )

fluxes_lygra <- fluxes_lygra |>
  mutate(
    f_fluxid = as.integer(f_fluxid),
    pairID = case_when(
      type=="nee" ~ f_fluxid,
      type=="reco" ~ f_fluxid -1
    ),
    f_fluxid = as_factor(f_fluxid),
    pairID = as_factor(pairID)
  )

gpp_lygra <- flux_gpp(
  fluxes_df = fluxes_lygra,
  type_col = type,
  f_datetime = datetime,
  id_cols = c("pairID","plotID"),
  cols_keep = "all",
  nee_arg = "nee",
  er_arg = "reco"
)



plot_gpp <- gpp_lygra %>%
  filter(type == "GPP") %>%
  filter(treatment.x == "c")%>%
  ggplot(aes(x = habitat.x, y = f_flux)) +
  geom_boxplot() +
  facet_wrap(~species)
plot_gpp









#########################################################################


#Sogndal


#here importing those with so in the name, aka just Sogndal site measurements
all_files_so <- list.files(path = file_path,
                        pattern = "so_",
                        full.names = TRUE)

#combininng all of the measurements and adding a coloumn with the filename,
#this will be the unique ID for each measurement
combined_data_so <- lapply(all_files_so,
                        function(all_files_so) {
                          df <- read_table(all_files_so,
                                           skip = 9,
                                           col_names = FALSE,
                                           col_types = cols(.default = col_character()))
                          df$filename <-basename(all_files_so)
                          df
                        }) %>% bind_rows()


#renaming coloumns
colnames(combined_data_so) <- c("relative_time",
                             "temp",
                             "pressure",
                             "aux_input",
                             "co2_absorptance",
                             "co2mmol/m3",
                             "co2mg/m3",
                             "conc",
                             "h2o_absorptance",
                             "h2ommol/m3",
                             "h2og/m3",
                             "h2ommol/mol",
                             "h2o(celsius)",
                             "cooler_voltage",
                             "filename")

combined_data_so <- combined_data_so %>%
  mutate(relative_time = as.numeric(relative_time))%>%
  mutate(conc = as.numeric(conc))%>%
  mutate(temp = as.numeric(temp))

#making a copy of the filename coloumn to later split one of them to extract the
#metadata info in the name
combined_data_so <- combined_data_so %>%
  mutate(plot_info = filename)

#cleaning up and naming the new coloumns with the matching plot meta data
#for each measurement
combined_data_so <- combined_data_so %>%
  separate(
    col= plot_info,
    into = c("site",
             "habitat",
             "species",
             "replicat",
             "measurement",
             "treatment"),
    sep= "_",
    fill = "right",
    extra = "merge"
  ) %>%
  mutate(treatment=replace_na(treatment, "r"))

combined_data_so <- combined_data_so %>%
  mutate(uniqueID = paste(site,
                          habitat,
                          species,
                          replicat,
                          measurement,
                          treatment,
                          sep = "_"))%>%
  mutate(plotID = paste(site,
                        habitat,
                        species,
                        replicat,
                        treatment,
                        sep = "_"))


#combine measurements

raw_combined_meta_logger_so <- combined_data_so %>%
  left_join(metadata_flux_measurements,
            by = "uniqueID",
            relationship = "many-to-many")

raw_combined_meta_logger_so <- raw_combined_meta_logger_so %>%
  mutate(datetime = start + relative_time)



columns_logger <- c("datetime", "conc", "relative_time", "temp", "pressure")

logger_so.df <- raw_combined_meta_logger_so[, columns_logger]
meta.df <- raw_combined_meta_logger_so[, !names(raw_combined_meta_logger_so) %in% columns_logger]
meta.df <- meta.df[, !names(meta.df) %in% c("aux_input", "co2_absorptance", "co2mmol/m3", "co2mg/m3",
                                            "h2o_absorptance", "h2ommol/m3", "h2og/m3",
                                            "h2ommol/mol", "h2o(celsius)", "cooler_voltage"
)]

meta.df <- meta.df %>% distinct()




######################################


conc_sogndal <- flux_match(
  raw_conc = logger_so.df,
  field_record = meta.df,
  f_datetime = datetime,
  start_col = start,
  measurement_length = 180,
  fixed_length = TRUE,
  time_diff = 0
)



slopes_sogndal <- flux_fitting(
  conc_df = conc_sogndal,
  f_conc = conc,
  f_datetime = datetime,
  f_start = f_start,
  f_end = f_end,
  f_fluxid = f_fluxid,
  fit_type = "exp_zhao18",
  start_cut = 30,
  end_cut = 60
)



flags_sogndal <- flux_quality(
  slopes_df = slopes_sogndal,
  f_conc = conc,
  ambient_conc = 445,
  error = 100,
  instr_error = 5
)

flags_sogndal |>
  flux_plot(
    f_conc = conc,
    f_datetime = datetime,
    f_ylim_upper = 600,
    f_ylim_lower = 400
  )

flags_sogndal |>
  flux_plot(
    f_conc = conc,
    f_datetime = datetime,
    print_plot = FALSE,
    output = "pdfpages",
    f_plotname = "plots_sogndal_evaluation_2",
    f_ylim_upper = 600,
    f_ylim_lower = 400
  )

fluxes_sogndal <- flux_calc(
  slopes_df = flags_sogndal,
  slope_col = f_slope_corr,
  f_datetime = datetime,
  temp_air_col = temp,
  conc_unit = "ppm",
  flux_unit = "mmol",
  temp_air_unit = "celsius",
  atm_pressure = 1,
  setup_volume = 250,
  plot_area = 0.25,
  cols_keep = c("filename",
                "site.x",
                "habitat.x",
                "species",
                "replicate",
                "measurement",
                "treatment.x",
                "plotID",
                "PAR1",
                "PAR2",
                "PAR3",
                "NDVI1",
                "NDVI2",
                "soilmoist1",
                "soilmoist2",
                "soilmoist3")
)

fluxes_sogndal <- fluxes_sogndal %>%
  rename(type=measurement
  )

fluxes_sogndal <- fluxes_sogndal |>
  mutate(
    f_fluxid = as.integer(f_fluxid),
    pairID = case_when(
      type=="nee" ~ f_fluxid,
      type=="reco" ~ f_fluxid -1
    ),
    f_fluxid = as_factor(f_fluxid),
    pairID = as_factor(pairID)
  )

gpp_sogndal <- flux_gpp(
  fluxes_df = fluxes_sogndal,
  type_col = type,
  f_datetime = datetime,
  id_cols = c("pairID","plotID"),
  cols_keep = "all",
  nee_arg = "nee",
  er_arg = "reco"
)



plot_gpp <- gpp_sogndal %>%
  filter(type == "GPP") %>%
 filter(treatment.x == "c")%>%
  ggplot(aes(x = habitat.x, y = f_flux, colour=treatment.x)) +
  geom_violin()+
  facet_wrap(~species)
plot_gpp



###############################

#combined

south_spring_gpp <- bind_rows(gpp_lygra, gpp_sogndal)
south_spring_gpp <- south_spring_gpp %>%
  filter(type == "GPP") %>%
  filter(f_flux <= 0) %>%
  filter(f_flux >= -60)

south_spring_gpp$PAR1 <- as.numeric(south_spring_gpp$PAR1)
south_spring_gpp$PAR2 <- as.numeric(south_spring_gpp$PAR2)
south_spring_gpp$PAR3 <- as.numeric(south_spring_gpp$PAR3)
south_spring_gpp$NDVI <- (south_spring_gpp$NDVI1+south_spring_gpp$NDVI2)/2
south_spring_gpp$soilmoisture <- (south_spring_gpp$soilmoist1 + south_spring_gpp$soilmoist2 + south_spring_gpp$soilmoist3)/3
south_spring_gpp$PAR <- (south_spring_gpp$PAR1 + south_spring_gpp$PAR2 + south_spring_gpp$PAR3)/3

plot.ndvi <- south_spring_gpp %>%
  drop_na(NDVI) %>%
  filter(treatment.x == "c")%>%
  ggplot(aes(x=NDVI, y=f_flux, colour=habitat.x))+
  geom_jitter()+
  geom_smooth(method = "lm")+
  facet_wrap(~site.x)

plot.ndvi

plot.temp <- south_spring_gpp %>%
  filter(treatment.x == "c")%>%
  ggplot(aes(x=f_temp_air_ave, y=f_flux, colour=habitat.x))+
  geom_jitter()+
  geom_smooth(method = "lm")+
  facet_wrap(~site.x)

plot.temp
site_names <- c('ly' = "Lygra (coastal)",
                   'so' = "Sogndal (continental)")
habitats <- c('f' = "Forest",
              'o' = "Open")


plot.soilmoist <- south_spring_gpp %>%
  drop_na(soilmoisture) %>%
  filter(treatment.x == "c")%>%
  ggplot(aes(x=soilmoisture,
             y=f_flux,
             colour=habitat.x,
             fill=habitat.x))+
  geom_jitter()+
  geom_smooth(method = "lm")+
  facet_wrap(~site.x, labeller = as_labeller(site_names)) +
  scale_fill_manual(values = c("f" ="#854836", #Forest
                               "o" ="#FFB22C" #Open
                               ),labels= habitats)+
  scale_colour_manual(values = c("f" ="#854836", #Forest
                                 "o" ="#FFB22C" #Open
  ),labels=habitats)+

  theme_bw()+
  theme(
    strip.text = element_text(size = 12),
    strip.background = element_blank()
  ) +
  labs(x= "Soil moisture (%)",
       y="GPP",
       color= "",
       fill="")

plot.soilmoist

ggsave("gpp_soilmoist_site_habitat.png",
       width = 7,
       height = 6,
       dpi = 300)

plot.PAR <- south_spring_gpp %>%
  drop_na(PAR) %>%
  filter(treatment.x == "c")%>%
  ggplot(aes(x=PAR, y=f_flux, colour=habitat.x))+
  geom_jitter()+
  geom_smooth(method = "lm")+
  facet_wrap(~site.x)

plot.PAR

plot_gpp_site_habitat <- south_spring_gpp %>%
 # filter(type == "GPP") %>%
  filter(treatment.x == "c")%>%
  ggplot(aes(x = site.x,
             y = f_flux,
             colour=habitat.x)) +
  geom_sina(size = 3) +
  geom_violin(fill = NA)+
  stat_summary(
    fun = mean,
    geom = "crossbar",
    #  width = 1,
    color = "black",
    fatten = 1,
    position = position_dodge(width = 0.9),
    aes(group = interaction(habitat.x,
                            site.x))
  ) +
  scale_x_discrete(labels= c("Lygra (coastal)",
                             "Sogndal (continental)"))+
  scale_colour_manual(values = c("#854836", #Forest
                                   "#FFB22C" #Open
  ),labels= c("Forest",
              "Open"))+
  theme_bw()+
  theme(
    strip.text = element_text(size = 12),
    strip.background = element_blank()
  ) +
  labs(x= "",
       y="GPP",
       color= "")

plot_gpp_site_habitat

ggsave("plot_gpp_site_habitat.png",
       width = 7,
       height = 6,
       dpi = 300)

plot_gpp <- south_spring_gpp %>%
  # filter(type == "GPP") %>%
  filter(treatment.x == "c")%>%
  ggplot(aes(x = site.x, y = f_flux)) +
  geom_sina(size = 2) +
  geom_violin(fill = NA)+
  stat_summary(
    fun = mean,
    geom = "crossbar",
    #  width = 1,
    color = "black",
    fatten = 1,
    position = position_dodge(width = 0.9),
    aes(group = interaction(site.x))
  )#+
# facet_wrap(~species)
plot_gpp

plot_gpp <- south_spring_gpp %>%
  # filter(type == "GPP") %>%
  filter(treatment.x == "c")%>%
  ggplot(aes(x = habitat.x, y = f_flux, colour = species)) +
  geom_sina(size = 3) +
  geom_violin(fill = NA)+
  stat_summary(
    fun = mean,
    geom = "crossbar",
    #  width = 1,
    color = "grey",
    fatten = 1,
    position = position_dodge(width = 0.9),
    aes(group = interaction(habitat.x, species))
  )#+
# facet_wrap(~species)
plot_gpp

mod.lm <- lm(f_flux ~site.x*habitat.x, data = south_spring_gpp)
anova(mod.lm)

mod.lmer <- lmer(f_flux ~site.x*habitat.x + (1|species/replicate),
                 data = south_spring_gpp)

anova(mod.lmer)

mod2.lm <- lm(f_flux ~site.x, data = south_spring_gpp)
anova(mod2.lm)

mod2.lm <- lm(f_flux ~site.x *habitat.x*species, data = south_spring_gpp)
anova(mod2.lm)
