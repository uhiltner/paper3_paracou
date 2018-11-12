# Description: A script to handle the calibration of FORMIND's soil water module. 
# To do the entire calibration, only the main function calibrate_swc() must be called. 
# All other functions are called there. For detailed descriptions of functions, 
# please see the docoumentations below.
# Contact: Ulrike Hiltner (ulrike.hiltner@ufz.de)
# Date: 2018-11-08

# Documentation of function packages_needed():
# Function checks for missing packages and ... 
# ... loads/installs them automatically.
# It takes one argument: a character string holding the package names
packages_needed <- function(x){
  for(i in x){
    # require() returns TRUE invisibly if it was able to load package
    if(!require( i , character.only = TRUE) ){
      #  If package was not able to be loaded then install
      install.packages(i, dependencies = TRUE)
      #  Load package after installing. 
      library(i, character.only = TRUE ) # library() will throw an exception ...
      # ... if the install wasn't successful
    }
  }
} # end packages_needed()

# Documentation of function run_formind():
# This function executes the FORMIND model by calling the *.cmd-file.
# It takes two arguments:
# filePathCmd and fileNameCmd (chr): file path and file name of .cmd-file.
run_formind <- function(filePathCmd, fileNameCmd){
  runFormind = -1
  runFormind <- system(paste0(filePathCmd, fileNameCmd), 
                       show.output.on.console = F, 
                       ignore.stdout = T, ignore.stderr = T)
  if(runFormind == 0) {
    print("FORMIND is running... Wait a moment.")
  }
  if(runFormind != 0) {
    print("ERROR. Can't run FORMIND.")
    stop(runFormind)
  }
}
  
# Documentation of function import_water()
# This function loads a single *.water-file (tab seperated text) from a working directory keeping simulation
# results after a minimum time. 
# It takes three arguments:
# filePathSim and fileNameSim (chr): working directory and *.water-file name.
# equilibriumTime (int): filter for minimum Time in *.water by default = 1.
import_water <- function(filePathSim, fileNameSim, equilibriumTime = 1) {
  # assign working directory (relative path)
  fileNameList <- paste0(filePathSim, fileNameSim)
  
  # load packages needed
  packagesUsed <- packagesUsed <- c("readr", "dplyr") # packagees needed
  packages_needed(packagesUsed)
  
  # import data
  data <- read_tsv(fileNameList, skip = 2, col_names = TRUE)
  data <- filter(data, Time > equilibriumTime)
  
  return(data)
} # end import_water()

# Documentation of function show_water()
# This function visualizes the values of different model outputs as per water column [mm/a]
# versus the reduction factor for water (RW [-]) as graphic device inside R. 
# Model outputs that can be visualized are:
# annual and mean soil water contents (SWC , mnSWC), potential wilting point (PWP),
# minimum soil water (MSW), field capacity (FC), transpiration (TRSP), and
# potential evapotranspiration (PET).
# If desired it also saves the graphic device outside R. 
# It takes 9 arguments:
# df1 and df2 (tibble): takes up to two data sets of FORMIND's *.water-file.
# fileNameSim (chr): *.water-file name.
# FC (num): value of field capacity [vol-%] (out of FORMIND's *.par-file).
# xmax (num): maximum extention of x-axis in graphic device. The default is 1800 mm/a.
# saveAs (logical): switch on/off for save graphical device outside R (default saveAs = FALSE).
# figureName (chr): file name and type ("png", "pdf") regarding saving graphic device outside R.
#                   The defalult is "calibrate_SWC.png", which produces a png-file format.
# figWidth and figHeight (num): size of output graphic device, by default 10 cm by 10 cm.
# figUnit (chr): unit of figWidth and figHeight ("cm", "in", "mm"), by default "cm".
show_water <- function(df1, df2 = NULL, fileNameSim,
                       FC,
                       xmax = 1800,
                       saveAs = FALSE, 
                       figureName = "calibrate_SWC.png",
                       figWidth = 10, figHeight = 10, figUnit = "cm") {
  
  
  # load packages needed
  packagesUsed <- packagesUsed <- c("stringr", "ggplot2", "dplyr") # packagees needed
  packages_needed(packagesUsed)
  
  # Visualize simulation results
  figure <- ggplot() +
    theme(aspect.ratio=1) +
    theme_bw() +
    coord_cartesian(ylim=c(0, 1), xlim = c(0:xmax)) +
    ## plot simulation results of current run:
    # swc during climate change
    geom_point(data = df1, mapping = aes(x = Soil_Water, y = RW), fill = "white", alpha = 0.7, size = 2.5, shape = 21) +
    # mean swc of all years
    geom_vline(data = df1, mapping = aes(xintercept = mean(Soil_Water)), color = 1, size = 1.5) +
    # PWP
    geom_vline(data = df1, mapping = aes(xintercept = mean(PWP)), color = 2, size = 1.0) +
    # MSW
    geom_vline(data = df1, mapping = aes(xintercept = mean(MSW)), color = 3, size = 1.0) +
    # Transpiration
    geom_vline(data = df1, mapping = aes(xintercept = mean(Transpiration)), color = 4, size = 1.0) +
    # FC
    geom_vline(data = df1, mapping = aes(xintercept = FC*10*2.5), color = 5, size = 1.0) +
    # PET
    geom_vline(data = df1, mapping = aes(xintercept = mean(PET)), color = 6, size = 1.0) +
    ## plot simulation results of previous run:
    # mean swc of all years
    geom_vline(data = df2, mapping = aes(xintercept = mean(Soil_Water)), color = 1, size = 1.0, alpha = 0.5 , linetype = 2) +
    # PWP
    geom_vline(data = df2, mapping = aes(xintercept = mean(PWP)), color = 2, size = 1.0, alpha = 0.5 , linetype = 2) +
    # MSW
    geom_vline(data = df2, mapping = aes(xintercept = mean(MSW)), color = 3, size = 1.0, alpha = 0.5 , linetype = 2) +
    # Transpiration
    geom_vline(data = df2, mapping = aes(xintercept = mean(Transpiration)), color = 4, size = 1.0, alpha = 0.5 , linetype = 2) +
    # FC
    geom_vline(data = df2, mapping = aes(xintercept = FC*10*2.5), color = 5, size = 1.0, alpha = 0.5 , linetype = 2) +
    # PET
    geom_vline(data = df2, mapping = aes(xintercept = mean(PET)), color = 6, size = 1.0, alpha = 0.5 , linetype = 2) +
    # legend and axis style
    scale_x_continuous(breaks = c(0, 200, 400, 600, 800, 1000, 1200, 1400, 1600, 1800)) +
    labs(x = "swc (mm/a)", y = "reduction factor for water RW (-)", 
         title = "Paramerter values of FORMIND's\nsoil water module",
         caption = paste0("scenario: ", str_sub(fileNameSim, end = -7))) +
    annotate(geom = "text", x = xmax-0.1*xmax, 
             y = 0.35,
             hjust = 0,
             vjust = 1,
             label = "o SWC",
             size = 3) +
    annotate(geom = "text", x = xmax-0.1*xmax, 
             y = c(.3, .25, .2, .15,.1, .05),
             hjust = 0,
             vjust = 1,
             color = c(1, 2, 3, 5, 4, 6),
             label = c("- MnSWC", "- PWP", "- MSW",  "- FC", "- TRSP", "- PET"), 
             size = 3)
  
  # Write graphic device----
  # print graphic device inside R 
  print(figure)
  # Save graphic device outside R 
  if(saveAs) ggplot2::ggsave(fileNameSim = figureName, plot = figure, 
                             width = figWidth, height = figHeight, units = figUnit)
} # end show_water()

# Main: calibrate_swc():
# This function calls run_formind(), import_water(), show_water().
# It takes 9 arguments:
# df1 and df2 (tibble): takes up to two data sets of FORMIND's *.water-file.
# filePathSim and fileNameSim (chr): working directory and *.water-file name.
# filePathCmd and fileNameCmd (chr): file path and file name of *.cmd-file.
# equilibriumTime (int): filter for minimum Time in *.water by default = 1.
# FC (num): value of field capacity [vol-%] (out of FORMIND's *.par-file).
# saveAs (logical): switch on/off for save graphical device outside R (default saveAs = FALSE).
# figureName (chr): file name and type ("png", "pdf") regarding saving graphic device outside R.
#                   The defalult is "calibrate_SWC.png", which produces a png-file format.
# figWidth and figHeight (num): size of output graphic device, by default 10 cm by 10 cm.
# figUnit (chr): unit of figWidth and figHeight ("cm", "in", "mm"), by default "cm".
# xmax (num): maximum extention of x-axis in graphic device. The default is 1800 mm/a.
calibrate_swc<- function(filePathSim, fileNameSim, 
                         filePathCmd, fileNameCmd,
                         equilibriumTime = 1, 
                         FC, 
                         saveAs = FALSE, 
                         figureName = "calibrate_SWC.png",
                         figWidth = 10, figHeight = 10, figUnit = "cm", 
                         xmax = 1800){
  
  # execute FORMIND 
  run_formind(filePathCmd, fileNameCmd)
  
  # import current data
  df1 <- import_water(filePathSim, fileNameSim, equilibriumTime)
  # import previous data if they exist
  if (file.exists(paste0(filePathSim , "temp_df1.txt"))){
    df2 <- read_tsv(paste0(filePathSim, fileNameSim = "temp_df1.txt"))
  } else{
    # dummy for df2: empty data frame only with headers
    df2 <- df1[NULL, ]
    # end if
  }
  
  # visualize data
  show_water(df1, df2, fileNameSim, FC, xmax, saveAs, figureName, figWidth , figHeight , figUnit)
  
  # save df1 temporary outside R
  write_tsv(df1, paste0(filePathSim, "temp_df1.txt"))
} # end calibrate_swc()


