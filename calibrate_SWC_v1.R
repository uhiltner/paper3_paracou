# Script to privide function calibrate_swc() and its documentation.
# Author: Ulrike Hiltner (ulrike.hiltner@ufz.de)
# Date: 2018-11-08

# Documtentation of function calibrate_swc()
# calibrate_swc() imports one *.water-file of FORMID's simulation results.
# It then visualizes the values of different model outputs as per water column [mm/a]
# versus the reduction factor for water (RW [-]) as graphic device inside R. 
# Model outputs that can be visualized are:
# annual and mean soil water contents (SWC , mnSWC), potential wilting point (PWP),
# minimum soil water (MSW), field capacity (FC), transpiration (TRSP), and
# potential evapotranspiration (PET).
# If desired it also saves the graphic device outside R. 
# It takes 10 arguments:
# filePath and fileName (chr): working directory and *.water-file name.
# equilibriumTime (int): filter for minimum Time in *.water by default = 1.
# FC (num): value of field capacity [vol-%] (out of FORMIND's *.par-file).
# saveAs (logical): switch on/off for save graphical device outside R.
# figureName (chr): file name and type ("png", "pdf") regarding saving graphic device outside R.
#                   The defalult is "calibrate_SWC.png", which produces a png-file format.
# figWidth and figHeight (num): size of output graphic device, by default 10 cm by 10 cm.
# figUnit (chr): unit of figWidth and figHeight ("cm", "in", "mm"), by default "cm".
# xmax (num): maximum extention of x-axis in graphic device. The default is 1800 mm/a.
#
# example: calibrate_swc(filePath = "C:/user/documents,   
#                       fileName = "^paracouForest_controlPlots_8pft.water",
#                       equilibriumTime = 500, 
#                       FC = 28.8)

calibrate_swc <- function(filePath, fileName, 
                          equilibriumTime = 1, 
                          FC, 
                          saveAs = FALSE, 
                          figureName = "calibrate_SWC.png",
                          figWidth = 10, figHeight = 10, figUnit = "cm", 
                          xmax = 1800) {
  #  Load/install packages----
  packagesUsed <- c("ggplot2", "readr", "dplyr") # enter packagees needed
  # check for missing packages and load/install them automatically.
  for(i in packagesUsed){
    # require() returns TRUE invisibly if it was able to load package
    if(!require( i , character.only = TRUE) ){
      #  If package was not able to be loaded then install
      install.packages(i, dependencies = TRUE)
      #  Load package after installing. 
      library(i, character.only = TRUE ) # library() will throw an exception if the install wasn't successful
    }
  }
  
  # import *.water data----
  wd <- getwd()
  pathSimRes <- paste0(wd, filePath)
  fileNameList <- dir(pathSimRes, pattern = fileName)
  
  data <- readr::read_tsv(paste0(pathSimRes, fileNameList[1]), skip = 2, col_names = TRUE)
  data <- dplyr::filter(data, Time > equilibriumTime)
  
  # Visualize simulation results----
  p <- ggplot() +
    theme(aspect.ratio=1) +
    theme_bw() +
    coord_cartesian(ylim=c(0, 1), xlim = c(0:xmax)) +
    # swc during climate change
    geom_point(data = subset(data, Time > equilibriumTime), mapping = aes(x = Soil_Water, y = RW), fill = "white", alpha = 0.7, size = 2.5, shape = 21) +
    # mean swc of all years
    geom_vline(data = subset(data, Time > equilibriumTime), mapping = aes(xintercept = mean(Soil_Water)), color = 1, size = 1.5) +
    # PWP
    geom_vline(data = subset(data, Time > equilibriumTime), mapping = aes(xintercept = mean(PWP)), color = 2, size = 1.0) +
    # MSW
    geom_vline(data = subset(data, Time > equilibriumTime), mapping = aes(xintercept = mean(MSW)), color = 3, size = 1.0) +
    # Transpiration
    geom_vline(data = subset(data, Time > equilibriumTime), mapping = aes(xintercept = mean(Transpiration)), color = 4, size = 1.0) +
    # FC
    geom_vline(data = subset(data, Time > equilibriumTime), mapping = aes(xintercept = FC*10*2.5), color = 5, size = 1.0) +
    # PET
    geom_vline(data = subset(data, Time > equilibriumTime), mapping = aes(xintercept = mean(PET)), color = 6, size = 1.0) +
    # legend and axis style
    scale_x_continuous(breaks = c(0, 200, 400, 600, 800, 1000, 1200, 1400, 1600, 1800)) +
    labs(x = "swc (mm/a)", y = "reduction factor for water RW (-)", 
         title = "Paramerter values of FORMIND's\nsoil water module",
         caption = paste0("scenario: ", stringr::str_sub(fileNameList, end = -7))) +
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
  print(p)
  # Save graphic device outside R 
  if(saveAs) ggplot2::ggsave(filename = figureName, plot = p, 
                             width = figWidth, height = figHeight, units = figUnit)
}


