
source("calibrate_SWC_v2.r")

calibrate_swc(filePathSim = paste0(getwd(), "/results/"), 
              fileNameSim = "paracouForest_controlPlots_8pft.water", 
              filePathCmd = paste0(getwd(), "/"), 
              fileNameCmd = "run_formind.cmd", 
              equilibriumTime = 500, 
              FC = 28.8, 
              saveAs = TRUE
              )
