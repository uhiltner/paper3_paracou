## autthor: ulrike.hiltner@ufz.de
## date: July 2018
## prerequisites: "ncdf" package, all *.nc files in same working directory, information about unit transformation of input to output desired.


### function make_ts() reads multiple *.nc files of the same RCP scenario and the same variable, extracts the variables, and makes a time line begining from the starting point.
# It takes seven arguments: 
# fnl: list of character strings holding the nc*filenames in the working directory; 
# varId: character vector of length 1 defining the type of climate parameter, which is specified in the filename of the nc-files (e.g. "co2mass"); 
# rcpId: character vector of length 1 defining the type of rcp szenario, which is specified in the filename of the nc-files (e.g. "rcp45"); 
# lat: latitude of study site (degree NS); 
# long: longitude of study site (degree E); 
# pelv: inter vector on length 1 is needed for extraction of co2 time series only! if "co2" then ENTER presure level 2 (92500 Pa): at ca. 100 m asl, else pelv = 0.
make_ts <- function(fnl, path, varId, rcpId, lat, lon, pelv = 0){
  
  # uniformize the argument's entries to lower letters (as given in *.nc-filenames)
  rcpType <- tolower(rcpId)
  varType <- tolower(varId)
  # decide which nc file should be read of the working directory
  # depending on rcp AND on climate variable
  cond1 <- fnl[grep(varType,fnl)]; cond2 <- fnl[grep(rcpType,fnl)]
  # create subset
  fnlSubs <- fnl[fnl %in% cond1 & fnl %in% cond2]
  # initialize empty list for time line
  timeLine_varType <- vector("numeric",0)
  
  # beginn for loop
  for (fn in fnlSubs) {
    # read nc files
    #ncFile <- open.ncdf(con=paste0(path,fn), write=FALSE, readunlim=FALSE)
    ncFile <- open.ncdf(con=paste0(path,fn), write=FALSE, readunlim=FALSE)
    
    # Extract all dimensions and other info for result list 
    nDim <- ncFile$ndims
    nVar <- ncFile$var[[varType]][[2]]
    fn <- ncFile$filename
    ncTime_unit <- gsub(pattern= "days",replacement = "time",x=ncFile$dim["time"]$time$units) 
    # initialize result list
    timeLine_list <- vector("list", nDim)
    # loop over dim and extract values of variables 
    timeLine_list <- lapply(1:nDim, function(i) get.var.ncdf(nc = ncFile, 
                                                             varid = ncFile$dim[[i]]$name))
    # name lists
    names(timeLine_list) <- lapply(1:nDim, function(i) ncFile$dim[[i]][[1]])
    ## now get values of climate variable:
    # Get the lower boundary of the the grid cell in which our point of interest lies
    bndLow <- max(timeLine_list$lat[timeLine_list$lat < lat])
    # Get the left boundary of the the grid cell in which our point of interest lies
    bndLeft <- max(timeLine_list$lon[timeLine_list$lon < lon])
    # Get the corresponding indices in the respective arrays
    bndLowIdx <- which(bndLow==timeLine_list$lat)
    bndLeftIdx <- which(bndLeft==timeLine_list$lon)
    # Extract the climate variable values of each month in the entire netCDF file contained in the file at one location
    # decision between 2 cases depending on if pressure level a dim in ncFile or not.
    if(pelv != 0) {
      timeLine_list$var <- get.var.ncdf(nc = ncFile, 
                                        varid = varType, 
                                        start = c(bndLeftIdx, bndLowIdx, pelv, 1), # Extract the co2 values contained in the file at one location and pressure level (remember dimensions [lon, lat, plev, time]).
                                        count=c(1, 1, 1, -1))
    } else {
      timeLine_list$var <- get.var.ncdf(nc = ncFile, 
                                        varid = varType, 
                                        start = c(bndLeftIdx, bndLowIdx, 1), 
                                        count=c(1, 1, -1))
    } ## end if-else
    
    # Store the results in the long time series vector
    timeLine_varType <- c(timeLine_varType, timeLine_list$var)
    
  } ## end for-loop
  
  # make calendar since time of origin:
  # time steps fter time of origin
  timeSteps <- 1:length(timeLine_varType)
  timeLine_varType <- data.frame(varType = timeLine_varType, time = timeSteps)
  names(timeLine_varType) <- c(nVar,ncTime_unit)
  
  # function output
  return(timeLine_varType)
  
}
