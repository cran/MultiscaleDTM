#' Calculates Topographic Position Index
#'
#' Calculates Topographic Position Index (TPI). This is the value of the focal pixel minus the mean of the surrounding pixels (i.e. local mean but excluding the value of the focal pixel).
#' @param r DTM as a SpatRaster or RasterLayer
#' @param w A vector of length 2 specifying the dimensions of the rectangular window to use where the first number is the number of rows and the second number is the number of columns. Window size must be an odd number. Default is 3x3.
#' @param na.rm A logical indicating whether or not to remove NA values before calculations
#' @param include_scale logical indicating whether to append window size to the layer names (default = FALSE)
#' @param filename character Output filename.
#' @param overwrite logical. If TRUE, filename is overwritten (default is FALSE).
#' @param wopt list with named options for writing files as in writeRaster
#' @return a SpatRaster or RasterLayer
#' @examples 
#' r<- rast(volcano, extent= ext(2667400, 2667400 + 
#' ncol(volcano)*10, 6478700, 6478700 + nrow(volcano)*10), 
#' crs = "EPSG:27200")
#' tpi<- TPI(r, w=c(5,5), na.rm = TRUE)
#' plot(tpi)
#' @import terra
#' @importFrom raster raster
#' @importFrom raster writeRaster
#' @references 
#' Weiss, A., 2001. Topographic Position and Landforms Analysis. Presented at the ESRI user conference, San Diego, CA.
#' @export
#' 

TPI<- function(r, w=c(3,3), na.rm=FALSE, include_scale=FALSE, filename=NULL, overwrite=FALSE, wopt=list()){
  og_class<- class(r)[1]
  if(og_class=="RasterLayer"){
    r<- terra::rast(r) #Convert to SpatRaster
  }
  #Input checks
  if(!(og_class %in% c("RasterLayer", "SpatRaster"))){
    stop("Error: Input must be a 'SpatRaster' or 'RasterLayer'")
  }
  if(terra::nlyr(r)!=1){
    stop("Error: Input raster must be one layer.")
  }
  if(length(w)==1){w<- rep(w, 2)}
  if(length(w) > 2){
    stop("Specified window exceeds 2 dimensions")}
  if(any(0 == (w %% 2))){
    stop("Error: w must be odd")}
  if(all(w<3)){
    stop("Error: w must be greater or equal to 3 in at least one dimension")
  }
  
  w_mat <- matrix(1, nrow=w[1], ncol=w[2])
  center_idx<- ceiling(0.5 * length(w_mat))
  w_mat[center_idx] <- NA_real_
  tpi<- r - terra::focal(x = r, w = w_mat, fun = mean, na.rm = na.rm, wopt=wopt)
  names(tpi)<- "tpi"
  if(include_scale){names(tpi)<- paste0(names(tpi), "_", w[1],"x", w[2])} #Add scale to layer names
  
  #Return
  if(og_class =="RasterLayer"){
    tpi<- raster::raster(tpi)
    if(!is.null(filename)){
      return(raster::writeRaster(tpi, filename=filename, overwrite=overwrite))
    }
  }
  if(!is.null(filename)){
    return(terra::writeRaster(tpi, filename=filename, overwrite=overwrite, wopt=wopt))
  }
  return(tpi)
}