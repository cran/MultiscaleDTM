% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SurfaceArea.R
\name{SurfaceArea}
\alias{SurfaceArea}
\title{Calculates surface area of a DTM}
\usage{
SurfaceArea(r, filename = NULL, overwrite = FALSE, wopt = list())
}
\arguments{
\item{r}{DTM as a SpatRaster or RasterLayer in a projected coordinate system where map units match elevation/depth units}

\item{filename}{character Output filename.}

\item{overwrite}{logical. If TRUE, filename is overwritten (default is FALSE).}

\item{wopt}{list with named options for writing files as in writeRaster}
}
\value{
a SpatRaster or RasterLayer
}
\description{
Calculates surface area on a per cell basis of a DTM based on Jenness, 2004.
}
\examples{
r<- rast(volcano, extent= ext(2667400, 2667400 + 
ncol(volcano)*10, 6478700, 6478700 + nrow(volcano)*10), 
crs = "EPSG:27200")
sa<- SurfaceArea(r)
plot(sa)
}
\references{
Jenness, J.S., 2004. Calculating landscape surface area from digital elevation models. Wildlife Society Bulletin 32, 829-839.
}
