#' Kernel density raster
#' @description Creates a raster from kernel density 
#' @param xmn Minimum x coordinate for output grid
#' @param xmx Maximum x coordinate for output grid
#' @param ymn Minimum y coordinate for output grid
#' @param ymx Maximum y coordinate for output grid
#' @param dxdy Resolution for output grid
#' @param pts SpatialPointsDataFrame showing distribution of observations 
#' @return Raster of density values
#' @export
kdensity <- function(xmn, xmx, ymn, ymx, dxdy, pts) {
  
  if(length(dxdy) == 1) dxdy <- rep(dxdy, 2)
  adj <- dxdy / 2
  
  d1 <- length(seq(xmn[1], xmx[2], dxdy[1]))
  d2 <- length(seq(ymn[1], ymx[2], dxdy[2]))
  
  # grid topology
  grd <- GridTopology(cellcentre.offset = c(xmn + adj[1], ymn + adj[2]), 
                      cellsize = c(dxdy[1], dxdy[2]), 
                      cells.dim = c(d1, d2))
  
  # container raster
  rst <- raster(xmn = xmn, xmx = xmx, ymn = ymn, ymx = ymx)
  res(rst) <- dxdy
  
  # kernel densities
  ppoly <- as(extent(rst), "SpatialPolygons")
  crds <- slot(slot(slot(ppoly, "polygons")[[1]], "Polygons")[[1]], "coords") 
  kern <- spkernel2d(resdat, crds, h0 = 1, grd)
  kgrid <- as(SpatialGridDataFrame(grd, data = data.frame(kern)),
              "SpatialPixelsDataFrame")

  krast <- raster(kgrid)
  return(krast)
}



