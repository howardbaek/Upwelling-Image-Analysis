imgVectortoRaster <- function(centers, datalist){
  
  # n_K = number of rows in centers
  n_K <- nrow(centers)
  
  # Get out dirty data
  Data_dirty <- datalist$dat
  # Latitude of dirty data
  lats <- Data_dirty[1,]
  # Longitude of dirty data
  lons <- Data_dirty[2,]
  # Aspect of dirty data
  asp <- c(table(lons)[1], table(lats)[1]) # lon (x) - lats (y)
  # bbox is a bounding box
  bb_box <- c(min(lons), max(lons), min(lats), max(lats))
  
  # Populate non-NA columns (indicated by pos_loc) of centroid_images with centers
  centroid_images <- matrix(NA, n_K, ncol(Data_dirty))
  centroid_images[, datalist$pos_loc] <- centers
  rownames(centroid_images) <- paste("Centroid", 1:n_K)
  
  # Empty list 
  img_list <- list()
  # Assign img_mat
  img_mat <- centroid_images
  for(i in 1:nrow(img_mat)){
    tmp <- matrix(img_mat[i,], asp[2], asp[1], byrow=TRUE)
    tmp <- tmp[asp[2]:1, ] # lat 7 at bottom not top
    tmp <- raster::raster(tmp)
    raster::extent(tmp) <- bb_box
    img_list[[i]] <- tmp
  }
  # Create a RasterStack object
  img_stack <- raster::stack(img_list)
  names(img_stack) <- paste("Centroid", 1:n_K)
  # crs is a coordinate reference system
  raster::crs(img_stack) <- "+proj=longlat +datum=WGS84"
  
  return(list(list=img_list, stack=img_stack))
  
  
}
