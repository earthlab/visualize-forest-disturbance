### Disturbance stack
### Create_sum_rasters
### Matt Bitters
### matthew.bitters@colorado.edu
### 4/14/2025

library(terra)

# Use gcmd in terminal to move disturbance stack data (~7 GB) into temporary instance.
# Read in data using relative path
data <- rast("/home/jovyan/data-store/visualize-forest-disturbance/forest-disturbance-stack_western-conus.tif")

# Data checks
print(head(values(data[[1]]), 1000))


# Efficient layer-by-layer reclassification function
reclassify_layer_by_layer <- function(raster_stack, reclass_matrix, layer_indices, 
                                      output_dir, output_filename) {
  dir.create(output_dir, showWarnings = FALSE)
  reclass_files <- c()
  
  # Convert reclassification matrix
  rclmat <- matrix(reclass_matrix, ncol = 2, byrow = TRUE)
  
  # Process each layer individually to avoid memory overload
  for (i in seq_along(layer_indices)) {
    lyr_index <- layer_indices[i]
    lyr <- raster_stack[[lyr_index]]
    
    # Check if the layer is empty (i.e., all NA or invalid values)
    if (all(is.na(lyr))) {
      warning(paste("Skipping empty layer", lyr_index))
      next  # Skip this iteration and move to the next layer
    }
    
    # Define output filename for reclassified layer
    out_file <- file.path(output_dir, paste0("reclass_", i, ".tif"))
    
    # Apply classification
    tryCatch({
      rc <- classify(lyr, rclmat, filename = out_file, 
                     overwrite = TRUE, datatype = "INT1U", NAflag = 255)
      reclass_files <- c(reclass_files, out_file)
      message("Reclassified layer ", lyr_index, " → ", out_file)
    }, error = function(e) {
      warning(paste("Error during classification of layer", lyr_index, ":", e$message))
    })
  }
  
  # Stack all reclassified layers from disk
  if (length(reclass_files) == 0) {
    stop("No valid layers to process. Please check your input data.")
  }
  
  rc_stack <- rast(reclass_files)
  
  # Sum across reclassified layers and write final result
  sum_raster <- app(rc_stack, fun = sum, filename = output_filename, 
                    overwrite = TRUE, datatype = "INT1U", NAflag = 255)
  
  return(sum_raster)
}

# Fire disturbance
fire_m <- c(
  0, 0, 1, 1, 2, 0, 3, 0, 4, 0, 5, 1, 6, 0, 7, 0,
  8, 0, 9, 1, 10, 0, 11, 0, 12, 0, 13, 1, 14, 0, 15, 0
)
fire_sum <- reclassify_layer_by_layer(
  raster_stack = data,
  reclass_matrix = fire_m,
  layer_indices = 1:22,
  output_dir = "tmp_fire_reclass",
  output_filename = "fire_stack.tif"
)

# Beetle disturbance
beetle_m <- c(
  0, 0, 1, 0, 2, 1, 3, 0, 4, 0, 5, 0, 6, 1, 7, 0,
  8, 0, 9, 0, 10, 1, 11, 0, 12, 0, 13, 0, 14, 1, 15, 0
)
beetle_sum <- reclassify_layer_by_layer(
  raster_stack = data,
  reclass_matrix = beetle_m,
  layer_indices = 1:22,
  output_dir = "tmp_beetle_reclass",
  output_filename = "beetle_stack.tif"
)

# Drought disturbance
drought_m <- c(
  0, 0, 1, 0, 2, 0, 3, 0, 4, 0, 5, 0, 6, 0, 7, 0,
  8, 0, 9, 0, 10, 0, 11, 0, 12, 1, 13, 1, 14, 1, 15, 1
)
drought_sum <- reclassify_layer_by_layer(
  raster_stack = data,
  reclass_matrix = drought_m,
  layer_indices = 1:22,
  output_dir = "tmp_drought_reclass",
  output_filename = "drought_stack.tif"
)


# Binary fire >1
fire_sum_rast <- rast("fire_stack.tif")

fire_sum_over_1 <- c(0, 1, 0, 1, 16, 1, 17, 255, 255)  # You had a 3-column reclass here
fso1_rclmat <- matrix(fire_sum_over_1, ncol=3, byrow=TRUE)

fso1_rc <- classify(fire_sum_rast, fso1_rclmat)
writeRaster(fso1_rc, filename="fire_sum_over_1.tif", overwrite=TRUE, 
            datatype='INT1U', NAflag=255)
