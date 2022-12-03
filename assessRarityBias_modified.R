
#### internal function to convert users' data to occAssess-friendly format

createData <- function(data, 
                       species,
                       x,
                       y,
                       year,
                       spatialUncertainty,
                       identifier) {
  
  dat <- data.frame(species = data[, species],
                    x = data[, x],
                    y = data[, y],
                    year = data[, year],
                    spatialUncertainty = data[, spatialUncertainty],
                    identifier = data[, identifier])
  
}


assessRarityBias_modified <- function(dat,
                                      species,
                                      x,
                                      y,
                                      year,
                                      spatialUncertainty,
                                      identifier,
                                      periods, 
                                      res, 
                                      prevPerPeriod, 
                                      maxSpatUncertainty = NULL,
                                      metric = "r2") {
  
  if (any(!(c(species, x, y, year, spatialUncertainty, identifier) %in% colnames(dat)))) stop("You have specified columns that don't exist in dat.")
  
  if (metric != "r2" & metric != "cor") stop("metric must be one of r2 or cor")
  
  dat <- createData(data = dat,
                    species,
                    x,
                    y,
                    year,
                    spatialUncertainty,
                    identifier)
  
  if (any(is.na(dat$identifier))) stop("One or more NAs in the identifier field. NAs are not permitted.")
  
  if (!is.null(maxSpatUncertainty)) dat <- dat[!is.na(dat$spatialUncertainty) & dat$spatialUncertainty <= maxSpatUncertainty, ]
  
  if (nrow(dat) == 0) stop("No records with with spatialUncertainty < maxSpatUncertainty")
  
  dat <- dat[order(dat$year), ]
  
  if (any(!dat$year %in% unlist(periods))) {
    
    drop <- which(!dat$year %in% unlist(periods))
    
    dat <- dat[-drop, ]
    
  }
  
  dat$Period <- NA
  
  for (i in 1: length(periods)) {
    
    dat$Period <- ifelse(dat$year %in% periods[[i]], i, dat$Period)
    
  }
  
  if (any(is.na(dat$year))) {
    
    warning(paste("Removing", nrow(dat[is.na(dat$year), ]), "records because they are do not have a year associated."))
    
    dat <- dat[-which(is.na(dat$year)), ]
    
  }
  
  if (any(is.na(dat$species))) {
    
    warning(paste("Removing", nrow(dat[is.na(dat$species), ]), "records because they are do not identified to species level."))
    
    dat <- dat[-which(is.na(dat$species)), ]
    
  }
  
  
  xmin <- min(dat$x, na.rm = T)
  
  xmax <- max(dat$x, na.rm = T)
  
  ymin <- min(dat$y, na.rm = T)
  
  ymax <- max(dat$y, na.rm = T)
  
  rast <- raster::raster(ncol=length(seq(xmin, xmax, res)),
                         nrow=length(seq(ymin, ymax, res)),
                         xmn=xmin,
                         xmx=xmax,
                         ymn=ymin,
                         ymx=ymax)
  
  for (i in unique(dat$identifier)) {
    
    x <- lapply(1:length(periods),
                function(y) {
                  
                  spp <- unique(dat$species[dat$Period == y & dat$identifier == i])
                  
                  if (length(spp) >= 5) {
                    
                    stats <- lapply(spp, 
                                    function(x) {
                                      if (prevPerPeriod == TRUE) {
                                        r <- raster::rasterize(cbind(dat$x[dat$species == x & dat$identifier == i & dat$Period == y], dat$y[dat$species == x& dat$identifier == i & dat$Period == y]), rast)
                                      } else {
                                        r <- raster::rasterize(cbind(dat$x[dat$species == x & dat$identifier == i], dat$y[dat$species == x& dat$identifier == i]), rast)
                                      }
                                      
                                      cells <- raster::getValues(r)
                                      cells <- length(cells[!is.na(cells)])
                                      recs <- nrow(dat[dat$species == x & dat$Period == y & dat$identifier == i, ])
                                      
                                      
                                      
                                      data.frame(species = x,
                                                 cells = cells,
                                                 recs = recs)
                                      
                                    })
                    
                    stats <- do.call("rbind", stats)
                    model <- summary(stats::lm(stats$recs ~ stats$cells))
                    
                    if(metric == "r2") {
                      
                      mod <- model$r.squared
                      res <- model$residuals
                      
                    } else {
                      
                      mod <- cor(stats$recs, stats$cells)
                      res <- model$residuals
                    }
                    
                    
                  } else {
                    
                    warning(paste("Fewer than five species in period", y, "for identifier", i,". No index will be calculated for this period/ identifier combination."))
                    
                    mod <- NA
                    
                  }
                  
                  
                  tibble::tibble(period = y, 
                                 id = i,
                                 index = mod,
                                 res = res)
                })
    
    assign(paste0("out", i), do.call("rbind", x))
    
  }
  
  out <- lapply(unique(dat$identifier), 
                function(x) {get(paste0("out", x))})
  
  out <- do.call("rbind", out)
  out2 <- out |>
    dplyr::select(-res) |> dplyr::distinct()
  
  p <- ggplot2::ggplot(data = out2, ggplot2::aes(x = period, y = index, colour = id, group = id)) +
    ggplot2::theme_linedraw() +
    ggplot2::geom_point() +
    ggplot2::geom_line() + 
    ggplot2::xlab("Period") +
    ggplot2::ylab("Taxonomic bias index") +
    ggplot2::labs(colour = "")
  
  return(list(plot = p, 
              data = out))
  
}