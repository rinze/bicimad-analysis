library(jsonlite)
library(ggplot2)
theme_set(theme_bw(10))
library(reshape2)

# Use your own path to the .json file
bicis <- stream_in(file("/tmp/station_data.json"))
bicis$datetime <- as.POSIXlt(sapply(bicis$datetime, function(x) 
                                                    gsub(".000+0000", "", x, fixed = TRUE)), 
                                             format = "%Y-%m-%dT%H:%M:%S")

# Factors
bicis$name <- factor(bicis$name)

# Correct time (add 2 hours)
bicis$datetime <- bicis$datetime + 60 * 60 * 2

# Compute percentages
bicis$available <- with(bicis, available_bikes / (total_spots - unavailable_bikes))
bicis$free <- with(bicis, free_spots / total_spots)
bicis$broken <- with(bicis, unavailable_bikes / total_spots)

# Generate "day" and "hour" variables.
bicis$date <- as.Date(bicis$datetime)
bicis$hour <- format(bicis$datetime, "%H")

bicis_orig <- bicis

# Plot one week of data
bicis <- bicis[bicis$datetime < as.POSIXct("2015-07-23 16:00:00"), ]

cat("Plotting broken...\n")
p1 <- ggplot(bicis_orig) + geom_line(aes(x = datetime, y = broken)) + 
      facet_wrap(~ name)
ggsave(p1, filename = "/tmp/broken.pdf", width = 20, height = 20)
cat("OK\n")


#### PCA analysis ####
# Generate "day" and "hour" variables.
cat("Doing PCA analysis...\n")
# Average available_bikes per station and date and hour.
X <- dcast(data = bicis_orig, formula = date ~ name + hour, 
           value.var = "available_bikes", fun = mean)
X[is.na(X)] <- 0
pca1 <- prcomp(X[, -1], scale. = TRUE, center = TRUE)
pcaplot <- data.frame(pca1$x[, 1:2], format(X$date, "%A"))
names(pcaplot) <- c("PC1", "PC2", "dayofweek")
pltpca <- ggplot(pcaplot) + 
          geom_point(aes(x = PC1, y = PC2, color = dayofweek), size = 3) +
          scale_color_manual(values = rainbow(7))
# Plot the bikes for both groups
X$cluster <- ifelse(pca1$x[, 1] > 10, 1, 2)
cinfo <- data.frame(date = X$date, cluster = X$cluster)
bicis_orig <- merge(bicis_orig, cinfo)
traj <- aggregate(available_bikes ~ hour + cluster, 
                  bicis_orig, sum, na.rm = TRUE)
traj$hour <- as.numeric(traj$hour)
plttraj <- ggplot(traj) + 
           geom_line(aes(x = hour, y = available_bikes, group = cluster)) + 
           facet_wrap(~ cluster)
plttraj

# Same for Puerta de Toledo
traj_pt <- aggregate(available_bikes ~ hour + cluster, 
                    bicis_orig[bicis_orig$name == "Puerta de Toledo", ], 
                   sum, na.rm = TRUE)
traj_pt$hour <- as.numeric(traj_pt$hour)
plttraj_pt <- ggplot(traj_pt) + 
              geom_line(aes(x = hour, y = available_bikes, group = cluster)) + 
              facet_wrap(~ cluster)
plttraj_pt
cat("OK\n")

#### Bimodal analysis ####
# Find stations that "follow" Puerta de Toledo and those that "oppose" it.
tmp1 <- bicis[bicis$name == "Puerta de Toledo", 
              c("datetime", "available")]
cor_to_ptol <- sapply(levels(bicis$name), function(x) {
                      cat(x, "\n")
                      # Unequal lengths, merge by datetime
                      tmp2 <- bicis[bicis$name == x, 
                                    c("datetime", "available")]
                      tmp <- merge(tmp1, tmp2, by = "datetime", 
                                   sort = FALSE)
                      tmp <- tmp[!is.na(tmp$available.y), ]
                      cor(tmp$available.x, tmp$available.y)
                      }
                      )

like_pt <- bicis_orig[bicis_orig$name %in% 
                      names(cor_to_ptol[cor_to_ptol > 0.45]), ]
unlike_pt <- bicis_orig[bicis_orig$name %in% 
                        names(cor_to_ptol[cor_to_ptol < -0.45]), ]
like_pt$group <- 1
unlike_pt$group <- 2
traj3 <- rbind(like_pt, unlike_pt)
traj3 <- traj3[traj3$datetime >= as.POSIXct("2015-07-20 00:00:00") &
               traj3$datetime < as.POSIXct("2015-07-25 00:00:00"), ]
traj3 <- aggregate(available_bikes ~ datetime + group, traj3, mean, na.rm = TRUE)
traj3$group <- factor(traj3$group)
plttraj3 <- ggplot(traj3) + 
            geom_line(aes(x = datetime, y = available_bikes, color = group)) +
            scale_color_manual(values = c("red", "blue"))
plttraj3

