rm(list=ls())

setwd('~/lokacije/USV_navigation')

library(trajr)

convert_ros2_to_trajr_time = function(ros2_timestamps) {

  timestamps_sec = ros2_timestamps
  
  # Normalize so first timestamp is 0
  t0 = timestamps_sec[1]
  normalized_times = timestamps_sec - t0
  
  return(normalized_times)
}

create_trajectory_from_data = function(data) {
  
  trajr_times = convert_ros2_to_trajr_time(data$timestamp)
  
  coords = data.frame(x = c(data[,c(6)]),
                      y = c(data[,c(5)]),
                      time = trajr_times)
  
  
  planned_traj = TrajFromCoords(track = coords)
  
  return(planned_traj)
}

calculate_trajectory_params = function(trajectory, visualize = FALSE) {
  
  # Trajectory length
  traj_length = TrajLength(trajectory)
  
  # Duration of travel
  traj_duration = TrajDuration(trajectory)
  
  # Diffusion distance
  diffusion_distance = TrajDistance(trajectory)
  
  # Mean travel velocity
  derivs = TrajDerivatives(trajectory)
  mean_velocity = mean(derivs$speed)
  
  # Straightness ([0, 1], 1 - straight line)
  straightness = TrajStraightness(trajectory)
  
  # Fractal dimension ([1, 2], 1 - straight line, 2 - infinitely variable curve)
  step_sizes = TrajLogSequence(1.0, 70.0, 30)
  
  fractal_dimension = TrajFractalDimension(trajectory, stepSizes = step_sizes)
  
  
  if (visualize) {
    plot(trajectory, main = "Trajectory")
    cat("Total length of the trajectory:", traj_length, "meters \n")
    cat("Temporal duration of the trajectory (simulated time):", traj_duration, "seconds \n")
    cat("Straight-line distance from the start to the end of the trajectory:", diffusion_distance, "meters \n")
    cat("Mean travel velocity:", mean_velocity, "m/s \n")
    cat("Straightness index:", straightness, "\n")
    cat("Fractal dimension:", fractal_dimension, "\n")
  }
  
  # Return a list with all of the statistics for this trajectory
  list(meanTravelSpeed = mean_velocity,
       straightness = straightness,
       diffusionDistance = diffusion_distance,
       trajectoryLength = traj_length,
       durationOfTravel = traj_duration,
       fractalDimension = fractal_dimension
  )
}

plot_trajectory_stats = function(stats_column, ylabel, title, alpha_threshold) {
  # Calculate the significance threshold
  ref_value = stats_column[1]
  
  upper_thresh_value = ref_value + ref_value*alpha_threshold
  lower_thresh_value = ref_value - ref_value*alpha_threshold
  
  lower_lim = min(lower_thresh_value, min(stats_column))
  upper_lim = max(upper_thresh_value, max(stats_column))
  
  cat("Alpha: ", alpha_threshold, ", Lower bound: ", lower_thresh_value, ", Upper bound: ", upper_thresh_value, "\n")
  
  # Plot trajectory parameter with significance thresholds
  plot.new()
  par(mar = c(8, 4, 4, 2) + 0.1)  # Increase bottom margin
  
  grid(nx = NULL, ny = NULL,
       lty = 1,
       col = "gray",
       lwd = 1)
  
  par(new = TRUE)
  
  plot(1:length(stats_column), 
       stats_column, 
       xaxt = "n",
       xlab = "", ylab = ylabel, main=title,
       pch=19, col="blue",cex=1.5, ylim=c(lower_lim, upper_lim))

  abline(h=c(lower_thresh_value, upper_thresh_value), col=c("red", "red"), lty=c(2,2))
  
  axis(1, at = 1:length(stats_column), 
       labels = c("Planned", "Low waves", "Medium waves", "High waves", "Slow wind", "Medium wind", "Strong wind"), 
       las = 2)  # Rotate labels vertically
  

}


# Load trajectory data

planned_data = read.csv("./data/planned_trajectory_data_500ms.csv")

low_waves_data = read.csv("./data/actual_trajectory_low_waves.csv")
medium_waves_data = read.csv("./data/actual_trajectory_medium_waves.csv")
high_waves_data = read.csv("./data/actual_trajectory_high_waves.csv")

slow_wind_data = read.csv("./data/actual_trajectory_slow_wind.csv")
medium_wind_data = read.csv("./data/actual_trajectory_medium_wind.csv")
strong_wind_data = read.csv("./data/actual_trajectory_strong_wind.csv")

# Create trajectories from the data

planned_traj = create_trajectory_from_data(planned_data)

low_wave_traj = create_trajectory_from_data(low_waves_data)
medium_wave_traj = create_trajectory_from_data(medium_waves_data)
high_wave_traj = create_trajectory_from_data(high_waves_data)

slow_wind_traj = create_trajectory_from_data(slow_wind_data)
medium_wind_traj = create_trajectory_from_data(medium_wind_data)
strong_wind_traj = create_trajectory_from_data(strong_wind_data)

trajs = list(planned_traj, 
             low_wave_traj, medium_wave_traj, high_wave_traj,
             slow_wind_traj, medium_wind_traj, strong_wind_traj
        )
traj_names = list(
  "planned", 
  "low_waves", "medium_waves", "high_waves",
  "slow_wind", "medium_wind", "strong_wind"
)

# Get trajectory stats
stats = TrajsMergeStats(trajs, calculate_trajectory_params)

# Print stats
rownames(stats) = traj_names
print(stats)

# Plot trajectory stats with defined statistical significance
alpha_threshold = 0.05
plot_trajectory_stats(stats$meanTravelSpeed, "Mean Travel Speed [m/s]", "Mean Travel Speed comparison", alpha_threshold)
plot_trajectory_stats(stats$straightness, "Straightness Index [0, 1]", "Straightness comparison", alpha_threshold)
plot_trajectory_stats(stats$diffusionDistance, "Diffusion Distance [m]", "Diffusion Distance comparison", alpha_threshold)
plot_trajectory_stats(stats$trajectoryLength, "Trajectory Length [m]", "Trajectory Length comparison", alpha_threshold)
plot_trajectory_stats(stats$durationOfTravel, "Time [s]", "Duration of Travel comparison", alpha_threshold)
plot_trajectory_stats(stats$fractalDimension, "FractalDimension [1, 2]", "Fractal Dimension comparison", alpha_threshold)



