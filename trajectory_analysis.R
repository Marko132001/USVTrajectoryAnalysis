rm(list=ls())

setwd('~/lokacije/USV_navigation')

library(trajr)

convert_ros2_to_trajr_time = function(ros2_timestamps) {
  # Convert nanoseconds to seconds
  #timestamps_sec = as.numeric(ros2_timestamps) / 1e9
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
  step_sizes = TrajLogSequence(1.0, 10.0, 15)
  
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

# Trajectory simulation parameters
#####################################################
# Constants:
#   - wind_vel_std = 0.5
#   - wind_direction = 240° (vessel will be pushed in the SW direction )
#   - wave_direction = 0°
#   - wave_gain = 1.0
#
# Wind trajectories are variable based on wind velocity [m/s]
# Wave trajectories are variable based on wave period [s]
#
#             Wind_vel Wind_dir Wave_gain Wave_period
# low_waves      /         /         1.0       4
# medium_waves   /         /         1.0       7
# high_waves     /         /         1.0       10
# slow_wind       1         240       0.0       0.0001
# medium_wind    2.5       240       0.0       0.0001
# strong_wind    5         240       0.0       0.0001
#####################################################

# Zadaci
#####################################################
# Q: Usporediti parametre planirane i stvarne putanje. 
# Statističkim testom odrediti statističku značajnost eventualne razlike.
# A: Koristiti z-test ili nekakav thresholding (alpha = 0.05) za
#   pojedinačne vrijednosti.
#
# Q: Prikazati razlike u dobivenim vrijednostima parametera putanje,
#     ovisno o pojedinačnim parametrima stanja okoliša.
# A: Usporedba parametara svake putanje sa parametrima planirane putanje.
#     Rezultate prikazati grafički.
#####################################################


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







