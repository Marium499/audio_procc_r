library(tuneR)

library(audio)
library(seewave)
library(stringr)
library(dplyr)
library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())
library(stringr)
library(matrixStats)
library(melfcc)
library(caTools)
library(class)
library(e1071)
library(caret)
library(factoextra)
library("FactoMineR")
library("ggcorrplot")

library("corrr")



library(tuneR)
library(audio)
library(seewave)
library(stringr)
library(dplyr)
library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())
library(stringr)
library(matrixStats)
library(melfcc)


##### DATA LOADING FOR DATASETS ########

### LOAD PRE-BUILT DATASET ########

df <- read.csv("output/df_final_combined_6_mfcc.csv")

### LOAD FROM SCRATCH #####


#########   SAVEE ########

savee_path <- "data/savee/AudioData"
savee_paths <-list.files(savee_path, pattern = ".wav", full.names = TRUE, recursive = TRUE)
savee_paths <- Filter(function(x) !any(grepl("AudioData/AudioData", x)), savee_paths)
savee_paths <- sort(savee_paths)
filenames_s <- sapply(savee_paths, function(x) substr(x, 25, nchar(x)-4))
duration_s <- round(sapply(savee_paths, function(x) duration(readWave(x))), 3)
sr_s <- sapply(savee_paths, function(x) readWave(x)@samp.rate)
emotion_s <- sapply(savee_paths, function(x) {
  if (substr(x, 25, 26) == 'sa') {
    return('sad')
  } else {
    switch(substr(x, 25, 25),
           'a' = 'angry',
           'd' = 'disgust',
           'f' = 'fear',
           'h' = 'happy',
           'n' = 'neutral',
           's' = 'surprise')
  }
})

df_savee <- data.frame(path = savee_paths,
                       filename = filenames_s,
                       dataset = 'SAVEE',
                       duration = duration_s,
                       sample_rate = sr_s,
                       gender = 'male',
                       age = 29,
                       emotion = emotion_s)


summary(df_savee)
################### CREMA D


crema_d_path <- "data/cremad/AudioWAV/"

# Get the list of audio files in the dataset folder
crema_audio_files <- list.files(crema_d_path, pattern = ".wav", full.names = TRUE)
# Initialize lists
filenames_c <- vector("list", length(crema_audio_files))
duration_c <- vector("numeric", length(crema_audio_files))
sr_c <- vector("numeric", length(crema_audio_files))
gender_c <- vector("character", length(crema_audio_files))
age_c <- vector("numeric", length(crema_audio_files))
emotion_c <- vector("character", length(crema_audio_files))

# Extract audio file names
audio_files <- str_extract(crema_audio_files, "\\d+.*\\.wav")
filenames_c <- audio_files

# Calculate duration and sample rate
for (i in seq_along(crema_audio_files)) {
  sound <- readWave(crema_audio_files[i])
  duration_c[i] <- seewave::duration(sound)
  sr_c[i] <- sound@samp.rate
}

# Extract actor ID from file path
actor_ID <- as.integer(str_extract(crema_audio_files, "(?<=/)[0-9]{4}"))

# Retrieve gender and age from demographics dataframe
gender_c <- tolower(crema_demographics_df$Sex[match(actor_ID, crema_demographics_df$ActorID)])
age_c <- crema_demographics_df$Age[match(actor_ID, crema_demographics_df$ActorID)]

# Map emotion based on file names
emotion_mapping <- c("SAD" = "sad", "ANG" = "angry", "DIS" = "disgust", "FEA" = "fear", "HAP" = "happy", "NEU" = "neutral")
emotion_c <- emotion_mapping[str_sub(audio_files, 10, 12)]

# Create dataframe
df_c <- data.frame(
  path = crema_audio_files,
  filename = filenames_c,
  dataset = rep("CREMA-D", length(crema_audio_files)),
  duration = duration_c,
  sample_rate = sr_c,
  gender = gender_c,
  age = age_c,
  emotion = emotion_c,
  stringsAsFactors = FALSE
)

summary(df_c)


###################### 
###### DATA EXPLORATION ######## ONLY FOR COMBINED DATASETS

# First subplot: Distribution of audio files by target emotion and dataset
subplot1 <- ggplot(df_c, aes(x = emotion, fill = dataset)) +
  geom_bar(position = "stack") +
  labs(
    y = "number of samples") +
  theme(legend.title = element_blank())

# Second subplot: Gender distribution for each emotion
subplot2 <- ggplot(df_c, aes(x = emotion, fill = gender)) +
  geom_bar(position = "stack") +
  labs(
    y = "number of samples") +
  theme(legend.title = element_blank())

# Combine subplots into a single figure
#combined_plot <- subplot1 + subplot2 + plot_layout(ncol = 2)

# Display the figure
print(subplot1)

# Display the figure
print(subplot2)

figure <- ggarrange(subplot1, subplot2,
                    labels = c("Distribution of audio files by target emotion and dataset", "Gender distribution for each emotion"),
                    ncol = 2, nrow = 1,widths = c(2, 2), heights = c(2, 2))


print(figure)

# Plotting sample rate values

value_counts <- table(df_c$sample_rate)

# Print the value counts
print(value_counts)

subplot3 <- ggplot(df_c, aes(x = factor(sample_rate))) +
  geom_bar() +
  labs(title = 'Sample rate values',
       x = 'sample frequencies (Hz)',
       y = 'number of samples') +
  theme(plot.title = element_text(hjust = 0.5))

print(subplot3)

# Plotting speaker age
subplot4 <- ggplot(df_c, aes(x = age)) +
  geom_histogram(bins = 30, fill = 'lightblue', color = 'black') +
  labs(title = 'Speakers age in the dataset',
       x = 'age',
       y = 'number of samples') +
  theme(plot.title = element_text(hjust = 0.5))

print(subplot4)

# Create the violin plot
subplot5 <- ggplot(df_c, aes(x = dataset, y = duration)) +
  geom_violin(trim = FALSE, fill = "lightpink", draw_quantiles = c(0.25, 0.5, 0.75)) +
  labs(x = "Dataset name", y = "Files duration (sec)", title = "Samples duration for each dataset") +
  theme_bw() +
  theme(plot.title = element_text(size = 12)) +
  coord_flip()

print(subplot5)

# Randomly select a few audio files from df_c
sample_rows <- sample(nrow(df), 3)
sample_audio_files <- df$path[sample_rows]

# Iterate through the selected audio files
for (audio_file in sample_audio_files) {
  # Read the audio file
  audio <- readWave(audio_file)
  
  # Print the other columns for the current audio file
  audio_info <- df[df$path == audio_file, ]
  print(audio_info)
  
  # Visualize the waveform
  par(mfrow = c(2, 2))
  plot(audio, main = "Waveform")
  
  # Compute and visualize the spectrogram
  #spec <- spec(audio)
  #plot(spec, main = "Spectrogram")
  
  # Compute and visualize the MFCCs
  mfcc <- melfcc(audio)
  #plot(mfcc, main = "MFCCs")
  print("MFCC COL MEANS")
  print(colMeans(mfcc))
  
  # Play the audio
  tuneR::play(audio)
}



##############################


######## DATA CLEANING ########


clean_dataset <- function(df, dataset) {
  # Select files linked to the dataset
  df_clean <- df[df$dataset == dataset, ]
  
  for (i in 1:nrow(df_clean)) {
    # Load audio file at a sample rate of 16000 Hz
    audio_file <- df_clean$path[i]
    audio <- readWave(audio_file)
    tuneR::play(audio)
    
    # Trim signal at a level of 20 dB
    y_trim <- noSilence(audio, level = -20, where = "both")
    
    # Remove 25% noise from audio samples
    y_noise_rem <- rmnoise(y_trim, output = "Wave", f = 16000)
    
    # Normalize the audio signal
    #y_normalized <- normalize(y_noise_rem)
    y_normalized <- normalize(y_noise_rem, method="range", range = c(-32768, 32767))
    
    # Create a new file name with '_cleaned.wav' suffix
    cleaned_file <- paste0(tools::file_path_sans_ext(audio_file), "_cleaned.wav")
    
    # Save output in a WAV file
    writeWave(y_noise_rem, cleaned_file)
    df_clean$path[i] <- str_replace(df_clean$path[i], "\\.wav$", "_cleaned.wav")
  }
  return (df_clean)
}


downsample_dataset<- function(df, dataset, sample_rate) {
  
  df_downsample <- df[df$dataset == dataset, ]
  
  for (i in 1:nrow(df_downsample)) {
    # Load audio file at a sample rate of 16000 Hz
    audio_file <- df_downsample$path[i]
    audio <- readWave(audio_file)
    
    y_down <- downsample(audio, samp.rate = sample_rate)
    
    # Create a new file name with '_cleaned.wav' suffix
    cleaned_file <- paste0(tools::file_path_sans_ext(audio_file), "_down.wav")
    
    # Save output in a WAV file
    writeWave(y_down, cleaned_file)
    df_downsample$path[i] <- str_replace(df_downsample$path[i], "\\.wav$", "_down.wav")
    df_downsample$sample_rate[i] <- sample_rate
    
  }
  return (df_downsample)
  
}

df_downsample <- downsample_dataset(df, "SAVEE", 16000)
str(df_downsample)
#df_downsample$path[df_downsample$dataset=="SAVEE"]

df_clean <- clean_dataset(df_downsample, "CREMA-D")

