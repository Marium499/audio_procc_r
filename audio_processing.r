
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


############## DATA CLEANING #############

clean_file <- function(audio_file) {
  
  audio <- readWave(audio_file)
  
  # Trim signal at a level of 20 dB
  y_trim <- noSilence(audio, level = -20, where = "both")
  
  # Remove 25% noise from audio samples
  y_noise_rem <- rmnoise(y_trim, output = "Wave", f = 16000)
  
  # Normalize the audio signal
  #y_normalized <- normalize(y_noise_rem)
  #y_normalized <- normalize(y_noise_rem, method="range", range = c(-32768, 32767))
  
  # Create a new file name with '_cleaned.wav' suffix
  cleaned_file <- paste0(tools::file_path_sans_ext(audio_file), "_cleaned.wav")
  
  # Save output in a WAV file
  writeWave(y_noise_rem, cleaned_file)
  
  return(y_noise_rem)
}



######################

########## FEATURE EXTRACTION ########


calculate_features <- function(audio_file) {
  
  # Read audio file
  
  audio <- readWave(audio_file)
  #tuneR::play(audio)
  # Convert audio to mono if it's in stereo
  if (nchannel(audio) > 1) {
    audio_mono <- audio@left + audio@right
  } else {
    audio_mono <- audio
  }
  
  spec <- periodogram(audio_mono)
  plot(spec)
  
  
  #nchannel(audio)
  # Calculate fundamental frequency
  f_zero <- FF(spec,peakheight = 0.4)
  
  ## zero crossing rate
  zcr = as.data.frame(zcr(audio_mono, f=16000, plot=FALSE))$zcr
  zcr_mean = mean(zcr,na.rm = TRUE)
  zcr_var = var(zcr,na.rm = TRUE)
  
  #spectral properties
  spec <- seewave::spec(audio_mono, f=16000, plot=FALSE)
  spec_prop <- specprop(spec, plot=FALSE)
  plot(spec, main = "Spectogram")
  
  # Return statistics as a data frame
  #data.frame(f0_mean = f0_mean, f0_median = f0_median, f0_std = f0_std, f0_min = f0_min, f0_max = f0_max, f0_25 = f0_25, f0_75 = f0_75)
  
  return(data.frame(f_zero = f_zero, zcr_mean = zcr_mean,
                    zcr_var = zcr_var, spec_centroid = spec_prop$cent, spec_skew = spec_prop$skewness,
                    spec_flatness = spec_prop$sfm, spec_peak = spec_prop$kurtosis, spec_entropy = spec_prop$sh,
                    spec_prec = spec_prop$prec))
}


# mel features

calculate_mfcc <- function(audio_file, n) {
  
  
  audio <- readWave(audio_file)
  
  if (nchannel(audio) > 1) {
    audio_mono <- audio$left + audio$right
  } else {
    audio_mono <- audio@left
  }

  
  mfcc <- melfcc(audio, sr= audio@samp.rate, minfreq = 50, numcep = n)
  plot(mfcc, main = "MFCCs")
  mfcc_mean <- colMeans(mfcc,na.rm = TRUE)
  mfcc_var <- colVars(mfcc, na.rm = TRUE)
  
  col_names <- paste0("mfcc_mean_", 1:n)
  
  # Create a vector of column names for 'bar' columns
  bar_names <- paste0("mfcc_var_", 1:n)
  
  # Combine the column names
  column_names <- c(col_names, bar_names)
  
  # Create an empty dataframe with m + n columns
  df <- data.frame(matrix(c(mfcc_mean[1:n],mfcc_var[1:n]), nrow = 1, ncol = n + n))
  colnames(df) <- column_names
  
  # Print the dataframe
  
  return (df)
  
  
}


### SAMPLE AUDIO ####
# angry, male, crema-d
AUDIO_PATH <- "data/cremad/AudioWAV/1001_DFA_ANG_XX.wav"
tuneR::play(readWave(AUDIO_PATH))

# data cleaning
#cleaned_audio <- clean_file(AUDIO_PATH)
#tuneR::play(cleaned_audio)

# calculate features
features <- calculate_features(AUDIO_PATH)
features

# calculate mfcc mean and variances
mel_features <- calculate_mfcc(AUDIO_PATH, n=6)
mel_features








