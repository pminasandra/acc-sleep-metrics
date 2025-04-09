## subset this individual's data to just that night
night_dat <- id_dat[ id_dat$night_date == night, ]

## should already be in order, but just in case
night_dat <- night_dat[ order( night_dat$local_timestamp ), ]

sleep_per[ sleep_per$tag == tag & sleep_per$night_date == night, ]$n_bursts <- unique( night_dat$n_bursts )

sleep_per[ sleep_per$tag == tag & sleep_per$night_date == night, ]$max_time_diff <- unique( night_dat$max_time_diff )

sleep_per[ sleep_per$tag == tag & sleep_per$night_date == night, ]$total_pot_sleep <- sum( night_dat$pot_sleep )

sleep_per[ sleep_per$tag == tag & sleep_per$night_date == night, ]$total_sleep_bouts <- sum( night_dat$sleep_bouts )

sleep_per[ sleep_per$tag == tag & sleep_per$night_date == night, ]$ave_vedba <- mean( night_dat$log_vedba )

sleep_per[ sleep_per$tag == tag & sleep_per$night_date == night, ]$dark_pot_sleep <- sum( night_dat$pot_sleep[ night_dat$local_time > dark_start | night_dat$local_time < dark_end ] )

sleep_per[ sleep_per$tag == tag & sleep_per$night_date == night, ]$dark_ave_vedba <- mean( night_dat$log_vedba[ night_dat$local_time > dark_start | night_dat$local_time < dark_end ] )



SPT_dat <- night_dat[ night_dat$sleep_per == 1, ]

if( nrow( SPT_dat ) > 0 ){
  
  onset <- min( SPT_dat$local_timestamp )
  
  sleep_per[ sleep_per$tag == tag & sleep_per$night_date == night, ]$onset <- onset
  
  waking <- max( SPT_dat$local_timestamp ) 
  
  sleep_per[ sleep_per$tag == tag & sleep_per$night_date == night, ]$waking <- waking
  
  SPT <- as.numeric( waking - onset, units = 'mins' ) + 1
  
  sleep_per[ sleep_per$tag == tag & sleep_per$night_date == night, ]$SPT <- SPT
  
  WASO <- sum( SPT_dat$sleep_bouts == 0 )
  
  sleep_per[ sleep_per$tag == tag & sleep_per$night_date == night, ]$WASO <- WASO
  
  TST <- sum( SPT_dat$sleep_bouts == 1 )
  
  sleep_per[ sleep_per$tag == tag & sleep_per$night_date == night, ]$TST <- TST
  
  sleep_per[ sleep_per$tag == tag & sleep_per$night_date == night, ]$sleep_eff <- TST/ nrow( SPT_dat )
  
  sleep_per[ sleep_per$tag == tag & sleep_per$night_date == night, ]$summed_VeDBA <- sum( SPT_dat$log_vedba )
  
  sleep_per[ sleep_per$tag == tag & sleep_per$night_date == night, ]$night_VeDBA_corr <- sum( SPT_dat$log_vedba ) / SPT
  
  temp <- rle( SPT_dat$sleep_bouts )
  
  runs <- as.numeric( rep( temp$lengths >= frag_block, times = temp$lengths ) )
  
  frag_wake_bouts <- as.numeric( SPT_dat$sleep_bouts == 0 & runs == 1 )
  
  diffs <- diff( c( 1, frag_wake_bouts ) )
  
  sleep_per[ sleep_per$tag == tag & sleep_per$night_date == night, ]$frag_wake_bouts <- sum( diffs == 1 )
  
  ## find the distinct sleep bouts (i.e. epochs of sleep separated by waking)
  diffs <- diff( c( 0, SPT_dat$sleep_bouts ) )
  
  ## save the number of distinct wake bouts
  sleep_per[ sleep_per$tag == tag & sleep_per$night_date == night, ]$wake_bouts <- sum( diffs == -1 )
  
  ## find durations of sleep and wake bouts
  temp <- rle( SPT_dat$sleep_bouts )
  
  ## add the duration of sleep bouts to the sleep bout duration vector
  sleep_durs <- c( sleep_durs, temp$lengths[ temp$values == 1 ] )
  ## add the duration of wake bouts to the wake bout duration vector
  wake_durs <- c( wake_durs, temp$lengths[ temp$values == 0 ] )
  
  
}