library(readr)
trips_df <- read_csv("~/Downloads/san_francisco-censustracts-2020-1-All-DatesByHourBucketsAggregate.csv")

speed_df <- read_csv("~/Downloads/movement-speeds-quarterly-by-hod-san-francisco-2020-Q1.csv")

head(trips_df)
# A tibble: 6 × 10
  sourceid dstid month day   start_hour end_hour mean_travel_time
     <dbl> <dbl> <chr> <chr>      <dbl>    <dbl>            <dbl>
1     1642   447 01    24            19        0            1252.
2      917   832 03    13            16       19             275.
3      918   822 02    23            16       19             436.
4     1066  2611 01    19            10       16             810.
5     2694   533 01    18            10       16             723.
6     2694   533 02    08            10       16             603.
# ℹ 3 more variables: standard_deviation_travel_time <dbl>,
#   geometric_mean_travel_time <dbl>, geometric_standard_deviation_travel_time <dbl>
head(speed_df)
# A tibble: 6 × 13
   year quarter hour_of_day segment_id     start_junction_id end_junction_id osm_way_id
  <dbl>   <dbl>       <dbl> <chr>          <chr>             <chr>                <dbl>
1  2020       1           0 8f4827ebed3c2… 8e555723c3dff790… 2278ad9374ec96…   40722998
2  2020       1          10 8f4827ebed3c2… 8e555723c3dff790… 2278ad9374ec96…   40722998
3  2020       1          15 8f4827ebed3c2… 8e555723c3dff790… 2278ad9374ec96…   40722998
4  2020       1          19 8f4827ebed3c2… 8e555723c3dff790… 2278ad9374ec96…   40722998
5  2020       1           1 8f4827ebed3c2… 8e555723c3dff790… 2278ad9374ec96…   40722998
6  2020       1          23 8f4827ebed3c2… 8e555723c3dff790… 2278ad9374ec96…   40722998
# ℹ 6 more variables: osm_start_node_id <dbl>, osm_end_node_id <dbl>,
#   speed_mph_mean <dbl>, speed_mph_stddev <dbl>, speed_mph_p50 <dbl>,
#   speed_mph_p85 <dbl>

missing_trips <- sum(is.na(trips_df))
missing_speed <- sum(is.na(speed_df))

0 Length as Uber has cleaned up the data 

library(dplyr)
grouped_by_hour <- speed_df %>% 
    group_by(hour_of_day) %>% 
    summarise(Mean_Speed = mean(speed_mph_mean, na.rm = TRUE),
              StdDev_Speed = sd(speed_mph_mean, na.rm = TRUE))

ggplot(grouped_by_hour, aes(x=hour_of_day)) +
    geom_line(aes(y=Mean_Speed, color="Mean Speed")) +
    geom_line(aes(y=StdDev_Speed, color="Std Dev of Speed")) +
    ggtitle('Average Speed and Speed Variability by Hour of Day') +
    xlab('Hour of Day') +
    ylab('Speed (mph)') +
    scale_color_manual(values = c("Mean Speed"="blue", "Std Dev of Speed"="red"))


grouped_by_segment <- speed_df %>% 
    group_by(segment_id) %>% 
    summarise(Mean_Speed = mean(speed_mph_mean, na.rm = TRUE))
top_5_segments <- head(arrange(grouped_by_segment, desc(Mean_Speed)), 5)
bottom_5_segments <- head(arrange(grouped_by_segment, Mean_Speed), 5)
combined_segments <- rbind(top_5_segments, bottom_5_segments)

ggplot(combined_segments, aes(x=segment_id, y=Mean_Speed)) +
    geom_bar(stat="identity", aes(fill=Mean_Speed)) +
    ggtitle('Top and Bottom 5 Road Segments by Average Speed') +
    xlab('Road Segment ID') +
    ylab('Average Speed (mph)') +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))


categorize_congestion <- function(speed) {
    if (speed < 40) {
        return('High')
    } else if (speed >= 40 & speed < 50) {
        return('Medium')
    } else {
        return('Low')
    }
}

speed_df$Congestion_Level <- sapply(speed_df$speed_mph_mean, categorize_congestion)

head(speed_df[, c('speed_mph_mean', 'Congestion_Level')])
# A tibble: 6 × 2
  speed_mph_mean Congestion_Level
           <dbl> <chr>           
1           26.1 High            
2           22.4 High            
3           20.2 High            
4           22.1 High            
5           27.0 High            
6           23.9 High            
congestion_by_hour <- speed_df %>% 
    group_by(hour_of_day, Congestion_Level) %>% 
    summarise(Count = n())

ggplot(congestion_by_hour, aes(x=hour_of_day, y=Count, color=Congestion_Level, shape=Congestion_Level)) +
    geom_line() +
    geom_point() +
    ggtitle('Congestion by Time of Day') +
    xlab('Hour of Day') +
    ylab('Count') +
    theme(legend.title = element_text("Congestion Level"))


