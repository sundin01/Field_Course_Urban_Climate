# Function for line-plot
overview_creater <- function(database, yvar, title = c("title"), subtitle = c("Subtitle"),
                             xlab = c("xlab"), ylab = c("ylab"), colnbr = NULL, whole = TRUE){
  if(whole == TRUE){
    std <- round(sd(database[[colnbr]]), digits = 3)
    mean <- round(mean(database[[colnbr]]), digits = 3)
    plot_1 <- database|>
      mutate(aggregate_hourly = lubridate::round_date(time, "60 minutes"))|>
      group_by(aggregate_hourly)|>
      drop_na()|>
      ggplot(aes(x = aggregate_hourly, y = {{yvar}})) +
      geom_line(linewidth = 0.2) +
      geom_ribbon(aes(y = mean, ymin = mean - std, ymax = mean + std), alpha = 0.1, fill = "red")+
      geom_hline(yintercept =  0, size = 0.3, linetype = "dotdash")+
      geom_hline(yintercept =  mean, size = 0.3, linetype = "dotted", color = "red")+
      labs(x = paste(xlab), y = paste(ylab), title = paste(title),subtitle = paste(subtitle)) +
      theme_light()
    return(plot_1)}else{
      std <- round(sd(database[[colnbr]]), digits = 3)
      mean <- round(mean(database[[colnbr]]), digits = 3)
      plot_1 <- database|>
        mutate(aggregate_hourly = lubridate::round_date(time, "60 minutes"))|>
        mutate(hour_of_day = hour(aggregate_hourly)) |>
        group_by(hour_of_day)|>
        drop_na()|>
        ggplot(aes(x = hour_of_day, y = {{yvar}})) +
        geom_point(alpha = 0.4) +
        geom_ribbon(aes(y = mean, ymin = mean - std, ymax = mean + std), alpha = 0.1, fill = "red")+
        geom_hline(yintercept =  0, size = 0.3, linetype = "dotdash")+
        geom_hline(yintercept =  mean, size = 0.3, linetype = "dotted", color = "red")+
        labs(x = paste(xlab), y = paste(ylab), title = paste(title),subtitle = paste(subtitle)) +
        theme_light()
      return(plot_1)
    }
}

# Function for box-plot
boxplot_creater <- function(database, yvar, title = c("title"), subtitle = c("Subtitle"),
                            xlab = c("xlab"), ylab = c("ylab"), colnbr = 2, whole = TRUE, day = TRUE){
  if(whole == TRUE){
    mean <- round(mean(database[[colnbr]]), digits = 3)
    plot_1 <- database|>
      mutate(aggregate_hourly = lubridate::round_date(time, "60 minutes"))|>
      mutate(hour_of_day = hour(aggregate_hourly)) |>
      group_by(hour_of_day)|>
      drop_na()|>
      ggplot(aes(x = factor(hour_of_day), y = {{yvar}})) +
      geom_boxplot(fill = "skyblue", alpha = 0.5, lwd = 0.3, width = 0.5,
                   outlier.color = "red", outlier.shape = 17, outlier.size = 2)+
      geom_jitter(width = 0, alpha = 0.01) +
      # Add an error-bar and make a individual setup
      stat_boxplot(geom = "errorbar", size = 0.3, width = 0.3)+
      geom_hline(yintercept =  0, size = 0.3, linetype = "dotdash")+
      geom_hline(yintercept =  mean, size = 0.3, linetype = "dotted", color = "red")+
      labs(x = paste(xlab), y = paste(ylab), title = paste(title),
           subtitle = paste(subtitle)) +
      theme_light()
    return(plot_1)}
  else if(day == TRUE){
    std <- round(sd(database[[colnbr]]), digits = 3)
    mean <- round(mean(database[[colnbr]]), digits = 3)
    plot_1 <- database|>
      mutate(aggregate_hourly = lubridate::round_date(time, "60 minutes"))|>
      group_by(aggregate_hourly)|>
      mutate(hourly = lubridate::hour(aggregate_hourly))|>
      filter(hourly >= 6 & hourly <= 22)|>
      drop_na()|>
      ggplot(aes(x = factor(hourly), y = {{yvar}})) +
      geom_boxplot(fill = "skyblue", alpha = 0.5, lwd = 0.3, width = 0.5,
                   outlier.color = "red", outlier.shape = 17, outlier.size = 2)+
      geom_jitter(width = 0, alpha = 0.01) +
      # Add an error-bar and make a individual setup
      stat_boxplot(geom = "errorbar", size = 0.3, width = 0.3)+
      geom_hline(yintercept =  0, size = 0.3, linetype = "dotdash")+
      geom_hline(yintercept =  mean, size = 0.3, linetype = "dotted", color = "red")+
      labs(x = paste(xlab), y = paste(ylab), title = paste(title),
           subtitle = paste(subtitle)) +
      theme_light()
    return(plot_1)}
  else{
    std <- round(sd(database[[colnbr]]), digits = 3)
    mean <- round(mean(database[[colnbr]]), digits = 3)
    plot_1 <- database|>
      mutate(aggregate_hourly = lubridate::round_date(time, "60 minutes"))|>
      group_by(aggregate_hourly)|>
      mutate(hourly = lubridate::hour(aggregate_hourly))|>
      filter(hourly < 7 | hourly > 21)|>
      drop_na()|>
      ggplot(aes(x = factor(hourly), y = {{yvar}})) +
      geom_boxplot(fill = "skyblue", alpha = 0.5, lwd = 0.3, width = 0.5,
                   outlier.color = "red", outlier.shape = 17, outlier.size = 2)+
      geom_jitter(width = 0, alpha = 0.01) +
      # Add an error-bar and make a individual setup
      stat_boxplot(geom = "errorbar", size = 0.3, width = 0.3)+
      geom_hline(yintercept =  0, size = 0.3, linetype = "dotdash")+
      geom_hline(yintercept =  mean, size = 0.3, linetype = "dotted", color = "red")+
      labs(x = paste(xlab), y = paste(ylab), title = paste(title),
           subtitle = paste(subtitle)) +
      theme_light()
    return(plot_1)}
}    

# Function for density-plot
densityplot_creater <- function(database, xvar, title = c("title"), subtitle = c("Subtitle"),
                                xlab = c("xlab"), ylab = c("ylab"), nrb.bins = 30, colnbr = 2, whole = TRUE, day = TRUE){
  if(whole == TRUE){
    mean <- round(mean(database[[colnbr]]), digits = 3)
    std <- round(sd(database[[colnbr]]), digits = 3)
    plot_1 <- database|>
      mutate(aggregate_hourly = lubridate::round_date(time, "60 minutes"))|>
      mutate(hour_of_day = hour(aggregate_hourly)) |>
      group_by(hour_of_day)|>
      drop_na()|>
      ggplot(aes(x = {{xvar}}, y = after_stat(density))) +
      geom_histogram(alpha = 0.3, color = "white", fill = "black",  bins = nrb.bins) +
      geom_density(color = "blue", linewidth = 0.7)+
      geom_vline(xintercept = 0, size = 0.1, linetype = "dotted" )+
      geom_vline(xintercept =  mean, size = 0.3, linetype = "dotted", color = "red")+
      labs(x = paste(xlab),y = paste(ylab), title = paste(title), 
           subtitle = paste(subtitle))+
      theme_light()
    return(plot_1)}
  else if(day == TRUE){
    std <- round(sd(database[[colnbr]]), digits = 3)
    mean <- round(mean(database[[colnbr]]), digits = 3)
    plot_1 <- database|>
      mutate(aggregate_hourly = lubridate::round_date(time, "60 minutes"))|>
      group_by(aggregate_hourly)|>
      mutate(hourly = lubridate::hour(aggregate_hourly))|>
      filter(hourly >= 6 & hourly <= 22)|>
      drop_na()|>
      ggplot(aes(x = {{xvar}}, y = after_stat(density))) +
      geom_histogram(alpha = 0.3, color = "white", fill = "black",  bins = nrb.bins) +
      geom_density(color = "blue", linewidth = 0.7)+
      geom_vline(xintercept = 0, size = 0.1, linetype = "dotted" )+
      geom_vline(xintercept =  mean, size = 0.3, linetype = "dotted", color = "red")+
      labs(x = paste(xlab),y = paste(ylab), title = paste(title), 
           subtitle = paste(subtitle))+
      theme_light()
    return(plot_1)}
  else{
    std <- round(sd(database[[colnbr]]), digits = 3)
    mean <- round(mean(database[[colnbr]]), digits = 3)
    plot_1 <- database|>
      mutate(aggregate_hourly = lubridate::round_date(time, "60 minutes"))|>
      group_by(aggregate_hourly)|>
      mutate(hourly = lubridate::hour(aggregate_hourly))|>
      filter(hourly < 7 | hourly > 21)|>
      drop_na()|>
      ggplot(aes(x = {{xvar}}, y = after_stat(density))) +
      geom_histogram(alpha = 0.3, color = "white", fill = "black",  bins = nrb.bins) +
      geom_density(color = "blue", linewidth = 0.7)+
      geom_vline(xintercept = 0, size = 0.1, linetype = "dotted" )+
      geom_vline(xintercept =  mean, size = 0.3, linetype = "dotted", color = "red")+
      labs(x = paste(xlab),y = paste(ylab), title = paste(title), 
           subtitle = paste(subtitle))+
      theme_light()
    return(plot_1)}
}