#n indicates the total number of videos for each channel.
top_ch <- function(data){
  aux <- data %>%
    distinct(channel_title, n) %>%
    arrange(-n) %>%
    head(20)
  
  return(data.frame(aux))
}

n_topic <- function(data){
  aux = data %>%
    group_by(topic) %>%
    summarise(vn_topic = sum(n))
  
  aux = aux %>%
    inner_join(data, by = 'topic') %>%
    distinct(topic, channel_title) %>%
    group_by(topic) %>%
    count() %>%
    inner_join(aux, by = 'topic')
  
  return(data.frame(aux))
}

topic_avg <- function(data){
  aux = data %>%
    group_by(topic) %>%
    summarise(avg_like = median(likes),
              avg_dislike = median(dislikes),
              avg_view = median(views, na.rm = T), 
              avg_comment = median(comment_count, na.rm = T))
  
  return(data.frame(aux))
}

us_time <- function(data){
  aux = data %>%
    select(video_id, topic, publish_time, trending_date) %>%
    mutate(upload_date = as.Date(publish_time, format = '%Y-%m-%d'), 
           period = as.integer(trending_date - upload_date))
  
  return(data.frame(aux))
}

keyword = function(x, data){
  # unnest token the tag column
  tag_tidy = data %>%
    #filter(topic == x) %>%
    filter(views > quantile(data$views, probs = .75)) %>%
    select(tags) %>%
    unnest_tokens(output = 'word', input = tags) 
  
  # wordcloud plot for hot keyword
  tag_tidy <- tag_tidy %>%
    count(word) %>%
    arrange(desc(n))
    
  return(data.frame(tag_tidy))
    #wordcloud::wordcloud(words = word, freq = n, min.freq = 5, scale = c(.5, 1.5), max.words = 25, colors = brewer.pal(8, 'Dark2'))
}

topic_order <- function(data){
  aux = arrange(data, desc(data$n))
  return(data.frame(aux))
}

view_date <- function(data){
  aux = data %>%
    group_by(trending_date) %>%
    summarise(sum_view = sum(views, na.rm = T))
  
  aux <- aux %>%
    mutate(month_year = format(trending_date, "%Y-%m")) %>%
    group_by(month_year) %>%
    summarise(total = sum(sum_view))
  
  return(data.frame(aux))
}
