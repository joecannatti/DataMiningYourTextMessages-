#install.packages("RSQLite")
library(RSQLite)

load_db <- function(){
  driver = dbDriver("SQLite")
  con <- dbConnect(driver,dbname="texts.db")
  return(con)
}

load_message_data <- function(con){
  rs <- dbSendQuery(con, "SELECT * FROM message m JOIN chat_message_join j ON j.message_id = m.ROWID JOIN chat c ON j.chat_id = c.ROWID JOIN demographics d ON c.chat_identifier =  ('+' || d.number);")
  data <- fetch(rs, n=Inf)
  return(data)
}

load_balance_data <- function(con){
  rs <- dbSendQuery(con, "  select chat_identifier, count(*), sum(is_from_me) from_me, 
  count(*) - sum(is_from_me) as to_me, name, gender from message m
  join chat_message_join j
  on j.message_id = m.ROWID
  join chat c
  on j.chat_id = c.ROWID
  JOIN demographics d 
  ON c.chat_identifier =  ('+' || d.number)
  group by chat_identifier;")
  data <- fetch(rs, n=Inf)
  return(data)
}

con <- load_db()
message_data <- load_message_data(con)
balance_data <- load_balance_data(con)

load_overall_balance_data <- function(con){
  rs <- dbSendQuery(con, "  select chat_identifier, count(*), sum(is_from_me) from_me, 
  count(*) - sum(is_from_me) as to_me, name, gender from message m
  join chat_message_join j
  on j.message_id = m.ROWID
  join chat c
  on j.chat_id = c.ROWID
  JOIN demographics d 
  ON c.chat_identifier =  ('+' || d.number);")
  data <- fetch(rs, n=Inf)
  return(data)
}
#overall_balance <- load_overall_balance_data(con)
#pie_chart_for_text_balance_ratio(overall_balance)

load_leaders <- function(con){
  rs <- dbSendQuery(con, "SELECT
  count(*), name
FROM
	message m
JOIN chat_message_join j ON j.message_id = m.ROWID
JOIN chat c ON j.chat_id = c.ROWID
JOIN demographics d ON c.chat_identifier =  ('+' || d.number)
group by name
order by count(*) desc;")
  data <- fetch(rs, n=Inf)
  return(data)
}
leaders <- load_leaders(con)
leaders_count <- sapply(leaders['count(*)'], as.integer)
hist(leaders_count, col=c('red'))
hist(leaders_count, breaks=10, col=c('red'))
hist(leaders_count, breaks="FD", col=c('red'))

text_frequency_by_month <- function(data){
  hist(as.Date(sort(data$date/100000), origin="2002-08-15"), 
       breaks='months', 
       freq = TRUE, 
       format = "%d %b", 
       col=c('red'))
}

pie_chart_for_text_balance_ratio <- function(data){
  pie(c(data$to_me, data$from_me), c('to_me','from_me'), main=data$name)
}
#pie_chart_for_text_balance_ratio(balance_data[14,])
#pie_chart_for_text_balance_ratio(balance_data[46,])
#pie_chart_for_text_balance_ratio(balance_data[45,])
#pie_chart_for_text_balance_ratio(balance_data[44,])
#pie_chart_for_text_balance_ratio(balance_data[43,])
#pie_chart_for_text_balance_ratio(balance_data[37,])

get_contacts <- function(){
  
}

add_to_db <- function(vect,con){
  funct <- function(data){
    rs <- dbSendQuery(con, paste("insert into demographics (number,gender,name) values(", data, ", NULL, NULL)"))
  }
  sapply(vect, FUN=funct)
}
#data <- sort(unique(message_data$chat_identifier))
#numbers <- data[substring(data, 1, 1) == "+"]
#add_to_db(numbers, con)

png('39.png')
pie_chart_for_text_balance_ratio(balance_data[39,])
dev.off()

#text_frequency_by_month(message_data)

percentage_men <- function(data){
  data[data]
}


load_times <- function(con){
  rs <- dbSendQuery(con, "SELECT
count(*), name, group_concat(date) as times, gender
FROM
message m
JOIN chat_message_join j ON j.message_id = m.ROWID
JOIN chat c ON j.chat_id = c.ROWID
JOIN demographics d ON c.chat_identifier =  ('+' || d.number)
group by name
order by count(*) desc;")
  data <- fetch(rs, n=Inf)
  return(data)
}
times <- load_times(con)
res <- apply(times,1,function(data){data[3]})

median_response_times <- rep(0, length(res))
count = 0
for(r in res){
  count = count + 1
  comps <- unlist(strsplit(r, ","))
  print(length(comps))
  diffs <- rep(0, length(comps) - 1)
  i = 0
  for(c in comps){
    i = i + 1
    if(i > 1){
      diffs[i] <- (as.integer(c) - as.integer(comps[i-1]))
    }
  }
  median_response_times[count] <- mean(diffs)
}
times$median_response_time <- median_response_times
male_response_times <- times[times$gender == 1,]
male_response_times_clean <- male_response_times[!is.na(male_response_times$median_response_time),]$median_response_time
female_response_times <- times[times$gender == 0,]
female_response_times_clean <- female_response_times[!is.na(female_response_times$median_response_time),]$median_response_time
mean(female_response_times_clean)
mean(male_response_times_clean)
t.test(female_response_times_clean, male_response_times_clean)
male_d <- density(male_response_times_clean)
female_d <- density(female_response_times_clean)
plot(female_d)
plot(male_d)

#message_data_ided <- message_data[!is.na(message_data$gender),]
#message_data_men <- message_data_ided[message_data_ided$gender == 1,]
