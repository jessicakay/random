
# install.packages(c("DBI","RSQLite","RPostgreSQL"))

install.packages("RPostgres")
install.packages("lubridate")
library(DBI)
library(RSQLite)
library(RPostgres)
library(dplyr)
library(ggplot2)

setwd("~/../Desktop/")

DBI::dbConnect(RPostgreSQL::postgresqlImportFile(), "data_raw/dumpsql")

pipe<-dbConnect(RPostgres::Postgres(),dbname = "postgres",
                              host = "localhost",
                              port = 5432,
                              user = "postgres",
                              pass=p)

wave2<-dbConnect(RPostgres::Postgres(),dbname = "wavetwo",
                host = "localhost",
                port = 5432,
                user = "postgres",
                pass=p)


dbListTables(wave2)

posts<-dbGetQuery(wave2,"select * from posts")
users<-dbGetQuery(wave2,"select * from users")
usertable <- users %>% select("id","title")
datetable <- posts %>% select("user_id","created_at")
colnames(datetable)[1]<-"id"
posts %>% 
  full_join(users,by=c("id")) %>% 
  View()


View(posts)
colnames(posts)


# approximation using dplyr and jitter

basic<-select(as.data.frame(createDate),created_at) %>%
  mutate(day=format(created_at,"%m-%d"))%>%
  mutate(ts=strftime(created_at,format = "%H:%M:%S")) %>%
  mutate(hr=strftime(created_at,format="%H")) 

  basic %>% ggplot(aes(day,hr))+
  geom_point(position = "jitter",alpha = 0.3)+
  labs(title = "Distribution of posts",
       subtitle = "Entries on Discourse server across time\n\nNote: plotting via jitter, less precise. Not for publication.",
       caption = "jkant@bu.edu")

# grid 

basic %B% ggplot(aes(day,hr))+
  geom_point(alpha = 0.1)+
  labs(title = "Distribution of posts",
       subtitle = "Entries on Discourse server across time\n\nNote: darker points represent largest cluster of posts per hour",
       caption = "jkant@bu.edu")

#  color code by participant type

datetable %>% 
  full_join(usertable,by=c("id")) %>% 
  filter(title %in% c("TNB","Caregivers","Siblings")) %>%
  mutate(day=format(created_at,"%m-%d"))%>%
  mutate(ts=strftime(created_at,format = "%H:%M:%S")) %>%
  mutate(hr=strftime(created_at,format="%H")) %>%
  ggplot(aes(day,hr,color=title))+
  geom_point(position = "jitter")+
  labs(title = "Distribution of posts",
       subtitle = "Entries on Discourse server across time\n\nNote: plotting via jitter, less precise. Not for publication.",
       caption = "jkant@bu.edu")

# using facet grid

datetable %>% 
  full_join(usertable,by=c("id")) %>% 
  filter(title %in% c("TNB","Caregivers","Siblings")) %>%
  filter(is.na(created_at)==FALSE) %>%
  mutate(day=format(created_at,"%m-%d"))%>%
  mutate(ts=strftime(created_at,format = "%H:%M:%S")) %>%
  mutate(hr=strftime(created_at,format="%H")) %>%
  ggplot(aes(day,hr,color=title))+
  geom_point(na.rm = TRUE)+
  labs(title = "Distribution of posts",
       subtitle = "Entries on Discourse server across time",
       caption = "jkant@bu.edu")+
  facet_grid(title~.)




# geom_points as centroids, stratified by participant type

png(filename = "centroids.png",
    width = 1000,
    height = 800)

datetable %>% 
  full_join(usertable,by=c("id")) %>% 
  filter(title %in% c("TNB","Caregivers","Siblings")) %>%
  filter(is.na(created_at)==FALSE) %>%
  mutate(day=format(created_at,"%m-%d"))%>%
  mutate(ts=strftime(created_at,format = "%H:%M:%S")) %>%
  mutate(hr=strftime(created_at,format="%H")) %>%
  group_by(hr,title,day) %>%
  mutate(n=n()) %>%
  ggplot(aes(day,hr,color=title,size=n))+
  scale_size(guide=FALSE)+
  theme(legend.position="bottom",legend.title = element_blank())+
  geom_point(na.rm = TRUE)+
  labs(title = "Distribution of posts",
       subtitle = "Entries on Discourse server across time, Wave 1",
       caption = "jkant@bu.edu")+
  ylab("Hour of day")+
  xlab("Date of post")+
  facet_grid(.~title)

dev.off()

# facet by participant type, wave

png(filename = "centroids2.png",
    width = 1000,
    height = 800)

datetable %>% 

  full_join(usertable,by=c("id")) %>% 
  filter(title %in% c("TNB","Caregivers","Siblings")) %>%
  filter(is.na(created_at)==FALSE) %>%
  mutate(day=format(created_at,"%m-%d"))%>%
  mutate(ts=strftime(created_at,format = "%H:%M:%S")) %>%
  mutate(hr=strftime(created_at,format="%H")) %>%
  mutate(wave=
           case_when(
             day < "11-03" ~ "Pre-Election",
             day > "11-04" ~ "Post-Election"
             )
         ) %>%
  filter(wave!="") %>%
  group_by(hr,title,day) %>%
  mutate(n=n()) %>%
  ggplot(aes(day,hr,color=title,size=n))+
  scale_size(guide=FALSE)+
  theme(legend.position="bottom",legend.title = element_blank())+
  geom_point(na.rm = TRUE)+
  labs(title = "Distribution of posts",
       subtitle = "Entries on Discourse server across time",
       caption = "jkant@bu.edu")+
  ylab("Hour of day")+
  xlab("Date of post")+
  geom_vline(xintercept = 5.5,linetype="dashed")+
  facet_grid(wave~title)

  dev.off()

  