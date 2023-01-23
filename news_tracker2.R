
# jkant@bu.edu

 install.packages("googlesheets4")
  
  library(ggplot2)
  library(dplyr)
  library(png)
  library(stringr)
  library(lubridate)
  library(googlesheets4)
 
# clean OAuth tokens and authenticate
#
#   detach(package:googlesheets4)
#   googlesheets4::gs4_auth()
#   headers <- c("EntryPublished","EntryTitle","EntryURL","EntryContent","FeedTitle","FeedURL","keyword","region")
#   headers   <- as.data.frame(cbind(headers))
#   googlesheets4::sheet_append(tsheetall,as.vector(headers),sheet =1) # inserts headers into blank sheet, only run first time
#
# set global variables

targsheet <- "https://docs.google.com/spreadsheets/d/1dSMwRLOJ1HbYixm7RzS_4Q8Uu1aq3326auxBkJ5g-JY/edit?usp=sharing"
tsheetall <- "https://docs.google.com/spreadsheets/d/1HW8m7xKLmCebdSa0RbmBdJkKaD3SZPc8XMQW-Q680FQ/edit?usp=sharing"

  read_sheet(targsheet) -> dat
  read_sheet(tsheetall) -> dat2
  ds <- as.data.frame(rbind(dat,dat2)) %>% as_tibble()
  ds %>%
    mutate(theday=str_extract(EntryPublished,pattern = "[a-zA-Z]+\\s[0-9]+\\,\\s20[0-9]+")) %>%
    mutate(the_day=as.Date(mdy(theday))) ->ds
  cat("\nlast 5 entries: \n\n") ; tail(ds %>% arrange(desc(EntryPublished)),n=10)

  
# basic plot

ds %>% group_by(the_day,region) %>% mutate(ct=n()) %>% ggplot()+
  geom_line(aes(x=the_day,y=ct,color=region, colour="daily")) +
  labs(title = "Articles about trans people in US + UK news media",
       subtitle = "https://tech.lgbt/@jessdkant",
       caption=paste("updated",Sys.time()))+
  xlab(element_blank())+
  ylab("number of articles")+
  theme_bw()+
  theme(legend.position = "bottom")


# stratify by keyword

ds %>% group_by(the_day,region,keyword) %>% mutate(ct=n()) %>% ggplot()+
  geom_line(aes(x=the_day,y=ct,color=region, colour="daily"))+
  geom_point(aes(x=the_day,y=ct,color=region, colour="daily"))+
  labs(title = "Articles about trans people in US + UK news media",
       subtitle = "https://tech.lgbt/@jessdkant",
       caption=paste("updated",Sys.time()))+
  xlab(element_blank())+
  ylab("number of articles")+
  theme_bw()+
  theme(legend.position = "bottom")+
  facet_grid(keyword~region)


# experimental NLP section, keywords used to further tag items

dat %>% mutate(topic=case_when(
  str_detect(EntryContent,"(?i)sport|(?i)athlet") == TRUE ~ "sports",
  str_detect(EntryContent,"(?i)school|(?i)educat|(?i)universit") == TRUE ~ list("education"),
  str_detect(EntryContent,"(?i)restroom|(?i)bathroom|(?i)locker")== TRUE ~ list("bathrooms"),
  str_detect(EntryContent,"(?i)legislat|(?i)bill") == TRUE ~ list("legislation"))) %>%
  select(EntryContent,topic) %>% View()

  ds$tag_sports <- ifelse(grepl("(?i)sport|(?i)athletic|(?i)athlete|(?i)competition", ds$EntryContent),1,0)
  ds$tag_leg    <- ifelse(grepl("(?i)bill|(?i)legislat",                              ds$EntryContent),1,0)
  ds$tag_school <- ifelse(grepl("(?i)school|(?i)educat|(?i)universit",                ds$EntryContent),1,0)
  ds$tag_sglsex <- ifelse(grepl("(?i)women\\sonly|(?i)single-sex|(?i)sex-based",      ds$EntryContent),1,0)

  names(select(ds, contains("tag_"))) -> colIDS # find all tags


ds %>% select(all_of(colIDS), region,the_day) %>%
  group_by(the_day,region )%>%
  mutate(tag_count = n()) %>% 
  as_tibble() -> thecount 

ds %>% select(all_of(colIDS), region, the_day) %>% group_by(the_day,region)%>% summarise(tag_count = n()) %>%
ggplot()+
    geom_count(aes(x=the_day,y=tag_count))

