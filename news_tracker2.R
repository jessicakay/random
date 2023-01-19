
# jkant@bu.edu

 install.packages("googlesheets4")

library(ggplot2)
library(dplyr)
library(png)
library(stringr)
library(lubridate)
library(googlesheets4)
 
# clean OAuth tokens and authenticate
# detach(package:googlesheets4)
# googlesheets4::gs4_auth()
 
# headers   <- as.data.frame(cbind("EntryPublished","EntryTitle","EntryURL","EntryContent","FeedTitle","FeedURL","keyword","region"))
# googlesheets4::sheet_append(tsheetall,as.vector(headers),sheet =1) # inserts headers into blank sheet, only run first time
 
# set global variables

targsheet <- "https://docs.google.com/spreadsheets/d/1dSMwRLOJ1HbYixm7RzS_4Q8Uu1aq3326auxBkJ5g-JY/edit?usp=sharing"
tsheetall <- "https://docs.google.com/spreadsheets/d/1HW8m7xKLmCebdSa0RbmBdJkKaD3SZPc8XMQW-Q680FQ/edit?usp=sharing"


  read_sheet(targsheet) -> dat
  read_sheet(tsheetall) -> dat2
  ds <- as.data.frame(rbind(dat,dat2)) %>% as_tibble()
  cat("\nlast 5 entries: \n\n")
  tail(ds %>% arrange(desc(EntryPublished)),n=10)

# basic plot

ds %>%
  mutate(theday=str_extract(ds$EntryPublished,pattern = "[a-zA-Z]+\\s[0-9]+\\,\\s20[0-9]+")) %>%
  rename(timestamp = EntryPublished) %>%
  mutate(the_day=as.Date(mdy(theday))) ->ds
  
# ds %>% group_by(the_day,region) %>% mutate(ct=n()) 

ds %>%
  group_by(the_day,region,keyword) %>%
  mutate(ct=n()) -> ds

ds %>%
  ggplot()+
  geom_line(aes(x=the_day,y=ct,color=region, colour="daily")) +
  labs(title = "Articles about trans people in US + UK news media",
       subtitle = "https://tech.lgbt/@jessdkant",
       caption=paste("updated",Sys.time()))+
  xlab(element_blank())+
  ylab("number of articles")+
  theme_bw()+
  theme(legend.position = "bottom")

# stratify by keyword


ds %>% 
  group_by(the_day,region,keyword) %>%
  mutate(ct=n()) %>%
  
  ggplot()+
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


ds %>%
  mutate(theday=str_extract(ds$EntryPublished,pattern = "[a-zA-Z]+\\s[0-9]+\\,\\s20[0-9]+")) %>%
  rename(timestamp = EntryPublished) %>%
  mutate(the_day=as.Date(mdy(theday))) %>%
  group_by(the_day,region,keyword) %>%
  mutate(ct=n()) %>%
  ggplot()+
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
  str_detect(EntryContent,"(?i)sport|(?i)athlet") == TRUE ~ ("sports"),
  str_detect(EntryContent,"(?i)school|(?i)educat|(?i)universit") == TRUE ~ list("education"),
  str_detect(EntryContent,"(?i)restroom|(?i)bathroom|(?i)locker")== TRUE ~ list("bathrooms"),
  str_detect(EntryContent,"(?i)legislat|(?i)bill") == TRUE ~ list("legislation"))) %>%
  select(EntryContent,topic) %>% View()


dat %>%
mutate(article_type = 
         case_when(
           grepl(("sport|athletic|athlete|competition"      ),  dat$EntryContent) == TRUE ~"sport",
           grepl(("prison|prisoner|inmate"                  ),  dat$EntryContent) == TRUE ~"prison",
           grepl(("school|pupil|principle|teacher|teach"    ),  dat$EntryContent) == TRUE ~"school"
         )) %>% View()
