
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
  cat("\nlast 5 entries: \n\n") ; tail(ds %>% arrange(EntryPublished),n=10)

  
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
  facet_grid(keyword~region) -> kw


ds %>% 
  mutate(topic=case_when(
  str_detect(EntryContent,"(?i)sport|(?i)athlet|(?i)competiti|(?i)swim") == TRUE ~ "sports",
  str_detect(EntryContent,"(?i)restroom|(?i)bathroom|(?i)locker|(?i)naked")== TRUE ~ "bathrooms",
  str_detect(EntryContent,"(?i)school|(?i)educat|(?i)universit|(?i)college") == TRUE ~ "education",
  str_detect(EntryContent,"(?i)detrans|(?i)desist|(?i)de-trans") == TRUE ~ "detransition",
  str_detect(EntryContent,"(?i)mental\\s(?i)ilness|(?i)disorder|(?i)therapy|(?i)psychiatr|(?i)psychology") == TRUE ~ "therapy",
  str_detect(EntryContent,"(?i)supreme|(?i)court|(?i)discriminat|(?i)lawsuit|(?i)legal") == TRUE ~ "courts",
  str_detect(EntryContent,"(?i)actor|(?i)film|(?i)movie|(?i)television|(?i)author|(?i)actress") == TRUE ~ "entertainment",
  str_detect(EntryContent,"(?i)legislat|(?i)bill|(?i)lawmaker|(?i)reform|(?i)senate|(?i)ban") == TRUE ~ "legislation",
  str_detect(EntryContent,"(?i)medical|(?i)healthcare|(?i)hormone|(?i)medication|(?i)surgery|(?i)physician") == TRUE ~ "healthcare")) %>% 
mutate(topic=case_when(
  str_detect(EntryTitle,"(?i)sport|(?i)athlet|(?i)competiti|(?i)swim") == TRUE ~ "sports",
  str_detect(EntryTitle,"(?i)restroom|(?i)bathroom|(?i)locker|(?i)naked")== TRUE ~ "bathrooms",
  str_detect(EntryTitle,"(?i)school|(?i)educat|(?i)universit|(?i)college") == TRUE ~ "education",
  str_detect(EntryTitle,"(?i)detrans|(?i)desist|(?i)de-trans") == TRUE ~ "detransition",
  str_detect(EntryTitle,"(?i)mental\\s(?i)ilness|(?i)disorder|(?i)therapy|(?i)psychiatr|(?i)psychology") == TRUE ~ "therapy",
  str_detect(EntryTitle,"(?i)supreme|(?i)court|(?i)discriminat|(?i)lawsuit|(?i)legal") == TRUE ~ "courts",
  str_detect(EntryTitle,"(?i)actor|(?i)film|(?i)movie|(?i)television|(?i)author|(?i)actress") == TRUE ~ "entertainment",
  str_detect(EntryTitle,"(?i)legislat|(?i)bill|(?i)lawmaker|(?i)reform|(?i)senate|(?i)ban") == TRUE ~ "legislation",
  str_detect(EntryTitle,"(?i)medical|(?i)healthcare|(?i)hormone|(?i)medication|(?i)surgery|(?i)physician") == TRUE ~ "healthcare")) %>%  
  ggplot()+
  geom_bar(aes(x=the_day, fill=topic), position="fill")+
  facet_grid(keyword~region)+
  theme(legend.position = "bottom")+
  theme_bw()+
  scale_fill_discrete(name="keyword")+
  labs(y="proportion of articles",x=element_blank()) -> bottom


gridExtra::grid.arrange(kw,bottom)


# experimental NLP section, keywords used to further tag items

  ds$tag_sports <- ifelse(grepl("(?i)sport|(?i)athletic|(?i)athlete|(?i)competition", ds$EntryContent),1,0)
  ds$tag_leg    <- ifelse(grepl("(?i)bill|(?i)legislat",                              ds$EntryContent),1,0)
  ds$tag_school <- ifelse(grepl("(?i)school|(?i)educat|(?i)universit",                ds$EntryContent),1,0)
  ds$tag_sglsex <- ifelse(grepl("(?i)women\\sonly|(?i)single-sex|(?i)sex-based",      ds$EntryContent),1,0)


colIDs <- names(select(ds, contains("tag_")))
  
ds[which(ds$tag_leg==1),] %>%
  select(tag_leg, region,the_day) %>% 
  ggplot()+geom_bar(aes(x=the_day,fill=region))+facet_grid(.~region)


ds %>% select(contains("tag_"), region,the_day) %>% ggplot(aes(x=the_day))+ 
  geom_bar(aes(fill=region))



ds %>% tidyr::unite("tags",colIDs, sep = ",", remove = FALSE) %>% select(tags) %>% View()
  

gsub("<b>|</b>|&nbsp;|;|&#39;|(|)|\\...|&quo","",paste(ds$EntryContent,collapse = " ")) -> x


TermDocumentMatrix(Corpus(VectorSource(x))) -> term_matrix
findFreqTerms(term_matrix,lowfreq = 50)
findAssocs(x,term="transgender")
