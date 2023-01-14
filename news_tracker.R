
# jessica.kant@childrens.harvard.edu

 install.packages("googlesheets4")

library(ggplot2)
library(dplyr)
library(png)


# clean OAuth tokens and authenticate

detach(package:googlesheets4)
library(googlesheets4)
# googlesheets4::gs4_auth()

# set global variables

targsheet <- "https://docs.google.com/spreadsheets/d/1dSMwRLOJ1HbYixm7RzS_4Q8Uu1aq3326auxBkJ5g-JY/edit?usp=sharing"
headers   <- as.data.frame(cbind("EntryPublished","EntryTitle","EntryURL","EntryContent","FeedTitle","FeedURL","keyword","region")) 

# googlesheets4::sheet_append(targsheet,as.vector(headers),sheet =1) # inserts headers into blank sheet, only run first time

read_sheet(targsheet) -> dat
head(dat)

getthatbread<-function(){
  read_sheet(targsheet) -> dat
  str_extract(dat$EntryPublished,pattern = "[a-zA-Z]+\\s[0-9]+\\,\\s20[0-9]+") -> dat$the_day
  dat %>%
    mutate(article_type = 
             case_when(
                grepl(("sport|athletic|athlete|competition"      ),  dat$EntryContent) == TRUE ~"sport",
                grepl(("prison|prisoner|inmate"                  ),  dat$EntryContent) == TRUE ~"prison",
                grepl(("school|pupil|principle|teacher|teach"    ),  dat$EntryContent) == TRUE ~"school"
                )
           ) %>%
    rename(timestamp = EntryPublished) %>%
    mutate(the_day=as.Date(mdy(dat$the_day))) %>%
    group_by(the_day,region) %>%
    mutate(ct=n()) ->> d_set

    d_set %>%
    ggplot()+
        geom_line(aes(x=the_day,y=ct,color=region, colour="daily")) +
        geom_point(aes(x=the_day,y=ct,color=region, colour="daily")) +
        labs(title = "Articles about trans people in US + UK news media",
             subtitle = "https://tech.lgbt/@jessdkant",
             caption=paste("updated",Sys.time()))+
        xlab(element_blank())+
        ylab("number of articles")+
              theme_classic()+
        theme(legend.position = "bottom") 
    } 

getthatbread()




d_set %>% select(region,article_type,EntryContent) %>% View()
