# twitter poll
# jkant@bu.edu
# github.com/jessicakay

# special thanks to the twitteR library for R, by Jeff Gentry
# http://geoffjentry.hexdump.org/twitteR.pdf

install.packages("twitteR")

# example using httr library

library(httr)

GET("https://twitter.com/jessdkant/status/1298637900200833026")

install.packages("rtweet")

total_N<-214
n_yes_q1<-.836*total_N
n_no_q1<-.164*total_N



library(MASS)
parcoord(fio[,c(3,3)])

install.packages("plotly")
library(plotly)


plot_ly(
  type="funnel",
  y=c("yes","no"),
  x=c(100,50,25,0)) %>%
    add_trace(type="funnel",
              y="yes","no")

plot_ly() %>%
  add_trace(type="funnel",
            y=c("yes","no","yes","no"),
            x=c(100,20,10,3))


