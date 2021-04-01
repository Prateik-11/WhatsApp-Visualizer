library(tidyverse)
Chat <- read.csv("~/R/Data/WhatsApp Chat with Amarthya Dutta Gupta.txt", header=FALSE)
Chat <- Chat[!grepl("changed their phone number",Chat$V2,fixed=TRUE),]
Chat <- as_tibble(Chat[-1, ]) %>%
  separate(V2, into = c("Time", "Name"), sep = " - ") %>%
  separate(Name, into = c("Name", "Message"), sep = ": ") %>%
  rename("Date" = "V1")
Chat$Date <- parse_date(Chat$Date, "%m/%d/%y")
Chat$Time <- parse_time(Chat$Time, "%I:%M %p")
Chat <- Chat[!(is.na(Chat$Date)), ]
ggplot(Chat, aes(x = `Date`, y = stat(count))) + geom_histogram(aes(fill =
                                                                      Name), position = "dodge", binwidth = 30) + labs(y = NULL, x = NULL, title = "Messages Per Month")
ggplot(Chat) + geom_histogram(aes(x = Time, y = stat(count), fill = Name), bins = 96) +
  labs(y = NULL, x = NULL, title = "Distribution over 24 Hours")
ggplot(Chat, aes(fill = `Name`)) + geom_bar(aes(x = "", y = stat(count)), stat = "count") +
  coord_polar(theta = "y") + labs(
    x = NULL,
    y = NULL,
    subtitle = paste0(
      "",
      count(Chat, Name)[1, 2],
      ":",
      count(Chat, Name)[2, 2],
      " | ",
      round(100 * count(Chat, Name)[1, 2] / count(Chat)),
      "%:",
      round(100 * count(Chat, Name)[2, 2] / count(Chat)),
      "%"
    ) ,
    title = paste0("Proportion of Messages [Total:", count(Chat), "]")
  ) + theme(
    panel.background = element_blank(),
    legend.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

