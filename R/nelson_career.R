# nelson's rise

library(tidyverse)
library(vistime)
library(ggimage)
library(sysfonts)
library(showtext)


#sysfonts::font_add_google("Libre Caslon Text")
showtext::showtext_auto()

nelson_raw <- tribble(
  ~"Rank", ~"Days", ~"color",
  "Midshipman",2178, "tan",
  "Lieutenant",619, "darkgrey",
  "Commander",193, "tomato",
  "Captain",6464, "gold",
  "Rear Admiral, Blue",724, "royalblue",
  "Rear Admiral, Red",687, "red2",
  "Vice Admiral, Blue",1208, "royalblue",
  "Vice Admiral, White",546,"white",
) %>%
  mutate(Rank = as_factor(Rank))


trafalgar_date <- as.Date("1805-10-21")
nelson <- nelson_raw %>%
  mutate(end = cumsum(Days)) %>%
  mutate(start = end - Days,.before = "end") %>%
  mutate(end_date = trafalgar_date - max(end) + end) %>%
  mutate(start_date = end_date - Days,.before = "end") %>%
  select(-start,-end) %>%
  mutate(Days = as.character(Days))

nelson$Days[4] <- paste(nelson$Days[4],"Days")

nelson_cols = nelson$color
names(nelson_cols) <- nelson$Rank

nelson <-
 nelson %>% arrange(desc(end_date))

# add events
events <- tribble(
  ~"Rank", ~"Days", ~"color",~"start_date",~"end_date",
  "Battle","Corsica","red",as.Date("1794-07-12"),as.Date("1794-07-12"),
  "Battle","Genoa","red",as.Date("1795-04-14"),as.Date("1795-04-14"),
  "Battle","St. Vincent","red",as.Date("1797-02-14"),as.Date("1797-02-14"),
  "Battle","Tenerife","red",as.Date("1797-07-15"),as.Date("1797-07-15"),
  "Battle","Nile","red",as.Date("1798-07-01"),as.Date("1798-07-01"),
  "Battle","Copenhagen","red",as.Date("1801-04-02"),as.Date("1801-04-02"),
  "Battle","Trafalgar","red",trafalgar_date,trafalgar_date,
  "Command", "Little Lucy", "green",as.Date("1777-06-15"),as.Date("1777-06-15"),
  "Command", "Badger", "green",as.Date("1778-06-15"),as.Date("1778-06-15"),
  "Command", "Hinchbrook","green",as.Date("1779-12-23"),as.Date("1779-12-23"),
  "Command", "Abermarle","green",as.Date("1781-10-23"),as.Date("1781-10-23"),
  "Command", "Boreas","green",as.Date("1784-01-15"),as.Date("1784-01-15"),
  "Command", "Agamemnon","green",as.Date("1793-01-15"),as.Date("1793-01-15"),
  "Command", "Captain","green",as.Date("1796-06-15"),as.Date("1796-06-15"),
  "Command", "Theseus","green",as.Date("1797-06-15"),as.Date("1797-06-15"),
  "Command", "Vanguard","green",as.Date("1798-03-28"),as.Date("1798-03-28"),
  "Command", "Elephant","green",as.Date("1801-04-02"),as.Date("1801-04-02"),
  "Command", "Victory","green",as.Date("1803-07-02"),as.Date("1803-07-02"),
  #  "Theatre", "Atlantic","darkgreen",as.Date("1771-01-15"),as.Date("1773-01-15"),
#  "Theatre", "Arctic","darkgreen",as.Date("1773-01-15"),as.Date("1773-12-15"),
#  "Theatre", "West Indies","darkgreen",as.Date("1773-01-15"),as.Date("1776-01-15"),
#  "Theatre", "East Indies","darkgreen",as.Date("1779-01-15"),as.Date("1780-01-15")

)

nelson <- bind_rows(nelson,events)

# nelson %>%
#   ggplot(aes(Rank,end,fill=Rank))+ geom_col() +
#   scale_fill_manual(values = nelson_cols) +
#   scale_y_continuous(sec_axis = sec_axis(~. + 100,name="Date")) +
#   geom_col(aes(Rank,start),fill="lightblue") +
#   geom_text(aes(label = Days),nudge_y= .2) +
#   theme_minimal() +
#   theme(legend.position = "none") +
#   theme(panel.grid = element_line(linetype = "blank")) +
#   theme(plot.background = element_rect(fill="lightblue")) +
#   coord_flip()


p <- nelson %>%
  gg_vistime(col.group = "Rank",
             col.event = "Days",
           col.start = "start_date",
           col.end = "end_date",
           optimize_y = FALSE,
           linewidth = 6) +
  geom_vline(xintercept = as.numeric(as.POSIXct("1794-07-12")), color = "red", size = .5) +
  annotate("text",label="Loses Eye",
           x = as.POSIXct("1792-07-12"), y= 36) +
  geom_vline(xintercept = as.numeric(as.POSIXct("1797-07-22")), color = "red", size = .5) +
  annotate("text",label="Loses Arm",
           x = as.POSIXct("1795-07-12"), y= 35) +
  geom_vline(xintercept = as.numeric(as.POSIXct("1805-10-21")), color = "red", size = .5) +
  annotate("text",label="Loses Life",
           x = as.POSIXct("1803-07-12"), y= 36) +
  theme(legend.position = "none") +
  theme(axis.text = element_text(family = "Libre Caslon Text",size = 15)) +
  theme(title = element_text(family = "Libre Caslon Text",size = 20)) +
  #  theme(axis.line = element_line(color = "black")) +
#  theme(panel.grid = element_line(linetype = "blank")) +
#  theme(plot.background = element_rect(fill="lightblue")) +
  theme(panel.background = element_rect(fill="lightgrey")) +
  labs(title = '"Pass the Salt"',
       subtitle = "A Selected Timeline of Admiral Nelson's Career",
       caption = "Viz: Art Steinmetz using R,ggplot2 and vistime")

p
ggimage::ggbackground(p, "img/surf_bg.jpg")
