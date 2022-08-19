# Technical Assessment: Economist Figure

## The Task

Recreate this figure: ![alt
text](http://cdn.static-economist.com/sites/default/files/imagecache/1872-width/20170415_WOC921.png)

The figure was published online
[here](https://www.economist.com/graphic-detail/2017/04/06/a-global-decline-in-smoking-masks-regional-variations-between-the-sexes)
by The Economist.

## Reproducing the Economist figure

    #Loading the libraries
    library(tidyverse)

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
    ## ✔ tibble  3.1.7     ✔ dplyr   1.0.9
    ## ✔ tidyr   1.2.0     ✔ stringr 1.4.0
    ## ✔ readr   2.1.2     ✔ forcats 0.5.1
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

    library(dplyr)
    library(ggrepel)
    library(ggthemes)

    #Reading the data
    smoking_prevalence <- read.csv("./IHME_GBD_2015_SMOKING_PREVALENCE_1980_2015_1/IHME_GBD_2015_SMOKING_PREVALENCE_1980_2015_Y2017M04D05.CSV")

    #Understanding the data
    #How many group ids?
    length(unique(smoking_prevalence$age_group_id))

    ## [1] 17

    #How many locations?
    length(unique(smoking_prevalence$location_name))

    ## [1] 231

    #How many years?
    length(unique(smoking_prevalence$year_id))

    ## [1] 36

    #keeping relevant features
    smoking_prev_filtered <- smoking_prevalence %>% select(location_name, sex, age_group_id, year_id, metric, mean) %>% filter (age_group_id == 27,sex %in% c("Male", "Female"), year_id == "1990" | year_id == "2015", metric == "Percent") %>% select(-c(metric, age_group_id)) %>% rename(Mean = mean, Sex = sex, Year =year_id, Location = location_name )

    # Calculating point change percentage
    # point_change_percentage = (New Number - Original Number) * 100
    smoking_prev_PerChange = smoking_prev_filtered %>%
      group_by(Location,Sex,Year) %>% summarise(my_Mean = mean(Mean)) %>%
      mutate(First = head(my_Mean,1),
             PercChange = case_when(my_Mean != First ~ (my_Mean - First) * 100,
                     TRUE ~ First* 100))%>%
      filter(Year == 2015)%>%select(-First)%>%
      select(Location, my_Mean,PercChange)

    ## `summarise()` has grouped output by 'Location', 'Sex'. You can override using
    ## the `.groups` argument.
    ## Adding missing grouping variables: `Sex`

    # Generating the figure
    smoking_prev_w <- pivot_wider(smoking_prev_PerChange, 
                                   id_cols = Location, 
                                   names_from = Sex, values_from = PercChange,
                                   values_fn = ~mean(.x))  %>% mutate(Group = 
                                   case_when(Male >0 & Female > 0 ~ "G1",
                                   Male < 0 & Female > 0 ~ "G2",
                                   Male < 0 & Female < 0 ~ "G3",
                                   Male > 0 & Female < 0 ~ "G4"))

    p2 <- ggplot(smoking_prev_w, aes (x=Male, y=Female,colour = Group)) + geom_point(size = 3, alpha = 0.9) + scale_color_manual(values = c("tomato4", "deepskyblue3", "lightskyblue1", "tomato2"))

    theme_Economist <- theme(panel.border = element_blank(),axis.ticks = element_blank(),axis.title = element_text(size = 10),panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"), legend.position="none",plot.caption = element_text(hjust = 0,size=7),plot.caption.position = "plot")

    p3 <- p2 +
      theme_bw()+
      theme_Economist+
      geom_text_repel(aes(label = Location), data = subset(smoking_prev_w, Location %in% c("South Korea", "Turkey", "Belarus", "Portugal", "Greece", "Bulgaria","Russia", "Kuwait", "Timor-Leste", "Chile", "Macedonia", "Cyprus", "Indonesia", "Azerbaijan","Montenegro", "Saudi Arabia", "Tonga", "India", "Japan", "China", "France", "Britain", "United States","Nepal", "Brazil", "Sweden", "Canada", "Norway", "Iceland", "Denmark")),col = "#6F7378", size = 3,nudge_x = -0.05)+
      geom_hline(yintercept = 0, size = 0.5) +
      geom_vline(xintercept = 0, size = 0.5)+
      annotate("text", label = "Female increase\nmale decrease", col = "deepskyblue3", size = 3.5, fontface = 2, x = -21, y = 8) +
      annotate("text", label = "Female decrease\nmale decrease", col = "deepskyblue3", size = 3.5,fontface = 2, x = -21, y = -15) +
      annotate("text", label = "Female decrease\nmale increase", col = "tomato2", size = 3.5, fontface = 2, x = 7, y = -17) +
      annotate("text", label = "Female increase\nmale increase", col = "tomato4", size = 3.5, fontface = 2, x = 7, y = 7)+
       scale_x_continuous(breaks = seq(from = -25, to = 10, by = 5)) + 
      scale_y_continuous(breaks = seq(from = -20, to = 10, by = 5)) +
    labs(x = expression(italic("Change in male rate")), y = expression(italic("Change in female rate")), title = "What a drag", subtitle = "Daily smoking prevalence, 1990-2015, percentage-point change",caption = expression(paste("Source: smoking prevalence and attributable disease burden in 195 countries and territories,\n1990-2015: a systematic analysis from the Global Burden of Disease Study 2015, the Lancet, 2017"))) 
     
      
    print(p3)

![](TheEconomistFigure_files/figure-markdown_strict/unnamed-chunk-8-1.png)

    ggsave("./TheEconomist_figure_task.jpg",last_plot(),width=190, height = 170,units = "mm",dpi = 300)

## Submission Checklist

Submit the following: 1. A link to your code (as a GitHub repository)
and the finished product. Specifically we want to see: - How you
obtained the data (it doesn’t need to be the original data but it should
be similar!) \

# 1st method (failed) 
I went to the **Supplementary appendix** of the Lancet publication and went to Data sources section. A
complete list of sources is available from the [GBD 2015 Data Input
Sources Tool](http://ghdx.healthdata.org/gbd-2015/data-input-sources).
These data are no longer available on the GHDx.

# 2nd method
I googled “Smoking prevalence and attributable disease burden” which is
part of the title of the
[Lancet](https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(17)30819-X/fulltext)
and went through the links. The 4th result was this
[link](https://ghdx.healthdata.org/record/ihme-data/gbd-2015-smoking-prevalence-1980-2015)
then went to files and downloaded this
[zip](https://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_GBD_2015_SMOKING_PREVALENCE_1980_2015_1.zip).

1.  Answers to the following:
    -   What was the most challenging part of this exercise? The most
        challenging part of the exercise was figuring out which columns
        and age groups are the relevant ones to reproduce the plot.
        Moreover, it was really challenging for me to find the formula
        of the percentage-point change.

    -   What took you the longest? The longest part was figuring out how
        to calculate the percentage-point change and styling the plot.
        For the percentage-point change I tried two formulas before
        finding the right one that reproduce the figure. The first one
        was :
        (*N**e**w**N**u**m**b**e**r*−*O**r**i**g**i**n**a**l**N**u**m**b**e**r*)/*O**r**i**g**i**n**a**l**N**u**m**b**e**r* \* 100
        Which can be written as:
        ((*N**e**w**N**u**m**b**e**r*−*O**r**i**g**i**n**a**l**N**u**m**b**e**r*)−1) \* 100

These are the pages I checked:
[page1](https://stackoverflow.com/questions/64977496/calculate-the-percentage-change-in-r)
[page2](https://stackoverflow.com/questions/31812864/obtaining-year-on-year-percentage-change-by-group)
[page3](https://stackoverflow.com/questions/61169183/percentage-change-in-values-in-r)
[page4](https://stackoverflow.com/questions/48196552/calculate-percentage-change-in-r-using-dplyr)
[page5](https://stackoverflow.com/questions/31352685/how-can-i-calculate-the-percentage-change-within-a-group-for-multiple-columns-in)

    - What did you enjoy the most?
    Seeing that once you get the right percentage-point change you get almost the same plot.I enjoyed styling the plot and using the  [R color palette](https://bookdown.org/hneth/ds4psy/D-3-apx-colors-basics.html).

    - What did you enjoy the least?
    I got frustrated when I didn't find the solution at first and so I had to go back and forth many times. 
