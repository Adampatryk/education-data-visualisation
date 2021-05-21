library(ggplot2)
library(tidyr)
library(dplyr)
library(GGally)
library(plotly)

#LOAD COMMON DATA ONCE (only needed once, this takes a while)
loadCommon <- FALSE

#Pick the question and part to run
  q1Part1 <- FALSE
  q1Part2 <- FALSE

  q2Part1 <- FALSE
  q2Part2 <- FALSE

  q3Part1 <- FALSE
  q3Part2 <- TRUE
  
if (loadCommon == TRUE){

  #This is the cleaning and transformations common to all questions answered
  
  #--------------------------------------------------------------------------------------------
  ##########
  #students#
  ##########
  students <- read.csv("C:/Users/Adam/Desktop/FIV/TFTF Anonymised DB/anon_students.csv") %>%
    select(gender, yeargroup, school_id, student_id) %>%
    #Make all years the same format
    mutate(yeargroup = as.numeric(gsub("Year", "", yeargroup))) %>%
    #Omit NAs
    na.omit(yeargroup) %>%
    #Change id to be school_student id
    mutate(student_school = paste(student_id, school_id, sep="-")) %>% 
    select(-student_id)

    
  
  #--------------------------------------------------------------------------------------------
  #--------------------------------------------------------------------------------------------
  #########
  #schools#
  #########
  schools <- read.csv("C:/Users/Adam/Desktop/FIV/TFTF Anonymised DB/anon_schools.csv") %>%
    select(everything()) %>%
    left_join(students, by="school_id") %>%
    group_by(school_id) %>%
    summarise(name=first(name), income=first(income), n_students=n(), type=first(type), religious=first(religious)) %>%
    filter(n_students > 10)
  
  head(schools)
  
  #--------------------------------------------------------------------------------------------
  ############
  #attendance#
  ############
  
  attendance <- read.table("C:/Users/Adam/Desktop/FIV/TFTF Anonymised DB/anon_attendance.csv", header=TRUE, quote="\"", fileEncoding="utf16", sep = ",") %>%
    select(date, mark, session, student_id, school_id) %>%
    #Create mark_school_id for joining the attendance codes
    mutate(mark_school_id = paste(mark, school_id, sep="-"))
  
  attendance_codes <- read.csv("C:/Users/Adam/Desktop/FIV/TFTF Anonymised DB/anon_attendancecodes.csv") %>%
    select(-id) %>%
    #Create mark_school_id for joining with the attendance
    mutate(mark_school_id = paste(mark, school_id, sep = "-")) %>%
    select(-mark, -school_id)
  
  attendance_codes$short_meaning_description[attendance_codes$short_meaning_description == "Approved"] <- "Present"
  attendance_codes$short_meaning_description <- factor(attendance_codes$short_meaning_description)
  
  atts <- inner_join(attendance, attendance_codes, by="mark_school_id") %>% 
    #Remove join by column
    select(-mark_school_id, -mark, -description) %>%
    filter(short_meaning_description != "Not Required", physical_meaning != "NOMARK") %>%
    group_by(school_id, student_id) %>%
    mutate(totalMarks = n()) %>%
    group_by(school_id, student_id, short_meaning_description) %>%
    mutate(markTypeCount = n()) %>%
    summarise(markTypePerc = 100*round(first(markTypeCount)/first(totalMarks), 3)) %>%
    ungroup() %>%
    spread(short_meaning_description, markTypePerc) %>%
    replace(is.na(.), 0) %>%
    mutate(student_school = paste(student_id, school_id, sep="-")) %>% 
    select(-student_id, -school_id) %>%
    inner_join(students, by="student_school") 
  
  #--------------------------------------------------------------------------------------------
  ############
  #BEHAVIOURS#
  ############
  
  #anon_staffbehaviours <- read.csv("C:/Users/Adam/Desktop/FIV/TFTF Anonymised DB/anon_staffbehaviours.csv")
  
  #Need this to get date
  behObjects <- 
    read.csv("C:/Users/Adam/Desktop/FIV/TFTF Anonymised DB/anon_behaviour.csv") %>%
    select(beh_time, behaviour_id, school_id) %>%
    mutate(school_behaviour_id = paste(school_id, behaviour_id, sep = "-")) %>%
    select(-school_id, -behaviour_id)
  
  behsJoined <- read.csv("C:/Users/Adam/Desktop/FIV/TFTF Anonymised DB/anon_studentbehaviour.csv") %>%
    mutate(school_behaviour_id = paste(school_id, behaviour_id, sep="-")) %>%
    inner_join(behObjects) %>%
    mutate(student_school = paste(student_id, school_id, sep="-")) 
  
  behs <-
    read.csv("C:/Users/Adam/Desktop/FIV/TFTF Anonymised DB/anon_studentbehaviour.csv") %>%
    mutate(student_school = paste(student_id, school_id, sep="-")) %>% 
    select(-student_id, -behaviour_id) %>%
    group_by(school_id, student_school) %>%
    summarise(summed_beh_points = sum(points), n_beh = n()) %>%
    ungroup() %>%
    select(-school_id) %>% #It will be in the student data
    inner_join(students, by="student_school") 
  
  
  #--------------------------------------------------------------------------------------------
  ##############
  #Achievements#
  ##############
  
  #anon_staffachievements <- read.csv("C:/Users/Adam/Desktop/FIV/TFTF Anonymised DB/anon_staffachievements.csv")
  
  #Need the object to fetch the date, if i want the date...
  achObjects <- 
    read.csv("C:/Users/Adam/Desktop/FIV/TFTF Anonymised DB/anon_achievement.csv") %>%
    select(achievement_id, date)
  
  #Get the individual student achivements and link to the date
  achs <- 
    read.table("C:/Users/Adam/Desktop/FIV/TFTF Anonymised DB/anon_studentachievement.csv", header=TRUE, quote="\"", fileEncoding="utf16", sep = ",") %>%
    select(achievement_id, points, student_id, school_id) %>%
    #inner_join(achObjects, by="achievement_id") %>%  #Use if I want the date
    mutate(student_school = paste(student_id, school_id, sep="-")) %>% 
    select(-student_id, -achievement_id) %>%
    group_by(school_id, student_school) %>%
    summarise(summed_ach_points = sum(points), n_ach = n()) %>%
    ungroup() %>%
    select(-school_id) %>% #It will be in the student data
    inner_join(students, by="student_school")
  
  
  #--------------------------------------------------------------------------------------------
  ############
  #exclusions#
  ############
  
  exclusions <- read.csv("C:/Users/Adam/Desktop/FIV/TFTF Anonymised DB/anon_studentexclusions.csv") %>%
    select(exclusion_days, start_date, student_id, school_id, exclusion_type, exclusion_reason, start_session) %>%
    filter(!start_session=="")
}
  

#---------------------------------------------------------------------------------------------------------
#Q1 Is attendance correlated to behaviour?


exclusionsPerStudent <- exclusions %>%
  mutate(student_school = paste(student_id, school_id, sep="-")) %>%
  group_by(student_school) %>%
  summarise(total_exclusion_days=sum(exclusion_days), n_exclusions= n())

studentsWithStats <- 
  atts %>% 
  select(-school_id,-gender,-yeargroup) %>%
  inner_join(achs, by="student_school") %>%
  select(-school_id,-gender,-yeargroup) %>%
  inner_join(behs, by="student_school") %>%
  left_join(exclusionsPerStudent, by="student_school") %>%
  select(-student_school)


head(studentsWithStats)


#Box plot of achievements/behaviours vs percentage attendance by gender
studentsWithStatsFiltered <- filter(studentsWithStats, Present > 50)
studentsWithStatsFiltered$Present <- round(studentsWithStatsFiltered$Present, 0)
bin_size <- 10

if (q1Part1 == TRUE) {
  studentsWithStatsFiltered %>%
    mutate(conduct_total = n_ach + n_beh) %>%
    mutate(n_ach_perc = n_ach/conduct_total*100) %>%
    mutate(n_beh_perc = n_beh/conduct_total*100) %>%
    mutate(bin_present = factor(Present%/%bin_size*bin_size)) %>% 
    filter(n_beh<2000) %>%
    ggplot(aes(x=bin_present, y=n_ach_perc, fill=gender)) +
    geom_boxplot(position=position_dodge(1)) +
    labs(title = "Average positive behaviour % against the % attendance of students, seperated by gender.",
         colour = "Gender",
         x = "% Attendance", 
         y = "% Positive Behaviour")
}


bin_size <- 5
studentsWithStatsByGender <- 
  studentsWithStatsFiltered %>%
  mutate(conduct_total = n_ach + n_beh) %>%
  mutate(n_ach_perc = n_ach/conduct_total*100) %>%
  mutate(n_beh_perc = n_beh/conduct_total*100) %>%
  mutate(bin_present = factor(Present%/%bin_size*bin_size)) %>% 
  filter(n_beh<2000) %>%
  group_by(gender, bin_present) %>%
  summarise(mean_ach_perc = mean(n_ach_perc))

bin_size <- 10
studentsWithStatsByYeargroup <- 
  studentsWithStatsFiltered %>%
  mutate(conduct_total = n_ach + n_beh) %>%
  mutate(n_ach_perc = n_ach/conduct_total*100) %>%
  mutate(n_beh_perc = n_beh/conduct_total*100) %>%
  mutate(bin_present = factor(Present%/%bin_size*bin_size)) %>% 
  filter(n_beh<2000, yeargroup<12) %>%
  replace(is.na(.), 0) %>%
  group_by(yeargroup, bin_present) %>%
  summarise(mean_ach_perc = mean(n_ach_perc), mean_exclusion_days = mean(total_exclusion_days))

if (q1Part2 == TRUE) {
  studentsWithStatsByYeargroup %>% 
    ggplot(aes(x=bin_present, y=mean_ach_perc, group=yeargroup)) +
    geom_line(aes(color=yeargroup, lwd=round(mean_exclusion_days))) + 
    geom_point(aes(color=yeargroup)) + 
    labs(title = "Average positive behaviour % against the % attendance of students, separated by yeargroup.",
         colour = "Yeargroup",
         x = "% Attendance", 
         y = "% Positive Behaviour",
         lwd = "Average Days Excluded") +
    scale_color_gradient(low="green", high="purple")
}

#----------------------------------------------------------------------------------------------------------
#Q2 How does behaviour and attendance differ throughout the day?
  
#count all attendance records by AM and PM seperated by category
attsAmPm <- inner_join(attendance, attendance_codes, by="mark_school_id") %>% 
  #Remove join by column
  select(-mark_school_id, -mark, -description) %>%
  mutate(student_school = paste(student_id, school_id, sep="-")) %>% 
  select(-student_id) %>%
  left_join(students, by="student_school") %>%
  filter(short_meaning_description != "Not Required", physical_meaning != "NOMARK", yeargroup < 14, yeargroup > 6 ) %>%
  group_by(yeargroup, short_meaning_description) %>%
  mutate(totalCountForType = n()) %>%
  group_by(yeargroup, short_meaning_description, session) %>%
  mutate(AmPmCount = n()) %>%
  summarise(markTypePerc = 100*round(first(AmPmCount)/first(totalCountForType), 3))

behTimesWithYeargroups <- behsJoined %>% 
  select(beh_time, student_school) %>%
  inner_join(students, by="student_school")%>%
  select(beh_time, yeargroup) %>%
  filter(!beh_time=="") %>%
  filter(!beh_time=="00:00") %>%
  filter(!beh_time=="04:00") %>%
  filter(yeargroup < 14, yeargroup > 6)

#Add AM and PM Factor Levels
levels(behTimesWithYeargroups$beh_time) <- c(levels(behTimesWithYeargroups$beh_time), "AM", "PM")

#Clean time field to AM and PM
behTimesWithYeargroups$beh_time[strptime(behTimesWithYeargroups$beh_time, "%H:%M") < strptime("12:00", "%H:%M")] <- as.factor("AM")
behTimesWithYeargroups$beh_time[strptime(behTimesWithYeargroups$beh_time, "%H:%M") >= strptime("12:00", "%H:%M")] <- as.factor("PM")
behTimesWithYeargroups$beh_time[grepl("AM", behTimesWithYeargroups$beh_time, fixed=TRUE)] <- as.factor("AM")
behTimesWithYeargroups$beh_time[grepl("Am", behTimesWithYeargroups$beh_time, fixed=TRUE)] <- as.factor("AM")
behTimesWithYeargroups$beh_time[grepl("PM", behTimesWithYeargroups$beh_time, fixed=TRUE)] <- as.factor("PM")
behTimesWithYeargroups$beh_time[grepl("Lunch", behTimesWithYeargroups$beh_time, fixed=TRUE)] <- as.factor("PM")
behTimesWithYeargroups$beh_time[grepl("lunch", behTimesWithYeargroups$beh_time, fixed=TRUE)] <- as.factor("PM")
behTimesWithYeargroups$beh_time[grepl("Break", behTimesWithYeargroups$beh_time, fixed=TRUE)] <- as.factor("AM")
behTimesWithYeargroups$beh_time[grepl("Before", behTimesWithYeargroups$beh_time, fixed=TRUE)] <- as.factor("AM")
behTimesWithYeargroups$beh_time[grepl("After", behTimesWithYeargroups$beh_time, fixed=TRUE)] <- as.factor("PM")
behTimesWithYeargroups$beh_time[grepl("Reg", behTimesWithYeargroups$beh_time, fixed=TRUE)] <- as.factor("AM")
behTimesWithYeargroups$beh_time[grepl("break", behTimesWithYeargroups$beh_time, fixed=TRUE)] <- as.factor("AM")
behTimesWithYeargroups$beh_time[grepl("[5,6,7]", behTimesWithYeargroups$beh_time)] <- as.factor("PM")
behTimesWithYeargroups$beh_time[grepl("[1,2,3,4]", behTimesWithYeargroups$beh_time)] <- as.factor("AM")
behTimesWithYeargroups$beh_time[grepl("End", behTimesWithYeargroups$beh_time)] <- as.factor("PM")

behTimesWithYeargroupsClean <- 
  behTimesWithYeargroups %>%
  filter(beh_time == "AM" | beh_time=="PM")

#Update Factor Levels
behTimesWithYeargroupsClean$beh_time <- factor(behTimesWithYeargroupsClean$beh_time)

#Count all AMs and PMs
behAmPm <-
  behTimesWithYeargroupsClean %>%
  group_by(yeargroup) %>%
  mutate(total = n()) %>%
  group_by(yeargroup, beh_time) %>%
  summarise(sessionCount = 100*round(n()/first(total), 3)) %>%
  ungroup() %>%
  transmute(yeargroup=yeargroup, session=beh_time, category = factor("Bad Behaviour"), sessionCount = sessionCount)

#Match behAmPm to attsAmPm
attsAmPm <-
  attsAmPm %>%
  ungroup() %>%
  transmute(yeargroup=yeargroup, session=session, category=short_meaning_description, sessionCount=markTypePerc)

exclusionsAmPm <-
  exclusions %>%
  mutate(student_school = paste(student_id, school_id, sep="-")) %>% 
  select(-student_id) %>%
  left_join(students, by="student_school") %>%
  filter(yeargroup > 6, yeargroup < 14) %>%
  group_by(yeargroup) %>%
  mutate(total = n()) %>%
  group_by(yeargroup, start_session) %>%
  summarise(sessionCount = 100*round(n()/first(total), 3)) %>%
  ungroup() %>%
  transmute(yeargroup=yeargroup, session=start_session, category = factor("Exclusions"), sessionCount=sessionCount)

#Make all factor levels the same
#attsAmPm$category <- factor(attsAmPm$category)
#attsAmPm$category <- c(levels(attsAmPm$category), "Exclusions", "Bad Behaviour")
#exclusionsAmPm$category <- c(levels(attsAmPm$category))
#behAmPm$category <- c(levels(attsAmPm$category))

q2 <- 
  bind_rows(attsAmPm, behAmPm, exclusionsAmPm) 

q2$category <- factor(q2$category, levels = c('Late', 'Exclusions', 'Bad Behaviour', 'Unauthorised', 'Present', 'Authorised'))

if (q2Part1 == TRUE) {
  q2 %>%
    ggplot(aes(x=category, y=sessionCount, fill=session, label=sessionCount) ) + 
    geom_bar(stat="identity", position="dodge") +
    labs(title = "Percentage of different student records split by time of day.",
         fill = "Time",
         x = "Category", 
         y = "%") + 
    scale_fill_manual("Time", values = c("AM" = "orange", "PM" = "darkgreen"))
}

if (q2Part2 == TRUE) {
  #Small multiples by yeargroup
  q2 %>%
    ggplot(aes(x=category, y=sessionCount, fill=session, label=sessionCount) ) + 
    geom_bar(stat="identity", position="dodge") +
    geom_text(size = 3, position = position_dodge(width=1)) + 
    facet_grid(yeargroup ~ .) + 
    labs(title = "Percentage of different student records split by time of day and yeargroup.",
         fill = "Time",
         x = "Category", 
         y = "%") +
    scale_fill_manual("Time", values = c("AM" = "orange", "PM" = "darkgreen"))
}
#---------------------------------------------------------------------------------------------

#Q3 How is attendance/behaviour affected by the average salary/household income in an area?

#%attendance + %positive behaviour + %of students with exclusions, against the average income in the area
head(schools)
head(atts)

#summarise attendance by school
schoolAtts <- atts %>%
  group_by(school_id) %>%
  summarise(p_att = mean(Present), p_auth=mean(Authorised), p_late=mean(Late), p_unauth=mean(Unauthorised)) %>%
  right_join(schools, by="school_id") %>%
  na.omit()

head(schoolAtts)

#summarise behaviour by school
head(behs)
head(achs)

schoolBehs <- behs %>%
  filter(summed_beh_points>0) %>%
  group_by(school_id) %>%
  summarise(beh_points = sum(summed_beh_points), n_beh=sum(n_beh))


schoolAchs <- achs %>%
  filter(summed_ach_points>0) %>%
  group_by(school_id) %>%
  summarise(ach_points = sum(summed_ach_points), n_ach=sum(n_ach))

head(schoolBehs)
head(schoolAchs)

schoolConducts <- schoolBehs %>%
  inner_join(schoolAchs, by="school_id") %>%
  mutate(p_n_ach = 100*n_ach/(n_ach+n_beh), p_ach_points=100*ach_points/(ach_points+beh_points)) %>%
  right_join(schools, by="school_id") %>%
  na.omit()

head(schoolConducts)

#Combine behaviours and attendance
schoolSummary <- schoolAtts %>%
  select(-name, -income, -n_students,-type, -religious) %>%
  inner_join(schoolConducts, by="school_id") %>% 
  mutate(score = p_att + p_n_ach)

#Convert income to integer 
schoolSummary$income <- as.integer(schoolSummary$income)

if (q3Part1 == TRUE) {
  ggplot(schoolSummary, aes(x=income, y=score, size=n_students)) +
    geom_smooth(method = "lm", se = FALSE, aes(group=2)) + 
    geom_point() + 
    labs(title="School scores plotted against the average income in the local area.", 
      x="Average Local Annual Income (£)",
      y="Score (% Attendance + % Positive Behaviour)",
      size="Number of Students")
}

Q3 <- ggplot(schoolSummary, aes(label=name, x=income, y=score, size=n_students, color=type, shape=religious)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, aes(group=1)) + 
  labs(title="School scores plotted against the average income in the local area.", 
         x="Average Local Annual Income (£)",
         y="Score (% Attendance + % Positive Behaviour)",
         color="Type",
         size="Number of Students",
         shape="Religious") + 
  theme( legend.title = element_blank() ) 

if (q3Part2 == TRUE) {
  ggplotly( Q3 ) %>%
    add_annotations( text="(Type, Religious)", xref="paper", yref="paper",
                     x=1.02, xanchor="left",
                     y=0.8, yanchor="bottom",    # Same y as legend below
                     legendtitle=TRUE, showarrow=FALSE ) %>%
    layout( legend=list(y=0.8, yanchor="top" ) )
}