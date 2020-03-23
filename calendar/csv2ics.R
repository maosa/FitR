library(data.table)
library(dplyr)
library(stringr)

write.table(data.frame(
  starttime = gsub("-", "", as.character(fread(plan)$start)),
  summary = fread(plan)$content),
  file = calendar,
  sep = "\t",
  row.names = FALSE,
  quote = FALSE)

df <- read.csv("/Users/andreasmaos/Desktop/MuscleApp/calendar.csv",
               sep = "\t",
               stringsAsFactors = FALSE)

ics_header <- readLines("/Users/andreasmaos/Desktop/MuscleApp/template_header.ics", warn = FALSE)
ics_body <- readLines("/Users/andreasmaos/Desktop/MuscleApp/template_body.ics", warn = FALSE)
ics_footer <- readLines("/Users/andreasmaos/Desktop/MuscleApp/template_footer.ics", warn = FALSE)

ics_events <- ""

for(i in 1:nrow(df)) {
  ics_body <- str_replace(ics_body, "DTSTART:.*", paste0("DTSTART:", df$starttime[i]))
  # ics_body <- str_replace(ics_body, "DTEND:.*", paste0("DTEND:", df$endtime[i]))
  # create unique identifier
  ics_body <- str_replace(ics_body, "UID:.*", paste0("UID:", paste0(df$starttime[i], df$endtime[i])))
  ics_body <- str_replace(ics_body, "SUMMARY:.*", paste0("SUMMARY:", df$summary[i]))
  ics_events <- append(ics_events, ics_body)
}

# combine template parts to one vector
ics_events <- append(ics_header, ics_events)
ics_events <- append(ics_events, ics_footer)

write(ics_events, file = "/Users/andreasmaos/Desktop/MuscleApp/Workouts.ics")
