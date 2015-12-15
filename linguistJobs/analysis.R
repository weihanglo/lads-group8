#' ---
#' title: Jobs on The Linguist List
#' author: Group 8
#' date:  Dec, 2015
#' output:
#'   html_document:
#'     toc: true
#'     toc_depth: 2
#'     number_sections: true
#'     theme: united
#' ---

#' # Import Library and Data
library(data.table)
library(wordcloud)
jobs <- fread("./jobs.csv", sep = ',')

#' # Data Pre-processing

#' ### Clean noises (\\n)
jobs[, names(jobs) := lapply(.SD, function(x) gsub("\\n", "", x))]

#' ### Initialize data.table
jobs[, uid := .I]
setkey(jobs, uid)
setnames(jobs, gsub("\\W", "", names(jobs)))
setcolorder(jobs, c('uid', names(jobs)[-ncol(jobs)]))

#' ### Add column: **OpenUntilFilled (logical)**
jobs[, `:=`(
    OpenUntilFilled = grepl("Open until filled", Deadline),
    Deadline = gsub("\\s|\\(.*\\)", "", Deadline)
)]

#' ### Update column **ApplicationStatus** to type **logical**
jobs[, ApplicationStatus := sapply(ApplicationStatus, function(x) {
    if(x == "open") T else if (x == "closed") F else NA
})]
    
#' ### Update column **Deadline** and **DatePosted** to type **Date**
cols <- c("Deadline", "DatePosted")
jobs[, (cols) := lapply(.SD, as.Date, format = "%d-%b-%Y"), .SDcols = cols]

#' ### Split column **JobLocation** into **JobLocation** and **JobSubLocation**
jobs[, tstrsplit(JobLocation, ": ")][]
jobs[Employer == "Lingvist Inc." & DatePosted == "2015-09-08", 
    tstrsplit(JobLocation, ": ")
]
#' ##### Handling exception (Actually, we want to ignore it.)
jobs[Employer == "Lingvist Inc." & DatePosted == "2015-09-08", 
    JobLocation := "Various: Various"
]
jobs[, c("JobLocation", "JobSubLocation") := tstrsplit(JobLocation, ": ")]

#' ### Create table **JobSpecialty** 
jobSpecialty <- jobs[, tstrsplit(Specialty, ";")]
jobSpecialty[, uid := .I]
jobSpecialty <- melt(jobSpecialty, 
    measure.vars = paste0("V", 2:ncol(jobSpecialty) - 1),
    id.vars = "uid",
    value.name = "Specialty"
)
jobSpecialty[, variable := NULL]
jobSpecialty <- jobSpecialty[!is.na(Specialty)]
setkey(jobSpecialty)
jobSpecialty[, Specialty := gsub("^\\s|\\s$", "", Specialty)]

#' # Data Analysis
#' We tried to answer questions below:
#' - 不同時代的專長趨勢分佈？
#' - 那個工作地點提供最高的薪資？
