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

#' ### Project on [Github](https://github.com/weihanglo/lads-group8/tree/master/linguistJobs)
#' # Import Library and Data
#+ warning=F, error=F, message=F 
library(colorspace)
library(data.table)
library(ggplot2)
library(ggmap)
library(pipeR)
library(rgdal)
library(rgeos)
library(leaflet)
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

#' ## Clean up the table
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


#' ## Create table *JobSpecialty*
jobSpecialty <- jobs[, tstrsplit(Specialty, ";")]
jobSpecialty[, uid := .I]
jobSpecialty <- melt(jobSpecialty, 
    measure.vars = paste0("V", 2:ncol(jobSpecialty) - 1),
    id.vars = "uid",
    value.name = "Specialty"
)
jobSpecialty[, variable := NULL]
jobSpecialty <- jobSpecialty[!is.na(Specialty)]
setkey(jobSpecialty, uid)
jobSpecialty[, Specialty := gsub("^\\s|\\s$", "", Specialty)]




#' # Process geospatial data
#' ## Find locations of employer
#+ eval=FALSE
geocodes <- sapply(1:nrow(jobs), function(i) {
    X <- geocode(jobs[i, paste(JobLocation, Employer)], source = "google")
    if (any(is.na(X)))
        X <- geocode(jobs[i, paste(JobLocation, JobSubLocation)], source = "google")
    if (any(is.na(X)))
        X <- geocode(jobs[i, Employer], source = "google")
    if (any(is.na(X)))
        X <- geocode(jobs[i, JobLocation], source = "google")
    return(X)
})
jobs[, c("lon", "lat") := as.data.frame(matrix(unlist(t(geocodes)), ncol = 2))]
write.table(jobs, "jobs_clear.csv", sep = ",", row.names = FALSE)

#' ### Get geoJSON of world country
#+ warning=F, error=F, message=F
worldCountry <- readOGR("worldCountry.json", "OGRGeoJSON")  
jobs_pts <- SpatialPointsDataFrame(
    coords = fread("jobs_clear.csv", sep = ",", select = c("lon", "lat")),
    data = jobs, 
    proj4string = worldCountry@proj4string
)

#' ### Handle points laying outside the ploygons
#+ warning=F, error=F, message=F
#plot(worldCountry[complete.cases(over(worldCountry, jobs_pts)), ])
cols <- c("continent", "subregion")
continent <- (jobs_pts %over% worldCountry)[, cols]
outer <- which(rowSums(is.na(continent)) > 1)
outerIndex <- sapply(jobs[outer, JobLocation], function(con) {
    x <- grep(con, worldCountry$name)
    if (length(x) == 0) x <- grep("China", worldCountry$name)
    x
})
continent[outer, ] <- worldCountry@data[outerIndex, cols]
jobs_pts@data <- cbind(jobs_pts@data, continent)
setDT(jobs_pts@data)

`%wo%` <- function(x, y) x[!x %in% y] #--  x without y

continentsDT <- jobs_pts@data[, .N, by = continent]
Nrow <- (nrow(continentsDT) + 1) : nlevels(continentsDT$continent)
continentsDT <- rbindlist(list(continentsDT, 
    data.table(continent = levels(continentsDT$continent) %wo% continentsDT$continent)
), fill = TRUE)
continentsDT[is.na(N), N := 0]
row.names(continentsDT) <- continentsDT$continent

subregionsDT <- jobs_pts@data[, .N, by = subregion]
Nrow <- (nrow(subregionsDT) + 1) : nlevels(subregionsDT$subregion)
subregionsDT <- rbindlist(list(subregionsDT, 
    data.table(subregion = levels(subregionsDT$subregion) %wo% subregionsDT$subregion)
), fill = TRUE)
subregionsDT[is.na(N), N := 0]
row.names(subregionsDT) <- subregionsDT$subregion


#' ### Dissolve country polygons to regions
#+ warning=F, error=F, message=F
continents <- gUnaryUnion(worldCountry, worldCountry$continent) %>>%
    SpatialPolygonsDataFrame(continentsDT, match.ID = TRUE)
subregions <- gUnaryUnion(worldCountry, worldCountry$subregion) %>>%
    SpatialPolygonsDataFrame(subregionsDT, match.ID = TRUE)


#' ## Create leaflet map 
#+ warning=F, error=F, message=F
popups <- sprintf(
    '<b style="font-size:140%%">%s</b><br/><br/>Seeking %s<br/><br/>%s, %s', 
    jobs$Employer, jobs$RankorTitle, jobs$JobSubLocation, jobs$JobLocation
)
colorCon <- data.table(col = rev(heat_hcl((length(continents)))))
colorCon[, c("N", "index") := sort.int(continents$N, index.return = TRUE)]
colorCon[colorCon$index, inv_col := colorCon$col] 
colorSub <- data.table(col = rev(terrain_hcl((length(subregions)))))
colorSub[, c("N", "index") := sort.int(subregions$N, index.return = TRUE)]
colorSub[colorSub$index, inv_col := colorSub$col] 


map <- leaflet() %>>%
    # Base map groups
    addProviderTiles("CartoDB.DarkMatter", group = "CartoDB") %>>%
    addTiles(group = "OpenStreetMap") %>>%
    addProviderTiles("Esri.WorldImagery", group = "Esri: World Imagery") %>>%

    # Layer groups
    addCircleMarkers(data = jobs_pts@coords,
        fillOpacity = 0.7, 
        color = 'green',
        popup = popups,
        clusterOptions = markerClusterOptions(), 
        group ="Employer"
    ) %>>%

    addPolygons(data = continents,
        color = colorCon[, inv_col], 
        fillOpacity = 0.55,
        layerId = 1,
        popup = sprintf('<b style="font-size:160%%">%d jobs here!</b>', continents$N),
        group ="Continent"
    ) %>>%

    addPolygons(data = subregions,
        color = colorSub$inv_col,
        fillOpacity = 0.55,
        layerId = 1,
        popup = sprintf('<b style="font-size:160%%">%d jobs here!</b>', subregions$N),
        group ="Subregion"
    ) %>>%

    addLegend("bottomright", 
        colors = colorCon$inv_col,
        labels = continents$continent,
        title = "Continents",
        layerId = 1,
        opacity = 1,
    ) %>>%

    addLegend("bottomright", 
        colors = colorSub$inv_col,
        labels = subregions$subregion,
        title = "Subregions",
        layerId = 1,
        opacity = 1,
    ) %>>%

    addLayersControl(
        baseGroups = c("CartoDB", "OpenStreetMap", "Esri: World Imagery", "Continent"),
        overlayGroups = c("Employer", "Continent", "Subregion")
#        options = layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)
    )


#' # Data Analysis
#' ### We tried to answer questions below
#' *  <p style="font-size:160%%">不同時代的專長趨勢分佈？</p>
#' *  <p style="font-size:160%%">那個工作地點提供最多的工作？</p>
#'    
#'    

#' ## 熱門專業
job_ana <- merge(jobs, jobSpecialty) 
jobSpecialty[, .(Rank = .N), by = Specialty][order(-Rank)][1:10]

#' 經過前處理後，可以看到熱門的專業需求前三名是比較大範圍的語言學專業（應用，一般與計算語言學），另外是比較專精的語言學次領域（Language Acquisition, Phonetics, Syntax），熱門語言則是英文，西班牙文和德文。
#+ fig.width=11, fig.height=6.5
g1 <- ggplot(data=job_ana, aes(x=as.POSIXct(DatePosted))) + 
        geom_bar(aes(fill=Specialty.y), binwidth=86400) + 
        scale_x_datetime("Date") +
        ggtitle ("不同時代的專長趨勢分佈")
g1

#'   

#' ## 成長趨勢
#+ fig.width=9, fig.height=6.5
end_day <- jobs[, max(DatePosted)] 
start_day <- jobs[, min(DatePosted)]
days <- data.table(date = seq(min(jobs$DatePosted), max(jobs$DatePosted), 1))
days[, Njobs := seq(1, nrow(jobs), len = .N)]

#' 從圖看來，`r start_day` 至今（`r end_day`），語言學相關工作並沒有顯著性的成長，很穩定地以將近每天`r nrow(jobs) / as.integer(end_day - start_day)`個的速度增加。
g2 <- ggplot(jobs[, .N, by = DatePosted][order(DatePosted, desc = TRUE)]) +
    ggtitle(sprintf("Number of Jobs from %s to %s", start_day, end_day)) +
    ylab("Cumulative # jobs") +
    geom_line(aes(DatePosted, cumsum(N)), col = 2) +
    geom_line(aes(DatePosted, rev(days[date %in% jobs$DatePosted, Njobs])), col = 4)
g2

#'  

#' ### 最積極尋找語言人才的公司：**GOOGLE**
jobs[, .N, by = Employer][order(-N)][1:10]
jobs[grep("[Gg]oogle", Employer), .N]

#' ### 最需要語言人才的工作場域：**大學**
jobs[grep("univ(ers)?|colleg", Employer, ignore.case = TRUE), .N]

#' ### 最多的工作職缺：教授（錢不好賺又累）
jobs[grep("Professor|Lectur|Instruc|Teach", RankorTitle, ignore.case = TRUE), .N]
jobs[grep("Linguist", RankorTitle, ignore.case = TRUE), .N]
jobs[grep("Research", RankorTitle, ignore.case = TRUE), .N]

#'  
#' ### 臺灣的現況（比較沒有計算語言學，除了**Google**...）
#+ echo=F
knitr::kable(jobs[JobLocation == "Taiwan", .(Employer, JobSubLocation, RankorTitle, DatePosted)])

#' <br> 
#'
#+ echo=F
knitr::kable(job_ana[JobLocation == "Taiwan", .(Specialty = unique(Specialty.y)), by = Employer])


#' <br>
#'   

#' ## 小尺度地理關係
#' ### 最積極找尋人才的國家：美國
#+ fig.width=9, fig.height=6.5
gtheme <- theme(
    title = element_text("bold"),
    axis.text.x = element_text(angle = 90, hjust = 1, color = 'grey40')
)
g3 <- ggplot(jobs[, .N, by = JobLocation][order(-N)][1L:9L]) +
    geom_bar(aes(JobLocation, N, fill = JobLocation), stat = "identity") +
    gtheme
g3

#' ### 最積極找尋人才的城市（地區）：加州
#' 推測與**Text Mining**有關
#+ fig.width=9, fig.height=6.5
g4 <- ggplot(jobs[, .N, by = JobSubLocation][order(-N)][1L:9L]) +
    geom_bar(aes(JobSubLocation, N, fill = JobSubLocation), stat = "identity") +
    gtheme
g4

g5 <- ggplot(job_ana[grep("[Cc]alifornia", JobSubLocation), .N, by = Specialty.y][order(-N)][1L:9L]) +
    geom_bar(aes(Specialty.y, N, fill = Specialty.y), stat = "identity") +
    gtheme + xlab("Specialty")
g5

#' ## 大尺度地理關係

#' 藉由以下動態地圖，可觀察到語言學相關的工作大多位與歐美文化圈，但也可能是因爲[**The Linguist List**](http://linguistlist.org/)網站是美國印第安納大學設立的，因此較少亞洲區的工作。  
#'   

#' ### Click to read more!!!
#+ warning=F, error=F, message=F, echo=F, fig.width=11, fig.height=7.5, fig.align='left'
map
#' <br><br><br><br><br><br><br><br><br><br>
