check_github_status <- function(services = c('Packages', 'Actions', 'API Requests', 'Git Operations')){
  components <- jsonlite::fromJSON('https://www.githubstatus.com/api/v2/summary.json')$components
  services <- components[components$name %in% services,]
  print(services[c('name', 'status', 'updated_at')])
  if(any(grep('outage', services$status, ignore.case = TRUE))){
    stop("GitHub problems. Not proceeding")
  }
}
