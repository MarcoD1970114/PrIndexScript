



library(jsonlite)
library(stringr)

#http://stats.stackexchange.com/questions/142400/quantifying-similarity-between-two-data-sets


rows = function(tab)
  lapply(seq_len(nrow(tab)),
         function(i)
           unclass(tab[i, , drop = F]))

# url with some information about projects in maven
url <-
  'http://search.maven.org/solrsearch/select?q=l:%22sources%22%20AND%20p:%22jar%22&rows=0'

# read url and convert to data.frame
document <- fromJSON(txt = url)

#number = document$response$numFound
number  = 1000
start = 0

investigated = 0

#use start param to shift rows!!

print(paste('number of libraries: ', number))

while (number > start) {
  tryCatch({
    url <- URLencode(
      paste(
        sep = ''
        ,
        'http://search.maven.org/solrsearch/select?q=l:"sources"+AND+p:"jar"&rows=100&start=',
        start
      )
    )
    urldocument <- fromJSON(txt = url)
  }, error=function(e){})
  #read them all, this may take a while.
  
  start = start + 100
  
  print(paste('loaded libraries: ', start))
  
  for (mavenlib in rows(urldocument$response$docs)) {
    #query versions
    tryCatch({
      url <-
        URLencode(
          paste(
            sep = '',
            'http://search.maven.org/solrsearch/select?q=g:"',
            mavenlib$g,
            '"+AND+a:"'
            ,
            mavenlib$a,
            '"&core=gav&rows=100'
          )
        )
      
      print(url)
      
      versions <- fromJSON(txt = url)
    }, error=function(e){ })
   # Sys.sleep(1)
    
    if (length(rows(versions$response$docs)) > 40) {
      majors = c()
      
      
      for (version in rows(versions$response$docs)) {
        #look for major versions (the first number for the dot)
        if (str_detect(version$v, "\\.") > 0) {
          major <- str_split(version$v, "\\.")[[1]]
          #print (paste(major[1], "\\", version$v))
          if (!is.na(suppressWarnings(as.numeric(major[1])))) {
            if (!length(which(majors == major[1])) > 0) {
              majors = c(majors, c(major[1]))
            }
          }
        }
        # add to list
      }
      
      majors = sort(majors, decreasing = FALSE)
      #print(majors)
      
      if (length(majors) >= 3) {
        #loop thru second to n-1 majors
        
        print ('found >3 majors')
        points2 = c()
        points3 = c()
        urls = c()
        PrIndex = c()
        id = c()
        top = c()
        
        for (major in majors[1:length(majors) - 1]) {
          #skip the first and the last.
          
          
          for (version in rows(versions$response$docs)) {
            if (startsWith(version$v, paste(sep = "", major, '.'))) {
              a = str_replace_all(version$a, "\\.", "/")
              g = str_replace_all(version$g, '\\.', '/')
              
              path <-
                paste(
                  sep = '',
                  'http://search.maven.org/remotecontent?filepath=',
                  g
                  ,
                  '/',
                  a,
                  '/',
                  version$v,
                  '/',
                  version$a,
                  '-',
                  version$v,
                  '-sources.jar'
                )
              
              #print(paste(major,  version$v))
              
 
              points2 = c(points2, str_split(version$v, "\\.")[[1]][2])
              points3 = c(points3, str_split(version$v, "\\.")[[1]][3])
              urls = c(urls, path)
              id = c(id, version$id)
              top = c(top, major)
              
            }
          }
        }
        df = data.frame(id, urls, top, points2, points3)
      }
    } else {
      #print (paste ('versions', length(rows(versions$response$docs))))
    }
  }
}
