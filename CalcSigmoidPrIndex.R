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

number = document$response$numFound
start = 0

investigated = 0

#use start param to shift rows!!

print(paste('number of libraries: ', number))

while (number > start) {
  url <- URLencode(
    paste(
      sep = ''
      ,
      'http://search.maven.org/solrsearch/select?q=l:"sources"+AND+p:"jar"&rows=10&start=',
      start
    )
  )
  urldocument <- fromJSON(txt = url)
  #read them all, this may take a while.
  
  start = start + 10
  
  print(paste('loaded libraries: ', start))
  
  for (mavenlib in rows(urldocument$response$docs)) {
    #query versions
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
    
    versions <- fromJSON(txt = url)
    
    if (length(rows(versions$response$docs)) > 40) {
      versions$response$ordereddocs <-
        versions$response$docs[with(versions$response$docs, order(v)), ]
      
      majors = c()
      
      
      for (version in rows(versions$response$docs)) {
        #look for major versions (the first number for the dot)
        if (str_detect(version$v, "\\.") > 0) {
          major <- str_split(version$v, "\\.")[[1]]
          #print (paste(major[1], "\\", version$v))
          if (!length(which(majors == major[1])) > 0) {
            majors = c(majors, c(major[1]))
          }
        }
        # add to list
      }
      
      print(majors)
      
      if (length(majors) >= 3) {
        #loop thru second to n-1 majors
        
        print ('found >3 majors')
        
        for (major in majors) {
        for (version in rows(versions$response$docs)) {
            
          
         
          
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
          
          #print(paste('loading source', path))
          
          #curl::curl(url=path )
        }
        }
      }
    } else {
      #print (paste ('versions', length(rows(versions$response$docs))))
    }
  }
  
}
