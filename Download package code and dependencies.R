## Description
# The goal here is to download tar.gz from CRAN for a list of packages
# And also download all dependencies

## List of packages to download
package_to_dowload = c("snowfall", "kohonen", "bit64", "reshape")

## rec_dep is a recursive function to find dependencies of all packages (and there dependencies...)
rec_dep <- function(package){
  list_dep = tools::package_dependencies(package)[[1]]
  if (length(list_dep) == 0){
    return(list_dep)  
  }
  for (elt in list_dep){
   print(elt)
   list_dep = c(list_dep, rec_dep(elt)) 
  }
  return(list_dep)
}

## We apply rec_dep to our list of packages
for (package in package_to_dowload){
  package_to_dowload = c(package_to_dowload, rec_dep(package))
}

# Perform unique to download each package only once
package_to_dowload = unique(package_to_dowload)

# Dowload them to specified path
download.packages(package_to_dowload, "E:/TEMP/")
