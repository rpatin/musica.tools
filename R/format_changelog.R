# format_changelog ---------------------------------------------------------
##' @name format_changelog
##' @author Remi Lemaire-Patin
##' 
##' @title Get information on ncdf dimensions
##' 
##' @description This function extract dimension id from the \code{ncdf4} object
##' 
##' 
##' @param filename a \code{character}, changelog file to reformat
##' 
##' @return
##' 
##' A \code{data.frame}
##' 
##' 
##' @family Tools
##' 
##'   
##' @examples
##' library(ncdf4)
##' @export
##' 
##' @importFrom stringr str_detect str_subset
##' @importFrom lubridate parse_date_time

format_changelog <- function(filename) {
  input <- readLines(filename)
  # input <- readLines("../MuSICA/master/changelog.md")
  start_section <-  which(str_detect(input, pattern = "# ") & 
                            !str_detect(input, pattern = "##"))
  
  input.authors <- input[seq(start_section[3] + 1, length(input))]
  input.v3 <- input[seq(start_section[1] + 1, start_section[2] - 1)]
  input.v3 <- input.v3[which(input.v3 != "")]
  
  input.v2 <- input[seq(start_section[2] + 1, start_section[3] - 1)]
  input.v2 <- input.v2[which(input.v2 != "")]
  
  # Managing v2 -----------------------------------------------------------
  start_date <- which(str_detect(input.v2, pattern = "# ") & 
                        !str_detect(input.v2, pattern = "###"))
  vec_begin <- start_date
  vec_end <- c(start_date[-1] - 1, length(input.v2))
  this_begin <- vec_begin[4]
  this_end <- vec_end[4]
  parsed_content <- 
    foreach(this_begin = vec_begin,
          this_end = vec_end,
          .combine = "rbind") %do% {
            this_version <-
              input.v2[this_begin] %>% 
              str_remove("## ") 
            this_date_formatted <-  
              parse_date_time(this_version, orders = "%b %Y")
            this_content <- input.v2[seq(this_begin + 1, this_end)]
            this_subsection <- which(str_detect(this_content, pattern = "###"))
            subvec_begin <- this_subsection
            subvec_end <- c(this_subsection[-1] - 1, length(this_content))
            # this_subbegin <- subvec_begin[1]
            # this_subend <- subvec_end[1]
            foreach(this_subbegin = subvec_begin,
                      this_subend = subvec_end, .combine = 'rbind') %do% {
                        this_subcontent <- this_content[seq(this_subbegin, this_subend)]
                        this_subtitle <-
                          this_subcontent[1] %>% 
                          str_remove("### ") 
                        this_subcontent <- this_subcontent[-1] %>% 
                          str_remove("- ")
                        data.frame(
                          "date" = as.Date(this_date_formatted),
                          "version" = this_version,
                          "category" = this_subtitle,
                          "content" =  this_subcontent
                        )
                      }
          }
  parsed_content
}