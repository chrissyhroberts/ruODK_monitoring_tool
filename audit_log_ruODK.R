##############################################################################
# ruODK Audit builder v1.0.0
# Chrissy Roberts 2023-07-07
##############################################################################
#
# This script runs in R and will create a fully detailed audit log of
# changes made to submissions in ODK Central
# It required ruODK for functions to interact with the ODK API
# Also askpass for collecting cleartext passwords
# and Tidyverse just because it is tidyverse
#
# In this example we use a project ID : 37 and a form called "geopoint_map"
#
##############################################################################
# Libraries
##############################################################################

library(ruODK)
library(tidyverse)
library(askpass)

##############################################################################
# ruODK setup
##############################################################################
ruODK::ru_setup(
  url = "YOUR_CENTRAL_URL",
  un = "YOUR_CENTRAL_USERNAME",
  pw = askpass("please enter your password"),
  tz = "Europe/London",
  verbose = FALSE
)


##############################################################################
##############################################################################
# START OF FUNCTIONS
##############################################################################
##############################################################################

##############################################################################
# Define a function that replaces NULLs in lists, allowing variables to be respected even when null
##############################################################################

replace_null <- function(x){
  x <- purrr::map(x, ~ replace(.x, is.null(.x), NA_character_))
  purrr::map(x, ~ if(is.list(.x)) replace_null(.x) else .x)
}

##############################################################################
# Define an adapted version of ruODK's function which gets the basic audit, but with extra metadata
##############################################################################

get_one_submission_audit_detailed<-function (iid,
                                             pid = get_default_pid(),
                                             fid = get_default_fid(),
                                             url = get_default_url(),
                                             un = get_default_un(),
                                             pw = get_default_pw(),
                                             retries = get_retries()) {
  ruODK:::yell_if_missing(url, un, pw, pid = pid, fid = fid, iid = iid)
  httr::RETRY("GET", glue::glue("{url}/v1/projects/{pid}/forms/",
                                "{URLencode(fid, reserved = TRUE)}/submissions/{iid}/audits"),
              httr::add_headers(
                "Accept" = "application/xml",
                "X-Extended-Metadata" = "true"
              ),
              httr::authenticate(un, pw), times = retries) %>% ruODK:::yell_if_error(.,
                                                                                     url, un, pw) %>% httr::content(.)
}

##############################################################################
# Define an adapted version of ruODK's function, but which gets the diffs, with extra metadata if it exists
##############################################################################


get_one_submission_diffs<-function (iid, pid = get_default_pid(), fid = get_default_fid(),
                                    url = get_default_url(), un = get_default_un(), pw = get_default_pw(),
                                    retries = get_retries())
{
  ruODK:::yell_if_missing(url, un, pw, pid = pid, fid = fid, iid = iid)
  httr::RETRY("GET", glue::glue("{url}/v1/projects/{pid}/forms/",
                                "{URLencode(fid, reserved = TRUE)}/submissions/{iid}/diffs"),
              httr::add_headers(
                "Accept" = "application/xml",
                "X-Extended-Metadata" = "true"
              ),
              httr::authenticate(un, pw), times = retries) %>% ruODK:::yell_if_error(.,
                                                                                     url, un, pw) %>% httr::content(.)

}

##############################################################################
# Define an adapted version of ruODK's function, but which gets the comments about the edits, with extra metadata
##############################################################################

get_one_submission_comments<-function (iid, pid = get_default_pid(), fid = get_default_fid(),
                                       url = get_default_url(), un = get_default_un(), pw = get_default_pw(),
                                       retries = get_retries())
{
  ruODK:::yell_if_missing(url, un, pw, pid = pid, fid = fid, iid = iid)
  httr::RETRY("GET", glue::glue("{url}/v1/projects/{pid}/forms/",
                                "{URLencode(fid, reserved = TRUE)}/submissions/{iid}/comments"),
              httr::add_headers(
                "Accept" = "application/xml",
                "X-Extended-Metadata" = "true"
              ),
              httr::authenticate(un, pw), times = retries) %>% ruODK:::yell_if_error(.,
                                                                                     url, un, pw) %>% httr::content(.)

}
##############################################################################
#create a function to fully unlist changes because the diffs are nested lists of lists
# especially when multiple changes are made within a group
##############################################################################

audit_unlister<-function(x){
  curr_path = str_c(x$path,collapse = "/")
  old_list = unlist(x$old)
  new_list = unlist(x$new)
  path_depth = names(unlist(x$new))

  out = tibble(old = old_list,
               new = new_list,
               path = str_c(curr_path,path_depth,sep = "/")
  )

  if(is.null(old_list)){out$old=NA}
  if(is.null(new_list)){out$new=NA}
  if(is.null(curr_path)){out$path=NA}

  out<-select(out,old,new,path)

  return(out)
}

##############################################################################
# Define function which creates a report that amalgamates data from the three sources
##############################################################################

get_audit_report_one_submission<-function(iid,
                                          pid = get_default_pid(),
                                          fid = get_default_fid(),
                                          url = get_default_url(),
                                          un = get_default_un(),
                                          pw = get_default_pw(),
                                          retries = get_retries()){

  # get the audit for the submission
  a<-replace_null(get_one_submission_audit_detailed(iid,pid,fid,url,un,pw,retries))
  # get the diffs for a single submission
  b<-replace_null(get_one_submission_diffs(iid,pid,fid,url,un,pw,retries))
  # get the comments from the edits
  d<-replace_null(get_one_submission_comments(iid,pid,fid,url,un,pw,retries))

  # convert the audit to a tibble providing a 'change number' to allow tidy pivot because ODK Central doesn't provide a truly unique hex
  x<-tibble(action = names(unlist(a)),
            value = unlist(a)) %>%
    group_by(action) %>%
    mutate(duplicateID = row_number(),
           actor.no.internal = case_when(action == "actorId" ~ duplicateID),
    ) %>%
    ungroup() %>%
    fill(actor.no.internal) %>%
    pivot_wider(id_cols = c(actor.no.internal),names_from = action,values_from = value) %>%
    rename(uuid = "details.instanceId")

  # if there have been edits, then turn the list of changes and the list of edit comments in to a tibble,
  # using duplicate row detection to find the start of each group of linked edits (i.e. to find all rows related to a single editing session)

  if(length(b)>0){
    y<- tibble(uuid = character(),old=character(),new=character(),path=character())

    # Loop through all levels of the file and unnest the list of lists
    for(i in 1:length(b)){
      #    print(i)
      mother = b[[i]]
      #    print(mother)
      for(j in 1:length(mother)){
        #      print(j)
        tmp = audit_unlister(mother[[j]])
        tmp$uuid <- names(b)[i]
        y = bind_rows(y,tmp)
      }
    }
  }


  # If there have been no edits, then make dummy variables for the initial submission,
  # so that the table can be merged with ones where there are edits
  if(length(b)==0){

    y<- x %>% mutate (
      acteeId = NA,
      old = NA,
      new = NA,
      path = NA,
      comment = NA,
      actor.updatedAt = NA
    )
  }



  # build the final df, for cases where there are edits
  if(length(b)>0){
    fin<-   full_join(x,y) %>%
      arrange(loggedAt)

    if(length(d)==0){fin<-fin %>% mutate(comment=NA)}
  }

  # build the final df, for cases where there are no edits
  if(length(b)==0){fin<-y}





  if(length(d)>0){
    # create the comment rows
    yy<-tibble(stat = names(unlist(d)),
               value = unlist(d))%>%
      group_by(stat) %>%
      mutate(duplicateID = row_number(),
             field.change.no = case_when(stat == "body" ~ duplicateID),
      ) %>%
      ungroup() %>%
      fill(field.change.no) %>%
      distinct() %>%
      pivot_wider(id_cols = c(duplicateID),names_from = stat,values_from = value,values_fn = list) %>%
      rename(loggedAt = createdAt) %>%
      mutate(
        loggedAt = as.character(loggedAt),
        uuid=NA,
        actor.no.internal = NA,
        action = "comment",
        old = NA,
        new = NA,
        path = NA,
        actor.displayName = as.character(actor.displayName),
        actor.createdAt=as.character(actor.createdAt),
        actor.type = as.character(actor.type),
        actorId = as.character(actorId),
        actor.id = actorId,
        actor.updatedAt = as.character(actor.updatedAt),
        actor.deletedAt = as.character(actor.deletedAt),
        acteeID = NA,
        details.submissionId = NA,
        comment = body
      ) %>%
      select(
        -duplicateID,
        -body
      )

    #bind the comment rows
    fin<-bind_rows(fin,yy)
  }

  # sort columns

  fin<- fin %>%
    mutate(
      project_id = pid,
      form_id = fid
    ) %>%
    select(
      project_id,
      form_id,
      details.submissionId,
      loggedAt,
      uuid,
      comment,
      action,
      path,
      old,
      new,
      actor.displayName,
      actor.type,
      actor.id,
      everything()
    ) %>%
    #    mutate_all(as.character) %>%
    arrange(loggedAt)



  return(fin)
}
##############################################################################
##############################################################################
# END OF FUNCTIONS
##############################################################################
##############################################################################



##############################################################################
# USING THE FUNCTIONS
##############################################################################

# First Get a list of all submissions for a specified form

sub.list<-submission_list(pid = 37,fid = "period.diary")
sub.list<-sub.list$instance_id

#Build an initial df, using the first submission ID from sub.list
df<-get_audit_report_one_submission(iid = sub.list[1],pid = 37,fid = "period.diary")
#Bind rows of all subsequent submission IDs from the sub.list
for(i in 2:length(sub.list)){
  message(str_c("getting audit ",i," of ", length(sub.list)))
  try(df <- bind_rows(df,get_audit_report_one_submission(iid = sub.list[i],pid = 37,fid = "period.diary")))}

#Arrange by date
df<-df %>% arrange(loggedAt)


write_csv(df,"example_audit.csv")
