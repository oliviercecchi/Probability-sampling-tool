# This script contains functions related to probability sampling.
# It includes functions for creating a sampling frame, performing cluster sampling, random sampling, and two-stage random sampling.
# The functions are used to simulate samples based on user-defined parameters.

require(data.table)
require(dplyr)
require(car)
require(reshape2)
require(stringr)
library(shiny)
library(shinyjs)
library(crayon)

# lift the data size limit
options(shiny.maxRequestSize=30*1024^2)


# humanTime function returns the current time in a specific format.
humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

# Calculate the sample size required for a given population proportion
#
# Parameters:
#   x: The population size
#   A: The desired level of confidence (between 0 and 1)
#   p: The estimated population proportion (between 0 and 1)
#   E: The desired margin of error
# Returns:
#   The sample size required to achieve the desired level of confidence and margin of error
Ssize<-function (x,A,p,E) {(qchisq(A,1)*x*p*(1-p)) / (E^2*(x-1)+qchisq(A,df=1)*p*(1-p))}


# create the sampling frame
# This function creates a sampling frame based on the input parameters.
# It adds additional columns to the input data frame, such as id_sampl, strata_id, psu_id, pop_numbers, and proba.
# The id_sampl column is created by concatenating "id_" with the row names of the input data frame.
# If the sampling method is "Stratified", the strata_id column is created by extracting the values from the specified strata column in the input data frame.
# If the sampling method is "Cluster sampling", the psu_id column is created by extracting the values from the specified col_psu column in the input data frame,
# and the pop_numbers column is created by extracting the values from the specified colpop column in the input data frame.
# If the sampling method is "Simple random - allocation" or any other method, the psu_id column is created using the id_sampl column,
# and the pop_numbers column is created with a value of 1 for each row.
# The SumDist column is calculated by summing the pop_numbers column within each strata_id group.
# The proba column is calculated by dividing the pop_numbers column by the SumDist column.
# Any rows with missing values in the proba column are removed.
# If the sampling method is "Cluster sampling", the psu_id column is converted to a factor.
# The resulting data frame is returned.
format_sampling_frame<-function(sframe,input){
  sframe$id_sampl<-paste0("id_",rownames(sframe))
  if(input$stratified=="Stratified"){
    sframe$strata_id<-sframe[[as.character(input$strata)]]
  } else {
    sframe$strata_id<-rep("all",nrow(sframe))
  }

  if(input$samp_type=="Cluster sampling"){
    sframe$psu_id<-sframe[[as.character(input$col_psu)]]
    sframe$pop_numbers<-sframe[[as.character(input$colpop)]]
  }else if(input$samp_type=="Simple random - allocation"){
    sframe$psu_id<-sframe$id_sampl
    sframe$pop_numbers<-sframe[[as.character(input$colpop)]]
  }else{
    sframe$psu_id<-sframe$id_sampl
    sframe$pop_numbers<-rep(1,nrow(sframe))
  }
  
  sumdist<-sframe %>% dplyr::group_by(strata_id) %>%  dplyr::summarise(SumDist = sum(pop_numbers,na.rm=T))
  sframe<-merge(sframe,sumdist,by="strata_id")
  proba<-as.numeric(sframe$pop_numbers)/as.numeric(sframe$SumDist)
  sframe<-cbind(sframe,proba)
  sframe<-sframe[!is.na(sframe$proba),]
  
  if(input$samp_type=="Cluster sampling"){
    sframe$psu_id<-as.factor(sframe$psu_id)
  }
  return(sframe) 
}



# Calculate the sample size required for a given population proportion
#
# This function takes in a  dataframe and an input list, and calculates the sample size required for a given population proportion.
# It creates a new column 'strata_id' in the  dataframe based on the input 'strata' value.
# It then groups the dataframe by 'strata_id' and calculates the maximum population value for each group.
# Finally, it calculates the target sample size based on the input parameters, and also calculates the target sample size with a buffer.
#
# Args:
#   sframe: A dataframe containing the sampling frame data.
#   input: A list containing the input parameters.
#
# Returns:
#   A modified version of the sampling frame dataframe with additional columns 'target' and 'target.with.buffer' representing the calculated sample sizes.

create_targets<-function(sframe,input){
  sframe |> 
    dplyr::group_by(strata_id) |> 
    dplyr::summarise(
      Population = sum(pop_numbers,na.rm=T)
    ) |> 
    dplyr::mutate(
      target = ifelse(
        input$topup=="Enter sample size",
        input$target,
        ceiling(Ssize(Population,input$conf_level,input$pror,input$e_marg))
      ) |> as.numeric(),
      target.with.buffer = ifelse(
        input$topup=="Enter sample size",
        target,
        as.numeric(ceiling(target * (1+input$buf)))
      )
    )
}


# clustersample function performs cluster sampling based on the given parameters.
# Parameters:
# - sframe: The sampling frame.
# - sampling_target: a dataframe with the sampling targets by strata.
# - cls: The cluster size.
# - buf: The buffer size.
# - ICC: The intra-cluster correlation coefficient.
# - sw_rand: The list of strata IDs that have been switched to random sampling.
# Returns:
# - A list containing the sampled output and the updated sw_rand list.
clustersample<-function(sframe,sampling_target,cls,buf,ICC,sw_rand=c()) {
  target<-as.numeric(as.character(sampling_target[["target"]]))
  dist<-as.character(sampling_target[["strata_id"]])
  out<-cluster_sampling(sframe,cls=cls,buf=buf,ICC=ICC,dist=dist,target=target)
  
  if(is.null(out)){
    dbr<-sframe[as.character(sframe$strata_id)==dist,]
    out<-sample(as.character(dbr$id_sampl),ceiling(as.numeric(sampling_target[["target"]])*(1+buf+0.1)),prob=dbr$proba,replace=TRUE)
   # showModal(modalDialog(
   #    title = paste(dist,": All PSUs have been selected"),
   #    "Set cluster size to 1 to reduce the design  effect and extra buffer to account for analysis DEFF",
   #    easyClose = TRUE,
   #    footer = NULL
   #  ))
    sw_rand<-c(sw_rand,dist)
  }
  # incProgress(round(1/nrow(sampling_target),2), detail = paste("Sampling", dist))
  return(list(output=out,sw_rand=sw_rand))
}

#' Randomly samples from a sampling frame
#' This function takes a sampling frame, and a buffer size as input.
#' It randomly samples from the sampling frame dataset based on the sampling frame, ensuring that the number of samples does not exceed the population size.
#' The buffer size is used to determine the maximum number of samples to be taken.
#' sframe A sampling frame dataset containing the population information
#' sampling_target A sampling frame containing the strata ID, target with buffer, and population information
#' buf The buffer size to for the samples
#' Returns A vector of randomly selected IDs from the sampling frame dataset
randomsample<-function(sframe,sampling_target,buf){
  dist<-as.character(sampling_target[["strata_id"]])
  dbr<-sframe[as.character(sframe$strata_id)==dist,]
  tosample<-as.numeric(sampling_target[["target.with.buffer"]])
  pop<-as.numeric(sampling_target[["Population"]])
  if(tosample>pop){
    target<-ceiling(pop)
  }else{
    target<-ceiling(tosample)
  }
  out<-sample(x=as.character(dbr$id_sampl),size =target,replace=FALSE)
  # incProgress(round(1/nrow(sampling_target),2), detail = paste("Sampling", dist))
  return(out)
}

#' stage2rdsample Function
#' This function performs stage 2 random sampling based on given parameters.
#' sframe A data frame containing the sampling frame data.
#' sampling_target A data frame containing the sampling data.
#' buf The buffer size for sampling.
#' returns A vector of randomly selected IDs from the sampling frame data.
stage2rdsample<-function(sframe,sampling_target,buf){
  dist<-as.character(sampling_target[["strata_id"]])
  dbr<-sframe[as.character(sframe$strata_id)==dist,]  
  tosample<-as.numeric(sampling_target[["target.with.buffer"]])
  pop<-as.numeric(sampling_target[["Population"]])
  if(tosample>pop){
    target<-ceiling(pop)
  }else{
    target<-ceiling(tosample)
  }
  out<-sample(x=as.character(dbr$id_sampl),size=target,prob=dbr$proba,replace=TRUE)
  # incProgress(round(1/nrow(sampling_target),2), detail = paste("Sampling", dist))
  return(out)
}


#' Perform cluster sampling
#'
#' This function performs cluster sampling based on specified parameters.
#' sframe A data frame containing the sampling frame data.
#' cls The minimum cluster size.
#' buf The buffer size.
#' ICC The intra-cluster correlation coefficient.
#' dist The stratum ID.
#' target The target sample size.
#' mode The sampling mode. Default is "notforced".
#' returns A vector of sampled cluster IDs.
cluster_sampling<-function(sframe,cls,buf,ICC,dist,target,mode="notforced"){
  Sys.sleep(0.25)
  dbr<-sframe[as.character(sframe$strata_id)==dist,]
  dbr<-dbr[dbr$pop_numbers>=cls,]
  out<-sample(as.character(dbr$id_sampl),ceiling(as.numeric(target*(1+buf))/cls),prob=dbr$proba,replace=TRUE)
  
  stop<-F
  
  while(stop==F){
    d<-as.data.frame(table(out))[,2]
    ms<-sum(d)/nrow(as.data.frame(d))
    DESS<-1+(ms*cls-1)*ICC
    targ<-DESS*(target*(1+buf))/cls		
    
    if(sum(d)>=targ){
      # message(green(paste0(dist," : yeah")))
      stop<-T
      return(out)
      
    } else if ((mode == "forced" & cls==1 & DESS > 3 )){
     # message(red(paste0(dist," : exited because of DESS > 3")))
      stop<-T
      return(out)
      
    } else {
      out<-c(out,sample(as.character(dbr$id_sampl),1,prob=dbr$proba,replace=TRUE))
      rd_check<-all(unique(dbr$id_sampl)%in%unique(out))
      
      if(rd_check & mode == "notforced" ){
        # message(paste0(dist," : reduced cluster size to 1"))
        out<-NULL
        stop<-T
        return(out)
      } 
    }
  }
}


#' Function to create a sample based on different sampling methods
#'
#' This function takes a sampling frame and input parameters as input and creates a sample based on the specified sampling method.
#' The sampling methods supported are Cluster sampling, Simple random - allocation, and Simple random sampling.
#' The function formats the sampling frame, creates the target sample, and applies the specified sampling method to generate the output.
#' It also calculates various summary statistics related to the sample.
#'
#' sampling_frame The sampling frame data.
#' input The input parameters for the sampling method.
#' return A list containing the sample, summary statistics, and any additional information.
make_sample<-function(sampling_frame,input){

  # format the sample frame
  sampl_f<-format_sampling_frame(sampling_frame,input)
  
  # create the target sample. 
  target<-create_targets(sampl_f,input)
  
  sw_rand<-c()
  output<-c()
  
  cls<-input$cls
  buf<-input$buf
  ICC<-input$ICC

  if(input$samp_type=="Cluster sampling"){
    if(input$topup=="Enter sample size"){
      clsampling<-apply(target,1,clustersample,sframe=sampl_f,cls=cls,buf=0,ICC=0) # in that case, the buffer is not used, neither is the ICC
      output<-lapply(clsampling,function(x) x$output) %>% unlist %>% c
      sw_rand<-lapply(clsampling,function(x) x$sw_rand) %>% unlist %>% c
    }else {
      clsampling<-apply(target,1,clustersample,sframe=sampl_f,cls=cls,buf=buf,ICC=ICC) 
      output<-lapply(clsampling,function(x) x$output) %>% unlist %>% c
      sw_rand<-lapply(clsampling,function(x) x$sw_rand) %>% unlist %>% c
    }
  } else if (input$samp_type=="Simple random - allocation"){
    output<-apply(target,1,stage2rdsample,sframe=sampl_f,buf=buf) %>% unlist 
    
  } else if (input$samp_type=="Simple random"){
    output<-apply(target,1,randomsample,sframe=sampl_f,buf=buf) %>% unlist 
  }
  
  output<-as.data.frame(table(output))
  dbout<-merge(output,sampl_f,by.x="output",by.y="id_sampl",all.x=T,all.y=F)
  
  if(input$samp_type=="Cluster sampling"){  
    dbout$Freq<-ifelse(dbout$strata%in%sw_rand,dbout$Freq,dbout$Freq*cls)
  }
  
  names(dbout)<-recode(names(dbout),"'output'='id_sampl';'Freq'='Survey'")
  dbout$survey_buffer<-dbout$Survey
  
  
  # create the summary table
  summary_sample <- dbout |> 
    dplyr::group_by(strata_id) |> 
    dplyr::summarise(
      Surveys = sum(Survey, na.rm = TRUE),
      PSUs = n(),
      NB_Population = max(SumDist, na.rm = TRUE)
      ) |>
    dplyr::mutate(
      Cluster_size = round(Surveys / PSUs, 2),
      Cluster_size_init = input$cls,
      ICC = input$ICC,
      DESS = 1 + (Cluster_size - 1) * ICC,
      Effective_sample = round(Surveys / DESS, 0),
      Surveys_buffer = input$buf,
      Confidence_level = input$conf_level,
      Error_margin = input$e_marg,
      Sampling_type = input$samp_type
    )
  
  if(input$samp_type=="Cluster sampling"){
    for(i in 1:nrow(summary_sample)){
      if(summary_sample$strata_id[i]%in%sw_rand){
        summary_sample$Surveys_buffer[i]<-summary_sample$Surveys_buffer[i]+.1
        summary_sample$Cluster_size[i]<-1
        summary_sample$DESS[i]<-1
        summary_sample$Effective_sample[i]<-summary_sample$Surveys[i]
        summary_sample$Sampling_type[i]<-"Cluster sampling with size 1 = random sampling"
      }
    }
  }
  
  if(input$samp_type!="Cluster sampling"){
    le<-nrow(summary_sample)
    summary_sample$Cluster_size<-rep(NA,le)
    summary_sample$Cluster_size_init <-rep(NA,le)
    summary_sample$ICC<-rep(NA,le)
    summary_sample$DESS<-rep(NA,le)
    summary_sample$Effective_sample<-rep(NA,le)
  }
  
  if(input$topup=="Enter sample size"){
    le<-nrow(summary_sample)
    summary_sample$ICC<-rep(NA,le)
    summary_sample$DESS<-rep(NA,le)
    summary_sample$Effective_sample<-rep(NA,le)
    summary_sample$Error_margin<-rep(NA,le)
    summary_sample$Confidence_level<-rep(NA,le)
    summary_sample$Surveys_buffer<-rep(NA,le)
  }
  
  names(summary_sample)<-c("Stratification","# surveys", "# units to assess","Population","Mean Cluster size","Cluster size set","ICC","DESS","Effective sample","% buffer","Confidence level","Error margin","Sampling type")
  return(list(sample=dbout,summary_sample=summary_sample,sw_rand=sw_rand))
  
}

