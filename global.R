require(data.table)
require(plyr)
require(dplyr)
require(car)
require(reshape2)
require(stringr)
library(shiny)
library(shinyjs)
library(crayon)

options(shiny.maxRequestSize=30*1024^2)

Ssize<-function (x,A,p,E) {(qchisq(A,1)*x*p*(1-p)) / (E^2*(x-1)+qchisq(A,df=1)*p*(1-p))}

humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

# create the smapling frame
create_sampling_frame<-function(cens,input){
  cens$id_sampl<-paste0("id_",row.names(cens))
  if(input$stratified=="Stratified"){
    cens$strata_id<-cens[[as.character(input$strata)]]
  } else {
    cens$strata_id<-rep("all",nrow(cens))
  }
  
  if(input$samp_type=="Cluster sampling"){
    cens$psu_id<-cens[[as.character(input$col_psu)]]
    cens$pop_numbers<-cens[[as.character(input$colpop)]]
	
  }else if(input$samp_type=="2 stages random - st1"){
    cens$psu_id<-cens$id_sampl
    cens$pop_numbers<-cens[[as.character(input$colpop)]]
  
  }else{
    cens$psu_id<-cens$id_sampl
    cens$pop_numbers<-rep(1,nrow(cens))
  }
  
  sumdist<-cens %>% dplyr::group_by(strata_id) %>%  dplyr::summarise(SumDist = sum(pop_numbers,na.rm=T))
  cens<-merge(cens,sumdist,by="strata_id")
  proba<-as.numeric(cens$pop_numbers)/as.numeric(cens$SumDist)
  cens<-cbind(cens,proba)
  cens<-cens[!is.na(cens$proba),]
  
  if(input$samp_type=="Cluster sampling"){
    cens$psu_id<-factor(cens$psu_id)
  }
  return(cens) 
}


clustersample<-function(cens,sam,cls,buf,ICC,sw_rand=c()) {

  target<-as.numeric(as.character(sam[["target"]]))
  dist<-as.character(sam[["strata_id"]])
  
  out<-something(cens,cls,buf,ICC,dist,target)
  
  if(is.null(out)){
    dbr<-cens[as.character(cens$strata_id)==dist,]
    out<-sample(as.character(dbr$id_sampl),ceiling(as.numeric(sam[["target"]])*(1+buf+0.1)),prob=dbr$proba,replace=TRUE)
    
    # out<-something(cens,1,buf,ICC,dist,target,mode="forced")
   showModal(modalDialog(
      title = paste(dist,": All PSUs have been selected"),
      "Set cluster size to 1 to reduce the design  effect and extra buffer to account for analysis DESS",
      easyClose = TRUE,
      footer = NULL
    ))
    sw_rand<-c(sw_rand,dist)
  }
  incProgress(round(1/nrow(sam),2), detail = paste("Sampling", dist))
  return(list(output=out,sw_rand=sw_rand))
}


randomsample<-function(cens,sam,buf){
  dist<-as.character(sam[["strata_id"]])
  dbr<-cens[as.character(cens$strata_id)==dist,]
  tosample<-as.numeric(sam[["target.with.buffer"]])
  pop<-as.numeric(sam[["Population"]])
  if(tosample>pop){target<-pop}else{target<-tosample}
  out<-sample(as.character(dbr$id_sampl),target,replace=FALSE)
  incProgress(round(1/nrow(sam),2), detail = paste("Sampling", dist))
  return(out)
}

stage2rdsample<-function(cens,sam,buf){
  dist<-as.character(sam[["strata_id"]])
  dbr<-cens[as.character(cens$strata_id)==dist,]  
  tosample<-as.numeric(sam[["target.with.buffer"]])
  pop<-as.numeric(sam[["Population"]])
  if(tosample>pop){target<-pop}else{target<-tosample}
  out<-sample(as.character(dbr$id_sampl),target,prob=dbr$proba,replace=TRUE)
  incProgress(round(1/nrow(sam),2), detail = paste("Sampling", dist))
  return(out)
}


something<-function(cens,cls,buf,ICC,dist,target,mode="notforced"){
  Sys.sleep(0.25)
  dbr<-cens[as.character(cens$strata_id)==dist,]
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


simulate_sample<-function(input_main,sampling_frame,titl="sampling"){
  
  for (j in 1:nrow(input_main)){
    
    params<-input_main[j,]
    frame<-prepare_sampling_frame(sampling_frame,ch(params$popu),ch(params$stratat),params$samp_type)
    
    sam<-prepare_sampling_target(frame,params)
    sasampl<-sssampling(sam,frame,params,titl)
    
    params$total_surveys<-sum(sasampl[[2]][["# surveys (buffer)"]],na.rm=T)
    params$total_unit<-sum(sasampl[[2]][["# units to assess"]],na.rm=T)
    params$group<-titl
    params$number_disaggregation<-nrow(sasampl[[2]])
    
    if(j==1){myresults<-params}else{myresults<-rbind(myresults,params)}
    
  }
  return(myresults)
  
}






