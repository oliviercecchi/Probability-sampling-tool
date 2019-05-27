function(input, output, session) {
  
  # get the data
   db <- reactive({
		inFile <- input$popdata
		if(input$testdata==TRUE){
			read.csv("data_example.csv")
		}else if (is.null(inFile)){
			return(NULL)
		}else{
			read.csv(inFile$datapath)
		}
	})
  
  # update the check ox based on the data headings
  observe({
		updateSelectInput(session, "col_psu", choices = c("None",colnames(db())),selected="None")
		updateSelectInput(session, "strata", choices = c("None",colnames(db())),selected="None")
		updateSelectInput(session, "colpop", choices = c("None",colnames(db())),selected="None")
	})
  
  # create the sampling frame
  frame<-eventReactive(input$f_apply,{
		cens<-create_sampling_frame(db(),input)
	})
	
	cible<-eventReactive(input$f_apply,{
	  if(input$topup=="Enter sample size"){
	    plyr::ddply(frame(),"strata_id", summarise, 
	                Population = max(SumDist,na.rm=T),
	                target = input$target,
	                target.with.buffer = ceiling(target * (1+input$buf))
	    )	%>% as.data.frame
	  } else {
  		plyr::ddply(frame(),"strata_id", summarise, 
  			Population = max(SumDist,na.rm=T),
  			target = ceiling(Ssize(Population,input$conf_level,input$pror,input$e_marg)),
  			target.with.buffer = ceiling(target * (1+input$buf))
  		)	%>% as.data.frame
	  }
	})
	
  # Display the results
	output$sampling_frame <- DT::renderDataTable(
	  frame(),
	  rownames = FALSE,
	  options = list(searching = FALSE, lengthChange = FALSE)
	)
	
	output$target_frame <- DT::renderDataTable(
		  cible(),
		  rownames = FALSE,
		  options = list(searching = FALSE, lengthChange = F)
	)
	
  # sample	
  out<-eventReactive(input$desButton,{

		cens<-frame()
		sam<-cible()
		sw_rand<-c()
		output<-c()
		
		cls<-input$cls
		buf<-input$buf
		ICC<-input$ICC
		
		if(input$samp_type=="Cluster sampling"){
		  
			withProgress(message = 'Select PSUs randomly', style="notification", detail = "Sampling", value = 0, {
				Sys.sleep(0.25)
			  if(input$topup=="Enter sample size"){
			    clsamapling<-apply(sam,1,clustersample,cens=cens,cls=cls,buf=buf,ICC=0) 
			    output<-lapply(clsamapling,function(x) x$output) %>% unlist %>% c
			    sw_rand<-lapply(clsamapling,function(x) x$sw_rand) %>% unlist %>% c
			  }else {
			    clsamapling<-apply(sam,1,clustersample,cens=cens,cls=cls,buf=buf,ICC=ICC) 
			    output<-lapply(clsamapling,function(x) x$output) %>% unlist %>% c
			    sw_rand<-lapply(clsamapling,function(x) x$sw_rand) %>% unlist %>% c
			  }
			  Sys.sleep(1)
			})	
		} else if (input$samp_type=="2 stages random - st1"){
			withProgress(message = 'Select PSUs randomly', detail = "Sampling", value = 0, {
			  Sys.sleep(0.25)
			  output<-apply(sam,1,stage2rdsample,cens=cens,buf=buf) %>% unlist 
			  Sys.sleep(1)
			 })
  	} else if (input$samp_type=="Simple random"){
  		withProgress(message = 'Select PSUs randomly', detail = "Sampling", value = 0, {
  			Sys.sleep(0.25)
  		  output<-apply(sam,1,randomsample,cens=cens,buf=buf) %>% unlist 
  			Sys.sleep(1)
  		})
  	}
  	
  	output<-as.data.frame(table(output))
  	dbout<-merge(output,cens,by.x="output",by.y="id_sampl",all.x=T,all.y=F)
  	 
  	if(input$samp_type=="Cluster sampling"){  
  		dbout$Freq<-ifelse(dbout$strata%in%sw_rand,dbout$Freq,dbout$Freq*cls)
  	}
  	
  	names(dbout)<-recode(names(dbout),"'output'='id_sampl';'Freq'='Survey'")
  	dbout$survey_buffer<-dbout$Survey
		
		summary_sample<- plyr::ddply(dbout,"strata_id", summarise, 
			Surveys = sum(Survey,na.rm=T), 
			PSUs = length(strata_id), 
			Cluster_size = round(Surveys/PSUs,2),
			Cluster_size_init = cls,
			ICC = ICC,
			DESS = 1+(Cluster_size-1)*ICC,
			Effective_sample = round(Surveys / DESS,0),
			Surveys_buffer = input$buf,
			Confidence_level = input$conf_level,
			Error_margin = input$e_marg,
			NB_Population = max(SumDist,na.rm=T),
			Sampling_type = input$samp_type
		)

  	if(input$samp_type=="Cluster sampling"){
  			for(i in 1:nrow(summary_sample)){
  				if(summary_sample$strata[i]%in%sw_rand){
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
		  summary_sample$Cluster_size<-rep(NA,le)
		  summary_sample$Cluster_size_init <-rep(NA,le)
		  summary_sample$ICC<-rep(NA,le)
		  summary_sample$DESS<-rep(NA,le)
		  summary_sample$Effective_sample<-rep(NA,le)
		  summary_sample$Error_margin<-rep(NA,le)
		  summary_sample$Confidence_level<-rep(NA,le)
		}
		
  	tab<-paste0("Sampling_output",gsub(":","_",Sys.time()),".xlsx")
  	
  	names(summary_sample)<-c("Stratification","# surveys", "# units to assess","Cluster size","Cluster size set","ICC","DESS","Effective sample","% buffer","Confidence level","Error margin","Population","Sampling type")
  	list(dbout,summary_sample,sw_rand)

  })
  	
  # diplay
  
	output$sampling_output <- DT::renderDataTable(
		  out()[[1]],
		  rownames = FALSE,
		  options = list(searching = FALSE)
	)
	
	output$test_1 <- DT::renderDataTable(
		  out()[[2]],
		  rownames = FALSE,
		  options = list(searching = FALSE, lengthChange = FALSE)
	)
		
	observeEvent(input$refresh, {
		output$test_1 <- DT::renderDataTable(
			out()[[2]],
			rownames = FALSE,
			options = list(searching = FALSE, lengthChange = FALSE)
		)
	})
	
	
	observeEvent(input$refresh, {
		output$sampling_output <- DT::renderDataTable(
			out()[[1]],
			rownames = FALSE,
			options = list(searching = FALSE, lengthChange = FALSE)
		)
	})
	
	output$downloadBtn <- downloadHandler(
		filename = function() { paste("sampling_frame",humanTime(), '.csv', sep='') },
		content = function(file) {
		  write.csv(out()[[1]], file)
		}
	)
	
	output$downloadBtn2 <- downloadHandler(
		filename = function() { paste("sampling_summary",humanTime(), '.csv', sep='') },
		content = function(file) {
		  write.csv(out()[[2]], file)
		}
	)		
	

	
	
}

