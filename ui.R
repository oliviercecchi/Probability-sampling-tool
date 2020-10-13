library(markdown)

navbarPage("Probability sampling tool",
	tags$head(
		tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")
	),
	tabPanel("Introduction",
		fluidRow(
			h1("Brief overview of the tool"),
			align = "center"
		),
		fluidRow(
			column(1,
				br()
			),
			column(4,
			    h2("Mode of sampling"),	
					  p(strong("Enter sample size:"),"You can input the target sample size"),
					  p(strong("Sample size based on population:"),"The sample size will be based on confidence level, error margin and proportion"),
				
					h2("Type of sampling"),
				
						p(strong("Random sampling:"),"Simple Random sampling, a random sample directly from the sampling frame which consists of every unit in the population of interest, thus ensuring equal probability of each unit to be selected."),
						p(strong("2 stages random - st1:"),"First stage of 2 stages random sampling. Units (PSUs) are first randomly selected with probabilities based on population size to ensure equal probability of each unit (e.g. households) during the second stage to be selected."),
						p(strong("Cluster sampling:"),"Clusters, or Primary Sampling Units (PSUs), are first randomly selected, before a set number of units (e.g. households) at each cluster is randomly selected."),
						p(strong("Stratified:"),"If ticked, a variable characterizing the stratification of the sampling will be ask to stratify the sample."),
					
					h2("Parameters"),
						p(strong("Confidence level:"),"The desired confidence level."),
						p(strong("Error Margin:"),"The desired error margin."),
						p(strong("Proportion:"),"The expected proportion in the sample."),
						p(strong("Buffer:"),"The desired percentage buffer."),
						p(strong("Cluster size:"),"The minimum number of interviews to conduct by cluster."),
						p(strong("ICC:"),"Intracluster  correlation : average value estimated in previous assessments = 0.06.")
			),
			column(1,br()),
			column(4,
				h2("Loading files"),
					p("The app takes only files .csv; each row of the csv need to be the primary sampling unit.",strong("The headers of the dataset must NOT contain special characters."),("You can use some example data by ticking 'example data'")),
					p("When the files is loaded, depending of the type of sampling loaded before, some parameters will have to be specify:"),
					p(strong("Cluster:"),"The variable including the name of each cluster, there should be no duplciates in this column."),
					p(strong("Stratification:"),"The variable in the data defining the stratification."),
					p(strong("Population:"),"The variable in the data defining the population number by PSU.",strong("Must be > 0 and > to cluster size")),
					p(strong("When all this parameters are defined please press'Apply'.")),
					
				h2("Computing target"),
					p("Under the target section will be compute the target based on the confidence level and margin error defined previously."),
					p("Click Apply to compute the target"),
					
				h2("Sampling"),
					p("When target appear in the tab, go to the 'Sampling tab' and click 'Sample!'. You can then download the list of units sampled and the sampling summary.")
			)
		)
	),
	tabPanel("Sampling frame",
		fluidRow(
		 h4("Sampling type",style="color:#707070;background-color:#f8f8f8"),
		 
  		column(2,
  		   selectInput("topup", "Mode of sampling", c("Enter sample size","Sample size based on population"),"Sample size based on population")
  		),
			column(2,						
				selectInput("samp_type", "Type of sampling", c("Simple random","2 stages random - st1","Cluster sampling"))
			),
			column(2,
			  selectInput("stratified", "Stratified ?", c("Not stratified","Stratified"))
			)
		),
    fluidRow(
		 h4("Sampling parameters",style="color:#707070;background-color:#f8f8f8"),
		 conditionalPanel(condition = "input.topup != 'Enter sample size'",column(2,						
				numericInput('conf_level', 'Confidence level',0.95,min=0.70,	max=0.99, step =0.005)
				)),
			conditionalPanel(condition = "input.topup != 'Enter sample size'",column(2,						
				numericInput('e_marg', 'Error Margin',0.05,min=0.01,	max=0.25, step =0.005)
			)),
			conditionalPanel(condition = "input.topup != 'Enter sample size'",column(2,						
				numericInput('pror', 'Proportion',0.5,min=0.01,	max=1, step =0.05)
			)),
			conditionalPanel(condition = "input.topup == 'Enter sample size'",column(2,						
		      numericInput('target', 'Enter target sample size', 0 ,min=0,	max=10000, step=1)
			)),
		  column(2,						
		     numericInput('buf', 'Buffer',0.05,min=0.00,	max=0.50, step =0.01)
		  )
		),
		fluidRow(
			column(6,	
				conditionalPanel(condition = "input.samp_type == 'Cluster sampling'",
					h4("Cluster Sampling",style="color:#707070;background-color:#f8f8f8"),
					column(6,						
						numericInput('cls', 'Cluster size',5,min=2, step =1)
					),
					column(6,						
						numericInput('ICC', 'ICC',0.06,min=0.01,	max=0.5, step =0.01)
					)
				),
				h4("Set up sampling frame",style="color:#707070;background-color:#f8f8f8"),
			    column(6,
					conditionalPanel(condition = "!input.testdata",
						fileInput('popdata', 'Choose CSV file',accept=c('text/csv', 'text/comma-separated-values,text/plain'))
					),
					checkboxInput("testdata", "Use test data", FALSE),
					conditionalPanel(condition = "input.samp_type == 'Cluster sampling'",
						selectInput('col_psu', 'Input cluster', choices = NULL)
				)),
				column(6,					
					conditionalPanel(condition = "input.stratified=='Stratified'",
						selectInput('strata', 'Input strata', choices = NULL, multiple = F)
					),
					conditionalPanel(condition = "input.samp_type == 'Cluster sampling'|input.samp_type =='2 stages random - st1'",
						selectInput('colpop', 'Input population', choices = NULL)
					)
				)
			),
			column(6,
				h4("TARGET sampling",style="color:#707070;background-color:#f8f8f8"),
				DT::dataTableOutput("target_frame")
			)
		),
		fluidRow(
			#column(6,	
				actionButton("f_apply", "Apply",width ="300",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
			align = "center"
			#),
			#column(6,	
			#	actionButton("t_apply", "Apply",width ="300",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
			#align = "center"
			#)
		),
		fluidRow(
			br(),
			br()
					
		),
		br(),
		br(),
		fluidRow(
				h3("Sampling frame"),
				DT::dataTableOutput("sampling_frame"),width=6
		)
	),
	tabPanel("Sampling", 
		sidebarLayout(
			sidebarPanel(
				fluidRow(
					actionButton("refresh", "Refresh", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
					br(),
					downloadButton("downloadBtn2", "Download Summary"),
					br(),
					downloadButton("downloadBtn", "Download SAMPLE"),
					h3(textOutput("caption"))
					
				),
				width = 2
			),
			mainPanel(
				fluidRow(
					actionButton("desButton", "Sample!",width ="200",style="color: #fff; background-color: #f15e05; border-color: #2e6da4"),
					align = "center"
				),
				h3("Sample summary"),
					DT::dataTableOutput("test_1"),
					br(),
					br(),
					br(),
					br(),
				h3("Sample"),
					DT::dataTableOutput("sampling_output")
			)
		)
	)
)


