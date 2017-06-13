
library(shiny)


##### data
############################################################

data_model_choices <- list(
  "Bernoulli" = "Bernoulli", 
  "Poisson" = "Poisson",
  "Normal (unknown variance)" = "Normal (unknown variance)",
  "Normal (known variance)" = "Normal (known variance)"
)


##### UI
############################################################

navbarPage("Graphical Elicitation MCMC",
  
  ##### main tabpanel
  ############################################################
  
  tabPanel("Main", icon = icon("home"),
    
  #  column(11, offset = 0,      # No. of columns app takes on screen
      
    navlistPanel(
      
      ##### select model
      ########################################
      tabPanel("Select Model", icon = icon("sign-in"),
        
        column(4,
          column(10,
            style = "padding-top: 20px;",
            
            selectInput("data_model", label = "Data Model", 
              choices = data_model_choices, selected = "Bernoulli"),
            
            strong("Prior Family"),
            verbatimTextOutput("prior_family"),
            tags$head(tags$style("#prior_family{
              font-family: helvetica; 
              font-size: 14px;}"
            ))
          ),
        		
          column(12,
            conditionalPanel(
              condition = "input.data_model == 'Normal (known variance)'",
              
              br(),
              p("What is the standard deviation of the population?"),
              numericInput("pop_sd", label = "Population SD", 10L, 
                width = "50%")
            )
          )
        ),

      	column(4,
        	conditionalPanel(
        		condition = "input.data_model == 'Bernoulli'",
        		
        		br(),
        		p("Suppose you are presented a new dataset of 100 observations. How many 
              successes would you expect (approximately)?"
        		),
        		numericInput("init_successes", label = "Expected Successes", 50, 
        		  width = "50%"
        		)
        	),
        	
        	conditionalPanel(
        		condition = "input.data_model != 'Bernoulli'",
        		
        		br(),
        		p("What is a typical value for the measurement (approximately)?"),
        		numericInput("init_value", label = "Typical Value", 50L, width = "50%")
        	),
        	
      		conditionalPanel(
      			condition = "input.data_model == 'Normal (unknown variance)'",
      			
      			p("What is the largest value you believe the measurement could be 
      			  (approximately)?"
      			),
      			numericInput("init_max", label = "Largest Value", 100L, width = "50%")
      		),
        	
					column(12,
						actionButton("set_inputs", label = "Set Inputs",
							class = "btn btn-primary")
        	)
        )  # end column
      ),  # end tabPanel("Select Model", ...)
      
        
      ##### start training
      ########################################
      tabPanel("Start Training", icon = icon("power-off"),
        conditionalPanel(
          condition = "input.data_model == 'Bernoulli'",
          
          column(7,
            h5("Suppose you are presented with a hypothetical dataset of size 100
              for your scenario."),
            h5("How many successes would you expect for such a dataset?"),
            h5("Look for expected and unexpected characteristics in the plots to
              get a better feel for natural variability in data."),
            
            style = "padding: 10px;"
            ),
          
          column(4,
            column(12,
              numericInput("training_successes", label = "Expected Successes", 
                50L, width = "60%")
            ),
            
            column(7,
              actionButton("generate_bern_training_plots", label = "Generate Graphs", 
                class = "btn btn-primary"),
              
              align = "center"
            )
          )
          ),
        
        conditionalPanel(
          condition = "input.data_model == 'Poisson'",
          
          column(7,
            h5("Suppose you are presented with a hypothetical dataset of size 100
              for your scenario."),
            h5("What is a typical value for your measurement?"),
            h5("Look for expected and unexpected characteristics in the plots to
              get a better feel for natural variability in data."),
            
            style = "padding: 10px;"
            ),
          
          column(4,
            column(12,
              numericInput("training_rate", label = "Typical Value", 25L, 
                width = "60%")
            ),
            
            column(7,
              actionButton("generate_pois_training_plots", 
                label = "Generate Graphs", class = "btn btn-primary"
              ),
              
              align = "center"
            )
          )
          ),
        
        conditionalPanel(
          condition = "input.data_model == 'Normal (known variance)'",
          
          column(7,
            h5("Suppose you are presented with a hypothetical dataset of size 100
              for your scenario."
            ),
            h5("What is a reasonable average value of your measurement?"),
            h5("What is the standard deviation of the population?"),
            h5("Look for expected and unexpected characteristics in the plots to
              get a better feel for natural variability in data."
            ),
            
            style = "padding: 10px;"
            ),
          
          column(4,
            column(12,
              numericInput("training_mean1", label = "Average", 100L, 
                width = "60%")
            ),
            
            column(12,
              numericInput("training_sd1", label = "Population SD", 10L,
                width = "60%")
            ),
            
            column(7,
              actionButton("generate_norm_kv_training_plots", 
                label = "Generate Graphs", 
                class = "btn btn-primary"
              ),
              
              align = "center"
            )
          )
        ),
        
        conditionalPanel(
          condition = "input.data_model == 'Normal (unknown variance)'",
          
          column(7,
            h5("Suppose you are presented with a hypothetical dataset of size 100
              for your scenario."),
            h5("What is a reasonable average value of your measurement?"),
            h5("What is a reasonable standard deviation of your measurement?"),
            h5("Look for expected and unexpected characteristics in the plots to
              get a better feel for natural variability in data."),
            
            style = "padding: 10px;"
          ),
          
          column(4,
            column(12,
              numericInput("training_mean2", label = "Average", 100L, 
                width = "60%")
            ),
            
            column(12,
              numericInput("training_sd2", label = "Standard Deviation", 10L,
                width = "60%")
            ),
            
            column(7,
              actionButton("generate_norm_uv_training_plots", 
                label = "Generate Graphs", 
                class = "btn btn-primary"
              ),
              
              align = "center"
            )
          )
        ),
        
        column(10, plotOutput("training_plots", height = 375)),
        column(2,	      		
          actionButton("new_training_plots", label = "Generate New Graphs", 
            class = "btn btn-primary"),
          style = "padding-top: 50px;"
        ) #,
        
        # column(6,
        #   checkboxInput("change_N", label = "Change Size of Hypothetical Dataset", 
        #     value = FALSE),
        #   
        #   conditionalPanel(
        #     condition = "input.change_N",
        #     
        #     h5("Change the number of observations in the hypothetical dataset."),
        #     
        #     numericInput("new_N", label = "Number of Observations", value = 100, 
        #       width = "45%"),
        #     actionButton("hypothetical_N", "Update", class = "btn btn-primary")
        #   )
        # )
      ),

      
      ##### make selections
      ########################################
      tabPanel("Select Graphs", icon = icon("random"),
        
        conditionalPanel(
          condition = "input.data_model == 'Bernoulli'",
          
          column(10,
            h4("Select an option below regarding how likely the two plots are 
                relative to one another."),
    				align = "center",
		      	style = "padding-top: 30px;"
          ),
          
      		column(2,
      			h4("Selections"),
      			verbatimTextOutput("selections_bern"),
      			tags$head(tags$style(type="text/css", "#selections_bern{max-width: 110px;}")),
      			align = "center"
      		),
          
        	column(4,
            actionButton("current_much_more_likely_bern", 
              label = "Much More",
      	      class = "btn btn-primary",
              width = "95px"
            ),
        	  style = "padding-left: 5%",
            actionButton("current_more_likely_bern",
              label = "25x More", 
              class = "btn btn-primary", 
              width = "95px"
            ),
            actionButton("current_little_more_likely_bern",
              label = "3x More",
      	      class = "btn btn-primary",
              width = "95px"
            )
        	),
      	  column(2,
            actionButton("equally_likely_bern", 
              label = "Equally", 
              class = "btn btn-primary", 
              width = "95px"
            ),
      	    style = "padding-left: 6.25%"
      	  ),
      	  column(2,
            actionButton("proposed_more_likely_bern", 
              label = "More", 
              class = "btn btn-primary", 
              width = "95px"
            ),
        	  style = "padding-left: 15%"
      	  ),

        	column(10, plotOutput("select_plot_bern", height = 350))
        ),
        
        conditionalPanel(
          condition = "input.data_model != 'Bernoulli'", 
                  
          column(10,
            h4("Select an option below regarding how likely the two plots are 
                relative to one another."),
    				align = "center",
		      	style = "padding-top: 30px;"
          ),
          
      		column(2,
      			h4("Selections"),
      			verbatimTextOutput("selections"),
      			tags$head(tags$style(type="text/css", "#selections{max-width: 110px;}")),
      			align = "center"
      		),
          
          column(10, plotOutput("select_plot_non_bern")),
          
          column(2,
            br(),
            actionButton("current_much_more_likely",
              label = "Much More",
      	      class = "btn btn-primary",
              width = "95px"),
            br(),
            actionButton("current_more_likely",
              label = "25x More", 
              class = "btn btn-primary",
              width = "95px"),
            br(),
            actionButton("current_little_more_likely",
              label = "3x More",
      	      class = "btn btn-primary",
              width = "95px"),
            br(),
            br(),
            br(),
            actionButton("equally_likely",
              label = "Equally", 
              class = "btn btn-primary", 
              width = "95px"),
            br(),
            br(),
            br(),
            actionButton("proposed_more_likely",
              label = "More", 
              class = "btn btn-primary", 
              width = "95px"),
            align = "center"
          )
        )
      ),
      
      
      ##### compute prior
      ########################################
      tabPanel("Compute Prior", icon = icon("flash"),
               
        conditionalPanel(
          condition = "input.data_model != 'Normal (unknown variance)'",

        	column(3,
          	strong("Prior Family"),
          	verbatimTextOutput("prior_family_output"),
          	tags$head(tags$style("#prior_family_output{font-family: helvetica;" 
          	  #font-size: 13px;}"
            )),

            strong("Elicited Parameters"),
            verbatimTextOutput("prior_params"),

            strong("Summaries"),
            verbatimTextOutput("prior_summaries"),

          	strong("Number of Selections"),
          	verbatimTextOutput("all_selections")
        	),
        	
        	column(9, plotOutput("prior_plot", height = 460)),
        		
      		column(3,
          	checkboxInput("more_prior_options", label = "Find Prior Probability", 
          		value = FALSE),

          	conditionalPanel(
          		condition = "input.more_prior_options",

          		column(12,
	          	 	h5("Find the probability between two values of the parameter 
                  (based on the elicited prior)."
	          	 	)
          		),
	          		
          		column(6, numericInput("lower_prob", "Lower Value", 0, width = "120%")),
		            
	          	column(6, numericInput("upper_prob", "Upper Value", 1, width = "120%")),
	          	
          		column(12,
	            	actionButton("interval_prob", "Find Probability", 
	            		class = "btn btn-primary"),
          			align = "center",
          			h3(""),
          			
          			column(10, offset = 1,
		          		verbatimTextOutput("prob_output"),
	          			align = "center"
          			)
          		)
          	)
      		),
        		
          column(2,
            checkboxInput("change_burnin", label = "Change Burn-in", value = FALSE),

            conditionalPanel(
              condition = "input.change_burnin",

              numericInput("new_burnin", label = "New Burn-in", value = 0),
              actionButton("burn_in", "Change Burn-in", class = "btn btn-primary")
            )
          ),

      		column(4,
	        	checkboxInput("add_kde", label = "Add Kernel Density Estimate to 
	        	  Plot", value = FALSE
	        	)
      		)
        ),

        conditionalPanel(
          condition = "input.data_model == 'Normal (unknown variance)'",
          
          column(3,
            # joint prior
            h3("Joint"),
            strong("Prior Family"),
            verbatimTextOutput("prior_family_output_joint"),
            tags$head(tags$style("#prior_family_output_joint{font-size: 13px;}")),
            
            strong("Elicited Parameters"),
            verbatimTextOutput("prior_params_joint"),
            
            # mean prior -- conditional on variance
            h3("Mean (conditional on variance)"),
            strong("Prior Family"),
            verbatimTextOutput("prior_family_output_mean"),
            tags$head(tags$style("#prior_family_output_mean{font-size: 13px;}")),
            
            strong("Elicited Parameters"),
            verbatimTextOutput("prior_params_mean"),
            p("(Sigma^2)* = mode of Inverse-Gamma prior on the variance"),
            
            strong("Summaries"),
            verbatimTextOutput("prior_summaries_mean"),
            
            strong("Number of Selections"),
            verbatimTextOutput("all_mean_selections"),
            
            # variance prior -- marginal
            h3("Variance (marginal)"),
            strong("Prior Family"),
            verbatimTextOutput("prior_family_output_var"),
            tags$head(tags$style("#prior_family_output_var{font-size: 13px;}")),
            
            strong("Elicited Parameters"),
            verbatimTextOutput("prior_params_var"),
            
            strong("Summaries"),
            verbatimTextOutput("prior_summaries_var"),
            
            strong("Number of Selections"),
            verbatimTextOutput("all_var_selections")
          ),
          
          column(9,
            style = "padding-top: 275px; padding-bottom: 50px;",
            
            # prior on mean plot
            plotOutput("prior_plot_normal_mean", height = 460),
            br(),
            br(),
            
            column(4,
              checkboxInput("change_normal_burnin", label = "Change Burn-in", 
                value = FALSE
              ),
              
              conditionalPanel(
                condition = "input.change_normal_burnin",
                
                numericInput("new_burnin_normal", label = "New Burn-in", 
                  value = 0, width = "60%"
                ),
                actionButton("burn_in_normal", "Update", class = "btn btn-primary")
              )
            ),
            
            # prior on variance plot
            plotOutput("prior_plot_normal_var", height = 460)
          )
        )
      ),
      
      
      ##### view trace plot
      ########################################
      tabPanel("View Trace Plot", icon = icon("line-chart"),
        conditionalPanel(
        	condition = "input.data_model != 'Normal (unknown variance)'",
        		
        	plotOutput("trace")
        ),

      	conditionalPanel(
      		condition = "input.data_model == 'Normal (unknown variance)'",

      		plotOutput("trace_normal_mean"),
      	  br(),
      		plotOutput("trace_normal_var"),
      	  br(),
      	  br(),
      	  plotOutput("trace_2d")
      	)
      ),
      
      
      ##### download report of pertinent info
      #######################################
    	tabPanel("Download Results", icon = icon("download"),
    		
    		column(4,
    			column(12,
      			h4("Download report of results"),
      			selectInput("format",
      				label = "File Type", 
      				choices = c("PDF", "Word", "HTML"),
      				selected = "PDF",
      				width = "50%")
    			),
    			
    			column(12,
    				downloadButton("download_report", "Download", class = "btn btn-primary"),
    				align = "center"
    			)
    		),
    		
    		column(5,	
    		  h4("Download selected values"),
    		  h5("File type: CSV"),
  				downloadButton("download_selected", "Download", class = "btn btn-primary"),
    			align = "center"
      	)
    	),
  
  
      # other arguments to navlistPanel()
      widths = c(2, 9)
  
    ) # end navlistPanel()
  
  ), # end tabPanel("Main", ...)
  
  
  ##### help tabpanel
  ############################################################
  
  tabPanel("Help", icon = icon("question-circle"),
    
    column(4,
      # select model
      h3("Select Model"),
      p("1. To start, you must input the following:"),
      p("(1) a data model"),
      p("(2) an approximate maximum value for the measurement (only for the 
        Normal data model with unknown variance). This number does not need to 
        be exact; its purpose is to initialize the algorithm."),
      p("(3) the population standard deviation (only for the Normal data model
        with known variance)"),
      p("Notes: the app defaults the prior family based on the chosen data 
        model. For the gamma prior, the parameterization used has mean 
        alpha/beta."),
      p("2. Once you have made these choices, click the 'Set Inputs' button. You 
        now have the option of moving to either the 'Start Training' tab or the 
        'Select Graphs' tab (if you wish to skip the training)."),
      hr(),
      
      # start training
      h3("Start Training"),
      p("Nine datasets, each of 100 hypothetical observations, are plotted based 
        on the chosen data model and either an expected number of successes
        (for a Bernoulli data model) or a typical measurement value (for the 
        other data models). These plots are meant to help you visualize the 
        natural variability that is often present in real datasets. Look for 
        expected and unexpected characteristics in the plots. This training 
        process is known as the Rorschach procedure."),
      p("Note: there are no selections to be made at this time."),
      p("Additional Options:"),
      p("(1) To see new sets of graphs, click the 'Generate New Graphs' button."),
      p("(2) To change the size of the hypothetical datasets for the training
        graphs, click the checkbox at the bottom of the screen and update the 
        number of observations."),
      hr()
    ),
    
    column(4,
      # select graphs
      h3("Select Graphs"),
      p("Five datasets, each with 100 observations, are plotted; these are based 
        on the inputs you specified in the 'Select Model' tab. You must select 
        one of five options: (1) the proposed graph is more likely than the 
        current graph, (2) the proposed and current graphs are equally likely, 
        (3) the current graph is 3 times more likely than the proposed graph, 
        (4) the current graph is 25 times more likely than the proposed graph, 
        or (5) the current graph is 10^6 times more likely than the proposed 
        graph. Once you select an option, two new graphs will be displayed. The 
        number of selections you must make is specified in the top-right corner 
        of the screen."),
      p("Note: you may not see any graphs that truly represent your beliefs,
        particularly at the beginning of the process. Choose the best option. 
        The algorithm may need a few selections to reach the point where you see 
        graphs that fit your beliefs."),
      hr(),
      
      # compute prior
      h3("Compute Prior"),
      p("After making the specified number of selections, you can move to the 
        'Compute Prior' tab. Here you will see (1) the prior family, (2) the 
        elicited parameters for this family, (3) summaries of the elicited 
        prior, (4) the total number of selections made, the number kept, and the 
        number discarded (referred to as the 'burn-in'), and (5) a density plot 
        of the elicited prior."),
      p("Additional Options:"),
      p("(1) Find Prior Probability: allows you to find the probability between 
        two values of the parameter (based on the elicited prior)."),
      p("(2) Change Burn-in: allows you to discard the first few values (you set
        how many); defaulted to 0."),
      p("(3) Plot Options: allows you to add a kernel density estimate (KDE) of 
        the accepted parameter values as well as change the x-axis values for 
        the density plot."),
      hr()
    ),
    
    column(4,
      # view trace plot
      h3("View Trace Plot"),
      p("After making the specified number of selections, you can view a plot of 
        the parameter values at each stage: proportions for a Bernoulli data 
        model, rates for a Poisson model, means for a Normal model with known 
        variance, and means and variances for a Normal model with unknown 
        variance)."),
      hr(),
      
      # download results
      h3("Download Results"),
      p("If you want to save your results, you can download two files:"),
      p("(1) a report that contains all of your inputs as well as all output 
        from the 'Compute Prior' and 'View History Plot' tabs. Three file types
        (PDF, Word, and HTML) are available for this document, which is created
        using R Markdown, and"),
      p("(2) a CSV file that contains all of the values used by the algorithm.")
    )
  ),
  
  
  ##### about tabpanel
  ############################################################
  
  tabPanel("About", icon = icon("info-circle"),
    
    column(4,
      
      # information about the developers
      h4("Developers"),
      p("This tool was developed by Chris Casement (Ph.D. candidate) and David 
        Kahle (Associate Professor), both of the Department of Statistical 
        Science at Baylor University."),
      br(),
      
      # information about the app
      h4("About the App"),
      p("This tool elicits prior distributions for Bayesian analyses by drawing
        on recent developments in the graphical inference literature on visual 
        hypothesis testing in addition to Metropolis-Hastings sampling."),
      br(),
      
      # contact info
      h4("Contact"),
      p("Email: casementc@gmail.com"),
      br(),
      br(),
      
      # copyright statement
      p("Copyright (c) 2017 Christopher J. Casement & David J. Kahle."),
      p("The license statement can be found", 
        a("here.", href = "https://opensource.org/licenses/MIT", target = "_blank")
      )
    )
  )
) # end navbarPage()

