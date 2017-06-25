
library(shiny)
library(ggplot2); theme_set(theme_bw(18))
library(invgamma)
library(rmarkdown)


##### shinyServer
############################################################

shinyServer(function(input, output, session) {
  
  ##### global data
  ########################################
  
  `%is_in_interval%` <- function(x, y) y[1] <= x & x <= y[2]
  `%is_not_in_interval%` <- function(x, y) !(y[1] <= x & x <= y[2])
  
  smallest_lambda <- function(x, q = 0.99, L = 0.001, U = 1e6){
    f <- function(la) ppois(x, la, lower.tail = FALSE) - q
    uniroot(f, lower = L, upper = U)$root
  }

  biggest_lambda <- function(x, q = 0.99, L = 0.001, U = 1e6){
    f <- function(la) ppois(x, la) - q
    uniroot(f, lower = L, upper = U)$root
  }
  
  # normal-inv-gamma density function
  dnorminvgamma <- function(x, mu0, lam, alpha, beta, log = FALSE) {
    if (log == FALSE) {
      if (is.matrix(x) == FALSE) {
        (sqrt(lam) * beta^alpha) / (sqrt(2*pi)*gamma(alpha)) * 
          x[2]^(-alpha-3/2) * exp(-(2*beta + 0.5*lam*(x[1]-mu0)^2)/x[2])
      } else {
        apply(x, 1, function(x) {
          (sqrt(lam) * beta^alpha) / (sqrt(2*pi)*gamma(alpha)) * 
            x[2]^(-alpha-3/2) * exp(-(2*beta + 0.5*lam*(x[1]-mu0)^2)/x[2])
        })
      }
    } else {
      if (is.matrix(x) == FALSE) {
        0.5*(log(lam) - log(2*pi)) + alpha*log(beta) - lgamma(alpha) -
          (alpha + 3/2)*log(x[2]) - beta/x[2] - 0.5*lam*(x[1] - mu0)^2/x[2]
      } else {
        apply(x, 1, function(x) {
          0.5*(log(lam) - log(2*pi)) + alpha*log(beta) - lgamma(alpha) -
            (alpha + 3/2)*log(x[2]) - beta/x[2] - 0.5*lam*(x[1] - mu0)^2/x[2]
        })
      }
    }
  }
  
  
  ##### define proposal generating function
  ########################################
  
  generate_proposal <- function(current, data_model) {
    
    # define parameter space and set proposal std dev
    if(data_model != "Normal (unknown variance)") {
      parameter_space <- switch(data_model,
        Bernoulli = c(0, 1),
        Poisson = c(0, Inf),
        "Normal (known variance)" = c(-Inf, Inf)
      )
      
      proposal_sd <- switch(data_model,
        Bernoulli = 0.05,
        Poisson = sqrt(input$init_value),
        "Normal (known variance)" = 2*sqrt(input$pop_sd)
      )
    } else {
      # mean case
      if(bag$mean_counter == bag$var_counter) {
        parameter_space <- c(-Inf, Inf)
        proposal_sd <- 2*sqrt(bag$init_sd)  # defined later in observeEvent(input$set_inputs)
      } else {       
        # variance case
        parameter_space <- c(0, Inf)
        proposal_sd <- bag$init_sd^2
      }
    }

    # generate proposal
    proposed <- current + rnorm(1, 0, proposal_sd)
    while(proposed %is_not_in_interval% parameter_space) {
      proposed <- current + rnorm(1, 0, proposal_sd)
    }
    
    # return proposal
    bag$proposed <<- proposed
  }
  
  
  ##### define dataset generating functions
  ########################################
  
  generate_data <- function(action) {
    
    u <- runif(bag$N)
    
    if(input$data_model != "Normal (unknown variance)") {
        bag$current_samples  <<- bag$init_rng_transform(u, bag$current)
        bag$proposed_samples <<- bag$init_rng_transform(u, bag$proposed)
    } else {
      bag$current_samples <<- bag$init_rng_transform(u, bag$current_mean, 
        sqrt(bag$current_var)
      )
      
      if(bag$mean_counter != bag$var_counter) {
        bag$proposed_samples <<- bag$init_rng_transform(u, bag$current_mean, 
          sqrt(bag$proposed)
        )
      } else {
        bag$proposed_samples <<- bag$init_rng_transform(u, bag$proposed, 
          sqrt(bag$current_var)
        )
      }
    }

    bag$select_data <<- rbind(
      data.frame(x = bag$current_samples, type = "Current", 
        stringsAsFactors = FALSE
      ), 
      data.frame(x = bag$proposed_samples, type = "Proposed", 
        stringsAsFactors = FALSE
      )
    )
    
  }

  
  ##### construct and initialize reactive stuff
  ############################################################
  
  bag <- reactiveValues()
  
  # default inputs
  bag$N <- 100   # hypothetical future sample size
  bag$total_bern_selections <- 100   # min number of selections for bern case
  bag$total_selections <- 100        # min number of selections for other cases
  
  # counters
  bag$counter <- 0
  bag$mean_counter <- 0
  bag$var_counter <- 0
  
  # storage
  bag$chain <- data.frame(step = 0, state = 1)
  bag$chain_normal_mean <- data.frame(step = 0, state = 1)
  bag$chain_normal_var <- data.frame(step = 0, state = 1)
  
  # initialize proposed
  bag$proposed <- 0
  
  # burn-in
  bag$burnin <- 0
  bag$burnin_normal <- 0
  

  ##### update chain
  ############################################################  
  
  update_chain <- function(next_state) {

      # all cases except normal (unknown var)
    if(input$data_model != "Normal (unknown variance)") {
      # update number of selections
      bag$counter <<- bag$counter + 1
      
      bag$chain <<- rbind(
        bag$chain, 
        data.frame(step = bag$counter, state = next_state)
      )
    }
    
    # normal (unknown var) case
    if(input$data_model == "Normal (unknown variance)") {
      if(bag$mean_counter == bag$var_counter) {
        bag$mean_counter <<- bag$mean_counter + 1
        
        bag$chain_normal_mean <<- rbind(
          bag$chain_normal_mean,
          data.frame(step = bag$mean_counter, state = next_state)
        )
      } else {
        bag$var_counter <<- bag$var_counter + 1
        
        bag$chain_normal_var <<- rbind(
          bag$chain_normal_var,
          data.frame(step = bag$var_counter, state = next_state)
        )
      }
    }
  }
  
  
  ##### generate training plots
  ###########################################################
  
  make_training_plots <- function() {
  	
  	# number of plots
		samples <- 1:9

		# bernoulli case
  	if(input$data_model == "Bernoulli") {
  	  training_datasets <- lapply(seq_along(samples), function(samples) {
      	data.frame(
      	  x = rbinom(bag$N, 1, input$training_successes / bag$N), 
      	  samp = samples,
      		stringsAsFactors = FALSE)
    	})
  	  training_df <- do.call(rbind, training_datasets)
  	  
  		g <- ggplot(training_df, aes(x = factor(x, levels = 0:1))) +
  			geom_bar() +
  			facet_wrap(~ samp) + 
  			scale_x_discrete("", breaks = 0:1, labels = c("Failure", "Success"))
  	
  	} else if(input$data_model == "Poisson") {
  	  training_datasets <- lapply(seq_along(samples), function(samples) {
      	data.frame(
      	  x = rpois(bag$N, input$training_rate), 
      	  samp = samples, 
      		stringsAsFactors = FALSE)
    	})
  	  training_df <- do.call(rbind, training_datasets)
  		
    	g <- ggplot(training_df, aes(x = x)) + 
    	  geom_histogram(bins = nclass.scott(training_df$x)) +
    		facet_wrap(~ samp) + #, scales = "free_y") +
      	scale_x_continuous("Number of Occurrences")
    	
  	} else if(input$data_model == "Normal (known variance)") {
  	  training_datasets <- lapply(seq_along(samples), function(samples) {
      	data.frame(
      	  x = rnorm(bag$N, input$training_mean1, input$training_sd1), 
      	  samp = samples,
      		stringsAsFactors = FALSE)
    	})
  	  training_df <- do.call(rbind, training_datasets)
  	  
    	g <- ggplot(training_df, aes(x = x)) + 
    	  geom_histogram(bins = nclass.scott(training_df$x)) +
    		facet_wrap(~ samp) + #, scales = "free_y") +
      	scale_x_continuous(NULL)
    	
  	} else {
			training_datasets <- lapply(seq_along(samples), function(samples) {
				data.frame(
				  x = rnorm(bag$N, input$training_mean2, input$training_sd2), 
				  samp = samples, 
					stringsAsFactors = FALSE)
			})
			training_df <- do.call(rbind, training_datasets)

    	g <- ggplot(training_df, aes(x = x)) +
    	  geom_histogram(bins = nclass.scott(training_df$x)) +
    		facet_wrap(~ samp) + #, scales = "free_y") +
      	scale_x_continuous(NULL)
		}

  	# return plot
  	g + ylab("Count") + 
  		theme(
  		  axis.title.x = element_text(margin = margin(15, 0, 0, 0)), 
    		axis.title.y = element_text(margin = margin(0, 10, 0, 0))
  		)
  }
  

  ###### set prior family
  ########################################
  observeEvent(input$data_model, {
  	
    # set prior family
    bag$prior_fam <- switch(input$data_model,
      "Bernoulli" = "Beta",
      "Poisson" = "Gamma",
      "Normal (known variance)" = "Normal",
      "Normal (unknown variance)" = "Normal-Inverse-Gamma"
    )
  	
  	output$prior_family <- renderText(bag$prior_fam)
  	output$prior_family_output <- renderText(bag$prior_fam)
  	output$prior_family_output_mean <- renderText("Normal")
  	output$prior_family_output_var <- renderText("Inverse-Gamma")
  	output$prior_family_output_joint <- renderText("Normal-Inverse-Gamma")
  	
    # set prior density
  	bag$prior_density <- switch(input$data_model, 
			Bernoulli = dbeta,
			Poisson = dgamma,
			"Normal (known variance)" = dnorm,
  	  "Normal (unknown variance)" = dnorminvgamma
		)
  })
  
  
  # create training plots
  observeEvent(input$generate_bern_training_plots, {
    output$training_plots <- renderPlot( make_training_plots() )
  })
  observeEvent(input$generate_pois_training_plots, {
    output$training_plots <- renderPlot( make_training_plots() )
  })
  observeEvent(input$generate_norm_kv_training_plots, {
    output$training_plots <- renderPlot( make_training_plots() )
  })
  observeEvent(input$generate_norm_uv_training_plots, {
    output$training_plots <- renderPlot( make_training_plots() )
  })
  
  # make new training plots
  observeEvent(input$new_training_plots, {
    output$training_plots <- renderPlot( make_training_plots() )
  })
  
 
  ##### initial parameters stuff
  ########################################
  
  observeEvent(input$set_inputs, {
    
    # define random number generator functions
    bag$init_rng <- switch(input$data_model,
      Bernoulli = function(n, th) rbinom(n, 1, th),
      Poisson = function(n, th) rpois(n, th),
      "Normal (known variance)" = function(n, th) rnorm(n, th, input$pop_sd),
    	"Normal (unknown variance)" = function(n, th1, th2) rnorm(n, th1, th2)
    )
    
    # define random number transform functions
    bag$init_rng_transform <- switch(input$data_model,
      Bernoulli = function(u, th) qbinom(u, 1, th),
      Poisson = function(u, th) qpois(u, th),
      "Normal (known variance)" = function(u, th) qnorm(u, th, input$pop_sd),
    	"Normal (unknown variance)" = function(u, th1, th2) qnorm(u, th1, th2)
    )
    
    if(input$data_model == "Bernoulli") {
      bag$chain$state <- bag$current <- input$init_successes / bag$N
      bag$current_samples <- bag$init_rng(bag$N, bag$current)
      generate_proposal(input$init_value / bag$N, input$data_model)
      generate_data("reject")      
    } else if(input$data_model == "Poisson" || 
        input$data_model == "Normal (known variance)"
    ) {
      bag$chain$state <- bag$current <- input$init_value
      bag$current_samples <- bag$init_rng(bag$N, bag$current)
      generate_proposal(input$init_value, input$data_model)
      generate_data("reject")
    } else {
      bag$chain_normal_mean$state <- bag$current_mean <- input$init_value
      bag$init_sd <- (input$init_max - input$init_value) / 3
      bag$chain_normal_var$state <- bag$current_var <- bag$init_sd^2
      bag$current_samples <- bag$init_rng(bag$N, bag$current_mean, sqrt(bag$current_var))
      generate_proposal(input$init_value, input$data_model)
      generate_data("reject")
    }
  })
  
  
  # make new training plots
	observeEvent(input$new_training_plots, {
		output$training_plots <- renderPlot( make_training_plots() )
	})

	
  ##### MCMC
  ########################################

	# determine if proposal accepted or rejected
  transition <- function(accept_prob) {
    if (runif(1) < accept_prob) accept()
    else reject()
  }
  
	# what to do if accept proposal
  accept <- function() {
    update_chain(bag$proposed)
    
    if(input$data_model == "Normal (unknown variance)") {
      if(bag$mean_counter != bag$var_counter){
        bag$current_mean <- bag$proposed
        generate_proposal(bag$current_var, input$data_model)
        generate_data("accept")
      } else {
        bag$current_var <- bag$proposed
        generate_proposal(bag$current_mean, input$data_model)
        generate_data("accept")        
      }
    } else {
      bag$current <<- bag$proposed
      generate_proposal(bag$current, input$data_model)
      generate_data("accept")
    }
  }
  
  # what to do if reject proposal
  reject <- function() {
    
    if(input$data_model != "Normal (unknown variance)") {
      update_chain(bag$current)
      generate_proposal(bag$current, input$data_model)
      generate_data("reject")
    } else {
      if(bag$mean_counter != bag$var_counter){
        update_chain(bag$current_var)
        generate_proposal(bag$current_mean, input$data_model)
        generate_data("reject")
      } else {
        update_chain(bag$current_mean)
        generate_proposal(bag$current_var, input$data_model)
        generate_data("reject")          
      }
    }
  }

  
  ##### set proposed and current values for next MCMC step
  #########################################################
  
  # bernoulli case
  observeEvent(input$proposed_more_likely_bern, transition(1))          # a =    100%
  observeEvent(input$equally_likely_bern, transition(1))                # a =    100%
  observeEvent(input$current_little_more_likely_bern, transition(0.34)) # a =     34%
  observeEvent(input$current_more_likely_bern, transition(0.04))        # a =      4%
  observeEvent(input$current_much_more_likely_bern, transition(1e-6))   # a = 0.0001%
  
  # non-bernoulli cases
  observeEvent(input$proposed_more_likely, transition(1))           # a =    100%
  observeEvent(input$equally_likely, transition(1))                 # a =    100%
  observeEvent(input$current_little_more_likely, transition(0.34))  # a =     34%
  observeEvent(input$current_more_likely, transition(0.04))         # a =      4%
  observeEvent(input$current_much_more_likely, transition(1e-6))    # a = 0.0001%
    

  ##### make the two plots
  ########################################
  
  # bernoulli case
  output$select_plot_bern <- renderPlot({
    ggplot(bag$select_data, aes(x = factor(x, levels = 0:1))) +
      geom_bar() + 
      scale_x_discrete("", breaks = 0:1, labels = c("Failure", "Success")) + 
      facet_grid(. ~ type) +
      ylab("Count")
  })
  
  output$select_plot_non_bern <- renderPlot({
    
    # poisson case
    if(input$data_model == "Poisson") {
      p <- ggplot(bag$select_data, aes(x = x)) +
        geom_histogram(bins = nclass.scott(bag$select_data$x)) +
        facet_grid(type ~ .) + #, scales = "free_y") +
        scale_x_continuous("Number of Occurrences")
    } else {
      # normal cases
      p <- ggplot(bag$select_data, aes(x = x)) +
        geom_histogram(bins = nclass.scott(bag$select_data$x)) +
        facet_grid(type ~ .) + #, scales = "free_y") +
        scale_x_continuous(NULL) 
    }

    # return plot
    p + ylab("Count") + 
  		theme(axis.title.x = element_text(margin = margin(15, 0, 0, 0)), 
    		axis.title.y = element_text(margin = margin(0, 10, 0, 0))
  		)
  })
  

  ##### print selection number
  ########################################
  
  output$selections_bern <- renderText({
    paste(bag$counter, "/", bag$total_bern_selections)
  })
  
  output$selections <- renderText({
    if(input$data_model != "Normal (unknown variance)") {
      paste(bag$counter, "/", bag$total_selections)
    } else {
      paste(bag$mean_counter + bag$var_counter, "/", 2*bag$total_selections)
    }
  })

  
  ##### change burn-in if user selects that option
  ########################################
  
  observeEvent(input$burn_in, bag$burnin <- input$new_burnin)
  
  observeEvent(input$burn_in_normal,
    bag$burnin_normal <- input$new_burnin_normal
  )

  
  ##### fit prior
  ########################################
  
  compute_hypers <- function() {
    
    # remove burn-in
    if(bag$burnin == 0) bag$chain_keep <- bag$chain[-1, ]       # -1 to get rid of initial
    else bag$chain_keep <- bag$chain[-(1:(bag$burnin + 1)), ]   # +1 to get rid of initial
    
    # calculate hyperparameters using MLE
    if(input$data_model != "Normal (unknown variance)") {
      if(input$data_model == "Bernoulli" || input$data_model == "Poisson") {
        lower_limit <- c(0.01, 0.01)
      } else if(input$data_model == "Normal (known variance)") {
        lower_limit <- c(-Inf, 0.01)
      }
      
      bag$th_hat <- optim(c(10, 10),
        function(th) {
          -sum(bag$prior_density(bag$chain_keep$state, th[1], th[2], log = TRUE
        ))},
        method = "L-BFGS-B", lower = lower_limit, upper = c(Inf, Inf)
      )$par
    } else {
      # remove burn-in
      bag$chain_keep_normal_mean <- bag$chain_normal_mean[-(1:(bag$burnin_normal + 1)), ]
      bag$chain_keep_normal_var <- bag$chain_normal_var[-(1:(bag$burnin_normal + 1)), ]
      
      bag$chain_keep_both$state <- cbind(
        bag$chain_keep_normal_mean$state, 
        bag$chain_keep_normal_var$state
      )
      
      lower_limit <- c(-Inf, 0.01, 0.01, 0.01)
      
      bag$th_hat <- optim(rep(10, 4),
        function(th) {
          -sum(bag$prior_density(bag$chain_keep_both$state, th[1], th[2], th[3], 
            th[4], log = TRUE
        ))},
        method = "L-BFGS-B", lower = lower_limit, upper = rep(Inf, 4)  
      )$par
    }
  }
  
	# print hyperparameters for all cases except normal (unknown var)
  output$prior_params <- renderPrint({
  	compute_hypers()
  	
  	if(input$data_model == "Bernoulli" || input$data_model == "Poisson") {
			bag$hyper_params <<- data.frame(
			  "." = round(c(bag$th_hat[1], bag$th_hat[2]), 3),
	  		row.names = c("Alpha  ", "Beta")
	  	)
  	}	else if(input$data_model == "Normal (known variance)") {
  		bag$hyper_params <<- data.frame(
  		  "." = round(c(bag$th_hat[1], bag$th_hat[2]^2), 3),
  			row.names = c("Mu", "Sigma^2  ")
  		)				
		}

  	cat(capture.output(bag$hyper_params)[-1], sep = "\n")
	  invisible(bag$hyper_params)
	})

  
  # print hyperparameters of joint prior for normal (unknown var) case
 	output$prior_params_joint <- renderPrint({
 		compute_hypers()
 		
  	if(input$data_model == "Normal (unknown variance)") {
			bag$hyper_params_joint <<- data.frame(
				"." = round(bag$th_hat, 3),
				row.names = c("Mu0", "Lambda  ", "Alpha", "Beta")
			)
  	}
			
  	cat(capture.output(bag$hyper_params_joint)[-1], sep = "\n")
	  invisible(bag$hyper_params_joint)
 	})
    
	# print hyperparameters for the normal (unknown var) mean
  output$prior_params_mean <- renderPrint({
  	if(input$data_model == "Normal (unknown variance)") {
  	  # (sigma^2)* = (mode of IG prior on variance) / lambda
  	  bag$th_hat2_mean <- bag$th_hat[4] / ((bag$th_hat[3] + 1) * bag$th_hat[2])
  	  
			bag$hyper_params_mean <<- data.frame(
			  "." = round(c(bag$th_hat[1], bag$th_hat2_mean), 3),
			  row.names = c("Mu0", "(Sigma^2)*/Lambda  ")
			)
  	}
			
  	cat(capture.output(bag$hyper_params_mean)[-1], sep = "\n")
	  invisible(bag$hyper_params_mean)
  })
  
  # print hyperparameters for the normal (unknown var) var
  output$prior_params_var <- renderPrint({

  	if(input$data_model == "Normal (unknown variance)") {
			bag$hyper_params_var <<- data.frame(
			  "." = round(c(bag$th_hat[3], bag$th_hat[4]), 3),
				row.names = c("Alpha  ", "Beta")
			)
  	}

  	cat(capture.output(bag$hyper_params_var)[-1], sep = "\n")
	  invisible(bag$hyper_params_var)
  })
  
  
  # print prior summaries
  compute_summaries <- function() {
    if(input$data_model != "Normal (unknown variance)") {
    	if(input$data_model == "Bernoulli") {
    		bag$prior_mean   <- bag$th_hat[1] / (bag$th_hat[1] + bag$th_hat[2])
    		bag$prior_median <- qbeta(0.5, bag$th_hat[1], bag$th_hat[2])
    		bag$prior_mode   <- (bag$th_hat[1] - 1) / (bag$th_hat[1] + bag$th_hat[2] - 2)
    		bag$prior_sd     <- sqrt(bag$th_hat[1]*bag$th_hat[2] / 
    		    ((bag$th_hat[1] + bag$th_hat[2])^2 * (bag$th_hat[1] + bag$th_hat[2] + 1)))
    		bag$prior_2_5    <- qbeta(0.025, bag$th_hat[1], bag$th_hat[2])
    		bag$prior_97_5   <- qbeta(0.975, bag$th_hat[1], bag$th_hat[2])
    		bag$ess          <- bag$th_hat[1] + bag$th_hat[2]
    	}	else if(input$data_model == "Poisson") {
    		bag$prior_mean   <- bag$th_hat[1] / bag$th_hat[2]
    		bag$prior_median <- qgamma(0.5, bag$th_hat[1], bag$th_hat[2])
    		bag$prior_mode   <- (bag$th_hat[1] - 1) / bag$th_hat[2]
    		bag$prior_sd     <- sqrt(bag$th_hat[1]/(bag$th_hat[2])^2)
    		bag$prior_2_5    <- qgamma(0.025, bag$th_hat[1], bag$th_hat[2])
    		bag$prior_97_5   <- qgamma(0.975, bag$th_hat[1], bag$th_hat[2])
    		bag$ess          <- bag$th_hat[2]
    	} else if(input$data_model == "Normal (known variance)") {
    		bag$prior_mean   <- bag$th_hat[1]
    		bag$prior_median <- bag$th_hat[1]
    		bag$prior_mode   <- bag$th_hat[1]
    		bag$prior_sd     <- bag$th_hat[2]
    		bag$prior_2_5    <- qnorm(0.025, bag$th_hat[1], bag$th_hat[2])
    		bag$prior_97_5   <- qnorm(0.975, bag$th_hat[1], bag$th_hat[2])
    		bag$ess          <- (input$pop_sd)^2 / bag$th_hat[2]^2
    	}
      
      bag$summaries <<- data.frame("." = round(c(bag$prior_mean, bag$prior_median, 
        bag$prior_mode, bag$prior_sd, bag$prior_2_5, bag$prior_97_5, bag$ess), 4
        ), 
        row.names = c("Mean", "Median", "Mode", "Std. Dev.  ", "2.5%", "97.5%", "ESS")
      )
    }	else {
  		# mean summaries -- normal prior
	  	bag$mean_prior_mean   <- bag$th_hat[1]
			bag$mean_prior_median <- bag$th_hat[1]
			bag$mean_prior_mode   <- bag$th_hat[1]
			bag$mean_prior_sd     <- sqrt(bag$th_hat2_mean)
			bag$mean_prior_2_5    <- qnorm(0.025, bag$th_hat[1], sqrt(bag$th_hat2_mean))
			bag$mean_prior_97_5   <- qnorm(0.975, bag$th_hat[1], sqrt(bag$th_hat2_mean))
			
			# variance summaries -- inverse-gamma prior
			bag$var_prior_mean   <- bag$th_hat[4] / (bag$th_hat[3] - 1)
			bag$var_prior_median <- qinvgamma(0.5, bag$th_hat[3], bag$th_hat[4])
			bag$var_prior_mode   <- bag$th_hat[4] / (bag$th_hat[3] + 1)
			bag$var_prior_sd     <- sqrt(bag$th_hat[4]^2 / ((bag$th_hat[3] - 1)^2 * 
													 			(bag$th_hat[3] - 2)))
			bag$var_prior_2_5    <- qinvgamma(0.025, bag$th_hat[3], bag$th_hat[4])
			bag$var_prior_97_5   <- qinvgamma(0.975, bag$th_hat[3], bag$th_hat[4])
			
			bag$summaries_mean <<- data.frame(
			  "." = round(c(bag$mean_prior_mean, bag$mean_prior_median, bag$mean_prior_mode, 
			    bag$mean_prior_sd, bag$mean_prior_2_5, bag$mean_prior_97_5), 4), 
			  row.names = c("Mean", "Median", "Mode", "Std. Dev.  ", "2.5%", "97.5%")
			)
			
			bag$summaries_var <<- data.frame(
			  "." = round(c(bag$var_prior_mean, bag$var_prior_median, bag$var_prior_mode, 
			    bag$var_prior_sd, bag$var_prior_2_5, bag$var_prior_97_5), 4), 
			  row.names = c("Mean", "Median", "Mode", "Std. Dev.  ", "2.5%", "97.5%")
			)
  	}
  }

  
  # print prior summaries for all cases except normal (unknown var)
  output$prior_summaries <- renderPrint({
  	compute_summaries()
  	cat(capture.output(bag$summaries)[-1], sep = "\n")
  	invisible(bag$summaries)
  })
  
  # print prior summaries for normal (unknown var) mean
  output$prior_summaries_mean <- renderPrint({
  	compute_summaries()
  	cat(capture.output(bag$summaries_mean)[-1], sep = "\n")
  	invisible(bag$summaries_mean)
  })
  
  # print prior summaries for normal (unknown var) variance
  output$prior_summaries_var <- renderPrint({
  	cat(capture.output(bag$summaries_var)[-1], sep = "\n")
  	invisible(bag$summaries_var)
  })
  
  
  # print number of selections for all cases except normal (unknown var)
  output$all_selections <- renderPrint({
  	bag$n_selections <<- data.frame(
			"." = c(nrow(bag$chain[-1, ]), bag$burnin, nrow(bag$chain_keep)),
		row.names = c("Total", "Burn-in  ", "Kept")
		)
		cat(capture.output(bag$n_selections)[-1], sep = "\n")
		invisible(bag$n_selections)
  })
 
  # print number of selections for normal (unknown var) mean     # change no. of selections **
  output$all_mean_selections <- renderPrint({
  	bag$n_mean_selections <<- data.frame(
			"." = c(bag$mean_counter, bag$burnin_normal, nrow(bag$chain_keep_normal_mean)),
			row.names = c("Total", "Burn-in  ", "Kept")
		)
		cat(capture.output(bag$n_mean_selections)[-1], sep = "\n")
		invisible(bag$n_mean_selections)
  })

  # print number of selections for normal (unknown var) var
  output$all_var_selections <- renderPrint({
  	bag$n_var_selections <<- data.frame(
			"." = c(bag$var_counter, bag$burnin_normal, nrow(bag$chain_keep_normal_var)),
			row.names = c("Total", "Burn-in  ", "Kept")
		)
		cat(capture.output(bag$n_var_selections)[-1], sep = "\n")
		invisible(bag$n_var_selections)
  })
  
  
  ##### plot elicited prior based on selected params
  ########################################
  
  output$prior_plot <- renderPlot({
    # make plot
    beta_density <- stat_function(fun = dbeta, geom = "area", n = 1e5,
      args = list(shape1 = bag$th_hat[1], shape2 = bag$th_hat[2]),
      fill = "blue", alpha = 0.5, color = NA
    )
    
    gamma_density <- stat_function(fun = dgamma, geom = "area", n = 1e5,
      args = list(shape = bag$th_hat[1], rate = bag$th_hat[2]),
      fill = "blue", alpha = 0.5, color = NA
    )
    
    normal_density <- stat_function(fun = dnorm, geom = "area", n = 1e5,
      args = list(mean = bag$th_hat[1], sd = bag$th_hat[2]),
      fill = "blue", alpha = 0.5, color = NA
    )
    
    p_plot <- ggplot(bag$chain_keep, aes(x = state, geom = "blank")) +
      labs(y = "Density", title = "Elicited Prior Density") +
      theme(
        plot.title = element_text(size = 17),
        axis.title.x = element_text(margin = margin(15, 0, 0, 0)), 
        axis.title.y = element_text(margin = margin(0, 10, 0, 0))
      )

    if(input$data_model == "Bernoulli") {
    	p_plot <- p_plot + beta_density + xlab("p") + xlim(c(0, 1))
    } else if(input$data_model == "Poisson") {
    	p_plot <- p_plot + gamma_density + xlab(expression(lambda)) +
    	  xlim(c(qgamma(1e-5, bag$th_hat[1], bag$th_hat[2]), 
    	    qgamma(1e-5, bag$th_hat[1], bag$th_hat[2], lower.tail = FALSE)
    	  ))
    } else if(input$data_model == "Normal (known variance)") {
    	p_plot <- p_plot + normal_density + xlab(expression(mu)) +
    	  xlim(c(qnorm(1e-5, bag$th_hat[1], bag$th_hat[2]), 
    	    qnorm(1e-5, bag$th_hat[1], bag$th_hat[2], lower.tail = FALSE)
    	  ))
    }
    
    if(input$add_kde) {
      p_plot <- p_plot + 
        geom_density(fill = "red", alpha = 0.35, color = NA) +
        geom_rug() +
        labs(title = "Blue:  Elicited prior density\nRed:  Nonparametric density of selections")
    }

    (bag$p_plot <- p_plot)
  })
  
  
  # plot elicited prior -- conditional of mean for normal (unknown var) case
  output$prior_plot_normal_mean <- renderPlot({
    p_plot_mean <- ggplot(data = bag$chain_keep_normal_mean, aes(x = state)) +
      stat_function(fun = dnorm, geom = "area", n = 1e5,
        args = list(mean = bag$th_hat[1], sd = sqrt(bag$th_hat2_mean)),
        fill = "blue", alpha = 0.5, color = NA
      ) +
      xlim(c(qnorm(1e-5, bag$th_hat[1], sqrt(bag$th_hat2_mean)), 
        qnorm(1e-5, bag$th_hat[1], sqrt(bag$th_hat2_mean), lower.tail = FALSE)
      )) +
      labs(x = expression(mu), y = "Density", title = "Elicited Prior Density") +
      theme(plot.title = element_text(size = 17),
        axis.title.x = element_text(margin = margin(15, 0, 0, 0)),
        axis.title.y = element_text(margin = margin(0, 10, 0, 0))
      )
    
    (bag$p_plot_mean <- p_plot_mean)
  })
  
  
  # plot elicited prior -- variance for normal (unknown var) case
  output$prior_plot_normal_var <- renderPlot({
    p_plot_var <- ggplot(data = bag$chain_keep_normal_var, aes(x = state)) +
      stat_function(fun = dinvgamma, geom = "area", n = 1e5,
        args = list(shape = bag$th_hat[3], rate = bag$th_hat[4]),
        fill = "blue", alpha = 0.5, color = NA
      ) +
      xlim(c(qinvgamma(1e-4, bag$th_hat[3], bag$th_hat[4]), 
        qinvgamma(1e-4, bag$th_hat[3], bag$th_hat[4], lower.tail = FALSE)
      )) +
      labs(x = expression(sigma^2), y = "Density", title = "Elicited Prior Density") +
      theme(plot.title = element_text(size = 17),
        axis.title.x = element_text(margin = margin(15, 0, 0, 0)),
        axis.title.y = element_text(margin = margin(0, 10, 0, 0))
      )
    
    (bag$p_plot_var <- p_plot_var)
  })

  
  # allow user to find probability for specific interval
  findProb <- eventReactive(input$interval_prob, {
  	
  	if(input$data_model == "Bernoulli") {
  		prior_prob <- round(pbeta(input$upper_prob, bag$th_hat[1], bag$th_hat[2])
                          - pbeta(input$lower_prob, bag$th_hat[1], bag$th_hat[2]), 4)
  	} else if(input$data_model == "Poisson") {
  		prior_prob <- round(pgamma(input$upper_prob, bag$th_hat[1], bag$th_hat[2])
                          - pgamma(input$lower_prob, bag$th_hat[1], bag$th_hat[2]), 4)
  	} else if(input$data_model == "Normal (known variance)") {
  		prior_prob <- round(pnorm(input$upper_prob, bag$th_hat[1], sqrt(bag$th_hat[2]))
  												- pnorm(input$lower_prob, bag$th_hat[1], sqrt(bag$th_hat[2])), 4)
  	}
    
    prior_prob_df <- data.frame("." = prior_prob, row.names = "Prob. =")
    cat(capture.output(prior_prob_df)[-1], sep = "\n")
    invisible(prior_prob_df)
  })
  
  # print user's selected prior interval
  output$prob_output <- renderPrint( findProb() ) 
  
  
  ##### make trace plot(s)
  ########################################
  
  # all cases except normal (unknown var)
  output$trace <- renderPlot({
    t_plot <- ggplot(bag$chain, aes(x = step, y = state)) +
      geom_point(size = 4) +
      geom_line(size = 1) +
      labs(x = "Step") +
			theme(axis.title.x = element_text(margin = margin(15, 0, 0, 0)), 
    		axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
				legend.position = "none"
			)
    
  	if(input$data_model == "Bernoulli") {
  	  t_plot <- t_plot + ylab("p") + ylim(c(0, 1))
  	} else if(input$data_model == "Poisson") {
  	  t_plot <- t_plot + ylab(expression(lambda))
  	} else if(input$data_model == "Normal (known variance)") {
  	  t_plot <- t_plot + ylab(expression(mu))    
  	}
  	
    t_plot
  })
  
  # normal (unknown var) mean
  output$trace_normal_mean <- renderPlot({
    bag$trace_mean <- ggplot(bag$chain_normal_mean, aes(x = step, y = state)) +
      geom_point(size = 5) +
      geom_line(size = 1) +
      labs(x = "Step", y = expression(mu)) +
			theme(
			  axis.title.x = element_text(margin = margin(15, 0, 0, 0)), 
    		axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
				legend.position = "none"
			)
    
    bag$trace_mean
  })  
  
  # normal (unknown var) variance
  output$trace_normal_var <- renderPlot({
    bag$trace_var <- ggplot(bag$chain_normal_var, aes(x = step, y = state)) +
      geom_point(size = 5) +
      geom_line(size = 1) +
      labs(x = "Step", y = expression(sigma^2)) +
			theme(
			  axis.title.x = element_text(margin = margin(15, 0, 0, 0)), 
    		axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
				legend.position = "none"
			)
    
    bag$trace_var
  })
  
  
  output$trace_2d <- renderPlot({
    
    bag$path_df <- data.frame(
      means = bag$chain_normal_mean$state, 
      vars = bag$chain_normal_var$state
    )
    
  	bag$trace_2d <- ggplot(bag$path_df, aes(x = means, y = vars)) + 
  	  geom_path() +
  		geom_point(aes(x = means[1], y = vars[1])) +
  		geom_point(aes(x = means[length(means)], y = vars[length(vars)])) +
  		labs(x = expression(mu), y = expression(sigma^2)) +
			theme(
			  axis.title.x = element_text(margin = margin(15, 0, 0, 0)), 
    		axis.title.y = element_text(margin = margin(0, 10, 0, 0))
			)
  	
  	bag$trace_2d
  })
  
  
  ##### create report of user inputs, elicited prior info, and trace plot
  ########################################
  
  output$download_report <- downloadHandler(
    filename = function() {
      paste("elicitation-results", sep = ".", switch(
        input$format, PDF = "pdf", HTML = "html", Word = "docx"
      ))
    },
    
    content = function(file){
  		if(input$data_model != "Normal (unknown variance)") {
  		  markdown_file <- "report.Rmd"
  		} else {
  		  markdown_file <- "report2.Rmd"
  		}
      
  		src <- normalizePath(markdown_file)
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, markdown_file, overwrite = TRUE)
      
      out <- render(markdown_file, switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      
      file.rename(out, file)
    }
  )
  
  
  ##### create csv file of selected parameters
  ########################################
  
  output$download_selected <- downloadHandler(
    filename = function() {
      paste("elicitation-selected-values", ".csv", sep = "")
    },
    
    content = function(file){
  		if(input$data_model != "Normal (unknown variance)") {
  			all_states <- data.frame(
  			  "Parameter" = bag$chain$state,
  			  "Step" = bag$chain$step
  			)
  		} else {
  			if(length(bag$chain_normal_mean$state) == length(bag$chain_normal_var$state)) {
  				all_states <- data.frame(
  					"Mean" = bag$chain_normal_mean$state,
  					"Variance" = bag$chain_normal_var$state
  				)
  			} else {
  				all_states <- data.frame(
  				"Mean" = bag$chain_normal_mean$state,
  				"Variance" = c(bag$chain_normal_var$state, NA)
  				)
  			}
  		}
      
      write.csv(all_states, file, row.names = FALSE)
    }
  )
  
})


