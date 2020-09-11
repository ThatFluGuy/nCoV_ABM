/***
* Name: 03_Model_Fitting
* Author: Michael L. Jackson
* Description: Using hill-climbing to estimate parameters
***/

model Fitting

/* Insert your model definition here */


/* Third program in the sequence of evaluating the impact of different COVID-19 control measures.  */
/* This program attempts to reproduce, as closely as possible, the actual interventions undertaken */
/* in the Seattle area during March and April 2020. Simulated incidence and hospitalization are    */
/* then compared with observed data to see whether the simulation is reasonably reproducing the	   */
/* actual course of the epidemic so far.														   */
/* This version uses the hill-climbing algorithm to find best parameter fits.					   */

import "../models/00_Base_Model.gaml"

/* Set up the global environment */
global {
	int max_days <- 205;

	float beta_HH  <- 0.015;			 	// Probability of infection given contact in household
	float beta_COM <- 0.010;				// Probability of infection given contact in workplace/community

	bool initialize_Settings <- false;
	bool initialize_Infectious <- false;
	bool initialize_csv <- false;
	
	// Only using a single simulation, so don't have other options
	list<int> all_sims <- [0]; // List of all sub-populations
	list<float> sim_sample_prob <- [1.0]; // Probability of sampling other patch (sim)
	
	// Social distancing variables
	// Set dates of interventions and initialize percent decline variables
	// Percent decline variables update in the UpdateDay reflex
	float comm_open_pct <- 1.0;						// Percent community site visits occurring during specified time period
	float work_open_pct <- 1.0;						// Percent of work occuring during specified time period

	int monday_counter <- 0; 			// Counter for weekly updates to contacts

	// Weekly percent reductions in workplace contacts
	list<float> work_close_pcts <- [0.00, 0.00, 0.00, 0.00, 0.10, 0.30, 0.52, 0.64, 0.70, 0.70, 0.68, 0.67, 0.65, 0.63, 
		0.62, 0.61, 0.63, 0.58, 0.57, 0.56, 0.57, 0.59, 0.57, 0.56, 0.56, 0.56, 0.57, 0.57, 0.57, 0.56, 0.57];
	
	// Weekly percent reductions in community contacts
	list<float> comm_close_pcts <- [0.00, 0.00, 0.00, 0.00, 0.05, 0.12, 0.29, 0.40, 0.41, 0.40, 0.39, 0.39, 0.36, 0.33, 0.34, 
		0.32, 0.33, 0.30, 0.28, 0.24, 0.23, 0.23, 0.22, 0.21, 0.21, 0.20, 0.20, 0.20, 0.20, 0.19, 0.19];
	
	// Weekly probability of visiting a NH or GQ
	list<float> nhgq_visit_pcts <- [0.001, 0.001, 0.001, 0.001, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
		0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0];
	
	// Starting in May, assume modest reduction in beta_COM due to widespread mask use
	float masks <- 0.9; 
		
	int school_close_day <- 41; 					// Close schools on day 41 of the simulation (March 12)
	bool school_open <- true;						// Flag for whether school is open

	// Track cumulative hospitalizations and optimize
	list<int> n_cumul_hosp <- list_with(max_days+1, 0);
	float sum_sq_err <- 0.0;
	
	list<float> obs_hosp_list <- [0.07, 0.07, 0.07, 0.07, 0.07, 0.07, 0.07, 0.11, 0.11, 0.11, 0.18, 0.21, 0.21, 0.21, 0.25, 0.25, 
		0.27, 0.30, 0.39, 0.46, 0.55, 0.62, 0.66, 0.80, 0.82, 0.84, 1.09, 1.28, 1.39, 1.46, 1.78, 2.03, 2.23, 2.76, 3.10, 3.37, 
		3.69, 4.08, 4.58, 4.95, 5.54, 5.86, 6.22, 6.54, 7.07, 7.50, 7.86, 8.46, 8.96, 9.50, 9.98, 10.96, 11.53, 12.31, 12.95, 
		13.74, 14.68, 15.54, 16.41, 17.30, 18.23, 19.12, 20.17, 20.74, 21.49, 22.09, 22.88, 23.43, 24.14, 24.82, 25.32, 25.71, 
		26.12, 26.49, 27.01, 27.60, 28.15, 28.60, 29.11, 29.47, 29.90, 30.25, 30.66, 30.98, 31.34, 31.57, 31.73, 32.12, 32.41, 
		32.57, 32.71, 33.00, 33.14, 33.48, 33.69, 33.87, 34.05, 34.26, 34.44, 34.74, 34.94, 35.15, 35.35, 35.60, 35.72, 35.76, 
		35.85, 36.01, 36.24, 36.40, 36.51, 36.70, 36.83, 37.02, 37.15, 37.20, 37.29, 37.36, 37.52, 37.59, 37.72, 37.86, 37.88, 
		38.00, 38.09, 38.27, 38.34, 38.43, 38.57, 38.61, 38.77, 38.79, 38.79, 38.82, 38.86, 39.04, 39.14, 39.23, 39.34, 39.45, 
		39.55, 39.71, 39.82, 39.96, 40.09, 40.32, 40.41, 40.62, 40.89, 41.00, 41.10, 41.16, 41.28, 41.44, 41.53, 41.64, 41.76, 
		41.98, 42.21, 42.46, 42.62, 42.80, 42.92, 43.08, 43.40, 43.51, 43.58, 43.76, 43.90, 44.10, 44.31, 44.49, 44.65, 44.77, 
		44.97, 45.08, 45.31, 45.52, 45.72, 46.09, 46.31, 46.57, 46.75, 46.84, 47.00, 47.30, 47.43, 47.61, 47.75, 47.93, 48.09, 
		48.30, 48.43, 48.59, 48.84, 49.10, 49.28, 49.39, 49.51, 49.73, 50.01, 50.19, 50.35, 50.44, 50.58, 50.78, 50.78, 50.78, 
		50.81, 50.81, 50.81, 50.81];

	// Initialize model, specify the number of infectious and susceptible hosts
	init {		
		// Create settings (geographies where agents can be)
		create Home number: nb_home_init;
		create School number: nb_school_init;
		create Workplace number: nb_work_init;
		create Community number: nb_comm_init;
		create NH number: nb_nh_init;
		create GQ number: nb_gq_init;
		
		// Create hosts
		create Toddler from: csv_file("../includes/sim_Toddler_50k_" + model_number + ".csv", true) 
					with: [sus::true, indexHome::int(get("indexHome")), 
						ageyrs::int(get("ageyrs")), male::bool(get("male"))];
		create Child from: csv_file("../includes/sim_Child_50k_" + model_number + ".csv", true) 
					with: [sus::true, indexHome::int(get("indexHome")), ageyrs::int(get("ageyrs")),  
							male::bool(get("male")), indexSchool::int(get("indexSchool"))];
		create Adult from: csv_file("../includes/sim_Adult_50k_" + model_number + ".csv", true)
					with: [sus::true, indexHome::int(get("indexHome")), ageyrs::int(get("ageyrs")),
							male::bool(get("male")), indexWorkplace::int(get("indexBus")),
							indexSchool::int(get("indexSchool")), indexNH::int(get("indexNH")),
							indexGQ::int(get("indexGQ"))];
		create Senior from: csv_file("../includes/sim_Senior_50k_" + model_number + ".csv", true) 
					with: [sus::true, indexHome::int(get("indexHome")), 
							ageyrs::int(get("ageyrs")), male::bool(get("male"))];
		create NHresident from: csv_file("../includes/sim_NH_50k_" + model_number + ".csv", true)
					with: [sus::true, ageyrs::int(get("ageyrs")), male::bool(get("male")), indexNH::int(get("indexNH"))];
		create GQresident from: csv_file("../includes/sim_GQ_50k_" + model_number + ".csv", true)
					with: [sus::true, ageyrs::int(get("ageyrs")), male::bool(get("male")), indexGQ::int(get("indexGQ"))];
		
		loop times: nb_inf_init {
			ask one_of(Adult){
				self.sus <- false;
				self.inf <- true;
				self.counter_inf <- dur_infect[rnd_choice([0.25, 0.5, 0.25])];
			}
		}					
	}

	// Drop the update_counts since they are not needed
	
	action update_counts {
		loop times: n_cross_inf {
			ask one_of(agents of_generic_species(Host_Master)){
				if self.sus = true {
					self.sus <- false;
					self.exp <- true;
					
					counter_exp <- dur_expose[rnd_choice([0.2, 0.5, 0.3])];
					counter_sym <- counter_exp + dur_incub[rnd_choice([0.2, 0.6, 0.2])];
				}
			}
		}
		n_cross_inf <- 0;
		
		// Update population count tracking variables
		if daypart = "evening" {
			sus_tod_list[day] <- Toddler count (each.sus);
			sus_chi_list[day] <- Child count (each.sus);
			sus_adu_list[day] <- Adult count (each.sus);
			sus_sen_list[day] <- Senior count (each.sus);
			sus_nh_list[day] <- NHresident count (each.sus);
			sus_gq_list[day] <- GQresident count (each.sus);
			
			exp_tod_list[day] <- Toddler count (each.exp);
			exp_chi_list[day] <- Child count (each.exp);
			exp_adu_list[day] <- Adult count (each.exp);
			exp_sen_list[day] <- Senior count (each.exp);
			exp_nh_list[day] <- NHresident count (each.exp);
			exp_gq_list[day] <- GQresident count (each.exp);
			
			inf_tod_list[day] <- Toddler count (each.inf);
			inf_chi_list[day] <- Child count (each.inf);
			inf_adu_list[day] <- Adult count (each.inf);
			inf_sen_list[day] <- Senior count (each.inf);
			inf_nh_list[day] <- NHresident count (each.inf);
			inf_gq_list[day] <- GQresident count (each.inf);
		}
		
		// Compile sum of squared errors
		if daypart = "evening" {
			if day = 0 {
				n_cumul_hosp[day] <- n_hosp_list[day];
			} else {
				n_cumul_hosp[day] <- n_hosp_list[day] + n_cumul_hosp[day-1];
			}
			
			sum_sq_err <- sum_sq_err + (n_cumul_hosp[day] - obs_hosp_list[day])^2;
		}
	}

	// Modify the update_day action to calculate probabilities of going to work or community based on closures
	action update_day {
		// Update simulation day and weekday
		day <- day + 1;
		if weekday = "Su"{
			weekday <- "Mo";
		} else if weekday = "Mo" {
			monday_counter <- monday_counter + 1;
			comm_open_pct <- 1 - comm_close_pcts[monday_counter];
			work_open_pct <- 1 - work_close_pcts[monday_counter];
			prob_nhgq_visit <- nhgq_visit_pcts[monday_counter];
			
			weekday <- "Tu";
		} else if weekday = "Tu" {
			weekday <- "We";
		} else if weekday = "We" {
			weekday <- "Th";
		} else if weekday = "Th" {
			weekday <- "Fr";
		} else if weekday = "Fr" {
			weekday <- "Sa";
		} else if weekday = "Sa" {
			weekday <- "Su";
		}

		// Flag for whether school is in session
		if day < school_close_day {
			school_open <- true;
		} else  {
			school_open <- false;
		}
		
		// Starting in May, reduce beta_COM for mask usage
		if day = 90 {
			beta_COM <- beta_COM * masks;
		} 
	}
	
	// Modify the save function
	// Exclude "do die;" so that results save for optimization	
	reflex save_output when: day=max_days{
		
		write "ALL DONE, SSE= " + sum_sq_err;						
	}
}

/* TODDLER, a subset of Host ages 0-5 who is not assigned to a "school" (e.g. daycare, pre-school) */
species Toddler parent: Toddler_Master {
	int ageyrs <- rnd(0, 5);	
	
	action move_afternoon {
		// Afternoon: some proportion go to random GQ or NH, some go to random community location, others go home;
		// Check for NH visit. If not, check for GQ visit (same prob). If not, check for community
		// Varies based on weekday vs. weekend
		if outside_sim = true {
			// If infecting someone in another sub-population, set location outside the grid
			self.location <- point(-1, -1, 0);
			outside_sim <- false;
		} else if flip(prob_nhgq_visit) {
			self.location <- one_of(NH).location;
		} else if flip(prob_nhgq_visit) {
			self.location <- one_of(GQ).location;
		} else if flip(prob_community_wkdy * comm_open_pct) and weekday in ["Mo", "Tu", "We", "Th", "Fr"] {
			self.location <- one_of(Community).location;
		} else if flip(prob_community_wknd * comm_open_pct) and weekday in ["Sa", "Su"]{
			self.location <- one_of(Community).location;
		} else {
			self.location <- (Home at indexHome).location;
		}
	}
}

/* CHILD, a subset of Host ages 0-17, has a school (could be daycare for age <6) */
species Child parent: Child_Master {
	int indexSchool <- rnd(0, nb_school_init-1);
	int ageyrs <- rnd(6, 17);
	
	/********* Child-specific movement processes ***********/
	// Morning: go to school until schools are closed
	action move_morning {
		if outside_sim = true {
			self.location <- point(-1, -1, 0);
			outside_sim <- false;
		} else if weekday in ["Mo", "Tu", "We", "Th", "Fr"] and school_open = true {
			// Morning: children go to school
			self.location <- (School at indexSchool).location;
		} 
	}
	
	action move_afternoon {
		// Afternoon: some proportion go to random GQ or NH, some go to random community location, others go home;
		// Check for NH visit. If not, check for GQ visit (same prob). If not, check for community
		// Varies based on weekday vs. weekend
		if outside_sim = true {
			// If infecting someone in another sub-population, set location outside the grid
			self.location <- point(-1, -1, 0);
			outside_sim <- false;
		} else if flip(prob_nhgq_visit) {
			self.location <- one_of(NH).location;
		} else if flip(prob_nhgq_visit) {
			self.location <- one_of(GQ).location;
		} else if flip(prob_community_wkdy * comm_open_pct) and weekday in ["Mo", "Tu", "We", "Th", "Fr"] {
			self.location <- one_of(Community).location;
		} else if flip(prob_community_wknd * comm_open_pct) and weekday in ["Sa", "Su"]{
			self.location <- one_of(Community).location;
		} else {
			self.location <- (Home at indexHome).location;
		}
	}
}

/* ADULT, a subset of Host, ages 18 - 74. Could have a workplace, which could be a school, NH, or GQ */
species Adult parent: Adult_Master {
	int indexWorkplace <- -1;
	int indexSchool <- -1;
	int indexNH <- -1;
	int indexGQ <- -1;
	int ageyrs <- rnd(18, 74);
	
	/********* Adult-specific movement processes ***********/
	// Teachers stop going to schools when schools close
	action move_morning {
		if outside_sim = true {
			self.location <- point(-1, -1, 0);
			outside_sim <- false;
		} else if weekday in ["Mo", "Tu", "We", "Th", "Fr"] and flip(work_open_pct) {
			if indexWorkplace >= 0 {
				self.location <- (Workplace at indexWorkplace).location;
			} else if indexSchool >= 0 {
				if school_open = true { 
					self.location <- (School at indexSchool).location; 	// Teachers work at school
				}
			} else if indexNH >= 0 {
				self.location <- (NH at indexNH).location;			// NH workers
			} else if indexGQ >= 0 {
				self.location <- (GQ at indexGQ).location;			// GQ workers
			}
		}
	}
	
	action move_afternoon {
		// Afternoon: some proportion go to random GQ or NH, some go to random community location, others go home;
		// Check for NH visit. If not, check for GQ visit (same prob). If not, check for community
		// Varies based on weekday vs. weekend
		if outside_sim = true {
			// If infecting someone in another sub-population, set location outside the grid
			self.location <- point(-1, -1, 0);
			outside_sim <- false;
		} else if flip(prob_nhgq_visit) {
			self.location <- one_of(NH).location;
		} else if flip(prob_nhgq_visit) {
			self.location <- one_of(GQ).location;
		} else if flip(prob_community_wkdy * comm_open_pct) and weekday in ["Mo", "Tu", "We", "Th", "Fr"] {
			self.location <- one_of(Community).location;
		} else if flip(prob_community_wknd * comm_open_pct) and weekday in ["Sa", "Su"]{
			self.location <- one_of(Community).location;
		} else {
			self.location <- (Home at indexHome).location;
		}
	}
}

/* SENIOR, as subset of Host, ages 75 - 99 (no workplace) */
species Senior parent: Senior_Master {
	int ageyrs <- rnd(75, 99);
	
	action move_afternoon {
		// Afternoon: some proportion go to random GQ or NH, some go to random community location, others go home;
		// Check for NH visit. If not, check for GQ visit (same prob). If not, check for community
		// Varies based on weekday vs. weekend
		if outside_sim = true {
			// If infecting someone in another sub-population, set location outside the grid
			self.location <- point(-1, -1, 0);
			outside_sim <- false;
		} else if flip(prob_nhgq_visit) {
			self.location <- one_of(NH).location;
		} else if flip(prob_nhgq_visit) {
			self.location <- one_of(GQ).location;
		} else if flip(prob_community_wkdy * comm_open_pct) and weekday in ["Mo", "Tu", "We", "Th", "Fr"] {
			self.location <- one_of(Community).location;
		} else if flip(prob_community_wknd * comm_open_pct) and weekday in ["Sa", "Su"]{
			self.location <- one_of(Community).location;
		} else {
			self.location <- (Home at indexHome).location;
		}
	}
}

/* NHresident, a subset of Host for those in nursing homes. Does not move */
species NHresident parent: NHresident_Master {
}

/* GQresident, a subset of Host for those in group quarters. Does not move */
species GQresident parent: GQresident_Master {
}

/* Parameter optimization */
experiment Tabu_Search type: batch repeat: 3 keep_seed: true until: (day >= max_days) parallel: true {
	parameter "HH beta" var: beta_HH min: 0.012 max: 0.017 step: 0.001;
	parameter "COM beta" var: beta_COM min: 0.008 max: 0.011 step: 0.001;
	parameter "Masks" var: masks min: 0.85 max: 1.0 step: 0.01; 
	
	method tabu minimize: sum_sq_err iter_max: 50 tabu_list_size: 5;
	
	reflex end_of_runs{
		ask simulations {
			write "SSE= " + sum_sq_err;
		} 
	}
}

