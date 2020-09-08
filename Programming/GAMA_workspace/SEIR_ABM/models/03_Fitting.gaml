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

	float beta_HH  <- 0.024;			 	// Probability of infection given contact in household
	float beta_COM <- 0.012;				// Probability of infection given contact in workplace/community

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
	list<float> work_close_pcts <- [0.0, 0.0, 0.0, 0.0, 10.2, 29.6, 51.8, 64.2, 69.6, 69.8, 67.8, 66.8, 65.4, 63.2, 61.8,
			60.6, 63, 58, 57, 56.4, 56.6, 58.8, 56.6, 56.2, 56.2, 56.4, 56.6, 56.8, 56.6, 56.2, 56.6, 56.6];
	
	// Weekly percent reductions in community contacts
	list<float> comm_close_pcts <- [0.0, 0.0, 0.0, 0.0, 5.0, 12.14, 29.43, 39.86, 40.57, 39.93, 38.86, 39.0, 36.0, 33.0, 
		34.36, 32.43, 33.0, 30.21, 27.93, 24.07, 23.43, 23.07, 22.0, 20.64, 20.57, 20.0, 20.36, 20.29, 20.29, 19.14, 19.14];
	
	// Weekly probability of visiting a NH or GQ
	list<float> nhgq_visit_pcts <- [0.001, 0.001, 0.001, 0.001, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
		0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0];
		
	int school_close_day <- 41; 					// Close schools on day 41 of the simulation (March 12)
	bool school_open <- true;						// Flag for whether school is open

	// Track cumulative hospitalizations and optimize
	list<int> n_cumul_hosp <- list_with(max_days+1, 0);
	float sum_sq_err <- 0.0;
	//float scale <- 1.0; // Scale the close percents list
	
	list<float> obs_hosp_list <- [0.68, 0.68, 0.68, 0.68, 0.68, 0.68, 0.68, 1.14, 1.14, 1.14, 1.82, 2.05, 2.05, 2.05, 2.51, 2.51, 
		2.74, 2.96, 3.87, 4.56, 5.47, 6.15, 6.61, 7.98, 8.21, 8.43, 10.94, 12.76, 13.90, 14.59, 17.78, 20.29, 22.34, 27.58, 31.00, 
		33.73, 36.92, 40.80, 45.81, 49.46, 55.39, 58.58, 62.22, 65.42, 70.66, 74.99, 78.64, 84.56, 89.58, 95.05, 99.83, 109.63, 
		115.33, 123.08, 129.46, 137.44, 146.79, 155.45, 164.11, 173.00, 182.34, 191.23, 201.72, 207.41, 214.94, 220.86, 228.84, 
		234.31, 241.38, 248.21, 253.23, 257.10, 261.21, 264.85, 270.09, 276.02, 281.49, 286.05, 291.06, 294.71, 299.04, 302.46, 
		306.56, 309.75, 313.40, 315.68, 317.28, 321.15, 324.11, 325.71, 327.08, 330.04, 331.41, 334.83, 336.88, 338.70, 340.52, 
		342.58, 344.40, 347.36, 349.41, 351.46, 353.52, 356.02, 357.16, 357.62, 358.53, 360.13, 362.41, 364.00, 365.14, 366.96, 
		368.33, 370.15, 371.52, 371.98, 372.89, 373.57, 375.17, 375.85, 377.22, 378.59, 378.82, 379.96, 380.87, 382.69, 383.37, 
		384.29, 385.65, 386.11, 387.71, 387.93, 387.93, 388.16, 388.62, 390.44, 391.35, 392.26, 393.40, 394.54, 395.45, 397.05, 
		398.19, 399.56, 400.93, 403.20, 404.12, 406.17, 408.90, 410.04, 410.95, 411.64, 412.78, 414.37, 415.28, 416.42, 417.56, 
		419.84, 422.12, 424.63, 426.23, 428.05, 429.19, 430.78, 433.97, 435.11, 435.80, 437.62, 438.99, 441.04, 443.09, 444.92, 
		446.51, 447.65, 449.70, 450.84, 453.12, 455.17, 457.22, 460.87, 463.15, 465.66, 467.48, 468.39, 469.99, 472.95, 474.32, 
		476.14, 477.51, 479.33, 480.93, 482.98, 484.35, 485.94, 488.45, 490.96, 492.78, 493.92, 495.06, 497.34, 500.07, 501.90, 
		503.49, 504.40, 505.77, 507.82, 507.82, 507.82, 508.05, 508.05, 508.05, 508.05];

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
	parameter "HH beta" var: beta_HH min: 0.019 max: 0.028 step: 0.001;
	parameter "COM beta" var: beta_COM min: 0.006 max: 0.015 step: 0.001; 
	//parameter "Scale" var: scale min: 0.85 max: 1.0 step: 0.01;
	
	method tabu minimize: sum_sq_err iter_max: 50 tabu_list_size: 5;
	
	reflex end_of_runs{
		ask simulations {
			write "SSE= " + sum_sq_err;
		} 
	}
}

