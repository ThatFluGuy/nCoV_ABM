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
	int max_days <- 117;

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

	list<int>   change_days <- [34, 41, 45, 57];					// Simulation days when work interventions change	
	list<float> work_close_pcts <- [0.395, 0.625, 0.67, 0.9];	// Percent reductions in work contacts at different periods
	list<float> comm_close_pcts <- [0.395, 0.625, 0.67, 0.9];	// Percent reductions in community contacts at different periods
	list<float> nhgq_close_pcts <- [1.0, 1.0, 1.0, 1.0, 1.0];		// Percent reductions in NH/GQ visits 

	int school_close_day <- 41; 					// Close schools on day 41 of the simulation (March 12)
	bool school_open <- true;						// Flag for whether school is open

	// Track cumulative hospitalizations and optimize
	list<int> n_cumul_hosp <- list_with(max_days+1, 0);
	float sum_sq_err <- 0.0;
	float scale <- 1.0; // Scale the close percents list
	
	list<float> obs_hosp_list <- [1.41, 1.57, 1.66, 1.75, 2.04, 2.29, 2.53, 3.03, 3.32, 3.57, 3.88, 4.22, 4.69, 5.02, 5.54, 5.87, 
			6.23, 6.52, 7.04, 7.44, 7.78, 8.34, 8.86, 9.37, 9.84, 10.78, 11.35, 12.06, 12.69, 13.43, 14.35, 15.2, 16.03, 16.93, 17.8, 
			18.68, 19.66, 20.22, 20.96, 21.48, 22.15, 22.65, 23.27, 23.88, 24.48, 24.87, 25.2, 25.58, 26.1, 26.64, 27.15, 27.6, 28.05, 
			28.48, 28.9, 29.35, 29.66, 29.98, 30.36, 30.67, 30.85, 31.21, 31.52, 31.7, 31.93, 32.26, 32.4, 32.67, 32.94, 33.18, 33.34, 
			33.54, 33.72, 33.99, 34.26, 34.42, 34.6, 34.78, 34.89, 35, 35.07, 35.29, 35.47, 35.67, 35.76, 36.03, 36.17, 36.3, 36.43, 
			36.5, 36.61];

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
			
			if day >= 26 {
				sum_sq_err <- sum_sq_err + (n_cumul_hosp[day] - obs_hosp_list[day-26])^2;
			}
		}
	}

	// Modify the update_day action to calculate probabilities of going to work or community based on closures
	action update_day {
		// Update simulation day and weekday
		day <- day + 1;
		if weekday = "Su"{
			weekday <- "Mo";
		} else if weekday = "Mo" {
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
		
		// Update work, community, and NH/GQ visit probabilities
		if day < change_days[0] {
			comm_open_pct <- 1.0;
			work_open_pct <- 1.0;
		} else if day < change_days[1] {
			comm_open_pct <- 1-(comm_close_pcts[0]*scale);
			work_open_pct <- 1-(work_close_pcts[0]*scale);
			prob_nhgq_visit <- 1-nhgq_close_pcts[0];
		} else if day < change_days[2] {
			comm_open_pct <- 1-(comm_close_pcts[1]*scale);
			work_open_pct <- 1-(work_close_pcts[1]*scale);
			prob_nhgq_visit <- 1-nhgq_close_pcts[1];
		} else if day < change_days[3] {
			comm_open_pct <- 1-(comm_close_pcts[2]*scale);
			work_open_pct <- 1-(work_close_pcts[2]*scale);
			prob_nhgq_visit <- 1-nhgq_close_pcts[2];
		} else {
			comm_open_pct <- 1-(comm_close_pcts[3]*scale);
			work_open_pct <- 1-(work_close_pcts[3]*scale);
			prob_nhgq_visit <- 1-nhgq_close_pcts[3];
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
	parameter "Scale" var: scale min: 0.85 max: 1.0 step: 0.01;
	
	method tabu minimize: sum_sq_err iter_max: 50 tabu_list_size: 5;
	
	reflex end_of_runs{
		ask simulations {
			write "SSE= " + sum_sq_err;
		} 
	}
}

