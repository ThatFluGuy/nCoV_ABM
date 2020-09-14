/***
* Name: ObservedThroughAugust
* Author: O992928
* Description: 
* Tags: Tag1, Tag2, TagN
***/

model ObservedThroughAugust

/* Fourth program in the updated sequence for evaluating COVID-19 control measures using the new   */
/* mobility data. This program runs the simulation through August 25th (the last date of observed  */
/* hospitalization data). It saves the final state of the simulation for use in future programs.   */
/* It also saves the summary calculations of the number in each host type/SEIR state + the number  */
/* hospitalized by day and by age. 																   */
/* Data from this program are used to calculate effective reproductive numbers during the observed */
/* time period, and to feed into simulations that start after the observed period (saving time by  */
/* avoiding re-running February - August for each model run).									   */

import "../models/00_Base_Model.gaml"

/* Set up the global environment */
global {
	int max_days <- 3;

	int sim_number <- 0;

	float beta_HH  <- 0.015;			 	// Probability of infection given contact in household
	float beta_COM <- 0.010;				// Probability of infection given contact in workplace/community

	bool initialize_Settings <- false;
	bool initialize_Infectious <- false;
	bool initialize_csv <- false;
	
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
	
	reflex save_output when: day=max_days{
		// Save population summary data
		save sus_tod_list type: "csv" to: dir + "toddler_sus_" + model_number  + ".csv";
	    save sus_chi_list type: "csv" to: dir + "child_sus_" + model_number + ".csv";
	    save sus_adu_list type: "csv" to: dir + "adult_sus_" + model_number  + ".csv";
	    save sus_sen_list type: "csv" to: dir + "senior_sus_" + model_number  + ".csv";
	    save sus_nh_list type: "csv" to: dir + "nh_sus_" + model_number  + ".csv";
	    save sus_gq_list type: "csv" to: dir + "gq_sus_" + model_number  + ".csv";
	    	
	    save exp_tod_list type: "csv" to: dir + "toddler_exp_" + model_number  + ".csv";
	    save exp_chi_list type: "csv" to: dir + "child_exp_" + model_number  + ".csv";
	    save exp_adu_list type: "csv" to: dir + "adult_exp_" + model_number  + ".csv";
	    save exp_sen_list type: "csv" to: dir + "senior_exp_" + model_number  + ".csv";
	    save exp_nh_list type: "csv" to: dir + "nh_exp_" + model_number  + ".csv";
	    save exp_gq_list type: "csv" to: dir + "gq_exp_" + model_number  + ".csv";
	    	
	    save inf_tod_list type: "csv" to: dir + "toddler_inf_" + model_number  + ".csv";
	    save inf_chi_list type: "csv" to: dir + "child_inf_" + model_number  + ".csv";
	    save inf_adu_list type: "csv" to: dir + "adult_inf_" + model_number  + ".csv";
	    save inf_sen_list type: "csv" to: dir + "senior_inf_" + model_number  + ".csv";
	    save inf_nh_list type: "csv" to: dir + "nh_inf_" + model_number + ".csv";
	    save inf_gq_list type: "csv" to: dir + "gq_inf_" + model_number + ".csv";
	    	
	    save n_hosp_list type: "csv" to: dir + "hosp" + model_number  + ".csv";
	    save hosp_age_list type: "csv" to: dir + "hospage" + model_number + ".csv";
	    
	    // Save current model state
	    ask Toddler {
	    	save [name, indexHome, ageyrs, male, sus, exp, inf, sym, outside_sim, counter_exp, counter_inf, counter_sym, counter_hosp] 
	    	to: dir + "Toddler_" + sim_number + "_" + model_number + ".csv" type:"csv" rewrite: false;
	    }
	    ask Child {
	    	save [name, indexHome, indexSchool, ageyrs, male, sus, exp, inf, sym, outside_sim, counter_exp, counter_inf, 
	    		counter_sym, counter_hosp] to: dir + "Child_" + sim_number + "_" + model_number + ".csv" type:"csv" rewrite:false;
	    }
	    ask Adult {
	    	save [name, indexHome, indexSchool, indexWorkplace, indexNH, indexGQ, ageyrs, male, sus, exp, inf, sym, 
	    		outside_sim, counter_exp, counter_inf, counter_sym, counter_hosp]
	    		to: dir + "Adult_" + sim_number + "_" + model_number + ".csv" type:"csv" rewrite: false;
	    }
	    ask Senior {
	    	save [name, indexHome, ageyrs, male, sus, exp, inf, sym, outside_sim, counter_exp, counter_inf, counter_sym, counter_hosp]
	    	to: dir + "Senior_" + sim_number + "_" + model_number + ".csv" type:"csv" rewrite: false;
	    }
	    ask NHresident {
	    	save [name, indexNH, ageyrs, male, sus, exp, inf, sym, outside_sim, counter_exp, counter_inf, counter_sym, counter_hosp]
	    	to: dir + "NHresident_" + sim_number + "_" + model_number + ".csv" type:"csv" rewrite: false;
	    }
	    ask GQresident {
	    	save [name, indexGQ, ageyrs, male, sus, exp, inf, sym, outside_sim, counter_exp, counter_inf, counter_sym, counter_hosp]
	    	to: dir + "GQresident_" + sim_number + "_" + model_number + ".csv" type:"csv" rewrite: false;
	    }
	       
	    do die;						
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
experiment Obs_to_Aug type: batch repeat: 1 keep_seed: true until: (day >= max_days) parallel: true {
	float seedValue <- rnd(1.0, 10000.0);
	float seed <- seedValue;
	 
	init{
		create simulation with: [seed::seedValue + 1, model_number::1, nb_inf_init::2];
		create simulation with: [seed::seedValue + 2, model_number::2, nb_inf_init::2];
		create simulation with: [seed::seedValue + 3, model_number::3, nb_inf_init::0];
		create simulation with: [seed::seedValue + 4, model_number::4, nb_inf_init::0];
		create simulation with: [seed::seedValue + 5, model_number::5, nb_inf_init::0];
		create simulation with: [seed::seedValue + 6, model_number::6, nb_inf_init::0];
		create simulation with: [seed::seedValue + 7, model_number::7, nb_inf_init::0];
		create simulation with: [seed::seedValue + 8, model_number::8, nb_inf_init::0];
		create simulation with: [seed::seedValue + 9, model_number::9, nb_inf_init::0];
	}
	
}



