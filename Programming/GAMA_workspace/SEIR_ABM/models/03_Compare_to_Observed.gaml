/***
* Name: ComparetoObsereved
* Author: Michael L. Jackson
* Description: Run the model under the observed interventions in the Seattle area
* Tags: Tag1, Tag2, TagN
***/

model ComparetoObsereved

/* Third program in the sequence of evaluating the impact of different COVID-19 control measures.  */
/* This program attempts to reproduce, as closely as possible, the actual interventions undertaken */
/* in the Seattle area during March and April 2020. Simulated incidence and hospitalization are    */
/* then compared with observed data to see whether the simulation is reasonably reproducing the	   */
/* actual course of the epidemic so far.														   */
/* A few assumptions:																			   */
/* Workers at nursing homes + group quarters still go to work regardless of other work reductions. */
/* NH and GQ are closed to visitors (but not staff) as of the first intervention time point.       */

import "../models/00_Base_Model.gaml"

/* Set up the global environment */
global {
	int max_days <- 130;

	string dir <- "H:/Scratch/GAMAout/MPE0KG/";  	// Output directory
		
	float beta_HH  <- 0.027;			 	// Probability of infection given contact in household
	float beta_COM <- 0.010;				// Probability of infection given contact in workplace/community

	bool initialize_Settings <- false;
	bool initialize_Infectious <- false;
	bool initialize_csv <- false;
	
	// Social distancing variables
	// Set dates of interventions and initialize percent decline variables
	// Percent decline variables update in the UpdateDay reflex
	float comm_open_pct <- 1.0;				// Percent community site visits occurring during specified time period
	float work_open_pct <- 1.0;				// Percent of work occuring during specified time period
	int school_close_day <- 41; 				// Close schools on day 41 of the simulation (March 12)
	list<int> close_days <- [34, 41, 45, 57];	// Simulation days when interventions change	
	list<float> close_pcts <- [0.3555, 0.5625, 0.603, 0.81];	// Percent reductions in contacts at different intervention periods

	
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
		create Adult number: nb_inf_init with: [sus::false, inf::true]; // Starting number infectious
		
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
	}

	// Modify the update_counts action to track Toddler not Toddler_Master etc
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
		
		if day < close_days[0] {
			comm_open_pct <- 1.0;
			work_open_pct <- 1.0;
		} else if day < close_days[1] {
			comm_open_pct <- 1-close_pcts[0];
			work_open_pct <- 1-close_pcts[0];
		} else if day < close_days[2] {
			comm_open_pct <- 1-close_pcts[1];
			work_open_pct <- 1-close_pcts[1];
		} else if day < close_days[3] {
			comm_open_pct <- 1-close_pcts[2];
			work_open_pct <- 1-close_pcts[2];
		} else {
			comm_open_pct <- 1-close_pcts[3];
			work_open_pct <- 1-close_pcts[3];
		}
	}
}

/* TODDLER, a subset of Host ages 0-5 who is not assigned to a "school" (e.g. daycare, pre-school) */
species Toddler parent: Toddler_Master {
	int ageyrs <- rnd(0, 5);	
	
	action move_afternoon {
		// Afternoon: some proportion go to random GQ or NH, some go to random community location, others go home;
		// Check for NH visit. If not, check for GQ visit (same prob). If not, check for community
		// Varies based on weekday vs. weekend
		if day < close_days[0] {
			if outside_sim = true {
				// If infecting someone in another sub-population, set location outside the grid
				self.location <- point(-1, -1, 0);
				outside_sim <- false;
			} else if flip(prob_nhgq_visit) {
				self.location <- one_of(NH).location;
			} else if flip(prob_nhgq_visit) {
				self.location <- one_of(GQ).location;
			} else if flip(prob_community_wkdy) and weekday in ["Mo", "Tu", "We", "Th", "Fr"] {
				self.location <- one_of(Community).location;
			} else if flip(prob_community_wknd) and weekday in ["Sa", "Su"]{
				self.location <- one_of(Community).location;
			} else {
				self.location <- (Home at indexHome).location;
			}
		} else {
			// Assume NH and GQ are closed to visitors after the intervention starts
			if outside_sim = true {
				self.location <- point(-1, -1, 0);
			} else if flip(prob_community_wkdy * comm_open_pct) and weekday in ["Mo", "Tu", "We", "Th", "Fr"] {
				self.location <- one_of(Community).location;
			} else if flip(prob_community_wknd * comm_open_pct) and weekday in ["Sa", "Su"] {
				self.location <- one_of(Community).location;
			} else {
				self.location <- (Home at indexHome).location;
			}
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
		} else if weekday in ["Mo", "Tu", "We", "Th", "Fr"] and day < school_close_day {
			// Morning: children go to school
			self.location <- (School at indexSchool).location;
		} 
	}
	
	action move_afternoon {
		// Afternoon: some proportion go to random GQ or NH, some go to random community location, others go home;
		// Check for NH visit. If not, check for GQ visit (same prob). If not, check for community
		// Varies based on weekday vs. weekend
		if day < close_days[0] {
			if outside_sim = true {
				// If infecting someone in another sub-population, set location outside the grid
				self.location <- point(-1, -1, 0);
				outside_sim <- false;
			} else if flip(prob_nhgq_visit) {
				self.location <- one_of(NH).location;
			} else if flip(prob_nhgq_visit) {
				self.location <- one_of(GQ).location;
			} else if flip(prob_community_wkdy) and weekday in ["Mo", "Tu", "We", "Th", "Fr"] {
				self.location <- one_of(Community).location;
			} else if flip(prob_community_wknd) and weekday in ["Sa", "Su"]{
				self.location <- one_of(Community).location;
			} else {
				self.location <- (Home at indexHome).location;
			}
		} else {
			// Assume NH and GQ are closed to visitors after the intervention starts
			if outside_sim = true {
				self.location <- point(-1, -1, 0);
			} else if flip(prob_community_wkdy * comm_open_pct) and weekday in ["Mo", "Tu", "We", "Th", "Fr"] {
				self.location <- one_of(Community).location;
			} else if flip(prob_community_wknd * comm_open_pct) and weekday in ["Sa", "Su"] {
				self.location <- one_of(Community).location;
			} else {
				self.location <- (Home at indexHome).location;
			}
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
				if day < school_close_day{ 
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
		if day < close_days[0] {
			if outside_sim = true {
				// If infecting someone in another sub-population, set location outside the grid
				self.location <- point(-1, -1, 0);
				outside_sim <- false;
			} else if flip(prob_nhgq_visit) {
				self.location <- one_of(NH).location;
			} else if flip(prob_nhgq_visit) {
				self.location <- one_of(GQ).location;
			} else if flip(prob_community_wkdy) and weekday in ["Mo", "Tu", "We", "Th", "Fr"] {
				self.location <- one_of(Community).location;
			} else if flip(prob_community_wknd) and weekday in ["Sa", "Su"]{
				self.location <- one_of(Community).location;
			} else {
				self.location <- (Home at indexHome).location;
			}
		} else {
			// Assume NH and GQ are closed to visitors after the intervention starts
			if outside_sim = true {
				self.location <- point(-1, -1, 0);
			} else if flip(prob_community_wkdy * comm_open_pct) and weekday in ["Mo", "Tu", "We", "Th", "Fr"] {
				self.location <- one_of(Community).location;
			} else if flip(prob_community_wknd * comm_open_pct) and weekday in ["Sa", "Su"] {
				self.location <- one_of(Community).location;
			} else {
				self.location <- (Home at indexHome).location;
			}
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
		if day < close_days[0] {
			if outside_sim = true {
				// If infecting someone in another sub-population, set location outside the grid
				self.location <- point(-1, -1, 0);
				outside_sim <- false;
			} else if flip(prob_nhgq_visit) {
				self.location <- one_of(NH).location;
			} else if flip(prob_nhgq_visit) {
				self.location <- one_of(GQ).location;
			} else if flip(prob_community_wkdy) and weekday in ["Mo", "Tu", "We", "Th", "Fr"] {
				self.location <- one_of(Community).location;
			} else if flip(prob_community_wknd) and weekday in ["Sa", "Su"]{
				self.location <- one_of(Community).location;
			} else {
				self.location <- (Home at indexHome).location;
			}
		} else {
			// Assume NH and GQ are closed to visitors after the intervention starts
			if outside_sim = true {
				self.location <- point(-1, -1, 0);
			} else if flip(prob_community_wkdy * comm_open_pct) and weekday in ["Mo", "Tu", "We", "Th", "Fr"] {
				self.location <- one_of(Community).location;
			} else if flip(prob_community_wknd * comm_open_pct) and weekday in ["Sa", "Su"] {
				self.location <- one_of(Community).location;
			} else {
				self.location <- (Home at indexHome).location;
			}
		}
	}
}

/* NHresident, a subset of Host for those in nursing homes. Does not move */
species NHresident parent: NHresident_Master {
}

/* GQresident, a subset of Host for those in group quarters. Does not move */
species GQresident parent: GQresident_Master {
}


/* Run the simulation in batch mode */
experiment SEIR_vsObserved type: batch repeat: 1 until: (day >= max_days) parallel: true {
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
