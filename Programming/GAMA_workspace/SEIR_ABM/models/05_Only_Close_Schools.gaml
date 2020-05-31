/***
* Name: OnlyCloseSchools
* Author: Mike Jackson
* Description: Estimate course of the epidemic if closing schools were the only intervention
* Tags: Tag1, Tag2, TagN
***/

model OnlyCloseSchools

/* This program estimates the course of the COVID-19 epidemic in the simulated population assuming */
/* that closing schools was the only intervention. Use March 12th as the date of the intervention. */
/* Note that this leaves workers, community sites, and nursing home/group quarter visits unchanged.*/

import "../models/00_Base_Model.gaml"

/* Set up the global environment */
global {
	int max_days <- 120;
	
	string dir <- "H:/Scratch/GAMAout/MPE0KG/";  	// Output directory
	
	float beta_HH  <- 0.024;			 	// Probability of infection given contact in household
	float beta_COM <- 0.012;				// Probability of infection given contact in workplace/community

	bool initialize_Settings <- false;
	bool initialize_Infectious <- false;
	bool initialize_csv <- false;
	
	// Social distancing variables
	// Set dates of interventions and initialize percent decline variables
	// Percent decline variables update in the UpdateDay reflex
	int school_close_day <- 41; 				// Close schools on day 41 of the simulation (March 12)
	
	// Initialize model, specify the number of infectious and susceptible hosts
	init {		
		// Create settings (geographies where agents can be)
		create Home number: nb_home_init;
		create School number: nb_school_init;
		create Workplace number: nb_work_init;
		create Community number: nb_comm_init;
		create NH number: nb_nh_init;
		create GQ number: nb_gq_init;
		
		create Toddler_Master from: csv_file("../includes/sim_Toddler_50k_" + model_number + ".csv", true) 
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
		create Senior_Master from: csv_file("../includes/sim_Senior_50k_" + model_number + ".csv", true) 
					with: [sus::true, indexHome::int(get("indexHome")), 
							ageyrs::int(get("ageyrs")), male::bool(get("male"))];
		create NHresident_Master from: csv_file("../includes/sim_NH_50k_" + model_number + ".csv", true)
					with: [sus::true, ageyrs::int(get("ageyrs")), male::bool(get("male")), indexNH::int(get("indexNH"))];
		create GQresident_Master from: csv_file("../includes/sim_GQ_50k_" + model_number + ".csv", true)
					with: [sus::true, ageyrs::int(get("ageyrs")), male::bool(get("male")), indexGQ::int(get("indexGQ"))];
					
		loop times: nb_inf_init {
			ask one_of(Adult){
				self.sus <- false;
				self.inf <- true;
				self.counter_inf <- dur_infect[rnd_choice([0.25, 0.5, 0.25])];
			}
		} 					
	}

	// Modify the update_counts action to track Child not Child_Master etc
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
		sus_tod_list[day] <- Toddler_Master count (each.sus);
		sus_chi_list[day] <- Child count (each.sus);
		sus_adu_list[day] <- Adult count (each.sus);
		sus_sen_list[day] <- Senior_Master count (each.sus);
		sus_nh_list[day] <- NHresident_Master count (each.sus);
		sus_gq_list[day] <- GQresident_Master count (each.sus);
		
		exp_tod_list[day] <- Toddler_Master count (each.exp);
		exp_chi_list[day] <- Child count (each.exp);
		exp_adu_list[day] <- Adult count (each.exp);
		exp_sen_list[day] <- Senior_Master count (each.exp);
		exp_nh_list[day] <- NHresident_Master count (each.exp);
		exp_gq_list[day] <- GQresident_Master count (each.exp);
		
		inf_tod_list[day] <- Toddler_Master count (each.inf);
		inf_chi_list[day] <- Child count (each.inf);
		inf_adu_list[day] <- Adult count (each.inf);
		inf_sen_list[day] <- Senior_Master count (each.inf);
		inf_nh_list[day] <- NHresident_Master count (each.inf);
		inf_gq_list[day] <- GQresident_Master count (each.inf);
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
		} else if weekday in ["Mo", "Tu", "We", "Th", "Fr"] {
			if indexWorkplace >= 0 {
				self.location <- (Workplace at indexWorkplace).location;
			} else if indexSchool >= 0 and day < school_close_day {
				self.location <- (School at indexSchool).location; 	// Teachers work at school
			} else if indexNH >= 0 {
				self.location <- (NH at indexNH).location;			// NH workers
			} else if indexGQ >= 0 {
				self.location <- (GQ at indexGQ).location;			// GQ workers
			}
		}
	}
}

/* Run the simulation in batch mode */
experiment OnlySchools type: batch repeat: 1 until: (day >= max_days) parallel: true {
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


