/***
* Name: TestandQuarantine
* Author: Michael L. Jackson
* Description: Model for impact of testing and quarantining HHs of positives
***/

model TestandQuarantine

/* This program estimates the course of the COVID-19 epidemic in the simulated population assuming */
/* that widespread testing is used to identify COVID-19 infections and that households of infected */
/* persons are quarantined until the end of their possible latent period. Use March 12th as the    */
/* date the intervention starts. Note that this leaves nursing homes, group quarters, schools, and */
/* workplace fully open.																		   */

import "../models/00_Base_Model.gaml"

/* Set up the global environment */
global {
	int max_days <- 150;

	//string dir <- "H:/Scratch/GAMAout/MPE0KG/";  	// Output directory
	string dir <- "C:/Users/O992928/Desktop/GAMAout/";
	
	float beta_HH  <- 0.024;			 	// Probability of infection given contact in household
	float beta_COM <- 0.012;				// Probability of infection given contact in workplace/community

	bool initialize_Settings <- false;
	bool initialize_Infectious <- false;
	bool initialize_csv <- false;
	bool initialize_unit_test <- false;		// Use only if testing quarantine effectiveness;
	
	// Social distancing variables
	// Set dates of interventions and initialize percent decline variables
	// Percent decline variables update in the UpdateDay reflex
	int trace_start_day <- 41; 				// Start contact tracing on day 41 of the simulation (March 12)
	float detect_prob <- 0.5;				// Probability that symptomatic person is detected by testing
	
	// Initialize model, specify the number of infectious and susceptible hosts
	init {		
		// Create settings (geographies where agents can be)
		create Home number: nb_home_init;
		create School number: nb_school_init;
		create Workplace number: nb_work_init;
		create Community number: nb_comm_init;
		create NH number: nb_nh_init;
		create GQ number: nb_gq_init;
		
		if initialize_unit_test = true {
			// For unit testing only
			// Create one Host of each community type in the first household to track
			// Do this first so that these have index 0
			create Toddler with: [sus::false, exp::true, inf::false, counter_exp::2, indexHome::0];
			create Child with: [sus::true, exp::false, inf::false, indexHome::0];
			create Adult with: [sus::true, exp::false, inf::false, indexHome::0];
			create Senior with: [sus::true, exp::false, inf::false, indexHome::0];
		}	
		
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
		
		if initialize_unit_test = false {
			// Create infectious host(s) under normal simulation runs
			loop times: nb_inf_init {
				ask one_of(agents of_generic_species(Host_Master)){
					self.sus <- false;
					self.inf <- true;
					self.counter_inf <- dur_infect[rnd_choice([0.25, 0.5, 0.25])];
				}
			} 
		} 

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
	}
}

/* TODDLER, a subset of Host ages 0-5 who is not assigned to a "school" (e.g. daycare, pre-school) */
species Toddler parent: Toddler_Master {
	bool detected <- false;
	bool quarantined <- false;
	int quarant_counter <- -1;
	int detect_counter <- -1;
	
	// Count-down to infection being detected
	// Quarantine self on detection
	reflex increment_detection when: detect_counter >= 0 and daypart = "evening" {
		detect_counter <- detect_counter - 1;
		if detect_counter = 0 {
			detected <- true;
			do make_quarantine;
		}
	}
	
	// Count-down for quarantine duration
	reflex increment_quarantine when: quarant_counter >= 0 and daypart = "evening" {
		quarant_counter <- quarant_counter - 1;
		if quarant_counter = 0 {
			quarantined <- false;
		}
	}
	
	// Modify make_symptomatic action to include detection probability
	action make_symptomatic {
		sym <- true;
		if flip(detect_prob){
			detect_counter <- 2;
		}
	}
	
	// Trigger detected status and quarantine of household members
	action make_quarantine {
		quarantined <- true;
		quarant_counter <- 14;
		ask agents of_generic_species Toddler where (each.indexHome = self.indexHome) {
			self.quarantined <- true;
			self.quarant_counter <- 14;
			self.location <- (Home at self.indexHome).location;
		}
		ask agents of_generic_species Child where (each.indexHome = self.indexHome) {
			self.quarantined <- true;
			self.quarant_counter <- 14;
			self.location <- (Home at self.indexHome).location;
		}
		ask agents of_generic_species Adult where (each.indexHome = self.indexHome) {
			self.quarantined <- true;
			self.quarant_counter <- 14;
			self.location <- (Home at self.indexHome).location;
		}
		ask agents of_generic_species Senior where (each.indexHome = self.indexHome) {
			self.quarantined <- true;
			self.quarant_counter <- 14;
			self.location <- (Home at self.indexHome).location;
		}
	}
	
	// Add quarantine modification to movement
	reflex take_actions {		
		// Move through SEIR states
		if self.sus = true {
			do count_exposures;
			if (self.infect_HH + infect_COM) >= 1 {
				do make_exposed;
			}
		} else if self.exp = true {
			do make_infectious;
		} else if self.inf = true {
			do make_recovered;
		}
		
		// Update symptomatic and hospitalization status
		if counter_sym = 0 {
			do make_symptomatic;
		}
		if daypart = "evening" and counter_hosp >= 0 {
			do make_hosp;
		}
		
		// After updating status, move as appropriate
		if quarantined = false {
			if daypart = "morning" {
				do move_morning;
			} else if daypart = "afternoon" {
				do move_afternoon;
			} else if daypart = "evening" {
				do move_evening;
			}
		}
	}
}

/* CHILD, a subset of Host ages 0-17, has a school (could be daycare for age <6) */
species Child parent: Child_Master {
	bool detected <- false;
	bool quarantined <- false;
	int quarant_counter <- -1;
	int detect_counter <- -1;
	
	// Count-down to infection being detected
	// Quarantine self on detection
	reflex increment_detection when: detect_counter >= 0 and daypart = "evening" {
		detect_counter <- detect_counter - 1;
		if detect_counter = 0 {
			detected <- true;
			do make_quarantine;
		}
	}
	
	// Count-down for quarantine duration
	reflex increment_quarantine when: quarant_counter >= 0 and daypart = "evening" {
		quarant_counter <- quarant_counter - 1;
		if quarant_counter = 0 {
			quarantined <- false;
		}
	}
	
	// Modify make_symptomatic action to include detection probability
	action make_symptomatic {
		sym <- true;
		if flip(detect_prob){
			detect_counter <- 2;
		}
	}
	
	// Trigger detected status and quarantine of household members
	action make_quarantine {
		quarantined <- true;
		quarant_counter <- 14;
		ask agents of_generic_species Toddler where (each.indexHome = self.indexHome) {
			self.quarantined <- true;
			self.quarant_counter <- 14;
			self.location <- (Home at self.indexHome).location;
		}
		ask agents of_generic_species Child where (each.indexHome = self.indexHome) {
			self.quarantined <- true;
			self.quarant_counter <- 14;
			self.location <- (Home at self.indexHome).location;
		}
		ask agents of_generic_species Adult where (each.indexHome = self.indexHome) {
			self.quarantined <- true;
			self.quarant_counter <- 14;
			self.location <- (Home at self.indexHome).location;
		}
		ask agents of_generic_species Senior where (each.indexHome = self.indexHome) {
			self.quarantined <- true;
			self.quarant_counter <- 14;
			self.location <- (Home at self.indexHome).location;
		}
	}

	// Add quarantine modification to movement
	reflex take_actions {		
		// Move through SEIR states
		if self.sus = true {
			do count_exposures;
			if (self.infect_HH + infect_COM) >= 1 {
				do make_exposed;
			}
		} else if self.exp = true {
			do make_infectious;
		} else if self.inf = true {
			do make_recovered;
		}
		
		// Update symptomatic and hospitalization status
		if counter_sym = 0 {
			do make_symptomatic;
		}
		if daypart = "evening" and counter_hosp >= 0 {
			do make_hosp;
		}
		
		// After updating status, move as appropriate
		if quarantined = false {
			if daypart = "morning" {
				do move_morning;
			} else if daypart = "afternoon" {
				do move_afternoon;
			} else if daypart = "evening" {
				do move_evening;
			}
		}
	}
}

/* ADULT, a subset of Host, ages 18 - 74. Could have a workplace, which could be a school, NH, or GQ */
species Adult parent: Adult_Master {
	bool detected <- false;
	bool quarantined <- false;
	int quarant_counter <- -1;
	int detect_counter <- -1;
	
	// Count-down to infection being detected
	// Quarantine self on detection
	reflex increment_detection when: detect_counter >= 0 and daypart = "evening" {
		detect_counter <- detect_counter - 1;
		if detect_counter = 0 {
			detected <- true;
			do make_quarantine;
		}
	}
	
	// Count-down for quarantine duration
	reflex increment_quarantine when: quarant_counter >= 0 and daypart = "evening" {
		quarant_counter <- quarant_counter - 1;
		if quarant_counter = 0 {
			quarantined <- false;
		}
	}
	
	// Modify make_symptomatic action to include detection probability
	action make_symptomatic {
		sym <- true;
		if flip(detect_prob){
			detect_counter <- 2;
		}
	}
	
		action make_quarantine {
		quarantined <- true;
		quarant_counter <- 14;
		ask agents of_generic_species Toddler where (each.indexHome = self.indexHome) {
			self.quarantined <- true;
			self.quarant_counter <- 14;
			self.location <- (Home at self.indexHome).location;
		}
		ask agents of_generic_species Child where (each.indexHome = self.indexHome) {
			self.quarantined <- true;
			self.quarant_counter <- 14;
			self.location <- (Home at self.indexHome).location;
		}
		ask agents of_generic_species Adult where (each.indexHome = self.indexHome) {
			self.quarantined <- true;
			self.quarant_counter <- 14;
			self.location <- (Home at self.indexHome).location;
		}
		ask agents of_generic_species Senior where (each.indexHome = self.indexHome) {
			self.quarantined <- true;
			self.quarant_counter <- 14;
			self.location <- (Home at self.indexHome).location;
		}
	}

	// Add quarantine modification to movement
	reflex take_actions {		
		// Move through SEIR states
		if self.sus = true {
			do count_exposures;
			if (self.infect_HH + infect_COM) >= 1 {
				do make_exposed;
			}
		} else if self.exp = true {
			do make_infectious;
		} else if self.inf = true {
			do make_recovered;
		}
		
		// Update symptomatic and hospitalization status
		if counter_sym = 0 {
			do make_symptomatic;
		}
		if daypart = "evening" and counter_hosp >= 0 {
			do make_hosp;
		}
		
		// After updating status, move as appropriate
		if quarantined = false {
			if daypart = "morning" {
				do move_morning;
			} else if daypart = "afternoon" {
				do move_afternoon;
			} else if daypart = "evening" {
				do move_evening;
			}
		}
	}
}

/* SENIOR, as subset of Host, ages 75 - 99 (no workplace) */
species Senior parent: Senior_Master {
	bool detected <- false;
	bool quarantined <- false;
	int quarant_counter <- -1;
	int detect_counter <- -1;
	
	// Count-down to infection being detected
	// Quarantine self on detection
	reflex increment_detection when: detect_counter >= 0 and daypart = "evening" {
		detect_counter <- detect_counter - 1;
		if detect_counter = 0 {
			detected <- true;
			do make_quarantine;
		}
	}
	
	// Count-down for quarantine duration
	reflex increment_quarantine when: quarant_counter >= 0 and daypart = "evening" {
		quarant_counter <- quarant_counter - 1;
		if quarant_counter = 0 {
			quarantined <- false;
		}
	}
	
	// Modify make_symptomatic action to include detection probability
	action make_symptomatic {
		sym <- true;
		if flip(detect_prob){
			detect_counter <- 2;
		}
	}
	
	action make_quarantine {
		quarantined <- true;
		quarant_counter <- 14;
		ask agents of_generic_species Toddler where (each.indexHome = self.indexHome) {
			self.quarantined <- true;
			self.quarant_counter <- 14;
			self.location <- (Home at self.indexHome).location;
		}
		ask agents of_generic_species Child where (each.indexHome = self.indexHome) {
			self.quarantined <- true;
			self.quarant_counter <- 14;
			self.location <- (Home at self.indexHome).location;
		}
		ask agents of_generic_species Adult where (each.indexHome = self.indexHome) {
			self.quarantined <- true;
			self.quarant_counter <- 14;
			self.location <- (Home at self.indexHome).location;
		}
		ask agents of_generic_species Senior where (each.indexHome = self.indexHome) {
			self.quarantined <- true;
			self.quarant_counter <- 14;
			self.location <- (Home at self.indexHome).location;
		}
	}

	// Add quarantine modification to movement
	reflex take_actions {		
		// Move through SEIR states
		if self.sus = true {
			do count_exposures;
			if (self.infect_HH + infect_COM) >= 1 {
				do make_exposed;
			}
		} else if self.exp = true {
			do make_infectious;
		} else if self.inf = true {
			do make_recovered;
		}
		
		// Update symptomatic and hospitalization status
		if counter_sym = 0 {
			do make_symptomatic;
		}
		if daypart = "evening" and counter_hosp >= 0 {
			do make_hosp;
		}
		
		// After updating status, move as appropriate
		if quarantined = false {
			if daypart = "morning" {
				do move_morning;
			} else if daypart = "afternoon" {
				do move_afternoon;
			} else if daypart = "evening" {
				do move_evening;
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
experiment TestAndQuarantine type: batch repeat: 1 until: (day >= max_days) parallel: true {
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

experiment UNIT_TEST_QUARANTINE type:gui until: (day >= max_days) {
	// Inspect Toddler[0], Child[0], Adult[0], and Senior[0]
	// Make sure quarantine occurs with proper timing, that movement stops during quarantine, and restarts after
	// Also inspect different Toddler/Child/Adult/Senior agents and make sure they don't quarantine
	
	float seed <- 1.0;
	parameter "Detection prob" var: detect_prob init: 1.0; // Make detection guaranteed to track quarantine process
	parameter "Unit testing" var: initialize_unit_test init: true;
	parameter "Number infectious" var: nb_inf_init init: 0;
	
}


