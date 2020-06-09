/***
* Name: MultiIntervention
* Author: Michael L. Jackson
* Description: 
***/

model MultiIntervention

/* Starting June 8th (simulation day 129), explore the possible effects of different interventions on */
/* COVID-19 hospitalizations into the future. Interventions to test include promoting voluntary work- */
/* from-home to the extent possible, cocooning seniors, and test-and quarantine. This program allows  */
/* each of these different interventions, with settings in different Experiments used to implement    */
/* the specific options. Starts with modeling the interventions as actually implemented through May.  */

import "../models/00_Base_Model.gaml"

/* Set up the global environment */
global {
	int max_days <- 366;

	string dir <- "H:/Scratch/GAMAout/MPE0KG/";  	// Output directory
	
	float beta_HH  <- 0.027;			 	// Probability of infection given contact in household
	float beta_COM <- 0.010;				// Probability of infection given contact in workplace/community

	bool initialize_Settings <- false;
	bool initialize_Infectious <- false;
	bool initialize_csv <- false;
	
	// Social distancing variables
	// Set dates of interventions and initialize percent decline variables
	// Percent decline variables update in the UpdateDay reflex
	float comm_open_pct <- 1.0;						// Percent community site visits occurring during specified time period
	float work_open_pct <- 1.0;						// Percent of work occuring during specified time period

	list<int>   change_days <- [34, 41, 45, 57, 129];				// Simulation days when work interventions change	
	list<float> work_close_pcts <- [0.3555, 0.5625, 0.603, 0.81, 0.395];	// Percent reductions in work contacts at different periods
	list<float> comm_close_pcts <- [0.3555, 0.5625, 0.603, 0.81, 0.0];		// Percent reductions in community contacts at different periods
	list<float> nhgq_close_pcts <- [1.0, 1.0, 1.0, 1.0, 1.0, 0.99];		// Percent reductions in NH/GQ visits 

	int school_close_day <- 41; 					// Close schools on day 41 of the simulation (March 12)
	int school_open_day <- 221;						// Simulation day when schools open
	bool school_open <- true;						// Flag for whether school is open

	// Test-and-quarantine variables
	bool use_test_trace <- false;
	int trace_start_day <- 129;
	float detect_prob <- 0.5;
	float quarantine_prob <- 0.7;
	
	// Senior shelter-in-place variable
	bool use_senior_cocoon <- false;
	int cocoon_start_day <- 129;
	float cocoon_prob <- 0.9;
	float senior_travel_prob <- 1.0;
	
	// Initialize model, specify the number of infectious and susceptible hosts
	init {		
		// Create settings (geographies where agents can be)
		create Home number: nb_home_init;
		create QuarantineFlag number: nb_home_init;
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
		
		// Update work, community, and NH/GQ visit probabilities
		if day < change_days[0] {
			comm_open_pct <- 1.0;
			work_open_pct <- 1.0;
		} else if day < change_days[1] {
			comm_open_pct <- 1-comm_close_pcts[0];
			work_open_pct <- 1-work_close_pcts[0];
			prob_nhgq_visit <- 1-nhgq_close_pcts[0];
		} else if day < change_days[2] {
			comm_open_pct <- 1-comm_close_pcts[1];
			work_open_pct <- 1-work_close_pcts[1];
			prob_nhgq_visit <- 1-nhgq_close_pcts[1];
		} else if day < change_days[3] {
			comm_open_pct <- 1-comm_close_pcts[2];
			work_open_pct <- 1-work_close_pcts[2];
			prob_nhgq_visit <- 1-nhgq_close_pcts[2];
		} else if day < change_days[4]{
			comm_open_pct <- 1-comm_close_pcts[3];
			work_open_pct <- 1-work_close_pcts[3];
			prob_nhgq_visit <- 1-nhgq_close_pcts[3];
		} else {
			comm_open_pct <- 1-comm_close_pcts[4];
			work_open_pct <- 1-work_close_pcts[4];
			prob_nhgq_visit <- 1-nhgq_close_pcts[4];
		}
		
		// Flag for whether school is in session
		if day < school_close_day {
			school_open <- true;
		} else if day < school_open_day {
			school_open <- false;
		} else {
			school_open <- true;
		}
		
		// Flag for senior cocoon probability
		if use_senior_cocoon = true {
			if day < cocoon_start_day {
				senior_travel_prob <- 1.0;
			} else {
				senior_travel_prob <- cocoon_prob;
			}
		}
	}
}

/* TODDLER, a subset of Host ages 0-5 who is not assigned to a "school" (e.g. daycare, pre-school) */
species Toddler parent: Toddler_Master {
	bool detected <- false;
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
	
	// Modify make_symptomatic action to include detection probability
	action make_symptomatic {
		sym <- true;
		if flip(detect_prob) and day >= trace_start_day {
			detect_counter <- 2;
		}
	}
	
	// Trigger detected status and quarantine of household members
	action make_quarantine {
		ask (QuarantineFlag at self.indexHome) {
			if flip(quarantine_prob){
				self.under_quarantine <- true;
				self.quarantine_counter <- 14;
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
	
	// Add quarantine modification to actions and movement
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
		if (QuarantineFlag at self.indexHome).under_quarantine = false {
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
	
	// Modify make_symptomatic action to include detection probability
	action make_symptomatic {
		sym <- true;
		if flip(detect_prob) and day >= trace_start_day {
			detect_counter <- 2;
		}
	}
	
	// Trigger detected status and quarantine of household members
	action make_quarantine {
		ask (QuarantineFlag at self.indexHome) {
			if flip(quarantine_prob){
				self.under_quarantine <- true;
				self.quarantine_counter <- 14;
			}
		} 
	}
	
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
	
	// Add quarantine modification to actions and movement
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
		if (QuarantineFlag at self.indexHome).under_quarantine = false {
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
	
	// Modify make_symptomatic action to include detection probability
	action make_symptomatic {
		sym <- true;
		if flip(detect_prob) and day >= trace_start_day {
			detect_counter <- 2;
		}
	}
	
	// Trigger detected status and quarantine of household members
	action make_quarantine {
		ask (QuarantineFlag at self.indexHome) {
			if flip(quarantine_prob){
				self.under_quarantine <- true;
				self.quarantine_counter <- 14;
			}
		} 
	}
	
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
	
	// Add quarantine modification to actions and movement
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
		if (QuarantineFlag at self.indexHome).under_quarantine = false {
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
	
	// Modify make_symptomatic action to include detection probability
	action make_symptomatic {
		sym <- true;
		if flip(detect_prob) and day >= trace_start_day {
			detect_counter <- 2;
		}
	}
	
	// Trigger detected status and quarantine of household members
	action make_quarantine {
		ask (QuarantineFlag at self.indexHome) {
			if flip(quarantine_prob){
				self.under_quarantine <- true;
				self.quarantine_counter <- 14;
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
		} else if flip(prob_nhgq_visit * senior_travel_prob) {
			self.location <- one_of(NH).location;
		} else if flip(prob_nhgq_visit * senior_travel_prob) {
			self.location <- one_of(GQ).location;
		} else if flip(prob_community_wkdy * comm_open_pct * senior_travel_prob) and weekday in ["Mo", "Tu", "We", "Th", "Fr"] {
			self.location <- one_of(Community).location;
		} else if flip(prob_community_wknd * comm_open_pct * senior_travel_prob) and weekday in ["Sa", "Su"]{
			self.location <- one_of(Community).location;
		} else {
			self.location <- (Home at indexHome).location;
		}
	}
	
	// Add quarantine modification to actions and movement
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
		if (QuarantineFlag at self.indexHome).under_quarantine = false {
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

/* Species that identifies households under quarantine */
species QuarantineFlag {
	bool under_quarantine <- false;
	int quarantine_counter <- -1;
	
	reflex increment_quarantine when: quarantine_counter >= 0 and daypart = "evening"{
		quarantine_counter <- quarantine_counter - 1;
		if quarantine_counter = 0 {
			under_quarantine <- false;
		}
	}
}

/* Experiment for voluntary work-from-home as the only intervention after June 7th */
experiment Voluntary_WFH type: batch repeat: 1 until: (day >= max_days) parallel: true {
	float seedValue <- rnd(1.0, 10000.0);
	float seed <- seedValue;

	parameter "Starting infectious" var: nb_inf_init init: 1;

	// Parameters for closures
	parameter "Workplace closure" var: work_close_pcts init: [0.395, 0.625, 0.67, 0.9, 0.395];
	parameter "Community closure" var: comm_close_pcts init: [0.395, 0.625, 0.67, 0.9, 0.0];
	parameter "NH/GQ closure" var: nhgq_close_pcts init: [1.0, 1.0, 1.0, 1.0, 1.0, 0.999];
	
	parameter "School open day" var: school_open_day init: 221;
	
	init{
		create simulation with: [seed::seedValue + 1, model_number::1, nb_inf_init::1];
		create simulation with: [seed::seedValue + 2, model_number::2, nb_inf_init::1];
		create simulation with: [seed::seedValue + 3, model_number::3, nb_inf_init::1];
		create simulation with: [seed::seedValue + 4, model_number::4, nb_inf_init::0];
		create simulation with: [seed::seedValue + 5, model_number::5, nb_inf_init::0];
		create simulation with: [seed::seedValue + 6, model_number::6, nb_inf_init::0];
		create simulation with: [seed::seedValue + 7, model_number::7, nb_inf_init::0];
		create simulation with: [seed::seedValue + 8, model_number::8, nb_inf_init::0];
		create simulation with: [seed::seedValue + 9, model_number::9, nb_inf_init::0];
	}
}

experiment WFH_Plus_Schools_Close type: batch repeat: 1 until: (day >= max_days) parallel: true {
		float seedValue <- rnd(1.0, 10000.0);
	float seed <- seedValue;

	parameter "Starting infectious" var: nb_inf_init init: 1;
	
	parameter "Workplace closure" var: work_close_pcts init: [0.3555, 0.5625, 0.603, 0.81, 0.395];
	parameter "Community closure" var: comm_close_pcts init: [0.3555, 0.5625, 0.603, 0.81, 0.0];
	parameter "NH/GQ closure" var: nhgq_close_pcts init: [1.0, 1.0, 1.0, 1.0, 1.0, 0.999];	

	// Parameters for schools
	parameter "School open day" var: school_open_day init: 366;	

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