/***
* Name: 00_Base_Model.gaml
* Author: Michael L. Jackson
* Description: Base model for all SARS-CoV-2 simulations 
***/

model BaseModel

/* This is the base model for estimating the impact of different social distancing interventions on incidence of SARS-CoV-2. */
/* It defines the global environment and the host and location agents. It also includes a basic single-run experiment.		 */
/* The program divides the full population (approximately 500,000 individuals) into 10 sub-patches (each of which is a 		 */
/* separate simulation); infectious agents in one patch may infect agents in other patches. 								 */

/* Transmission is modeled by identifying all the infectious contacts each host has at each time point, and calculating the  */
/* total probability of infection based on the per-contact probability of infection. Contacts within a household are assumed */
/* to be more likely to spread the virus than contact in other settings (schools, workplaces, etc). 						 */
/* To count infectious contacts, at the start of each time step, each location counts the number of infectious agents at the */
/* location. Next, each host has a chance to be infected based on the number of infectious contacts at their location, and   */
/* then (potentially) moves to a new location. This approach is used, rather than having hosts directly calculate their own  */
/* contacts based on shared locations, because all of an individual host's actions are evaluated before moving to the next   */
/* host. This means that some hosts move locations before other hosts can "find" them as contacts.							 */

/* For scheduling, set Host_Master and the Setting species all to have null schedules. Then create a non-initiated species 	 */
/* called "timekeeper" that schedules the Settings, then the Hosts. Defining the order explictly this way means that 		 */
/* locations update the number of hosts in them before the hosts calculate infectious contacts and move.					 */


/* Set up the global environment */
global {
	int max_days <- 100;				// Number of days to simulate
	
	// Output directory
	string dir <- "H:/Scratch/GAMAout/MPE0KG/";
	
	// Initialization variables
	float step <- 8 #h;					// Time step per model iteration (8 hours)
	int nb_inf_init <- 2;				// Starting number infectious 
	int nb_home_init <- 21070;			// Number of households
	int nb_school_init <- 68;			// Number of schools
	int nb_work_init <- 1025;			// Number of workplaces
	int nb_comm_init <- 26000; 			// Number of community locations; scaled to have up to 20 contacts (on average) per location
	int nb_nh_init <- 6;				// Number of nursing homes
	int nb_gq_init <- 34;				// Number of group quarters
	int neighbors_size <- 0;	 		// Distance to look for infectious contacts (i.e. only within host's current location)
	bool initialize_csv <- true;		// Import population data from .csv files? (Allows unit tests to ignore .csv import)
	bool initialize_Settings <- true;	// Initialize with the values above? (Allows unit tests to ignore instantiation)
	bool initialize_Infectious <- true; // Start with nb_inf_init infectious adults? 
	
	// Variables to handle multiple geographic patches
	float cross_prob <- 0.005;			// Probability of infectious in one patch making contact with a host in another patch
	int model_number <- 0;				// Identifier for the specific simulation (i.e. sub-population)
	list<int> all_sims <- [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]; // List of all sub-populations
	list<float> sim_sample_prob <- [0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1]; // Probability of sampling other patch (sim)
	int n_cross_inf <- 0;				// Number of new infectious to add from contact with infectious in other patches (starts at 0)
	
	// Infectious property variables
	list<int> dur_expose <- [3, 4, 5];	// Duration of pre-infectious period until infectious onset (days)
	list<int> dur_incub <-  [1, 2, 3];	// Duration of incubation period until symptom onset (added days to latent)
	list<int> dur_infect <- [6, 7, 8];	// Duration of infectiousness (days)
	list<int> dur_prehosp <-[3, 4, 5];  // Duration of time from symptom onset to hospitalization (days)
	float prob_community_wkdy <- 0.4; 	// Probability of going to community location during weekday afternoon
	float prob_community_wknd <- 0.46;  // Probability of going to community location during weekend afternoon
	float prob_nhgq_visit <- 0.001;		// Probability of visiting a NH or GQ location during afternoon
	float beta_HH <- 0.03;				// Probability of infection given contact in household
	float beta_COM <- 0.01;				// Probability of infection given contact in workplace/community
	
	// Probability that infection will result in hospitalization, by age (in years)
	list<float>hosp_prob <- [0.0021, 0.0021, 0.0021, 0.0021, 0.0021, 0.0021, 0.0021, 0.0021, 0.0021, 0.0021, 
							 0.0021, 0.0021, 0.0021, 0.0021, 0.0021, 0.0021, 0.0021, 0.0021, 0.0021, 0.0021,
							 0.0176, 0.0176, 0.0176, 0.0176, 0.0176, 0.0176, 0.0176, 0.0176, 0.0176, 0.0176, 
							 0.0176, 0.0176, 0.0176, 0.0176, 0.0176, 0.0176, 0.0176, 0.0176, 0.0176, 0.0176, 
							 0.0176, 0.0176, 0.0176, 0.0176, 0.0176, 0.0248, 0.0248, 0.0248, 0.0248, 0.0248, 
							 0.0248, 0.0248, 0.0248, 0.0248, 0.0248, 0.0253, 0.0253, 0.0253, 0.0253, 0.0253, 
							 0.0253, 0.0253, 0.0253, 0.0253, 0.0253, 0.0361, 0.0361, 0.0361, 0.0361, 0.0361, 
							 0.0361, 0.0361, 0.0361, 0.0361, 0.0361, 0.0446, 0.0446, 0.0446, 0.0446, 0.0446, 
							 0.0446, 0.0446, 0.0446, 0.0446, 0.0446, 0.0508, 0.0508, 0.0508, 0.0508, 0.0508, 
							 0.0508, 0.0508, 0.0508, 0.0508, 0.0508, 0.0508, 0.0508, 0.0508, 0.0508, 0.0508];
	
	// Tracking variables: Daily number of susceptible/exposed/infectious persons by host category
	list<int> sus_tod_list <- list_with(max_days+1, 0);
	list<int> sus_chi_list <- list_with(max_days+1, 0);
	list<int> sus_adu_list <- list_with(max_days+1, 0);
	list<int> sus_sen_list <- list_with(max_days+1, 0);
	list<int> sus_nh_list <- list_with(max_days+1, 0);
	list<int> sus_gq_list <- list_with(max_days+1, 0);
	
	list<int> exp_tod_list <- list_with(max_days+1, 0);
	list<int> exp_chi_list <- list_with(max_days+1, 0);
	list<int> exp_adu_list <- list_with(max_days+1, 0);
	list<int> exp_sen_list <- list_with(max_days+1, 0);
	list<int> exp_nh_list <- list_with(max_days+1, 0);
	list<int> exp_gq_list <- list_with(max_days+1, 0);
	
	list<int> inf_tod_list <- list_with(max_days+1, 0);
	list<int> inf_chi_list <- list_with(max_days+1, 0);
	list<int> inf_adu_list <- list_with(max_days+1, 0);
	list<int> inf_sen_list <- list_with(max_days+1, 0);
	list<int> inf_nh_list <- list_with(max_days+1, 0);
	list<int> inf_gq_list <- list_with(max_days+1, 0);
	
	// Tracking variables: total number of hospitalizations per day and total hospitalizations by age 
	list<int> n_hosp_list <- list_with(max_days+1, 0); 		 
	list<int> hosp_age_list <- list_with(100, 0);
		
	// Day and time variables	
	string daypart <- "evening";
	string weekday <- "Su";
	int day <- 0;
	
	// Initialize model
	init {
		if initialize_Settings = true {
			// Create settings (locations where agents can be)
			create Home number: nb_home_init;
			create School number: nb_school_init;
			create Workplace number: nb_work_init;
			create Community number: nb_comm_init;
			create NH number: nb_nh_init;
			create GQ number: nb_gq_init;
		}

		if initialize_Infectious = true {
			// Create infectious host(s)
			create Adult_Master number: nb_inf_init with: [sus::false, inf::true]; 
		}
				
		if initialize_csv = true {
			create Toddler_Master from: csv_file("../includes/sim_Toddler_50k_" + model_number + ".csv", true) 
						with: [sus::true, indexHome::int(get("indexHome")), 
							ageyrs::int(get("ageyrs")), male::bool(get("male"))];
			create Child_Master from: csv_file("../includes/sim_Child_50k_" + model_number + ".csv", true) 
						with: [sus::true, indexHome::int(get("indexHome")), ageyrs::int(get("ageyrs")),  
								male::bool(get("male")), indexSchool::int(get("indexSchool"))];
			create Adult_Master from: csv_file("../includes/sim_Adult_50k_" + model_number + ".csv", true)
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
		}				
	}

	/* Attempt to infect n_cross_inf number of hosts in this sub-population from other sub-populations. */
	/* Also update tracking variables. 																	*/	
	action update_counts {
		loop times: n_cross_inf {
			ask one_of(agents of_generic_species Host_Master){
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
		sus_chi_list[day] <- Child_Master count (each.sus);
		sus_adu_list[day] <- Adult_Master count (each.sus);
		sus_sen_list[day] <- Senior_Master count (each.sus);
		sus_nh_list[day] <- NHresident_Master count (each.sus);
		sus_gq_list[day] <- GQresident_Master count (each.sus);
		
		exp_tod_list[day] <- Toddler_Master count (each.exp);
		exp_chi_list[day] <- Child_Master count (each.exp);
		exp_adu_list[day] <- Adult_Master count (each.exp);
		exp_sen_list[day] <- Senior_Master count (each.exp);
		exp_nh_list[day] <- NHresident_Master count (each.exp);
		exp_gq_list[day] <- GQresident_Master count (each.exp);
		
		inf_tod_list[day] <- Toddler_Master count (each.inf);
		inf_chi_list[day] <- Child_Master count (each.inf);
		inf_adu_list[day] <- Adult_Master count (each.inf);
		inf_sen_list[day] <- Senior_Master count (each.inf);
		inf_nh_list[day] <- NHresident_Master count (each.inf);
		inf_gq_list[day] <- GQresident_Master count (each.inf);
	}
	
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
	
	// Output to console to track simulation progress
	action write_out {
		write daypart + " " + day + " " + weekday + " Model=" + model_number + " Infected=" + 
			(inf_tod_list[day-1] + inf_chi_list[day-1] + inf_adu_list[day-1] + inf_sen_list[day-1] + inf_nh_list[day-1] +
				inf_gq_list[day-1]);
	}
	
	// Simulation-wide updates
	// Note that this happens first, and then the individual agents' reflexes happen
	// If an experiment has a reflex, that happens before this global reflex
	reflex time_step {
		do update_counts;
		
		if daypart = "morning" {
			daypart <- "afternoon";
		} else if daypart = "afternoon" {
			daypart <- "evening";
		} else if daypart = "evening" {
			daypart <- "morning";
		}

		if daypart = "morning"{
			do update_day;
		}
		
		// Putting the write_out here means the console will display the status as of the beginning of the day
		// (i.e. before the individual agents take their actions)
		do write_out;
	}
	
	// Save the simulation tracking data at the end	
	reflex save_output when: day=max_days{
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
	    	    
	    do die;						
	}
}

/* Parent species for population members. All human hosts are subtypes ("children") of this class */
species Host_Master use_individual_shapes: false schedules:[] {
	int indexHome <- rnd(0, nb_home_init-1); // All hosts have a home
	init {
		self.location <- (Home at indexHome).location; // Start the simulation at home
	}
	
	// Demographics
	int ageyrs ;				// Placeholder for age in years; always specified by the specific host type
	bool male <- flip(0.5);
	
	// Infection status (SEIR, with additional flag for symptomatic)
	bool sus <- true;
	bool exp <- false;
	bool inf <- false;
	bool sym <- false;
	bool outside_sim <- false; // Marker for "travel" to a different sub-population to cause potential infection there
	
	// Counters for duration of time to stay in current infection state
	int counter_exp <- (exp? dur_expose[rnd_choice([0.2, 0.5, 0.3])]:0);		// Counter for latent period
	int counter_inf <- (inf? dur_infect[rnd_choice([0.25, 0.5, 0.25])]:0);  	// Counter for days infectious, 0 if not infectious
	int counter_sym <- (exp? counter_exp + dur_incub[rnd_choice([0.1, 0.7, 0.2])]:-1);	// Counter for days asymptomatic, 0 if not exposed
	int counter_hosp <- -1;
	
	// Number of infectious contacts at the current time step from household and community
	int infect_HH <- 0;
	int infect_COM <- 0;
	
	/* Increment infection counters daily */
	reflex increment_infection when: daypart="evening" and (exp or inf) {
		if exp {
			counter_exp <- counter_exp - 1;
		} else {
			counter_inf <- counter_inf - 1;
		}
	}
	
	/* Increment counter for onset of symptoms */
	reflex increment_sympt when: daypart="evening" and counter_sym > 0 {
		counter_sym <- counter_sym - 1;
	}
	
	/********* Actions for Infection and Recovery Dynamics ***************/
	// Count the number of infective contacts (neighbors)
	action count_exposures {
		if self.location = (Home at indexHome).location {
			infect_HH <- (Home at indexHome).nb_inf_hosts;
		} else {
			// Get the first (and should be only) Setting within 0 units of agent to get nb_inf_hosts
			infect_COM <- (agents_at_distance(neighbors_size) of_generic_species Setting)[0].nb_inf_hosts;
		}
	}
	
	// Infection process: if at the same location as >=1 infective, probably of infection is 1-(escape prob from all infectives)
	// Convert from susceptible to exposed and set counters for latent and incubation periods
	action make_exposed {
		float prob_infect <- 1 - (((1-beta_HH)^infect_HH) * ((1-beta_COM)^infect_COM));
		if flip(prob_infect){
			sus <- false;
			exp <- true;
			counter_exp <- dur_expose[rnd_choice([0.2, 0.5, 0.3])];
			counter_sym <- counter_exp + dur_incub[rnd_choice([0.2, 0.6, 0.2])];
		}
		
		// Reset number of contacts
		infect_HH <- 0;
		infect_COM <- 0;
	}
	
	// Convert from exposed (pre-infectious) to infectious and set counter to hosp entry if needed
	action make_infectious {
		if counter_exp = 0 {
			exp <- false;
			inf <- true;
			counter_inf <- dur_infect[rnd_choice([0.25, 0.5, 0.25])];
			if flip(hosp_prob[ageyrs]){
				counter_hosp <- dur_prehosp[rnd_choice([1/3, 1/3, 1/3])];
			}
		}
	}
	
	// Start symptomatic
	action make_symptomatic{
		sym <- true;
	}
	
	// While infected, random possibility of infecting someone from another sub-population
	// Once recovered, no longer contributing to dynamics and can be removed from simulation
	action make_recovered {
		if counter_inf = 0 {
			do die;
		} else {
			if flip(cross_prob){
				int my_target <- all_sims[rnd_choice(sim_sample_prob)];
				BaseModel_model[my_target].n_cross_inf <- BaseModel_model[my_target].n_cross_inf + 1;
				// Flag if infecting cross-population, so won't infect in this population this time step
				outside_sim <- true; 
			}
		}		
	}
	
	// Hospitalization process - update hospital trackers and then remove from simulation
	// (Assumes hospitalized no longer meaningfully contribute to community infection dynamics
	action make_hosp {
		if counter_hosp = 0 {
			n_hosp_list[day] <- n_hosp_list[day] + 1;
			hosp_age_list[ageyrs] <- hosp_age_list[ageyrs] + 1;
			do die;
		}			
		counter_hosp <- counter_hosp - 1;
	}
	
	/********* Actions for Movement ***************/
	// Morning default is no movement except for cross-infections, altered for Child and Adult agents
	action move_morning {
		if outside_sim = true {
			self.location <- point(-1, -1, 0);
			outside_sim <- false;
		}
	}
	
	// Afternoon default is to move to home or community or visit NH or GQ, 
	action move_afternoon {
		// Afternoon: some proportion go to random GQ or NH, some go to random community location, others go home;
		// Check for NH visit. If not, check for GQ visit (same prob). If not, check for community
		// Varies based on weekday vs. weekend
		
		// If infecting someone in another sub-population, set location outside the grid so can't also infect in this sub-pop
		if outside_sim = true {
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
	}
	
	// Evening default is to move to home, 	altered for NHresident and GQresident agents
	action move_evening {
		if outside_sim = true {
			self.location <- point(-1, -1, 0);
			outside_sim <- false;
		} else {
			self.location <- (Home at indexHome).location;
		}
	}
	
	
	/********* Reflex: make sure infection and movement events happen in the correct order */
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
		if daypart = "morning" {
			do move_morning;
		} else if daypart = "afternoon" {
			do move_afternoon;
		} else if daypart = "evening" {
			do move_evening;
		}
	}
}

/* TODDLER, a subset of Host ages 0-5 who is not assigned to a "school" (e.g. daycare, pre-school) */
species Toddler_Master parent: Host_Master {
	int ageyrs <- rnd(0, 5);	
}

/* CHILD, a subset of Host ages 0-17, has a school (can be pre-school or daycare for age <6 years) */
species Child_Master parent: Host_Master {
	int indexSchool <- rnd(0, nb_school_init-1);
	int ageyrs <- rnd(6, 17);
	
	/********* Child-specific movement processes ***********/
	action move_morning {
		if outside_sim = true {
			self.location <- point(-1, -1, 0);
			outside_sim <- false;
		} else if weekday in ["Mo", "Tu", "We", "Th", "Fr"]{
			// Morning: children go to school
			self.location <- (School at indexSchool).location;
		} 
	}
}

/* ADULT, a subset of Host, ages 18 - 74. Could have a workplace, which could be a school, NH, or GQ */
species Adult_Master parent: Host_Master {
	int indexWorkplace <- -1;
	int indexSchool <- -1;
	int indexNH <- -1;
	int indexGQ <- -1;
	int ageyrs <- rnd(18, 74);
	
	/********* Adult-specific movement processes ***********/
	action move_morning {
		if outside_sim = true {
			self.location <- point(-1, -1, 0);
			outside_sim <- false;
		} else if weekday in ["Mo", "Tu", "We", "Th", "Fr"]{
			if indexWorkplace >= 0 {
				self.location <- (Workplace at indexWorkplace).location;
			} else if indexSchool >= 0 { 
				self.location <- (School at indexSchool).location; 	// Teachers work at school
			} else if indexNH >= 0 {
				self.location <- (NH at indexNH).location;			// NH workers
			} else if indexGQ >= 0 {
				self.location <- (GQ at indexGQ).location;			// GQ workers
			}
		}
	}
}

/* SENIOR, as subset of Host, ages 75 - 99 (no workplace) */
species Senior_Master parent: Host_Master {
	int ageyrs <- rnd(75, 99);
}

/* NHresident, a subset of Host for those in nursing homes. Does not move */
species NHresident_Master parent: Host_Master {
	int ageyrs <- rnd(65, 99);
	int indexNH <- rnd(0, nb_nh_init-1);
	
	init{
		self.location <- (NH at indexNH).location;
	}
	
	action move_morning{
	}
	
	action move_afternoon{
	}
	
	action move_evening {
	}
}

/* GQresident, a subset of Host for those in group quarters. Does not move */
species GQresident_Master parent: Host_Master {
	int ageyrs <- rnd(18, 64);
	int indexGQ <- rnd(0, nb_gq_init-1);
	
	init{
		self.location <- (GQ at indexGQ).location;
	}
	
	action move_morning{
	}
	
	action move_afternoon{
	}
	
	action move_evening {
	}
}


/* Parent species (class) for geographic locations, including homes, schools, and so on. */
species Setting schedules:[] {
	int nb_inf_hosts <- 0;
	
	reflex count_infectives {
		nb_inf_hosts <- agents_at_distance(neighbors_size) of_generic_species Host_Master count (each.inf);
	}
} 

/* Home agent */
species Home parent: Setting {
}

/* School agent */
species School parent: Setting {
}

/* Workplace agent */
species Workplace parent: Setting {
}

/* Community agent */
species Community parent: Setting {
}

/* Nursing home agent */
species NH parent: Setting{
}

/* Group quarters agent */
species GQ parent: Setting{
}


/**** Scheduling species ******/
species timekeeper schedules:Host_Master+Setting{
}


/* Run the simulation in batch mode */
experiment SEIR_one type: batch repeat: 1 until: (day >= max_days) parallel: true {
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
