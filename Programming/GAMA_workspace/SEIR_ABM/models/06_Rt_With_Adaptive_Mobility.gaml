/***
* Name: 06RtWithAdaptiveMobility
* Author: O992928
* Description: 
* Tags: Tag1, Tag2, TagN
***/

model RtWithAdaptiveMobility

/* This program runs the SARS-CoV-2 simulation starting on August 24th, importing the population states */
/* created by 04_Observed_Through_August.gaml. The program modifies workplace and community social      */
/* distancing to try and maintain a specified effective reproductive number (Rt) for as long as 		*/
/* possible. It does this by calculating Rt on a daily basis and comparing to the target value.		    */
/* For simplicity, assumes schools are closed, but the same process could be run with schools open.	    */

import "../models/00_Base_Model.gaml"

/* Set up the global environment */
global {
	
	string read_dir <- "C:/Users/O992928/Desktop/GAMAin/";
	string dir <- "C:/Users/O992928/Desktop/GAMAout/";  	// Output directory
	//string dir <- "H:/Scratch/GAMAout/MPE0KG/";  			// Output directory
	//string dir <- "H:/Scratch/GAMAout/roc31v/";  			// Output directory
	
	int max_days <- 200;

	float Rt_target <- 1.25;
	float Rt_now;

	int sim_number <- 0;		 		// Import specific simulation state (range, 0-19)

	float beta_HH  <- 0.015;			// Probability of infection given contact in household
	float beta_COM <- 0.010;			// Probability of infection given contact in workplace/community, without mask use

	// Generation time interval probabilties based on latent and infectious periods
	// Goes from day 4 to day 13 pre-onset
	list<float> gentime_probs <- [0.0290, 0.101, 0.1443, 0.1443, 0.1443, 0.1443, 0.136, 0.1010, 0.0464, 0.0094];
	list<int> infected_daily <- list_with(13, 0);

	bool initialize_Settings <- false;
	bool initialize_Infectious <- false;
	bool initialize_csv <- false;
	
	// Social distancing variables
	// Set dates of interventions and initialize percent decline variables
	// Percent decline variables update in the UpdateDay reflex
	float work_open_pct <- 1 - 0.57;						// Percent community site visits occurring during specified time period
	float comm_open_pct <- 1 - 0.19;						// Percent of work occuring during specified time period
	
	float prob_nhgq_visit <- 0.0;	
	
	bool school_open <- false;						// Flag for whether school is open

	// Initialize model, specify the number of infectious and susceptible hosts
	// Use the host outputs as of August 25th as inputs for this model
	init {		
		// Create settings (geographies where agents can be)
		create Home number: nb_home_init;
		create School number: nb_school_init;
		create Workplace number: nb_work_init;
		create Community number: nb_comm_init;
		create NH number: nb_nh_init;
		create GQ number: nb_gq_init;
		
		// Create hosts
		create Toddler from: csv_file(read_dir + "Toddler_" + sim_number + "_" + model_number + ".csv", true)
			with: [name::string(get("name")), indexHome::int(get("indexHome")), ageyrs::int(get("ageyrs")), male::bool(get("male")),
				sus::bool(get("sus")), exp::bool(get("exp")), inf::bool(get("inf")), sym::bool(get("sym")),
				outside_sim::bool(get("outside_sim")), counter_exp::int(get("counter_ext")), counter_inf::int(get("counter_inf")), 
				counter_sym::int(get("counter_sym")), counter_hosp::int(get("counter_hosp"))];
		
		create Child from: csv_file(read_dir + "Child_" + sim_number + "_" + model_number + ".csv", true)
			with: [name::string(get("name")), indexHome::int(get("indexHome")), indexSchool::int(get("indexSchool")), 
				male::bool(get("male")), ageyrs::int(get("ageyrs")), sus::bool(get("sus")), exp::bool(get("exp")), 
				inf::bool(get("inf")), sym::bool(get("sym")), counter_exp::int(get("counter_ext")), 
				outside_sim::bool(get("outside_sim")), counter_inf::int(get("counter_inf")), 
				counter_sym::int(get("counter_sym")), counter_hosp::int(get("counter_hosp"))];
		
		create Adult from: csv_file(read_dir + "Adult_" + sim_number + "_" + model_number + ".csv", true)
			with: [name::string(get("name")), indexHome::int(get("indexHome")), indexSchool::int(get("indexSchool")), 
				indexWorkplace::int(get("indexWorkplace")), indexNH::int(get("indexNH")), indexGQ::int(get("indexGQ")),
				ageyrs::int(get("ageyrs")), male::bool(get("male")), sus::bool(get("sus")), exp::bool(get("exp")), 
				inf::bool(get("inf")), sym::bool(get("sym")), outside_sim::bool(get("outside_sim")), 
				counter_exp::int(get("counter_ext")), counter_inf::int(get("counter_inf")), counter_sym::int(get("counter_sym")),
				counter_hosp::int(get("counter_hosp"))];
		
		create Senior from: csv_file(read_dir + "Senior_" + sim_number + "_" + model_number + ".csv", true)
			with: [name::string(get("name")), indexHome::int(get("indexHome")), ageyrs::int(get("ageyrs")), 
				male::bool(get("male")), sus::bool(get("sus")), exp::bool(get("exp")), 
				inf::bool(get("inf")), sym::bool(get("sym")), outside_sim::bool(get("outside_sim")), 
				counter_exp::int(get("counter_ext")), counter_inf::int(get("counter_inf")), counter_sym::int(get("counter_sym")),
				counter_hosp::int(get("counter_hosp"))];
				
		create NHresident from: csv_file(read_dir + "NHresident_" + sim_number + "_" + model_number + ".csv", true)
			with: [name::string(get("name")), indexNH::int(get("indexNH")), ageyrs::int(get("ageyrs")), male::bool(get("male")),
				sus::bool(get("sus")), exp::bool(get("exp")), inf::bool(get("inf")), sym::bool(get("sym")),
				outside_sim::bool(get("outside_sim")), counter_exp::int(get("counter_ext")), counter_inf::int(get("counter_inf")), 
				counter_sym::int(get("counter_sym")), counter_hosp::int(get("counter_hosp"))];
				
		create GQresident from: csv_file(read_dir + "GQresident_" + sim_number + "_" + model_number + ".csv", true)
			with: [name::string(get("name")), indexGQ::int(get("indexGQ")), ageyrs::int(get("ageyrs")), male::bool(get("male")),
				sus::bool(get("sus")), exp::bool(get("exp")), inf::bool(get("inf")), sym::bool(get("sym")),
				outside_sim::bool(get("outside_sim")), counter_exp::int(get("counter_ext")), counter_inf::int(get("counter_inf")), 
				counter_sym::int(get("counter_sym")), counter_hosp::int(get("counter_hosp"))];

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
			
			// Infected during past 13 days
			if (day >= 13) {infected_daily[12] <- infected_daily[11];}
			if (day >= 12) {infected_daily[11] <- infected_daily[10];}
			if (day >= 11) {infected_daily[10] <- infected_daily[9];}
			if (day >= 10) {infected_daily[9] <- infected_daily[8];}
			if (day >= 9) {infected_daily[8] <- infected_daily[7];}
			if (day >= 8) {infected_daily[7] <- infected_daily[6];}
			if (day >= 7) {infected_daily[6] <- infected_daily[5];}
			if (day >= 6) {infected_daily[5] <- infected_daily[4];}
			if (day >= 5) {infected_daily[4] <- infected_daily[3];}
			if (day >= 4) {infected_daily[3] <- infected_daily[2];}
			if (day >= 3) {infected_daily[2] <- infected_daily[1];}
			if (day >= 2) {infected_daily[1] <- infected_daily[0];}
			if (day >= 1){
				infected_daily[0] <- (sus_tod_list[day-1] + sus_chi_list[day-1] + sus_adu_list[day-1] + sus_sen_list[day-1] +
					sus_nh_list[day-1] + sus_gq_list[day-1]) - (sus_tod_list[day] + sus_chi_list[day] + sus_adu_list[day] + 
					sus_sen_list[day] + sus_nh_list[day] + sus_gq_list[day]);
			}
		}
	}

	// Modify the update_day action to calculate probabilities of going to work or community based on closures
	action update_day {
		// Adaptively update work and community mobility to try and keep Rt at the target value
		// First calculate current Rt using the method of Cori et al (AJE 2013 178(9):1505-12) 
		if day >= 13 {
			Rt_now <- infected_daily[0] / 
				sum(infected_daily[3] * gentime_probs[0],
					infected_daily[4] * gentime_probs[1],
					infected_daily[5] * gentime_probs[2],
					infected_daily[6] * gentime_probs[3],
					infected_daily[7] * gentime_probs[4],
					infected_daily[8] * gentime_probs[5],
					infected_daily[9] * gentime_probs[6],
					infected_daily[10] * gentime_probs[7],
					infected_daily[11] * gentime_probs[8],
					infected_daily[12] * gentime_probs[9]);
			if Rt_now < Rt_target {
				comm_open_pct <- comm_open_pct + 0.01;
				work_open_pct <- work_open_pct + 0.01;
			} else if Rt_now > Rt_target {
				comm_open_pct <- comm_open_pct - 0.01;
				work_open_pct <- work_open_pct - 0.01;
			}
			comm_open_pct <- max(min(comm_open_pct, 1.0), 0.0);
			work_open_pct <- max(min(work_open_pct, 1.0), 0.0);
		}
		
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
	    
	    do die;						
	}
	
	// Output to console to track simulation progress
	action write_out {
		write daypart + " " + day + " Infected=" + 
			(inf_tod_list[day-1] + inf_chi_list[day-1] + inf_adu_list[day-1] + inf_sen_list[day-1] + inf_nh_list[day-1] +
				inf_gq_list[day-1]) + " Rt=" + Rt_now + " W=" + work_open_pct +  " C=" + comm_open_pct;
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
experiment Run_from_Aug type: batch repeat: 1 keep_seed: true until: (day >= max_days) parallel: true {
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


