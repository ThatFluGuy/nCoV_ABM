/***
* Name: ComparetoObsereved
* Author: O992928
* Description: 
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
	int max_days <- 100;
	
	// Output directory
	string dir <- "H:/Scratch/GAMAout/MPE0KG/";
	
	float cross_prob <- 0.0;			// Set to zero to avoid out-of-patch movement
	float prob_nhgq_visit <- 0.0;		// Set to zero to focus on home/work/community
	float prob_community_wkdy <- 1.0; 	// Probability of going to community location during weekday afternoon
	float prob_community_wknd <- 1.0; 	// Probability of going to community location during weekend afternoon
	
	
	// Social distancing variables
	// Set dates of interventions and initialize percent decline variables
	// "Percent open" variables update in the UpdateDay reflex
	// Using mock values now for easier debugging
	float comm_open_pct <- 1.0;				// Percent community site visits occurring during specified time period
	float work_open_pct <- 1.0;				// Percent of work occuring during specified time period
	int school_close_day <- 10; 				// Close schools on day 15 of the simulation (March 12)
	list<int> close_days <- [5, 10, 15, 20];	// Simulation days when interventions change
	list<float> close_pcts <- [0.25, 0.5, 0.75, 1.0];	// Percent reductions in contacts at different intervention periods

	list<int> all_sims <- [0]; // List of all sub-populations
	list<float> sim_sample_prob <- [1.0]; // Probability of sampling other patch (sim)
	
	
	// Initialize model, specify the number of infectious and susceptible hosts
	init {		
		// Create settings (geographies where agents can be)
		create Home number: 1;
		create School number: 1;
		create Workplace number: 1;
		create Community number: 1;

		create Adult number: 1 with:[sus::true, inf::false, indexHome::0, indexWorkplace::0,
			indexSchool::-1, indexNH::-1, indexGQ::-1];
					
	}

	// Modify the update_counts action to track Toddler not Toddler_Master etc
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
		sus_adu_list[day] <- Adult count (each.sus);
		exp_adu_list[day] <- Adult count (each.exp);
		inf_adu_list[day] <- Adult count (each.inf);
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
				write "Going to work";
			} else if indexSchool >= 0 {
				if day < close_days[0]{ 
					self.location <- (School at indexSchool).location; 	// Teachers work at school
					write "Going to School";
				}
			} else if indexNH >= 0 {
				self.location <- (NH at indexNH).location;			// NH workers
				write "Going to NH";
			} else if indexGQ >= 0 {
				self.location <- (GQ at indexGQ).location;			// GQ workers
				write "Going to GQ";
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
				write "Going to NH";
			} else if flip(prob_nhgq_visit) {
				self.location <- one_of(GQ).location;
				write "Going to GQ";
			} else if flip(prob_community_wkdy) and weekday in ["Mo", "Tu", "We", "Th", "Fr"] {
				self.location <- one_of(Community).location;
				write "Going to Community";
			} else if flip(prob_community_wknd) and weekday in ["Sa", "Su"]{
				self.location <- one_of(Community).location;
				write "Going to Community";
			} else {
				self.location <- (Home at indexHome).location;
				write "Going to Home";
			}
		} else {
			// Assume NH and GQ are closed to visitors after the intervention starts
			if outside_sim = true {
				self.location <- point(-1, -1, 0);
			} else if flip(prob_community_wkdy * comm_open_pct) and weekday in ["Mo", "Tu", "We", "Th", "Fr"] {
				self.location <- one_of(Community).location;
				write "Going to Community";
			} else if flip(prob_community_wknd * comm_open_pct) and weekday in ["Sa", "Su"] {
				self.location <- one_of(Community).location;
				write "Going to Community";
			} else {
				self.location <- (Home at indexHome).location;
				write "Going to Home";
			}
		}
	}
	
	action move_evening {
		if outside_sim = true {
			self.location <- point(-1, -1, 0);
			outside_sim <- false;
		} else {
			self.location <- (Home at indexHome).location;
			write "Going to Home";
		}
	}
}



/* Run the simulation in batch mode */
experiment TEST_SocDist type: gui repeat: 1 until: (day >= max_days) {
	// Creates a single Adult agent. Should move between home/work/community (weekdays) and home/community (weekend)
	// until close_days[0]. Then prob of movement down by 25%, then 50%, etc
	parameter "Initialize 1" var: initialize_Settings init:false;
	parameter "Initialize 2" var: initialize_Infectious init:false;
	parameter "Initialze 3" var: use_csv init:false;
	
	
	float seedValue <- rnd(1.0, 10000.0);
	float seed <- seedValue;
}

