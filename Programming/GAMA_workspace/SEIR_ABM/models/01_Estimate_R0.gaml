/***
* Name: EstimateR0
* Author: Michael L. Jackson
* Description: Empirically estimate basic reproductive number
***/

model EstimateR0

/* First program in the sequence of evaluating the impact of different COVID-19 control measures. */
/* This program empirically estimates the basic reproductive number by introducing a single 	  */
/* infectious person into the population and observing how many secondary cases result. Program   */
/* will be run repeatedly to estimate across different starting age groups and other traits etc.  */
/* Note that this simulation models the full population of ~520,000 by using separate sub-pops    */
/* of ~52,000 agents. In the full model, infectious agents can infect agents across sub-pops. In  */
/* this program, since cross-pop infection is rare (Prob 0.5% per step), ignore in order to use   */
/* each sub-population as a separate susceptible population for estimating Ro.					  */

/* In order to cleanly count number of infections, use "Toddler" instead of "Toddler_Master", 	  */
/* "Child" instead of "Child_Master" etc, where Toddler is a child of the class Toddler_Master.   */
/* In each of these new classes, set the "make_infectious" action to empty. This means that 	  */
/* infected persons stay in the exposed category, and the number of infections created in each    */
/* simulation is just the sum of the number of persons in the exposed class after 9 days.		  */

import "../models/00_Base_Model.gaml"

/* Set up the global environment */
global {
	int max_days <- 9;
	
	string dir <- "H:/Scratch/GAMAout/MPE0KG/"; // Output directory
	
	float beta_HH  <- 0.016;			// Probability of infection given contact in household
	float beta_COM <- 0.008;			// Probability of infection given contact in workplace/community
	
	// Override initialization defaults
	bool initialize_Settings <- false;
	bool initialize_Infectious <- false;
	bool initialize_csv <- false;
	int nb_inf_init <- 1;
	
	// Initialize model, specify the number of infectious and susceptible hosts
	init {
		// Create settings (locations where agents can be)
		create Home number: nb_home_init;
		create School number: nb_school_init;
		create Workplace number: nb_work_init;
		create Community number: nb_comm_init;
		create NH number: nb_nh_init;
		create GQ number: nb_gq_init;
		
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
			
		// Create infectious host(s)
		loop times: nb_inf_init {
			ask one_of(agents of_generic_species(Host_Master)){
				self.sus <- false;
				self.inf <- true;
				self.counter_inf <- dur_infect[rnd_choice([0.25, 0.5, 0.25])];
			}
		}		
	}
	
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

/* R0 versions of hosts do not progress from exposed, so total infections can be counted */
species Toddler parent: Toddler_Master {
	action make_infectious{
	}
}

species Child parent: Child_Master {
	action make_infectious{
	}
}

species Adult parent: Adult_Master {
	action make_infectious{
	}
}

species Senior parent: Senior_Master {
	action make_infectious{
	}
}

species NHresident parent: NHresident_Master {
	action make_infectious{
	}
}

species GQresident parent: GQresident_Master {
	action make_infectious{
	}
}

/* Run the simulation in batch mode */
experiment SEIR_R0 type: batch repeat: 1 until: (day >= max_days) parallel: true {
	float seedValue <- rnd(1.0, 10000.0);
	float seed <- seedValue;
		
	init{
		create simulation with: [seed::seedValue + 1, model_number::1];
		create simulation with: [seed::seedValue + 2, model_number::2];
		create simulation with: [seed::seedValue + 3, model_number::3];
		create simulation with: [seed::seedValue + 4, model_number::4];
		create simulation with: [seed::seedValue + 5, model_number::5];
		create simulation with: [seed::seedValue + 6, model_number::6];
		create simulation with: [seed::seedValue + 7, model_number::7];
		create simulation with: [seed::seedValue + 8, model_number::8];
		create simulation with: [seed::seedValue + 9, model_number::9];
	}
}
