/***
* Name: Unit_Tests_Hosts
* Author: Michael L. Jackson
* Version date: 11 May 2020 
* Tags: Tag1, Tag2, TagN
***/

model UnitTestsHosts

/* This program runs unit tests on the Host agents of the Base Model. Test inputs are described and  */
/* provided in individual experiments, along with expected outputs. The global agent is streamlined  */
/* to focus on a single agent with customizable states. Removes cross-population infection from		 */
/* hosts as well. 																					 */

/* For the unit test to count number of contacts at each time step, modify the hosts to print their  */
/* number of contacts at each time step, before the counters get reset.								 */

import "../models/00_Base_Model.gaml"

global {
	// Only include the parts of the global agent that need updates for host unit testing	
	
	// Initialization variables
	int nb_home_init <- 1;				// Number of households
	int nb_inf_init <- 0;				// Starting number infectious
	int nb_school_init <- 1;			// Number of schools
	int nb_work_init <- 1;				// Number of workplaces
	int nb_comm_init <- 1; 				// Number of community locations; scaled to have community size events encounter ~ 20 people
	int nb_nh_init <- 1;				// Number of nursing homes
	int nb_gq_init <- 1;				// Number of group quarters
	
	// For unit testing, create variables allowing creation of different agents with different attributes
	int agent_type_init <- 1;			// Allow 1, 2, 3, 4, 5, 6 [for single agents], 7, 8 [for multi agents]
	bool sus_init <- true;
	bool exp_init <- false;
	bool inf_init <- false;
	bool print_event <- false;  
	
	// Variables to handle multiple geographic patches
	float cross_prob <- 0.0;			// Probability of infectious in one patch making contact with a host in another patch
	int model_number <- 0;				// Identifier for the specific simulation (i.e. sub-population)
	list<int> all_sims <- [0]; 			// List of all sub-populations

	// Infectious property variables
	list<int> dur_expose <- [3, 4, 5];	// Duration of pre-infectious period until infectious onset (days)
	list<int> dur_incub <-  [1, 2, 3];	// Duration of incubation period until symptom onset (added days to latent)
	list<int> dur_infect <- [6, 7, 8];	// Duration of infectiousness (days)
	list<int> dur_prehosp <-[3, 4, 5];  // Duration of time from symptom onset to hospitalization (days)
	float prob_community_wkdy <- 0.4; 	// Probability of going to community location during weekday afternoon
	float prob_community_wknd <- 0.46;  // Probability of going to community location during weekend afternoon
	float prob_nhgq_visit <- 0.001;		// Probability of visiting a NH or GQ location during afternoon
	float beta_HH <- 0.06;				// Probability of infection given contact in household
	float beta_COM <- 0.02;				// Probability of infection given contact in workplace/community
	float senior_COM <- 0.15;			// Added amount that seniors interact in community spaces
	
	
	// Initialize model
	init {
		create Home number: nb_home_init;
		create School number: nb_school_init;
		create Workplace number: nb_work_init;
		create Community number: nb_comm_init;
		create NH number: nb_nh_init;
		create GQ number: nb_gq_init;
		
		// Create agents per scenario.
		// 1 - 6 = a single agent (to test SEIR progression or movement functions)
		// 7 = multiple agents (to test calculation of infectious contacts)
		// 8 = two toddlers (to test infection process)
		if agent_type_init = 1 {
			create Toddler with:[sus::sus_init, exp::exp_init, inf::inf_init];
		} else if agent_type_init = 2 {
			create Child with:[sus::sus_init, exp::exp_init, inf::inf_init];
		} else if agent_type_init = 3 {
			create Adult with:[sus::sus_init, exp::exp_init, inf::inf_init, indexWorkplace::0];
		} else if agent_type_init = 4 {
			create Senior with:[sus::sus_init, exp::exp_init, inf::inf_init];
		} else if agent_type_init = 5 {
			create NHresident with:[sus::sus_init, exp::exp_init, inf::inf_init];
		} else if agent_type_init = 6 {
			create GQresident with:[sus::sus_init, exp::exp_init, inf::inf_init];
		} else if agent_type_init = 7 {
			create Toddler with:[sus::sus_init, exp::exp_init, inf::inf_init];
			create Child with:[sus::sus_init, exp::exp_init, inf::inf_init];
			create Adult with:[sus::sus_init, exp::exp_init, inf::inf_init, indexNH::0];
			create Senior with:[sus::sus_init, exp::exp_init, inf::inf_init];
			create NHresident with:[sus::sus_init, exp::exp_init, inf::inf_init];
		} else if agent_type_init = 8 {
			create Toddler with:[sus::false, exp::false, inf::true];
			create Toddler with:[sus::true, exp::false, inf::false];
		}
	}
	
	// Need to count number infected Toddler (not Toddler_Master) etc, so modify update_counts
	action update_counts {
		loop times: n_cross_inf {
			ask one_of(agents of_generic_species (Host_Master)){
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
		inf_tod_list[day] <- Toddler count (each.inf);
		inf_chi_list[day] <- Child count (each.inf);
		inf_adu_list[day] <- Adult count (each.inf);
		inf_sen_list[day] <- Senior count (each.inf);
		inf_nh_list[day] <- NHresident count (each.inf);
		inf_gq_list[day] <- GQresident count (each.inf);
	}
}

/* Create "child" incidences of the master versions that allow output of infectious contacts */

species Toddler parent: Toddler_Master{
	action count_exposures {
		if self.location = (Home at indexHome).location {
			infect_HH <- (Home at indexHome).nb_inf_hosts;
		} else {
			infect_COM <- (agents_at_distance(neighbors_size) of_generic_species Setting)[0].nb_inf_hosts;
		}
		if print_event = true {
			write "Toddler: HH = " + infect_HH + " COM = " + infect_COM;
		}
	}
}
species Child parent: Child_Master{
	action count_exposures {
		if self.location = (Home at indexHome).location {
			infect_HH <- (Home at indexHome).nb_inf_hosts;
		} else {
			infect_COM <- (agents_at_distance(neighbors_size) of_generic_species Setting)[0].nb_inf_hosts;
		}
		if print_event = true {
			write "Child: HH = " + infect_HH + " COM = " + infect_COM;
		}
	}
}
species Adult parent: Adult_Master{
	action count_exposures {
		if self.location = (Home at indexHome).location {
			infect_HH <- (Home at indexHome).nb_inf_hosts;
		} else {
			infect_COM <- (agents_at_distance(neighbors_size) of_generic_species Setting)[0].nb_inf_hosts;
		}
		if print_event = true {
			write "Adult: HH = " + infect_HH + " COM = " + infect_COM;
		}
	}	
}
species Senior parent: Senior_Master {
	action count_exposures {
		if self.location = (Home at indexHome).location {
			infect_HH <- (Home at indexHome).nb_inf_hosts;
		} else {
			infect_COM <- (agents_at_distance(neighbors_size) of_generic_species Setting)[0].nb_inf_hosts;
		}
		if print_event = true {
			write "Senior: HH = " + infect_HH + " COM = " + infect_COM;
		}
	}
}
species NHresident parent: NHresident_Master {
	action count_exposures {
		if self.location = (Home at indexHome).location {
			infect_HH <- (Home at indexHome).nb_inf_hosts;
		} else {
			infect_COM <- (agents_at_distance(neighbors_size) of_generic_species Setting)[0].nb_inf_hosts;
		}
		if print_event = true {
			write "NHresident: HH = " + infect_HH + " COM = " + infect_COM;
		}
	}
}
species GQresident parent: GQresident_Master {
	action count_exposures {
		if self.location = (Home at indexHome).location {
			infect_HH <- (Home at indexHome).nb_inf_hosts;
		} else {
			infect_COM <- (agents_at_distance(neighbors_size) of_generic_species Setting)[0].nb_inf_hosts;
		}
		if print_event = true {
			write "GQresident: HH = " + infect_HH + " COM = " + infect_COM;
		}
	}
}

experiment TEST_SEIR_flow type: gui {
	// For a single starting exposed, trace flow through exposed -> infectious -> symptomatic
	// Success = correct incrementing of counters and boolean variables in experiment output
	parameter "Max days" var: max_days init: 12;
	parameter "Agent type" var: agent_type_init init: 1;
	parameter "Initial suscep" var: sus_init init: false;
	parameter "Initial exposed" var: exp_init init: true;
	parameter "Initialize 1" var: initialize_Settings init: false;
	parameter "Initialize 2" var: initialize_Infectious init: false;
	parameter "Use external csv" var: initialize_csv init: false;
	
	reflex simout {
		Host_Master lone_host;
		if agent_type_init = 1 {
			lone_host <- Toddler[0];
		} else if agent_type_init = 2 {
			lone_host <- Child[0];
		} else if agent_type_init = 3 {
			lone_host <- Adult[0];
		} else if agent_type_init = 4 {
			lone_host <- Senior[0];
		} else if agent_type_init = 5 {
			lone_host <- NHresident[0];
		} else if agent_type_init = 6 {
			lone_host <- GQresident[0];
		}
		write daypart + " " + day + " Exposed=   " + lone_host.exp + " Latent days=    " + lone_host.counter_exp;
		write daypart + " " + day + " Infectious=" + lone_host.inf + " Infectious days=" + lone_host.counter_inf; 
		write daypart + " " + day + " Symptomat= " + lone_host.sym + " Incubation days=" + lone_host.counter_sym; 
		write daypart + " " + day + " Hospital=  " + lone_host.counter_hosp;
	}
}

experiment TEST_movement type: gui {
	// Try with each type of human agent on both weekday and weekend. Verify proper location in 
	// morning (home/school/work), afternoon (home/community), evening (home). Set community probabilities to 100%
	parameter "Max days" var: max_days init: 7;
	parameter "Agent type" var: agent_type_init init: 1;
	parameter "Use external csv" var: initialize_csv init: false;
	parameter "Initialize 1" var: initialize_Settings init: false;
	parameter "Initialize 2" var: initialize_Infectious init: false;
	parameter "Community weekday" var: prob_community_wkdy init: 1.0;
	parameter "Community weekend" var: prob_community_wknd init: 1.0;  
	parameter "Nursing home visit" var: prob_nhgq_visit init: 0.0;
	
	reflex simout {
		Host_Master lone_host;
		if agent_type_init = 1 {
			lone_host <- Toddler[0];
		} else if agent_type_init = 2 {
			lone_host <- Child[0];
		} else if agent_type_init = 3 {
			lone_host <- Adult[0];
		} else if agent_type_init = 4 {
			lone_host <- Senior[0];
		} else if agent_type_init = 5 {
			lone_host <- NHresident[0];
		} else if agent_type_init = 6 {
			lone_host <- GQresident[0];
		}
		write daypart + " " + day + " " + weekday;
		write "At home =      " + (lone_host.location = Home[0].location);
		write "At school =    " + (lone_host.location = School[0].location);
		write "At work =      " + (lone_host.location = Workplace[0].location);
		write "At community = " + (lone_host.location = Community[0].location);
		write "At NH =        " + (lone_host.location = NH[0].location);
		write "At GQ =        " + (lone_host.location = GQ[0].location);
		write "-----------";
	}
}

experiment TEST_contacts type: gui {
	// Verify that the correct number of infectious contacts are being identified for each person type at each
	// time step. Correct results are:
	// Morning, 2 infectious persons  for Toddler + Senior (at home), 2 for Adult + NHresident (in NH), 1 for Child (in school)
	// Afternoon, 4 for all (at community) except NHresident (should be 1)
	// Evening, 4 for all (at home) except NHresident (should be 1)
	parameter "Max days" var: max_days init: 7;
	parameter "Agent type" var: agent_type_init init: 7;
	parameter "Use external csv" var: initialize_csv init: false;
	parameter "Initialize 1" var: initialize_Settings init: false;
	parameter "Initialize 2" var: initialize_Infectious init: false;
	parameter "Community weekday" var: prob_community_wkdy init: 1.0;
	parameter "Community weekend" var: prob_community_wknd init: 1.0;  
	parameter "Nursing home visit" var: prob_nhgq_visit init: 0.0;
	// Make agents both susceptible (so that count_exposures is invoked) and infectious (to get counted by others)
	parameter "Initial suscep" var: sus_init init: true;
	parameter "Initial inf" var: inf_init init: true;
	// Set infection probabilities to zero to keep counting exposures
	parameter "Beta HH" var: beta_HH init: 0.0;
	parameter "Beta COM" var: beta_COM init: 0.0;
	// Allow printing of contacts
	parameter "Print contacts" var: print_event init: true;

}

experiment TEST_infections type: gui {
	// Verify that infection is working correctly
	// With 50% household probability of infection and shared household over 6-8 infection days,
	// the susceptible toddler should be infected in almost all simulation runs
	parameter "Max days" var: max_days init: 8;
	parameter "Agent type" var: agent_type_init init: 8;
	parameter "Use external csv" var: initialize_csv init: false;
	parameter "Initialize 1" var: initialize_Settings init: false;
	parameter "Initialize 2" var: initialize_Infectious init: false;
	parameter "Nursing home visit" var: prob_nhgq_visit init: 0.0;
	parameter "Initial suscep" var: sus_init init: true;
	parameter "Beta HH" var: beta_HH init: 0.5;
	parameter "Print contacts" var: print_event init: true;
	
}

experiment TEST_crossInfect type: gui {
	// Verify that cross-simulation infection is working correctly
	// With crossprob = 1.0, infection should spread from one simulation (geographic patch) to another rapidly
	parameter "Max days" var: max_days init: 10;
	parameter "Agent type" var: agent_type_init init: 3;
	parameter "Use external csv" var: initialize_csv init: false;
	parameter "Initialize 1" var: initialize_Settings init: false;
	parameter "Initialize 2" var: initialize_Infectious init: false;
	parameter "Cross-sim prob" var: cross_prob init: 1.0;
	parameter "List of sims" var: all_sims init: [0,1];
	parameter "Selection probs" var: sim_sample_prob init:[0.5, 0.5];
	parameter "Initial suscep" var: sus_init init: false;
	parameter "Initial infect" var: inf_init init: true;
	
	init{
		create simulation with: [model_number::1, max_days::10, agent_type_init::3, initialize_csv::false, 
			initialize_Settings::false, initialize_Infectious::false, cross_prob::1.0, all_sims::[0,1],
			sim_sample_prob::[0.5, 0.5], sus_init::true, inf_init::false];	
	} 	
	
	reflex simout {
		write "Simulation = " + model_number + " infectious = " + Adult[0].inf;
	}
	
}
