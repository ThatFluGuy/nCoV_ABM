/***
* Name: InterventionStartDate
* Author: Michael L. Jackson
* Description: Model to estimate the simulation day that corresponds to the start of the real-world intervention
***/

model InterventionStartDate

/* Second program in the sequence of evaluating the impact of different COVID-19 control measures. */
/* This program runs the first few weeks of the epidemic to identify the correct timing for the	   */
/* start of interventions, based on when cases cross a pre-specified threshold. Assume the model   */
/* starts with two infectious persons in each of three geographic patchs.						   */

import "../models/00_Base_Model.gaml"

/* Set up the global environment */
global {
	int max_days <- 21;
	
	string dir <- "H:/Scratch/GAMAout/MPE0KG/"; // Output directory
	
	float beta_HH <- 0.03;				// Probability of infection given contact in household
	float beta_COM <- 0.01;				// Probability of infection given contact in workplace/community
		
}


/* Run the simulation in batch mode */
experiment SEIR_StartDate type: batch repeat: 1 until: (day >= max_days) parallel: true {
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
