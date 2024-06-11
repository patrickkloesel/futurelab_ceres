READ ME PATENT PROJECT

00_project_functions.R contains all the functions that are used in the patent project. It draws most of the functions from the OECD global evidence paper, with some small changes to adapt it to the patent project. 

01_clean_and_controls.R gets the raw patent data and raw control data (gdp and population) as an input, transforms it in panel data and gives patents_panel_5techs_spread.csv as an output. 

02_run_model.R gets patents_panel_5techs_spread.csv as an input, runs the break detection model with some robustness checks and outputs 29_04_ihs_top22.RDS as the main model output. [Robustness checks models are saved as: 15_04_rob_brown.RDS (for brown patents control robustness check)].

03_policy_data_preprocessing.R takes CAPMF_2023_Policy.csv as an input, preprocesses it by coding introduction, phase-out, tightening, loosening from the OECD stringency index, including a specific filtering for RDD policy types. It is merged with existing additional policy data and is preprocessed to satisfy the needs for the policy attribution of the patent project (e.g. some instruments are unified across sectors). The output is: OECD_data_preprocessed_June_24.csv. 

04_policy_match.R takes OECD_data_preprocessed_June_24.csv (preprocessed policy data) and 29_04_ihs_top22.RDS (model output) as inputs, transform the model output and divides it into positive and negative breaks. It then matches breaks with policy data using the analytical confidence interval around each break. It gives 28_05_policy_out_neg.RDS and 28_05_policy_out_pos.RDS as an output.  

05_plot_policy_match.R takes the preprocessed policy data (OECD_data_preprocessed_June_24.csv) and the matched model-policy data 28_05_policy_out_neg.RDS and 28_05_policy_out_pos.RDS. It creates plots showing counterfactuals together with the policy attribution, saving them into png format. The output are: Ccmt_Energy_05_06_pos.png, Wind_Solar_Storage_05_06_pos.png, Ccmt_Energy_05_06_neg and Wind_Solar_Storage_05_06_neg for positive and negative breaks respectively. 

06_venn_diagrams.R takes the preprocessed policy data (OECD_data_preprocessed_June_24.csv) and the matched model-policy data for positive breaks '28_05_policy_out_pos.RDS'. It creates venn diagrams plots for each of the 5 technologies using effects from positive breaks of model output. It outputs ven_diagrams_09_06.png. 

07_effect_means.R takes the preprocessed policy data (OECD_data_preprocessed_June_24.csv) and the matched model-policy data for positive breaks '28_05_policy_out_pos.RDS'. It automatically counts the number of matched breaks by technology, differentiating by policy mix or single policy. It filters out for breaks resulting from low patent counts. It creates a barchart showing mean effect sizes for each policy type by technology class. It gives the figure mean_effect_size_bars_no_low_patents.png and the related dataframe mean_dfs_fig_4b_no_low_patents.csv as an output. 

08_wwcs_plot.R takes positive breaks model results 28_05_policy_out_pos.RDS and outputs a faceted barchart with the number of breaks by country (overview_breaks.png) and a map highlighting the treated countries (overview_map.png). Together with a second input which is the preprocessed OECD data (OECD_data_preprocessed_June_24.csv), it  creates a counterfactual-policy plot specific for the breaks of China (China_09_06_pos_big.png).