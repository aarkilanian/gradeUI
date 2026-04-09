parsed <- parse("C:/Users/aarki/OneDrive/Documents/R projects/autograde/solutions_grading/solution_script_1002.R")
parsed_list <- as.list(parsed)

sub_env <- new.env()
eval(parsed, envir = sub_env)
sol_list <- as.list(sub_env)

answers <- list(
  list(
    answer = sol_list[["moosedata"]],
    answer_call = parsed_list[[2]]
  ),
  list(
    answer = sol_list[["moose_clean"]],
    answer_call = parsed_list[[3]]
  ),
  list(
    answer = sol_list[["moose_sel"]],
    answer_call = parsed_list[[4]]
  ),
  list(
    answer = sol_list[["year_min"]],
    answer_call = parsed_list[[5]]
  ),
  list(
    answer = sol_list[["moose_max"]],
    answer_call = parsed_list[[6]]
  ),
  list(
    answer = sol_list[["moosedata2"]],
    answer_call = parsed_list[[7]]
  ),
  list(
    answer = NULL,
    answer_call = parsed_list[[8]]
  ),
  list(
    answer = sol_list[["moose_west"]],
    answer_call = parsed_list[[9]]
  ),
  list(
    answer = NULL,
    answer_call = parsed_list[[10]]
  ),
  list(
    answer = sol_list[["moose_2020"]],
    answer_call = parsed_list[[11]]
  ),
  list(
    answer = sol_list[["moose_2020_high"]],
    answer_call = parsed_list[[12]]
  ),
  list(
    answer = sol_list[["moose_2020_high_byD"]],
    answer_call = parsed_list[[13]]
  ),
  list(
    answer = sol_list[["moosefinal"]],
    answer_call = parsed_list[[14]]
  ),
  list(
    answer = sol_list[["saplings"]],
    answer_call = parsed_list[[15]]
  ),
  list(
    answer = sol_list[["sap_clean"]],
    answer_call = parsed_list[[16]]
  ),
  list(
    answer = sol_list[["sap_reg_browse"]],
    answer_call = parsed_list[[17]]
  ),
  list(
    answer = sol_list[["avg_browse_reg"]],
    answer_call = ""
  ),
  list(
    answer = sol_list[["sap_reg_height"]],
    answer_call = parsed_list[[19]]
  ),
  list(
    answer = sol_list[["sap_reg_height_low"]],
    answer_call = ""
  ),
  list(
    answer = sol_list[["sap_spe_browse"]],
    answer_call = parsed_list[[21]]
  ),
  list(
    answer = sol_list[["avg_browse_spe"]],
    answer_call = parsed_list[[22]]
  ),
  list(
    answer = sol_list[["fir_reg_browse"]],
    answer_call = parsed_list[[23]]
  ),
  list(
    answer = NULL,
    answer_call = parsed_list[[24]]
  ),
  list(
    answer = sol_list[["spruce_reg_browse"]],
    answer_call = parsed_list[[25]]
  ),
  list(
    answer = NULL,
    answer_call = parsed_list[[26]]
  ),
  list(
    answer = NULL,
    answer_call = "Balsam Fir browsing is more consistent across regions versus Black Spruce"
  ),
  list(
    answer = sol_list[["sap_reg_tally"]],
    answer_call = parsed_list[[27]]
  ),
  list(
    answer = sol_list[["sap_spe_tally"]],
    answer_call = parsed_list[[28]]
  ),
  list(
    answer = NULL,
    answer_call = "Black Ash is an underrepresented species. Maritime barrens and Strait of Belle Isle barrens are underrepresented ecoregions"
  ),
  list(
    answer = NULL,
    answer_call = "Important to recognize bias to provide context to patterns"
  ),
  list(
    answer = sol_list[["moose_2020b"]],
    answer_call = parsed_list[[29]]
  ),
  list(
    answer = sol_list[["moose_sap"]],
    answer_call = parsed_list[[30]]
  ),
  list(
    answer = sol_list[["sum_spe_browse"]],
    answer_call = parsed_list[[31]]
  ),
  list(
    answer = NULL,
    answer_call = "Yes, the figure supports specialist browsing at low density and generalist browsing at high density"
  ),
  list(
    answer = NULL,
    answer_call = "Most browsed species is Black Spruce. Least browsed species is Willow"
  ),
  list(
    answer = NULL,
    answer_call = "Black Ash is not present in the figure. Hard to find at study sites"
  ),
  list(
    answer = sol_list[["moose_coll"]],
    answer_call = parsed_list[[35]]
  ),
  list(
    answer = sol_list[["coll_merge_per_capita"]],
    answer_call = parsed_list[[36]]
  ),
  list(
    answer = NULL,
    answer_call = parsed_list[[37]]
  ),
  list(
    answer = NULL,
    answer_call = " Regions with less people see higher per capita collisions. Rural areas have more moose"
  )
)

saveRDS(answers, "data/1002_answers.rda")
