#' Individual data from a survey of SAC from 2261 households in the Ivory Coast.
#'
#' A dataset containing household and individual (ind) level information from a coverage survey in three health districts
#' in the Ivory Coast following the
#' \href{https://apps.who.int/iris/bitstream/handle/10665/329376/9789241516464-eng.pdf}{WHO CES Methodology}.
#'
#' @format A data frame with 4929 rows and 79 columns:
#' \describe{
#'   \item{admin_1_code}{t}
#'   \item{admin_1_name}{t}
#'   \item{admin_1_label}{t}
#'   \item{admin_2_code}{u}
#'   \item{admin_2_name}{u}
#'   \item{admin_2_label}{u}
#'   \item{moh_1_code}{u}
#'   \item{moh_1_name}{u}
#'   \item{moh_1_label}{u}
#'   \item{subunit_name}{u}
#'   \item{vil_segment_surveyed}{u}
#'   \item{vil_segment_total}{u}
#'   \item{hh_house_number}{u}
#'   \item{hh_interviewed}{u}
#'   \item{hh_reason_no_interview}{u}
#'   \item{hh_consent}{u}
#'   \item{eligible}{u}
#'   \item{hh_sac_female}{u}
#'   \item{hh_sac_male}{u}
#'   \item{hh_eligible}{u}
#'   \item{hh_eligible_pop_child}{u}
#'   \item{no_interviews}{u}
#'   \item{electricity}{u}
#'   \item{television}{u}
#'   \item{refrigerator}{u}
#'   \item{canal}{u}
#'   \item{cooker}{u}
#'   \item{cd}{u}
#'   \item{bank_account}{u}
#'   \item{water}{u}
#'   \item{toilet}{u}
#'   \item{cooking_fuel}{u}
#'   \item{floor}{u}
#'   \item{KEY_hh}{u}
#'   \item{KEY_ind}{u}
#'   \item{hh_ind_visit_date}{u}
#'   \item{hh_ind_submission}{u}
#'   \item{ind_hh_code}{u}
#'   \item{ind_interviewer_init}{u}
#'   \item{ind_group}{u}
#'   \item{ind_consent}{u}
#'   \item{ind_interviewed}{u}
#'   \item{ind_reason_no_interview}{u}
#'   \item{ind_confidentially}{u}
#'   \item{ind_age}{u}
#'   \item{ind_sex}{u}
#'   \item{ind_child_attendance}{u}
#'   \item{ind_child_education_level}{u}
#'   \item{ind_child_school_type}{u}
#'   \item{ind_sens_bin}{u}
#'   \item{ind_sens_how}{u}
#'   \item{ind_sens_teacher}{u}
#'   \item{ind_sens_vill_meet}{u}
#'   \item{ind_sens_poster}{u}
#'   \item{ind_sens_health_pro}{u}
#'   \item{ind_sens_newspaper}{u}
#'   \item{ind_sens_radio}{u}
#'   \item{ind_sens_tv}{u}
#'   \item{ind_sens_town_crier}{u}
#'   \item{ind_sens_worship}{u}
#'   \item{ind_sens_banner}{u}
#'   \item{ind_sens_other}{u}
#'   \item{ind_knowledge}{u}
#'   \item{ind_know_name_sch}{u}
#'   \item{ind_know_pzq}{u}
#'   \item{ind_know_dose_pole}{u}
#'   \item{ind_know_none}{u}
#'   \item{ind_offered_pzq}{u}
#'   \item{ind_offered_pzq_bin}{u}
#'   \item{ind_swallow_pzq}{u}
#'   \item{ind_swallow_pzq_bin}{u}
#'   \item{ind_reason_no_pzq}{u}
#'   \item{ind_where_swallow_pzq}{u}
#'   \item{ind_how_pzq_taken}{u}
#'   \item{ind_distributor_present}{u}
#'   \item{ind_prior_food}{u}
#'   \item{ind_decision_to_take}{u}
#'   \item{ind_mda_knowledge}{u}
#'   \item{ind_dist_distrib_point}{u}
#' }
#' @source Anonymised and amended based on survey data from the SCI Foundation
"vignette_ind"

#' Village data from a survey of 90 subunits in 3 health districts in the Ivory Coast.
#'
#' A dataset containing village (vil) level information from a coverage survey in three health districts in the Ivory Coast
#' following the \href{https://apps.who.int/iris/bitstream/handle/10665/329376/9789241516464-eng.pdf}{WHO CES Methodology}.
#'
#' @format A data frame with 90 rows and 32 columns:
#' \describe{
#'   \item{int_position}{Text}
#'   \item{vil_population}{Text}
#'   \item{vil_no_hhs}{Text}
#'   \item{vil_pop_source}{Text}
#'   \item{know_date}{Text}
#'   \item{community_how}{Text}
#'   \item{vil_segment_total}{Text}
#'   \item{KEY_vil}{Text}
#'   \item{vil_visit_date}{Text}
#'   \item{vil_submission}{Text}
#'   \item{vil_mda_type}{Text}
#'   \item{vil_date_mda}{Text}
#'   \item{vil_adults_treated}{Text}
#'   \item{vil_community_how}{Text}
#'   \item{vil_how_door_to_door_bin}{Text}
#'   \item{vil_how_village_head_bin}{Text}
#'   \item{vil_how_village_centre_bin}{Text}
#'   \item{vil_how_village_health_centre_bin}{Text}
#'   \item{vil_how_village_school_bin}{Text}
#'   \item{vil_how_other_bin}{Text}
#'   \item{vil_how_no_knowledge_bin}{Text}
#'   \item{vil_interviewer_init}{Text}
#'   \item{admin_2_name}{Text}
#'   \item{admin_1_name}{Text}
#'   \item{moh_1_name}{Text}
#'   \item{admin_2_code}{Text}
#'   \item{admin_1_code}{Text}
#'   \item{moh_1_code}{Text}
#'   \item{admin_1_label}{Text}
#'   \item{admin_2_label}{Text}
#'   \item{moh_1_label}{Text}
#'   \item{subunit_name}{Text}
#' }
#' @source Anonymised and amended based on survey data from the SCI Foundation.
"vignette_vil"


#' HH data from a survey of 2261 households in the Ivory Coast.
#'
#' A dataset containing Household (HH) level information from a coverage survey in three health districts in the Ivory Coast
#' following the \href{https://apps.who.int/iris/bitstream/handle/10665/329376/9789241516464-eng.pdf}{WHO CES Methodology}.
#'
#' @format A data frame with 2261 rows and 39 columns:
#' \describe{
#'   \item{KEY_hh}{Individual HH KEY, identifies them uniquely}
#'   \item{int_date}{Interview date, date, month, and year}
#'   \item{segment}{Subunit segment surveyed}
#'   \item{eligible}{u}
#'   \item{sac_boys}{u}
#'   \item{sac_girls}{u}
#'   \item{hh_eligible}{u}
#'   \item{no_interviews}{u}
#'   \item{electricity}{u}
#'   \item{television}{u}
#'   \item{refrigerator}{u}
#'   \item{canal}{u}
#'   \item{cooker}{u}
#'   \item{cd}{u}
#'   \item{bank_account}{u}
#'   \item{water}{u}
#'   \item{toilet}{u}
#'   \item{cooking_fuel}{u}
#'   \item{floor}{u}
#'   \item{hh_ind_visit_date}{u}
#'   \item{hh_ind_submission}{u}
#'   \item{hh_house_number}{u}
#'   \item{hh_interviewed}{u}
#'   \item{hh_reason_no_interview}{u}
#'   \item{hh_consent}{u}
#'   \item{hh_sac_male}{u}
#'   \item{hh_sac_female}{u}
#'   \item{hh_eligible_pop_child}{u}
#'   \item{admin_2_name}{u}
#'   \item{admin_2_label}{u}
#'   \item{admin_2_code}{u}
#'   \item{admin_1_name}{u}
#'   \item{admin_1_label}{u}
#'   \item{admin_1_code}{u}
#'   \item{moh_1_name}{u}
#'   \item{moh_1_label}{u}
#'   \item{moh_1_code}{u}
#'   \item{subunit_name}{u}
#' }
#' @source Anonymised and amended based on survey data from the SCI Foundation
"vignette_hh"


#' Ivory Coast Equity Tool Standard Questions.
#'
#' A dataset containing the questions, answer codes and answer scores to the
#' \href{https://www.equitytool.org/development}{Equity Tool Website} set of questions for the Ivory Coast.
#'
#' @format A data frame with 11 rows and 7 columns:
#' \describe{
#'   \item{question}{Simplified code on what the question is about (\href{https://www.equitytool.org/cote-divoire}{see} for more details)}
#'   \item{op1}{Answer Option 1, text}
#'   \item{op1_s}{Answer Option 1 score, numeric}
#'   \item{op2}{Answer Option 2, text}
#'   \item{op2_s}{Answer Option 2 score, numeric}
#'   \item{op3}{Answer Option 3, text (not always available)}
#'   \item{op3_s}{Answer Option 3 score, numeric (not always available)}
#' }
#' @source Simplified data frame based on the \href{https://www.equitytool.org/cote-divoire}{Equity Tool Website's}
#' form for the Ivory Coast. Requires requesting Excel Workbook to be sent. Based on the content in the "Other software" sheet.
"eq_answers"
