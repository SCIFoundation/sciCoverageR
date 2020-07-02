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
"hh"


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
"civ_equity_tool_file_2017"
