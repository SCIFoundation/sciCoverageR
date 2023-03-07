#' Translate variables to french
#'
#' Function for translating a fixed set of variables to french
#'
#' @param dataset A dataframe containing variable to be translated
#' @param which Specifies which set of translations to match.
#' Individual questionnaire:
#' *`sens_how` for how they heard about MDA,
#' *`treatment_info` for what treatment info received,
#' *`resist`,`benefits`,`symptoms`,`prevent` for what negative rumours/benefits/symptoms/prevention they were aware of
#' *`recognise` for recognition of drugs and terms,
#' *`reason_no` for reason not swallowed,
#' *`where_swallow` for where swallowed,
#' *`decision` for who decided,
#' *`how_taken` for how were drugs swallowed,
#' *`yesno` for binary yes/no questions,
#' *`gender` for male/female questions,
#' *`school` for education level and school type questions.
#' Village questionnaire:
#' *`vil_mda_type` for mda delivery platform,
#' *`vil_community_how` for community-wide delivery platform.
#' @param var The variable from dataframe to be translated
#'
#' @return An object of the same type as `dataset`, with additional column created named `var_fr` of the french translations.
#' @export
#'
#' @examples dataset %>% tofrench("yesno",ind_child_attendance)
#' will return `dataset` object with a new column named ind_child_attendance_fr, with values `Oui` and `Non`.
tofrench <- function(dataset,which=c("sens_how","treatment_info","resist","benefits","symptoms","prevent", "recognise",
                                     "reason_no","where_swallow","decision","how_taken",
                                     "vil_mda_type","vil_community_how",
                                     "yesno", "gender", "school"),
                     var){

  if(which=="sens_how"){
    dataset %>%
      mutate("{{var}}_fr" := forcats::fct_inorder(case_when(grepl("teacher", {{var}}, ignore.case = T) ~"Enseignant",
                                                            grepl("village.*meeting", {{var}}, ignore.case = T) ~"Réunion du village",
                                                            grepl("posters", {{var}}, ignore.case = T) ~"Posters",
                                                            grepl("health.*professional", {{var}}, ignore.case = T) ~"Professionnel de la santé",
                                                            grepl("newspaper", {{var}}, ignore.case = T) ~"Journal",
                                                            grepl("radio", {{var}}, ignore.case = T) ~"Radio",
                                                            grepl("TV", {{var}}, ignore.case = T) ~"Tv",
                                                            grepl("town.*crier", {{var}}, ignore.case = T) ~"Crieur public",
                                                            grepl("worship", {{var}}, ignore.case = T) ~"Lieu de culte",
                                                            grepl("banner", {{var}}, ignore.case = T) ~"Bannière",
                                                            grepl("funeral", {{var}}, ignore.case = T) ~"Funérailles",
                                                            grepl("children", {{var}}, ignore.case = T) ~"De la part des enfants eux-mêmes",
                                                            grepl("parents", {{var}}, ignore.case = T) ~"Par les parents ou les proches adultes",
                                                            grepl("friends", {{var}}, ignore.case = T) ~"Par des amis",
                                                            grepl("other", {{var}}, ignore.case = T) ~"Autre",
                                                            TRUE ~ as.character({{var}}))))  }
  else if(which=="treatment_info"){
    dataset %>%
      mutate("{{var}}_fr" := forcats::fct_inorder(case_when(grepl("date", {{var}}, ignore.case = T) ~"Date de la distribution",
                                                            grepl("place", {{var}}, ignore.case = T) ~"Lieu de distribution",
                                                            grepl("diseases", {{var}}, ignore.case = T) ~"Les maladies que ces comprimés peuvent traiter",
                                                            grepl("side.*effects", {{var}}, ignore.case = T) ~"Effets secondaires",
                                                            grepl("food.*prior", {{var}}, ignore.case = T) ~"Nécessité de manger avant de prendre les comprimés",
                                                            grepl("eligibility", {{var}}, ignore.case = T) ~"Eligibilité",
                                                            grepl("health", {{var}}, ignore.case = T) ~"Santé",
                                                            TRUE ~ as.character({{var}}))))
  }

  else if(which=="resist"){
    dataset %>%
      mutate("{{var}}_fr" := forcats::fct_inorder(case_when(grepl("death", {{var}}, ignore.case = T) ~"La mort",
                                                            grepl("infertility", {{var}}, ignore.case = T) ~"Infertilité",
                                                            grepl("nausea", {{var}}, ignore.case = T) ~"Nausées",
                                                            grepl("dizziness", {{var}}, ignore.case = T) ~"Vertiges",
                                                            grepl("mental", {{var}}, ignore.case = T) ~"Handicap mental",
                                                            grepl("physical", {{var}}, ignore.case = T) ~"Handicap physique",
                                                            grepl("other", {{var}}, ignore.case = T) ~"Autre",
                                                            TRUE ~ as.character({{var}}))))
  }

  else if(which=="benefits"){
    dataset %>%
      mutate("{{var}}_fr" := forcats::fct_inorder(case_when(grepl("cure", {{var}}, ignore.case = T) ~"Guérir la SCH",
                                                            grepl("fertility", {{var}}, ignore.case = T) ~"Prévenir l'infertilité",
                                                            grepl("cancer", {{var}}, ignore.case = T) ~"Prévenir le cancer",
                                                            grepl("sch.*performance", {{var}}, ignore.case = T) ~"Améliorer les performances scolaires",
                                                            grepl("healthier", {{var}}, ignore.case = T) ~"Devenir en meilleure santé",
                                                            grepl("other", {{var}}, ignore.case = T) ~"Autre",
                                                            TRUE ~ as.character({{var}}))))
  }

  else if(which=="symptoms"){
    dataset %>%
      mutate("{{var}}_fr" := forcats::fct_inorder(case_when(grepl("no.*details", {{var}}, ignore.case = T) ~"Ne peut pas fournir de détails",
                                                            grepl("blood.*urine", {{var}}, ignore.case = T) ~"Sang dans les urines",
                                                            grepl("blood.*stool", {{var}}, ignore.case = T) ~"Sang dans les selles",
                                                            grepl("abd.*pain", {{var}}, ignore.case = T) ~"Douleur dans l'abdomen",
                                                            grepl("urine.*pain", {{var}}, ignore.case = T) ~"Miction douloureuse",
                                                            grepl("freq.*urine", {{var}}, ignore.case = T) ~"Miction fréquente",
                                                            grepl("swollen.*stomach", {{var}}, ignore.case = T) ~"Gonflement du ventre",
                                                            grepl("nausea", {{var}}, ignore.case = T) ~"Nausées/vomissements",
                                                            grepl("other", {{var}}, ignore.case = T) ~"Autre",
                                                            TRUE ~ as.character({{var}}))))
  }

  else if(which=="prevent"){
    dataset %>%
      mutate("{{var}}_fr" := forcats::fct_inorder(case_when(grepl("no.*details", {{var}}, ignore.case = T) ~"Ne peut pas fournir de détails",
                                                            grepl("tablets", {{var}}, ignore.case = T) ~"Prise de comprimés",
                                                            grepl("doctor", {{var}}, ignore.case = T) ~"Visite chez le médecin/à l'hôpital/au centre de santé",
                                                            grepl("toilets", {{var}}, ignore.case = T) ~"Utilisation des toilettes/latrines",
                                                            grepl("avoid.*contaminated.*water", {{var}}, ignore.case = T) ~"Éviter tout contact avec de l'eau contaminée ou stagnante",
                                                            grepl("well", {{var}}, ignore.case = T) ~"Utiliser un puits ou une pompe à eau",
                                                            grepl("treat.*water", {{var}}, ignore.case = T) ~"Traiter l'eau avant utilisation/consommation",
                                                            grepl("other", {{var}}, ignore.case = T) ~"Autre",
                                                            TRUE ~ as.character({{var}}))))
  }

  else if(which=="recognise"){
    dataset %>%
      mutate("{{var}}_fr" := forcats::fct_inorder(case_when(grepl("treats.*sch", {{var}}, ignore.case = T) ~"Traitement contre la schistosomiase",
                                                            grepl("treats.*sth", {{var}}, ignore.case = T) ~"Traitement de la géohelminthiase",
                                                            grepl("measure.*dose", {{var}}, ignore.case = T) ~"Mesure de la dose de PZQ",
                                                            grepl("other", {{var}}, ignore.case = T) ~"Autre",
                                                            TRUE ~ as.character({{var}}))))
  }

  else if(which=="reason_no"){
    dataset %>%
      mutate("{{var}}_fr" := forcats::fct_inorder(case_when(grepl("too.*young", {{var}}, ignore.case = T) ~"Trop jeune",
                                                            grepl("too.*old", {{var}}, ignore.case = T) ~"Trop âgé",
                                                            grepl("pregnant", {{var}}, ignore.case = T) ~"Enceinte",
                                                            grepl("breast.*feeding", {{var}}, ignore.case = T) ~"Allaitement",
                                                            grepl("too.*sick", {{var}}, ignore.case = T) ~"Trop malade",
                                                            grepl("feels.*healthy", {{var}}, ignore.case = T) ~"Se sent en bonne santé",
                                                            grepl("fear.*side.*effects", {{var}}, ignore.case = T) ~"Effets secondaires de la peur",
                                                            grepl("bad.*smell.*taste", {{var}}, ignore.case = T) ~"Mauvaise odeur goût",
                                                            grepl("tablets.*too.*large", {{var}}, ignore.case = T) ~"Tablettes trop grandes",
                                                            grepl("rumours", {{var}}, ignore.case = T) ~"Rumeurs",
                                                            grepl("unknown", {{var}}, ignore.case = T) ~"Ne sait pas",
                                                            grepl("drugs.*ran.*out", {{var}}, ignore.case = T) ~"Pénurie de médicaments",
                                                            grepl("was.*at.*work", {{var}}, ignore.case = T) ~"Était au travail",
                                                            grepl("not.*living.*in.*village", {{var}}, ignore.case = T) ~"Ne vivant pas dans le village",
                                                            grepl("absent.*from.*school", {{var}}, ignore.case = T) ~"Absent de l'école",
                                                            grepl("non.*attending", {{var}}, ignore.case = T) ~"Ne fréquente pas l’école",
                                                            grepl("no.*MDA", {{var}}, ignore.case = T) ~"Pas de DMM",
                                                            grepl("unaware", {{var}}, ignore.case = T) ~"Inconscient",
                                                            grepl("too.*far", {{var}}, ignore.case = T) ~"Trop loin",
                                                            grepl("refused.*answer", {{var}}, ignore.case = T) ~"Réponse refusée",
                                                            grepl("not.*invited", {{var}}, ignore.case = T) ~"Pas invité",
                                                            grepl("not.*eaten", {{var}}, ignore.case = T) ~"Pas mangé",
                                                            grepl("too.*many.*tablets", {{var}}, ignore.case = T) ~"Trop de tablettes",
                                                            grepl("medicine.*ineffective", {{var}}, ignore.case = T) ~"Médecine inefficace",
                                                            grepl("other", {{var}}, ignore.case = T) ~"Autre",
                                                            TRUE ~ as.character({{var}}))))

  }

  else if(which=="where_swallow"){
    dataset %>%
      mutate("{{var}}_fr" := forcats::fct_inorder(case_when(grepl("school", {{var}}, ignore.case = T) ~"École",
                                                            grepl("home", {{var}}, ignore.case = T) ~"Domicile",
                                                            grepl("village.*head.*house", {{var}}, ignore.case = T) ~"Siège du village",
                                                            grepl("central.*point", {{var}}, ignore.case = T) ~"Point central",
                                                            grepl("local.*health.*centre", {{var}}, ignore.case = T) ~"Le centre de santé local",
                                                            grepl("district.*clinic", {{var}}, ignore.case = T) ~"Clinique de district",
                                                            grepl("other", {{var}}, ignore.case = T) ~"Autre",
                                                            grepl("unknown", {{var}}, ignore.case = T) ~"Ne sait pas",
                                                            TRUE ~ as.character({{var}}))))

  }

  else if(which=="decision"){
    dataset %>%
      mutate("{{var}}_fr" := forcats::fct_inorder(case_when(grepl("me", {{var}}, ignore.case = T) ~"Moi",
                                                            grepl("father", {{var}}, ignore.case = T) ~"Père",
                                                            grepl("mother", {{var}}, ignore.case = T) ~"Mère",
                                                            grepl("other.*family", {{var}}, ignore.case = T) ~"Autre famille",
                                                            grepl("teacher", {{var}}, ignore.case = T) ~"Enseignant",
                                                            grepl("village.*head", {{var}}, ignore.case = T) ~"Chef de village",
                                                            grepl("trad.*healer", {{var}}, ignore.case = T) ~"Guérisseur traditionnel",
                                                            grepl("health.*professional", {{var}}, ignore.case = T) ~"Professionnel de la santé",
                                                            grepl("other", {{var}}, ignore.case = T) ~"Autre",
                                                            grepl("NA|did.*not.*know.*about.*the.*MDA", {{var}}, ignore.case = T) ~"Je ne connaissais pas le DMM",
                                                            TRUE ~ as.character({{var}}))))

  }

  else if(which=="how_taken"){

    dataset %>%
      mutate("{{var}}_fr" := forcats::fct_inorder(case_when(grepl("all.*together", {{var}}, ignore.case = T) ~"Tous ensemble",
                                                            grepl("all.*throughout.*day", {{var}}, ignore.case = T) ~"Tout au long de la journée",
                                                            grepl("all.*different.*days", {{var}}, ignore.case = T) ~"Tous les jours différents",
                                                            grepl("one.*per.*day", {{var}}, ignore.case = T) ~"Un jour par jour",
                                                            grepl("some.*not.*all", {{var}}, ignore.case = T) ~"Certains, pas tous",
                                                            grepl("received.*not.*swallowed", {{var}}, ignore.case = T) ~"Reçu, pas avalé",
                                                            grepl("unknown", {{var}}, ignore.case = T) ~"Ne pas se souvenir",
                                                            grepl("other", {{var}}, ignore.case = T) ~"Autre",
                                                            TRUE ~ as.character({{var}}))) )
  }

  else if(which=="vil_mda_type"){
    dataset %>%
      mutate("{{var}}_fr" := forcats::fct_inorder(case_when(grepl("school-based.*&.*community-wide|SBT_CWT", {{var}}, ignore.case = T) ~"À l'école et dans la communauté",
                                                            grepl("no.*MDA", {{var}}, ignore.case = T) ~"Pas de TDM",
                                                            grepl("^school-based$|SBT$", {{var}}, ignore.case = T) ~"Traitement à l'école",
                                                            grepl("^community-wide$|CWT$", {{var}}, ignore.case = T) ~"Traitement dans la communauté",
                                                            grepl("unknown", {{var}}, ignore.case = T) ~"Inconnu",
                                                            TRUE ~ as.character({{var}}))))

  }

  else if(which=="vil_community_how"){
    dataset %>%
      mutate("{{var}}_fr" := forcats::fct_inorder(case_when(grepl("door.*door", {{var}}, ignore.case = T) ~"Porte à porte",
                                                            grepl("village.*head.*house|villageheadhouse", {{var}}, ignore.case = T) ~"Maison du chef de village",
                                                            grepl("central.*point", {{var}}, ignore.case = T) ~"Point central",
                                                            grepl("local.*health.*centre", {{var}}, ignore.case = T) ~"Le centre de santé local",
                                                            grepl("school", {{var}}, ignore.case = T) ~"École",
                                                            grepl("other", {{var}}, ignore.case = T) ~"Autre",
                                                            TRUE ~ as.character({{var}}))) )
  }
  else if(which=="yesno"){
    dataset %>%
      mutate("{{var}}_fr" := forcats::fct_inorder(case_when(grepl("^yes$", {{var}}, ignore.case = T) ~"Oui",
                                                            grepl("\\bno\\b", {{var}}, ignore.case = T) ~"Non",
                                                            grepl("unknown", {{var}}, ignore.case = T) ~"Ne sait pas",
                                                            grepl("yes.*first", {{var}}, ignore.case = T) ~"Oui, première fois",
                                                            grepl("yes.*second", {{var}}, ignore.case = T) ~"Oui deuxième fois",
                                                            TRUE ~ as.character({{var}}))) )
  }
  else if(which=="gender"){
    dataset %>%
      mutate("{{var}}_fr" := forcats::fct_inorder(case_when(grepl("female|girl", {{var}}, ignore.case = T) ~"Femme",
                                                            grepl("male|boy", {{var}}, ignore.case = T) ~"Homme",
                                                            TRUE ~ as.character({{var}}))) )
  }
  else if(which=="school"){
    dataset %>%
      mutate("{{var}}_fr" := forcats::fct_inorder(case_when(grepl("primary", {{var}}, ignore.case = T) ~"École primaire",
                                                            grepl("secondary", {{var}}, ignore.case = T) ~"École secondaire",
                                                            grepl("public", {{var}}, ignore.case = T) ~"École publique",
                                                            grepl("private", {{var}}, ignore.case = T) ~"École privée",
                                                            grepl("religious", {{var}}, ignore.case = T) ~"École religieuse",
                                                            TRUE ~ as.character({{var}}))) )
  }

  else print("Please specify one of which to translate.")
}
