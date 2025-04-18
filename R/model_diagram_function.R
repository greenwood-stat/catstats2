#
# Author: Greta Linse
# Last revised: April 17, 2025
#
# Call this function using model_diagram(model_name) where "model_name" is an
# lme object created from the nlme package.
#

library(nlme)
library(tidyverse)
library(ggthemes)
library(car)
library(effects)
library(DiagrammeR)
library(DiagrammeRsvg)
library(forcats)

#------------function is below------------------#
#' Hierarchical model diagramming
#'
#' `model_diagram()` takes a hierarchical nested model and returns a DiagrammeR
#' object visualizing the fixed and random effects structure.
#'
#' @param this_model Input model. Either a lme or merMod object with a nested
#'    random effects structure.
#' @param this_file_path Optional. Path to a location to export the diagram.
#'    Default is `NULL`.
#' @param this_file_type Optional. File type to export the diagram.
#'    Default is `"PNG"`.
#' @param widthVal Optional. Width of diagram in pixels. Default is `800`.
#' @param heightVal Optional. Height of diagram in pixels. Default is `1600`.
#' @param includeSizes Optional. Include group sizes in random effect labels.
#'    Default is `TRUE`.
#' @param includeLabels Optional. Include labels for the model diagram components.
#'    Default is `TRUE`.
#' @param nodeColors Optional. Function specifying the colors ([md_color()])
#'    for the outline of the nodes. Components can be specified individually
#'    (`diagram`, `random`, and `fixed`).
#' @param nodeFillColors Optional. Function specifying the colors ([md_fill()])
#'    for the fill color of the nodes. Components can be specified individually
#'    (`diagram`, `random`, and `fixed`).
#' @param nodeFontColors Optional. Function specifying the colors ([md_fontColor()])
#'    for the font color of text in the nodes. Components can be specified
#'    individually (`diagram`, `random`, and `fixed`).
#'
#' @details
#' NOTE: When including this function in an RMD document and knitting to PDF, the
#' graphic size variables `widthVal` and `heightVal` do not currently work. It is
#' recommended that instead the image is exported to a file such as a PDF and then
#' reimported to the document. See the examples below.
#' @returns A DiagrammeR object.
#' @export
#'
#' @examples
#' library(tidyverse)
#' library(DiagrammeR)
#' # merMod object example
#' library(lme4)
#' library(nlme)
#'
#' sleepstudy_lmer <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
#' summary(sleepstudy_lmer)
#' model_diagram(sleepstudy_lmer)
#'
#' # lme object example
#'
#' sleepstudy_lme <- lme(Reaction ~ Days, random=~Days|Subject, data=sleepstudy)
#' summary(sleepstudy_lme)
#' model_diagram(sleepstudy_lme)
#'
#' # Knitting to PDF example - Don't run
#' \dontrun{
#' model_diagram(sleepstudy_lmer,
#'               this_file_path="sleepstudy_lmer_modeldiagram.PDF",
#'               this_file_type="PDF")
#' knitr::include_graphics("sleepstudy_lmer_modeldiagram.PDF")
#'               }
#'
model_diagram <- function(this_model, this_file_path = NULL, this_file_type = "PNG",
                          widthVal = 800, heightVal = 1600, includeSizes = TRUE,
                          includeLabels = TRUE,
                          nodeColors = md_color(diagram="gray25", random="gray25", fixed="gray25"),
                          nodeFillColors = md_fill(diagram="aliceblue", random="aliceblue", fixed="darkseagreen1"),
                          nodeFontColors = md_fontColor(diagram="black", random="black",fixed="black")
                          ){
  # if(is.null(nodeColors$diagram)){
  #   nodeColors$diagram <- "gray25"
  # }
  # if(is.null(nodeColors$random)){
  #   nodeColors$random <- "gray25"
  # }
  # if(is.null(nodeColors$fixed)){
  #   nodeColors$fixed <- "gray25"
  # }
  # if(is.null(nodeFillColors$diagram)){
  #   nodeFillColors$diagram <- "aliceblue"
  # }
  # if(is.null(nodeFillColors$random)){
  #   nodeFillColors$random <- "aliceblue"
  # }
  # if(is.null(nodeFillColors$fixed)){
  #   nodeFillColors$fixed <- "darkseagreen1"
  # }
  # if(is.null(nodeFontColors$diagram)){
  #   nodeFontColors$diagram <- "black"
  # }
  # if(is.null(nodeFontColors$random)){
  #   nodeFontColors$random <- "black"
  # }
  # if(is.null(nodeFontColors$fixed)){
  #   nodeFontColors$fixed <- "black"
  # }
  if(is(this_model,"merMod")){
    lmer_formula <- deparse1(formula(this_model), collapse=" ")
    lmer_formula_fixed <- deparse1(formula(this_model, fixed.only=TRUE), collapse=" ")
    lmer_formula_random <- substring(str_remove(lmer_formula, fixed(lmer_formula_fixed)),4)

    if(str_detect(lmer_formula_random,fixed("+"))){
      stop("Specification of random effects in multiple parts crossed or otherwise is not yet implemented.")
    }
    if(str_detect(lmer_formula_fixed, fixed("+ offset("))){
      lmer_formula_fixed_clean <- substring(lmer_formula_fixed,
                                            first=1,
                                            last=str_locate(lmer_formula_fixed, fixed("offset("))[1]-3)
    }  else if(str_detect(lmer_formula_fixed, fixed("cbind("))){
      numeratorResponse <- substring(lmer_formula_fixed,
                                     first=7,
                                     last = str_locate(lmer_formula_fixed,fixed(","))-1)[[1]]
      lmer_formula_fixed_clean <- paste(numeratorResponse, substring(lmer_formula_fixed,
                                                                     first=str_locate(lmer_formula_fixed, fixed(")"))+1,
                                                                     last=str_length(lmer_formula_fixed))[[1]])
    } else{
      lmer_formula_fixed_clean <- lmer_formula_fixed
    }
    if(str_count(lmer_formula_random, fixed(":"))>=1){
      lmer_formula_random_nested <- str_replace(lmer_formula_random,fixed(":"),"/")
      lmer_formula_random_clean <- str_remove(str_remove(lmer_formula_random_nested, fixed("(")),fixed(")"))
      lmer_formula_random_clean_asForm <- as.formula(paste0("~",parse(text=lmer_formula_random_clean)))
    } else {
      lmer_formula_random_clean <- str_remove(str_remove(lmer_formula_random, fixed("(")),fixed(")"))
      lmer_formula_random_clean_asForm <- as.formula(paste0("~",parse(text=lmer_formula_random_clean)))
    }

    lmer_model_data <- model.frame(this_model)
    if(str_detect(lmer_formula_fixed, fixed("cbind("))){
      as.data.frame(model.frame(this_model)[[1]])[1]
      lmer_model_data <- cbind(as.data.frame(model.frame(this_model)[[1]])[1],model.frame(this_model)[-1])
    }
    tryCatch(
      lme_model <- lme(eval(parse(text=lmer_formula_fixed_clean)),
                       random=lmer_formula_random_clean_asForm,
                       data=lmer_model_data),
      error = function(e){
        message("Could not convert the merMod object to an lme object.")
      })
    if(!exists("lme_model")){
      stop("Invalid specification of random effects. Please check that the random effects are specified in hierarchical notation.")
    }
  } else if(is(this_model, "lme")){
    lme_model <- this_model
  } else {
    stop("Please provide a linear mixed effects (lme) model with random effects obtained using the nlme package.",call. = FALSE)
  }

  ### Get information from provided model and data

  ## Random effects structure

  RElist <- names(lme_model$dims$qvec)[1:lme_model$dims$Q]

  numDelims <- max(str_count(lme_model$groups[,RElist[1]],fixed("/")))
  numRE <- lme_model$dims$Q

  theseGroups <- lme_model$groups %>%
    separate_wider_delim(!!RElist[1],
                         cols_remove=FALSE,
                         delim="/", names=paste0("RE",1:numRE)) %>%
    relocate(paste0("RE",1:numRE), .after=everything())

  if(numRE > 1){
    theseGroups_elipses <- theseGroups %>%
      mutate(onesCol = 1)
    reIdx <- numRE
    while(reIdx > 0){
      tempREName <- paste0("RE",reIdx)
      thisRE_levels <- unique(theseGroups_elipses[,tempREName][[1]])
      theseRElevels_numeric <- suppressWarnings(as.numeric(thisRE_levels))
      if(sum(as.numeric(!is.na(theseRElevels_numeric)))==length(thisRE_levels)){
        theseGroups_elipses <- theseGroups_elipses %>%
          mutate(thisLevel = fct_relevel(get(tempREName),
                                         as.character(sort(theseRElevels_numeric))),
                 numThisLvl = as.numeric(thisLevel)) %>%
          arrange(numThisLvl) %>%
          select(-c(thisLevel, numThisLvl))
      } else{
        theseGroups_elipses <- theseGroups_elipses %>%
          mutate(thisLevel = fct_relevel(get(tempREName),
                                         sort(thisRE_levels)),
                 numThisLvl = as.numeric(thisLevel)) %>%
          arrange(numThisLvl) %>%
          select(-c(thisLevel, numThisLvl))
      }

      reIdx <- reIdx - 1
    }
    theseGroups_elipses <- theseGroups_elipses %>%
      rowid_to_column(var="orderID") %>%
      group_by(get(RElist[1])) %>%
      rename(thisRE = `get(RElist[1])`) %>%
      mutate(cumSumCol = cumsum(onesCol),
             ObsLevel = case_when(cumSumCol==1 ~ "Obs. 1",
                                  cumSumCol==max(cumSumCol, na.rm=TRUE) ~
                                    paste("Obs.",cumSumCol),
                                  TRUE ~ "...")) %>%
      ungroup() %>%
      select(-c(onesCol, cumSumCol, thisRE))
  } else if(numRE==1){
    theseGroups_elipses <- theseGroups %>%
      mutate(onesCol = 1,
             firstLevel = fct_relevel(get(RElist[1]),
                                      sort(levels(get(RElist[1])))),
             numFirstLvl = as.numeric(firstLevel)) %>%
      arrange(numFirstLvl) %>%
      rowid_to_column(var="orderID") %>%
      select(-c(firstLevel,numFirstLvl)) %>%
      group_by(get(RElist[1])) %>%
      rename(thisRE = `get(RElist[1])`) %>%
      mutate(cumSumCol = cumsum(onesCol),
             ObsLevel = case_when(cumSumCol==1 ~ "Obs. 1",
                                  cumSumCol==max(cumSumCol, na.rm=TRUE) ~
                                    paste("Obs.",cumSumCol),
                                  TRUE ~ "...")) %>%
      ungroup() %>%
      select(-c(onesCol, cumSumCol, thisRE))
  } else{
    error("Model does not have any random effects.")
  }


  theseGroups_names <- names(theseGroups)
  RE_gen_names <- paste0("RE",1:numRE)


  for(i in 1:numRE){
    thisColName <- paste0("RE",i)
    prevColName <- names(theseGroups_elipses)[which(names(theseGroups_elipses)==thisColName)-1]

    names(theseGroups_elipses)[which(names(theseGroups_elipses)==prevColName)] <- "prevRE"
    if(i == 1){
      thisRE_levels <- unique(theseGroups_elipses[,thisColName][[1]])
      theseRElevels_numeric <- suppressWarnings(as.numeric(thisRE_levels))
      if(sum(as.numeric(!is.na(theseRElevels_numeric)))==length(thisRE_levels)){
        maxGrpNum <- length(theseRElevels_numeric)
        firstGrpName <- head(sort(theseRElevels_numeric),n=1)
        lastGrpName <- tail(sort(theseRElevels_numeric),n=1)
      } else{
        maxGrpNum <- length(thisRE_levels)
        firstGrpName <- head(sort(thisRE_levels),n=1)
        lastGrpName <- tail(sort(thisRE_levels),n=1)
      }

      theseGroups_elipses <- theseGroups_elipses %>%
        group_by(get(thisColName)) %>%
        rename(thisRE = `get(thisColName)`) %>%
        mutate(thisRE = case_when(thisRE==firstGrpName & prevRE != "..." ~ thisRE,
                                  thisRE==lastGrpName & prevRE != "..."  ~ thisRE,
                                  TRUE ~ "..."),
        )
    } else{
      #this RE has some character labels
      prevNestedName <- RElist[numRE-i + 2]
      fullNestedName <- RElist[numRE-i+1]
      prevNestedOrder <- as.character(unique(theseGroups_elipses[,prevNestedName][[1]]))
      group_subgroup_table <- theseGroups_elipses %>%
        mutate(thisRE = get(thisColName),
               onescol = 1,
               fPrevNestedVar = fct_relevel(get(prevNestedName),
                                            prevNestedOrder)) %>%
        group_by(fPrevNestedVar) %>%
        mutate(groupID = cur_group_id(),
               keepThis = case_when(prevRE != "..." ~ 1,
                                    TRUE ~ 0))

      for(j in 1:length(unique(group_subgroup_table$groupID))){
        sub_group_subgroup_table <- group_subgroup_table %>%
          filter(groupID==j)
        sortedThisRE <- sub_group_subgroup_table$thisRE
        minThisRE <- sortedThisRE[[1]]
        maxThisRE <- sortedThisRE[[length(sortedThisRE)]]
        for(k in 1:length(sortedThisRE)){
          thisREVal <- sortedThisRE[[k]]
          if(thisREVal!=minThisRE & thisREVal!=maxThisRE){
            sub_group_subgroup_table$keepThis[[k]] <- 0
          }
        }
        if(j==1){
          new_group_subgroup_table <- sub_group_subgroup_table
        } else{
          new_group_subgroup_table <- new_group_subgroup_table %>%
            bind_rows(sub_group_subgroup_table)
        }
      }
      suppressMessages(
        group_subgroup_table <- new_group_subgroup_table %>%
        select(-c(onescol, fPrevNestedVar, groupID)) %>%
        filter(keepThis==1)
        )

      fullNestedName <- RElist[numRE-i+1]
      names(theseGroups_elipses)[which(names(theseGroups_elipses)==thisColName)] <- "thisRE"
      names(theseGroups_elipses)[which(names(theseGroups_elipses)==fullNestedName)] <- "fullNestedName"
      theseGroups_elipses <- theseGroups_elipses %>%
        rowwise() %>%
        mutate(keepThis = ifelse(fullNestedName %in% group_subgroup_table[RElist[numRE-i+1]][[1]],1,0 )) %>%
        ungroup() %>%
        mutate(thisRE = case_when(keepThis==1 & prevRE != "..." ~ thisRE,
                                  TRUE ~ "..."),
        )

      names(theseGroups_elipses)[which(names(theseGroups_elipses)=="fullNestedName")] <- fullNestedName
    }


    names(theseGroups_elipses)[which(names(theseGroups_elipses)=="prevRE")] <- "thisREold"
    if(thisColName %in% names(theseGroups_elipses)){
      theseGroups_elipses <- theseGroups_elipses %>%
        relocate(thisRE, .after=thisREold) %>%
        select(-!!thisColName)
    } else{
      theseGroups_elipses <- theseGroups_elipses %>%
        relocate(thisRE, .after=thisREold)
    }

    names(theseGroups_elipses)[which(names(theseGroups_elipses)=="thisRE")] <- thisColName
    names(theseGroups_elipses)[which(names(theseGroups_elipses)=="thisREold")] <- prevColName

  }

  for(j in 1:nrow(theseGroups_elipses)){
    obsLevelColNum <- which(names(theseGroups_elipses)=="ObsLevel")
    if(theseGroups_elipses[j,obsLevelColNum-1] == "..."){
      theseGroups_elipses[j,obsLevelColNum] <- "..."
    }
  }

  theseGroups_subset <- theseGroups_elipses %>%
    select(-any_of(c("orderID", names(lme_model$groups), "keepThis"))) %>%
    unique()

  theseGroups_subset_posInfo <- theseGroups_subset %>%
    rowid_to_column(var = "ObsLevelPos") %>%
    relocate(ObsLevelPos, .after=everything())

  # Observation level is first - no grouping by anything, just in the order it is
  theseGroups_subset2 <- theseGroups_subset %>%
    rowid_to_column(var="ObsLevel_rawPos") %>%
    relocate(ObsLevel_rawPos, .after=everything())

  for(i in numRE:1){
    thisREname <- paste0("RE",i)
    rawIdxVal <- 1
    theseGroups_subset2$thisRE_raw <- rep(rawIdxVal, nrow(theseGroups_subset2))
    for(j in 2:nrow(theseGroups_subset2)){
      if(theseGroups_subset2[j-1,thisREname][[1]]!=theseGroups_subset2[j,thisREname][[1]]){
        rawIdxVal <- rawIdxVal + 1
      }
      theseGroups_subset2[j,"thisRE_raw"] <- rawIdxVal
    }
    names(theseGroups_subset2)[which(names(theseGroups_subset2)=="thisRE_raw")] <- paste0(thisREname, "_rawPos")
  }
  theseGroups_subset3 <- theseGroups_subset2
  rawColIdx <- rev(which(str_ends(names(theseGroups_subset2),"rawPos")))

  for(i in rawColIdx){
    thisColName <- names(theseGroups_subset3)[i]
    thisCol <- theseGroups_subset3[[i]]
    if(i==ncol(theseGroups_subset2)){
      theseGroups_subset3$newCol <- thisCol
      maxNextCol <- 0
    } else{
      maxNextCol <- maxNextCol + max(theseGroups_subset3[[i+1]])
      theseGroups_subset3$newCol <- thisCol + maxNextCol
    }

    names(theseGroups_subset3)[which(names(theseGroups_subset3)=="newCol")] <- paste0(str_remove(thisColName,"rawPos"),"edgePos")
  }

  theseGroups_subset3_clean <- theseGroups_subset3 %>%
    select(-ends_with("rawPos")) %>%
    as_tibble()

  n_levels <- c()
  for(i in 1:(numRE+1)){
    n_levels[i] <- sum(2^seq(from=0,to=i,by=1))
  }
  n_nodes <- sum(n_levels)

  numReps <- sum(2^seq(from=0, to=(numRE), by=1))

  grp_lng_lme_model <- data.frame(name=as.character(),
                                  label=as.character(),
                                  x_pos=as.numeric(),
                                  y_pos=as.numeric())

  names_list <- c(paste0("RE",1:numRE),"ObsLevel")
  reNames_nested <- rep("",length(names_list))
  reNames_nested_wSize <- rep("",length(names_list))
  nested_df <- theseGroups_subset3_clean
  for(i in 1:length(names_list)){

    thisName <- names_list[i]
    nested_df$actName <- theseGroups_subset3_clean[[thisName]]

    if(i > 1){
      prevName <- paste0("Nested",i-1)
      nested_df[[thisName]] <- paste0(nested_df[[prevName]],"/",theseGroups_subset3_clean[[thisName]])

    }
    theseLevelsOrdered <- unique(nested_df[[thisName]])
    endPosVal <- length(unique(nested_df[[thisName]]))
    rowRange <- rep(as.numeric(NA), endPosVal)
    names(nested_df)[which(names(nested_df)==thisName)] <- "thisName"
    suppressMessages(
      rowRange <- nested_df %>%
      group_by(thisName) %>%
      summarize(rowVal = mean(cur_group_rows())) %>%
      arrange(rowVal) %>%
      left_join(nested_df %>% select(thisName, actName),
                multiple="first")
      )

    names(nested_df)[which(names(nested_df)=="thisName")] <- paste0("Nested",i)

    grp_lng_lme_model <- grp_lng_lme_model %>%
      bind_rows(data.frame(name=rep(thisName, endPosVal),
                           label=rowRange$actName,
                           x_pos=rep((i*2-1), endPosVal),
                           y_pos=rowRange$rowVal))
    if(i == 1){
      reNames_nested[i] <- names(lme_model$groups)[1]
      reNames_nested_wSize[i] <- paste0(reNames_nested[i],"\nNum.=",lme_model$dims$ngrps[[numRE]])
    } else if(i == length(names_list)){
      reNames_nested[i] <- "Observation Error"
      reNames_nested_wSize[i] <- paste0(reNames_nested[i],"\nNum.=",lme_model$dims$N)
    } else{
      ngrpsCol <- which(names(lme_model$dims$ngrps)==names(lme_model$groups)[i])
      reNames_nested[i] <- paste(names(lme_model$groups)[i], "in", reNames_nested[i-1])
      reNames_nested_wSize[i] <- paste0(reNames_nested[i],"\nNum.=", lme_model$dims$ngrps[[ngrpsCol]])
    }
  }

  if(includeSizes){
    grp_lng_lme_model <- grp_lng_lme_model %>%
      bind_rows(data.frame(name=rep("RE Label", numRE+1),
                           label=reNames_nested_wSize,
                           x_pos=seq(from=1, to=numRE*2+1,by=2),
                           y_pos=max(grp_lng_lme_model$y_pos)+2))
  } else{
    grp_lng_lme_model <- grp_lng_lme_model %>%
      bind_rows(data.frame(name=rep("RE Label", numRE+1),
                           label=reNames_nested,
                           x_pos=seq(from=1, to=numRE*2+1,by=2),
                           y_pos=max(grp_lng_lme_model$y_pos)+2))
  }


  if(is(this_model,"merMod")){
    fixedLevelInfo <- getFixLevel(lme_model,
                                  fixedCall = parse(text=lmer_formula_fixed_clean),
                                  randomCall = parse(text=lmer_formula_random_clean))
  } else if(is(this_model, "lme")){
    fixedLevelInfo <- getFixLevel(lme_model,
                                  fixedCall = lme_model$call$fixed,
                                  randomCall = lme_model$call$random)
  }

  namesFixed <- names(fixedLevelInfo)
  numFixed <- length(namesFixed)
  fixed_RElevel <- rep(numRE+1,numFixed)
  fixedPlaceholders <- rep(as.character(NA), length(names_list))

  reNamesPlusOE <- c(names(lme_model$groups), "Observation Error")
  if(numFixed > 0){
    for(i in 1:(numRE+1)){
      thisREName <- reNamesPlusOE[i]
      fixedNameList <- names(fixedLevelInfo[fixedLevelInfo==thisREName])
      if(length(fixedNameList) == 0){
        fixedPlaceholders[i] <- ""
      } else{
        fixedPlaceholders[i] <- paste(fixedNameList, collapse="\n")
      }
    }
  }

  fixedPlaceholders[is.na(fixedPlaceholders)] <- ""

  grp_lng_lme_model <- grp_lng_lme_model %>%
    bind_rows(data.frame(name=rep("Fixed Label", numRE+1),
                         label=fixedPlaceholders,
                         x_pos=seq(from=1, to=numRE*2+1,by=2),
                         y_pos=max(grp_lng_lme_model$y_pos) - 1))

  labelOffset <- 0
  topOnlyTwoLevels <- FALSE
  numTopRELevels <- 3
  if(includeLabels){
    grp_lng_lme_model$x_pos <- grp_lng_lme_model$x_pos + 1
    get_y_pos <- grp_lng_lme_model %>%
      filter(name=="RE1" & label=="...") %>%
      select(y_pos) %>%
      pull()
    if(length(get_y_pos)==0){
      get_y_pos <- grp_lng_lme_model %>%
        filter(name=="RE1") %>%
        summarize(meanVal = mean(y_pos)) %>%
        select(meanVal) %>%
        pull()
      topOnlyTwoLevels <- TRUE
      numTopRELevels <- 2
    }
    grp_lng_lme_model <- grp_lng_lme_model %>%
      bind_rows(data.frame(name=rep("Diagram Label", 3),
                           label=c("Measurement\nDiagram", "Fixed", "Random"),
                           x_pos=1,
                           y_pos=c(get_y_pos,
                                   max(grp_lng_lme_model$y_pos) - 1,
                                   max(grp_lng_lme_model$y_pos))))
    labelOffset <- 3
  }
  # if("extrafont" %in% rownames(installed.packages())){
  #   do.call('library', list("extrafont"))
  #   useFontName = "Times New Roman Bold"
  # } else{
  #   useFontName = "Arial"
  # }
  useFontName = "Arial"
  if(includeLabels){
    suppressMessages(
      nodes_lme_model <- create_node_df(
        n = nrow(grp_lng_lme_model),
        type="a",
        label = grp_lng_lme_model$label,
        style="filled",
        color=c(rep(nodeColors$diagram, numTopRELevels), # First RE will always have three circles for first, middle, and last
                rep(nodeColors$diagram,nrow(grp_lng_lme_model) -
                      (numTopRELevels+2*(numRE+1))-labelOffset),
                rep(nodeColors$random,numRE+1),
                rep(nodeColors$fixed,numRE+1),
                nodeColors$diagram,nodeColors$fixed,nodeColors$random),
        fillcolor=c(rep(nodeFillColors$diagram, numTopRELevels), # First RE will always have three circles for first, middle, and last
                    rep(nodeFillColors$diagram,nrow(grp_lng_lme_model) -
                          (numTopRELevels+2*(numRE+1))-labelOffset),
                    rep(nodeFillColors$random,numRE+1),
                    rep(nodeFillColors$fixed,numRE+1),
                    nodeFillColors$diagram,nodeFillColors$fixed,nodeFillColors$random),
        fontname=useFontName,
        fontcolor=c(rep(nodeFontColors$diagram, numTopRELevels), # First RE will always have three circles for first, middle, and last
                    rep(nodeFontColors$diagram,nrow(grp_lng_lme_model) -
                          (numTopRELevels+2*(numRE+1))-labelOffset),
                    rep(nodeFontColors$random,numRE+1),
                    rep(nodeFontColors$fixed,numRE+1),
                    nodeFontColors$diagram,nodeFontColors$fixed,nodeFontColors$random),
        shape=c(rep("circle", numTopRELevels), # First RE will always have three circles for first, middle, and last
                rep("circle",nrow(grp_lng_lme_model) -
                      (numTopRELevels+2*(numRE+1))-labelOffset),
                rep("box",2*(numRE+1) + labelOffset )), # 2*numRE boxes on the top for the random effect labels and fixed effect boxes
        node_aes(
          x=grp_lng_lme_model$x_pos,
          y=grp_lng_lme_model$y_pos,
          fixedsize = FALSE))
    )
  } else{
    suppressMessages(
      nodes_lme_model <- create_node_df(
        n = nrow(grp_lng_lme_model),
        type="a",
        label = grp_lng_lme_model$label,
        style="filled",
        color=c(rep(nodeColors$diagram, numTopRELevels), # First RE will always have three circles for first, middle, and last
                rep(nodeColors$diagram,nrow(grp_lng_lme_model) -
                      (numTopRELevels+2*(numRE+1))-labelOffset),
                rep(nodeColors$random,numRE+1),
                rep(nodeColors$fixed,numRE+1)),
        fillcolor=c(rep(nodeFillColors$diagram, numTopRELevels), # First RE will always have three circles for first, middle, and last
                    rep(nodeFillColors$diagram,nrow(grp_lng_lme_model) -
                          (numTopRELevels+2*(numRE+1))-labelOffset),
                    rep(nodeFillColors$random,numRE+1),
                    rep(nodeFillColors$fixed,numRE+1)),
        fontname=useFontName,
        fontcolor=c(rep(nodeFontColors$diagram, numTopRELevels), # First RE will always have three circles for first, middle, and last
                    rep(nodeFontColors$diagram,nrow(grp_lng_lme_model) -
                          (numTopRELevels+2*(numRE+1))-labelOffset),
                    rep(nodeFontColors$random,numRE+1),
                    rep(nodeFontColors$fixed,numRE+1)),
        shape=c(rep("circle", numTopRELevels), # First RE will always have three circles for first, middle, and last
                rep("circle",nrow(grp_lng_lme_model) -
                      (numTopRELevels+2*(numRE+1))-labelOffset),
                rep("box",2*(numRE+1) + labelOffset)), # 2*numRE boxes on the top for the random effect labels and fixed effect boxes
        node_aes(
          x=grp_lng_lme_model$x_pos,
          y=grp_lng_lme_model$y_pos,
          fixedsize = FALSE))
    )
  }

  setNames <- paste0("Set", 1:numRE)
  groupSets <- list(rep(list(), numRE))

  for(i in 1:numRE){
    namesRE <- names(theseGroups_subset3_clean)[c(i,i+1)]
    names_edgePos <- names(theseGroups_subset3_clean)[which(names(theseGroups_subset3_clean) %in% paste0(namesRE, "_edgePos"))]
    groups_subset <- theseGroups_subset3_clean[,c(namesRE, names_edgePos)] %>%
      unique() %>%
      rename(fromLabel=!!namesRE[1],
             toLabel=!!namesRE[2],
             fromPos=!!names_edgePos[1],
             toPos=!!names_edgePos[2])
    groupSets[[i]] <- groups_subset

  }

  group_edges_df <- groupSets[[1]]
  if(numRE > 1){
    for(i in 2:numRE){
      group_edges_df <- group_edges_df %>%
        bind_rows(groupSets[[i]])
    }
  }


  edges_lme_model <-
    create_edge_df(
      from = group_edges_df$fromPos,
      to = group_edges_df$toPos,
      rel = "requires",
      color = "black")
  graph_lme_model <- create_graph(nodes_df=nodes_lme_model,
                                  edges_df=edges_lme_model)
  if(!is.null(this_file_path)){
    suppressWarnings(
      graph_lme_model %>%
        export_graph(file_name = this_file_path,
                     file_type = this_file_type,
                     width=widthVal,height=heightVal)
    )
  }
  graph_lme_model %>%
      render_graph(width=widthVal,height=heightVal,
                   output="graph", as_svg=TRUE)

}

# https://stackoverflow.com/questions/24129124/how-to-determine-if-a-character-vector-is-a-valid-numeric-or-integer-vector
# Post by Adriano Rivolli on Sep 21, 2017 at 16:30 accessed on Dec 10, 2024 at 4:30 pm

#' Catch numeric values stored as strings
#'
#' `catchNumeric()` takes a character list and determines if it is a valid numeric list.
#' Adapted from: https://stackoverflow.com/questions/24129124/how-to-determine-if-a-character-vector-is-a-valid-numeric-or-integer-vector
#' Post by Adriano Rivolli on Sep 21, 2017 at 16:30 accessed on Dec 10, 2024 at 4:30 pm
#'
#' @param strList A character vector
#'
#' @returns A list of vectors, with each vector having the correct type, either character or numeric.
#' @export
#'
#' @examples
#' charList <- c("A","B","C")
#' catchNumeric(charList)
#'
#' numList <- c("1","2","3")
#' catchNumeric(numList)
#'
#' mixList <- c("A","2","C","4")
#' catchNumeric(mixList)
catchNumeric <- function(strList) {
  numlist <- suppressWarnings(as.numeric(strList))
  fixedList <- as.list(strList)
  fixedList[!is.na(numlist)] <- numlist[!is.na(numlist)]
  fixedList
}

#' Identify the fixed effect model structure level
#'
#' `getFixLevel()` uses the degrees of freedom information from `lme()`
#' to identify the correct placement of each fixed effect and interaction term
#' in a model structure diagram for hierarchical mixed effects models.
#'
#' @param lme_model A `nlme` lme model object inherited from `model_diagram()`
#' @param fixedCall The fixedCall object from an lme model or parsed text
#'    containing fixed variable names from a merMod object
#' @param randomCall The randomCall object from an lme model or parsed text
#'    containing the random effect structure from a merMod object
#'
#' @returns The random effect at which the fixed effect is placed at for degrees
#'    of freedom calculations.
#' @export
#'
#' @examples
#' #' library(nlme)
#'
#' sleepstudy_lme <- lme(Reaction ~ Days, random=~Days|Subject, data=sleepstudy)
#' getFixLevel(sleepstudy_lme, fixedCall = sleepstudy_lme$call$fixed, randomCall = sleepstudy_lme$call$random)
getFixLevel <- function(lme_model, fixedCall, randomCall){

  data <- lme_model$data

  this.call <- as.list(match.call())[-1L]
  this.call$fixed <- eval(fixedCall)
  fixed <- this.call$fixed

  if (!inherits(fixed, "formula") || length(fixed) != 3) {
    stop("fixed-effects model must be a formula of the form \"resp ~ pred\"")
  }
  method <- "REML"
  REML <- method == "REML"
  if(class(randomCall)=="call"){
    random <- eval(randomCall)
  } else if(class(randomCall)=="expression"){
    random <- eval(parse(text=paste0("~", randomCall)))
  } else{
    stop(paste("randomCall has a class that is", class(randomCall)))
  }



  random <- reStruct(formula(random), data = NULL)
  reSt <- reStruct(random, REML = REML, data = NULL)
  groups <- getGroupsFormula(reSt)

  correlation <- NULL

  weights <- NULL
  na.action <- na.fail

  corQ <- lmeQ <- 1

  ## create an lme structure containing the random effects model and plug-ins
  lmeSt <- lmeStruct(reStruct = reSt, corStruct = correlation,
                     varStruct = varFunc(weights))

  ## extract a data frame with enough information to evaluate
  ## fixed, groups, reStruct, corStruct, and varStruct
  mfArgs <- list(formula = asOneFormula(formula(lmeSt), fixed, groups),
                 data = data, na.action = na.action)

  mfArgs$drop.unused.levels <- TRUE
  dataMix <- do.call(model.frame, mfArgs)

  origOrder <- row.names(dataMix)	# preserve the original order

  ## sort the model.frame by groups and get the matrices and parameters
  ## used in the estimation procedures
  grps <- getGroups(dataMix, groups)
  ## ordering data by groups
  if (inherits(grps, "factor")) {	# single level
    ord <- order(grps)	#"order" treats a single named argument peculiarly
    grps <- data.frame(grps)
    row.names(grps) <- origOrder
    names(grps) <- as.character(deparse((groups[[2L]])))
  } else {
    ord <- do.call(order, grps)
    ## making group levels unique
    for(i in 2:ncol(grps)) {
      grps[, i] <-
        as.factor(paste(as.character(grps[, i-1]),
                        as.character(grps[, i  ]), sep = "/"))
    }
  }
  if (corQ > lmeQ) {
    ## may have to reorder by the correlation groups
    ord <- do.call(order, getGroups(dataMix,
                                    getGroupsFormula(correlation)))
  }
  grps <- grps[ord, , drop = FALSE]
  dataMix <- dataMix[ord, ,drop = FALSE]
  revOrder <- match(origOrder, row.names(dataMix)) # putting in orig. order

  ## obtaining basic model matrices
  N <- nrow(grps)
  Z <- model.matrix(reSt, dataMix)      # stores contrasts in matrix form
  ncols <- attr(Z, "ncols")
  Names(lmeSt$reStruct) <- attr(Z, "nams")
  ## keeping the contrasts for later use in predict
  contr <- attr(Z, "contr")
  X <- model.frame(fixed, dataMix)

  ngrps <- lme_model$dims$ngrps
  Terms <- attr(X, "terms")
  if (length(attr(Terms, "offset")))
    stop("offset() terms are not supported")
  auxContr <- lapply(X, function(el)
    if (inherits(el, "factor") &&
        length(levels(el)) > 1) contrasts(el))
  contr <- lme_model$contrasts
  contr <- c(contr, auxContr[is.na(match(names(auxContr), names(contr)))])
  contr <- contr[!unlist(lapply(contr, is.null))]
  X <- model.matrix(fixed, data=X)
  y <- eval(fixed[[2L]], dataMix)
  ncols <- c(ncols, dim(X)[2L], 1)
  Q <- ncol(grps)

  # Below is adapted from getFixDF() from nlme's lme.R script
  assign <- attr(X, "assign")
  terms <- Terms
  ## calculates degrees of freedom for fixed effects Wald tests
  if (!is.list(assign)) {               # in R
    namTerms <- attr(terms, "term.labels")
    if (attr(terms, "intercept") > 0) {
      namTerms <- c("(Intercept)", namTerms)
    }
    namTerms <- factor(assign, labels = namTerms)
    assign <- split(order(assign), namTerms)
  }
  ## function to check if a vector is (nearly) a multiple of (1,1,...,1)
  const <- function(x, tolerance = sqrt(.Machine$double.eps)) {
    if (length(x) < 1) return(NA)
    x <- as.numeric(x)
    ## return
    all(abs(if(x[1L] == 0) x else x/x[1L] - 1) < tolerance)
  }
  N <- nrow(X)
  p <- ncol(X)
  Q <- ncol(grps)
  Qp1 <- Q + 1L
  namX <- colnames(X)
  ngrps <- rev(ngrps)[-(1:2)]
  stratNam <- c(names(ngrps), "Observation Error")

  dfX <- dfTerms <- setNames(c(ngrps, N) - c(0, ngrps), stratNam)
  valX <- setNames(double(p), namX)
  namTerms <- names(assign)
  valTerms <- double(length(assign))
  names(valTerms) <- namTerms
  if (any(notIntX <- !apply(X, 2, const))) {
    ## percentage of groups for which columns of X are inner
    innP <- array(c(rep(1,p),
                    inner_perc_table_R(X, grps)),
                  dim = c(p, Qp1),
                  dimnames = list(namX, stratNam))
    ## strata in which columns of X are estimated
    ## ignoring fractional inner percentages for now
    stratX <- stratNam[apply(innP, 1, function(el, index) max(index[el > 0]),
                             index = 1:Qp1)]
    ## strata in which terms are estimated
    notIntTerms <- unlist(lapply(assign,
                                 function(el, notIntX) {
                                   any(notIntX[el])
                                 }, notIntX = notIntX))
    stratTerms <- stratNam[unlist(lapply(assign,
                                         function(el) max(match(stratX[el], stratNam))
    ))][notIntTerms] # Can stop here for determining the level of Fixed effects
    names(stratTerms) <- namTerms[-1]
    # stratX <- stratX[notIntX]
    # names(stratX) <- namX[-1]
    return(stratTerms)
  }
}


#
# Code translation assistance provided by Anthropic's Claude 3.5 Sonnet (2024 version) on December 17, 2024.
# translated inner_perc() and inner_perc_table() from nlme's nlmefit.c file (https://github.com/cran/nlme/blob/master/src/nlmefit.c)
#

# # static double
# inner_perc(double *x, int *grp, int n)
# /* percentage of groups for which x is inner */
#   {
#     /* x - column of X matrix to be assessed
#     grp - integer vector with groups
#     n - length of x and grp
#     data are assumed to be ordered by grp */
#
#       int currGrp, nn = 0, isInner;
#     double nInner = 0., nGrp = 0., currVal;
#
#     while (nn < n) {
#       currGrp = grp[nn];
#       currVal = x[nn];
#       nGrp++;
#       isInner = 0;
#       do {
#         if (isInner == 0 && x[nn] != currVal) {
#           nInner++;
#           isInner = 1;
#         }
#         nn++;
#       } while (nn < n && currGrp == grp[nn]);
#     }
#     return(nInner/nGrp);
#   }

#' Calculates inner group percentage
#'
#' `inner_perc_R()` Calculates the percentage of groups for which the fixed
#'  effect `x` is "inner". Data are assumed to be ordered by `grp`.
#'
#'  Translated `inner_perc()` from nlme's nlmefit.c file (https://github.com/cran/nlme/blob/master/src/nlmefit.c)
#
#'  Code translation assistance provided by Anthropic's Claude 3.5 Sonnet (2024 version) on December 17, 2024.
#'
#' @param x Column of X matrix to be assessed
#' @param grp Integer vector with groups
#'
#' @returns Numeric vector of percentages
#' @export
#'
#' @examples
#' \dontrun{inner_perc_R(X[, j], grps[, i])}
inner_perc_R <- function(x, grp){

  if (length(x) != length(grp)) {
    stop("x and grp must have the same length")
  }

  n <- length(x)
  nInner <- 0
  nGrp <- 0
  nn <- 1

  while(nn <= n){
    currGrp <- grp[[nn]]
    currVal <- x[[nn]]
    nGrp <- nGrp + 1
    isInner <- FALSE

    while(nn <= n & currGrp == grp[nn]){
      if(!isInner & x[[nn]] != currVal){
        nInner <- nInner + 1
        isInner <- TRUE
      }
      nn <- nn + 1
    }


  }
  return(nInner/nGrp)
}


#' Table of inner group percentages for each fixed effect variable
#'
#' `inner_perc_table_R()` Calculates the inner group percentage for each fixed
#'  effect variable.
#'
#'  Translated `inner_perc_table()` from nlme's nlmefit.c file (https://github.com/cran/nlme/blob/master/src/nlmefit.c)
#
#'  Code translation assistance provided by Anthropic's Claude 3.5 Sonnet (2024 version) on December 17, 2024.
#'
#'
#' @param X Matrix or data frame from lme object
#' @param grps Matrix or data frame where each column is a grouping variable
#' @param p If `NULL` the number of columns of `X`, else a pre-specified number
#'        of fixed effects.
#' @param Q If `NULL` the number of columns in `grps`, else a pre-specified number
#'        of random effects or groups.
#'
#' @returns A matrix of inner group proportions.
#' @export
#'
#' @examples
#' \dontrun{inner_perc_table_R(X, grps)}
inner_perc_table_R <- function(X, grps, p = NULL, Q = NULL){

    # Input validation and preparation
  if (is.null(p)){
    p <- ncol(X)
  }
  if (is.null(Q)){
    Q <- ncol(grps)
  }

  if (!is.matrix(X) & !is.data.frame(X)){
    X <- as.matrix(X)
  }
  if (!is.matrix(grps) & !is.data.frame(grps)){
    grps <- as.matrix(grps)
  }

  # Validate dimensions
  if (nrow(X) != nrow(grps)) {
    stop("X and grps must have the same number of rows")
  }
  if (ncol(grps) != Q) {
    stop("Number of grouping variables doesn't match Q")
  }
  if (ncol(X) != p) {
    stop("Number of columns in X doesn't match p")
  }

  # Initialize results matrix
  pTable <- matrix(0, nrow = p, ncol = Q)

  # Calculate inner percentages
  for (i in 1:Q) {
    for (j in 1:p) {
      pTable[j, i] <- inner_perc_R(X[, j], grps[, i])
    }
  }

  # Add row and column names if they exist
  if (!is.null(colnames(X))){
    rownames(pTable) <- colnames(X)
  }
  if (!is.null(colnames(grps))){
    colnames(pTable) <- colnames(grps)
  }

  return(pTable)

}
#' Diagram elements
#'
#' @description
#' Similar to \link{ggplot2}'s \link{theme} system, the `md_` functions
#' specify the display of how the node components of the diagram are drawn.
#'
#'   - `md_color()`: border of the node.
#'   - `md_fill()`: fill color of the node.
#'   - `md_fontColor()`: font color of the text in the nodes.
#'
#'
#' @param diagram Specifies the outline color, fill color, or font color
#'    for the elements in the measurement diagram circles (hierarchical diagram).
#'    Default is `"gray25"` for ([md_color()]), `"aliceblue"` for ([md_fill()]),
#'    and `"black"` for ([md_fontColor()]).
#' @param random Specifies the outline color, fill color, or font color
#'    for the elements in the random effect variable boxes.
#'    Default is `"gray25"` for ([md_color()]), `"aliceblue"` for ([md_fill()]),
#'    and `"black"` for ([md_fontColor()]).
#' @param fixed Specifies the outline color, fill color, or font color
#'    for the elements in the fixed effect variable boxes.
#'    Default is `"gray25"` for ([md_color()]), `"darkseagreen1"` for ([md_fill()]),
#'    and `"black"` for ([md_fontColor()]).
#'
#' @examples
#' library(tidyverse)
#' library(DiagrammeR)
#' # merMod object example
#' library(lme4)
#' library(nlme)
#'
#' sleepstudy_lmer <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
#' summary(sleepstudy_lmer)
#' model_diagram(sleepstudy_lmer,
#'   nodeColors = md_color(diagram="steelblue", random="steelblue", fixed="darkolivegreen1"),
#'   nodeFillColors = md_fill(diagram="aliceblue", random="aliceblue", fixed="darkseagreen1"),
#'   nodeFontColors = md_fontColor(diagram="black", random="blue", fixed="black"))
#'
#' @name md
#' @aliases NULL
NULL

#' @export
#' @rdname md
md_color <- function(diagram="gray25", random="gray25", fixed="gray25") {

  structure(
    list(diagram = diagram, random=random, fixed=fixed),
    class = c("md_color", "md")
  )
}

#' @export
#' @rdname md
md_fill <- function(diagram="aliceblue", random="aliceblue", fixed="darkseagreen1") {

  structure(
    list(diagram = diagram, random=random, fixed=fixed),
    class = c("md_color", "md")
  )
}

#' @export
#' @rdname md
md_fontColor <- function(diagram="black", random="black", fixed="black") {

  structure(
    list(diagram = diagram, random=random, fixed=fixed),
    class = c("md_color", "md")
  )
}
