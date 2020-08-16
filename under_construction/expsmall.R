
diffLogDensExpOrigDestSmall <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(diffLogDensExpOrigDestSmall_R, combined)
    }
    else {
        components <- combined@account@components
        iterators.comp <- combined@iteratorsComp
        model.uses.exposure <- combined@modelUsesExposure
        mappings.from.exp <- combined@mappingsFromExp
        mappings.to.exp <- combined@mappingsToExp
        i.expose.up.orig <- combined@iExposure
        i.expose.low.orig <- combined@iExposureOther
        i.expose.up.dest <- combined@iExpFirst
        i.expose.low.dest <- combined@iExpFirstOther
        iterator.exposure <- combined@iteratorExposure
        diff <- combined@diffProp
        i.comp <- combined@iComp
        i.orig.dest <- combined@iOrigDest
        i.pool <- combined@iPool
        i.int.net <- combined@iIntNet
        i.births <- combined@iBirths
        exposure <- combined@exposure
        systemModels <- combined@systemModels
        age.time.step <- combined@ageTimeStep
        diff.up.orig <- -diff
        diff.low.orig <- diff
        diff.up.dest <- diff
        diff.low.dest <- -diff
        ans <- 0
        for (i in seq_along(components)) {
            if (model.uses.exposure[i + 1]) { ## note that 'net' components never use exposure
                component <- components[[i]]
                theta <- systemModels[[i + 1L]]@theta
                strucZeroArray <- systemModels[[i + 1L]]@strucZeroArray
                iterator.comp <- iterators.comp[[i]]
                mapping.from.exp <- mappings.from.exp[[i]]
                is.orig.dest <- i == i.orig.dest
                is.pool <- i == i.pool
                is.births <- i == i.births
                if (is.births) {
                    i.cell.up.orig <- getICellBirthsFromExp(i = i.expose.up.orig,
                                                            mapping = mapping.from.exp)
                    i.cell.low.orig <- getICellBirthsFromExp(i = i.expose.low.orig,
                                                             mapping = mapping.from.exp)
                    i.cell.up.dest <- getICellBirthsFromExp(i = i.expose.up.dest,
                                                            mapping = mapping.from.exp)
                    i.cell.low.dest <- getICellBirthsFromExp(i = i.expose.low.dest,
                                                             mapping = mapping.from.exp)
                    cell.births.is.affected <- i.cell.up.orig > 0L
                    if (!cell.births.is.affected)
                        next
                    mapping.to.exp <- mappings.to.exp[[i]]
                    ## copying use of 'getIExposureFromBirths' from
                    ## 'diffLogDensExpComp', but may be possible to just
                    ## use 'i.expose.up'?
                    i.exp.first.up.orig <- getIExposureFromBirths(i = i.cell.up.orig,
                                                                  mapping = mapping.to.exp)
                    i.exp.first.low.orig <- getIExposureFromBirths(i = i.cell.low.orig,
                                                                   mapping = mapping.to.exp)
                    i.exp.first.up.dest <- getIExposureFromBirths(i = i.cell.up.dest,
                                                                  mapping = mapping.to.exp)
                    i.exp.first.low.dest <- getIExposureFromBirths(i = i.cell.low.dest,
                                                                   mapping = mapping.to.exp)
                }
                else {
                    i.cell.up.orig <- getICellCompFromExp(i = i.expose.up.orig,
                                                          mapping = mapping.from.exp)
                    i.cell.low.orig <- getICellCompFromExp(i = i.expose.low.orig,
                                                           mapping = mapping.from.exp)
                    i.cell.up.dest <- getICellCompFromExp(i = i.expose.up.dest,
                                                          mapping = mapping.from.exp)
                    i.cell.low.dest <- getICellCompFromExp(i = i.expose.low.dest,
                                                           mapping = mapping.from.exp)
                    i.exp.first.up.orig <- i.expose.up.orig
                    i.exp.first.low.orig <- i.expose.low.orig
                    i.exp.first.up.dest <- i.expose.up.dest
                    i.exp.first.low.dest <- i.expose.low.dest
                }
                if (is.orig.dest || is.pool || is.births) {
                    diff.log.up.orig <- diffLogDensExpOneOrigDestParChPool(iCell = i.cell.up.orig,
                                                                           hasAge = TRUE,
                                                                           ageTimeStep = age.time.step,
                                                                           updatedPopn = FALSE,
                                                                           updatedBirths = FALSE,
                                                                           component = component,
                                                                           theta = theta,
                                                                           strucZeroArray = strucZeroArray,
                                                                           iteratorComp = iterator.comp,
                                                                           iExpFirst = i.exp.first.up.orig,
                                                                           exposure = exposure,
                                                                           iteratorExposure = iterator.exposure,
                                                                           diff = diff.up.orig,
                                                                           firstOnly = TRUE)
                    diff.log.low.orig <- diffLogDensExpOneOrigDestParChPool(iCell = i.cell.low.orig,
                                                                            hasAge = TRUE,
                                                                            ageTimeStep = age.time.step,
                                                                            updatedPopn = FALSE,
                                                                            updatedBirths = FALSE,
                                                                            component = component,
                                                                            theta = theta,
                                                                            strucZeroArray = strucZeroArray,
                                                                            iteratorComp = iterator.comp,
                                                                            iExpFirst = i.exp.first.low.orig,
                                                                            exposure = exposure,
                                                                            iteratorExposure = iterator.exposure,
                                                                            diff = diff.low.orig,
                                                                            firstOnly = TRUE)
                    diff.log.up.dest <- diffLogDensExpOneOrigDestParChPool(iCell = i.cell.up.dest,
                                                                           hasAge = TRUE,
                                                                           ageTimeStep = age.time.step,
                                                                           updatedPopn = FALSE,
                                                                           updatedBirths = FALSE,
                                                                           component = component,
                                                                           theta = theta,
                                                                           strucZeroArray = strucZeroArray,
                                                                           iteratorComp = iterator.comp,
                                                                           iExpFirst = i.exp.first.up.dest,
                                                                           exposure = exposure,
                                                                           iteratorExposure = iterator.exposure,
                                                                           diff = diff.up.dest,
                                                                           firstOnly = TRUE)
                    diff.log.low.dest <- diffLogDensExpOneOrigDestParChPool(iCell = i.cell.low.dest,
                                                                            hasAge = TRUE,
                                                                            ageTimeStep = age.time.step,
                                                                            updatedPopn = FALSE,
                                                                            updatedBirths = FALSE,
                                                                            component = component,
                                                                            theta = theta,
                                                                            strucZeroArray = strucZeroArray,
                                                                            iteratorComp = iterator.comp,
                                                                            iExpFirst = i.exp.first.low.dest,
                                                                            exposure = exposure,
                                                                            iteratorExposure = iterator.exposure,
                                                                            diff = diff.low.dest,
                                                                            firstOnly = TRUE)
                }
                else {
                    diff.log.up.orig <- diffLogDensExpOneComp(iCell = i.cell.up.orig,
                                                              hasAge = TRUE,
                                                              ageTimeStep = age.time.step,
                                                              updatedPopn = FALSE,
                                                              updatedBirths = FALSE,
                                                              component = component,
                                                              theta = theta,
                                                              strucZeroArray = strucZeroArray,
                                                              iteratorComp = iterator.comp,
                                                              iExpFirst = i.exp.first.up.orig,
                                                              exposure = exposure,
                                                              iteratorExposure = iterator.exposure,
                                                              diff = diff.up.orig,
                                                              firstOnly = TRUE)
                    diff.log.low.orig <- diffLogDensExpOneComp(iCell = i.cell.low.orig,
                                                               hasAge = TRUE,
                                                               ageTimeStep = age.time.step,
                                                               updatedPopn = FALSE,
                                                               updatedBirths = FALSE,
                                                               component = component,
                                                               theta = theta,
                                                               strucZeroArray = strucZeroArray,
                                                               iteratorComp = iterator.comp,
                                                               iExpFirst = i.exp.first.low.orig,
                                                               exposure = exposure,
                                                               iteratorExposure = iterator.exposure,
                                                               diff = diff.low.orig,
                                                               firstOnly = TRUE)
                    diff.log.up.dest <- diffLogDensExpOneComp(iCell = i.cell.up.dest,
                                                              hasAge = TRUE,
                                                              ageTimeStep = age.time.step,
                                                              updatedPopn = FALSE,
                                                              updatedBirths = FALSE,
                                                              component = component,
                                                              theta = theta,
                                                              strucZeroArray = strucZeroArray,
                                                              iteratorComp = iterator.comp,
                                                              iExpFirst = i.exp.first.up.dest,
                                                              exposure = exposure,
                                                              iteratorExposure = iterator.exposure,
                                                              diff = diff.up.dest,
                                                              firstOnly = TRUE)
                    diff.log.low.dest <- diffLogDensExpOneComp(iCell = i.cell.low.dest,
                                                               hasAge = TRUE,
                                                               ageTimeStep = age.time.step,
                                                               updatedPopn = FALSE,
                                                               updatedBirths = FALSE,
                                                               component = component,
                                                               theta = theta,
                                                               strucZeroArray = strucZeroArray,
                                                               iteratorComp = iterator.comp,
                                                               iExpFirst = i.exp.first.low.dest,
                                                               exposure = exposure,
                                                               iteratorExposure = iterator.exposure,
                                                               diff = diff.low.dest,
                                                               firstOnly = TRUE)
                }
                if (is.infinite(diff.log.up.orig))
                    return(diff.log.up.orig)
                if (is.infinite(diff.log.low.orig))
                    return(diff.log.low.orig)
                if (is.infinite(diff.log.up.dest))
                    return(diff.log.up.dest)
                if (is.infinite(diff.log.low.dest))
                    return(diff.log.low.dest)
                ans <- ans + diff.log.up.orig + diff.log.low.orig + diff.log.up.dest + diff.log.low.dest
            }
        }
        ans
    }
}




diffLogDensExpCompSmall <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(diffLogDensExpCompSmall_R, combined)
    }
    else {
        components <- combined@account@components
        iterators.comp <- combined@iteratorsComp
        model.uses.exposure <- combined@modelUsesExposure
        mappings.from.exp <- combined@mappingsFromExp
        mappings.to.exp <- combined@mappingsToExp
        i.expose.up <- combined@iExposure
        i.expose.low <- combined@iExposureOther
        is.increment <- combined@isIncrement
        diff <- combined@diffProp
        i.comp <- combined@iComp
        i.orig.dest <- combined@iOrigDest
        i.pool <- combined@iPool
        i.births <- combined@iBirths
        exposure <- combined@exposure
        systemModels <- combined@systemModels
        age.time.step <- combined@ageTimeStep
        if (is.increment) {
            diff.up <- diff
            diff.low <- -1 * diff
        }
        else {
            diff.up <- -1 * diff
            diff.low  <- diff
        }
        ans <- 0
        for (i in seq_along(components)) {
            if (model.uses.exposure[i + 1L]) {
                component <- components[[i]]
                theta <- systemModels[[i + 1L]]@theta
                strucZeroArray <- systemModels[[i + 1L]]@strucZeroArray
                iterator.comp <- iterators.comp[[i]]
                mapping.from.exp <- mappings.from.exp[[i]]
                is.orig.dest <- i == i.orig.dest
                is.pool <- i == i.pool
                is.births <- i == i.births
                if (is.births) {
                    i.cell.up <- getICellBirthsFromExp(i = i.expose.up,
                                                       mapping = mapping.from.exp)
                    i.cell.low <- getICellBirthsFromExp(i = i.expose.low,
                                                        mapping = mapping.from.exp)
                    cell.births.is.affected <- i.cell.up > 0L
                    if (!cell.births.is.affected)
                        next
                    mapping.to.exp <- mappings.to.exp[[i]]
                    ## copying use of 'getIExposureFromBirths' from
                    ## 'diffLogDensExpComp', but may be possible to just
                    ## use 'i.expose.up'?
                    i.exp.first.up <- getIExposureFromBirths(i = i.cell.up,
                                                             mapping = mapping.to.exp)
                    i.exp.first.low <- getIExposureFromBirths(i = i.cell.low,
                                                              mapping = mapping.to.exp)
                }
                else {
                    i.cell.up <- getICellCompFromExp(i = i.expose.up,
                                                     mapping = mapping.from.exp)
                    i.cell.low <- getICellCompFromExp(i = i.expose.low,
                                                      mapping = mapping.from.exp)
                    i.exp.first.up <- i.expose.up
                    i.exp.first.low <- i.expose.low
                }
                if (is.orig.dest || is.pool || is.births) {
                    diff.log.up <- diffLogDensExpOneOrigDestParChPool(iCell = i.cell.up,
                                                                      hasAge = TRUE,
                                                                      ageTimeStep = age.time.step,
                                                                      updatedPopn = FALSE,
                                                                      updatedBirths = FALSE,
                                                                      component = component,
                                                                      theta = theta,
                                                                      strucZeroArray = strucZeroArray,
                                                                      iteratorComp = iterator.comp,
                                                                      iExpFirst = i.exp.first.up,
                                                                      exposure = exposure,
                                                                      iteratorExposure = iterator.exposure,
                                                                      diff = diff.up,
                                                                      firstOnly = TRUE)
                    diff.log.low <- diffLogDensExpOneOrigDestParChPool(iCell = i.cell.low,
                                                                       hasAge = TRUE,
                                                                       ageTimeStep = age.time.step,
                                                                       updatedPopn = FALSE,
                                                                       updatedBirths = FALSE,
                                                                       component = component,
                                                                       theta = theta,
                                                                       strucZeroArray = strucZeroArray,
                                                                       iteratorComp = iterator.comp,
                                                                       iExpFirst = i.exp.first.low,
                                                                       exposure = exposure,
                                                                       iteratorExposure = iterator.exposure,
                                                                       diff = diff.low,
                                                                       firstOnly = TRUE)
                    
                }
                else {
                    diff.log.up <- diffLogDensExpOneComp(iCell = i.cell.up,
                                                         hasAge = TRUE,
                                                         ageTimeStep = age.time.step,
                                                         updatedPopn = FALSE,
                                                         updatedBirths = FALSE,
                                                         component = component,
                                                         theta = theta,
                                                         strucZeroArray = strucZeroArray,
                                                         iteratorComp = iterator.comp,
                                                         iExpFirst = i.exp.first.up,
                                                         exposure = exposure,
                                                         iteratorExposure = iterator.exposure,
                                                         diff = diff.up,
                                                         firstOnly = TRUE)
                    diff.log.low <- diffLogDensExpOneComp(iCell = i.cell.low,
                                                          hasAge = TRUE,
                                                          ageTimeStep = age.time.step,
                                                          updatedPopn = FALSE,
                                                          updatedBirths = FALSE,
                                                          component = component,
                                                          theta = theta,
                                                          strucZeroArray = strucZeroArray,
                                                          iteratorComp = iterator.comp,
                                                          iExpFirst = i.exp.first.low,
                                                          exposure = exposure,
                                                          iteratorExposure = iterator.exposure,
                                                          diff = diff.low,
                                                          firstOnly = TRUE)
                }
                if (is.infinite(diff.log.up))
                    return(diff.log.up)
                if (is.infinite(diff.log.low))
                    return(diff.log.low)
                ans <- ans + diff.log.up + diff.log.low
            }
        }
        ans
    }
}



double
diffLogDensExpCompSmall(SEXP combined_R)
{
    SEXP account_R = GET_SLOT(combined_R, account_sym);
    SEXP components_R = GET_SLOT(account_R, components_sym);
    int nComponents = LENGTH(components_R);
    SEXP iteratorsComp_R = GET_SLOT(combined_R, iteratorsComp_sym);
    int * modelUsesExposure = LOGICAL(GET_SLOT(combined_R, modelUsesExposure_sym));
    SEXP mappingsFromExposure_R = GET_SLOT(combined_R, mappingsFromExp_sym);
    SEXP mappingsToExposure_R = GET_SLOT(combined_R, mappingsToExp_sym);
    int iExposeUp_r = *INTEGER(GET_SLOT(combined_R, iExposure_sym));
    int iExposeLow_r = *INTEGER(GET_SLOT(combined_R, iExposureOther_sym));
    int * isIncrementVec = LOGICAL(GET_SLOT(combined_R, isIncrement_sym));
    int diff = *INTEGER(GET_SLOT(combined_R, diffProp_sym));
    int iComp_r = *INTEGER(GET_SLOT(combined_R, iComp_sym));
    int iOrigDest_r = *INTEGER(GET_SLOT(combined_R, iOrigDest_sym));
    int iPool_r = *INTEGER(GET_SLOT(combined_R, iPool_sym));
    int iBirths_r = *INTEGER(GET_SLOT(combined_R, iBirths_sym));
    double * exposure = REAL(GET_SLOT(combined_R, exposure_sym));
    SEXP systemModels_R = GET_SLOT(combined_R, systemModels_sym);
    double ageTimeStep = *REAL(GET_SLOT(combined_R, ageTimeStep_sym));
    int isIncrement = isIncrementVec[iComp_r - 1];
    int diffUp = diff;
    int diffLow = -1 * diff;
    if (!isIncrement) {
        diffUp = -1 * diffUp.;
        diffLow = -1 * diffLow;
    }
    int updatedPopnFalse = 0;
    int updatedBirthsFalse = 0;
    int firstOnlyTrue = 1;
    double ans = 0;
    for (int i = 0; i < nComponents; ++i) {
        int thisModelUsesExposure = modelUsesExposure[i + 1];
        if (thisModelUsesExposure) {
            SEXP component_R = VECTOR_ELT(components_R, i);
            SEXP thisSystemModel_R = VECTOR_ELT(systemModels_R, i+1);
            double * theta = REAL(GET_SLOT(thisSystemModel_R, theta_sym));
            int * strucZeroArray = INTEGER(GET_SLOT(thisSystemModel_R, strucZeroArray_sym));
            SEXP iteratorComp_R = VECTOR_ELT(iteratorsComp_R, i);
            SEXP mappingFromExposure_R = VECTOR_ELT(mappingsFromExposure_R, i);
            int isOrigDest = (i == iOrigDest_r - 1);
            int isPool = (i == iPool_r - 1);
            int isBirths = (i == iBirths_r - 1);
            int iCellUp_r;
            int iCellLow_r;
            int iExpFirstUp_r;
            int iExpFirstLow_r;
            double diffLogUp;
            double diffLogLow;
            if (isBirths) {
                iCellUp_r = getICellBirthsFromExp(iExposeUp_r, mappingFromExposure_R);
                int cellIsAffected = (iCellUp_r > 0);
                if (!cellIsAffected)
                    continue;
                iCellLow_r = getICellBirthsFromExp(iExposeLow_r, mappingFromExposure_R);
                SEXP mappingToExposure_R = VECTOR_ELT(mappingsToExposure_R, i);
                iExpFirstUp_r = getIExposureFromBirths(iCellUp_r, mappingToExposure_R);
                iExpFirstLow_r = getIExposureFromBirths(iCellLow_r, mappingToExposure_R);
            }
            else {
                iCellUp_r = getICellCompFromExp(iExposeUp_r,
                                                mappingFromExposure_R);
                iCellLow_r = getICellCompFromExp(iExposeLow_r,
                                                 mappingFromExposure_R);
                iExpFirstUp_r = iExposeUp_r;
                iExpFirstLow_r = iExposeLow_r;
            }
            if (isOrigDest || isPool || isBirths) {
                diffLogUp = diffLogDensExpOneOrigDestParChPool(iCellUp_r,
                                                             hasAge, ageTimeStep,
                                                             updatedPopnFalse,
                                                             updatedBirthsFalse,
                                                             component_R, theta,
                                                             strucZeroArray,
                                                             iteratorComp_R,
                                                             iExpFirst_r,
                                                             exposure,
                                                             iteratorExposure_R,
                                                             diff,
                                                             firstOnlyTrue);
                diffLogLow = diffLogDensExpOneOrigDestParChPool(iCellLow_r,
                                                             hasAge, ageTimeStep,
                                                             updatedPopnFalse,
                                                             updatedBirthsFalse,
                                                             component_R, theta,
                                                             strucZeroArray,
                                                             iteratorComp_R,
                                                             iExpFirst_r,
                                                             exposure,
                                                             iteratorExposure_R,
                                                             diff,
                                                             firstOnlyTrue);
            }
            else {
                diffLogUp = diffLogDensExpOneComp(iCellUp_r,
                                                  hasAge, ageTimeStep,
                                                  updatedPopnFalse,
                                                  updatedBirthsFalse,
                                                  component_R, theta,
                                                  strucZeroArray,
                                                  iteratorComp_R,
                                                  iExpFirst_r,
                                                  exposure,
                                                  iteratorExposure_R,
                                                  diff,
                                                  firstOnlyTrue);
                diffLogLow = diffLogDensExpOneComp(iCellLow_r,
                                                  hasAge, ageTimeStep,
                                                  updatedPopnFalse,
                                                  updatedBirthsFalse,
                                                  component_R, theta,
                                                  strucZeroArray,
                                                  iteratorComp_R,
                                                  iExpFirst_r,
                                                  exposure,
                                                  iteratorExposure_R,
                                                  diff,
                                                  firstOnlyTrue);
            }
            if (R_finite(diffLogUp)) {
                ans += diffLogUp;
            }
            else {
                ans = diffLogUp;
                break;
            }
            if (R_finite(diffLogLow)) {
                ans += diffLogLow;
            }
            else {
                ans = diffLogLow;
                break;
            }
        } /* end modelUsesExposure */
    } /* end for loop through components */
    return ans;
}
