#include "mapping-functions.h"
#include "helper-functions.h"
#include "demest.h"

/* File "update-accounts.c" contains C versions of functions
 * from "update-accounts.R". */


void
updateAccount(SEXP object_R)
{
    int nCellAccount = *INTEGER(GET_SLOT(object_R, nCellAccount_sym));
    double scaleNoise = *REAL(GET_SLOT(object_R, scaleNoise_sym));
    for (int i = 0; i < (2 * nCellAccount); ++i) {
        updateProposalAccount(object_R);
        int generatedNewProposal = *LOGICAL(GET_SLOT(object_R, generatedNewProposal_sym));

        if(generatedNewProposal) {
            double diffLogLik = diffLogLikAccount(object_R);
            double diffLogDens = diffLogDensAccount(object_R);
            int isInvalid = (!R_finite(diffLogLik)
                   && !R_finite(diffLogDens)
                   && ((diffLogLik > diffLogDens) || (diffLogLik < diffLogDens)));
            if (!isInvalid) {
                double log_r = diffLogLik + diffLogDens;
                if (scaleNoise > 0) {
                    log_r += scaleNoise * rt(1);
                }
                int accept = ( log_r > 0 ) || ( runif(0,1) < exp(log_r) );
                if (accept) {
                    updateValuesAccount(object_R);
                }
            }
        }
    }
}

/* ****************** Updating proposals *************************** */

void
updateProposalAccountMovePopn(SEXP combined_R)
{
    SEXP account_R = GET_SLOT(combined_R, account_sym);
    SEXP population_R = GET_SLOT(account_R, population_sym);
    int maxAttempt = *INTEGER(GET_SLOT(combined_R, maxAttempt_sym));
    int hasAge = *LOGICAL(GET_SLOT(combined_R, hasAge_sym));
    
    SEXP accession_R;
    SEXP iteratorAcc_R;

    SEXP descriptions_R = GET_SLOT(combined_R, descriptions_sym);
    SEXP description_R = VECTOR_ELT(descriptions_R, 0);

    SEXP iteratorPopn_R = GET_SLOT(combined_R, iteratorPopn_sym);

    SEXP systemModels_R = GET_SLOT(combined_R, systemModels_sym);
    SEXP thisSystemModel_R = VECTOR_ELT(systemModels_R, 0);
    double * theta = REAL(GET_SLOT(thisSystemModel_R, theta_sym));
    int * strucZeroArray = INTEGER(GET_SLOT(thisSystemModel_R, strucZeroArray_sym));

    int iCell_r = 0;
    int iCell = 0;
    int generatedNewProposal = 0;

    for (int i = 0; i < maxAttempt; i++) {
        iCell_r = chooseICellPopn(description_R);
        iCell = iCell_r - 1;
        int isStrucZero = strucZeroArray[iCell] == 0;
        if (!isStrucZero) {
            generatedNewProposal = 1;
            break;
        }
    }

    int iExposure_r = 0;
    int iExpFirst_r = getIExpFirstFromPopn(iCell_r, description_R);
    int iPopnNext_r = getIPopnNextFromPopn(iCell_r, description_R);

    int minVal;
    if (hasAge) {
      accession_R = GET_SLOT(combined_R, accession_sym);
      iteratorAcc_R = GET_SLOT(combined_R, iteratorAcc_sym);
      int isOldestAgeGroupValue = isOldestAgeGroup(iCell_r, description_R);
      if (isOldestAgeGroupValue)
	minVal = NA_INTEGER;
      else
	minVal = getMinValCohortPopulationHasAge(iPopnNext_r, population_R,
						 accession_R, iteratorPopn_R);
    }
    else
      minVal = getMinValCohortPopulationNoAge(iPopnNext_r, population_R,
                                                        iteratorPopn_R);
      

    int iAccNext_r = 0;

    if (hasAge) {

        iAccNext_r = getIAccNextFromPopn(iCell_r, description_R);

        int hasLaterAccession = (iAccNext_r > 0);

        if (hasLaterAccession) {

            int minAcc = getMinValCohortAccession(iAccNext_r, accession_R,
                                                        iteratorAcc_R);
            if ((minVal == NA_INTEGER) || (minAcc < minVal)) {
                minVal = minAcc;
            }
        }
    } /* end hasAge */

    int * population = INTEGER(population_R);
    int diffProp = 0;

    if (generatedNewProposal) {

        int valCurr = population[iCell];
	int lower = valCurr - minVal;
        int upper = NA_INTEGER;

        double lambda = theta[iCell];
        int valProp = rpoisTrunc1(lambda, lower, upper, maxAttempt);

        int foundValue = !(valProp == NA_INTEGER);

        if(foundValue) {
            diffProp = valProp - valCurr;
            generatedNewProposal = (diffProp != 0);
        }
        else {
            generatedNewProposal = 0;
        }

    }

    SET_LOGICALSCALE_SLOT(combined_R, generatedNewProposal_sym, generatedNewProposal);
    SET_LOGICALSCALE_SLOT(combined_R, isSmallUpdate_sym, 0);
    SET_LOGICALSCALE_SLOT(combined_R, isSmallUpdateFinal_sym, 0);

    if (!generatedNewProposal) {
         iCell_r = NA_INTEGER;
         iPopnNext_r = NA_INTEGER;
         iAccNext_r = NA_INTEGER;
         iExposure_r = NA_INTEGER;
         iExpFirst_r = NA_INTEGER;
         diffProp = NA_INTEGER;
    }

    SET_INTSCALE_SLOT(combined_R, iCell_sym, iCell_r);
    SET_INTSCALE_SLOT(combined_R, iCellOther_sym, NA_INTEGER);
    SET_INTSCALE_SLOT(combined_R, iPopnNext_sym, iPopnNext_r);
    SET_INTSCALE_SLOT(combined_R, iPopnNextOther_sym, NA_INTEGER);

    if (hasAge) {
        SET_INTSCALE_SLOT(combined_R, iAccNext_sym, iAccNext_r);
        SET_INTSCALE_SLOT(combined_R, iAccNextOther_sym, NA_INTEGER);
        SET_LOGICALSCALE_SLOT(combined_R, isLowerTriangle_sym, NA_LOGICAL);
    }

    SET_INTSCALE_SLOT(combined_R, iExposure_sym, iExposure_r);
    SET_INTSCALE_SLOT(combined_R, iExposureOther_sym, NA_INTEGER);
    SET_INTSCALE_SLOT(combined_R, iExpFirst_sym, iExpFirst_r);
    SET_INTSCALE_SLOT(combined_R, iExpFirstOther_sym, NA_INTEGER);
    SET_INTSCALE_SLOT(combined_R, diffProp_sym, diffProp);
}

void
updateProposalAccountMoveBirths(SEXP combined_R)
{
    SEXP account_R = GET_SLOT(combined_R, account_sym);
    SEXP population_R = GET_SLOT(account_R, population_sym);
    int iComp_r = *INTEGER(GET_SLOT(combined_R, iComp_sym));
    int iComp = iComp_r - 1;

    SEXP components_R = GET_SLOT(account_R, components_sym);
    SEXP component_R = VECTOR_ELT(components_R, iComp);

    int maxAttempt = *INTEGER(GET_SLOT(combined_R, maxAttempt_sym));
    int hasAge = *LOGICAL(GET_SLOT(combined_R, hasAge_sym));

    SEXP accession_R;
    SEXP iteratorAcc_R;

    SEXP mappingsToPopn_R = GET_SLOT(combined_R, mappingsToPopn_sym);
    SEXP mappingToPopn_R = VECTOR_ELT(mappingsToPopn_R, iComp);

    SEXP iteratorPopn_R = GET_SLOT(combined_R, iteratorPopn_sym);

    int * usesExposureVec = LOGICAL(GET_SLOT(combined_R, modelUsesExposure_sym));
    int usesExposure = usesExposureVec[iComp + 1];

    SEXP mappingsToExp_R = GET_SLOT(combined_R, mappingsToExp_sym);
    SEXP mappingToExp_R = VECTOR_ELT(mappingsToExp_R, iComp);

    SEXP descriptions_R = GET_SLOT(combined_R, descriptions_sym);
    SEXP description_R = VECTOR_ELT(descriptions_R, iComp + 1);

    SEXP systemModels_R = GET_SLOT(combined_R, systemModels_sym);
    SEXP thisSystemModel_R = VECTOR_ELT(systemModels_R, iComp + 1);
    double * theta = REAL(GET_SLOT(thisSystemModel_R, theta_sym));
    int * strucZeroArray = INTEGER(GET_SLOT(thisSystemModel_R, strucZeroArray_sym));

    int iCell_r = 0;
    int iCell = 0;
    int generatedNewProposal = 0;

    for (int i = 0; i < maxAttempt; i++) {
        iCell_r = chooseICellComp(description_R);
        iCell = iCell_r - 1;
        int isStrucZero = strucZeroArray[iCell] == 0;
        if (!isStrucZero) {
            generatedNewProposal = 1;
            break;
        }
    }

    int iExpFirst_r = getIExpFirstFromBirths(iCell_r, mappingToExp_R);
    int iPopnNext_r = getIPopnNextFromComp(iCell_r, mappingToPopn_R);

    int minVal;
    if (hasAge) {
      accession_R = GET_SLOT(combined_R, accession_sym);
      iteratorAcc_R = GET_SLOT(combined_R, iteratorAcc_sym);
      minVal = getMinValCohortPopulationHasAge(iPopnNext_r, population_R,
					       accession_R, iteratorPopn_R);
    }
    else
      minVal = getMinValCohortPopulationNoAge(iPopnNext_r, population_R,
					      iteratorPopn_R);
      
    int iAccNext_r = 0;
    int isLowerTriangleValue = 0;

    if (hasAge) {

        isLowerTriangleValue = isLowerTriangle(iCell_r, description_R);

        SEXP mappingsToAcc_R = GET_SLOT(combined_R, mappingsToAcc_sym);
        SEXP mappingToAcc_R = VECTOR_ELT(mappingsToAcc_R, iComp);

        iAccNext_r = getIAccNextFromComp(iCell_r, mappingToAcc_R);

        int hasLaterAccession = (iAccNext_r > 0);

        if (hasLaterAccession) {

            int minAcc = getMinValCohortAccession(iAccNext_r, accession_R,
                                                        iteratorAcc_R);
            if (minAcc < minVal) {
                minVal = minAcc;
            }
        }
    } /* end hasAge */

    int * component = INTEGER(component_R);
    int diffProp = 0;
    int iExposure_r = 0;

    if (generatedNewProposal) {

        int valCurr = component[iCell];
        int lower = valCurr - minVal;
        int upper = NA_INTEGER;

        double thetaCell = theta[iCell];

        double lambda = thetaCell;

        if(usesExposure) {
            double * expectedExposure = REAL(GET_SLOT(combined_R, expectedExposure_sym));
            iExposure_r = getIExposureFromBirths(iCell_r, mappingToExp_R);
            int iExposure = iExposure_r - 1;
            double expectedExposureCell = expectedExposure[iExposure];
            lambda *= expectedExposureCell;
        }

        int valProp = rpoisTrunc1(lambda, lower, upper, maxAttempt);

        int foundValue = !(valProp == NA_INTEGER);

        /* printf("valCurr %d, valProp %d, thetaCell %f, lambda %f\n", */
        /*        valCurr, valProp, thetaCell, lambda); */

        if(foundValue) {
            diffProp = valProp - valCurr;
            generatedNewProposal = (diffProp != 0);
        }
        else {
            generatedNewProposal = 0;
        }

    } /* end generatedNewProposal */

    SET_LOGICALSCALE_SLOT(combined_R, generatedNewProposal_sym, generatedNewProposal);
    SET_LOGICALSCALE_SLOT(combined_R, isSmallUpdate_sym, 0);
    SET_LOGICALSCALE_SLOT(combined_R, isSmallUpdateFinal_sym, 0);

    if (!generatedNewProposal) {
         iCell_r = NA_INTEGER;
         iPopnNext_r = NA_INTEGER;
         iAccNext_r = NA_INTEGER;
         isLowerTriangleValue = NA_LOGICAL;
         iExposure_r = NA_INTEGER;
         iExpFirst_r = NA_INTEGER;
         diffProp = NA_INTEGER;
    }

    SET_INTSCALE_SLOT(combined_R, iCell_sym, iCell_r);
    SET_INTSCALE_SLOT(combined_R, iCellOther_sym, NA_INTEGER);
    SET_INTSCALE_SLOT(combined_R, iPopnNext_sym, iPopnNext_r);
    SET_INTSCALE_SLOT(combined_R, iPopnNextOther_sym, NA_INTEGER);

    if (hasAge) {
        SET_INTSCALE_SLOT(combined_R, iAccNext_sym, iAccNext_r);
        SET_INTSCALE_SLOT(combined_R, iAccNextOther_sym, NA_INTEGER);
        SET_LOGICALSCALE_SLOT(combined_R, isLowerTriangle_sym, isLowerTriangleValue);
    }

    SET_INTSCALE_SLOT(combined_R, iExposure_sym, iExposure_r);
    SET_INTSCALE_SLOT(combined_R, iExposureOther_sym, NA_INTEGER);
    SET_INTSCALE_SLOT(combined_R, iExpFirst_sym, iExpFirst_r);
    SET_INTSCALE_SLOT(combined_R, iExpFirstOther_sym, NA_INTEGER);
    SET_INTSCALE_SLOT(combined_R, diffProp_sym, diffProp);
}


void
updateProposalAccountMoveBirthsSmall(SEXP combined_R)
{

    SEXP account_R = GET_SLOT(combined_R, account_sym);
    int iComp_r = *INTEGER(GET_SLOT(combined_R, iComp_sym));
    int iComp = iComp_r - 1;

    SEXP components_R = GET_SLOT(account_R, components_sym);
    SEXP component_R = VECTOR_ELT(components_R, iComp);
    int * component = INTEGER(component_R);

    int maxAttempt = *INTEGER(GET_SLOT(combined_R, maxAttempt_sym));

    int * usesExposureVec = LOGICAL(GET_SLOT(combined_R, modelUsesExposure_sym));
    int usesExposure = usesExposureVec[iComp + 1];

    SEXP mappingsToExp_R = GET_SLOT(combined_R, mappingsToExp_sym);
    SEXP mappingToExp_R = VECTOR_ELT(mappingsToExp_R, iComp);

    SEXP descriptions_R = GET_SLOT(combined_R, descriptions_sym);
    SEXP description_R = VECTOR_ELT(descriptions_R, iComp + 1);

    SEXP systemModels_R = GET_SLOT(combined_R, systemModels_sym);
    SEXP thisSystemModel_R = VECTOR_ELT(systemModels_R, iComp + 1);
    int * strucZeroArray = INTEGER(GET_SLOT(thisSystemModel_R, strucZeroArray_sym));

    int i_cell_up_r = 0;
    int i_cell_up = 0;
    int generatedNewProposal = 0;

    for (int i = 0; i < maxAttempt; i++) {
        i_cell_up_r = chooseICellCompUpperTri(description_R);
        i_cell_up = i_cell_up_r - 1;
        int isStrucZero = strucZeroArray[i_cell_up] == 0;
        if (!isStrucZero) {
            generatedNewProposal = 1;
            break;
        }
    }

    int i_cell_low_r = 0;
    int iAccNext_r = 0;
    int isLowerTriangleValue = 0;
    int i_expose_up_r = 0;
    int i_expose_low_r = NA_INTEGER;
    int diff_prop = NA_INTEGER;

    if (generatedNewProposal) {

        i_cell_low_r = getICellLowerTriFromComp(i_cell_up_r, description_R);
        int i_cell_low = i_cell_low_r - 1;
        int val_up_curr = component[i_cell_up];
        int val_low_curr = component[i_cell_low];

        if(usesExposure) {
            i_expose_up_r = getIExposureFromBirths(i_cell_up_r, mappingToExp_R);
            i_expose_low_r = getIExposureFromBirths(i_cell_low_r, mappingToExp_R);
        }

        int size = val_up_curr + val_low_curr;
        int val_up_prop = (int)(runif(0, 1) * (size + 1));
	if (val_up_prop > size) /* paranoia */
	  val_up_prop = size;
        diff_prop = val_up_prop - val_up_curr;
        generatedNewProposal = (diff_prop != 0);
    } /* end generatedNewProposal */

    SET_LOGICALSCALE_SLOT(combined_R, generatedNewProposal_sym, generatedNewProposal);
    SET_LOGICALSCALE_SLOT(combined_R, isSmallUpdate_sym, 1);
    SET_LOGICALSCALE_SLOT(combined_R, isSmallUpdateFinal_sym, 0);

    SET_INTSCALE_SLOT(combined_R, iPopnNext_sym, NA_INTEGER);
    SET_INTSCALE_SLOT(combined_R, iPopnNextOther_sym, NA_INTEGER);
    SET_INTSCALE_SLOT(combined_R, iAccNextOther_sym, NA_INTEGER);
    SET_INTSCALE_SLOT(combined_R, iExpFirst_sym, NA_INTEGER);
    SET_INTSCALE_SLOT(combined_R, iExpFirstOther_sym, NA_INTEGER);

    if (!generatedNewProposal) {
         i_cell_up_r = NA_INTEGER;
         i_cell_low_r = NA_INTEGER;
         iAccNext_r = NA_INTEGER;
         isLowerTriangleValue = NA_LOGICAL;
         i_expose_up_r = NA_INTEGER;
         i_expose_low_r = NA_INTEGER;
	 diff_prop = NA_INTEGER;
    }

    SET_INTSCALE_SLOT(combined_R, iCell_sym, i_cell_up_r);
    SET_INTSCALE_SLOT(combined_R, iCellOther_sym, i_cell_low_r);
    SET_INTSCALE_SLOT(combined_R, iAccNext_sym, iAccNext_r);
    SET_LOGICALSCALE_SLOT(combined_R, isLowerTriangle_sym, isLowerTriangleValue);

    SET_INTSCALE_SLOT(combined_R, iExposure_sym, i_expose_up_r);
    SET_INTSCALE_SLOT(combined_R, iExposureOther_sym, i_expose_low_r);
    SET_INTSCALE_SLOT(combined_R, diffProp_sym, diff_prop);
}


void
updateProposalAccountMoveOrigDest(SEXP combined_R)
{
    SEXP account_R = GET_SLOT(combined_R, account_sym);
    SEXP population_R = GET_SLOT(account_R, population_sym);
    int iComp_r = *INTEGER(GET_SLOT(combined_R, iComp_sym));
    int iComp = iComp_r - 1;

    SEXP components_R = GET_SLOT(account_R, components_sym);
    SEXP component_R = VECTOR_ELT(components_R, iComp);

    int maxAttempt = *INTEGER(GET_SLOT(combined_R, maxAttempt_sym));
    int hasAge = *LOGICAL(GET_SLOT(combined_R, hasAge_sym));

    SEXP accession_R;
    SEXP iteratorAcc_R;

    SEXP mappingsToPopn_R = GET_SLOT(combined_R, mappingsToPopn_sym);
    SEXP mappingToPopn_R = VECTOR_ELT(mappingsToPopn_R, iComp);

    SEXP iteratorPopn_R = GET_SLOT(combined_R, iteratorPopn_sym);

    int * usesExposureVec = LOGICAL(GET_SLOT(combined_R, modelUsesExposure_sym));
    int usesExposure = usesExposureVec[iComp + 1];

    SEXP mappingsToExp_R = GET_SLOT(combined_R, mappingsToExp_sym);
    SEXP mappingToExp_R = VECTOR_ELT(mappingsToExp_R, iComp);

    SEXP descriptions_R = GET_SLOT(combined_R, descriptions_sym);
    SEXP description_R = VECTOR_ELT(descriptions_R, iComp + 1);

    SEXP systemModels_R = GET_SLOT(combined_R, systemModels_sym);
    SEXP thisSystemModel_R = VECTOR_ELT(systemModels_R, iComp + 1);
    double * theta = REAL(GET_SLOT(thisSystemModel_R, theta_sym));
    int * strucZeroArray = INTEGER(GET_SLOT(thisSystemModel_R, strucZeroArray_sym));

    int iCell_r = 0;
    int iCell = 0;
    int generatedNewProposal = 0;

    for (int i = 0; i < maxAttempt; i++) {
        iCell_r = chooseICellComp(description_R);
        iCell = iCell_r - 1;
        int isStrucZero = strucZeroArray[iCell] == 0;
        if (!isStrucZero) {
            generatedNewProposal = 1;
            break;
        }
    }

    int pairArray[2]; /* use for all the pairs */

    int diffProp = 0;
    int iExposure_r = 0;

    getIExpFirstPairFromOrigDestInternal(pairArray, iCell_r, mappingToExp_R);

    int iExpFirstOrig_r = pairArray[0];
    int iExpFirstDest_r = pairArray[1];

    getIPopnNextFromOrigDestInternal(pairArray, iCell_r, mappingToPopn_R);

    int iPopnNextOrig_r = pairArray[0];
    int iPopnNextDest_r = pairArray[1];

    int minValOrig;
    int minValDest;
    int isLowerTriangleValue;
    if (hasAge) {
      accession_R = GET_SLOT(combined_R, accession_sym);
      iteratorAcc_R = GET_SLOT(combined_R, iteratorAcc_sym);
      isLowerTriangleValue = isLowerTriangle(iCell_r, description_R);
      int isOldestAgeGroupValue = isOldestAgeGroup(iCell_r, description_R);
      if (isOldestAgeGroupValue && !isLowerTriangleValue) {
	minValOrig = NA_INTEGER;
	minValDest = NA_INTEGER;
      }
      else {
	minValOrig = getMinValCohortPopulationHasAge(iPopnNextOrig_r, population_R,
						     accession_R, iteratorPopn_R);
	minValDest = getMinValCohortPopulationHasAge(iPopnNextDest_r, population_R,
						     accession_R, iteratorPopn_R);
      }
    }
    else {
      minValOrig = getMinValCohortPopulationNoAge(iPopnNextOrig_r, population_R,
						      iteratorPopn_R);
      minValDest = getMinValCohortPopulationNoAge(iPopnNextDest_r, population_R,
						      iteratorPopn_R);
    }
      
    int iAccNextOrig_r = 0;
    int iAccNextDest_r = 0;



    if (hasAge) {
	
        SEXP mappingsToAcc_R = GET_SLOT(combined_R, mappingsToAcc_sym);
        SEXP mappingToAcc_R = VECTOR_ELT(mappingsToAcc_R, iComp);

        getIAccNextFromOrigDestInternal(pairArray, iCell_r, mappingToAcc_R);

        iAccNextOrig_r = pairArray[0];
        iAccNextDest_r = pairArray[1];

        int hasLaterAccession = (iAccNextOrig_r > 0);

        if (hasLaterAccession) {

            int minAccOrig = getMinValCohortAccession(iAccNextOrig_r,
                                        accession_R, iteratorAcc_R);
            int minAccDest = getMinValCohortAccession(iAccNextDest_r,
                                        accession_R, iteratorAcc_R);
            if ((minValOrig == NA_INTEGER) || (minAccOrig < minValOrig)) {
                minValOrig = minAccOrig;
            }
            if ((minValDest == NA_INTEGER) || (minAccDest < minValDest)) {
                minValDest = minAccDest;
            }
        }
    } /* end hasAge */

    int * component = INTEGER(component_R);


    if (generatedNewProposal) {

        int valCurr = component[iCell];
        int lower = valCurr - minValDest;
        int upper = valCurr + minValOrig;

        double thetaCell = theta[iCell];

        double lambda = thetaCell;

        if(usesExposure) {
            double * expectedExposure = REAL(GET_SLOT(combined_R, expectedExposure_sym));
            iExposure_r = getIExposureFromOrigDest(iCell_r, mappingToExp_R);
            int iExposure = iExposure_r - 1;
            double expectedExposureCell = expectedExposure[iExposure];
            lambda *= expectedExposureCell;
        }

        int foundValue = 0;
        int valProp = 0;

        if (!(lower > upper)) {

            valProp = rpoisTrunc1(lambda, lower, upper, maxAttempt);

            foundValue = !(valProp == NA_INTEGER);
        }

        if(foundValue) {
            diffProp = valProp - valCurr;
            generatedNewProposal = (diffProp != 0);
        }
        else {
            generatedNewProposal = 0;
        }

    }

    SET_LOGICALSCALE_SLOT(combined_R, generatedNewProposal_sym, generatedNewProposal);
    SET_LOGICALSCALE_SLOT(combined_R, isSmallUpdate_sym, 0);
    SET_LOGICALSCALE_SLOT(combined_R, isSmallUpdateFinal_sym, 0);

    if (!generatedNewProposal) {
         iCell_r = NA_INTEGER;
         iPopnNextOrig_r = NA_INTEGER;
         iPopnNextDest_r = NA_INTEGER;
         iAccNextOrig_r = NA_INTEGER;
         iAccNextDest_r = NA_INTEGER;
         isLowerTriangleValue = NA_LOGICAL;
         iExposure_r = NA_INTEGER;
         iExpFirstOrig_r = NA_INTEGER;
         iExpFirstDest_r = NA_INTEGER;
         diffProp = NA_INTEGER;
    }

    SET_INTSCALE_SLOT(combined_R, iCell_sym, iCell_r);
    SET_INTSCALE_SLOT(combined_R, iCellOther_sym, NA_INTEGER);
    SET_INTSCALE_SLOT(combined_R, iPopnNext_sym, iPopnNextOrig_r);
    SET_INTSCALE_SLOT(combined_R, iPopnNextOther_sym, iPopnNextDest_r);

    if (hasAge) {
        SET_INTSCALE_SLOT(combined_R, iAccNext_sym, iAccNextOrig_r);
        SET_INTSCALE_SLOT(combined_R, iAccNextOther_sym, iAccNextDest_r);
        SET_LOGICALSCALE_SLOT(combined_R, isLowerTriangle_sym, isLowerTriangleValue);
    }

    SET_INTSCALE_SLOT(combined_R, iExposure_sym, iExposure_r);
    SET_INTSCALE_SLOT(combined_R, iExposureOther_sym, NA_INTEGER);
    SET_INTSCALE_SLOT(combined_R, iExpFirst_sym, iExpFirstOrig_r);
    SET_INTSCALE_SLOT(combined_R, iExpFirstOther_sym, iExpFirstDest_r);
    SET_INTSCALE_SLOT(combined_R, diffProp_sym, diffProp);
}

void
updateProposalAccountMoveOrigDestSmall(SEXP combined_R)
{
    SEXP account_R = GET_SLOT(combined_R, account_sym);
    int i_comp_r = *INTEGER(GET_SLOT(combined_R, iComp_sym));
    int i_comp = i_comp_r - 1;
    
    SEXP population_R = GET_SLOT(account_R, population_sym);
    int * population = INTEGER(population_R);

    SEXP components_R = GET_SLOT(account_R, components_sym);
    SEXP component_R = VECTOR_ELT(components_R, i_comp);
    int * component = INTEGER(component_R);

    int maxAttempt = *INTEGER(GET_SLOT(combined_R, maxAttempt_sym));

    SEXP accession_R = GET_SLOT(combined_R, accession_sym);
    int * accession = INTEGER(accession_R);

    SEXP mappingsToAcc_R = GET_SLOT(combined_R, mappingsToAcc_sym);
    SEXP mappingToAcc_R = VECTOR_ELT(mappingsToAcc_R, i_comp);

    SEXP mappingsToPopn_R = GET_SLOT(combined_R, mappingsToPopn_sym);
    SEXP mappingToPopn_R = VECTOR_ELT(mappingsToPopn_R, i_comp);

    int * usesExposureVec = LOGICAL(GET_SLOT(combined_R, modelUsesExposure_sym));
    int usesExposure = usesExposureVec[i_comp + 1];

    SEXP mappingsToExp_R = GET_SLOT(combined_R, mappingsToExp_sym);
    SEXP mappingToExp_R = VECTOR_ELT(mappingsToExp_R, i_comp);

    int nAge = *INTEGER(GET_SLOT(mappingToExp_R, nAgeCurrent_sym));
    int stepAge = *INTEGER(GET_SLOT(mappingToExp_R, stepAgeCurrent_sym));

    SEXP descriptions_R = GET_SLOT(combined_R, descriptions_sym);
    SEXP description_R = VECTOR_ELT(descriptions_R, i_comp + 1);

    SEXP systemModels_R = GET_SLOT(combined_R, systemModels_sym);
    SEXP thisSystemModel_R = VECTOR_ELT(systemModels_R, i_comp + 1);
    int * strucZeroArray = INTEGER(GET_SLOT(thisSystemModel_R, strucZeroArray_sym));

    int i_cell_up_r = 0;
    int i_cell_up = 0;
    int generatedNewProposal = 0;
    int is_final_age_group = 0;

    for (int i = 0; i < maxAttempt; i++) {
        i_cell_up_r = chooseICellCompUpperTri(description_R);
        i_cell_up = i_cell_up_r - 1;
        int isStrucZero = (strucZeroArray[i_cell_up] == 0);
        if (!isStrucZero) {
            generatedNewProposal = 1;
            break;
        }
    }

    int i_cell_low_r = 0;
    int i_cell_low = 0;
    int isLowerTriangleValue = 0;
    int i_expose_up_r = 0;
    int i_expose_low_r = 0;
    int i_expose_up_oth_r = 0;
    int i_expose_low_oth_r = 0;

    int pairArray[2]; /* use for all the pairs */

    int diff_prop = 0;

    int i_acc_orig_r = 0;
    int i_acc_dest_r = 0;

    if (generatedNewProposal) {

        i_cell_low_r = getICellLowerTriNextFromComp(i_cell_up_r, description_R);
	i_cell_low = i_cell_low_r - 1;

        getIAccNextFromOrigDestInternal(pairArray, i_cell_up_r, mappingToAcc_R);

        i_acc_orig_r = pairArray[0];
        i_acc_dest_r = pairArray[1];
        int i_acc_orig = i_acc_orig_r - 1;
        int i_acc_dest = i_acc_dest_r - 1;

	int i_age_r = ((i_cell_up / stepAge) % nAge) + 1;
	is_final_age_group = (i_age_r == nAge);

        int val_acc_orig = accession[i_acc_orig];
        int val_acc_dest = accession[i_acc_dest];
        int val_up_curr = component[i_cell_up];
	int val_low_curr = component[i_cell_low];

        if(usesExposure) {
	  getIExpFirstPairFromOrigDestInternal(pairArray, i_cell_up_r, mappingToExp_R);
	  i_expose_up_r = pairArray[0];
	  i_expose_up_oth_r = pairArray[1];	  
	  getIExpFirstPairFromOrigDestInternal(pairArray, i_cell_low_r, mappingToExp_R);
	  i_expose_low_r = pairArray[0];
	  i_expose_low_oth_r = pairArray[1];
        }

	int val_popn_orig = 0;
	int val_popn_dest = 0;
	if (is_final_age_group) {
	  getIPopnNextFromOrigDestInternal(pairArray, i_cell_low_r, mappingToPopn_R);
	  int i_popn_orig_r = pairArray[0];
	  int i_popn_dest_r = pairArray[1];
	  val_popn_orig = population[i_popn_orig_r - 1];
	  val_popn_dest = population[i_popn_dest_r - 1];
	}


	int lower = val_up_curr - val_acc_dest;
	int upper = val_up_curr + val_acc_orig;
	if (is_final_age_group) {
	  if (val_popn_orig < val_acc_orig + val_acc_dest) {
	    lower = val_up_curr - val_popn_orig + val_acc_orig;
	  }
	  if (val_popn_dest < val_acc_orig + val_acc_dest) {
	    upper = val_up_curr + val_popn_dest - val_acc_dest;
	  }
	}

	if (lower < 0)
	  lower = 0;
	if (upper < 0)
	  upper = 0;
	int size = val_up_curr + val_low_curr;
	if (upper > size)
	  upper = size;
	
	int val_up_prop = (int)(runif(0, 1) * (upper - lower + 1)) + lower;
	if (val_up_prop > upper) /* paranoia */
	  val_up_prop = upper;
	diff_prop = val_up_prop - val_up_curr;
	generatedNewProposal = (diff_prop != 0);

    }

    SET_LOGICALSCALE_SLOT(combined_R, generatedNewProposal_sym, generatedNewProposal);
    SET_LOGICALSCALE_SLOT(combined_R, isSmallUpdate_sym, 1);
    SET_LOGICALSCALE_SLOT(combined_R, isSmallUpdateFinal_sym, is_final_age_group);

    SET_INTSCALE_SLOT(combined_R, iPopnNext_sym, NA_INTEGER);
    SET_INTSCALE_SLOT(combined_R, iPopnNextOther_sym, NA_INTEGER);

    if (!generatedNewProposal) {
         i_cell_up_r = NA_INTEGER;
         i_cell_low_r = NA_INTEGER;
         i_acc_orig_r = NA_INTEGER;
         i_acc_dest_r = NA_INTEGER;

         isLowerTriangleValue = NA_LOGICAL;
         i_expose_up_r = NA_INTEGER;
         i_expose_low_r = NA_INTEGER;
         i_expose_up_oth_r = NA_INTEGER;
         i_expose_low_oth_r = NA_INTEGER;
         diff_prop = NA_INTEGER;
    }

    SET_INTSCALE_SLOT(combined_R, iCell_sym, i_cell_up_r);
    SET_INTSCALE_SLOT(combined_R, iCellOther_sym, i_cell_low_r);
    SET_INTSCALE_SLOT(combined_R, iAccNext_sym, i_acc_orig_r);
    SET_INTSCALE_SLOT(combined_R, iAccNextOther_sym, i_acc_dest_r);

    SET_LOGICALSCALE_SLOT(combined_R, isLowerTriangle_sym, isLowerTriangleValue);

    SET_INTSCALE_SLOT(combined_R, iExposure_sym, i_expose_up_r);
    SET_INTSCALE_SLOT(combined_R, iExposureOther_sym, i_expose_low_r);
    SET_INTSCALE_SLOT(combined_R, iExpFirst_sym, i_expose_up_oth_r);
    SET_INTSCALE_SLOT(combined_R, iExpFirstOther_sym, i_expose_low_oth_r);
    SET_INTSCALE_SLOT(combined_R, diffProp_sym, diff_prop);
}


void
updateProposalAccountMovePool(SEXP combined_R)
{
    SEXP account_R = GET_SLOT(combined_R, account_sym);
    SEXP population_R = GET_SLOT(account_R, population_sym);
    int iComp_r = *INTEGER(GET_SLOT(combined_R, iComp_sym));
    int iComp = iComp_r - 1;

    SEXP components_R = GET_SLOT(account_R, components_sym);
    SEXP component_R = VECTOR_ELT(components_R, iComp);

    int maxAttempt = *INTEGER(GET_SLOT(combined_R, maxAttempt_sym));
    int hasAge = *LOGICAL(GET_SLOT(combined_R, hasAge_sym));

    SEXP accession_R;
    SEXP iteratorAcc_R;

    SEXP mappingsToPopn_R = GET_SLOT(combined_R, mappingsToPopn_sym);
    SEXP mappingToPopn_R = VECTOR_ELT(mappingsToPopn_R, iComp);

    SEXP iteratorPopn_R = GET_SLOT(combined_R, iteratorPopn_sym);

    int * usesExposureVec = LOGICAL(GET_SLOT(combined_R, modelUsesExposure_sym));
    int usesExposure = usesExposureVec[iComp + 1];

    SEXP mappingsToExp_R = GET_SLOT(combined_R, mappingsToExp_sym);
    SEXP mappingToExp_R = VECTOR_ELT(mappingsToExp_R, iComp);

    SEXP descriptions_R = GET_SLOT(combined_R, descriptions_sym);
    SEXP description_R = VECTOR_ELT(descriptions_R, iComp + 1);

    SEXP systemModels_R = GET_SLOT(combined_R, systemModels_sym);
    SEXP thisSystemModel_R = VECTOR_ELT(systemModels_R, iComp + 1);
    double * theta = REAL(GET_SLOT(thisSystemModel_R, theta_sym));
    int * strucZeroArray = INTEGER(GET_SLOT(thisSystemModel_R, strucZeroArray_sym));

    int pairArray[2];

    int iCellOut_r = 0;
    int iCellIn_r = 0;
    int iCellOut = 0;
    int iCellIn = 0;
    int generatedNewProposal = 0;

    for (int i = 0; i < maxAttempt; i++) {
        chooseICellOutInPoolInternal(pairArray, description_R);
        iCellOut_r = pairArray[0];
        iCellIn_r = pairArray[1];
        iCellOut = iCellOut_r - 1;
        iCellIn = iCellIn_r - 1;
        int isStrucZero = ((strucZeroArray[iCellOut] == 0)
               || (strucZeroArray[iCellIn] == 0));
        if (!isStrucZero) {
            generatedNewProposal = 1;
            break;
        }
    }

    int iExpFirstOut_r = getIExpFirstFromComp(iCellOut_r, mappingToExp_R);
    int iExpFirstIn_r = getIExpFirstFromComp(iCellIn_r, mappingToExp_R);


    int iPopnNextOut_r = getIPopnNextFromComp(iCellOut_r, mappingToPopn_R);
    int iPopnNextIn_r = getIPopnNextFromComp(iCellIn_r, mappingToPopn_R);

    int isLowerTriangleValue;
    int minValOut;
    int minValIn;
    if (hasAge) {
      accession_R = GET_SLOT(combined_R, accession_sym);
      iteratorAcc_R = GET_SLOT(combined_R, iteratorAcc_sym);
      isLowerTriangleValue = isLowerTriangle(iCellOut_r, description_R);
      int isOldestAgeGroupValue = isOldestAgeGroup(iCellOut_r, description_R);
      if (isOldestAgeGroupValue && !isLowerTriangleValue) {
	minValOut = NA_INTEGER;
	minValIn = NA_INTEGER;
      }
      else {
	minValOut = getMinValCohortPopulationHasAge(iPopnNextOut_r, population_R,
						  accession_R, iteratorPopn_R);
	minValIn = getMinValCohortPopulationHasAge(iPopnNextIn_r, population_R,
						 accession_R, iteratorPopn_R);
      }
    }
    else {
      minValOut = getMinValCohortPopulationNoAge(iPopnNextOut_r, population_R,
						 iteratorPopn_R);
      minValIn = getMinValCohortPopulationNoAge(iPopnNextIn_r, population_R,
						iteratorPopn_R);
    }
    int iAccNextOut_r = 0;
    int iAccNextIn_r = 0;

    if (hasAge) {

        SEXP mappingsToAcc_R = GET_SLOT(combined_R, mappingsToAcc_sym);
        SEXP mappingToAcc_R = VECTOR_ELT(mappingsToAcc_R, iComp);

        iAccNextOut_r = getIAccNextFromComp(iCellOut_r, mappingToAcc_R);
        iAccNextIn_r = getIAccNextFromComp(iCellIn_r, mappingToAcc_R);

        int hasLaterAccession = (iAccNextOut_r > 0);

        if (hasLaterAccession) {

            int minAccOut = getMinValCohortAccession(iAccNextOut_r,
                                        accession_R, iteratorAcc_R);
            int minAccIn = getMinValCohortAccession(iAccNextIn_r,
                                        accession_R, iteratorAcc_R);
            if ((minValOut == NA_INTEGER) || (minAccOut < minValOut)) {
                minValOut = minAccOut;
            }
            if ((minValIn == NA_INTEGER) || (minAccIn < minValIn)) {
                minValIn = minAccIn;
            }
        }
    } /* end hasAge */

    int * component = INTEGER(component_R);
    int diffProp = 0;
    int iExposureOut_r = 0;
    int iExposureIn_r = 0;

    if (generatedNewProposal) {

        int valCurrOut = component[iCellOut];
        int valCurrIn = component[iCellIn];
        int lower = valCurrIn - minValIn;
        int upper = valCurrOut + minValOut;

        double thetaOut = theta[iCellOut];

        double lambdaOut = thetaOut;

        if(usesExposure) {

            double * expectedExposure = REAL(GET_SLOT(combined_R, expectedExposure_sym));
            iExposureOut_r = getIExposureFromComp(iCellOut_r, mappingToExp_R);
            iExposureIn_r = getIExposureFromComp(iCellIn_r, mappingToExp_R);
            int iExposureOut = iExposureOut_r - 1;
            double expectedExposureOut = expectedExposure[iExposureOut];
            lambdaOut *= expectedExposureOut;
        }

        int foundValue = 0;
        int valPropOut = 0;

        if (!(lower > upper)) {

            valPropOut = rpoisTrunc1(lambdaOut, lower, upper, maxAttempt);

            foundValue = !(valPropOut == NA_INTEGER);
        }

        if (foundValue) {
            diffProp = valPropOut - valCurrOut;
            generatedNewProposal = (diffProp != 0);
        }
        else {
            generatedNewProposal = 0;
        }

    }

    SET_LOGICALSCALE_SLOT(combined_R, generatedNewProposal_sym, generatedNewProposal);
    SET_LOGICALSCALE_SLOT(combined_R, isSmallUpdate_sym, 0);
    SET_LOGICALSCALE_SLOT(combined_R, isSmallUpdateFinal_sym, 0);

    if (!generatedNewProposal) {
         iCellOut_r = NA_INTEGER;
         iCellIn_r = NA_INTEGER;
         iPopnNextOut_r = NA_INTEGER;
         iPopnNextIn_r = NA_INTEGER;
         iAccNextOut_r = NA_INTEGER;
         iAccNextIn_r = NA_INTEGER;
         isLowerTriangleValue = NA_LOGICAL;
         iExposureOut_r = NA_INTEGER;
         iExposureIn_r = NA_INTEGER;
         iExpFirstOut_r = NA_INTEGER;
         iExpFirstIn_r = NA_INTEGER;
         diffProp = NA_INTEGER;
    }
    else if (!usesExposure) {
        iExposureIn_r = NA_INTEGER;
    }

    SET_INTSCALE_SLOT(combined_R, iCell_sym, iCellOut_r);
    SET_INTSCALE_SLOT(combined_R, iCellOther_sym, iCellIn_r);
    SET_INTSCALE_SLOT(combined_R, iPopnNext_sym, iPopnNextOut_r);
    SET_INTSCALE_SLOT(combined_R, iPopnNextOther_sym, iPopnNextIn_r);

    if (hasAge) {
        SET_INTSCALE_SLOT(combined_R, iAccNext_sym, iAccNextOut_r);
        SET_INTSCALE_SLOT(combined_R, iAccNextOther_sym, iAccNextIn_r);
        SET_LOGICALSCALE_SLOT(combined_R, isLowerTriangle_sym, isLowerTriangleValue);
    }

    SET_INTSCALE_SLOT(combined_R, iExposure_sym, iExposureOut_r);
    SET_INTSCALE_SLOT(combined_R, iExposureOther_sym, iExposureIn_r);
    SET_INTSCALE_SLOT(combined_R, iExpFirst_sym, iExpFirstOut_r);
    SET_INTSCALE_SLOT(combined_R, iExpFirstOther_sym, iExpFirstIn_r);
    SET_INTSCALE_SLOT(combined_R, diffProp_sym, diffProp);
}


void
updateProposalAccountMoveNet(SEXP combined_R)
{
    SEXP account_R = GET_SLOT(combined_R, account_sym);
    SEXP population_R = GET_SLOT(account_R, population_sym);
    int iComp_r = *INTEGER(GET_SLOT(combined_R, iComp_sym));
    int iComp = iComp_r - 1;

    SEXP components_R = GET_SLOT(account_R, components_sym);
    SEXP component_R = VECTOR_ELT(components_R, iComp);

    /* maxAttempts not used */
    int hasAge = *LOGICAL(GET_SLOT(combined_R, hasAge_sym));

    SEXP accession_R;
    SEXP iteratorAcc_R;

    SEXP mappingsToPopn_R = GET_SLOT(combined_R, mappingsToPopn_sym);
    SEXP mappingToPopn_R = VECTOR_ELT(mappingsToPopn_R, iComp);

    SEXP iteratorPopn_R = GET_SLOT(combined_R, iteratorPopn_sym);

    SEXP mappingsToExp_R = GET_SLOT(combined_R, mappingsToExp_sym);
    SEXP mappingToExp_R = VECTOR_ELT(mappingsToExp_R, iComp);

    SEXP descriptions_R = GET_SLOT(combined_R, descriptions_sym);
    SEXP description_R = VECTOR_ELT(descriptions_R, iComp + 1);

    SEXP systemModels_R = GET_SLOT(combined_R, systemModels_sym);
    SEXP thisSystemModel_R = VECTOR_ELT(systemModels_R, iComp + 1);
    double * theta = REAL(GET_SLOT(thisSystemModel_R, theta_sym));
    double varsigma = *REAL(GET_SLOT(thisSystemModel_R, varsigma_sym));
    double * w = REAL(GET_SLOT(thisSystemModel_R, w_sym));

    int pairArray[2];

    chooseICellSubAddNetInternal(pairArray, description_R);

    int iCellAdd_r = pairArray[0];
    int iCellSub_r = pairArray[1];

    int iExpFirstAdd_r = getIExpFirstFromComp(iCellAdd_r, mappingToExp_R);
    int iExpFirstSub_r = getIExpFirstFromComp(iCellSub_r, mappingToExp_R);

    int iPopnNextAdd_r = getIPopnNextFromComp(iCellAdd_r, mappingToPopn_R);
    int iPopnNextSub_r = getIPopnNextFromComp(iCellSub_r, mappingToPopn_R);

    int isLowerTriangleValue;
    int minValAdd;
    int minValSub;
    if (hasAge) {
      accession_R = GET_SLOT(combined_R, accession_sym);
      iteratorAcc_R = GET_SLOT(combined_R, iteratorAcc_sym);
      isLowerTriangleValue = isLowerTriangle(iCellAdd_r, description_R);
      int isOldestAgeGroupValue = isOldestAgeGroup(iCellAdd_r, description_R);
      if (isOldestAgeGroupValue && !isLowerTriangleValue) {
	minValAdd = NA_INTEGER;
	minValSub = NA_INTEGER;
      }
      else {
	minValAdd = getMinValCohortPopulationHasAge(iPopnNextAdd_r, population_R,
						  accession_R, iteratorPopn_R);
	minValSub = getMinValCohortPopulationHasAge(iPopnNextSub_r, population_R,
						  accession_R, iteratorPopn_R);
      }
    }
    else {
      minValAdd = getMinValCohortPopulationNoAge(iPopnNextAdd_r, population_R,
						 iteratorPopn_R);
      minValSub = getMinValCohortPopulationNoAge(iPopnNextSub_r, population_R,
						 iteratorPopn_R);
    }
    int iAccNextAdd_r = 0;
    int iAccNextSub_r = 0;

    if (hasAge) {

        SEXP mappingsToAcc_R = GET_SLOT(combined_R, mappingsToAcc_sym);
        SEXP mappingToAcc_R = VECTOR_ELT(mappingsToAcc_R, iComp);

        iAccNextAdd_r = getIAccNextFromComp(iCellAdd_r, mappingToAcc_R);
        iAccNextSub_r = getIAccNextFromComp(iCellSub_r, mappingToAcc_R);

        int hasLaterAccession = (iAccNextAdd_r > 0);

        if (hasLaterAccession) {

            int minAccAdd = getMinValCohortAccession(iAccNextAdd_r,
                                        accession_R, iteratorAcc_R);
            int minAccSub = getMinValCohortAccession(iAccNextSub_r,
                                        accession_R, iteratorAcc_R);
            if ((minValAdd == NA_INTEGER) || (minAccAdd < minValAdd)) {
                minValAdd = minAccAdd;
            }
            if ((minValSub == NA_INTEGER) || (minAccSub < minValSub)) {
                minValSub = minAccSub;
            }
        }
    } /* end hasAge */

    int * component = INTEGER(component_R);
    int iCellAdd = iCellAdd_r - 1;
    int iCellSub = iCellSub_r - 1;

    int valCurrAdd = component[iCellAdd];
    int valCurrSub = component[iCellSub];
    int lower = valCurrSub - minValSub;
    int upper = valCurrAdd + minValAdd;

    double meanAdd = theta[iCellAdd];

    int foundValue = 0;
    int valPropAdd = 0;

    if (!(lower > upper)) {

        double wAdd = w[iCellAdd];
        double sdAdd = varsigma/sqrt(wAdd);

        valPropAdd = rnormIntTrunc1(meanAdd, sdAdd, lower, upper);

        foundValue = !(valPropAdd == NA_INTEGER);
    }

    int generatedNewProposal = 0;
    int diffProp = 0;

    if(foundValue) {
        diffProp = valPropAdd - valCurrAdd;
        generatedNewProposal = (diffProp != 0);
    }
    else {
        generatedNewProposal = 0;
    }
    SET_LOGICALSCALE_SLOT(combined_R, generatedNewProposal_sym,
                                    generatedNewProposal);
    SET_LOGICALSCALE_SLOT(combined_R, isSmallUpdate_sym, 0);
    SET_LOGICALSCALE_SLOT(combined_R, isSmallUpdateFinal_sym, 0);

    if (!generatedNewProposal) {
         iCellAdd_r = NA_INTEGER;
         iCellSub_r = NA_INTEGER;
         iPopnNextAdd_r = NA_INTEGER;
         iPopnNextSub_r = NA_INTEGER;
         iAccNextAdd_r = NA_INTEGER;
         iAccNextSub_r = NA_INTEGER;
         isLowerTriangleValue = NA_LOGICAL;
         iExpFirstAdd_r = NA_INTEGER;
         iExpFirstSub_r = NA_INTEGER;
         diffProp = NA_INTEGER;
    }

    SET_INTSCALE_SLOT(combined_R, iCell_sym, iCellAdd_r);
    SET_INTSCALE_SLOT(combined_R, iCellOther_sym, iCellSub_r);
    SET_INTSCALE_SLOT(combined_R, iPopnNext_sym, iPopnNextAdd_r);
    SET_INTSCALE_SLOT(combined_R, iPopnNextOther_sym, iPopnNextSub_r);

    if (hasAge) {
        SET_INTSCALE_SLOT(combined_R, iAccNext_sym, iAccNextAdd_r);
        SET_INTSCALE_SLOT(combined_R, iAccNextOther_sym, iAccNextSub_r);
        SET_LOGICALSCALE_SLOT(combined_R, isLowerTriangle_sym,
                                            isLowerTriangleValue);
    }

    SET_INTSCALE_SLOT(combined_R, iExposure_sym, NA_INTEGER);
    SET_INTSCALE_SLOT(combined_R, iExposureOther_sym, NA_INTEGER);
    SET_INTSCALE_SLOT(combined_R, iExpFirst_sym, iExpFirstAdd_r);
    SET_INTSCALE_SLOT(combined_R, iExpFirstOther_sym, iExpFirstSub_r);
    SET_INTSCALE_SLOT(combined_R, diffProp_sym, diffProp);
}


void
updateProposalAccountMoveComp(SEXP combined_R)
{
    SEXP account_R = GET_SLOT(combined_R, account_sym);
    SEXP population_R = GET_SLOT(account_R, population_sym);
    int iComp_r = *INTEGER(GET_SLOT(combined_R, iComp_sym));
    int iComp = iComp_r - 1;

    SEXP components_R = GET_SLOT(account_R, components_sym);
    SEXP component_R = VECTOR_ELT(components_R, iComp);

    int * isIncrementVec = LOGICAL(GET_SLOT(combined_R, isIncrement_sym));
    int isIncrement = isIncrementVec[iComp];

    int maxAttempt = *INTEGER(GET_SLOT(combined_R, maxAttempt_sym));
    int hasAge = *LOGICAL(GET_SLOT(combined_R, hasAge_sym));

    SEXP accession_R;
    SEXP iteratorAcc_R;

    SEXP mappingsToPopn_R = GET_SLOT(combined_R, mappingsToPopn_sym);
    SEXP mappingToPopn_R = VECTOR_ELT(mappingsToPopn_R, iComp);

    SEXP iteratorPopn_R = GET_SLOT(combined_R, iteratorPopn_sym);

    int * usesExposureVec = LOGICAL(GET_SLOT(combined_R, modelUsesExposure_sym));
    int usesExposure = usesExposureVec[iComp + 1];

    SEXP mappingsToExp_R = GET_SLOT(combined_R, mappingsToExp_sym);
    SEXP mappingToExp_R = VECTOR_ELT(mappingsToExp_R, iComp);

    SEXP descriptions_R = GET_SLOT(combined_R, descriptions_sym);
    SEXP description_R = VECTOR_ELT(descriptions_R, iComp + 1);

    SEXP systemModels_R = GET_SLOT(combined_R, systemModels_sym);
    SEXP thisSystemModel_R = VECTOR_ELT(systemModels_R, iComp + 1);
    double * theta = REAL(GET_SLOT(thisSystemModel_R, theta_sym));

    int * isNetVec = LOGICAL(GET_SLOT(combined_R, isNet_sym));
    int isNet = isNetVec[iComp];

    int * component = INTEGER(component_R);

    int iCell_r = 0;
    int iCell = 0;
    int iExposure_r = 0;
    int generatedNewProposal = 0;
    int diffProp = 0;

    if (isNet) {
      iCell_r = chooseICellComp(description_R);
      generatedNewProposal = 1;
    }
    else {
      generatedNewProposal = 0;
      int * strucZeroArray = INTEGER(GET_SLOT(thisSystemModel_R, strucZeroArray_sym));
      for (int i = 0; i < maxAttempt; i++) {
        iCell_r = chooseICellComp(description_R);
        iCell = iCell_r - 1;
        int isStrucZero = strucZeroArray[iCell] == 0;
        if (!isStrucZero) {
	  generatedNewProposal = 1;
	  break;
        }
      }
    }

    int iExpFirst_r = getIExpFirstFromComp(iCell_r, mappingToExp_R);

    int iPopnNext_r = getIPopnNextFromComp(iCell_r, mappingToPopn_R);

    int isLowerTriangleValue;
    int minVal;
    if (hasAge) {
      accession_R = GET_SLOT(combined_R, accession_sym);
      iteratorAcc_R = GET_SLOT(combined_R, iteratorAcc_sym);
      isLowerTriangleValue = isLowerTriangle(iCell_r, description_R);
      int isOldestAgeGroupValue = isOldestAgeGroup(iCell_r, description_R);
      if (isOldestAgeGroupValue && !isLowerTriangleValue)
	minVal = NA_INTEGER;
      else
	minVal = getMinValCohortPopulationHasAge(iPopnNext_r, population_R,
						 accession_R, iteratorPopn_R);
    }
    else
      minVal = getMinValCohortPopulationNoAge(iPopnNext_r, population_R,
					      iteratorPopn_R);
      

    int iAccNext_r = 0;

    if (hasAge) {

        SEXP mappingsToAcc_R = GET_SLOT(combined_R, mappingsToAcc_sym);
        SEXP mappingToAcc_R = VECTOR_ELT(mappingsToAcc_R, iComp);

        iAccNext_r = getIAccNextFromComp(iCell_r, mappingToAcc_R);

        int hasLaterAccession = (iAccNext_r > 0);

        if (hasLaterAccession) {

            int minAcc = getMinValCohortAccession(iAccNext_r,
                                        accession_R, iteratorAcc_R);
            if ((minVal == NA_INTEGER) || (minAcc < minVal)) {
                minVal = minAcc;
            }
        }
    } /* end hasAge */

    if (generatedNewProposal) {

        int valCurr = component[iCell];

        int lower = NA_INTEGER;
        int upper = valCurr + minVal;

        if (isIncrement) {
            lower = valCurr - minVal;
            upper = NA_INTEGER;
        }

        int valProp = 0;
        if(usesExposure) {

            iExposure_r = getIExposureFromComp(iCell_r, mappingToExp_R);

        }

        if (isNet) {

            double varsigmaComp = *REAL(GET_SLOT(thisSystemModel_R, varsigma_sym));
            double * wComp = REAL(GET_SLOT(thisSystemModel_R, w_sym));

            double mean = theta[iCell];
            double wCell = wComp[iCell];
            double sd = varsigmaComp/sqrt(wCell);
            valProp = rnormIntTrunc1(mean, sd, lower, upper);
        }
        else {

            double thetaCell = theta[iCell];
            double lambda = thetaCell;

            if(usesExposure) {
                double * expectedExposure = REAL(GET_SLOT(combined_R, expectedExposure_sym));
                double expectedExposureCell = expectedExposure[iExposure_r - 1];
                lambda *= expectedExposureCell;
            }

            valProp = rpoisTrunc1(lambda, lower, upper, maxAttempt);
        }

        int foundValue = !(valProp == NA_INTEGER);

        if(foundValue) {
            diffProp = valProp - valCurr;
            generatedNewProposal = (diffProp != 0);
        }
        else {
            generatedNewProposal = 0;
        }

    }

    SET_LOGICALSCALE_SLOT(combined_R, generatedNewProposal_sym, generatedNewProposal);
    SET_LOGICALSCALE_SLOT(combined_R, isSmallUpdate_sym, 0);
    SET_LOGICALSCALE_SLOT(combined_R, isSmallUpdateFinal_sym, 0);

    if (!generatedNewProposal) {
         iCell_r = NA_INTEGER;
         iPopnNext_r = NA_INTEGER;
         iAccNext_r = NA_INTEGER;
         isLowerTriangleValue = NA_LOGICAL;
         iExposure_r = NA_INTEGER;
         iExpFirst_r = NA_INTEGER;
         diffProp = NA_INTEGER;
    }

    SET_INTSCALE_SLOT(combined_R, iCell_sym, iCell_r);
    SET_INTSCALE_SLOT(combined_R, iCellOther_sym, NA_INTEGER);
    SET_INTSCALE_SLOT(combined_R, iPopnNext_sym, iPopnNext_r);
    SET_INTSCALE_SLOT(combined_R, iPopnNextOther_sym, NA_INTEGER);

    if (hasAge) {
        SET_INTSCALE_SLOT(combined_R, iAccNext_sym, iAccNext_r);
        SET_INTSCALE_SLOT(combined_R, iAccNextOther_sym, NA_INTEGER);
        SET_LOGICALSCALE_SLOT(combined_R, isLowerTriangle_sym, isLowerTriangleValue);
    }

    SET_INTSCALE_SLOT(combined_R, iExposure_sym, iExposure_r);
    SET_INTSCALE_SLOT(combined_R, iExposureOther_sym, NA_INTEGER);
    SET_INTSCALE_SLOT(combined_R, iExpFirst_sym, iExpFirst_r);
    SET_INTSCALE_SLOT(combined_R, iExpFirstOther_sym, NA_INTEGER);
    SET_INTSCALE_SLOT(combined_R, diffProp_sym, diffProp);
}


void
updateProposalAccountMoveCompSmall(SEXP combined_R)
{
    SEXP account_R = GET_SLOT(combined_R, account_sym);
    int i_comp_r = *INTEGER(GET_SLOT(combined_R, iComp_sym));
    int i_comp = i_comp_r - 1;

    SEXP population_R = GET_SLOT(account_R, population_sym);
    int * population = INTEGER(population_R);
    
    SEXP components_R = GET_SLOT(account_R, components_sym);
    SEXP component_R = VECTOR_ELT(components_R, i_comp);
    int * component = INTEGER(component_R);

    int * isIncrementVec = LOGICAL(GET_SLOT(combined_R, isIncrement_sym));
    int isIncrement = isIncrementVec[i_comp];

    int maxAttempt = *INTEGER(GET_SLOT(combined_R, maxAttempt_sym));

    SEXP accession_R = GET_SLOT(combined_R, accession_sym);
    int * accession = INTEGER(accession_R);

    SEXP mappingsToAcc_R = GET_SLOT(combined_R, mappingsToAcc_sym);
    SEXP mappingToAcc_R = VECTOR_ELT(mappingsToAcc_R, i_comp);

    SEXP mappingsToPopn_R = GET_SLOT(combined_R, mappingsToPopn_sym);
    SEXP mappingToPopn_R = VECTOR_ELT(mappingsToPopn_R, i_comp);

    int * usesExposureVec = LOGICAL(GET_SLOT(combined_R, modelUsesExposure_sym));
    int usesExposure = usesExposureVec[i_comp + 1];

    SEXP mappingsToExp_R = GET_SLOT(combined_R, mappingsToExp_sym);
    SEXP mappingToExp_R = VECTOR_ELT(mappingsToExp_R, i_comp);

    int nAge = *INTEGER(GET_SLOT(mappingToExp_R, nAgeCurrent_sym));
    int stepAge = *INTEGER(GET_SLOT(mappingToExp_R, stepAgeCurrent_sym));

    SEXP descriptions_R = GET_SLOT(combined_R, descriptions_sym);
    SEXP description_R = VECTOR_ELT(descriptions_R, i_comp + 1);

    SEXP systemModels_R = GET_SLOT(combined_R, systemModels_sym);
    SEXP systemModelComp_R = VECTOR_ELT(systemModels_R, i_comp + 1);

    int * strucZeroArray = INTEGER(GET_SLOT(systemModelComp_R, strucZeroArray_sym));

    int i_cell_up_r = 0;
    int i_cell_up = 0;
    int generatedNewProposal = 0;
    int diff_prop = 0;
    int is_final_age_group = 0;

    for (int i = 0; i < maxAttempt; i++) {
        i_cell_up_r = chooseICellCompUpperTri(description_R);
        i_cell_up = i_cell_up_r - 1;
        int isStrucZero = (strucZeroArray[i_cell_up] == 0);
        if (!isStrucZero) {
            generatedNewProposal = 1;
            break;
        }
    }

    int i_cell_low_r = 0;
    int i_acc_r = 0;
    int isLowerTriangleValue = 0;
    int i_expose_up_r = 0;
    int i_expose_low_r = NA_INTEGER;

    if (generatedNewProposal) {

        i_cell_low_r = getICellLowerTriNextFromComp(i_cell_up_r, description_R);
        int i_cell_low = i_cell_low_r - 1;
        i_acc_r = getIAccNextFromComp(i_cell_up_r, mappingToAcc_R);
        int i_acc = i_acc_r - 1;

	int i_age_r = ((i_cell_up / stepAge) % nAge) + 1;
	is_final_age_group = (i_age_r == nAge);
	int val_acc = accession[i_acc];
        int val_up_curr = component[i_cell_up];
        int val_low_curr = component[i_cell_low];

        if (usesExposure) {
            i_expose_up_r = getIExposureFromComp(i_cell_up_r, mappingToExp_R);
            i_expose_low_r = getIExposureFromComp(i_cell_low_r, mappingToExp_R);
        }

	int val_popn;
	if (is_final_age_group) {
	  int i_popn_r = getIPopnNextFromComp(i_cell_low_r, mappingToPopn_R);
	  val_popn = population[i_popn_r - 1];
	}
       
	int lower = 0;
	int lower_alt = lower;
	int upper = val_up_curr + val_low_curr;
	int upper_alt = upper;
	
	if (isIncrement) {
	  lower_alt = val_up_curr - val_acc;
	  if (is_final_age_group) {
            upper_alt = val_up_curr + val_popn - val_acc;
	  }
        }
        else {
	  if (is_final_age_group) {
            lower_alt = val_up_curr - val_popn + val_acc;
	  }
	  upper_alt = val_up_curr + val_acc;
	}

	if (lower_alt > lower)
	  lower = lower_alt;
	if (upper_alt < upper)
	  upper = upper_alt;

	int val_up_prop = (int)(runif(0, 1) * (upper - lower + 1) + lower);
	if (val_up_prop > upper) /* paranoia */
	  val_up_prop = upper;
	diff_prop = val_up_prop - val_up_curr;
	generatedNewProposal = (diff_prop != 0);

    } /* end generatedNewProposal */

    SET_LOGICALSCALE_SLOT(combined_R, generatedNewProposal_sym, generatedNewProposal);
    SET_LOGICALSCALE_SLOT(combined_R, isSmallUpdate_sym, 1);
    SET_LOGICALSCALE_SLOT(combined_R, isSmallUpdateFinal_sym, is_final_age_group);

    SET_INTSCALE_SLOT(combined_R, iPopnNext_sym, NA_INTEGER);
    SET_INTSCALE_SLOT(combined_R, iPopnNextOther_sym, NA_INTEGER);
    SET_INTSCALE_SLOT(combined_R, iAccNextOther_sym, NA_INTEGER);
    SET_INTSCALE_SLOT(combined_R, iExpFirst_sym, NA_INTEGER);
    SET_INTSCALE_SLOT(combined_R, iExpFirstOther_sym, NA_INTEGER);

    if (!generatedNewProposal) {
         i_cell_up_r = NA_INTEGER;
         i_cell_low_r = NA_INTEGER;
         i_acc_r = NA_INTEGER;
         isLowerTriangleValue = NA_LOGICAL;
         i_expose_up_r = NA_INTEGER;
         i_expose_low_r = NA_INTEGER;
         diff_prop = NA_INTEGER;
    }

    SET_INTSCALE_SLOT(combined_R, iCell_sym, i_cell_up_r);
    SET_INTSCALE_SLOT(combined_R, iCellOther_sym, i_cell_low_r);
    SET_INTSCALE_SLOT(combined_R, iAccNext_sym, i_acc_r);
    SET_LOGICALSCALE_SLOT(combined_R, isLowerTriangle_sym, isLowerTriangleValue);

    SET_INTSCALE_SLOT(combined_R, iExposure_sym, i_expose_up_r);
    SET_INTSCALE_SLOT(combined_R, iExposureOther_sym, i_expose_low_r);
    SET_INTSCALE_SLOT(combined_R, diffProp_sym, diff_prop);
}


/* ******************** Log-Likelihood ********************** */

double
diffLogLikAccountMovePopn(SEXP combined_R)
{
    SEXP account_R = GET_SLOT(combined_R, account_sym);
    SEXP population_R = GET_SLOT(account_R, population_sym);
    SEXP iterator_R = GET_SLOT(combined_R, iteratorPopn_sym);
    SEXP dataModels_R = GET_SLOT(combined_R, dataModels_sym);
    SEXP datasets_R = GET_SLOT(combined_R, datasets_sym);
    SEXP seriesIndices_R = GET_SLOT(combined_R, seriesIndices_sym);
    SEXP transforms_R = GET_SLOT(combined_R, transforms_sym);
    int iCell = *INTEGER(GET_SLOT(combined_R, iCell_sym));
    int diff = *INTEGER(GET_SLOT(combined_R, diffProp_sym));

    double ans = diffLogLikPopn(diff, iCell, iterator_R,
                        population_R, dataModels_R, datasets_R,
                        seriesIndices_R, transforms_R);
    return ans;
}


double
diffLogLikPopn(int diff, int iFirst_r, SEXP iterator_R,
                        SEXP population_R, SEXP dataModels_R,
                        SEXP datasets_R, SEXP seriesIndices_R,
                        SEXP transforms_R)
{
    double ans = 0;
    int * seriesIndices = INTEGER(seriesIndices_R);

    int nDatasets = LENGTH(datasets_R);

    for (int iDataset = 0; iDataset < nDatasets; ++iDataset) {

        int assocWithPopn = (seriesIndices[iDataset] == 0);

        if (assocWithPopn) {
            SEXP model_R = VECTOR_ELT(dataModels_R, iDataset);
            SEXP dataset_R = VECTOR_ELT(datasets_R, iDataset);
            SEXP transform_R = VECTOR_ELT(transforms_R, iDataset);

            double diffLogLik = diffLogLikPopnOneDataset(diff, iFirst_r,
                                    iterator_R, population_R,
                                    model_R, dataset_R, transform_R);
            if (R_finite(diffLogLik) ) {
                ans += diffLogLik;
            }
            else { /* infinite */
                ans = diffLogLik;
                break; /* break out of for loop */
            }
        }
    }

    return ans;
}

double
diffLogLikPopnOneDataset(int diff, int iFirst_r, SEXP iterator_R,
                        SEXP population_R, SEXP model_R,
                        SEXP dataset_R, SEXP transform_R)
{
    double ans = 0;
    int iAfter_r = dembase_getIAfter(iFirst_r, transform_R);

    if (iAfter_r > 0) {
        ans = diffLogLikPopnOneCell(iAfter_r, diff, population_R,
                                    model_R, dataset_R, transform_R);
    }
    resetCP(iterator_R, iFirst_r);

    int * finished_ptr = LOGICAL(GET_SLOT(iterator_R, finished_sym));
    int finished = *finished_ptr;
    int * i_ptr = INTEGER(GET_SLOT(iterator_R, i_sym));

    while ( !finished ) {
        advanceCP(iterator_R);
        finished = *finished_ptr;
        int i_r = *i_ptr;

        iAfter_r = dembase_getIAfter(i_r, transform_R);

        if (iAfter_r > 0) {
            ans += diffLogLikPopnOneCell(iAfter_r, diff, population_R,
                                    model_R, dataset_R, transform_R);
        }
    }
    return ans;
}

double
diffLogLikPopnOneCell(int iAfter_r, int diff, SEXP population_R,
                        SEXP model_R, SEXP dataset_R, SEXP transform_R)
{
    double retValue = 0;

    int * dataset = INTEGER(dataset_R);
    int iAfter = iAfter_r - 1;

    int cellHasNoData = ( dataset[iAfter] == NA_INTEGER );

    if (!cellHasNoData) {

        int * population = INTEGER(population_R);

        SEXP vec_iBefore_R = dembase_getIBefore(iAfter_r, transform_R);
        int nBefore = LENGTH(vec_iBefore_R);
        int *vec_iBefore = INTEGER(vec_iBefore_R);

        int totalPopnCurr = 0;

        for (int i = 0; i < nBefore; ++i) {
            int iBefore = vec_iBefore[i] - 1;
            totalPopnCurr += population[iBefore];
        }

        int totalPopnProp = totalPopnCurr + diff;

        double logLikProp = logLikelihood(model_R, totalPopnProp, dataset_R, iAfter_r);
        double logLikCurr = logLikelihood(model_R, totalPopnCurr, dataset_R, iAfter_r);

        if(R_finite(logLikProp) || R_finite(logLikCurr)) {
            retValue = logLikProp - logLikCurr;
        }
        else {
            double diffProp = abs(totalPopnProp - dataset[iAfter]);
            double diffCurr = abs(totalPopnCurr - dataset[iAfter]);
            if (diffProp > diffCurr) {
                retValue = -1000000;
            }
            else {
                retValue = 1000000;
            }
        }
    }
    /* if cellHasNoData, retValue stays at default value */

    return retValue;
}


double
diffLogLikAccountMoveOrigDest(SEXP combined_R)
{
    SEXP account_R = GET_SLOT(combined_R, account_sym);
    int iComp_r = *INTEGER(GET_SLOT(combined_R, iComp_sym));

    SEXP component_R = VECTOR_ELT(GET_SLOT(account_R, components_sym), iComp_r - 1);

    SEXP population_R = GET_SLOT(account_R, population_sym);
    SEXP iterator_R = GET_SLOT(combined_R, iteratorPopn_sym);
    SEXP dataModels_R = GET_SLOT(combined_R, dataModels_sym);
    SEXP datasets_R = GET_SLOT(combined_R, datasets_sym);
    SEXP seriesIndices_R = GET_SLOT(combined_R, seriesIndices_sym);
    SEXP transforms_R = GET_SLOT(combined_R, transforms_sym);
    int iCell_r = *INTEGER(GET_SLOT(combined_R, iCell_sym));
    int iPopnOrig_r = *INTEGER(GET_SLOT(combined_R, iPopnNext_sym));
    int iPopnDest_r = *INTEGER(GET_SLOT(combined_R, iPopnNextOther_sym));
    int diff = *INTEGER(GET_SLOT(combined_R, diffProp_sym));
    int diffOrig = -1 * diff;
    int diffDest = diff;

    double ans = 0;

    double diffLogLikCell = diffLogLikCellComp( diff, iComp_r, iCell_r,
                                        component_R,
                                        dataModels_R, datasets_R,
                                        seriesIndices_R, transforms_R);
    if (R_finite(diffLogLikCell) ) {

        ans += diffLogLikCell;

        double diffLogLikPopn = diffLogLikPopnPair(diffOrig, diffDest,
                                iPopnOrig_r, iPopnDest_r,
                                iterator_R, population_R,
                                dataModels_R, datasets_R,
                                seriesIndices_R, transforms_R);

        if (R_finite(diffLogLikPopn) ) {
            ans += diffLogLikPopn;

        }
        else { /* infinite */
            ans = diffLogLikPopn;
        }
    }
    else { /* infinite */
        ans = diffLogLikCell;
    }

    return ans;
}


double
diffLogLikCellComp(int diff, int iComp_r, int iCell_r,
                        SEXP component_R, SEXP dataModels_R,
                        SEXP datasets_R, SEXP seriesIndices_R,
                        SEXP transforms_R)
{
    double ans = 0;

    int nDatasets = LENGTH(datasets_R);
    int * seriesIndices = INTEGER(seriesIndices_R);

    for(int iDataset = 0; iDataset < nDatasets; ++iDataset) {

        int assocWithComp = ( seriesIndices[iDataset] == iComp_r );

        if (assocWithComp) {

            SEXP model_R = VECTOR_ELT(dataModels_R, iDataset);
            SEXP dataset_R = VECTOR_ELT(datasets_R, iDataset);
            SEXP transform_R = VECTOR_ELT(transforms_R, iDataset);

            double diffDataset = diffLogLikCellOneDataset( diff,
                                    iCell_r, component_R,
                                    model_R, dataset_R, transform_R);
            if (R_finite(diffDataset) ) {

                ans += diffDataset;

            }
            else { /* infinite */
                ans = diffDataset;
                break; /* break out of for loop */
            }
        }
    }

    return ans;
}

double
diffLogLikCellOneDataset(int diff, int iCell_r, SEXP component_R,
                        SEXP model_R, SEXP dataset_R, SEXP transform_R)
{
    double ans = 0;

    int iAfter_r = dembase_getIAfter(iCell_r, transform_R);

    int * dataset = INTEGER(dataset_R);
    int iAfter = iAfter_r - 1;

    int cellHasNoData = ( (iAfter_r == 0 ) ||
                                    ( dataset[iAfter] == NA_INTEGER ) );

    if (!cellHasNoData) {

        int * component = INTEGER(component_R);

        SEXP vec_iBefore_R = dembase_getIBefore(iAfter_r, transform_R);
        int nBefore = LENGTH(vec_iBefore_R);
        int *vec_iBefore = INTEGER(vec_iBefore_R);

        int totalCompCurr = 0;

        for (int i = 0; i < nBefore; ++i) {
            int iBefore = vec_iBefore[i] - 1;
            totalCompCurr += component[iBefore];
        }

        int totalCompProp = totalCompCurr + diff;

        double logLikProp = logLikelihood(model_R, totalCompProp,
                      dataset_R, iAfter_r);
        double logLikCurr = logLikelihood(model_R, totalCompCurr,
                      dataset_R, iAfter_r);

        if (R_finite(logLikProp) || R_finite(logLikCurr)) {
            ans = logLikProp - logLikCurr;
        }
        else {
            double diffProp = abs(totalCompProp - dataset[iAfter]);
            double diffCurr = abs(totalCompCurr - dataset[iAfter]);
            if (diffProp > diffCurr) {
                ans = -1000000;
            }
            else {
                ans = 1000000;
            }
        }
    }
    /* if cellHasNoData, retValue stays at default value */

    return ans;
}


double
diffLogLikPopnPair(int diffOrig, int diffDest,
		   int iPopnOrig_r, int iPopnDest_r,
		   SEXP iterator_R,
		   SEXP population_R, SEXP dataModels_R,
		   SEXP datasets_R, SEXP seriesIndices_R,
		   SEXP transforms_R)
{
  SEXP iteratorDest_R = NULL;
  PROTECT(iteratorDest_R = duplicate(iterator_R));
  int * iPopnOrigHere_ptr = INTEGER(GET_SLOT(iterator_R, i_sym));
  int * iPopnDestHere_ptr = INTEGER(GET_SLOT(iteratorDest_R, i_sym));
  int * finished_ptr = LOGICAL(GET_SLOT(iterator_R, finished_sym));
  int finished;
  double ans = 0;
  if (iPopnOrig_r != iPopnDest_r) {
    int nDatasets = LENGTH(datasets_R);
    int * seriesIndices = INTEGER(seriesIndices_R);
    for (int iDataset = 0; iDataset < nDatasets; ++iDataset) {
      int assocWithPopn = ( seriesIndices[iDataset] == 0 );
      if (assocWithPopn) {
	SEXP transform_R = VECTOR_ELT(transforms_R, iDataset);
	SEXP model_R = VECTOR_ELT(dataModels_R, iDataset);
	SEXP dataset_R = VECTOR_ELT(datasets_R, iDataset);
	int iAfterOrig_r = dembase_getIAfter(iPopnOrig_r, transform_R);
	int iAfterDest_r = dembase_getIAfter(iPopnDest_r, transform_R);
	if (iAfterOrig_r != iAfterDest_r) {
	  if (iAfterOrig_r > 0) {
	    double diffLogLikOrig = diffLogLikPopnOneCell(iAfterOrig_r,
							  diffOrig,
							  population_R,  
							  model_R,
							  dataset_R,
							  transform_R);
	    ans += diffLogLikOrig;
	  }
	  if (iAfterDest_r > 0) {
	    double diffLogLikDest = diffLogLikPopnOneCell(iAfterDest_r,
							  diffDest,
							  population_R,  
							  model_R,
							  dataset_R,
							  transform_R);
	    ans += diffLogLikDest;
	  }
	}
	resetCP(iterator_R, iPopnOrig_r);
	resetCP(iteratorDest_R, iPopnDest_r);
	finished = *finished_ptr;
	while (!finished) {
	  advanceCP(iterator_R);
	  advanceCP(iteratorDest_R);
	  int iPopnOrigHere_r = *iPopnOrigHere_ptr;
	  int iPopnDestHere_r = *iPopnDestHere_ptr;
	  int iAfterOrig_r = dembase_getIAfter(iPopnOrigHere_r, transform_R);
	  int iAfterDest_r = dembase_getIAfter(iPopnDestHere_r, transform_R);
	  if (iAfterOrig_r != iAfterDest_r) {
	    if (iAfterOrig_r > 0) {
	      double diffLogLikOrig = diffLogLikPopnOneCell(iAfterOrig_r,
							    diffOrig,
							    population_R,  
							    model_R,
							    dataset_R,
							    transform_R);
	      ans += diffLogLikOrig;
	    }
	    if (iAfterDest_r > 0) {
	      double diffLogLikDest = diffLogLikPopnOneCell(iAfterDest_r,
							    diffDest,
							    population_R,  
							    model_R,
							    dataset_R,
							    transform_R);
	      ans += diffLogLikDest;
	    }
	  }
	  finished = *finished_ptr;
	}
      }	/* assocWithPopn */
    }	/* for (int iDataset = 0; iDataset < nDatasets; ++iDataset) */
  }     /* iPopnOrig_r != iPopnDest_r */
  UNPROTECT(1); 		/* iteratorDest_R */
  return ans;
}



double
diffLogLikAccountMovePool(SEXP combined_R)
{
    SEXP account_R = GET_SLOT(combined_R, account_sym);
    int iComp_r = *INTEGER(GET_SLOT(combined_R, iComp_sym));

    SEXP component_R = VECTOR_ELT(GET_SLOT(account_R, components_sym), iComp_r - 1);

    SEXP population_R = GET_SLOT(account_R, population_sym);
    SEXP iterator_R = GET_SLOT(combined_R, iteratorPopn_sym);
    SEXP dataModels_R = GET_SLOT(combined_R, dataModels_sym);
    SEXP datasets_R = GET_SLOT(combined_R, datasets_sym);
    SEXP seriesIndices_R = GET_SLOT(combined_R, seriesIndices_sym);
    SEXP transforms_R = GET_SLOT(combined_R, transforms_sym);
    int iCellOut_r = *INTEGER(GET_SLOT(combined_R, iCell_sym));
    int iCellIn_r = *INTEGER(GET_SLOT(combined_R, iCellOther_sym));
    int iPopnOut_r = *INTEGER(GET_SLOT(combined_R, iPopnNext_sym));
    int iPopnIn_r = *INTEGER(GET_SLOT(combined_R, iPopnNextOther_sym));
    int diff = *INTEGER(GET_SLOT(combined_R, diffProp_sym));
    int diffOrig = -1 * diff;
    int diffDest = diff;

    double ans = 0;

    double diffLogLikCells = diffLogLikCellsPool( diff, iComp_r,
                                        iCellOut_r, iCellIn_r,
                                        component_R,
                                        dataModels_R, datasets_R,
                                        seriesIndices_R, transforms_R);
    if (R_finite(diffLogLikCells) ) {

        ans += diffLogLikCells;

        double diffLogLikPopn = diffLogLikPopnPair( diffOrig, diffDest,
                                iPopnOut_r, iPopnIn_r,
                                iterator_R, population_R,
                                dataModels_R, datasets_R,
                                seriesIndices_R, transforms_R);

        if (R_finite(diffLogLikPopn) ) {
            ans += diffLogLikPopn;

        }
        else { /* infinite */
            ans = diffLogLikPopn;
        }
    }
    else { /* infinite */
        ans = diffLogLikCells;
    }

    return ans;
}


double
diffLogLikCellsPool(int diff, int iComp_r, int iCellOut_r, int iCellIn_r,
                        SEXP component_R, SEXP dataModels_R,
                        SEXP datasets_R, SEXP seriesIndices_R,
                        SEXP transforms_R)
{
    double ans = 0;

    int nDatasets = LENGTH(datasets_R);
    int * seriesIndices = INTEGER(seriesIndices_R);

    for(int iDataset = 0; iDataset < nDatasets; ++iDataset) {

        int assocWithComp = ( seriesIndices[iDataset] == iComp_r );

        if (assocWithComp) {

            SEXP model_R = VECTOR_ELT(dataModels_R, iDataset);
            SEXP dataset_R = VECTOR_ELT(datasets_R, iDataset);
            SEXP transform_R = VECTOR_ELT(transforms_R, iDataset);

            double diffLogLikOut = diffLogLikCellOneDataset( diff,
                                    iCellOut_r, component_R,
                                    model_R, dataset_R, transform_R);
            if (R_finite(diffLogLikOut) ) {

                ans += diffLogLikOut;

                double diffLogLikIn = diffLogLikCellOneDataset( diff,
                                    iCellIn_r, component_R,
                                    model_R, dataset_R, transform_R);
                if (R_finite(diffLogLikIn) ) {

                    ans += diffLogLikIn;

                }
                else { /* infinite */
                    ans = diffLogLikIn;
                    break; /* break out of for loop */
                }

            }
            else { /* infinite */
                ans = diffLogLikOut;
                break; /* break out of for loop */
            }
        }
    }

    return ans;
}


double
diffLogLikAccountMoveNet(SEXP combined_R)
{
    SEXP account_R = GET_SLOT(combined_R, account_sym);
    int iComp_r = *INTEGER(GET_SLOT(combined_R, iComp_sym));

    SEXP component_R = VECTOR_ELT(GET_SLOT(account_R, components_sym), iComp_r - 1);

    SEXP population_R = GET_SLOT(account_R, population_sym);
    SEXP iterator_R = GET_SLOT(combined_R, iteratorPopn_sym);
    SEXP dataModels_R = GET_SLOT(combined_R, dataModels_sym);
    SEXP datasets_R = GET_SLOT(combined_R, datasets_sym);
    SEXP seriesIndices_R = GET_SLOT(combined_R, seriesIndices_sym);
    SEXP transforms_R = GET_SLOT(combined_R, transforms_sym);
    int iCellAdd_r = *INTEGER(GET_SLOT(combined_R, iCell_sym));
    int iCellSub_r = *INTEGER(GET_SLOT(combined_R, iCellOther_sym));
    int iPopnAdd_r = *INTEGER(GET_SLOT(combined_R, iPopnNext_sym));
    int iPopnSub_r = *INTEGER(GET_SLOT(combined_R, iPopnNextOther_sym));
    int diff = *INTEGER(GET_SLOT(combined_R, diffProp_sym));
    int diffAdd = diff;
    int diffSub = -1 * diff;

    double ans = 0;

    double diffLogLikCells = diffLogLikCellsNet( diff, iComp_r,
                                        iCellAdd_r, iCellSub_r,
                                        component_R,
                                        dataModels_R, datasets_R,
                                        seriesIndices_R, transforms_R);
    if (R_finite(diffLogLikCells) ) {

        ans += diffLogLikCells;

        double diffLogLikPopn = diffLogLikPopnPair(diffAdd, diffSub,
                                iPopnAdd_r, iPopnSub_r,
                                iterator_R, population_R,
                                dataModels_R, datasets_R,
                                seriesIndices_R, transforms_R);

        if (R_finite(diffLogLikPopn) ) {
            ans += diffLogLikPopn;

        }
        else { /* infinite */
            ans = diffLogLikPopn;
        }
    }
    else { /* infinite */
        ans = diffLogLikCells;
    }

    return ans;
}


double
diffLogLikCellsNet(int diff, int iComp_r, int iCellAdd_r, int iCellSub_r,
                        SEXP component_R, SEXP dataModels_R,
                        SEXP datasets_R, SEXP seriesIndices_R,
                        SEXP transforms_R)
{
    double ans = 0;

    int nDatasets = LENGTH(datasets_R);
    int * seriesIndices = INTEGER(seriesIndices_R);

    for(int iDataset = 0; iDataset < nDatasets; ++iDataset) {

        int assocWithComp = ( seriesIndices[iDataset] == iComp_r );

        if (assocWithComp) {

            SEXP model_R = VECTOR_ELT(dataModels_R, iDataset);
            SEXP dataset_R = VECTOR_ELT(datasets_R, iDataset);
            SEXP transform_R = VECTOR_ELT(transforms_R, iDataset);

            double diffLogLikAdd = diffLogLikCellOneDataset( diff,
                                    iCellAdd_r, component_R,
                                    model_R, dataset_R, transform_R);
            if (R_finite(diffLogLikAdd) ) {

                ans += diffLogLikAdd;

                double diffLogLikSub = diffLogLikCellOneDataset( -diff,
                                    iCellSub_r, component_R,
                                    model_R, dataset_R, transform_R);
                if (R_finite(diffLogLikSub) ) {

                    ans += diffLogLikSub;

                }
                else { /* infinite */
                    ans = diffLogLikSub;
                    break; /* break out of for loop */
                }

            }
            else { /* infinite */
                ans = diffLogLikAdd;
                break; /* break out of for loop */
            }
        }
    }

    return ans;
}

double
diffLogLikAccountMoveComp(SEXP combined_R)
{

    SEXP account_R = GET_SLOT(combined_R, account_sym);
    int iComp_r = *INTEGER(GET_SLOT(combined_R, iComp_sym));

    SEXP component_R = VECTOR_ELT(GET_SLOT(account_R, components_sym), iComp_r - 1);

    SEXP population_R = GET_SLOT(account_R, population_sym);
    SEXP iterator_R = GET_SLOT(combined_R, iteratorPopn_sym);

    SEXP dataModels_R = GET_SLOT(combined_R, dataModels_sym);
    SEXP datasets_R = GET_SLOT(combined_R, datasets_sym);
    SEXP seriesIndices_R = GET_SLOT(combined_R, seriesIndices_sym);
    SEXP transforms_R = GET_SLOT(combined_R, transforms_sym);
    int iCell_r = *INTEGER(GET_SLOT(combined_R, iCell_sym));
    int iPopnNext_r = *INTEGER(GET_SLOT(combined_R, iPopnNext_sym));
    int diff = *INTEGER(GET_SLOT(combined_R, diffProp_sym));

    int * isIncrementVec = LOGICAL(GET_SLOT(combined_R, isIncrement_sym));
    int isIncrement = isIncrementVec[iComp_r - 1];


    double ans = 0;

    double diffLogLikCell = diffLogLikCellComp( diff, iComp_r,
                                        iCell_r, component_R,
                                        dataModels_R, datasets_R,
                                        seriesIndices_R, transforms_R);
    if (R_finite(diffLogLikCell) ) {

        ans += diffLogLikCell;

        double diffPopn = ( isIncrement ? diff : -diff );

        double diffLogLikPopnValue = diffLogLikPopn( diffPopn,
                                iPopnNext_r,
                                iterator_R, population_R,
                                dataModels_R, datasets_R,
                                seriesIndices_R, transforms_R);

        if (R_finite(diffLogLikPopnValue) ) {
            ans += diffLogLikPopnValue;

        }
        else { /* infinite */
            ans = diffLogLikPopnValue;
        }
    }
    else { /* infinite */
        ans = diffLogLikCell;
    }

    return ans;
}


double
diffLogLikAccountMoveCompSmall(SEXP combined_R)
{

    SEXP account_R = GET_SLOT(combined_R, account_sym);
    int i_comp_r = *INTEGER(GET_SLOT(combined_R, iComp_sym));
    int i_comp = i_comp_r - 1;

    SEXP component_R = VECTOR_ELT(GET_SLOT(account_R, components_sym), i_comp);

    SEXP dataModels_R = GET_SLOT(combined_R, dataModels_sym);
    SEXP datasets_R = GET_SLOT(combined_R, datasets_sym);
    SEXP seriesIndices_R = GET_SLOT(combined_R, seriesIndices_sym);
    SEXP transforms_R = GET_SLOT(combined_R, transforms_sym);
    int i_cell_up_r = *INTEGER(GET_SLOT(combined_R, iCell_sym));
    int i_cell_low_r = *INTEGER(GET_SLOT(combined_R, iCellOther_sym));

    int diff = *INTEGER(GET_SLOT(combined_R, diffProp_sym));

    double ans = 0;

    double diffLogLikUp = diffLogLikCellComp( diff, i_comp_r,
                                        i_cell_up_r, component_R,
                                        dataModels_R, datasets_R,
                                        seriesIndices_R, transforms_R);
    if (R_finite(diffLogLikUp) ) {
      
	diff *= -1;

        double diffLogLikLow = diffLogLikCellComp(diff, i_comp_r,
                                        i_cell_low_r, component_R,
                                        dataModels_R, datasets_R,
                                        seriesIndices_R, transforms_R);

        if (R_finite(diffLogLikLow) ) {
            ans = diffLogLikUp + diffLogLikLow;
        }
        else { /* infinite */
            ans = diffLogLikLow;
        }
    }
    else { /* infinite */
        ans = diffLogLikUp;
    }

    return ans;
}

/*------------------ LOG DENSITY ------------------------*/


double
diffLogDensPopn(SEXP combined_R)
{
    SEXP account_R = GET_SLOT(combined_R, account_sym);

    SEXP population_R = GET_SLOT(account_R, population_sym);
    SEXP iterator_R = GET_SLOT(combined_R, iteratorPopn_sym);

    SEXP systemModels_R = GET_SLOT(combined_R, systemModels_sym);
    SEXP thisModel_R = VECTOR_ELT(systemModels_R, 0);

    double * theta = REAL(GET_SLOT(thisModel_R, theta_sym));
    int * strucZeroArray = INTEGER(GET_SLOT(thisModel_R, strucZeroArray_sym));

    int iPopnNext_r = *INTEGER(GET_SLOT(combined_R, iPopnNext_sym));
    int iPopnNextOther_r = *INTEGER(GET_SLOT(combined_R, iPopnNextOther_sym));
    int diff = *INTEGER(GET_SLOT(combined_R, diffProp_sym));
    int iCell_r = *INTEGER(GET_SLOT(combined_R, iCell_sym));
    int iComp_r = *INTEGER(GET_SLOT(combined_R, iComp_sym));

    int iOrigDest_r = *INTEGER(GET_SLOT(combined_R, iOrigDest_sym));
    int iPool_r = *INTEGER(GET_SLOT(combined_R, iPool_sym));
    int iIntNet_r = *INTEGER(GET_SLOT(combined_R, iIntNet_sym));
    int * isIncrementVec = LOGICAL(GET_SLOT(combined_R, isIncrement_sym));

    int isPopn = (iComp_r == 0);
    int isOrigDest = (iComp_r == iOrigDest_r);
    int isPool = (iComp_r == iPool_r);
    int isIntNet = (iComp_r == iIntNet_r);

    double ans = 0;

    if (isPopn) {
        ans = diffLogDensPopnOneCohort(diff, population_R,
                       iCell_r, iterator_R, theta, strucZeroArray);
    }
    else if (isOrigDest || isPool) {

        double ansOrig = diffLogDensPopnOneCohort(-diff, population_R,
                          iPopnNext_r, iterator_R, theta, strucZeroArray);

        double ansDest = diffLogDensPopnOneCohort(diff, population_R,
                          iPopnNextOther_r, iterator_R, theta, strucZeroArray);
        ans = ansOrig + ansDest;

    }
    else if (isIntNet) {

        double ansAdd = diffLogDensPopnOneCohort(diff, population_R,
                         iPopnNext_r, iterator_R, theta, strucZeroArray);

        double ansSub = diffLogDensPopnOneCohort(-diff, population_R,
                         iPopnNextOther_r, iterator_R, theta, strucZeroArray);
        ans = ansAdd + ansSub;

    }
    else {

        if ( !isIncrementVec[iComp_r - 1] ) {
            diff = -diff;
        }

        ans = diffLogDensPopnOneCohort(diff, population_R,
                       iPopnNext_r, iterator_R, theta, strucZeroArray);

    }

    return ans;
}


double
diffLogDensPopnOneCohort(int diff, SEXP population_R, int i_r,
             SEXP iterator_R, double * theta,
             int * strucZeroArray)
{
    int * population = INTEGER(population_R);
    resetCP(iterator_R, i_r);
    double ans = 0;

    int * i_ptr = INTEGER(GET_SLOT(iterator_R, i_sym));
    int * finished_ptr = LOGICAL(GET_SLOT(iterator_R, finished_sym));

    int valCurr = 0;
    int valProp = 0;
    double lambda = 0;
    double logDensProp = 0;
    double logDensCurr = 0;

    int i = *i_ptr - 1;
    int finished = *finished_ptr;

    int isStrucZero = strucZeroArray[i] == 0;
    if (!isStrucZero) {
        valCurr = population[i];
        valProp = valCurr + diff;
        lambda = theta[i];
        logDensProp = dpois(valProp, lambda, USE_LOG);
        logDensCurr = dpois(valCurr, lambda, USE_LOG);
        ans += (logDensProp - logDensCurr);
    }

    while (!finished) {

        advanceCP(iterator_R);
        i = *i_ptr - 1;
        finished = *finished_ptr;

        isStrucZero = strucZeroArray[i] == 0;
        if (!isStrucZero) {
            valCurr = population[i];
            valProp = valCurr + diff;
            lambda = theta[i];
            logDensProp = dpois(valProp, lambda, USE_LOG);
            logDensCurr = dpois(valCurr, lambda, USE_LOG);
            ans += (logDensProp - logDensCurr);
        }

    }
    return ans;
}


double
diffLogDensExpPopn(SEXP combined_R)
{
    double ans = 0;

    SEXP account_R = GET_SLOT(combined_R, account_sym);

    SEXP components_R = GET_SLOT(account_R, components_sym);
    int nComponents = LENGTH(components_R);

    SEXP iteratorsComp_R = GET_SLOT(combined_R, iteratorsComp_sym);

    int * modelUsesExposure = LOGICAL(GET_SLOT(combined_R, modelUsesExposure_sym));
    SEXP mappingsFromExp_R = GET_SLOT(combined_R, mappingsFromExp_sym);
    SEXP mappingsToExp_R = GET_SLOT(combined_R, mappingsToExp_sym);

    int iExpFirst_r = *INTEGER(GET_SLOT(combined_R, iExpFirst_sym));
    SEXP iteratorExposure_R = GET_SLOT(combined_R, iteratorExposure_sym);

    int diff = *INTEGER(GET_SLOT(combined_R, diffProp_sym));

    int iOrigDest_r = *INTEGER(GET_SLOT(combined_R, iOrigDest_sym));
    int iPool_r = *INTEGER(GET_SLOT(combined_R, iPool_sym));

    int iBirths_r = *INTEGER(GET_SLOT(combined_R, iBirths_sym));

    double * exposure = REAL(GET_SLOT(combined_R, exposure_sym));

    SEXP systemModels_R = GET_SLOT(combined_R, systemModels_sym);

    int hasAge = *LOGICAL(GET_SLOT(combined_R, hasAge_sym));
    double ageTimeStep = *REAL(GET_SLOT(combined_R, ageTimeStep_sym));

    int updatedPopnTrue = 1;
    int updatedBirthsFalse = 0;
    int firstOnlyFalse = 0;
    int isSmallUpdateFinal = 0;


    for (int i = 0; i < nComponents; ++i) {

        int next_i = i + 1;

        int usesExp = modelUsesExposure[next_i];

        if(usesExp) {

            SEXP component_R = VECTOR_ELT(components_R, i);

            SEXP thisModel_R = VECTOR_ELT(systemModels_R, next_i);
            double * theta = REAL(GET_SLOT(thisModel_R, theta_sym));
            int * strucZeroArray = INTEGER(GET_SLOT(thisModel_R, strucZeroArray_sym));

            SEXP iteratorComp_R = VECTOR_ELT(iteratorsComp_R, i);
            SEXP mappingFromExp_R = VECTOR_ELT(mappingsFromExp_R, i);

            int i_r = i + 1;
            int isOrigDest = (i_r == iOrigDest_r);
            int isPool = (i_r == iPool_r);
            int isBirths = (i_r == iBirths_r);

            double diffLog = 0;

            if(isOrigDest || isPool) {

                int iCell_r = getICellCompFromExp(iExpFirst_r, mappingFromExp_R);

                diffLog = diffLogDensExpOneOrigDestParChPool(iCell_r,
                                        hasAge, ageTimeStep,
  				        updatedPopnTrue, updatedBirthsFalse,
                                        component_R, theta, strucZeroArray,
                                        iteratorComp_R, iExpFirst_r,
                                        exposure, iteratorExposure_R, diff,
   				     firstOnlyFalse, isSmallUpdateFinal);
		


            }
            else if(isBirths) {

	      int iCell_r = getICellBirthsFromExp(iExpFirst_r, mappingFromExp_R, 1);

                int cellIsAffected = (iCell_r > 0);
                if (cellIsAffected) {

		    SEXP mappingToExp_R = VECTOR_ELT(mappingsToExp_R, i);
		    int iExpFirstBirths_R = getIExposureFromBirths(iCell_r, mappingToExp_R);

                        diffLog = diffLogDensExpOneOrigDestParChPool(iCell_r,
                                        hasAge, ageTimeStep, updatedPopnTrue,
  				        updatedBirthsFalse,
                                        component_R, theta, strucZeroArray,
                                        iteratorComp_R, iExpFirstBirths_R,
                                        exposure, iteratorExposure_R, diff,
   				     firstOnlyFalse, isSmallUpdateFinal);

                }
            }
            else {
                int iCell_r = getICellCompFromExp(iExpFirst_r, mappingFromExp_R);

                diffLog = diffLogDensExpOneComp(iCell_r,
                                        hasAge, ageTimeStep, updatedPopnTrue,
						updatedBirthsFalse,
                                        component_R, theta, strucZeroArray,
                                        iteratorComp_R, iExpFirst_r,
                                        exposure, iteratorExposure_R,
						diff, firstOnlyFalse, isSmallUpdateFinal);
            }

            if (R_finite(diffLog) ) {
                ans += diffLog;
            }
            else { /* infinite */
                ans = diffLog;
                break; /* break out of for loop */
            }
        }

    } /* end loop through components */

    return ans;
}



double
diffLogDensExpOneOrigDestParChPool(int iCell_r, int hasAge,
				   double ageTimeStep, int updatedPopn, int updatedBirths,
				   SEXP component_R, double * theta,
				   int * strucZeroArray,
				   SEXP iteratorComp_R,
				   int iExpFirst_r, double * exposure,
				   SEXP iteratorExposure_R,
				   int diff, int firstOnly, int isSmallUpdateFinal)
{
  double ans = 0;
  int * component = INTEGER(component_R);
  resetCODPCP(iteratorComp_R, iCell_r);
  resetCC(iteratorExposure_R, iExpFirst_r);
  int * iExp_ptr = INTEGER(GET_SLOT(iteratorExposure_R, i_sym));
  int * iCompVec = INTEGER(GET_SLOT(iteratorComp_R, iVec_sym));
  int * iAge_ptr = INTEGER(GET_SLOT(iteratorComp_R, iAge_sym));
  int * iTri_ptr = INTEGER(GET_SLOT(iteratorComp_R, iTriangle_sym));
  int * finishedComp_ptr = LOGICAL(GET_SLOT(iteratorComp_R, finished_sym));
  int isFirstCell = 1;
  int isFinal = 0;
  int iAge;
  int iTri;
  int nAge;
  int lastAgeGroupOpen;
  if (hasAge) {
    nAge = *INTEGER(GET_SLOT(iteratorComp_R, nAge_sym));
    lastAgeGroupOpen = *INTEGER(GET_SLOT(iteratorComp_R, lastAgeGroupOpen_sym));
  }
  int lengthVec = *INTEGER(GET_SLOT(iteratorComp_R, lengthVec_sym));
  double tolExposure = 0.000001;
  int isUpper = 0;
  int keepGoing = 1;
  while (keepGoing) {
    int iExp_r = *iExp_ptr;
    double diffExposure = 0;
    if (hasAge) {
      iAge = *iAge_ptr;
      iTri = *iTri_ptr;
      isFinal = iAge == nAge;
      isUpper = iTri == 2;
      if (isFirstCell) {
	if (updatedPopn) {       
	  if (isFinal && lastAgeGroupOpen) {
	    if (isUpper)
	      diffExposure += ageTimeStep * diff;
	    else
	      diffExposure += 0.5 * ageTimeStep * diff;
	  }
	  else
	    diffExposure += 0.5 * ageTimeStep * diff;
	}
	else {
	  /* adjust for increments */
	  if (!updatedBirths) {
	    if (isFinal && lastAgeGroupOpen) {
	      if (isUpper)
		diffExposure += 0.5 * ageTimeStep * diff;
	      else
		diffExposure -= (1.0/6.0) * ageTimeStep * diff;
	    }
	    else {
	      if (isUpper)
		diffExposure += (1.0/6.0) * ageTimeStep * diff;
	      else
		diffExposure -= (1.0/6.0) * ageTimeStep * diff;
	    }
	  }
	  /* adjust for popn */
	  int popnSame = isUpper || (firstOnly && !isSmallUpdateFinal);
	  if (!popnSame)
	    diffExposure += 0.5 * ageTimeStep * diff;
	}
      }
      else { 			/* not first cell, so only */
	if (isFinal && lastAgeGroupOpen) {		/* adjust for popn */
	  if (isUpper)
	    diffExposure += ageTimeStep * diff;
	  else
	      diffExposure += 0.5 * ageTimeStep * diff;
	}
	else
	  diffExposure += 0.5 * ageTimeStep * diff;
      }
    }
    else { 			/* no age */
      if (isFirstCell) {
	if (updatedPopn)
	  diffExposure += ageTimeStep * diff;
	else
	  diffExposure += 0.5 * ageTimeStep * diff;
      }
      else
	diffExposure += ageTimeStep * diff;
    }	
    double exposureCurr = exposure[iExp_r - 1];
    double exposureProp = exposureCurr + diffExposure;
    if (exposureProp < tolExposure) {
      if (exposureProp > -1 * tolExposure)
	exposureProp = 0;
      else {
	if (exposureCurr < 0) {
	  ans = R_NegInf;
	  keepGoing = 0;
	}
	else
	  error("negative value for 'exposureProp' : %f", exposureProp);
      }
    }
    int j = 0;
    while (j < lengthVec && keepGoing) {
      int iComp = iCompVec[j] - 1;
      int isStrucZero = strucZeroArray[iComp] == 0;
      if (!isStrucZero) {
	int compCurr = component[iComp];
	if ( (compCurr > 0) && !(exposureProp > 0) ) {
	  ans = R_NegInf;
	  keepGoing = 0;
	}
	else {
	  double thetaCurr = theta[iComp];
	  double lambdaProp = thetaCurr * exposureProp;
	  double lambdaCurr = thetaCurr * exposureCurr;
	  double diffLogLik = dpois(compCurr, lambdaProp, USE_LOG)
	    -
	    dpois(compCurr, lambdaCurr, USE_LOG);
	  ans += diffLogLik;
	}
      }
      ++j;
    }
    if (keepGoing) {
      isFirstCell = 0;
      int finishedComp = *finishedComp_ptr;
      if (finishedComp || firstOnly) {
	keepGoing = 0;
      }
      else {
	advanceCODPCP(iteratorComp_R);
	advanceCC(iteratorExposure_R);
      }
    }
  } /* end main while loop */
  return ans;
}


double
diffLogDensExpOneComp(int iCell_r, int hasAge,
		      double ageTimeStep, int updatedPopn, int updatedBirths,
		      SEXP component_R, double * theta, int * strucZeroArray,
		      SEXP iteratorComp_R,
		      int iExpFirst_r, double * exposure,
		      SEXP iteratorExposure_R,
		      int diff, int firstOnly, int isSmallUpdateFinal)
{
  double ans = 0;
  int * component = INTEGER(component_R);
  resetCC(iteratorComp_R, iCell_r);
  resetCC(iteratorExposure_R, iExpFirst_r);
  int * iExp_ptr = INTEGER(GET_SLOT(iteratorExposure_R, i_sym));
  int * iComp_ptr = INTEGER(GET_SLOT(iteratorComp_R, i_sym));
  int * iAge_ptr = INTEGER(GET_SLOT(iteratorComp_R, iAge_sym));
  int * iTri_ptr = INTEGER(GET_SLOT(iteratorComp_R, iTriangle_sym));
  int * finishedComp_ptr = LOGICAL(GET_SLOT(iteratorComp_R, finished_sym));
  int keepGoing = 1;
  int isFirstCell = 1;
  int isFinal = 0;
  double tolExposure = 0.000001;
  int nAge;
  int lastAgeGroupOpen;
  if (hasAge) {
    nAge = *INTEGER(GET_SLOT(iteratorComp_R, nAge_sym));
    lastAgeGroupOpen = *INTEGER(GET_SLOT(iteratorComp_R, lastAgeGroupOpen_sym));
  }
  int iAge;
  int iTri;
  int isUpper;
  while (keepGoing) {
    int iComp_r = *iComp_ptr;
    int iExp = *iExp_ptr - 1;
    int iComp = iComp_r - 1;
    if (hasAge) {
      iAge = *iAge_ptr;
      iTri = *iTri_ptr;
      isFinal = iAge == nAge;
      isUpper = iTri == 2;
    }
    int isStrucZero = strucZeroArray[iComp] == 0;
    if (!isStrucZero) {
      int compCurr = component[iComp];
      double diffExposure = 0;
      if (hasAge) {
	if (isFirstCell) {
	  if (updatedPopn) {
	    if (isFinal && lastAgeGroupOpen) {
	      if (isUpper)
		diffExposure += ageTimeStep * diff;
	      else
		diffExposure += 0.5 * ageTimeStep * diff;
	    }
	    else {
	      diffExposure += 0.5 * ageTimeStep * diff;
	    }
	  }
	  else {
	    /* increment due to component */
	    if (!updatedBirths) {
	      if (isFinal && lastAgeGroupOpen) {
		if (isUpper) {
		  diffExposure += 0.5 * ageTimeStep * diff;
		}
		else {
		  diffExposure -= (1.0/6.0) * ageTimeStep * diff;
		}
	      }
	      else {
		if (isUpper) {
		  diffExposure += (1.0/6.0) * ageTimeStep * diff;
		}
		else {
		  diffExposure -= (1.0/6.0) * ageTimeStep * diff;
		}
	      }
	    }
	    /* increment due to popn */
	    int popnSame = isUpper || (firstOnly && !isSmallUpdateFinal);
	    if (!popnSame) {
	      diffExposure += 0.5 * ageTimeStep * diff;
	    }
	  }
	}
	else {
	  if (isFinal && lastAgeGroupOpen) {
	    if (isUpper) {
	      diffExposure += ageTimeStep * diff;
	    }
	    else {
	      diffExposure += 0.5 * ageTimeStep * diff;
	    }
	  }
	  else {
	    diffExposure += 0.5 * ageTimeStep * diff;
	  }
	}
      }
      else { 			/* no age */
	if (isFirstCell) {
	  if (updatedPopn) {
	    diffExposure += ageTimeStep * diff;
	  }
	  else {
	    diffExposure += 0.5 * ageTimeStep * diff;
	  }
	}
	else {
	  diffExposure += ageTimeStep * diff;
	}
      }
      double exposureCurr = exposure[iExp];
      double exposureProp = exposureCurr + diffExposure;

      /* if (isFinal) { */
      /* 	printf("'exposureProp'=%f, 'exposureCurr'=%f, diff=%d, isFinal=%d, isUpper=%d, iCell_r=%d, firstOnly=%d, compCurr=%d\n\n", */
      /* 	       exposureProp, exposureCurr, diff, isFinal, isUpper, iCell_r, firstOnly, compCurr); */
      /* } */
      
      if (exposureProp < tolExposure) {
	if (exposureProp > -1 * tolExposure)
	  exposureProp = 0;
	else {
	  if (exposureCurr < 0) {
	    ans = R_NegInf;
	    keepGoing = 0;
	  }
	  else
	    error("negative value for 'exposureProp' : %f, 'exposureCurr'=%f, diff=%d, isFinal=%d, isUpper=%d, iCell_r=%d, firstOnly=%d, compCurr=%d, updatedPopn=%d, updatedBirths=%d",
		  exposureProp, exposureCurr, diff, isFinal, isUpper, iCell_r, firstOnly, compCurr,
		  updatedPopn, updatedBirths);
	}
      }
      if ( (compCurr > 0) && !(exposureProp > 0) ) {
	ans = R_NegInf;
	keepGoing = 0;
      }
      if (keepGoing) {
	double thetaCurr = theta[iComp];
	double lambdaProp = thetaCurr * exposureProp;
	double lambdaCurr = thetaCurr * exposureCurr;
	double diffLogLik = dpois(compCurr, lambdaProp, USE_LOG)
	  -
	  dpois(compCurr, lambdaCurr, USE_LOG);
	ans += diffLogLik;
      }
    }
    if (keepGoing) {
      isFirstCell = 0;
      int finishedComp = *finishedComp_ptr;
      if (finishedComp || firstOnly) {
	keepGoing = 0;
      }
      else {
	advanceCC(iteratorComp_R);
	advanceCC(iteratorExposure_R);
      }
    }

  } /* end main while loop */

  return ans;
}

double
diffLogDensJumpOrigDest(SEXP combined_R)
{
  SEXP account_R = GET_SLOT(combined_R, account_sym);
  int iComp_r = *INTEGER(GET_SLOT(combined_R, iComp_sym));
  SEXP components_R = GET_SLOT(account_R, components_sym);
  SEXP component_R = VECTOR_ELT(components_R, iComp_r - 1);
  int * component = INTEGER(component_R);
  SEXP systemModels_R = GET_SLOT(combined_R, systemModels_sym);
  SEXP thisSystemModel_R = VECTOR_ELT(systemModels_R, iComp_r); /* iComp_r is c-style index + 1*/
  SEXP theta_R = GET_SLOT(thisSystemModel_R, theta_sym);
  double * theta = REAL(theta_R);
  double * exposure = REAL(GET_SLOT(combined_R, exposure_sym));
  double * expectedExposure = REAL(GET_SLOT(combined_R, expectedExposure_sym));
  int iCell_r = *INTEGER(GET_SLOT(combined_R, iCell_sym));
  int iExposure_r = *INTEGER(GET_SLOT(combined_R, iExposure_sym));
  int diff = *INTEGER(GET_SLOT(combined_R, diffProp_sym));
  SEXP iterators_R = GET_SLOT(combined_R, iteratorsComp_sym);
  SEXP iterator_R = VECTOR_ELT(iterators_R, iComp_r - 1);
  int hasAge = *LOGICAL(GET_SLOT(iterator_R, hasAge_sym));
  double ageTimeStep = *REAL(GET_SLOT(combined_R, ageTimeStep_sym));
  double tolExposure = 0.000001;
  int iCell = iCell_r - 1;
  int iExposure = iExposure_r - 1;
  double thetaCell = theta[iCell];
  double exposureCellCurr = exposure[iExposure];
  double exposureCellJump = expectedExposure[iExposure];
  double exposureCellProp = exposureCellCurr;
  int keepGoing = 1;
  double ans = 0;
  if (hasAge) {
    resetCC(iterator_R, iCell_r);
    int nAge = *INTEGER(GET_SLOT(iterator_R, nAge_sym));
    int iAge = *INTEGER(GET_SLOT(iterator_R, iAge_sym));
    int iTri = *INTEGER(GET_SLOT(iterator_R, iTriangle_sym));
    int isFinal = iAge == nAge;
    int isUpper = iTri == 2;
    if (isFinal) {
      if (isUpper)
	exposureCellProp -= 0.5 * ageTimeStep * diff;
      else
	exposureCellProp -= (1.0/3.0) * ageTimeStep * diff;
    }
    else {
      if (isUpper)
	exposureCellProp -= (1.0/6.0) * ageTimeStep * diff;
      else
	exposureCellProp -= (1.0/3.0) * ageTimeStep * diff;
    }
  }
  else {
    exposureCellProp -= 0.5 * ageTimeStep * diff;
  }
  if (exposureCellProp < tolExposure) {
    if (exposureCellProp > -1 * tolExposure)
      exposureCellProp = 0;
    else {
      if (exposureCellCurr < 0) {
	ans = R_NegInf;
	keepGoing = 0;
      }
      else
	error("negative value for 'exposureCellProp' : %f", exposureCellProp);
    }
  }
  if (keepGoing) {
    int valCurr = component[iCell];
    int valProp = valCurr + diff;
    if ((valProp > 0) && !(exposureCellProp > 0))
      ans = R_NegInf;
    else {
      double lambdaDensProp = thetaCell * exposureCellProp;
      double lambdaJump = thetaCell * exposureCellJump;
      double diffLogDens = dpois(valProp, lambdaDensProp, USE_LOG) -
	dpois(valCurr, lambdaDensProp, USE_LOG);
      double diffLogJump = dpois(valCurr, lambdaJump, USE_LOG) -
	dpois(valProp, lambdaJump, USE_LOG);
      ans = diffLogDens + diffLogJump;
    }
  }
  return ans;
}


double
diffLogDensExpOrigDestPoolNet(SEXP combined_R)
{
    SEXP account_R = GET_SLOT(combined_R, account_sym);
    SEXP components_R = GET_SLOT(account_R, components_sym);
    int nComponents = LENGTH(components_R);

    SEXP iteratorsComp_R = GET_SLOT(combined_R, iteratorsComp_sym);

    int * modelUsesExposure = LOGICAL(GET_SLOT(combined_R, modelUsesExposure_sym));
    SEXP mappingsFromExposure_R = GET_SLOT(combined_R, mappingsFromExp_sym);
    SEXP mappingsToExposure_R = GET_SLOT(combined_R, mappingsToExp_sym);

    int iExpFirstOrig_r = *INTEGER(GET_SLOT(combined_R, iExpFirst_sym));
    int iExpFirstDest_r = *INTEGER(GET_SLOT(combined_R, iExpFirstOther_sym));

    SEXP iteratorExposure_R = GET_SLOT(combined_R, iteratorExposure_sym);

    int diff = *INTEGER(GET_SLOT(combined_R, diffProp_sym));

    int iComp_r = *INTEGER(GET_SLOT(combined_R, iComp_sym));
    int iOrigDest_r = *INTEGER(GET_SLOT(combined_R, iOrigDest_sym));
    int iPool_r = *INTEGER(GET_SLOT(combined_R, iPool_sym));
    int iIntNet_r = *INTEGER(GET_SLOT(combined_R, iIntNet_sym));

    int iBirths_r = *INTEGER(GET_SLOT(combined_R, iBirths_sym));

    double * exposure = REAL(GET_SLOT(combined_R, exposure_sym));

    SEXP systemModels_R = GET_SLOT(combined_R, systemModels_sym);

    int hasAge = *LOGICAL(GET_SLOT(combined_R, hasAge_sym));
    double ageTimeStep = *REAL(GET_SLOT(combined_R, ageTimeStep_sym));

    int updatedPopnFalse = 0;
    int updatedBirthsFalse = 0;

    int firstOnlyFalse = 0;
    int isSmallUpdateFinalFalse = 0;

    double ans = 0;

    int noExposureAffected = ( (iExpFirstOrig_r == 0)
                                || (iExpFirstOrig_r == iExpFirstDest_r) );
    if(!noExposureAffected) {

        int diffOrig = -diff;
        int diffDest = diff;

        int compIsIntNet = (iComp_r == iIntNet_r);

        if(compIsIntNet) {
            diffOrig = diff;
            diffDest = -diff;
        }

        for(int i = 0; i < nComponents; ++i) {

            int thisModelUsesExposure = modelUsesExposure[i + 1];
            if(thisModelUsesExposure) {
                SEXP component_R = VECTOR_ELT(components_R, i);
                SEXP thisSystemModel_R = VECTOR_ELT(systemModels_R, i+1);
                double * theta = REAL(GET_SLOT(thisSystemModel_R, theta_sym));
                int * strucZeroArray = INTEGER(GET_SLOT(thisSystemModel_R, strucZeroArray_sym));
                SEXP iteratorComp_R = VECTOR_ELT(iteratorsComp_R, i);
                SEXP mappingFromExposure_R = VECTOR_ELT(mappingsFromExposure_R, i);

                int isOrigDest = (i == iOrigDest_r - 1);
                int isPool = (i == iPool_r - 1);
                int isBirths = (i == iBirths_r - 1);

                double diffLogOrig = 0;
                double diffLogDest = 0;

                if (isOrigDest || isPool) {

		  int iCellOrig_r = getICellCompFromExp(iExpFirstOrig_r,
							mappingFromExposure_R);
		  int iCellDest_r = getICellCompFromExp(iExpFirstDest_r,
							mappingFromExposure_R);
		  diffLogOrig
		    = diffLogDensExpOneOrigDestParChPool(iCellOrig_r,
							 hasAge, ageTimeStep,
							 updatedPopnFalse,
							 updatedBirthsFalse,
							 component_R, theta,
							 strucZeroArray,
							 iteratorComp_R,
							 iExpFirstOrig_r,
							 exposure,
							 iteratorExposure_R,
							 diffOrig,
							 firstOnlyFalse,
							 isSmallUpdateFinalFalse);
		  if (R_finite(diffLogOrig)) {

		      diffLogDest
			= diffLogDensExpOneOrigDestParChPool(iCellDest_r,
							     hasAge, ageTimeStep,
							     updatedPopnFalse,
							     updatedBirthsFalse,
							     component_R, theta,
							     strucZeroArray,
							     iteratorComp_R,
							     iExpFirstDest_r,
							     exposure,
							     iteratorExposure_R,
							     diffDest,
							     firstOnlyFalse,
							     isSmallUpdateFinalFalse);
                    }
                } /* end if isOrigDest || isPool */

                else if(isBirths) {
                    int iCellOrig_r = getICellBirthsFromExp(iExpFirstOrig_r,
							    mappingFromExposure_R,
							    1);
                    int iCellDest_r = getICellBirthsFromExp(iExpFirstDest_r,
							    mappingFromExposure_R,
							    1);

                    int cellIsAffected = (iCellOrig_r > 0);
                    if(cellIsAffected) {

		    SEXP mappingToExposure_R = VECTOR_ELT(mappingsToExposure_R, i);
		    int iExpFirstOrigBirths_R = getIExposureFromBirths(iCellOrig_r, mappingToExposure_R);
		    int iExpFirstDestBirths_R = getIExposureFromBirths(iCellDest_r, mappingToExposure_R);
		      
		    diffLogOrig
		      = diffLogDensExpOneOrigDestParChPool(iCellOrig_r,
							   hasAge, ageTimeStep,
							   updatedPopnFalse,
							   updatedBirthsFalse,
							   component_R, theta,
							   strucZeroArray,
							   iteratorComp_R,
							   iExpFirstOrigBirths_R,
							   exposure,
							   iteratorExposure_R,
							   diffOrig,
							   firstOnlyFalse,
							   isSmallUpdateFinalFalse);

                            if (R_finite(diffLogOrig)) {

			      diffLogDest
				= diffLogDensExpOneOrigDestParChPool(iCellDest_r,
								     hasAge, ageTimeStep,
								     updatedPopnFalse,
								     updatedBirthsFalse,
								     component_R, theta,
								     strucZeroArray,
								     iteratorComp_R,
								     iExpFirstDestBirths_R,
								     exposure,
								     iteratorExposure_R,
								     diffDest,
								     firstOnlyFalse,
								     isSmallUpdateFinalFalse);
                            }

                    } /* end cellIsAffected */

                } /* end if isBirths */

                else { /* is net */

                    int iCellOrig_r = getICellCompFromExp(iExpFirstOrig_r,
                                                mappingFromExposure_R);
                    int iCellDest_r = getICellCompFromExp(iExpFirstDest_r,
                                                mappingFromExposure_R);

                    diffLogOrig
                            = diffLogDensExpOneComp(iCellOrig_r,
                                                    hasAge, ageTimeStep,
                                                    updatedPopnFalse,
						    updatedBirthsFalse,
                                                    component_R, theta,
                                                    strucZeroArray,
                                                    iteratorComp_R,
                                                    iExpFirstOrig_r,
                                                    exposure,
                                                    iteratorExposure_R,
                                                    diffOrig,
						    firstOnlyFalse,
						    isSmallUpdateFinalFalse);

                    if (R_finite(diffLogOrig)) {

                        diffLogDest
                            = diffLogDensExpOneComp(iCellDest_r,
                                                    hasAge, ageTimeStep,
                                                    updatedPopnFalse,
						    updatedBirthsFalse,
                                                    component_R, theta,
                                                    strucZeroArray,
                                                    iteratorComp_R,
                                                    iExpFirstDest_r,
                                                    exposure,
                                                    iteratorExposure_R,
                                                    diffDest,
						    firstOnlyFalse,
						    isSmallUpdateFinalFalse);
                   }
                }/* end else */

                int isFinOrig = R_finite(diffLogOrig);

                if (isFinOrig && R_finite(diffLogDest)) {

                    ans += (diffLogOrig + diffLogDest);
                }
                else {
                    ans = (isFinOrig? diffLogDest : diffLogOrig);
                    break;
                    /* break out of loop through components */
                }

            } /* end modelUsesExposure */
        } /* end for loop through components */
    } /* end not noExposureAffected */
    /* if noExposureAffected ans will be default 0 */

    return ans;
}



double
diffLogDensExpOrigDestSmall(SEXP combined_R)
{
  SEXP account_R = GET_SLOT(combined_R, account_sym);
  SEXP components_R = GET_SLOT(account_R, components_sym);
  int nComponents = LENGTH(components_R);
  SEXP iteratorsComp_R = GET_SLOT(combined_R, iteratorsComp_sym);
  int * modelUsesExposure = LOGICAL(GET_SLOT(combined_R, modelUsesExposure_sym));
  SEXP mappingsFromExposure_R = GET_SLOT(combined_R, mappingsFromExp_sym);
  SEXP mappingsToExposure_R = GET_SLOT(combined_R, mappingsToExp_sym);
  int iExposeUpOrig_r = *INTEGER(GET_SLOT(combined_R, iExposure_sym));
  int iExposeLowOrig_r = *INTEGER(GET_SLOT(combined_R, iExposureOther_sym));
  int iExposeUpDest_r = *INTEGER(GET_SLOT(combined_R, iExpFirst_sym));
  int iExposeLowDest_r = *INTEGER(GET_SLOT(combined_R, iExpFirstOther_sym));
  if (iExposeUpOrig_r == iExposeUpDest_r)
    return 0;
  SEXP iteratorExposure_R = GET_SLOT(combined_R, iteratorExposure_sym);
  int diff = *INTEGER(GET_SLOT(combined_R, diffProp_sym));
  int iOrigDest_r = *INTEGER(GET_SLOT(combined_R, iOrigDest_sym));
  int iPool_r = *INTEGER(GET_SLOT(combined_R, iPool_sym));
  int iBirths_r = *INTEGER(GET_SLOT(combined_R, iBirths_sym));
  double * exposure = REAL(GET_SLOT(combined_R, exposure_sym));
  SEXP systemModels_R = GET_SLOT(combined_R, systemModels_sym);
  double ageTimeStep = *REAL(GET_SLOT(combined_R, ageTimeStep_sym));
  int isSmallUpdateFinal = *INTEGER(GET_SLOT(combined_R, isSmallUpdateFinal_sym));
  int diffUpOrig = -1 * diff;
  int diffLowOrig = diff;
  int diffUpDest = diff;
  int diffLowDest = -1 * diff;
  int hasAgeTrue = 1;
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
      int iCellUpOrig_r;
      int iCellLowOrig_r;
      int iCellUpDest_r;
      int iCellLowDest_r;
      int iExpFirstUpOrig_r;
      int iExpFirstLowOrig_r;
      int iExpFirstUpDest_r;
      int iExpFirstLowDest_r;
      double diffLogUpOrig;
      double diffLogLowOrig;
      double diffLogUpDest;
      double diffLogLowDest;
      if (isBirths) {
	iCellUpOrig_r = getICellBirthsFromExp(iExposeUpOrig_r, mappingFromExposure_R, 0);
	iCellLowOrig_r = getICellBirthsFromExp(iExposeLowOrig_r, mappingFromExposure_R, 0);
	iCellUpDest_r = getICellBirthsFromExp(iExposeUpDest_r, mappingFromExposure_R, 0);
	iCellLowDest_r = getICellBirthsFromExp(iExposeLowDest_r, mappingFromExposure_R, 0);
	SEXP mappingToExposure_R = VECTOR_ELT(mappingsToExposure_R, i);
	if (iCellUpOrig_r > 0) {
	  iExpFirstUpOrig_r = getIExposureFromBirths(iCellUpOrig_r, mappingToExposure_R);
	  iExpFirstUpDest_r = getIExposureFromBirths(iCellUpDest_r, mappingToExposure_R);
	}
	if (iCellLowOrig_r > 0) {
	  iExpFirstLowOrig_r = getIExposureFromBirths(iCellLowOrig_r, mappingToExposure_R);
	  iExpFirstLowDest_r = getIExposureFromBirths(iCellLowDest_r, mappingToExposure_R);
	}
      }
      else {
	iCellUpOrig_r = getICellCompFromExp(iExposeUpOrig_r, mappingFromExposure_R);
	iCellLowOrig_r = getICellCompFromExp(iExposeLowOrig_r, mappingFromExposure_R);
	iCellUpDest_r = getICellCompFromExp(iExposeUpDest_r, mappingFromExposure_R);
	iCellLowDest_r = getICellCompFromExp(iExposeLowDest_r, mappingFromExposure_R);
	iExpFirstUpOrig_r = iExposeUpOrig_r;
	iExpFirstLowOrig_r = iExposeLowOrig_r;
	iExpFirstUpDest_r = iExposeUpDest_r;
	iExpFirstLowDest_r = iExposeLowDest_r;
      }
      if (isOrigDest || isPool || isBirths) {
	if (!isBirths || (iCellUpOrig_r > 0)) {
	  diffLogUpOrig = diffLogDensExpOneOrigDestParChPool(iCellUpOrig_r,
							     hasAgeTrue, ageTimeStep,
							     updatedPopnFalse,
							     updatedBirthsFalse,
							     component_R, theta,
							     strucZeroArray,
							     iteratorComp_R,
							     iExpFirstUpOrig_r,
							     exposure,
							     iteratorExposure_R,
							     diffUpOrig,
							     firstOnlyTrue,
							     isSmallUpdateFinal);
	  diffLogUpDest = diffLogDensExpOneOrigDestParChPool(iCellUpDest_r,
							     hasAgeTrue, ageTimeStep,
							     updatedPopnFalse,
							     updatedBirthsFalse,
							     component_R, theta,
							     strucZeroArray,
							     iteratorComp_R,
							     iExpFirstUpDest_r,
							     exposure,
							     iteratorExposure_R,
							     diffUpDest,
							     firstOnlyTrue,
							     isSmallUpdateFinal);
	}
	else {
	  diffLogUpOrig = 0;
	  diffLogUpDest = 0;
	}
	if (!isBirths || (iCellLowOrig_r > 0)) {
	  diffLogLowOrig = diffLogDensExpOneOrigDestParChPool(iCellLowOrig_r,
							      hasAgeTrue, ageTimeStep,
							      updatedPopnFalse,
							      updatedBirthsFalse,
							      component_R, theta,
							      strucZeroArray,
							      iteratorComp_R,
							      iExpFirstLowOrig_r,
							      exposure,
							      iteratorExposure_R,
							      diffLowOrig,
							      firstOnlyTrue,
							      isSmallUpdateFinal);
	  diffLogLowDest = diffLogDensExpOneOrigDestParChPool(iCellLowDest_r,
							      hasAgeTrue, ageTimeStep,
							      updatedPopnFalse,
							      updatedBirthsFalse,
							      component_R, theta,
							      strucZeroArray,
							      iteratorComp_R,
							      iExpFirstLowDest_r,
							      exposure,
							      iteratorExposure_R,
							      diffLowDest,
							      firstOnlyTrue,
							      isSmallUpdateFinal);
	}
	else {
	  diffLogLowOrig = 0;
	  diffLogLowDest = 0;
	}
      }
      else {
	diffLogUpOrig = diffLogDensExpOneComp(iCellUpOrig_r,
					      hasAgeTrue, ageTimeStep,
					      updatedPopnFalse,
					      updatedBirthsFalse,
					      component_R, theta,
					      strucZeroArray,
					      iteratorComp_R,
					      iExpFirstUpOrig_r,
					      exposure,
					      iteratorExposure_R,
					      diffUpOrig,
					      firstOnlyTrue,
					      isSmallUpdateFinal);
	diffLogLowOrig = diffLogDensExpOneComp(iCellLowOrig_r,
					       hasAgeTrue, ageTimeStep,
					       updatedPopnFalse,
					       updatedBirthsFalse,
					       component_R, theta,
					       strucZeroArray,
					       iteratorComp_R,
					       iExpFirstLowOrig_r,
					       exposure,
					       iteratorExposure_R,
					       diffLowOrig,
					       firstOnlyTrue,
					       isSmallUpdateFinal);
	diffLogUpDest = diffLogDensExpOneComp(iCellUpDest_r,
					      hasAgeTrue, ageTimeStep,
					      updatedPopnFalse,
					      updatedBirthsFalse,
					      component_R, theta,
					      strucZeroArray,
					      iteratorComp_R,
					      iExpFirstUpDest_r,
					      exposure,
					      iteratorExposure_R,
					      diffUpDest,
					      firstOnlyTrue,
					      isSmallUpdateFinal);
	diffLogLowDest = diffLogDensExpOneComp(iCellLowDest_r,
					       hasAgeTrue, ageTimeStep,
					       updatedPopnFalse,
					       updatedBirthsFalse,
					       component_R, theta,
					       strucZeroArray,
					       iteratorComp_R,
					       iExpFirstLowDest_r,
					       exposure,
					       iteratorExposure_R,
					       diffLowDest,
					       firstOnlyTrue,
					       isSmallUpdateFinal);
      }
      if (R_finite(diffLogUpOrig)) {
	ans += diffLogUpOrig;
      }
      else {
	ans = diffLogUpOrig;
	break;
      }
      if (R_finite(diffLogLowOrig)) {
	ans += diffLogLowOrig;
      }
      else {
	ans = diffLogLowOrig;
	break;
      }
      if (R_finite(diffLogUpDest)) {
	ans += diffLogUpDest;
      }
      else {
	ans = diffLogUpDest;
	break;
      }
      if (R_finite(diffLogLowDest)) {
	ans += diffLogLowDest;
      }
      else {
	ans = diffLogLowDest;
	break;
      }        } /* end modelUsesExposure */
  } /* end for loop through components */
  return ans;
}




double
diffLogDensJumpPoolWithExpose(SEXP combined_R)
{
    SEXP account_R = GET_SLOT(combined_R, account_sym);

    int iComp_r = *INTEGER(GET_SLOT(combined_R, iComp_sym));
    SEXP components_R = GET_SLOT(account_R, components_sym);
    SEXP component_R = VECTOR_ELT(components_R, iComp_r - 1);
    int * component = INTEGER(component_R);

    SEXP systemModels_R = GET_SLOT(combined_R, systemModels_sym);
    SEXP thisSystemModel_R = VECTOR_ELT(systemModels_R, iComp_r); /* iComp_r is c-style index + 1*/
    SEXP theta_R = GET_SLOT(thisSystemModel_R, theta_sym);
    double * theta = REAL(theta_R);

    double * exposure = REAL(GET_SLOT(combined_R, exposure_sym));
    double * expectedExposure = REAL(GET_SLOT(combined_R, expectedExposure_sym));

    int iCellOut_r = *INTEGER(GET_SLOT(combined_R, iCell_sym));
    int iCellIn_r = *INTEGER(GET_SLOT(combined_R, iCellOther_sym));
    int iExposureOut_r = *INTEGER(GET_SLOT(combined_R, iExposure_sym));
    int iExposureIn_r = *INTEGER(GET_SLOT(combined_R, iExposureOther_sym));

    SEXP iterators_R = GET_SLOT(combined_R, iteratorsComp_sym);
    SEXP iterator_R = VECTOR_ELT(iterators_R, iComp_r - 1);

    int hasAge = *LOGICAL(GET_SLOT(iterator_R, hasAge_sym));

    int diff = *INTEGER(GET_SLOT(combined_R, diffProp_sym));

    double ageTimeStep = *REAL(GET_SLOT(combined_R, ageTimeStep_sym));

    int iCellOut = iCellOut_r - 1;
    int iCellIn = iCellIn_r - 1;
    int iExposureOut = iExposureOut_r - 1;
    int iExposureIn = iExposureIn_r - 1;

    double thetaOut = theta[iCellOut];
    double thetaIn = theta[iCellIn];
    double exposureOutCurr = exposure[iExposureOut];
    double exposureInCurr = exposure[iExposureIn];
    double exposureOutJump = expectedExposure[iExposureOut];

    double exposureOutProp = exposureOutCurr;
    double exposureInProp = exposureInCurr;
    double tolExposure = 0.000001;

    double incrExpose = 0;

    int keepGoing = 1;
    double ans = 0;

    if (hasAge) {
      resetCC(iterator_R, iCellOut_r);
      int nAge = *INTEGER(GET_SLOT(iterator_R, nAge_sym));
      int iAge = *INTEGER(GET_SLOT(iterator_R, iAge_sym));
      int iTri = *INTEGER(GET_SLOT(iterator_R, iTriangle_sym));
      int isFinal = iAge == nAge;
      int isUpper = iTri == 2;
      if (isFinal) {
	if (isUpper)
	  incrExpose -= 0.5 * diff * ageTimeStep;
	else
	  incrExpose -= (1.0/3.0) * diff * ageTimeStep;
      }
      else {
	if (isUpper)
	  incrExpose -= (1.0/6.0) * diff * ageTimeStep;
	else
	  incrExpose -= (1.0/3.0) * diff * ageTimeStep;
      }
    }
    else {
	  incrExpose -= 0.5 * diff * ageTimeStep;
    }
    
    exposureOutProp += incrExpose;
    exposureInProp -= incrExpose;

    if (exposureOutProp < tolExposure) {
      if (exposureOutProp > -1 * tolExposure)
	exposureOutProp = 0;
      else {
	if (exposureOutCurr < 0) {
	  ans = R_NegInf;
	  keepGoing = 0;
	}
	else
	  error("negative value for 'exposureOutProp' : %f", exposureOutProp);
      }
    }
    if (exposureInProp < tolExposure) {
      if (exposureInProp > -1 * tolExposure)
	exposureInProp = 0;
      else {
        if (exposureInCurr < 0) {
	  ans = R_NegInf;
	  keepGoing = 0;
	}
	else
	  error("negative value for 'exposureInProp' : %f", exposureInProp);
      }
    }

    if (keepGoing) {
      int valOutCurr = component[iCellOut];
      int valInCurr = component[iCellIn];
      int valOutProp = valOutCurr + diff;
      int valInProp = valInCurr + diff;
      if (((valOutProp > 0) && !(exposureOutProp > 0))
	  || ((valInProp > 0) && !(exposureInProp > 0)))
	ans = R_NegInf;
      else {
	double lambdaDensOutProp = thetaOut * exposureOutProp;
	double lambdaDensInProp = thetaIn * exposureInProp;
	double lambdaDensOutCurr = thetaOut * exposureOutCurr;
	double lambdaDensInCurr = thetaIn * exposureInCurr;
	double lambdaJump = thetaOut * exposureOutJump;
	double diffLogDens = dpois(valOutProp, lambdaDensOutProp, USE_LOG)
	  - dpois(valOutCurr, lambdaDensOutCurr, USE_LOG)
	  + dpois(valInProp, lambdaDensInProp, USE_LOG)
	  - dpois(valInCurr, lambdaDensInCurr, USE_LOG);
	double diffLogJump = dpois(valOutCurr, lambdaJump, USE_LOG)
	  - dpois(valOutProp, lambdaJump, USE_LOG);
	ans = diffLogDens + diffLogJump;
      }
    }
    
    return ans;
}


double
diffLogDensJumpPoolNoExpose(SEXP combined_R)
{
    SEXP account_R = GET_SLOT(combined_R, account_sym);

    int iComp_r = *INTEGER(GET_SLOT(combined_R, iComp_sym));
    SEXP components_R = GET_SLOT(account_R, components_sym);
    SEXP component_R = VECTOR_ELT(components_R, iComp_r - 1);
    int * component = INTEGER(component_R);

    SEXP systemModels_R = GET_SLOT(combined_R, systemModels_sym);
    SEXP thisSystemModel_R = VECTOR_ELT(systemModels_R, iComp_r); /* iComp_r is c-style index + 1*/
    SEXP theta_R = GET_SLOT(thisSystemModel_R, theta_sym);
    double * theta = REAL(theta_R);

    int iCellIn_r = *INTEGER(GET_SLOT(combined_R, iCellOther_sym));

    int diff = *INTEGER(GET_SLOT(combined_R, diffProp_sym));

    int iCellIn = iCellIn_r - 1;

    double thetaIn = theta[iCellIn];
    int valInCurr = component[iCellIn];
    int valInProp = valInCurr + diff;

    double ans = dpois(valInProp, thetaIn, USE_LOG)
                          - dpois(valInCurr, thetaIn, USE_LOG);

    return ans;
}


double
diffLogDensJumpNet(SEXP combined_R)
{
    SEXP account_R = GET_SLOT(combined_R, account_sym);

    int iComp_r = *INTEGER(GET_SLOT(combined_R, iComp_sym));
    SEXP components_R = GET_SLOT(account_R, components_sym);
    SEXP component_R = VECTOR_ELT(components_R, iComp_r - 1);
    int * component = INTEGER(component_R);

    SEXP systemModels_R = GET_SLOT(combined_R, systemModels_sym);
    SEXP model_R = VECTOR_ELT(systemModels_R, iComp_r); /* iComp_r is c-style index + 1*/
    SEXP theta_R = GET_SLOT(model_R, theta_sym);
    double * theta = REAL(theta_R);
    double varsigma = *REAL(GET_SLOT(model_R, varsigma_sym));
    double * w = REAL(GET_SLOT(model_R, w_sym));

    int iCellSub_r = *INTEGER(GET_SLOT(combined_R, iCellOther_sym));

    int diff = *INTEGER(GET_SLOT(combined_R, diffProp_sym));

    int iCellSub = iCellSub_r - 1;
    double meanSub = theta[iCellSub];
    double wSub = w[iCellSub];
    double sdSub = varsigma/sqrt(wSub);
    int valSubCurr = component[iCellSub];
    int valSubProp = valSubCurr - diff;

    double ans = dnorm(valSubProp, meanSub, sdSub, USE_LOG)
                          - dnorm(valSubCurr, meanSub, sdSub, USE_LOG);

    return ans;
}


double
diffLogDensJumpComp(SEXP combined_R)
{
    SEXP account_R = GET_SLOT(combined_R, account_sym);

    int iComp_r = *INTEGER(GET_SLOT(combined_R, iComp_sym));
    SEXP components_R = GET_SLOT(account_R, components_sym);
    SEXP component_R = VECTOR_ELT(components_R, iComp_r - 1);
    int * component = INTEGER(component_R);

    int iBirths_r = *INTEGER(GET_SLOT(combined_R, iBirths_sym));
    int isBirths = iComp_r == iBirths_r;

    SEXP systemModels_R = GET_SLOT(combined_R, systemModels_sym);
    SEXP thisSystemModel_R = VECTOR_ELT(systemModels_R, iComp_r); /* iComp_r is c-style index + 1*/
    SEXP theta_R = GET_SLOT(thisSystemModel_R, theta_sym);
    double * theta = REAL(theta_R);

    double * exposure = REAL(GET_SLOT(combined_R, exposure_sym));
    double * expectedExposure = REAL(GET_SLOT(combined_R, expectedExposure_sym));

    int iCell_r = *INTEGER(GET_SLOT(combined_R, iCell_sym));
    int iExposure_r = *INTEGER(GET_SLOT(combined_R, iExposure_sym));
    int iExpFirst_r = *INTEGER(GET_SLOT(combined_R, iExpFirst_sym));

    SEXP iterators_R = GET_SLOT(combined_R, iteratorsComp_sym);
    SEXP iterator_R = VECTOR_ELT(iterators_R, iComp_r - 1);

    int diff = *INTEGER(GET_SLOT(combined_R, diffProp_sym));

    int hasAge = *LOGICAL(GET_SLOT(combined_R, hasAge_sym));
    double ageTimeStep = *REAL(GET_SLOT(combined_R, ageTimeStep_sym));

    int * isIncrementVec = LOGICAL(GET_SLOT(combined_R, isIncrement_sym));

    int iCell = iCell_r - 1;
    int iExposure = iExposure_r - 1;

    double thetaCell = theta[iCell];
    double exposureCellCurr = exposure[iExposure];
    double exposureCellJump = expectedExposure[iExposure];

    double exposureCellProp = 0;
    double tolExposure = 0.000001;

    int isIncrement = isIncrementVec[iComp_r -1];

    double incrExp = 0;
    int expChanges = 1;

    int keepGoing = 1;

    double ans = 0;

    if (isBirths)
      expChanges = (iExpFirst_r != 0) && (iExpFirst_r == iExposure_r);

    if (expChanges) {
      if (hasAge) {
	resetCC(iterator_R, iCell_r);
	int nAge = *INTEGER(GET_SLOT(iterator_R, nAge_sym));
	int lastAgeGroupOpen = *INTEGER(GET_SLOT(iterator_R, lastAgeGroupOpen_sym));
	int iAge = *INTEGER(GET_SLOT(iterator_R, iAge_sym));
	int iTri = *INTEGER(GET_SLOT(iterator_R, iTriangle_sym));
	int isFinal = iAge == nAge;
	int isUpper = iTri == 2;
	if (isFinal && lastAgeGroupOpen) {
	  if (isUpper)
	    incrExp = 0.5 * ageTimeStep * diff;
	  else
	    incrExp = (1.0/3.0) * ageTimeStep * diff;
	}
	else {
	  if (isUpper)
	    incrExp = (1.0/6.0) * ageTimeStep * diff;
	  else
	    incrExp = (1.0/3.0) * ageTimeStep * diff;
	}
      }
      else {
	incrExp = 0.5 * ageTimeStep * diff;
      }
      if (!isIncrement)
	incrExp = -1 * incrExp;
    }

    exposureCellProp = exposureCellCurr + incrExp;

    if (exposureCellProp < tolExposure) {
      if (exposureCellProp > -1 * tolExposure)
	exposureCellProp = 0;
      else {
	if (exposureCellCurr < 0) {
	  ans = R_NegInf;
	  keepGoing = 0;
	}
	else 
	  error("negative value for 'exposureCellProp' : %f", exposureCellProp);
      }
    }

    if (keepGoing) {
      int valCurr = component[iCell];
      int valProp = valCurr + diff;
      if ((valProp > 0) && !(exposureCellProp > 0)) {
	ans = R_NegInf;
      }
      else  {
	double lambdaDensProp = thetaCell * exposureCellProp;
	double lambdaJump = thetaCell * exposureCellJump;
	double diffLogDens = dpois(valProp, lambdaDensProp, USE_LOG)
	  - dpois(valCurr, lambdaDensProp, USE_LOG);
	double diffLogJump = dpois(valCurr, lambdaJump, USE_LOG)
	  - dpois(valProp, lambdaJump, USE_LOG);
	ans = diffLogDens + diffLogJump;
      }
    }

    return ans;
}


double
diffLogDensExpComp(SEXP combined_R)
{

    SEXP account_R = GET_SLOT(combined_R, account_sym);
    SEXP components_R = GET_SLOT(account_R, components_sym);
    int nComponents = LENGTH(components_R);

    SEXP iteratorsComp_R = GET_SLOT(combined_R, iteratorsComp_sym);

    int * modelUsesExposure = LOGICAL(GET_SLOT(combined_R, modelUsesExposure_sym));
    SEXP mappingsFromExposure_R = GET_SLOT(combined_R, mappingsFromExp_sym);
    SEXP mappingsToExposure_R = GET_SLOT(combined_R, mappingsToExp_sym);

    int iExpFirst_r = *INTEGER(GET_SLOT(combined_R, iExpFirst_sym));

    SEXP iteratorExposure_R = GET_SLOT(combined_R, iteratorExposure_sym);

    int * isIncrementVec = LOGICAL(GET_SLOT(combined_R, isIncrement_sym));

    int diff = *INTEGER(GET_SLOT(combined_R, diffProp_sym));

    int iComp_r = *INTEGER(GET_SLOT(combined_R, iComp_sym));
    int iOrigDest_r = *INTEGER(GET_SLOT(combined_R, iOrigDest_sym));
    int iPool_r = *INTEGER(GET_SLOT(combined_R, iPool_sym));
    int iBirths_r = *INTEGER(GET_SLOT(combined_R, iBirths_sym));

    double * exposure = REAL(GET_SLOT(combined_R, exposure_sym));

    SEXP systemModels_R = GET_SLOT(combined_R, systemModels_sym);

    int hasAge = *LOGICAL(GET_SLOT(combined_R, hasAge_sym));
    double ageTimeStep = *REAL(GET_SLOT(combined_R, ageTimeStep_sym));

    int updatedPopnFalse = 0;
    int firstOnlyFalse = 0;
    int isSingleUpdateFinalFalse = 0;
    int updatedBirths = iComp_r == iBirths_r;

    double ans = 0;

    int noExposureAffected = (iExpFirst_r == 0) ;
    if(!noExposureAffected) {

        int isIncrement = isIncrementVec[iComp_r - 1];
        if ( !isIncrement ) {
            diff = -diff;
        }

        for(int i = 0; i < nComponents; ++i) {

            int thisModelUsesExposure = modelUsesExposure[i + 1];
            if(thisModelUsesExposure) {
                SEXP component_R = VECTOR_ELT(components_R, i);
                SEXP thisSystemModel_R = VECTOR_ELT(systemModels_R, i+1);
                double * theta = REAL(GET_SLOT(thisSystemModel_R, theta_sym));
                int * strucZeroArray = INTEGER(GET_SLOT(thisSystemModel_R, strucZeroArray_sym));
                SEXP iteratorComp_R = VECTOR_ELT(iteratorsComp_R, i);
                SEXP mappingFromExposure_R = VECTOR_ELT(mappingsFromExposure_R, i);

                int isOrigDest = (i == iOrigDest_r - 1);
                int isPool = (i == iPool_r - 1);
                int isBirths = (i == iBirths_r - 1);

                double diffLog = 0;

                if (isOrigDest || isPool) {

		  int iCell_r = getICellCompFromExp(iExpFirst_r,
						    mappingFromExposure_R);
		  diffLog = diffLogDensExpOneOrigDestParChPool(iCell_r,
							       hasAge, ageTimeStep,
							       updatedPopnFalse,
							       updatedBirths,
							       component_R, theta,
							       strucZeroArray,
							       iteratorComp_R,
							       iExpFirst_r,
							       exposure,
							       iteratorExposure_R,
							       diff,
							       firstOnlyFalse,
							       isSingleUpdateFinalFalse);
                } /* end if isOrigDest || isPool */

                else if(isBirths) {
                    int iCell_r = getICellBirthsFromExp(iExpFirst_r, mappingFromExposure_R, 1);

                    int cellIsAffected = (iCell_r > 0);
                    if(cellIsAffected) {

		    SEXP mappingToExposure_R = VECTOR_ELT(mappingsToExposure_R, i);
		    int iExpFirstBirths_R = getIExposureFromBirths(iCell_r, mappingToExposure_R);

		    diffLog
		      = diffLogDensExpOneOrigDestParChPool(iCell_r,
							   hasAge, ageTimeStep,
							   updatedPopnFalse,
							   updatedBirths,
							   component_R, theta,
							   strucZeroArray,
							   iteratorComp_R,
							   iExpFirstBirths_R,
							   exposure,
							   iteratorExposure_R,
							   diff,
							   firstOnlyFalse,
							   isSingleUpdateFinalFalse);

                    } /* end cellIsAffected */

                } /* end if isBirths */

                else { /* is net */

                    int iCell_r = getICellCompFromExp(iExpFirst_r,
                                                mappingFromExposure_R);
                    diffLog = diffLogDensExpOneComp(iCell_r,
                                                    hasAge, ageTimeStep,
                                                    updatedPopnFalse,
						    updatedBirths,
                                                    component_R, theta,
                                                    strucZeroArray,
                                                    iteratorComp_R,
                                                    iExpFirst_r,
                                                    exposure,
                                                    iteratorExposure_R,
                                                    diff,
						    firstOnlyFalse,
						    isSingleUpdateFinalFalse);
                }/* end else */

                if (R_finite(diffLog)) {

                    ans += diffLog;
                }
                else {
                    ans = diffLog;
                    break;
                    /* break out of loop through components */
                }
            } /* end modelUsesExposure */
        } /* end for loop through components */
    } /* end not noExposureAffected */
    /* if noExposureAffected ans will be default 0 */

    return ans;
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
  SEXP iteratorExposure_R = GET_SLOT(combined_R, iteratorExposure_sym);
  int diff = *INTEGER(GET_SLOT(combined_R, diffProp_sym));
  int iComp_r = *INTEGER(GET_SLOT(combined_R, iComp_sym));
  int iOrigDest_r = *INTEGER(GET_SLOT(combined_R, iOrigDest_sym));
  int iPool_r = *INTEGER(GET_SLOT(combined_R, iPool_sym));
  int iBirths_r = *INTEGER(GET_SLOT(combined_R, iBirths_sym));
  double * exposure = REAL(GET_SLOT(combined_R, exposure_sym));
  SEXP systemModels_R = GET_SLOT(combined_R, systemModels_sym);
  double ageTimeStep = *REAL(GET_SLOT(combined_R, ageTimeStep_sym));
  int isSmallUpdateFinal = *INTEGER(GET_SLOT(combined_R, isSmallUpdateFinal_sym));
  int isIncrement = isIncrementVec[iComp_r - 1];
  int diffUp = diff;
  int diffLow = -1 * diff;
  if (!isIncrement) {
    diffUp = -1 * diffUp;
    diffLow = -1 * diffLow;
  }
  int hasAgeTrue = 1;
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
	iCellUp_r = getICellBirthsFromExp(iExposeUp_r, mappingFromExposure_R, 0);
	iCellLow_r = getICellBirthsFromExp(iExposeLow_r, mappingFromExposure_R, 0);
	SEXP mappingToExposure_R = VECTOR_ELT(mappingsToExposure_R, i);
	if (iCellUp_r > 0) {
	  iExpFirstUp_r = getIExposureFromBirths(iCellUp_r, mappingToExposure_R);
	}
	if (iCellLow_r > 0) {
	  iExpFirstLow_r = getIExposureFromBirths(iCellLow_r, mappingToExposure_R);
	}
      }
      else {
	iCellUp_r = getICellCompFromExp(iExposeUp_r, mappingFromExposure_R);
	iCellLow_r = getICellCompFromExp(iExposeLow_r, mappingFromExposure_R);
	iExpFirstUp_r = iExposeUp_r;
	iExpFirstLow_r = iExposeLow_r;
      }
      if (isOrigDest || isPool || isBirths) {
	if (!isBirths || (iCellUp_r > 0)) {
	  diffLogUp = diffLogDensExpOneOrigDestParChPool(iCellUp_r,
							 hasAgeTrue, ageTimeStep,
							 updatedPopnFalse,
							 updatedBirthsFalse,
							 component_R, theta,
							 strucZeroArray,
							 iteratorComp_R,
							 iExpFirstUp_r,
							 exposure,
							 iteratorExposure_R,
							 diffUp,
							 firstOnlyTrue,
							 isSmallUpdateFinal);
	}
	else {
	  diffLogUp = 0;
	}
	if (!isBirths || (iCellLow_r > 0)) {
	  diffLogLow = diffLogDensExpOneOrigDestParChPool(iCellLow_r,
							  hasAgeTrue, ageTimeStep,
							  updatedPopnFalse,
							  updatedBirthsFalse,
							  component_R, theta,
							  strucZeroArray,
							  iteratorComp_R,
							  iExpFirstLow_r,
							  exposure,
							  iteratorExposure_R,
							  diffLow,
							  firstOnlyTrue,
							  isSmallUpdateFinal);
	}
	else {
	  diffLogLow = 0;
	}
      }
      else {
	diffLogUp = diffLogDensExpOneComp(iCellUp_r,
					  hasAgeTrue, ageTimeStep,
					  updatedPopnFalse,
					  updatedBirthsFalse,
					  component_R, theta,
					  strucZeroArray,
					  iteratorComp_R,
					  iExpFirstUp_r,
					  exposure,
					  iteratorExposure_R,
					  diffUp,
					  firstOnlyTrue,
					  isSmallUpdateFinal);
	diffLogLow = diffLogDensExpOneComp(iCellLow_r,
					   hasAgeTrue, ageTimeStep,
					   updatedPopnFalse,
					   updatedBirthsFalse,
					   component_R, theta,
					   strucZeroArray,
					   iteratorComp_R,
					   iExpFirstLow_r,
					   exposure,
					   iteratorExposure_R,
					   diffLow,
					   firstOnlyTrue,
					   isSmallUpdateFinal);
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





double
diffLogDensJumpBirthsSmall(SEXP combined_R)
{
  int iComp = *INTEGER(GET_SLOT(combined_R, iComp_sym)) - 1;
  SEXP account_R = GET_SLOT(combined_R, account_sym);
  SEXP components_R = GET_SLOT(account_R, components_sym);
  SEXP component_R = VECTOR_ELT(components_R, iComp);
  int * component = INTEGER(component_R);
  int i_cell_up = *INTEGER(GET_SLOT(combined_R, iCell_sym)) - 1;
  int i_cell_low = *INTEGER(GET_SLOT(combined_R, iCellOther_sym)) - 1;
  int * usesExposure_vec = LOGICAL(GET_SLOT(combined_R, modelUsesExposure_sym));
  int usesExposure = usesExposure_vec[iComp + 1];
  int diff = *INTEGER(GET_SLOT(combined_R, diffProp_sym));
  SEXP systemModels_R = GET_SLOT(combined_R, systemModels_sym);
  SEXP thisSystemModel_R = VECTOR_ELT(systemModels_R, iComp + 1);
  SEXP theta_R = GET_SLOT(thisSystemModel_R, theta_sym);
  double * theta = REAL(theta_R);
  int val_up_curr = component[i_cell_up];
  int val_low_curr = component[i_cell_low];
  int val_up_prop = val_up_curr + diff;
  int val_low_prop = val_low_curr - diff;
  double val_up_expect = theta[i_cell_up];
  double val_low_expect = theta[i_cell_low];
  int val_curr_invalid = 0;
  int val_prop_invalid = 0;
  double ans;
  if (usesExposure) {
    double * exposure = REAL(GET_SLOT(combined_R, exposure_sym));
    int i_expose_up = *INTEGER(GET_SLOT(combined_R, iExposure_sym)) - 1;
    int i_expose_low = *INTEGER(GET_SLOT(combined_R, iExposureOther_sym)) - 1;
    double expose_up = exposure[i_expose_up];
    double expose_low = exposure[i_expose_low];
    val_prop_invalid = (((val_up_prop > 0) && !(expose_up > 0))
			|| ((val_low_prop > 0) && !(expose_low > 0)));
    val_curr_invalid = (((val_up_curr > 0) && !(expose_up > 0))
			|| ((val_low_curr > 0) && !(expose_low > 0)));
    val_up_expect *= expose_up;
    val_low_expect *= expose_low;
  }
  if (val_prop_invalid) {
    ans = R_NegInf;
  }
  else if (val_curr_invalid) {
    ans = R_PosInf;
  }
  else {
    double ans_dens_prop = dpois(val_up_prop, val_up_expect, USE_LOG) +
      dpois(val_low_prop, val_low_expect, USE_LOG);
    double ans_dens_curr = dpois(val_up_curr, val_up_expect, USE_LOG) +
      dpois(val_low_curr, val_low_expect, USE_LOG);
    ans = ans_dens_prop - ans_dens_curr;
  }
  return ans;
}


double
diffLogDensJumpOrigDestSmall(SEXP combined_R)
{
  int iComp = *INTEGER(GET_SLOT(combined_R, iComp_sym)) - 1;
  SEXP account_R = GET_SLOT(combined_R, account_sym);
  SEXP components_R = GET_SLOT(account_R, components_sym);
  SEXP component_R = VECTOR_ELT(components_R, iComp);
  int * component = INTEGER(component_R);
  SEXP systemModels_R = GET_SLOT(combined_R, systemModels_sym);
  SEXP systemModel_R = VECTOR_ELT(systemModels_R, iComp + 1);
  double * theta = REAL(GET_SLOT(systemModel_R, theta_sym));
  SEXP iterators_R = GET_SLOT(combined_R, iteratorsComp_sym);
  SEXP iterator_R = VECTOR_ELT(iterators_R, iComp);
  SEXP iteratorDup_R = NULL;
  PROTECT(iteratorDup_R = iterator_R);
  int i_cell_up = *INTEGER(GET_SLOT(combined_R, iCell_sym)) - 1;
  int i_cell_low = *INTEGER(GET_SLOT(combined_R, iCellOther_sym)) - 1;
  int * usesExposure_vec = LOGICAL(GET_SLOT(combined_R, modelUsesExposure_sym));
  int usesExposure = usesExposure_vec[iComp + 1];
  int diff = *INTEGER(GET_SLOT(combined_R, diffProp_sym));
  double ageTimeStep = *REAL(GET_SLOT(combined_R, ageTimeStep_sym));
  double tolExposure = 0.000001;
  int val_up_curr = component[i_cell_up];
  int val_low_curr = component[i_cell_low];
  int val_up_prop = val_up_curr + diff;
  int val_low_prop = val_low_curr - diff;
  double val_up_expect_curr = theta[i_cell_up];
  double val_low_expect_curr = theta[i_cell_low];
  double val_up_expect_prop = val_up_expect_curr;
  double val_low_expect_prop = val_low_expect_curr;
  int val_prop_invalid = 0;
  int val_curr_invalid = 0;
  int keepGoing = 1;
  double ans = 0;
  if (usesExposure) {
    double * exposure = REAL(GET_SLOT(combined_R, exposure_sym));
    int i_expose_up = *INTEGER(GET_SLOT(combined_R, iExposure_sym)) - 1;
    int i_expose_low = *INTEGER(GET_SLOT(combined_R, iExposureOther_sym)) - 1;
    double expose_up_curr = exposure[i_expose_up];
    double expose_low_curr = exposure[i_expose_low];
    double expose_up_prop = expose_up_curr;
    double expose_low_prop = expose_low_curr;
    resetCC(iteratorDup_R, i_cell_up + 1);
    int nAge = *INTEGER(GET_SLOT(iteratorDup_R, nAge_sym));
    int lastAgeGroupOpen = *INTEGER(GET_SLOT(iteratorDup_R, lastAgeGroupOpen_sym));
    int iAge_r = *INTEGER(GET_SLOT(iteratorDup_R, iAge_sym));
    int isFinal = iAge_r == nAge;
    if (isFinal && lastAgeGroupOpen) {
      expose_up_prop -= 0.5 * ageTimeStep * diff;
      expose_low_prop += (1.0/3.0) * ageTimeStep * diff;
    }
    else {
      expose_up_prop -= (1.0/6.0) * ageTimeStep * diff;
      expose_low_prop -= (1.0/6.0) * ageTimeStep * diff;
    }
    if (expose_up_prop < tolExposure) {
      if (expose_up_prop > -1 * tolExposure)
	expose_up_prop = 0;
      else {
	if (expose_up_curr < 0) {
	  ans = R_NegInf;
	  keepGoing = 0;
	}
	else
	  error("negative value for 'expose_up_prop' : %f", expose_up_prop);
      }
    }
    if (expose_low_prop < tolExposure) {
      if (expose_low_prop > -1 * tolExposure)
	expose_low_prop = 0;
      else {
	if (expose_low_curr < 0) {
	  ans = R_NegInf;
	  keepGoing = 0;
	}
	else
	  error("negative value for 'expose_low_prop' : %f", expose_low_prop);
      }
      val_prop_invalid = (((val_up_prop > 0) && !(expose_up_prop > 0))
			  || ((val_low_prop > 0) && !(expose_low_prop > 0)));
      val_curr_invalid = (((val_up_curr > 0) && !(expose_up_curr > 0))
			  || ((val_low_curr > 0) && !(expose_low_curr > 0)));
    }
    val_up_expect_curr *= expose_up_curr;
    val_low_expect_curr *= expose_low_curr;
    val_up_expect_prop *= expose_up_prop;
    val_low_expect_prop *= expose_low_prop;
  }
  if (keepGoing) {
    if (val_prop_invalid) {
      ans = R_NegInf;
    }
    else if (val_curr_invalid) {
      ans = R_PosInf;
    }
    else {
      double ans_dens_prop = dpois(val_up_prop, val_up_expect_prop, USE_LOG) +
	dpois(val_low_prop, val_low_expect_prop, USE_LOG);
      double ans_dens_curr = dpois(val_up_curr, val_up_expect_curr, USE_LOG) +
	dpois(val_low_curr, val_low_expect_curr, USE_LOG);
      ans = ans_dens_prop - ans_dens_curr;
    }
  }
  UNPROTECT(1);
  return ans;
}


double
diffLogDensJumpCompSmall(SEXP combined_R)
{
  int iComp = *INTEGER(GET_SLOT(combined_R, iComp_sym)) - 1;
  SEXP account_R = GET_SLOT(combined_R, account_sym);
  SEXP components_R = GET_SLOT(account_R, components_sym);
  SEXP component_R = VECTOR_ELT(components_R, iComp);
  int * component = INTEGER(component_R);
  SEXP systemModels_R = GET_SLOT(combined_R, systemModels_sym);
  SEXP systemModel_R = VECTOR_ELT(systemModels_R, iComp + 1);
  double * theta = REAL(GET_SLOT(systemModel_R, theta_sym));
  int * isIncrementVec = LOGICAL(GET_SLOT(combined_R, isIncrement_sym));
  int isIncrement = isIncrementVec[iComp];
  SEXP iterators_R = GET_SLOT(combined_R, iteratorsComp_sym);
  SEXP iterator_R = VECTOR_ELT(iterators_R, iComp);
  SEXP iteratorDup_R = NULL;
  PROTECT(iteratorDup_R = iterator_R);
  int i_cell_up = *INTEGER(GET_SLOT(combined_R, iCell_sym)) - 1;
  int i_cell_low = *INTEGER(GET_SLOT(combined_R, iCellOther_sym)) - 1;
  int * usesExposure_vec = LOGICAL(GET_SLOT(combined_R, modelUsesExposure_sym));
  int usesExposure = usesExposure_vec[iComp + 1];
  int diff = *INTEGER(GET_SLOT(combined_R, diffProp_sym));
  double ageTimeStep = *REAL(GET_SLOT(combined_R, ageTimeStep_sym));
  double tolExposure = 0.000001;
  int val_up_curr = component[i_cell_up];
  int val_low_curr = component[i_cell_low];
  int val_up_prop = val_up_curr + diff;
  int val_low_prop = val_low_curr - diff;
  double val_up_expect_curr = theta[i_cell_up];
  double val_low_expect_curr = theta[i_cell_low];
  double val_up_expect_prop = val_up_expect_curr;
  double val_low_expect_prop = val_low_expect_curr;
  int val_prop_invalid = 0;
  int val_curr_invalid = 0;
  int keepGoing = 1;
  double ans = 0;
  if (usesExposure) {
    double * exposure = REAL(GET_SLOT(combined_R, exposure_sym));
    int i_expose_up = *INTEGER(GET_SLOT(combined_R, iExposure_sym)) - 1;
    int i_expose_low = *INTEGER(GET_SLOT(combined_R, iExposureOther_sym)) - 1;
    double expose_up_curr = exposure[i_expose_up];
    double expose_low_curr = exposure[i_expose_low];
    double expose_up_prop = expose_up_curr;
    double expose_low_prop = expose_low_curr;
      resetCC(iteratorDup_R, i_cell_up + 1);
      int nAge = *INTEGER(GET_SLOT(iteratorDup_R, nAge_sym));
      int lastAgeGroupOpen = *INTEGER(GET_SLOT(iteratorDup_R, lastAgeGroupOpen_sym));
      int iAge_r = *INTEGER(GET_SLOT(iteratorDup_R, iAge_sym));
      int isFinal = iAge_r == nAge;
      int sign = isIncrement ? 1 : -1;
      if (isFinal && lastAgeGroupOpen) {
	expose_up_prop += 0.5 * sign * ageTimeStep * diff;
	expose_low_prop -= (1.0/3.0) * sign * ageTimeStep * diff;
      }
      else {
	expose_up_prop += (1.0/6.0) * sign * ageTimeStep * diff;
	expose_low_prop += (1.0/6.0) * sign * ageTimeStep * diff;
      }
      if (expose_up_prop < tolExposure) {
	if (expose_up_prop > -1 * tolExposure)
	  expose_up_prop = 0;
	else {
	  if (expose_up_curr < 0) {
	    ans = R_NegInf;
	    keepGoing = 0;
	  }
	  else
	    error("negative value for 'expose_up_prop' : %f", expose_up_prop);
	}
      }
      if (expose_low_prop < tolExposure) {
	if (expose_low_prop > -1 * tolExposure)
	  expose_low_prop = 0;
	else {
	  if (expose_low_curr < 0) {
	    ans = R_NegInf;
	    keepGoing = 0;
	  }
	  else
	    error("negative value for 'expose_low_prop' : %f", expose_low_prop);
	}
	val_prop_invalid = (((val_up_prop > 0) && !(expose_up_prop > 0))
			    || ((val_low_prop > 0) && !(expose_low_prop > 0)));
	val_curr_invalid = (((val_up_curr > 0) && !(expose_up_curr > 0))
			    || ((val_low_curr > 0) && !(expose_low_curr > 0)));
      }
      val_up_expect_curr *= expose_up_curr;
      val_low_expect_curr *= expose_low_curr;
      val_up_expect_prop *= expose_up_prop;
      val_low_expect_prop *= expose_low_prop;
  }
  if (keepGoing) {
    if (val_prop_invalid) {
      ans = R_NegInf;
    }
    else if (val_curr_invalid) {
      ans = R_PosInf;
    }
    else {
      double ans_dens_prop = dpois(val_up_prop, val_up_expect_prop, USE_LOG) +
	dpois(val_low_prop, val_low_expect_prop, USE_LOG);
      double ans_dens_curr = dpois(val_up_curr, val_up_expect_curr, USE_LOG) +
	dpois(val_low_curr, val_low_expect_curr, USE_LOG);
      ans = ans_dens_prop - ans_dens_curr;
    }
  }
  UNPROTECT(1);
  return ans;
}


void
updateAccSmall(SEXP combined_R)
{
  int i_comp_r = *INTEGER(GET_SLOT(combined_R, iComp_sym));
  int i_births_r = *INTEGER(GET_SLOT(combined_R, iBirths_sym));
  int is_births = i_comp_r == i_births_r;
  if (!is_births) {
    int i_acc_r = *INTEGER(GET_SLOT(combined_R, iAccNext_sym));
    int has_accession = (i_acc_r > 0);
    if (has_accession) {
      int diff = *INTEGER(GET_SLOT(combined_R, diffProp_sym));
      int * accession = INTEGER(GET_SLOT(combined_R, accession_sym));
      int i_orig_dest_r = *INTEGER(GET_SLOT(combined_R, iOrigDest_sym));
      int is_orig_dest = i_comp_r == i_orig_dest_r;
      if (is_orig_dest) {
	int i_acc_dest_r = *INTEGER(GET_SLOT(combined_R, iAccNextOther_sym));
	accession[i_acc_r - 1] -= diff;
	accession[i_acc_dest_r - 1] += diff;
      }
      else {
	int * isIncrementVec = LOGICAL(GET_SLOT(combined_R, isIncrement_sym));
	int is_increment = isIncrementVec[i_comp_r - 1];
	if (is_increment)
	  accession[i_acc_r - 1] += diff;
	else
	  accession[i_acc_r - 1] -= diff;
      }
    }
  }
}

void
updateExpSmall(SEXP combined_R)
{
  int i_comp_r = *INTEGER(GET_SLOT(combined_R, iComp_sym));
  int i_births_r = *INTEGER(GET_SLOT(combined_R, iBirths_sym));
  int is_births = i_comp_r == i_births_r;
  if (!is_births) {
    int i_orig_dest_r = *INTEGER(GET_SLOT(combined_R, iOrigDest_sym));
    int is_orig_dest = i_comp_r == i_orig_dest_r;
    int * isIncrementVec = LOGICAL(GET_SLOT(combined_R, isIncrement_sym));
    int is_increment = isIncrementVec[i_comp_r - 1];
    int i_cell_up_r = *INTEGER(GET_SLOT(combined_R, iCell_sym));
    int i_exp_up_r = *INTEGER(GET_SLOT(combined_R, iExposure_sym));
    int i_exp_low_r = *INTEGER(GET_SLOT(combined_R, iExposureOther_sym));
    int diff = *INTEGER(GET_SLOT(combined_R, diffProp_sym));
    double tolExposure = 0.000001;
    double age_time_step = *REAL(GET_SLOT(combined_R, ageTimeStep_sym));
    SEXP iterators_R = GET_SLOT(combined_R, iteratorsComp_sym);
    SEXP iteratorsDup_R = NULL;
    PROTECT(iteratorsDup_R = duplicate(iterators_R));
    SEXP iterator_R = VECTOR_ELT(iteratorsDup_R, i_comp_r - 1);
    resetCC(iterator_R, i_cell_up_r);
    int n_age = *INTEGER(GET_SLOT(iterator_R, nAge_sym));
    int last_age_group_open = *INTEGER(GET_SLOT(iterator_R, lastAgeGroupOpen_sym));
    int i_age_r = *INTEGER(GET_SLOT(iterator_R, iAge_sym));
    int is_final = i_age_r == n_age;
    int sign = (is_orig_dest || !is_increment) ? -1 : 1;
    double incr_exp_up = 0;
    double incr_exp_low = 0;
    double * exposure = REAL(GET_SLOT(combined_R, exposure_sym));
    if (is_final && last_age_group_open) {
      incr_exp_up += 0.5 * sign * age_time_step * diff;
      incr_exp_low -= (1.0/3.0) * sign * age_time_step * diff;
    }
    else {
      incr_exp_up += (1.0/6.0) * sign * age_time_step * diff;
      incr_exp_low += (1.0/6.0) * sign * age_time_step * diff;
    }
    double expose_new_up = exposure[i_exp_up_r - 1] + incr_exp_up;
    double expose_new_low = exposure[i_exp_low_r - 1] + incr_exp_low;
    if (expose_new_up < tolExposure) {
      if (expose_new_up > -1 * tolExposure)
	expose_new_up = 0;
      else
	error("negative value for 'expose_new_up' : %f", expose_new_up);
    }
    if (expose_new_low < tolExposure) {
      if (expose_new_low > -1 * tolExposure)
	expose_new_low = 0;
      else
	error("negative value for 'expose_new_low' : %f", expose_new_low);
    }
    exposure[i_exp_up_r - 1] = expose_new_up;
    exposure[i_exp_low_r - 1] = expose_new_low;
    if (is_orig_dest) { 
      int i_exp_up_oth_r = *INTEGER(GET_SLOT(combined_R, iExpFirst_sym));
      int i_exp_low_oth_r = *INTEGER(GET_SLOT(combined_R, iExpFirstOther_sym));
      double expose_new_up_oth = exposure[i_exp_up_oth_r - 1] - incr_exp_up;
      double expose_new_low_oth = exposure[i_exp_low_oth_r - 1] - incr_exp_low;
      if (expose_new_up_oth < tolExposure) {
	if (expose_new_up_oth > -1 * tolExposure)
	  expose_new_up_oth = 0;
	else
	  error("negative value for 'expose_new_up_oth' : %f", expose_new_up_oth);
      }
      if (expose_new_low_oth < tolExposure) {
	if (expose_new_low_oth > -1 * tolExposure)
	  expose_new_low_oth = 0;
	else
	  error("negative value for 'expose_new_low_oth' : %f", expose_new_low_oth);
      }
      exposure[i_exp_up_oth_r - 1] = expose_new_up_oth;
      exposure[i_exp_low_oth_r - 1] = expose_new_low_oth;
    }
    UNPROTECT(1);
  }
}



void
updateCellMove(SEXP combined_R)
{
    int iComp_r = *INTEGER(GET_SLOT(combined_R, iComp_sym));
    int iCell_r = *INTEGER(GET_SLOT(combined_R, iCell_sym));
    int iCellOther_r = *INTEGER(GET_SLOT(combined_R, iCellOther_sym));
    int iPool_r = *INTEGER(GET_SLOT(combined_R, iPool_sym));
    int iIntNet_r = *INTEGER(GET_SLOT(combined_R, iIntNet_sym));
    int isSmallUpdate = *LOGICAL(GET_SLOT(combined_R, isSmallUpdate_sym));


    int diff = *INTEGER(GET_SLOT(combined_R, diffProp_sym));

    int isPopn = (iComp_r == 0);
    int isPool = (iComp_r == iPool_r);
    int isIntNet = (iComp_r == iIntNet_r);

    SEXP account_R = GET_SLOT(combined_R, account_sym);
    int iCell = iCell_r - 1;

    if (isPopn) {
        int * population = INTEGER(GET_SLOT(account_R, population_sym));
        population[iCell] += diff;

    }
    else { /* not population so use component */

        int iComp = iComp_r - 1;
        int iCellOther = iCellOther_r - 1;

        SEXP components_R = GET_SLOT(account_R, components_sym);
        int * component = INTEGER(VECTOR_ELT(components_R, iComp));

        if(isPool) {
            component[iCell] += diff;
            component[iCellOther] += diff;
        }
        else if(isIntNet || isSmallUpdate) {
            component[iCell] += diff;
            component[iCellOther] -= diff;
        }
        else {
            component[iCell] += diff;
        }
    }
}


void
updateSubsequentPopnMove(SEXP combined_R)
{
    int iComp_r = *INTEGER(GET_SLOT(combined_R, iComp_sym));
    int iOrigDest_r = *INTEGER(GET_SLOT(combined_R, iOrigDest_sym));
    int iPool_r = *INTEGER(GET_SLOT(combined_R, iPool_sym));
    int iIntNet_r = *INTEGER(GET_SLOT(combined_R, iIntNet_sym));
    int iPopnNext_r = *INTEGER(GET_SLOT(combined_R, iPopnNext_sym));

    SEXP iterator_R = GET_SLOT(combined_R, iteratorPopn_sym);

    int diff = *INTEGER(GET_SLOT(combined_R, diffProp_sym));

    int * isIncrementVec = LOGICAL(GET_SLOT(combined_R, isIncrement_sym));

    int isPopn = (iComp_r == 0);
    int isOrigDest = (iComp_r == iOrigDest_r);
    int isPool = (iComp_r == iPool_r);
    int isIntNet = (iComp_r == iIntNet_r);

    int updateTwoCohorts = (isOrigDest || isPool || isIntNet);

    SEXP account_R = GET_SLOT(combined_R, account_sym);
    int * population = INTEGER(GET_SLOT(account_R, population_sym));

    if (isPopn) {

        resetCP(iterator_R, iPopnNext_r);
        int * i_ptr = INTEGER(GET_SLOT(iterator_R, i_sym));
        int * finished_ptr = LOGICAL(GET_SLOT(iterator_R, finished_sym));
        int i = *i_ptr - 1;
        int finished = *finished_ptr;
        population[i] += diff;

        while(!finished) {
            advanceCP(iterator_R);
            i = *i_ptr - 1 ;
            finished = *finished_ptr;
            population[i] += diff;
        }

    }
    else if(updateTwoCohorts) {

        int iPopnNextOther_r = *INTEGER(GET_SLOT(combined_R, iPopnNextOther_sym));

        int diffOrig = diff;
        int diffDest = -diff;

        if(isOrigDest || isPool) {
            diffOrig = -diff;
            diffDest = diff;
        }
        SEXP iteratorDest_R;
        PROTECT(iteratorDest_R = duplicate(iterator_R));
        resetCP(iterator_R, iPopnNext_r);
        resetCP(iteratorDest_R, iPopnNextOther_r);

        int * iOrig_ptr = INTEGER(GET_SLOT(iterator_R, i_sym));
        int * finishedOrig_ptr = LOGICAL(GET_SLOT(iterator_R, finished_sym));
        int * iDest_ptr = INTEGER(GET_SLOT(iteratorDest_R, i_sym));
        int iOrig = *iOrig_ptr - 1;
        int iDest = *iDest_ptr - 1;
        int finishedOrig = *finishedOrig_ptr;
        population[iOrig] += diffOrig;
        population[iDest] += diffDest;

        while(!finishedOrig) {
            advanceCP(iterator_R);
            advanceCP(iteratorDest_R);
            iOrig = *iOrig_ptr - 1;
            iDest = *iDest_ptr - 1;
            finishedOrig = *finishedOrig_ptr;
            population[iOrig] += diffOrig;
            population[iDest] += diffDest;
        }

        UNPROTECT(1); /* iteratorDest_R */
    }
    else {
        int iComp = iComp_r - 1;
        int isIncrement = isIncrementVec[iComp];

        if(!isIncrement) {
            diff = -diff;
        }

        resetCP(iterator_R, iPopnNext_r);
        int * i_ptr = INTEGER(GET_SLOT(iterator_R, i_sym));
        int * finished_ptr = LOGICAL(GET_SLOT(iterator_R, finished_sym));
        int i = *i_ptr - 1;
        int finished = *finished_ptr;
        population[i] += diff;

        while(!finished) {
            advanceCP(iterator_R);
            i = *i_ptr - 1;
            finished = *finished_ptr;
            population[i] += diff;
        }

    }
}

void
updateSubsequentAccMove(SEXP combined_R)
{
    int iComp_r = *INTEGER(GET_SLOT(combined_R, iComp_sym));
    int iOrigDest_r = *INTEGER(GET_SLOT(combined_R, iOrigDest_sym));
    int iPool_r = *INTEGER(GET_SLOT(combined_R, iPool_sym));
    int iIntNet_r = *INTEGER(GET_SLOT(combined_R, iIntNet_sym));
    int iAccNext_r = *INTEGER(GET_SLOT(combined_R, iAccNext_sym));

    int noSubsequentAccession = (iAccNext_r == 0);

    if (!noSubsequentAccession) {

        SEXP iterator_R = GET_SLOT(combined_R, iteratorAcc_sym);

        int diff = *INTEGER(GET_SLOT(combined_R, diffProp_sym));

        int * isIncrementVec = LOGICAL(GET_SLOT(combined_R, isIncrement_sym));

        int isPopn = (iComp_r == 0);
        int isOrigDest = (iComp_r == iOrigDest_r);
        int isPool = (iComp_r == iPool_r);
        int isIntNet = (iComp_r == iIntNet_r);

        int updateTwoCohorts = (isOrigDest || isPool || isIntNet);

        int * accession = INTEGER(GET_SLOT(combined_R, accession_sym));

        if (isPopn) {

            resetCA(iterator_R, iAccNext_r);
            int * i_ptr = INTEGER(GET_SLOT(iterator_R, i_sym));
            int * finished_ptr = LOGICAL(GET_SLOT(iterator_R, finished_sym));
            int i = *i_ptr - 1;
            int finished = *finished_ptr;
            accession[i] += diff;

            while(!finished) {
                advanceCA(iterator_R);
                i = *i_ptr - 1 ;
                finished = *finished_ptr;
                accession[i] += diff;
            }

        }
        else if(updateTwoCohorts) {

            int iAccNextOther_r = *INTEGER(GET_SLOT(combined_R, iAccNextOther_sym));

            int diffOrig = diff;
            int diffDest = -diff;

            if(isOrigDest || isPool) {
                diffOrig = -diff;
                diffDest = diff;
            }
            SEXP iteratorDest_R;
            PROTECT(iteratorDest_R = duplicate(iterator_R));
            resetCA(iterator_R, iAccNext_r);
            resetCA(iteratorDest_R, iAccNextOther_r);

            int * iOrig_ptr = INTEGER(GET_SLOT(iterator_R, i_sym));
            int * finishedOrig_ptr = LOGICAL(GET_SLOT(iterator_R, finished_sym));
            int * iDest_ptr = INTEGER(GET_SLOT(iteratorDest_R, i_sym));
            int iOrig = *iOrig_ptr - 1;
            int iDest = *iDest_ptr - 1;
            int finishedOrig = *finishedOrig_ptr;
            accession[iOrig] += diffOrig;
            accession[iDest] += diffDest;

            while(!finishedOrig) {
                advanceCA(iterator_R);
                advanceCA(iteratorDest_R);
                iOrig = *iOrig_ptr - 1;
                iDest = *iDest_ptr - 1;
                finishedOrig = *finishedOrig_ptr;
                accession[iOrig] += diffOrig;
                accession[iDest] += diffDest;
            }

            UNPROTECT(1); /* iteratorDest_R */
        }
        else {
            int iComp = iComp_r - 1;
            int isIncrement = isIncrementVec[iComp];

            if(!isIncrement) {
                diff = -diff;
            }

            resetCA(iterator_R, iAccNext_r);
            int * i_ptr = INTEGER(GET_SLOT(iterator_R, i_sym));
            int * finished_ptr = LOGICAL(GET_SLOT(iterator_R, finished_sym));
            int i = *i_ptr - 1;
            int finished = *finished_ptr;
            accession[i] += diff;

            while(!finished) {
                advanceCA(iterator_R);
                i = *i_ptr - 1;
                finished = *finished_ptr;
                accession[i] += diff;
            }

        }
    } /* end if !noSubsequentAccession */
}

void
updateSubsequentExpMove(SEXP combined_R)
{
  int iComp_r = *INTEGER(GET_SLOT(combined_R, iComp_sym));
  int iBirths_r = *INTEGER(GET_SLOT(combined_R, iBirths_sym));
  int iOrigDest_r = *INTEGER(GET_SLOT(combined_R, iOrigDest_sym));
  int iPool_r = *INTEGER(GET_SLOT(combined_R, iPool_sym));
  int iIntNet_r = *INTEGER(GET_SLOT(combined_R, iIntNet_sym));
  int isPopn = (iComp_r == 0);
  int isBirths = (iComp_r == iBirths_r);
  int isOrigDest = (iComp_r == iOrigDest_r);
  int isPool = (iComp_r == iPool_r);
  int isIntNet = (iComp_r == iIntNet_r);
  int updateTwoCohorts = (isOrigDest || isPool || isIntNet);
  SEXP iterator_R = GET_SLOT(combined_R, iteratorExposure_sym);
  SEXP iteratorOth_R;
  PROTECT(iteratorOth_R = duplicate(iterator_R));
  int * i_ptr = INTEGER(GET_SLOT(iterator_R, i_sym));
  int * iOth_ptr = INTEGER(GET_SLOT(iteratorOth_R, i_sym));
  int * finished_ptr = LOGICAL(GET_SLOT(iterator_R, finished_sym));
  int diff = *INTEGER(GET_SLOT(combined_R, diffProp_sym));
  double ageTimeStep = *REAL(GET_SLOT(combined_R, ageTimeStep_sym));
  int hasAge = *LOGICAL(GET_SLOT(combined_R, hasAge_sym));
  int * isIncrementVec = LOGICAL(GET_SLOT(combined_R, isIncrement_sym));
  int isIncrement = isPopn || isIncrementVec[iComp_r - 1];
  int nAge = 0;
  int iAge = 0;
  int iTri = 0;
  int isUpper = 0;
  int isFinal = 0;
  double tolExposure = 0.000001;
  int iExpFirst_r = *INTEGER(GET_SLOT(combined_R, iExpFirst_sym));
  int iExpFirstOther_r = *INTEGER(GET_SLOT(combined_R, iExpFirstOther_sym));
  /* death with orig-dest */
  int origEqualsDest = isOrigDest && (iExpFirst_r == iExpFirstOther_r);
  if (!origEqualsDest) {
    /* deal with first cell */
    resetCC(iterator_R, iExpFirst_r);
    double incrExp = 0;
    double * exposure = REAL(GET_SLOT(combined_R, exposure_sym));
    if (hasAge) {
      nAge = *INTEGER(GET_SLOT(iterator_R, nAge_sym));
      iAge = *INTEGER(GET_SLOT(iterator_R, iAge_sym));
      iTri = *INTEGER(GET_SLOT(iterator_R, iTriangle_sym));
      isFinal = iAge == nAge;
      isUpper = iTri == 2;
      if (isPopn) {
	if (isFinal)
	  incrExp += ageTimeStep * diff;
	else
	  incrExp += 0.5 * ageTimeStep * diff;
      }
      else {
	if (!isBirths) {
	  if (isFinal) {
	    if (isUpper)
	      incrExp += 0.5 * ageTimeStep * diff;
	    else
	      incrExp -= (1.0/6.0) * ageTimeStep * diff;
	  }
	  else {
	    if (isUpper)
	      incrExp += (1.0/6.0) * ageTimeStep * diff;
	    else
	      incrExp -= (1.0/6.0) * ageTimeStep * diff;
	  }
	}
	if (!isUpper) {
	  incrExp += 0.5 * ageTimeStep * diff;
	}
      }
    }
    else { 			/* no age */
      if (isPopn)
	incrExp += ageTimeStep * diff;
      else
	incrExp += 0.5 * ageTimeStep * diff;
    }
    if (updateTwoCohorts) {
      resetCC(iteratorOth_R, iExpFirstOther_r);
      double incrExpOrig = incrExp;
      if (isOrigDest || isPool)
	incrExpOrig *= -1;
      double incrExpDest = -1 * incrExpOrig;
      double exposeNewOrig = exposure[iExpFirst_r - 1] + incrExpOrig;
      double exposeNewDest = exposure[iExpFirstOther_r - 1] + incrExpDest;
      if (exposeNewOrig < tolExposure) {
	if (exposeNewOrig > -1 * tolExposure)
	  exposeNewOrig = 0;
	else
	  error("negative value for 'exposeNewOrig' : %f", exposeNewOrig);
      }
      if (exposeNewDest < tolExposure) {
	if (exposeNewDest > -1 * tolExposure)
	  exposeNewDest = 0;
	else
	  error("negative value for 'exposeNewDest' : %f", exposeNewDest);
      }
      exposure[iExpFirst_r - 1] = exposeNewOrig;
      exposure[iExpFirstOther_r - 1] = exposeNewDest;
    }
    else {
      if (!isIncrement)
	incrExp *= -1;
      double exposeNew = exposure[iExpFirst_r - 1] + incrExp;
      if (exposeNew < tolExposure) {
	if (exposeNew > -1 * tolExposure)
	  exposeNew = 0;
	else
	  error("negative value for 'exposeNew' : %f", exposeNew);
      }
      exposure[iExpFirst_r - 1] = exposeNew;
    }
    /* subsequent cells */
    int finished = *finished_ptr;
    while (!finished) {
      advanceCC(iterator_R);
      int i_r = *i_ptr;
      incrExp = 0;
      if (hasAge) {
	iAge = *INTEGER(GET_SLOT(iterator_R, iAge_sym));
	iTri = *INTEGER(GET_SLOT(iterator_R, iTriangle_sym));
	isFinal = iAge == nAge;
	isUpper = iTri == 2;
	if (isFinal) {
	  if (isUpper)
	    incrExp += ageTimeStep * diff;
	  else
	    incrExp += 0.5 * ageTimeStep * diff;
	}
	else
	  incrExp += 0.5 * ageTimeStep * diff;
      }
      else {
	incrExp += ageTimeStep * diff;
      }
      if (updateTwoCohorts) {
	advanceCC(iteratorOth_R);
	double incrExpOrig = incrExp;
	if (isOrigDest || isPool)
	  incrExpOrig *= -1;
	double incrExpDest = -1 * incrExpOrig;
	int iOth_r = *iOth_ptr;
	double exposeNewOrig = exposure[i_r - 1] + incrExpOrig;
	double exposeNewDest = exposure[iOth_r - 1] + incrExpDest;
	if (exposeNewOrig < tolExposure) {
	  if (exposeNewOrig > -1 * tolExposure)
	    exposeNewOrig = 0;
	  else
	    error("negative value for 'exposeNewOrig' : %f", exposeNewOrig);
	}
	if (exposeNewDest < tolExposure) {
	  if (exposeNewDest > -1 * tolExposure)
	    exposeNewDest = 0;
	  else
	    error("negative value for 'exposeNewDest' : %f", exposeNewDest);
	}
	exposure[i_r - 1] = exposeNewOrig;
	exposure[iOth_r - 1] = exposeNewDest;
      }
      else {
	if (!isIncrement)
	  incrExp *= -1;
	double exposeNew = exposure[i_r - 1] + incrExp;
	if (exposeNew < tolExposure) {
	  if (exposeNew > -1 * tolExposure)
	    exposeNew = 0;
	  else
	    error("negative value for 'exposeNew' : %f", exposeNew);
	}
	exposure[i_r - 1] = exposeNew;
      }
      finished = *finished_ptr;
    }
  }
  UNPROTECT(1); /* iteratorOth_R */
}
