#include "mapping-functions.h"
#include "helper-functions.h"
#include "demest.h"

/* File "update-accounts.c" contains C versions of functions 
 * from "update-accounts.R". */

/* ****************** Updating proposals *************************** */


/* duplicates the iterators and replaces the unused iterators when finished
 * used when calling the code directly from R */
void
updateProposalAccountMovePopn_external(SEXP combined_R)
{
    SEXP iteratorPopn_R = GET_SLOT(combined_R, iteratorPopn_sym);
    SEXP iteratorPopnDup_R = NULL;
    PROTECT(iteratorPopnDup_R = duplicate(iteratorPopn_R));
    int nProtected = 1;
    int hasAge = *LOGICAL(GET_SLOT(combined_R, hasAge_sym));
    
    SEXP iteratorAccDup_R = NULL;
    if(hasAge) {
        SEXP iteratorAcc_R = GET_SLOT(combined_R, iteratorAcc_sym);
        
        PROTECT(iteratorAccDup_R = duplicate(iteratorAcc_R));
    
        ++nProtected;
    }
        
    updateProposalAccountMovePopn(combined_R);
    
    SET_SLOT(combined_R, iteratorPopn_sym, iteratorPopnDup_R);
    
    if(hasAge) {
        SET_SLOT(combined_R, iteratorAcc_sym, iteratorAccDup_R);
    }
    
    UNPROTECT(nProtected);
}

void
updateProposalAccountMovePopn(SEXP combined_R)
{
    SEXP account_R = GET_SLOT(combined_R, account_sym);
    SEXP population_R = GET_SLOT(account_R, population_sym);
    int maxAttempt = *INTEGER(GET_SLOT(combined_R, maxAttempt_sym));
    int hasAge = *LOGICAL(GET_SLOT(combined_R, hasAge_sym));
    
    SEXP descriptions_R = GET_SLOT(combined_R, descriptions_sym);
    SEXP description_R = VECTOR_ELT(descriptions_R, 0);
    
    SEXP iteratorPopn_R = GET_SLOT(combined_R, iteratorPopn_sym);
    
    SEXP systemModels_R = GET_SLOT(combined_R, systemModels_sym);
    SEXP thisSystemModel_R = VECTOR_ELT(systemModels_R, 0);
    double * theta = REAL(GET_SLOT(thisSystemModel_R, theta_sym));
    
    int iCell_r = chooseICellPopn(description_R);
    int iExposure_r = 0;
    
    int iExpFirst_r = getIExpFirstFromPopn(iCell_r, description_R);
    int iPopnNext_r = getIPopnNextFromPopn(iCell_r, description_R);
    int minVal = getMinValCohortPopulation(iPopnNext_r, population_R,
                                                        iteratorPopn_R);
    
    int iAccNext_r = 0;
    
    if (hasAge) {
        SEXP accession_R = GET_SLOT(combined_R, accession_sym);
        SEXP iteratorAcc_R = GET_SLOT(combined_R, iteratorAcc_sym);
        
        iAccNext_r = getIAccNextFromPopn(iCell_r, description_R);
        
        int hasLaterAccession = (iAccNext_r > 0);
        
        if (hasLaterAccession) {
            
            int minAcc = getMinValCohortAccession(iAccNext_r, accession_R,
                                                        iteratorAcc_R);
            if (minAcc < minVal) {
                minVal = minAcc;
            }                                            
        }
    } /* end hasAge */
    
    int * population = INTEGER(population_R);
    int iCell = iCell_r - 1;
    
    int valCurr = population[iCell];
    int lower = valCurr - minVal;
    int upper = NA_INTEGER;
    
    double lambda = theta[iCell];
    int valProp = rpoisTrunc1(lambda, lower, upper, maxAttempt);
    
    int foundValue = !(valProp == NA_INTEGER);
    
    int generatedNewProposal = 0; 
    int diffProp = 0;
    
    if(foundValue) {
        diffProp = valProp - valCurr;
        generatedNewProposal = (diffProp != 0);
    }
    
    SET_LOGICALSCALE_SLOT(combined_R, generatedNewProposal_sym, generatedNewProposal);
    
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

/* duplicates the iterators and replaces the unused iterators when finished
 * used when calling the code directly from R */
void
updateProposalAccountMoveBirths_external(SEXP combined_R)
{
    SEXP iteratorPopn_R = GET_SLOT(combined_R, iteratorPopn_sym);
    SEXP iteratorPopnDup_R = NULL;
    PROTECT(iteratorPopnDup_R = duplicate(iteratorPopn_R));
    int nProtected = 1;
    int hasAge = *LOGICAL(GET_SLOT(combined_R, hasAge_sym));
    
    SEXP iteratorAccDup_R = NULL;
    if(hasAge) {
        SEXP iteratorAcc_R = GET_SLOT(combined_R, iteratorAcc_sym);
        
        PROTECT(iteratorAccDup_R = duplicate(iteratorAcc_R));
    
        ++nProtected;
    }
        
    updateProposalAccountMoveBirths(combined_R);
    
    SET_SLOT(combined_R, iteratorPopn_sym, iteratorPopnDup_R);
    
    if(hasAge) {
        SET_SLOT(combined_R, iteratorAcc_sym, iteratorAccDup_R);
    }
    
    UNPROTECT(nProtected);
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
    
    int iCell_r = chooseICellComp(description_R);
    
    int iExpFirst_r = getIExpFirstFromBirths(iCell_r, mappingToExp_R);
    int iPopnNext_r = getIPopnNextFromComp(iCell_r, mappingToPopn_R);
    int minVal = getMinValCohortPopulation(iPopnNext_r, population_R,
                                                        iteratorPopn_R);
    int iAccNext_r = 0;
    int isLowerTriangleValueValue = 0;

    if (hasAge) {
        
        isLowerTriangleValueValue = isLowerTriangle(iCell_r, description_R);
        
        SEXP accession_R = GET_SLOT(combined_R, accession_sym);
        SEXP iteratorAcc_R = GET_SLOT(combined_R, iteratorAcc_sym);
        
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
    int iCell = iCell_r - 1;
    
    int valCurr = component[iCell];
    int lower = valCurr - minVal;
    int upper = NA_INTEGER;
    
    double thetaCell = theta[iCell];
        
    double lambda = thetaCell;

    int iExposure_r = 0;
    
    if(usesExposure) {
        double * expectedExposure = REAL(GET_SLOT(combined_R, expectedExposure_sym));
        iExposure_r = getIExposureFromBirths(iCell_r, mappingToExp_R);
        int iExposure = iExposure_r - 1;
        double expectedExposureCell = expectedExposure[iExposure];
        lambda *= expectedExposureCell;
    }
    
    int valProp = rpoisTrunc1(lambda, lower, upper, maxAttempt);
    
    int foundValue = !(valProp == NA_INTEGER);
    
    int generatedNewProposal = 0; 
    int diffProp = 0;
    
    if(foundValue) {
        diffProp = valProp - valCurr;
        generatedNewProposal = (diffProp != 0);
    }
    
    SET_LOGICALSCALE_SLOT(combined_R, generatedNewProposal_sym, generatedNewProposal);
    
    if (!generatedNewProposal) {
         iCell_r = NA_INTEGER;
         iPopnNext_r = NA_INTEGER;
         iAccNext_r = NA_INTEGER;
         isLowerTriangleValueValue = NA_LOGICAL;
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
        SET_LOGICALSCALE_SLOT(combined_R, isLowerTriangle_sym, isLowerTriangleValueValue);
    }

    SET_INTSCALE_SLOT(combined_R, iExposure_sym, iExposure_r);
    SET_INTSCALE_SLOT(combined_R, iExposureOther_sym, NA_INTEGER);
    SET_INTSCALE_SLOT(combined_R, iExpFirst_sym, iExpFirst_r);
    SET_INTSCALE_SLOT(combined_R, iExpFirstOther_sym, NA_INTEGER);
    SET_INTSCALE_SLOT(combined_R, diffProp_sym, diffProp);
}


/* duplicates the iterators and replaces the unused iterators when finished
 * used when calling the code directly from R */
void
updateProposalAccountMoveOrigDest_external(SEXP combined_R)
{
    SEXP iteratorPopn_R = GET_SLOT(combined_R, iteratorPopn_sym);
    SEXP iteratorPopnDup_R = NULL;
    PROTECT(iteratorPopnDup_R = duplicate(iteratorPopn_R));
    int nProtected = 1;
    int hasAge = *LOGICAL(GET_SLOT(combined_R, hasAge_sym));
    
    SEXP iteratorAccDup_R = NULL;
    if(hasAge) {
        SEXP iteratorAcc_R = GET_SLOT(combined_R, iteratorAcc_sym);
        
        PROTECT(iteratorAccDup_R = duplicate(iteratorAcc_R));
    
        ++nProtected;
    }
        
    updateProposalAccountMoveOrigDest(combined_R);
    
    SET_SLOT(combined_R, iteratorPopn_sym, iteratorPopnDup_R);
    
    if(hasAge) {
        SET_SLOT(combined_R, iteratorAcc_sym, iteratorAccDup_R);
    }
    
    UNPROTECT(nProtected);
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
    
    int iCell_r = chooseICellComp(description_R);
    
    int pairArray[2]; /* use for all the pairs */
    
    getIExpFirstPairFromOrigDestInternal(pairArray, iCell_r, mappingToExp_R);
    
    int iExpFirstOrig_r = pairArray[0];
    int iExpFirstDest_r = pairArray[1];
    
    getIPopnNextFromOrigDestInternal(pairArray, iCell_r, mappingToPopn_R);
    
    int iPopnNextOrig_r = pairArray[0];
    int iPopnNextDest_r = pairArray[1];
    
    int minValOrig = getMinValCohortPopulation(iPopnNextOrig_r, population_R,
                                                        iteratorPopn_R);
    int minValDest = getMinValCohortPopulation(iPopnNextDest_r, population_R,
                                                        iteratorPopn_R);
    int iAccNextOrig_r = 0;
    int iAccNextDest_r = 0;
    
    int isLowerTriangleValue = 0;
    
    if (hasAge) {
        
        isLowerTriangleValue = isLowerTriangle(iCell_r, description_R);
        
        SEXP accession_R = GET_SLOT(combined_R, accession_sym);
        SEXP iteratorAcc_R = GET_SLOT(combined_R, iteratorAcc_sym);
        
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
            if (minAccOrig < minValOrig) {
                minValOrig = minAccOrig;
            }
            if (minAccDest < minValDest) {
                minValDest = minAccDest;
            }                                             
        }
    } /* end hasAge */
    
    int * component = INTEGER(component_R);
    int iCell = iCell_r - 1;
    
    int valCurr = component[iCell];
    int lower = valCurr - minValDest;
    int upper = valCurr + minValOrig;
    
    double thetaCell = theta[iCell];
        
    double lambda = thetaCell;

    int iExposure_r = 0;
    
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
    
    int generatedNewProposal = 0; 
    int diffProp = 0;
    
    if(foundValue) {
        diffProp = valProp - valCurr;
        generatedNewProposal = (diffProp != 0);
    }
    
    SET_LOGICALSCALE_SLOT(combined_R, generatedNewProposal_sym, generatedNewProposal);
    
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


/* duplicates the iterators and replaces the unused iterators when finished
 * used when calling the code directly from R */
void
updateProposalAccountMovePool_external(SEXP combined_R)
{
    SEXP iteratorPopn_R = GET_SLOT(combined_R, iteratorPopn_sym);
    SEXP iteratorPopnDup_R = NULL;
    PROTECT(iteratorPopnDup_R = duplicate(iteratorPopn_R));
    int nProtected = 1;
    int hasAge = *LOGICAL(GET_SLOT(combined_R, hasAge_sym));
    
    SEXP iteratorAccDup_R = NULL;
    if(hasAge) {
        SEXP iteratorAcc_R = GET_SLOT(combined_R, iteratorAcc_sym);
        
        PROTECT(iteratorAccDup_R = duplicate(iteratorAcc_R));
    
        ++nProtected;
    }
        
    updateProposalAccountMovePool(combined_R);
    
    SET_SLOT(combined_R, iteratorPopn_sym, iteratorPopnDup_R);
    
    if(hasAge) {
        SET_SLOT(combined_R, iteratorAcc_sym, iteratorAccDup_R);
    }
    
    UNPROTECT(nProtected);
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
    
    int pairArray[2]; 
    
    chooseICellOutInPoolInternal(pairArray, description_R);
    
    int iCellOut_r = pairArray[0];
    int iCellIn_r = pairArray[1];
    
    int iExpFirstOut_r = getIExpFirstFromComp(iCellOut_r, mappingToExp_R);
    int iExpFirstIn_r = getIExpFirstFromComp(iCellIn_r, mappingToExp_R);
    
    
    int iPopnNextOut_r = getIPopnNextFromComp(iCellOut_r, mappingToPopn_R);
    int iPopnNextIn_r = getIPopnNextFromComp(iCellIn_r, mappingToPopn_R);
    
    int minValOut = getMinValCohortPopulation(iPopnNextOut_r, population_R,
                                                        iteratorPopn_R);
    int minValIn = getMinValCohortPopulation(iPopnNextIn_r, population_R,
                                                        iteratorPopn_R);
    int iAccNextOut_r = 0;
    int iAccNextIn_r = 0;
    
    int isLowerTriangleValue = 0;
    
    if (hasAge) {
        
        isLowerTriangleValue = isLowerTriangle(iCellOut_r, description_R);
        
        SEXP accession_R = GET_SLOT(combined_R, accession_sym);
        SEXP iteratorAcc_R = GET_SLOT(combined_R, iteratorAcc_sym);
        
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
            if (minAccOut < minValOut) {
                minValOut = minAccOut;
            }
            if (minAccIn < minValIn) {
                minValIn = minAccIn;
            }                                             
        }
    } /* end hasAge */
    
    int * component = INTEGER(component_R);
    int iCellOut = iCellOut_r - 1;
    int iCellIn = iCellIn_r - 1;
    
    int valCurrOut = component[iCellOut];
    int valCurrIn = component[iCellIn];
    int lower = valCurrIn - minValIn;
    int upper = valCurrOut + minValOut;
    
    double thetaOut = theta[iCellOut];
        
    double lambdaOut = thetaOut;

    int iExposureOut_r = 0;
    int iExposureIn_r = 0;
    
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
    
    int generatedNewProposal = 0; 
    int diffProp = 0;
    
    if(foundValue) {
        diffProp = valPropOut - valCurrOut;
        generatedNewProposal = (diffProp != 0);
    }
    
    SET_LOGICALSCALE_SLOT(combined_R, generatedNewProposal_sym, generatedNewProposal);
    
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



/*
## READY_TO_TRANSLATE
## HAS_TESTS
updateProposalAccountMoveNet <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(updateProposalAccountMoveNet_R, combined)
    }
    else {
        account <- combined@account
        population <- account@population
        i.comp <- combined@iComp
        component <- account@components[[i.comp]]
        max.attempt <- combined@maxAttempt
        has.age <- combined@hasAge
        if (has.age) {
            accession <- combined@accession
            iterator.acc <- combined@iteratorAcc
            mapping.to.acc <- combined@mappingsToAcc[[i.comp]]
        }        
        mapping.to.popn <- combined@mappingsToPopn[[i.comp]]
        iterator.popn <- combined@iteratorPopn
        mapping.to.exp <- combined@mappingsToExp[[i.comp]]
        description <- combined@descriptions[[i.comp + 1L]]
        system.model <- combined@systemModels[[i.comp + 1L]]
        theta <- system.model@theta
        varsigma <- system.model@varsigma
        w <- system.model@w
        pair.cell <- chooseICellSubAddNet(description)
        i.cell.add <- pair.cell[1L] # 'diffProp' added to this cell
        i.cell.sub <- pair.cell[2L] # 'diffProp' subtracted from this cell
        if (has.age) {
            is.lower.triangle <- isLowerTriangle(i = i.cell.add,
                                                 description = description)
        }
        i.exp.first.add <- getIExpFirstFromComp(i = i.cell.add,
                                                mapping = mapping.to.exp)
        i.exp.first.sub <- getIExpFirstFromComp(i = i.cell.sub,
                                                mapping = mapping.to.exp)
        i.popn.next.add <- getIPopnNextFromComp(i = i.cell.add,
                                                mapping = mapping.to.popn)
        i.popn.next.sub <- getIPopnNextFromComp(i = i.cell.sub,
                                                mapping = mapping.to.popn)
        min.val.add <- getMinValCohortPopulation(i = i.popn.next.add,
                                                 series = population,
                                                 iterator = iterator.popn)
        min.val.sub <- getMinValCohortPopulation(i = i.popn.next.sub,
                                                 series = population,
                                                 iterator = iterator.popn)
        if (has.age) {
            i.acc.next.add <- getIAccNextFromComp(i = i.cell.add,
                                                  mapping = mapping.to.acc)
            i.acc.next.sub <- getIAccNextFromComp(i = i.cell.sub,
                                                  mapping = mapping.to.acc)
            has.later.accession <- i.acc.next.add > 0L
            if (has.later.accession) {
                min.acc.add <- getMinValCohortAccession(i = i.acc.next.add,
                                                        series = accession,
                                                        iterator = iterator.acc)
                min.acc.sub <- getMinValCohortAccession(i = i.acc.next.sub,
                                                        series = accession,
                                                        iterator = iterator.acc)
                min.val.add <- min(min.val.add, min.acc.add)
                min.val.sub <- min(min.val.sub, min.acc.sub)
            }
        }
        mean.add <- theta[i.cell.add]
        val.curr.add <- component[i.cell.add]
        val.curr.sub <- component[i.cell.sub]
        lower <- val.curr.sub - min.val.sub
        upper <- val.curr.add + min.val.add
        if (lower > upper)
            found.value <- FALSE
        else {
            w.add <- w[i.cell.add]
            sd.add <- varsigma / sqrt(w.add)
            val.prop.add <- rnormIntTrunc1(mean = mean.add,
                                           sd = sd.add,
                                           lower = lower,
                                           upper = upper)
            found.value <- !is.na(val.prop.add)
        }
        if (found.value) {
            diff.prop <- val.prop.add - val.curr.add
            generated.new.proposal <- diff.prop != 0L
        }
        else
            generated.new.proposal <- FALSE
        combined@generatedNewProposal@.Data <- generated.new.proposal
        if (generated.new.proposal) {
            combined@iCell <- i.cell.add
            combined@iCellOther <- i.cell.sub
            combined@iPopnNext <- i.popn.next.add
            combined@iPopnNextOther <- i.popn.next.sub
            if (has.age) {
                combined@iAccNext <- i.acc.next.add
                combined@iAccNextOther <- i.acc.next.sub
                combined@isLowerTriangle <- is.lower.triangle
            }
            combined@iExposure <- NA_integer_
            combined@iExposureOther <- NA_integer_
            combined@iExpFirst <- i.exp.first.add
            combined@iExpFirstOther <- i.exp.first.sub
            combined@diffProp <- diff.prop
        }
        else {
            combined@iCell <- NA_integer_
            combined@iCellOther <- NA_integer_
            combined@iPopnNext <- NA_integer_
            combined@iPopnNextOther <- NA_integer_
            if (has.age) {
                combined@iAccNext <- NA_integer_
                combined@iAccNextOther <- NA_integer_
                combined@isLowerTriangle <- NA
            }
            combined@iExposure <- NA_integer_
            combined@iExposureOther <- NA_integer_
            combined@iExpFirst <- NA_integer_
            combined@iExpFirstOther <- NA_integer_
            combined@diffProp <- NA_integer_
        }
        combined
    }
}
*/


/*
## READY_TO_TRANSLATE
## HAS_TESTS
updateProposalAccountMoveComp <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(updateProposalAccountMoveComp_R, combined)
    }
    else {
        account <- combined@account
        population <- account@population
        i.comp <- combined@iComp
        component <- account@components[[i.comp]]
        is.increment <- combined@isIncrement[[i.comp]]
        max.attempt <- combined@maxAttempt
        has.age <- combined@hasAge@.Data
        if (has.age) {
            accession <- combined@accession
            iterator.acc <- combined@iteratorAcc
            mapping.to.acc <- combined@mappingsToAcc[[i.comp]]
        }        
        mapping.to.popn <- combined@mappingsToPopn[[i.comp]]
        iterator.popn <- combined@iteratorPopn
        uses.exposure <- combined@modelUsesExposure[i.comp + 1L]
        mapping.to.exp <- combined@mappingsToExp[[i.comp]]
        description <- combined@descriptions[[i.comp + 1L]]
        theta <- combined@systemModels[[i.comp + 1L]]@theta
        is.net <- combined@isNet[i.comp]
        if (is.net) {
            varsigma.comp <- sys.mod.comp@varsigma
            w.comp <- sys.mod.comp@w
        }
        i.cell <- chooseICellComp(description)
        if (has.age)
            is.lower.triangle <- isLowerTriangle(i = i.cell,
                                                 description = description)
        if (uses.exposure) {
            expected.exposure <- combined@expectedExposure
            i.exposure <- getIExposureFromComp(i = i.cell,
                                               mapping = mapping.to.exp)
        }
        i.exp.first <- getIExpFirstFromComp(i = i.cell,
                                            mapping = mapping.to.exp)
        i.popn.next <- getIPopnNextFromComp(i = i.cell,
                                            mapping = mapping.to.popn)
        min.val <- getMinValCohortPopulation(i = i.popn.next,
                                             series = population,
                                             iterator = iterator.popn)
        if (has.age) {
            i.acc.next <- getIAccNextFromComp(i = i.cell,
                                              mapping = mapping.to.acc)
            has.later.accession <- i.acc.next > 0L
            if (has.later.accession) {
                min.acc <- getMinValCohortAccession(i = i.acc.next,
                                                    series = accession,
                                                    iterator = iterator.acc)
                min.val <- min(min.val, min.acc)
            }
        }
        val.curr <- component[i.cell]
        if (is.increment) {
            lower <- val.curr - min.val
            upper <- NA_integer_
        }
        else {
            lower <- NA_integer_
            upper <- val.curr + min.val
        }
        if (is.net) {
            mean <- theta[i.cell]
            w.cell <- w.comp[i.cell]
            sd <- varsigma.comp / sqrt(w.cell)
            val.prop <- rnormIntTrunc1(mean = mean,
                                       sd = sd,
                                       lower = lower,
                                       upper = upper)
        }
        else {
            theta.cell <- theta[i.cell]
            if (uses.exposure) {
                expected.exposure.cell <- expected.exposure[i.exposure]
                lambda <- theta.cell * expected.exposure.cell
            }
            else
                lambda <- theta.cell
            val.prop <- rpoisTrunc1(lambda = lambda,
                                    lower = lower,
                                    upper = upper,
                                    maxAttempt = max.attempt)
        }
        found.value <- !is.na(val.prop)
        if (found.value) {
            diff.prop <- val.prop - val.curr
            generated.new.proposal <- diff.prop != 0L
        }
        else
            generated.new.proposal <- FALSE
        if (generated.new.proposal) {
            combined@generatedNewProposal@.Data <- TRUE
            combined@iCell <- i.cell
            combined@iCellOther <- NA_integer_
            combined@iPopnNext <- i.popn.next
            combined@iPopnNextOther <- NA_integer_
            if (has.age) {
                combined@iAccNext <- i.acc.next
                combined@iAccNextOther <- NA_integer_
                combined@isLowerTriangle <- is.lower.triangle
            }
            if (uses.exposure) {
                combined@iExposure <- i.exposure
                combined@iExposureOther <- NA_integer_
            }
            else {
                combined@iExposure <- 0L
                combined@iExposureOther <- NA_integer_
            }
            combined@iExpFirst <- i.exp.first
            combined@iExpFirstOther <- NA_integer_
            combined@diffProp <- diff.prop
        }
        else {
            combined@generatedNewProposal@.Data <- FALSE
            combined@iCell <- NA_integer_
            combined@iCellOther <- NA_integer_
            combined@iPopnNext <- NA_integer_
            combined@iPopnNextOther <- NA_integer_
            if (has.age) {
                combined@iAccNext <- NA_integer_
                combined@iAccNextOther <- NA_integer_
                combined@isLowerTriangle <- NA
            }
            combined@iExposure <- NA_integer_
            combined@iExposureOther <- NA_integer_
            combined@iExpFirst <- NA_integer_
            combined@iExpFirstOther <- NA_integer_
            combined@diffProp <- NA_integer_
        }
        combined
    }
}

*/


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
        
        if(R_finite(logLikProp)) {
            
            double logLikCurr = logLikelihood(model_R, totalPopnCurr, dataset_R, iAfter_r);
            retValue = logLikProp - logLikCurr;
            
        }
        else { /* logLikProp infinite */
            retValue = logLikProp;
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
    
    double ans = 0;
    
    double diffLogLikCell = diffLogLikCellComp( diff, iComp_r, iCell_r, 
                                        component_R, 
                                        dataModels_R, datasets_R, 
                                        seriesIndices_R, transforms_R);
    if (R_finite(diffLogLikCell) ) {

        ans += diffLogLikCell;
        
        double diffLogLikPopn = diffLogLikPopnPair( diff,
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
        
        if(R_finite(logLikProp)) {
            
            double logLikCurr = logLikelihood(model_R, totalCompCurr, 
                                                dataset_R, iAfter_r);
            ans = logLikProp - logLikCurr;
            
        }
        else { /* logLikProp infinite */
            ans = logLikProp;
        }
    }
    /* if cellHasNoData, retValue stays at default value */
    
    return ans;
}


double 
diffLogLikPopnPair(int diff, int iPopnOrig_r, int iPopnDest_r,
                        SEXP iterator_R, 
                        SEXP population_R, SEXP dataModels_R, 
                        SEXP datasets_R, SEXP seriesIndices_R, 
                        SEXP transforms_R)
{
    double ans = 0;
    
    if (iPopnOrig_r != iPopnDest_r) {
        
        int nDatasets = LENGTH(datasets_R);
        int * seriesIndices = INTEGER(seriesIndices_R);
        
        for(int iDataset = 0; iDataset < nDatasets; ++iDataset) {
            
            int assocWithPopn = ( seriesIndices[iDataset] == 0 );
            
            if (assocWithPopn) {
                
                SEXP transform_R = VECTOR_ELT(transforms_R, iDataset);
                
                int iAfterOrig_r = dembase_getIAfter(iPopnOrig_r, transform_R);
                int iAfterDest_r = dembase_getIAfter(iPopnDest_r, transform_R);
                
                if(iAfterOrig_r != iAfterDest_r) {
                
                    SEXP model_R = VECTOR_ELT(dataModels_R, iDataset);
                    SEXP dataset_R = VECTOR_ELT(datasets_R, iDataset);
                    
                    double diffOrig = diffLogLikPopnOneDataset( -diff,
                                            iPopnOrig_r, 
                                            iterator_R, population_R, 
                                            model_R, dataset_R, transform_R);
                    if (R_finite(diffOrig) ) {
                        
                        ans += diffOrig;
                        
                        double diffDest = diffLogLikPopnOneDataset( diff,
                                                iPopnDest_r, 
                                                iterator_R, population_R, 
                                                model_R, dataset_R, transform_R);
                        if (R_finite(diffDest) ) {
                            ans += diffDest;
                           
                        }
                        else { /* infinite */
                            ans = diffDest;
                            break; /* break out of for loop */
                        }    
                    }
                    else { /* infinite */
                        ans = diffOrig;
                        break; /* break out of for loop */
                    }
                }
            }
        }
    }
    
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
    
    double ans = 0;
    
    double diffLogLikCells = diffLogLikCellsPool( diff, iComp_r,
                                        iCellOut_r, iCellIn_r, 
                                        component_R, 
                                        dataModels_R, datasets_R, 
                                        seriesIndices_R, transforms_R);
    if (R_finite(diffLogLikCells) ) {

        ans += diffLogLikCells;
        
        double diffLogLikPopn = diffLogLikPopnPair( diff,
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
    
    double ans = 0;
    
    double diffLogLikCells = diffLogLikCellsNet( diff, iComp_r,
                                        iCellAdd_r, iCellSub_r, 
                                        component_R, 
                                        dataModels_R, datasets_R, 
                                        seriesIndices_R, transforms_R);
    if (R_finite(diffLogLikCells) ) {

        ans += diffLogLikCells;
        
        /*'diffLogLikPopnPair assumes 'diff' is subtracted from first cohort
         and added to second, which is what happens with orig-dest and pool.
         To instead add and subtract, we use -diff.         */
        double diffLogLikPopn = diffLogLikPopnPair( -diff,
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

/*
## READY_TO_TRANSLATE
## HAS_TESTS
diffLogLikAccountMoveComp <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(diffLogLikAccountMoveComp_R, combined)
    }
    else {
        account <- combined@account
        i.comp <- combined@iComp
        component <- combined@account@components[[i.comp]]
        population <- combined@account@population
        iterator <- combined@iteratorPopn
        data.models <- combined@dataModels
        datasets <- combined@datasets
        series.indices <- combined@seriesIndices
        transforms <- combined@transforms
        i.cell <- combined@iCell
        i.popn.next <- combined@iPopnNext
        diff <- combined@diffProp
        is.increment <- combined@isIncrement[i.comp]
        diff.log.lik.cell <- diffLogLikCellComp(diff = diff,
                                                iComp = i.comp,
                                                iCell = i.cell,
                                                component = component,
                                                dataModels = data.models,
                                                datasets = datasets,
                                                seriesIndices = series.indices,
                                                transforms = transforms)
        if (is.infinite(diff.log.lik.cell))
            return(diff.log.lik.cell)
        diff.popn <- if (is.increment) diff else -diff
        diff.log.lik.popn <- diffLogLikPopn(diff = diff.popn,
                                            iFirst = i.popn.next,
                                            iterator = iterator,
                                            population = population,
                                            dataModels = data.models,
                                            datasets = datasets,
                                            seriesIndices = series.indices,
                                            transforms = transforms)
        if (is.infinite(diff.log.lik.popn))
            return(diff.log.lik.popn)
        diff.log.lik.cell + diff.log.lik.popn
    }
}

*/


double 
diffLogLikAccountMoveComp(SEXP combined_R)
{
    /*        account <- combined@account
        i.comp <- combined@iComp
        component <- combined@account@components[[i.comp]]
        population <- combined@account@population
        iterator <- combined@iteratorPopn
        data.models <- combined@dataModels
        datasets <- combined@datasets
        series.indices <- combined@seriesIndices
        transforms <- combined@transforms
        i.cell <- combined@iCell
        i.popn.next <- combined@iPopnNext
        diff <- combined@diffProp
        is.increment <- combined@isIncrement[i.comp]
*/
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
    
    /* is.increment <- combined@isIncrement[i.comp] */
    
    double ans = 0;
    /*diff.log.lik.cell <- diffLogLikCellComp(diff = diff,
                                                iComp = i.comp,
                                                iCell = i.cell,
                                                component = component,
                                                dataModels = data.models,
                                                datasets = datasets,
                                                seriesIndices = series.indices,
                                                transforms = transforms)*/
    
    double diffLogLikCell = diffLogLikCellComp( diff, iComp_r,
                                        iCell_r, component_R, 
                                        dataModels_R, datasets_R, 
                                        seriesIndices_R, transforms_R);
    if (R_finite(diffLogLikCell) ) {

        ans += diffLogLikCell;
        
        /*diff.popn <- if (is.increment) diff else -diff
        diff.log.lik.popn <- diffLogLikPopn(diff = diff.popn,
                                            iFirst = i.popn.next,
                                            iterator = iterator,
                                            population = population,
                                            dataModels = data.models,
                                            datasets = datasets,
                                            seriesIndices = series.indices,
                                            transforms = transforms)
        */
        
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
                                    iCell_r, iterator_R, theta);
    }   
    else if (isOrigDest || isPool) {
        
        double ansOrig = diffLogDensPopnOneCohort(-diff, population_R, 
                                    iPopnNext_r, iterator_R, theta);
        
        double ansDest = diffLogDensPopnOneCohort(diff, population_R, 
                                    iPopnNextOther_r, iterator_R, theta);
        ans = ansOrig + ansDest;
        
    } 
    else if (isIntNet) {
        
        double ansAdd = diffLogDensPopnOneCohort(diff, population_R, 
                                    iPopnNext_r, iterator_R, theta);
        
        double ansSub = diffLogDensPopnOneCohort(-diff, population_R, 
                                    iPopnNextOther_r, iterator_R, theta);
        ans = ansAdd + ansSub;
        
    }
    else {
        
        if ( !isIncrementVec[iComp_r - 1] ) {
            diff = -diff;
        }
        
        ans = diffLogDensPopnOneCohort(diff, population_R, 
                                    iPopnNext_r, iterator_R, theta);
        
    }
    
    return ans;
}


double
diffLogDensPopnOneCohort(int diff, SEXP population_R, int i_r, 
                        SEXP iterator_R, double * theta)
{
    int * population = INTEGER(population_R);
    resetCP(iterator_R, i_r);
    double ans = 0;
    
    int * i_ptr = INTEGER(GET_SLOT(iterator_R, i_sym));
    int * finished_ptr = LOGICAL(GET_SLOT(iterator_R, finished_sym));
    
    int i = *i_ptr - 1;
    int finished = *finished_ptr;
    
    int valCurr = population[i];
    int valProp = valCurr + diff;
    double lambda = theta[i];
    
    double logDensProp = dpois(valProp, lambda, USE_LOG);
    double logDensCurr = dpois(valCurr, lambda, USE_LOG);
    
    ans += (logDensProp - logDensCurr);
    
    while (!finished) {
        
        advanceCP(iterator_R);
        i = *i_ptr - 1;
        finished = *finished_ptr;
        
        valCurr = population[i];
        valProp = valCurr + diff;
        lambda = theta[i];
        
        logDensProp = dpois(valProp, lambda, USE_LOG);
        logDensCurr = dpois(valCurr, lambda, USE_LOG);
        
        ans += (logDensProp - logDensCurr);
        
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
    
    int iExpFirst_r = *INTEGER(GET_SLOT(combined_R, iExpFirst_sym));
    SEXP iteratorExposure_R = GET_SLOT(combined_R, iteratorExposure_sym);
    
    int diff = *INTEGER(GET_SLOT(combined_R, diffProp_sym));
    
    int iOrigDest_r = *INTEGER(GET_SLOT(combined_R, iOrigDest_sym));
    int iPool_r = *INTEGER(GET_SLOT(combined_R, iPool_sym));
    
    int iBirths_r = *INTEGER(GET_SLOT(combined_R, iBirths_sym));
    int iParCh_r = *INTEGER(GET_SLOT(combined_R, iParCh_sym));
    
    double * exposure = REAL(GET_SLOT(combined_R, exposure_sym));
    
    SEXP systemModels_R = GET_SLOT(combined_R, systemModels_sym);
    
    int hasAge = *LOGICAL(GET_SLOT(combined_R, hasAge_sym));
    double ageTimeStep = *REAL(GET_SLOT(combined_R, ageTimeStep_sym));
    
    int updatedPopnTrue = 1;
    
    
    for (int i = 0; i < nComponents; ++i) {
        
        int next_i = i + 1;
        
        int usesExp = modelUsesExposure[next_i];
        
        if(usesExp) {
        
            SEXP component_R = VECTOR_ELT(components_R, i);
            
            SEXP thisModel_R = VECTOR_ELT(systemModels_R, next_i);
            double * theta = REAL(GET_SLOT(thisModel_R, theta_sym));
            
            SEXP iteratorComp_R = VECTOR_ELT(iteratorsComp_R, i);
            SEXP mappingFromExp_R = VECTOR_ELT(mappingsFromExp_R, i);
            
            int i_r = i + 1;
            int isOrigDest = (i_r == iOrigDest_r);
            int isPool = (i_r == iPool_r);
            int isBirths = (i_r == iBirths_r);  
            int isParCh = (i_r == iParCh_r);    
            
            double diffLog = 0;
            
            if(isOrigDest || isPool) {
                
                int iCell_r = getICellCompFromExp(iExpFirst_r, mappingFromExp_R);
                
                diffLog = diffLogDensExpOneOrigDestParChPool(iCell_r, 
                                        hasAge, ageTimeStep, updatedPopnTrue,
                                        component_R, theta, 
                                        iteratorComp_R, iExpFirst_r, 
                                        exposure, iteratorExposure_R,
                                        diff);
    
                
            }
            else if(isBirths) {
                
                int iCell_r = getICellBirthsFromExp(iExpFirst_r, mappingFromExp_R);
                
                int cellIsAffected = (iCell_r > 0);
                if (cellIsAffected) {
                    
                    if(isParCh) {
                        
                        diffLog = diffLogDensExpOneOrigDestParChPool(iCell_r, 
                                        hasAge, ageTimeStep, updatedPopnTrue,
                                        component_R, theta, 
                                        iteratorComp_R, iExpFirst_r, 
                                        exposure, iteratorExposure_R,
                                        diff);
                    }
                    
                    else {
                        
                        diffLog = diffLogDensExpOneComp(iCell_r, 
                                        hasAge, ageTimeStep, updatedPopnTrue,
                                        component_R, theta, 
                                        iteratorComp_R, iExpFirst_r, 
                                        exposure, iteratorExposure_R,
                                        diff);
                    }
                }
            }
            else {
                int iCell_r = getICellCompFromExp(iExpFirst_r, mappingFromExp_R);
                
                diffLog = diffLogDensExpOneComp(iCell_r, 
                                        hasAge, ageTimeStep, updatedPopnTrue,
                                        component_R, theta, 
                                        iteratorComp_R, iExpFirst_r, 
                                        exposure, iteratorExposure_R,
                                        diff);
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
                        double ageTimeStep, int updatedPopn,
                        SEXP component_R, double * theta,
                        SEXP iteratorComp_R, 
                        int iExpFirst_r, double * exposure,
                        SEXP iteratorExposure_R,
                        int diff)

{
    double ans = 0;
    
    int * component = INTEGER(component_R);
    
    resetCODPCP(iteratorComp_R, iCell_r);
    resetCC(iteratorExposure_R, iExpFirst_r);
    
    int lengthVec = *INTEGER(GET_SLOT(iteratorComp_R, lengthVec_sym));
    
    int * iExp_ptr = INTEGER(GET_SLOT(iteratorExposure_R, i_sym)); 
    int *iCompVec = INTEGER(GET_SLOT(iteratorComp_R, iVec_sym));
    
    int * finishedComp_ptr = LOGICAL(GET_SLOT(iteratorComp_R, finished_sym)); 
    
    int keepGoing = 1;
    while (keepGoing) { 
       
       int iExp_r = *iExp_ptr;
       
       double diffExposure = 0;
       
       if (hasAge) {
           diffExposure = 0.5 * diff * ageTimeStep;
       }
       else {
           
           int isFirstCell = (iExp_r == iExpFirst_r);
           if (isFirstCell && !updatedPopn) {
               diffExposure = 0.5 * diff * ageTimeStep;
           }
            else {
               diffExposure = diff * ageTimeStep;
            }
        }
        double exposureCurr = exposure[iExp_r - 1];
        double exposureProp = exposureCurr + diffExposure;
        
        int j = 0;
        
        while (j < lengthVec && keepGoing ) {
            int iComp = iCompVec[j] - 1;
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
            
            ++j;
        } 

        if (keepGoing) {
            int finishedComp = *finishedComp_ptr;
            if (finishedComp) {
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
                        double ageTimeStep, int updatedPopn,
                        SEXP component_R, double * theta,
                        SEXP iteratorComp_R, 
                        int iExpFirst_r, double * exposure,
                        SEXP iteratorExposure_R,
                        int diff)
{
    double ans = 0;
    
    int * component = INTEGER(component_R);
    
    resetCC(iteratorComp_R, iCell_r);
    resetCC(iteratorExposure_R, iExpFirst_r);
    
    int * iExp_ptr = INTEGER(GET_SLOT(iteratorExposure_R, i_sym)); 
    int * iComp_ptr = INTEGER(GET_SLOT(iteratorComp_R, i_sym)); 
    
    int * finishedComp_ptr = LOGICAL(GET_SLOT(iteratorComp_R, finished_sym)); 
    
    int keepGoing = 1;
    
    while (keepGoing) { 
       
       int iComp_r = *iComp_ptr;
       int iExp = *iExp_ptr - 1;
       int iComp = iComp_r - 1;
       
       int compCurr = component[iComp];
       
       double diffExposure = 0;
       
       if (hasAge) {
           diffExposure = 0.5 * diff * ageTimeStep;
       }
       else {
           
           int isFirstCell = (iComp_r == iCell_r);
           if (isFirstCell && !updatedPopn) {
               diffExposure = 0.5 * diff * ageTimeStep;
           }
            else {
               diffExposure = diff * ageTimeStep;
            }
        }
        double exposureCurr = exposure[iExp];
        double exposureProp = exposureCurr + diffExposure;
        
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
        
        if (keepGoing) {
            int finishedComp = *finishedComp_ptr;
            if (finishedComp) {
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
    
    int hasAge = *LOGICAL(GET_SLOT(combined_R, hasAge_sym));
    double ageTimeStep = *REAL(GET_SLOT(combined_R, ageTimeStep_sym));
    
    int iCell = iCell_r - 1;
    int iExposure = iExposure_r - 1;
    
    double thetaCell = theta[iCell];
    double exposureCellCurr = exposure[iExposure];
    double exposureCellJump = expectedExposure[iExposure];
    
    double exposureCellProp = 0;
    if (hasAge) {
        int isLowerTriangle = *LOGICAL(GET_SLOT(combined_R, isLowerTriangle_sym));
        
        if(isLowerTriangle) {
            exposureCellProp = exposureCellCurr - 0.5 * diff * ageTimeStep;
        }
        else {
            exposureCellProp = exposureCellCurr;
        }
    }
    else {
        exposureCellProp = exposureCellCurr - 0.5 * diff * ageTimeStep;
    }
    
    double lambdaDensProp = thetaCell * exposureCellProp;
    double lambdaJump = thetaCell * exposureCellJump;
    int valCurr = component[iCell];
    int valProp = valCurr + diff;
    
    double diffLogDens = dpois(valProp, lambdaDensProp, USE_LOG) - 
                            dpois(valCurr, lambdaDensProp, USE_LOG);
    double diffLogJump = dpois(valCurr, lambdaJump, USE_LOG) - 
                            dpois(valProp, lambdaJump, USE_LOG);
    double ans = diffLogDens + diffLogJump;
    
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
    
    int iExpFirstOrig_r = *INTEGER(GET_SLOT(combined_R, iExpFirst_sym));
    int iExpFirstDest_r = *INTEGER(GET_SLOT(combined_R, iExpFirstOther_sym));
    
    SEXP iteratorExposure_R = GET_SLOT(combined_R, iteratorExposure_sym);
    
    int diff = *INTEGER(GET_SLOT(combined_R, diffProp_sym));
    
    int iComp_r = *INTEGER(GET_SLOT(combined_R, iComp_sym));
    int iOrigDest_r = *INTEGER(GET_SLOT(combined_R, iOrigDest_sym));
    int iPool_r = *INTEGER(GET_SLOT(combined_R, iPool_sym));
    int iIntNet_r = *INTEGER(GET_SLOT(combined_R, iIntNet_sym));
    
    int iBirths_r = *INTEGER(GET_SLOT(combined_R, iBirths_sym));
    int iParCh_r = *INTEGER(GET_SLOT(combined_R, iParCh_sym));
    
    double * exposure = REAL(GET_SLOT(combined_R, exposure_sym));
    
    SEXP systemModels_R = GET_SLOT(combined_R, systemModels_sym);
    
    int hasAge = *LOGICAL(GET_SLOT(combined_R, hasAge_sym));
    double ageTimeStep = *REAL(GET_SLOT(combined_R, ageTimeStep_sym));
    
    int updatedPopnFalse = 0;
    
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
                SEXP iteratorComp_R = VECTOR_ELT(iteratorsComp_R, i);
                SEXP mappingFromExposure_R = VECTOR_ELT(mappingsFromExposure_R, i);
                
                int isOrigDest = (i == iOrigDest_r - 1);
                int isPool = (i == iPool_r - 1);
                int isBirths = (i == iBirths_r - 1);
                int isParCh = (i == iParCh_r - 1);
                
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
                                                    component_R, theta,
                                                    iteratorComp_R,
                                                    iExpFirstOrig_r,
                                                    exposure,
                                                    iteratorExposure_R,
                                                    diffOrig);
                    if (R_finite(diffLogOrig)) {
                        
                        diffLogDest 
                            = diffLogDensExpOneOrigDestParChPool(iCellDest_r,
                                                    hasAge, ageTimeStep,
                                                    updatedPopnFalse,
                                                    component_R, theta,
                                                    iteratorComp_R,
                                                    iExpFirstDest_r,
                                                    exposure,
                                                    iteratorExposure_R,
                                                    diffDest);
                    }
                } /* end if isOrigDest || isPool */
                
                else if(isBirths) {
                    int iCellOrig_r = getICellBirthsFromExp(iExpFirstOrig_r,
                                                mappingFromExposure_R);
                    int iCellDest_r = getICellBirthsFromExp(iExpFirstDest_r,
                                                mappingFromExposure_R);
                    
                    int cellIsAffected = (iCellOrig_r > 0);
                    if(cellIsAffected) {
                        if (isParCh) {
                            
                            diffLogOrig 
                            = diffLogDensExpOneOrigDestParChPool(iCellOrig_r,
                                                    hasAge, ageTimeStep,
                                                    updatedPopnFalse,
                                                    component_R, theta,
                                                    iteratorComp_R,
                                                    iExpFirstOrig_r,
                                                    exposure,
                                                    iteratorExposure_R,
                                                    diffOrig);
                                                    
                            if (R_finite(diffLogOrig)) {
                        
                                diffLogDest 
                                    = diffLogDensExpOneOrigDestParChPool(iCellDest_r,
                                                            hasAge, ageTimeStep,
                                                            updatedPopnFalse,
                                                            component_R, theta,
                                                            iteratorComp_R,
                                                            iExpFirstDest_r,
                                                            exposure,
                                                            iteratorExposure_R,
                                                            diffDest);
                            }
                        }
                        else { /* not isParCh */
                            
                            diffLogOrig 
                            = diffLogDensExpOneComp(iCellOrig_r,
                                                    hasAge, ageTimeStep,
                                                    updatedPopnFalse,
                                                    component_R, theta,
                                                    iteratorComp_R,
                                                    iExpFirstOrig_r,
                                                    exposure,
                                                    iteratorExposure_R,
                                                    diffOrig);
                                                    
                            if (R_finite(diffLogOrig)) {
                        
                                diffLogDest 
                                    = diffLogDensExpOneComp(iCellDest_r,
                                                            hasAge, ageTimeStep,
                                                            updatedPopnFalse,
                                                            component_R, theta,
                                                            iteratorComp_R,
                                                            iExpFirstDest_r,
                                                            exposure,
                                                            iteratorExposure_R,
                                                            diffDest);
                                
                            }
                            
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
                                                    component_R, theta,
                                                    iteratorComp_R,
                                                    iExpFirstOrig_r,
                                                    exposure,
                                                    iteratorExposure_R,
                                                    diffOrig);
                                                    
                    if (R_finite(diffLogOrig)) {
                
                        diffLogDest 
                            = diffLogDensExpOneComp(iCellDest_r,
                                                    hasAge, ageTimeStep,
                                                    updatedPopnFalse,
                                                    component_R, theta,
                                                    iteratorComp_R,
                                                    iExpFirstDest_r,
                                                    exposure,
                                                    iteratorExposure_R,
                                                    diffDest);
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
    
    int diff = *INTEGER(GET_SLOT(combined_R, diffProp_sym));
    
    int hasAge = *LOGICAL(GET_SLOT(combined_R, hasAge_sym));
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
    
    double exposureOutProp = 0;
    double exposureInProp = 0;
    
    if (hasAge) {
        int isLowerTriangle = *LOGICAL(GET_SLOT(combined_R, isLowerTriangle_sym));
        
        if(isLowerTriangle) {
            exposureOutProp = exposureOutCurr - 0.5 * diff * ageTimeStep;
            exposureInProp = exposureInCurr + 0.5 * diff * ageTimeStep;
        }
        else {
            exposureOutProp = exposureOutCurr;
            exposureInProp = exposureInCurr;
        }
    }
    else {
        exposureOutProp = exposureOutCurr - 0.5 * diff * ageTimeStep;
        exposureInProp = exposureInCurr + 0.5 * diff * ageTimeStep;
    }
    
    double lambdaDensOutProp = thetaOut * exposureOutProp;
    double lambdaDensInProp = thetaIn * exposureInProp;
    double lambdaDensOutCurr = thetaOut * exposureOutCurr;
    double lambdaDensInCurr = thetaIn * exposureInCurr;
    double lambdaJump = thetaOut * exposureOutJump;
    int valOutCurr = component[iCellOut];
    int valInCurr = component[iCellIn];
    int valOutProp = valOutCurr + diff;
    int valInProp = valInCurr + diff;
    
    double diffLogDens = dpois(valOutProp, lambdaDensOutProp, USE_LOG)  
                          - dpois(valOutCurr, lambdaDensOutCurr, USE_LOG)
                          + dpois(valInProp, lambdaDensInProp, USE_LOG) 
                          - dpois(valInCurr, lambdaDensInCurr, USE_LOG);
    double diffLogJump = dpois(valOutCurr, lambdaJump, USE_LOG) 
                          - dpois(valOutProp, lambdaJump, USE_LOG);
    double ans = diffLogDens + diffLogJump;
    
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

    SEXP systemModels_R = GET_SLOT(combined_R, systemModels_sym);
    SEXP thisSystemModel_R = VECTOR_ELT(systemModels_R, iComp_r); /* iComp_r is c-style index + 1*/
    SEXP theta_R = GET_SLOT(thisSystemModel_R, theta_sym);
    double * theta = REAL(theta_R);
    
    double * exposure = REAL(GET_SLOT(combined_R, exposure_sym));
    double * expectedExposure = REAL(GET_SLOT(combined_R, expectedExposure_sym));
    
    int iCell_r = *INTEGER(GET_SLOT(combined_R, iCell_sym));
    int iExposure_r = *INTEGER(GET_SLOT(combined_R, iExposure_sym));
    
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
    
    int isIncrement = isIncrementVec[iComp_r -1];
    
    if (hasAge) {
        int isLowerTriangle = *LOGICAL(GET_SLOT(combined_R, isLowerTriangle_sym));
        
        if(isLowerTriangle) {

            if (isIncrement) {
                exposureCellProp = exposureCellCurr + 0.5 * diff * ageTimeStep;
            }
            else {
                exposureCellProp = exposureCellCurr - 0.5 * diff * ageTimeStep;
            }
        }
        else {
            exposureCellProp = exposureCellCurr;
        }
    }
    else {
        if (isIncrement) {
            exposureCellProp = exposureCellCurr + 0.5 * diff * ageTimeStep;
        }
        else {
            exposureCellProp = exposureCellCurr - 0.5 * diff * ageTimeStep;
        }
    }
    
    double lambdaDensProp = thetaCell * exposureCellProp;
    double lambdaJump = thetaCell * exposureCellJump;
    int valCurr = component[iCell];
    int valProp = valCurr + diff;
    
    double diffLogDens = dpois(valProp, lambdaDensProp, USE_LOG)  
                          - dpois(valCurr, lambdaDensProp, USE_LOG);
    double diffLogJump = dpois(valCurr, lambdaJump, USE_LOG) 
                          - dpois(valProp, lambdaJump, USE_LOG);
    double ans = diffLogDens + diffLogJump;
    
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
    
    int iExpFirst_r = *INTEGER(GET_SLOT(combined_R, iExpFirst_sym));
    
    SEXP iteratorExposure_R = GET_SLOT(combined_R, iteratorExposure_sym);
    
    int * isIncrementVec = LOGICAL(GET_SLOT(combined_R, isIncrement_sym));
    
    int diff = *INTEGER(GET_SLOT(combined_R, diffProp_sym));
    
    int iComp_r = *INTEGER(GET_SLOT(combined_R, iComp_sym));
    int iOrigDest_r = *INTEGER(GET_SLOT(combined_R, iOrigDest_sym));
    int iPool_r = *INTEGER(GET_SLOT(combined_R, iPool_sym));
    int iBirths_r = *INTEGER(GET_SLOT(combined_R, iBirths_sym));
    int iParCh_r = *INTEGER(GET_SLOT(combined_R, iParCh_sym));
    
    double * exposure = REAL(GET_SLOT(combined_R, exposure_sym));
    
    SEXP systemModels_R = GET_SLOT(combined_R, systemModels_sym);
    
    int hasAge = *LOGICAL(GET_SLOT(combined_R, hasAge_sym));
    double ageTimeStep = *REAL(GET_SLOT(combined_R, ageTimeStep_sym));
    
    int updatedPopnFalse = 0;
    
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
                SEXP iteratorComp_R = VECTOR_ELT(iteratorsComp_R, i);
                SEXP mappingFromExposure_R = VECTOR_ELT(mappingsFromExposure_R, i);
                
                int isOrigDest = (i == iOrigDest_r - 1);
                int isPool = (i == iPool_r - 1);
                int isBirths = (i == iBirths_r - 1);
                int isParCh = (i == iParCh_r - 1);
                
                double diffLog = 0;
                
                if (isOrigDest || isPool) {
                    
                    int iCell_r = getICellCompFromExp(iExpFirst_r, 
                                                mappingFromExposure_R);
                    diffLog = diffLogDensExpOneOrigDestParChPool(iCell_r,
                                                    hasAge, ageTimeStep,
                                                    updatedPopnFalse,
                                                    component_R, theta,
                                                    iteratorComp_R,
                                                    iExpFirst_r,
                                                    exposure,
                                                    iteratorExposure_R,
                                                    diff);
                } /* end if isOrigDest || isPool */
                
                else if(isBirths) {
                    int iCell_r = getICellBirthsFromExp(iExpFirst_r,
                                                mappingFromExposure_R);
                                                
                    int cellIsAffected = (iCell_r > 0);
                    if(cellIsAffected) {
                        if (isParCh) {
                            
                            diffLog 
                            = diffLogDensExpOneOrigDestParChPool(iCell_r,
                                                    hasAge, ageTimeStep,
                                                    updatedPopnFalse,
                                                    component_R, theta,
                                                    iteratorComp_R,
                                                    iExpFirst_r,
                                                    exposure,
                                                    iteratorExposure_R,
                                                    diff);
                                                    
                        }
                        else { /* not isParCh */
                            
                            diffLog
                            = diffLogDensExpOneComp(iCell_r,
                                                    hasAge, ageTimeStep,
                                                    updatedPopnFalse,
                                                    component_R, theta,
                                                    iteratorComp_R,
                                                    iExpFirst_r,
                                                    exposure,
                                                    iteratorExposure_R,
                                                    diff);
                                                    
                        }
                    } /* end cellIsAffected */
                    
                } /* end if isBirths */
                
                else { /* is net */
                    
                    int iCell_r = getICellCompFromExp(iExpFirst_r,
                                                mappingFromExposure_R);
                    diffLog = diffLogDensExpOneComp(iCell_r,
                                                    hasAge, ageTimeStep,
                                                    updatedPopnFalse,
                                                    component_R, theta,
                                                    iteratorComp_R,
                                                    iExpFirst_r,
                                                    exposure,
                                                    iteratorExposure_R,
                                                    diff);
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



/* ******************* UPDATE VALUES ***************** */

void 
updateCellMove(SEXP combined_R)
{
    int iComp_r = *INTEGER(GET_SLOT(combined_R, iComp_sym));
    int iCell_r = *INTEGER(GET_SLOT(combined_R, iCell_sym));
    int iCellOther_r = *INTEGER(GET_SLOT(combined_R, iCellOther_sym));
    int iPool_r = *INTEGER(GET_SLOT(combined_R, iPool_sym));
    int iIntNet_r = *INTEGER(GET_SLOT(combined_R, iIntNet_sym));
   
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
        else if(isIntNet) {
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
    
    SEXP iterator_R;
    PROTECT(iterator_R = duplicate(GET_SLOT(combined_R, iteratorPopn_sym)));
   
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
    UNPROTECT(1); /* iterator_R */
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
    
        SEXP iterator_R;
        PROTECT(iterator_R = duplicate(GET_SLOT(combined_R, iteratorAcc_sym)));
       
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
        UNPROTECT(1); /* iterator_R */
    } /* end if !noSubsequentAccession */
}



void 
updateSubsequentExpMove(SEXP combined_R)
{
    int iComp_r = *INTEGER(GET_SLOT(combined_R, iComp_sym));
    int iOrigDest_r = *INTEGER(GET_SLOT(combined_R, iOrigDest_sym));
    int iPool_r = *INTEGER(GET_SLOT(combined_R, iPool_sym));
    int iIntNet_r = *INTEGER(GET_SLOT(combined_R, iIntNet_sym));
    int iExpFirst_r = *INTEGER(GET_SLOT(combined_R, iExpFirst_sym));
    
    int noSubsequentExposure = (iExpFirst_r == 0);
    
    if (!noSubsequentExposure) {
    
        SEXP iterator_R;
        PROTECT(iterator_R = duplicate(GET_SLOT(combined_R, iteratorExposure_sym)));
       
        int diff = *INTEGER(GET_SLOT(combined_R, diffProp_sym));
        
        int * isIncrementVec = LOGICAL(GET_SLOT(combined_R, isIncrement_sym));
        
        int hasAge = *LOGICAL(GET_SLOT(combined_R, hasAge_sym));
        double ageTimeStep = *REAL(GET_SLOT(combined_R, ageTimeStep_sym));
        
        int isPopn = (iComp_r == 0);
        int isOrigDest = (iComp_r == iOrigDest_r);
        int isPool = (iComp_r == iPool_r);
        int isIntNet = (iComp_r == iIntNet_r);
        
        int updateTwoCohorts = (isOrigDest || isPool || isIntNet);
        
        double * exposure = REAL(GET_SLOT(combined_R, exposure_sym));
        
        double diffDouble = diff;
        
        if (isPopn) {
            
            diffDouble *= ageTimeStep;
            if(hasAge) {
                diffDouble *= 0.5;
            }
            
            resetCC(iterator_R, iExpFirst_r);
            int * i_ptr = INTEGER(GET_SLOT(iterator_R, i_sym));
            int * finished_ptr = LOGICAL(GET_SLOT(iterator_R, finished_sym));
            int i = *i_ptr - 1;
            int finished = *finished_ptr;
            exposure[i] += diffDouble;
            
            while(!finished) {
                advanceCC(iterator_R);
                i = *i_ptr - 1 ;
                finished = *finished_ptr;
                exposure[i] += diffDouble;
            }
        }
        else if(updateTwoCohorts) {
            
            int iExpFirstOther_r = *INTEGER(GET_SLOT(combined_R, iExpFirstOther_sym));
            
            diffDouble *= 0.5 * ageTimeStep;
            
            double diffOrig = diffDouble;
            double diffDest = -diffDouble;
            
            if(isOrigDest || isPool) {
                diffOrig = -diffDouble;
                diffDest = diffDouble;
            }
            
            exposure[iExpFirst_r - 1] += diffOrig;
            exposure[iExpFirstOther_r - 1] += diffDest;
            
            if ( !hasAge ) {
                diffOrig *= 2;
                diffDest *= 2;
            }
            
            SEXP iteratorDest_R;
            PROTECT(iteratorDest_R = duplicate(iterator_R));
            resetCC(iterator_R, iExpFirst_r);
            resetCC(iteratorDest_R, iExpFirstOther_r);
            
            int * iOrig_ptr = INTEGER(GET_SLOT(iterator_R, i_sym));
            int * finishedOrig_ptr = LOGICAL(GET_SLOT(iterator_R, finished_sym));
            int * iDest_ptr = INTEGER(GET_SLOT(iteratorDest_R, i_sym));
            int finishedOrig = *finishedOrig_ptr;
            
            while(!finishedOrig) {
                advanceCC(iterator_R);
                advanceCC(iteratorDest_R);
                int iOrig = *iOrig_ptr - 1;
                int iDest = *iDest_ptr - 1;
                finishedOrig = *finishedOrig_ptr;
                exposure[iOrig] += diffOrig;
                exposure[iDest] += diffDest;
                
            }
            
            UNPROTECT(1); /* iteratorDest_R */
        }
        else {
            int iComp = iComp_r - 1;
            int isIncrement = isIncrementVec[iComp];
            
            diffDouble *= 0.5 * ageTimeStep;
            
            if(!isIncrement) {
                diffDouble = -diffDouble;
            }
            
            exposure[iExpFirst_r - 1] += diffDouble;
            
            if ( !hasAge ) {
                diffDouble *= 2;
            }
            
            resetCC(iterator_R, iExpFirst_r);
            int * i_ptr = INTEGER(GET_SLOT(iterator_R, i_sym));
            int * finished_ptr = LOGICAL(GET_SLOT(iterator_R, finished_sym));
            int finished = *finished_ptr;
            
            while(!finished) {
                advanceCC(iterator_R);
                int i = *i_ptr - 1;
                finished = *finished_ptr;
                exposure[i] += diffDouble;
            }
            
        }
        UNPROTECT(1); /* iterator_R */
    } /* end if !noSubsequentExposure */
}

