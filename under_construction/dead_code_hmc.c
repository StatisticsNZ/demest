
void
updateBetas(SEXP object_R)
{
  updateBetasGibbs(object_R);
  updateBetasHMC(object_R);
}


  int *useHMCToUpdateBeta = LOGICAL(GET_SLOT(object_R, useHMCToUpdateBeta_sym));

  ADD_SYM(useHMCBetas);
  ADD_SYM(betasOld);
  ADD_SYM(gradientBetas);
  ADD_SYM(momentumBetas);
  ADD_SYM(useHMCToUpdateBeta);
  ADD_SYM(sizeStep);
  ADD_SYM(nStep);


  useHMCBetas_sym,
  betasOld_sym,
  gradientBetas_sym,
  momentumBetas_sym,
  useHMCToUpdateBeta_sym,
  sizeStep_sym,
  nStep_sym,
