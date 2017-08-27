
setClass("AccountMixin",
         contains = c("VIRTUAL",
                      "CumProbPopnMixin",
                      "DiffPropMixin",
                      "ExposureMixin",
                      "GeneratedNewProposalMixin",
                      "HasAgeMixin",
                      "ICellMixin",
                      "ICompMixin",
                      "IExpFirstMixin",
                      "IExposureMixin",
                      "IPopnNextMixin",
                      "IsIncrementMixin",
                      "IteratorPopnMixin",
                      "MappingsFromExpMixin",
                      "MappingsToExpMixin",
                      "MappingsToPopnMixin",
                      "NCellAccountMixin",
                      "ProbPopnMixin"))

setClass("MovementsAgeMixin",
         contains = c("VIRTUAL",
                      "Accession",
                      "IAccNextMixin",
                      "IsLowerTriangleMixin",
                      "IteratorAccMixin",
                      "MappingsToAccMixin"))

setClass("ObservationMixin",
         contains = c("VIRTUAL",
                      "ObservationModelsMixin",
                      "DatasetsMixin",
                      "NamesDatasetsMixin",
                      "TransformsMixin"))

setClass("SystemMixin",
         contains = c("VIRTUAL",
                      "SystemModelsMixin",
                      "ModelUsesExposureMixin"))
         
