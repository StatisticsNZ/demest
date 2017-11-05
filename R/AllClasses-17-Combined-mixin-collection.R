
setClass("AccountMixin",
         contains = c("VIRTUAL",
                      "CumProbPopnMixin",
                      "DescriptionsMixin",
                      "DiffPropMixin",
                      "ExpectedExposureMixin",
                      "ExposureMixin",
                      "GeneratedNewProposalMixin",
                      "HasAgeMixin",
                      "ICellMixin",
                      "ICompMixin",
                      "IExpFirstMixin",
                      "IExposureMixin",
                      "IPopnNextMixin",
                      "IsIncrementMixin",
                      "IsNetMixin",
                      "IteratorExposureMixin",
                      "IteratorPopnMixin",
                      "IteratorsCompMixin",
                      "MappingsFromExpMixin",
                      "MappingsToExpMixin",
                      "MappingsToPopnMixin",
                      "MaxAttemptMixin",
                      "NCellAccountMixin",
                      "ProbPopnMixin"))

setClass("MovementsAgeMixin",
         contains = c("VIRTUAL",
                      "AccessionMixin",
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
         
