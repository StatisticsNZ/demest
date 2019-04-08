
setClass("AccountMixin",
         contains = c("VIRTUAL",
                      "AgeTimeStepMixin",
                      "ProbAccountMixin",
                      "DescriptionsMixin",
                      "DiffPropMixin",
                      "ExpectedExposureMixin",
                      "ExposureMixin",
                      "GeneratedNewProposalMixin",
                      "HasAgeMixin",
                      "ICellMixin",
                      "ICompMixin",
                      "IExpFirstMixin",
                      "IsIncrementMixin",
                      "IsNetMixin",
                      "IteratorsAccountMixin",
                      "MappingsAccountMixin",
                      "MaxAttemptMixin",
                      "NCellAccountMixin",
                      "ScaleNoiseMixin"))


setClass("MovementsAgeMixin",
         contains = c("VIRTUAL",
                      "AccessionMixin",
                      "IAccNextMixin",
                      "IsLowerTriangleMixin",
                      "IteratorAccMixin",
                      "MappingsToAccMixin"))

setClass("DataMixin",
         contains = c("VIRTUAL",
                      "DataModelsMixin",
                      "DataModelsUseAgMixin",
                      "DatasetsMixin",
                      "NamesDatasetsMixin",
                      "TransformsMixin"))

setClass("SystemMixin",
         contains = c("VIRTUAL",
                      "SystemModelsMixin",
                      "SystemModelsUseAgMixin",
                      "ModelUsesExposureMixin",
                      "TransformExpToBirthsMixin",
                      "TransformsExpToCompMixin"))
         
