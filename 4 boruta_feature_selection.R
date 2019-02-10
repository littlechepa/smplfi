
#Train the boruta model
boruta.train <- Boruta(y ~ ., data = train, doTrace = 2)
print(boruta.train)

#Take decision on tentative attributes
final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)

#obtain the list of confirmed attributes
getSelectedAttributes(final.boruta, withTentative = FALSE)

#create a data frame of the final result derived from Boruta.
boruta.df <- attStats(final.boruta)
class(boruta.df)

print(boruta.df)

boruta_confirmed_formula <- getConfirmedFormula(final.boruta)

boruta_imp_features <- getSelectedAttributes(final.boruta)
