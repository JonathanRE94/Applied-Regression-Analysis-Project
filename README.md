Applied different Data analysis methods In R to create a Reduced regression model that influences the salaries among programmers and engineers in the Silicon Valley in the Census year 2000, ultimately a better fit than the original data base model.


The Reduced model is much better than the initial Base model based on fit, exaplining variance and, predictive capability.

Following are the main data predictors in significance order:
-Wage income (wageinc)
-Education level (educ)
-Age (age)
-Number of weeks worked (wkswrkd)
-Occupation type (occ)
-Sex type (sex)
-English Proficiency (engl)
-Citizenship (cit)
-Place of Birth (powspuma)
-Year of Entry (yrentry)
-Birth Year (birth)


The below model is the Base model before regression analysis:
Initial Base model <- wageinc ~age + cit + educ + engl + occ + birth + 
                        sex + wkswrkd + yrentry + powspuma

Following is the Reduced model after regression analysis with interaction variables including non-linear polynomial terms:
Reduced model <- logwageinc ~ educ + age + wkswrkd + occ + sex + engl + 
                             cit + I(age^2) + I(wkswrkd^2) + educ:occ + wkswrkd:occ + 
                             educ:age + age:engl + educ:sex + educ:wkswrkd + sex:engl + 
                             wkswrkd:engl + wkswrkd:sex


