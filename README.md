
Target columns and remove columns

Avganncount:  remove, redundant 

Avgdeathsperyear: remove, redundant

Target_deathrate: Predicting variable, dependent, KEEP IT

incidecerate: leaning towards removal, but could keep it
  I think remove as this would be a dependent variable in this problem, 
  so it would not be a meaningful predictor for death rate

Medincome: keep it

Popest2015: remove, predicting variable is predicted per 100,000 people, so population is insignificant

Poverty percent: keep it

Studypercap: keep it, dollars allocated to an individual in studies in that county

Binnedinc: remove it

Medianage: keep it

Medianagemale: remove it

Medianagefemale: remove it, concerned about if we keep male and female and male is kept in our models, but not female, seems redundant and extra work when we just have median age

Geography: remove it

Percent married: keep it, health outcomes are often connected to marriage

pctnohs18_24: remove it, small age range, adds extra noise

pcths18_24: remove it, redundant

Pctsomecol18_24: remove it, small age range, redundant

Pctbachdeg18_24: remove it, small age range

Pcths25_over: keep it, gives education outcomes for a large age range of pops

pctbachdeg25_over: keep it, gives education outcomes for large age range of pops

pctemployed16_over: remove, redundant with unemployed
   I think keep because the unemployment rate only counts people who are actively looking for a job and can't find one,
   so children + retired people + stay-at-home parents are not counted in either employed or unemployed.
   The percent of people in a county who do not work because they don't have to could be a good indicator of the
   county's wealth, which may be a good indicator of cancer death rates.

pctunemployed16_over: keep this, is more valuable information compared to employed, it you are not in the unemployed percentage, then you are employed

Pctprivatecoverage: keep it

Pctprivatecoveragealone: remove it, redundant

Pctempprivcoverage: remove it, redundant

pctpubliccoverage: keep it

Pctpubliccoveragealone: remove it, redundant

Pct white: keep it

Pct black: keep it

Pctasian: keep it

Pctotherrace: remove it, redundant

Pctmarriedhouseholds: remove it

birthrate: remove it, not sure exactly what is measuring and has low correlation rate to target_deathrate


Columns to keep:
- target_deathrate
- Medincome
- Povertypercent
- Studypercap
- Medianage
- Percentmarried
- pcths25_over
- pctbachdeg25_over
- pctemployed16_over
- pctunemployed16_over
- Pctprivatecoverage
- Pctpubliccoverage
- Pctwhite
- Pctblack
- Pctasian 
