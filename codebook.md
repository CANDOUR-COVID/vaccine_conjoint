---
title: "Codebook for CANDOUR Conjoint Analysis"
author: "Raymond Duch et al"
date: "19/12/2020"
output: pdf_document
---



All data used for the conjoint analysis and variables is stored within the `data/nbh_clean_global.*` files. We provide a raw `.CSV` version and a formatted `.Rds` file which contains the same data but with factor-variable formatting pre-applied to the conjoint attribute variables.

Each row of the dataset is a hypothetical vaccine recipient, or "profile". Two profiles comprise a single conjoint "round", and forms the basis of the dichotomous choice the respondent makes between potential recipients. Each respondent is presented with 8 rounds of of the conjoint experiment, and therefore makes 8 choices.

## Respondent Characteristics

**id** -- *Character variable* -- Distinguishes individual respondents in the dataset, and takes the form of `[country]_` and a unique number corresponding to each respondent within that country.


|        |     id|
|:-------|------:|
|Obs.    | 214960|
|Unique  |  13435|
|Missing |      0|
E.g.,

```
## [1] "Canada_1, Canada_2, Canada_3, Canada_4, Canada_5, ..."
```

**country** -- Country location of respondent. 


|Value     |  Obs.|
|:---------|-----:|
|Australia | 21600|
|Brazil    | 22784|
|Canada    | 18400|
|Chile     | 17088|
|China     | 20656|
|Colombia  | 13728|
|France    | 16496|
|India     | 10864|
|Italy     | 16976|
|Spain     | 18448|
|Uganda    |  2688|
|UK        | 16912|
|US        | 18320|
|Missing   |     0|

\newpage

**age** -- Age of respondent.


|         |age    |
|:--------|:------|
|Obs.     |214960 |
|Mean     |44.7   |
|Std.Dev. |47.4   |
|Min.     |2      |
|Max.     |1987   |
|Missing  |0      |


**gender** -- Gender of respondent.


|Value   |   Obs.|
|:-------|------:|
|Female  | 107136|
|Male    | 107024|
|Other   |    272|
|Missing |    528|


**ideology** -- Ideological self-placement of respondent on an 11-point scale from 0 (Left) to 10 (Right). Note, this question was not asked in China. 


|         |ideology |
|:--------|:--------|
|Obs.     |214960   |
|Mean     |5.2      |
|Std.Dev. |2.6      |
|Min.     |0        |
|Max.     |10       |
|Missing  |50592    |


**ind_inc** -- Dichotomous classification of respondents' income level. `High` and `Low` values are relative to the median estimated income for each country.


|Value   |   Obs.|
|:-------|------:|
|High    |  85440|
|Low     | 106176|
|Missing |  23344|



\newpage

**education** -- Trichotomous classification of respondents' level of education, relative to the educational levels within each country.


|Value   |  Obs.|
|:-------|-----:|
|High    | 98816|
|Medium  | 75616|
|Low     | 30224|
|Missing | 10304|


**hes_covid_2** -- Five-point Likert scale from `Strongly agree` to `Strongly disagree`, asking respondents' opinions on the following statement: "I am concerned about serious side effects of the COVID-19 vaccine".


|Value                      |  Obs.|
|:--------------------------|-----:|
|Strongly agree             | 64400|
|Agree                      | 75312|
|Disagree                   | 21504|
|Neither agree nor disagree | 39632|
|Strongly disagree          |  8864|
|Do not know                |  5040|
|Missing                    |   208|


**wtp_access** -- Response to the following prompt:

"Talking about vaccines in general, in some countries vaccines are only available from the government either at low or no cost. In some countries vaccines are only available for private purchase. And in some countries vaccines are available from the government but citizens can pay privately to gain early access.

Which of these three approaches do you think should be applied to the COVID-19 vaccine? Would you prefer

* Vaccines only made available by government at low or no cost?
* Vaccines are only available for private purchase?
* Vaccines made available by government but citizens can pay privately to gain access?"


|Value                                                                               |   Obs.|
|:-----------------------------------------------------------------------------------|------:|
|Do not know                                                                         |  14448|
|Vaccines only made available by government at low or no cost                        | 155568|
|Vaccines made available by government but citizens can pay privately to gain access |  38608|
|Vaccines are only available for private purchase                                    |   6128|
|Prefer not to say                                                                   |    208|
|Missing                                                                             |      0|


\newpage
**wtp_private** -- Response to the following prompt:

"Consider the following situation: a COVID-19 vaccine becomes available and is provided by government health agencies. For 80 out of 100 people the vaccine would provide protection for at least 18 months. But there are limited initial supplies of the vaccine. For this reason, you would have to wait 6 months before you could receive it. If a COVID-19 vaccine was also available for private purchase and you could receive it immediately would you considering buying it?"


|Value             |  Obs.|
|:-----------------|-----:|
|Do not know       | 47440|
|No                | 70224|
|Yes               | 97152|
|Prefer not to say |   144|
|Missing           |     0|


**int_pol_implem_6** -- 0-100 scale, asking respondent's how much they agree with the following statement: "The government should make the COVID-19 vaccine mandatory for everybody"


|         |int_pol_implem_6 |
|:--------|:----------------|
|Obs.     |214960           |
|Mean     |57.5             |
|Std.Dev. |37.5             |
|Min.     |0                |
|Max.     |100              |
|Missing  |14720            |

\newpage
## Conjoint Variables

### Meta-data

**person** - distinguishes rounds of the conjoint experiment, and takes the form of `person[i]` where `i` corresponds to the ith comparison between two hypothetical profiles. Each respondent was presented with 8 rounds of the conjoint.


|        | person|
|:-------|------:|
|Obs.    | 214960|
|Unique  |      8|
|Missing |      0|



**candidate** -- distinguishes the two candidate profiles within each round of the conjoint experiment ("A" or "B").


|        | candidate|
|:-------|---------:|
|Obs.    |    214960|
|Unique  |         2|
|Missing |         0|



**ans** -- indicates which candidate ("A" or "B") the respondent chose in the round of the experiment corresponding to this observation.


|        |    ans|
|:-------|------:|
|Obs.    | 214960|
|Unique  |      2|
|Missing |      0|



**select** -- binary indicator of whether each profile (the unit of observation) was chosen (1) or not (0).


|Value   |   Obs.|
|:-------|------:|
|1       | 107480|
|0       | 107480|
|Missing |      0|

\newpage
### Attributes

*Note: reference levels for each attribute are indicated with an asterisk.*



**vulnerability** -- Risk of COVID-19 related death.


|Value                                                |  Obs.|
|:----------------------------------------------------|-----:|
|*Average risk of COVID-19 death                      | 71190|
|Moderate (Twice the average risk of COVID-19 death)  | 71839|
|High (Five times the average risk of COVID-19 death) | 71931|
|Missing                                              |     0|



**transmission** -- Risk of catching and transmitting the COVID-19 virus.


|Value                                                                                   |  Obs.|
|:---------------------------------------------------------------------------------------|-----:|
|*Average risk of catching and transmitting the COVID-19 virus                           | 71593|
|Moderate risk (Twice the average risk of catching and transmitting the COVID-19 virus)  | 71765|
|High risk (Five times the average risk of catching and transmitting the COVID-19 virus) | 71602|
|Missing                                                                                 |     0|


**income** -- Income level.


|Value                    |  Obs.|
|:------------------------|-----:|
|*Lowest 20% income level | 71439|
|Average income level     | 71774|
|Highest 20% income level | 71747|
|Missing                  |     0|


**occupation** -- Occupation status.


|Value                                     |  Obs.|
|:-----------------------------------------|-----:|
|*Not working                              | 27068|
|Non-Key worker: Can work at home          | 26807|
|Non-Key worker: Cannot work at home       | 26919|
|Key worker: Education and childcare       | 26738|
|Key worker: Factory worker                | 26692|
|Key worker: Water and electricity service | 26920|
|Key worker: Police and fire-fighting      | 26819|
|Key worker: Health and social care        | 26997|
|Missing                                   |     0|


\newpage
**age_category** -- Age category.


|Value         |  Obs.|
|:-------------|-----:|
|*25 years old | 53900|
|40 years old  | 53715|
|65 years old  | 53676|
|79 years old  | 53669|
|Missing       |     0|

