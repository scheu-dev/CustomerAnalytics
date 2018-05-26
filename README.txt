================================================================ 
GENERAL DESCRIPTION
================================================================
Each of you obtain a random subset of 300 respondents from 
the complete dataset after data filtration.

To subset the complete data:
1) take your studentID from file "StudentList" 
2) select the ids that correspond to your studentID in file "idList"
3) subset choiceData and indivData retaining the selected ids in step 2. 



Note that the complete data has been filtered using the following 
criteria:

  1) Based on RESPONSE TIME: 
	- total response time < 5 minutes 
	  OR 
	- response time to CBC tasks < 1 minute.

  2) Based on ATTENTION: if 0 out of 3 attention 
     questions integrated in the questionnaire have 
     been answered correctly, which would imply these 
     respondents were not reading any of the instructions 
     and questions and have been most likely 
     clicking through.

  3) Based on the number of STRAIGHTLINED SCALES: 
     if 2 or more scales (e.g., Subjective knowledge 
     (SubjKnow), Product involvement inventory (PII), etc.) 
     have been straightlined, i.e., for all items in 
     the scale the same answer was given, for example:
		
	            1   2   3   4   5   6   7

     important	    o   o   o   o   x   o   o    unimportant
     means nothing  o   o   o   o   x   o   o    means a lot
     significant    o   o   o   o   x   o   o    insignifcant	



For all the scales included (SubjKnow and PII) all the items 
have been brought to the same scale. That is, in the example 
above items "important" and "significant" are reversed scale:
lower value indicates more importance or more significance.
In such cases the corresponding items have been reversed so 
that higher value (from 1 to 7) would indicate e.g., more 
importance and more significance. 
Therefore, they DO NOT NEED MODIFICATIONS and can be directly 
used for factor analysis.

Note that both scales are in fact one factor constructs!

Below is the description of the specific datasets and variables



================================================================ 
CHOICEDATA
================================================================ 

ChoiceData has 14400 rows and 16 columns. Please check that you 
have loaded the data correctly and you get these dimensions when
you execute dim(choiceData) command in R.


COLUMNS 1 TO 5 INCLUDE:
----------------------------------------------------------------

id     - index for the respondents. PLEASE NOTE that this is not 
         necessarily sequential 
         (in total 300 ids). 

cs     - index of the choice tasks 
         (12 choice tasks per each id)
	 NOTE: choice task (design) are not the same for all ids

alt    - index of the alternative in the choice tasks
	 (4 alternatives per choice task)
	 NOTE that alt = 4 is the "none" option

choice - a dummy variable, indicating the alternative chosen

none   - a dummy variable, indicating the "NONE" option
         NOTE: none = 1 when alt = 4
----------------------------------------------------------------


COLUMNS 6 TO 16 INCLUDE:
----------------------------------------------------------------

priceL   - Linear coded. is in €, varies from 70 - 150

Rest of the attributes are effect coded. The last level
(highest value) of the attribute is always omitted.

Variable Name	Attribute Level Label
-----------------------------------------
battery1 	8 hours
battery2	10 hours
battery3	12 hours
battery4    	14 hours
		16 hours  (omitted level)

weight1		400 grams
weight2		500 grams
weight3		600 grams
		700 grams (omitted level)

sound1		3.5 stars
sound2		4.0 stars
sound3		4.5 stars
		5.0 stars (omitted level)
-----------------------------------------
----------------------------------------------------------------
================================================================ 



================================================================ 
INDIVDATA
================================================================ 

indivData has 300 rows and 37 columns. Please check that you 
have loaded the data correctly and you get these dimensions when
you execute dim(indivData) command in R. 

NOTE: Each row represents one respondent.


COLUMNS 1 TO 3 CONTAIN:
----------------------------------------------------------------

id  - index of respondents (should be the same as in choiceData)


Own - a dummy variable. = 1 if the respondent owns a 
      portable Bluetooth speaker, otherwise 0


IntentToBuy - a dummy variable. = 1 if the respondent has stated
              that (s)he intend to buy a portable Bluetooth speaker

----------------------------------------------------------------



COLUMNS 4 TO 12 CONTAIN:
----------------------------------------------------------------
8 BrandAwareness_[brand] dummies indicating whether the 
respondents is aware of the specific brand to produce portable 
Bluetooth speakers.

the 9th BrandAwareness_None is a dummy indicating that the 
respondent is not aware of any of the 8 brands.


COLUMNS 13 TO 17 CONTAIN:
----------------------------------------------------------------
5 items of Subjective Knowledge scale. 

Source: Flynn & Goldsmith (1999). A short, and reliable measure of 
subjective knowledge.Journal of Business Research, Vol. 46, 57-66 


ITEMS were evaluated on 7-point Likert scale (1 = strongly disagree,
2 = disagree, 3 = slightly disagree, 4 = neutral, 5 = slightly agree,
6 = agree, 7 = strongly agree):

SubjKnow_r1	I know pretty much about portable Bluetooth speakers.
SubjKnow_r2(*)	I do not feel very knowledgeable about portable Bluetooth speakers.
SubjKnow_r3	Among my circle of friends, I'm one of the "experts" on portable Bluetooth speakers.
SubjKnow_r4(*)	Compared to most other people, I know less about portable Bluetooth speakers.
SubjKnow_r5(*)	When it comes to portable Bluetooth speakers, I really don't know a lot.

(*) indicates that the items have been on reversed scale in the questionnaire
NOTE: all the reversed scale items are already brought to the same scale
----------------------------------------------------------------



COLUMNS 18 TO 22 CONTAIN:
----------------------------------------------------------------
5 items of modified PII (product category involvement) 
scale (Mittal 1995).


Source for modified PII: Mittal (1995). A Comparative Analysis 
of four scales of consumer involvement. Psychology & Marketing, 
Vol. 12, 663-682

Source for original PII: Zaichkowsky (1985). Measuring the 
involvement construct. Journal of Consumer Research, 
Vol. 12, 341-352


ITEMS were evaluated on 7-point semantic differential scale:

			  1 2 3 4 5 6 7
1(*)	are important 	  O O O O O O O	are unimportant
2	mean nothing 	  O O O O O O O	mean a lot
3(*)	matter 		  O O O O O O O	do not matter
4(*)	are significant   O O O O O O O	are insignificant
5	are of no concern O O O O O O O	are of concern

(*) indicates that the items have been on reversed scale in the questionnaire
NOTE: all the reversed scale items are already brought to the same scale
----------------------------------------------------------------



COLUMNS 23 TO 26 CONTAIN:
----------------------------------------------------------------
Relative importance of attributes as STATED by respondents

RelImp_battery
RelImp_weight
RelImp_price
RelImp_sound

NOTE: that it is scales from 0 - 100% and the sum of all the 
relative importances is 100% by construct.
----------------------------------------------------------------



COLUMNS 27 TO 37 CONTAIN:
----------------------------------------------------------------
Socio demographic information often numeric & character coded:


----------------------------------------
Gender	GenderLabel

1	Female
2	Male
3	Prefer not to answer [Exclusive]
----------------------------------------

Age	AgeLabel

1	< 18 years
2	18 - 24 years
3	25 - 29 years
4	30 - 34 years
5	35 - 39 years
6	40 - 44 years
7	45 - 49 years
8	>= 50 years

----------------------------------------

Residence - coded as a character

----------------------------------------

Occupation	OccupationLabel

1		Employed
2		Self-employed
3		Student
4		Unemployed
5		Retired

----------------------------------------

Education	EducationLabel

----------------------------------------

Income	IncomeLabel

1	<= 500 €
2	501 - 1000 €
3	1001 - 1500 €
4	1501 - 2000 €
5	2001 - 2500 €
6	2501 - 3000 €
7	>= 3001 €
8	rather not say

----------------------------------------
----------------------------------------------------------------
================================================================ 