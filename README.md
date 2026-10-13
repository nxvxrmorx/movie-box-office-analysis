Key findings
* Sequels earn about 4x as much on the surface. Median worldwide revenue was $240M for sequels versus $62M for originals, but sequels also had larger budgets ($70M vs. $30M median).
* The premium survives controls. After accounting for budget, release year, genre and season, sequels earned about 79% more (95% CI +61% to +99%).
* Budget is the biggest driver. A 1% larger budget is associated with about 0.84% more revenue.
* Sequels are safer bets. 94.5% of sequels earned more than their production budget, versus 74.7% of originals.
* The model is reliable. R² = 0.48; on an 80/20 train/test split, test R² = 0.45, so it is not overfitting.
  
Data:
TMDb 5000 Movie Dataset (4,803 films). 
I kept English-language films released 1990-2016 with a budget and worldwide revenue of at least $1M, leaving 2,515 films
(271 sequels, 2,244 originals).

The dataset has no sequel field, so I built one: films were grouped into series by title (plus a few manual franchise groups),
the first-released film in a series is an original, and every later installment is a sequel. Reboots, remakes and ambiguous
title matches (21 films) were excluded. Revenue is in nominal dollars.

Methods:
1. Cleaned and filtered the data and built the sequel indicator.
2. Compared groups with descriptive statistics, a Welch t-test on log revenue and a Mann-Whitney test.
3. Visualized revenue by film type and budget vs. revenue with ggplot2.
4. Fit three linear regressions on log(revenue), adding controls step by step (budget and year, then genre and release season).
5. Checked the model with multicollinearity (VIF), influence (Cook's distance), heteroskedasticity (Breusch-Pagan, robust HC3 standard errors) and an 80/20 train/test split.
