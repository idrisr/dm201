DataMining 201 Notes
==========

Day 1
---------------

datamining.webex.com
password - abc123

Linear Regresssion
----------------------

authors recommend picking two algos and learning them insdie out
* regressions
* k nearest neighbors

Linear Regression needs numeric inputs
You can code categorical variables as numbers to apply regression

Linear models work really well for sparse wide matrix, including text mining,
where the matrix can be millions of input variables wide

OLS will give very large errrors for the data points you are most confident
about, because they will give large OLS, because the mechanics of regression
aren't intended for classification. 

But in a practical sense, it won't make much of a difference. This assumes your
input data are explained by a linear model.

what is a mixture model?
In statistics, a mixture model is a probabilistic model for representing the
presence of sub-populations within an overall population, without requiring that
an observed data-set should identify the sub-population to which an individual
observation belongs.

Don't require assumptions on the data, doesn't require independence for linear
regression. [[not sure about this]]

The matrix X * t(X) has to be singular, ie not degenerate. [[You have to be able
to find its determinant]]

If they're perfectly correlated, then it won't work. For example, inches and
centimeters.

But in the end, all you care about is how well does the model predict unseen
data.


k nearest neighbors
----------------------
Let's for example predict a point's presence in a group by looking at it's k
nearest neighbors.

If k is larger than the number of inputs, then all inputs are used to determine
and point in the test. Therefore, k should at least be lower than the number of
inputs.

We can try multiple k's and then need to decide which model to pick.

When k is lower, the model gets more complex in that the boundary becomes
crazier. IE, you start to overfit.

See page ~17 of book, section 2.3.

For each point in the training set, pretend like we don't know its class and
then say, what would we predict its label. Doing that, we generate a curve where
we systemically go thourgh every point and generate out of sample behaviour.

Scaling
------------
For k-nearest neighbors, you want to scale the data to sd=1 and mean=0. If you
change the scaling and it changes the answer, then your algo is sensitive to
arbitrary scale. Also do this to avoid overflow and underflow. 

scale does this in R column wise

some packages do this automatically.

[[send resume]]
email: sgottipati@ebay.com

See prostate.R file

Forward step-wise regression
----------------------


Backward step-wise regression
----------------------


Lasso Regression
----------------------
There are a few regressions which put a penalty on the coefficient

In addition to minimizing LSE, also try to minimize the coeffcients that get
used in the regression model

The penalty is the coefficient of the parameter. This is like step-wise, where
we put no penalties on the parameters we kept, and a total penalty on the
parameters we did not want to keep.

This tends to give you sparse coefficients -> anywhere you have a really wide
attribute space like genomics and text mining. It helps you get an understanding
of the problem to get a sparse solution.


LARS Regression
----------------------
[[read about this]]
