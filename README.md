# foia_shiny_app

This R Shiny App was created to predict the success rate of Freedom of Information Act requests based on a dataset of 9,000+ previous FOIA requests and their outcomes.

[View the data on data.world](https://data.world/rdowns26/foia-analysis)

## Methodology
This model uses K Nearest Neighbors with k=5. First, it reads in the previous reqests that have already had the important features extracted ([see feature extraction python notebook](https://data.world/rdowns26/foia-analysis/file/FOIA_Feature_Extraction.ipynb)) and builds a model. Then, it takes the user's text input and performs the same feature extraction in real time. It also translates the user's agency into a boolean variable based on whether that agency has a success rate > 50%. Then the model outputs a predicted likelihood of success, as well as the features the script extracted from the text input.

**Have ideas on how to make this model better? We'd love your help! Submit a pull request or [request to be a contributor on data.world.](https://data.world/rdowns26/foia-analysis/contributors)**
