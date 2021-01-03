import pandas as pd

# weights
wt_denom = 1
wt_age = 1
wt_leader = 1

# read in data and sample
df = pd.read_csv('/Users/andrewcamp/Documents/Python/CoreGroupOpt/coreGroupDataMore.csv')
df = df.sample(160).reset_index()

# one hot encode attribute columns
df_oneHot =(pd.concat(
    [pd.get_dummies(df['AgeGroup'])*wt_denom,
    pd.get_dummies(df['Denomination'])*wt_age ,
    #pd.get_dummies(df['CoreGroupLeader'])*wt_leader
    ], axis = 1))

# add names back into dataframe
df_oneHot['Name'] = df['Name']

# write to folder for input into julia
df_oneHot.to_csv("/Users/andrewcamp/Documents/Python/CoreGroupOpt/df_optInput.csv", index = False)
