import pandas as pd
import seaborn as sns

# read data
df = pd.read_csv('/Users/andrewcamp/Documents/Python/CoreGroupOpt/df_output2.csv')
df_inputs = pd.read_csv('/Users/andrewcamp/Documents/Python/CoreGroupOpt/coreGroupDataMore.csv')

# choose columns
df = df[['Name', 'GrpInd', 'VarName','VarVals']]
df_inputs = df_inputs[['Name','HomeChurch', 'AgeGroup', 'Denomination']].drop_duplicates()

# merge with input data
df2 = df.merge(df_inputs, on = 'Name')

# visual
attr = 'AgeGroup'
df_agg = df2[[attr,'GrpInd','VarVals']].groupby([attr, 'GrpInd']).sum().reset_index()
ax = sns.barplot(x=attr, y="VarVals", data=df_agg, hue = 'GrpInd', palette="Blues_d")
ax.set_xticklabels(ax.get_xticklabels(),rotation=90)
ax.legend_.remove()
ax.set(xlabel='Age Group', ylabel='Count')

# group sizes
df_groups = df2[['GrpInd','VarVals']].groupby(['GrpInd']).sum().reset_index()
print(df_groups)
