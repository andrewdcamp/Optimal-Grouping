using JuMP, CSV, DataFrames, Cbc

print("h")

# read one hot coded data
f = "/Users/andrewcamp/Documents/Python/CoreGroupOpt/df_optInput.csv"
df = CSV.read(f, DataFrame)
nms = df.Name
attrMx = Matrix(select(df, Not(:Name))) # create a matrix from one hot coded attributes

# input parameters
Group_size = 16
N_People = floor(Int,size(df, 1))
N_Groups = floor(Int,N_People/Group_size)
AttrVecSize = floor(Int, size(df, 2) - 1)
idealMx = sum(attrMx, dims=1)/N_Groups # array of ideal sum of attributes in each group

# create Model
m = Model(Cbc.Optimizer)

# create vars
@variable(m, y[1:N_Groups,1:N_People], Bin) # binary group assignment variables
@variable(m, g[1:N_Groups,1:AttrVecSize]) # group statistics
@variable(m, dist[1:N_Groups, 1:AttrVecSize] >= 0) # manhattan distances between each group stat and ideal array
@variable(m, s1 >=0) # group size upper slack variable
@variable(m, s2 >=0) # group size lower slack variable

# group stat defintions
for yRow in 1:size(y, 1)
    for mxCol in 1:size(attrMx, 2)
        @constraint(m, g[yRow, mxCol] == sum(y[yRow,:].*attrMx[:,mxCol]))
    end
end

# distance metric defintions
for i in 1:size(g,1)
    @constraint(m, g[i,:] .- idealMx .<= dist[i,:])
    @constraint(m, idealMx .- g[i,:] .<= dist[i,:])
end

# constraints - group size with slack vars
for i in 1:N_Groups
   @constraint(m, sum(y[i,:]) <= (Group_size + s1))
   @constraint(m, sum(y[i,:]) >= (Group_size - s2))
end

# constraints - each person assigned to only one group
for i in 1:N_People
    @constraint(m, sum(y[:,i]) == 1)
end

# objective
@objective(m, Min, sum(dist[i, j] for i=1:N_Groups, j=1:AttrVecSize) + 1000*s1 + 1000*s2)

# solve
optimize!(m)
#termination_status(m)
#objective_value(m)

# collect output
df_vars = DataFrame()

ind1 = [keys(y)[i][1] for i in keys(y)] # first index
ind2 = [keys(y)[i][2] for i in keys(y)] # second index
vals = [value(v) for v in y]

df_vars.GrpInd = [(ind1...)...]
df_vars.NameInd = [(ind2...)...]
df_vars.VarName = [(y...)...]
df_vars.VarVals = [(vals...)...]

# filter to nonzero variables
df_vars = df_vars[df_vars[:, :VarVals] .== 1, :]

# merge output with input
df.NameInd = 1:N_People
df_output = innerjoin(df_vars, df, on = :NameInd)

#write dataframe
CSV.write("/Users/andrewcamp/Documents/Python/CoreGroupOpt/df_output2.csv", df_output)