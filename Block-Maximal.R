# -----------------------------------------------------------------------------------
# ---- Packages ---------------------------------------------------------------------
# -----------------------------------------------------------------------------------

{

# package for: fast data manipulation

require(data.table)

# packages for: fast for-loops and parrallel computing

require(foreach)
require(doParallel)

# package for: solving LPs, IPs, and MIPs

require(lpSolve)

# package for: Graphing

require(ggplot2)

}

# -----------------------------------------------------------------------------------
# ---- Functions --------------------------------------------------------------------
# -----------------------------------------------------------------------------------

{

# prints the data types of each column in a data frame 

types = function(dat)
{
	dat = data.frame(dat)
	
	Column = sapply(1:ncol(dat), function(i) colnames(dat)[i])
	Data.Type = sapply(1:ncol(dat), function(i) class(dat[,i]))
	
	results = data.frame(cbind(Column, Data.Type))	
	results
}

# this is a modified verision of rbindlist()
# it adds a column with the name of each list element

rbindlistn = function(l, names = "ID")
{
	if(identical(names, FALSE)) 
	{
		return(rbindlist(l))
	}  

	if(identical(names, TRUE))
	{
		names = ".Names"
	}

	output = rbindlist(l)
	nm = names(l)

	# Try to guess the names from the call, if possible
	
	if(is.null(nm))
	{

		# Examine the call to get names, if possible
		
		call = match.call()
		listCall = call[["l"]]
		
		if(listCall[[1]] == "list")
		{
			for(i in 2:length(listCall)) 
			{
				if(!is.symbol(listCall[[i]])) break
			}

			nm = character(length(listCall) - 1)
			
			for(i in 2:length(listCall))
			{
				nm[[i - 1]] = as.character(listCall[[i]])
			}
		}
	}

	# Fallback if still NULL
	
	if(is.null(nm))
	{
		warning("The 'names' attribute of your list is NULL")
		nm = paste0("V", 1:length(l))
	}

	if(any(nm == ''))
	{
		warning("Some elements in your list are unnamed")
		nm[nm == ''] = paste0("V", 1:length(l))[nm == '']
	}
	
	output[, (names) := rep(nm, sapply(l, function(x) length(x[[1]]), USE.NAMES=FALSE))]
	return(output)
}

# this function performs integration across elements of a vector

int = function(x, from, to)
{
	# error catching
	
	if(is.vector(x) == FALSE)
	{
		result = noquote("'x' must be a vector")
		
	} else if(FALSE %in% sapply(1:length(x), function(i) is.numeric(x[i])))
	{
		result = noquote("'x' must be a vector of numbers")
		
	} else if(to < from)
	{
		result = noquote("'to' cannot be less than 'from'")
		
	} else if(from < 0)
	{
		result = noquote("'from' cannot be less than zero")
	
	} else if(to > length(x))
	{
		result = noquote("'to' cannot be greater than the length of 'x'")
	
	} else
	{
		# computing the requested integration
		
		if(from == to)
		{
			result = 0
			
		} else
		{
			# we need to get indices to know what whole elements are to be summed
			# we need to get decimals to know what fractional part of elements are to be summed
			
			from.index = ceiling(from)
			to.index = ceiling(to)
			
			from.decimal = ceiling(from) - from
			to.decimal = to - floor(to)
			
			# no fractional elements, so its just a sum of whole elements of x
			
			if(to.decimal == 0 & from.decimal == 0)
			{
				result = sum(x[(from.index + 1):(to.index)])
			
			# just a fraction of one element of x
			
			} else if(to.index == from.index)
			{
				decimal = to - from
				result = decimal * x[from.index]
				
			} else
			{
				# val1 represents the fractional value of the element >= 'from'
				
				if(from == 0) 
				{
					val1 = 0
					
				} else
				{
					val1 = from.decimal * x[from.index]
				}
				
				# val2 represents the fractional value of the element >= 'to'
				
				if(to == length(x))
				{
					val2 = x[to.index]
					
				} else
				{
					val2 = to.decimal * x[to.index]
				}
				
				# val3 represents the whole value(s) of the element(s) between 'from' and 'to' 
				
				if((to.index - 1) < (from.index + 1))
				{
					val3 = 0
					
				} else
				{
					val3 = sum(x[(from.index + 1):(to.index - 1)])
				}
				
				result = val1 + val2 + val3			
			}
			
		}
	}
	
	return(result)	
}

}

# -----------------------------------------------------------------------------------
# ---- Import Data ------------------------------------------------------------------
# -----------------------------------------------------------------------------------

{

# make sure the work directory is where the resource csv files are

getwd()
# setwd("F:/Documents/Working/Production Control/HW 3")

# how many resource csv files are there?

R = 2

# what is the prefix name of each resource csv file?

prefix = "Resource"

# import the data

dat = foreach(i = 1:R) %do%
{
	return(data.table(read.csv(paste0(prefix, i, ".csv"))))
}

names(dat) = sapply(1:R, function(i) paste0(prefix, i))
rm(R, prefix, i)

# lets bind the list of resource tables together into one table

dat = rbindlistn(dat, names = "Resource")

}

# -----------------------------------------------------------------------------------
# ---- Prepare Data -----------------------------------------------------------------
# -----------------------------------------------------------------------------------

{

# lets melt the SKUs into two columns: SKU and Demand
	# melt is a function for converting a set of columns in wide format to long format, and vis versa

dat = melt(dat, id.vars = c("Period", "Production", "Resource"), variable.name = "SKU", value.name = "Demand")

# lets order the rows by the period

dat = dat[order(Period)]

# lets create a mapping for Resource and SKU
	# so we can analyze with ID numbers but convert back to the given labels when the analysis is over

# make sure Resource and SKU are factor types

dat[, Resource := factor(Resource)]
dat[, SKU := factor(SKU)]

# create the mapping

ResourceID = data.table("Resource" = unique(as.character(dat$Resource)), "ID" = unique(as.numeric(dat$Resource)))
SKUID = data.table("SKU" = unique(as.character(dat$SKU)), "ID" = unique(as.numeric(dat$SKU)))

# convert all columns to numeric data types

dat[, Period := as.numeric(Period)]
dat[, Production := as.numeric(Production)]
dat[, Resource := as.numeric(Resource)]
dat[, SKU := as.numeric(SKU)]
dat[, Demand := as.numeric(Demand)]

}

# -----------------------------------------------------------------------------------
# ---- Plot Data ------------------------------------------------------------------
# -----------------------------------------------------------------------------------

{

# plot demand and resource across all sku and resources

newdat = melt(dat, measure.vars = c("Production", "Demand"))
newdat[, SKU := ifelse(SKU == 1, "SKUA", ifelse(SKU == 2, "SKUB", ifelse(SKU == 3, "SKUC", ifelse(SKU == 4, "SKUD", "SKUE"))))]
newdat[, Resource := ifelse(Resource == 1, "Resource1", "Resource2")]

p1 = ggplot(data = newdat[Period > 0], aes(x = Period, y = value, color = variable, group = variable)) +
		geom_point(size = 3) +
		geom_line(size = 1) + 
		facet_wrap(~SKU + Resource) +
		ggtitle("SKU & Resource Requirements") + 
		labs(x = "Period", y = "Units", color = "Capacity") + 
		theme_bw(base_size = 20) +
		theme(legend.position = "top", legend.key.size = unit(.25, "in"), plot.title = element_text(hjust = 0.5)) +
		guides(fill = guide_legend(override.aes = list(size = 10, linetype = 0)))

p1

# plot total demand and resource across all resources

newdat = data.table(dat[, .(Demand = sum(Demand), Production = mean(Production)), by = .(Period, Resource)])
newdat = melt(newdat, measure.vars = c("Production", "Demand"))
newdat[, Resource := ifelse(Resource == 1, "Resource1", "Resource2")]

p2 = ggplot(data = newdat[Period > 0], aes(x = Period, y = value, color = variable, group = variable)) +
		geom_point(size = 3) +
		geom_line(size = 1) + 
		facet_wrap(~Resource) +
		ggtitle("Resource Requirements") + 
		labs(x = "Period", y = "Units", color = "Capacity") + 
		theme_bw(base_size = 25) +
		theme(legend.position = "top", legend.key.size = unit(.25, "in"), plot.title = element_text(hjust = 0.5)) +
		guides(fill = guide_legend(override.aes = list(size = 10, linetype = 0)))

p2

# plot net flow of demand and resource across all resources

newdat = data.table(dat[, .(Demand = sum(Demand), Production = mean(Production)), by = .(Period, Resource)])
newdat[, Net := Production - Demand]
newdat[, Resource := ifelse(Resource == 1, "Resource1", "Resource2")]

p3 = ggplot(data = newdat[Period > 0], aes(x = Period, y = Net)) +
		geom_point(size = 3) +
		geom_line(size = 1) + 
		facet_wrap(~Resource) +
		geom_hline(yintercept = 0, color = "red") +
		ggtitle("Net Resource Requirements") + 
		labs(x = "Period", y = "Units", color = "Capacity") + 
		theme_bw(base_size = 25) +
		theme(legend.position = "top", legend.key.size = unit(.25, "in"), plot.title = element_text(hjust = 0.5)) +
		guides(fill = guide_legend(override.aes = list(size = 10, linetype = 0)))

p3

}

# -----------------------------------------------------------------------------------
# ---- Block Maximal Heuristic ------------------------------------------------------
# -----------------------------------------------------------------------------------

{

# Block Maximal is a technique for determining the time at which to start and end the production of products across multiple production cycles
# This is an approach to build a master schedule for a production system that has products with lengthy changeover times
# If the production system doesn't have lengthy changeover times then this approach may not be the most cost effective, because it allows inventory to build up, assuming that it is more cost effective to minimize changeovers

# This implementation will be able to build a master schedule for a production system with multiple products and resources
# In this specific implementation we have 5 products, 2 resources, and the schedule will be 26 periods long

# max.usage is a parameter that answers: what is the max proportion of a resource is one SKU is allowed to use?
# where 0.5 ~ 50%

max.usage = 0.34

# p.next is the percent of future demand to use when replacing zero demand values with the next nonzero future demand
# where 0.3 ~ 30%

p.next = 0.34

# p is the percent to increases the average demand
# p is used in the parameter boost below 

p = 0.66

# boost is a parameter that increases the average demand by a percentage, where 0.5 ~ 50%
# boost is a list of vectors where:
	# each vector entry corresponds to how much to boost the demand for each period
	# each list entry corresponds to a resource
# make sure boost is the same length as the total number of periods, including period 0

boost = list("Resource1" = c(0, rep(p, 26)), 			
				"Resource2" = c(0, rep(p, 26)))

name = unlist(lapply(1:length(boost), function(i) rep(i, length(boost[[i]]))))

# heres a table showing what periods each of your boosts correspond to

boost.table = data.table(PERIOD = unique(dat$Period), Resource = name, BOOST = unlist(boost))
boost.table

# num is a parameter that conrols which SKUs are boosted
# 2 ~ the two SKUs with the lowest runout time in each cycle
# 5 ~ in this case all SKUs becuase there's only 5 SKUs in this implementation

num = 1

complete = FALSE

while(complete == FALSE)
{

# -----------------------------------------------------------------------------------
# ---- Computing Runout Times -------------------------------------------------------
# -----------------------------------------------------------------------------------

{

# DT is a copy of dat that will represent period 0 with respect to each cycle
# DT will eventually lose rows as the master schedule gets built

if("master.schedule" %in% ls())
{
	DT = DT
	
} else
{
	DT = data.table(dat)
}

# lets specify how many cores we are willing to use to compute runout times for all SKUs and resources
# this is unnecessary for just 5 SKUs & 2 resources, but if this were to be scaled to thousands of SKUs and hundreds of resources then parrallel computing would be very useful
	# given that this small problem doesn't require multiple cores we will leave the parrallel computing syntax commented out
	# therefore if you wish to use parrallel computing then uncomment the parrallel computing syntax and comment out the sequential computing syntax
	
detectCores()

# my laptop has 8 so i would use just 6, personally

cores = 6

# number of resources

R = max(DT$Resource)

# number of SKUs

S = max(DT$SKU)

# intialize runout time for each SKU and resource

runout = matrix(rep(0, S * R), nrow = S, ncol = R)

# initialize the period we are evalutating the demand of

period = matrix(rep(0, S * R), nrow = S, ncol = R)

# initialize the inventory for each SKU and resource

inv = matrix(rep(0, S * R), nrow = S, ncol = R)

# initialize the ratio which controls the growth of runout for each SKU and Resource

ratio = matrix(rep(0, S * R), nrow = S, ncol = R)

# compute runout times

# registerDoParallel(cores = cores) 

# results = foreach(j = 1:R) %dopar%
results = foreach(j = 1:R) %do%
{
	require(foreach)
	require(data.table)
	
	# foreach(i = 1:S) %dopar%
	foreach(i = 1:S) %do%
	{

		require(foreach)
		require(data.table)
		
		# extract the initial inventory available of Resource j for SKU i
		
		inv[i,j] = DT[SKU == i & Resource == j & Period == period[i,j], Demand]
		
		# compute how many periods of demand the intitial inventory can satisfy (ie. compute the runout time)
		
		ratio[i,j] = ifelse(period[i,j] + 1 == max(DT$Period), -1, inv[i,j] / DT[SKU == i & Resource == j & Period == period[i,j] + 1, Demand])
		
		while(ratio[i,j] > 1)
		{
			runout[i,j] = runout[i,j] + 1
			period[i,j] = period[i,j] + 1
			inv[i,j] = inv[i,j] - DT[SKU == i & Resource == j & Period == period[i,j], Demand]
			ratio[i,j] = ifelse(period[i,j] + 1 == max(DT$Period), -1, inv[i,j] / DT[SKU == i & Resource == j & Period == period[i,j] + 1, Demand])
		}
		
		runout[i,j] = runout[i,j] + min(1, (inv[i,j] / DT[SKU == i & Resource == j & Period == period[i,j] + 1, Demand]))
	}
	
	return(runout)
}

# registerDoSEQ()

# extract the runout times of each SKU per resource

# when using %dopar%
# runout = data.table(Reduce("+", results))

# when using %do%
runout = data.table(results[[R]])

# name the columns appropriately

setnames(runout, sapply(1:R, function(i) paste0("Resource", i)))

rm(R, S, period, inv, results, ratio)

# lets extract the minimum runout times for each SKU, across all resources

minrunout = apply(as.matrix(runout), 1, min)
minresource = apply(as.matrix(runout), 1, which.min)
SKU = 1:length(minrunout)

# lets rebuild runout to have only the minimum runout times
# lets also redefine SKU IDs based on runout times

runout = data.table(ID = SKU, Resource = minresource, periods = minrunout)
runout = runout[order(minrunout)]
runout[, ID.new := 1:nrow(runout)]

rm(minrunout, minresource, SKU)

# lets join ID.new onto SKUID to create new IDs based on runout times

IDs = data.table(ID = runout$ID, ID.new = runout$ID.new)

setkey(SKUID, ID)
setkey(IDs, ID)

SKUID = IDs[SKUID]

# lets join ID.new onto dat and DT to create new IDs based on runout times

setkey(SKUID, ID)
setkey(dat, SKU)
setkey(DT, SKU)

dat = IDs[dat]
DT = IDs[DT]

rm(IDs)

# lets remove the old IDs

SKUID[, ID := ID.new]
SKUID[, ID.new := NULL]
runout[, ID := ID.new]
runout[, ID.new := NULL]
dat[, ID := ID.new]
dat[, ID.new := NULL]
DT[, ID := ID.new]
DT[, ID.new := NULL]

# lets rename the columns of dat and DT because column SKU got renamed to ID after the join
# lets rename the columns of runout as well

setnames(dat, c("SKU", "Period", "Production", "Resource", "Demand"))
setnames(DT, c("SKU", "Period", "Production", "Resource", "Demand"))
setnames(runout, c("SKU", "Resource", "periods"))

}

# -----------------------------------------------------------------------------------
# ---- Initializing Production & Demand Rates ---------------------------------------
# -----------------------------------------------------------------------------------

{

# initial estimate of cycle.length

cycle.length = min(max(DT$Period), max(runout$periods) + (max(runout$periods) / nrow(runout)))

# lets replace all periods of zero demand with the nearest nonzero future demand
# this is so the avergae demand is not brought down due to random periods of no demand

DT.copy = data.table(DT)

if(p.next > 0)
{
	while(0 %in% DT.copy$Demand)
	{
		DT.copy[, lagDemand := shift(Demand, type = "lead"), by = .(SKU, Resource)]
		DT.copy[Demand == 0, Demand := p.next * lagDemand]
	}

	DT.copy[, lagDemand := NULL]
	DT.copy[Period == 0, Demand := DT[Period == 0, Demand]]
}

# lets increase the demands of each period based on DT.copy

DT.copy[, PERIOD := ifelse(Period == 0, 0, Period + (max(dat$Period) - max(DT$Period)))]

setkey(DT.copy, PERIOD, Resource)
setkey(boost.table, PERIOD, Resource)

DT.copy = boost.table[DT.copy]
DT.copy[SKU %in% 1:num, Demand := Demand * (1 + BOOST)]

DT.copy[, PERIOD := NULL]
DT.copy[, BOOST := NULL]

# compute the initial estimate of avg production and avg demand rates

# registerDoParallel(cores = cores) 

# rates = foreach(i = 1:nrow(runout)) %dopar%
rates = foreach(i = 1:nrow(runout)) %do%
{
	require(foreach)
	require(data.table)
	
	# compute average production
	
	production = DT.copy[Period > 0 & Resource == runout$Resource[i] & SKU == runout$SKU[i], Production]
	avgP = int(x = production, from = 0, to = cycle.length) / cycle.length
	
	# compute average demand
	
	demand = DT.copy[Period > 0 & Resource == runout$Resource[i] & SKU == runout$SKU[i], Demand]
	avgD = int(x = demand, from = 0, to = cycle.length) / cycle.length
	
	if(avgD == 0)
	{
		avgD = 1e-14
	}
	
	# build a table of the results
	
	results = data.table(SKU = runout$SKU[i], Resource = runout$Resource[i], avgP = avgP, avgD = avgD)

	return(results)
}

# registerDoSEQ()

rates = rbindlist(rates)

# the average demand cannot be higher than the max usage of average production
	# this is so that the LP will provide a solution
	# backorders will be tracked and pushed forward one period if neccessary

rates[, avgD := ifelse(avgD > (max.usage * avgP), (max.usage * avgP), avgD)]

rm(cycle.length)

}

# -----------------------------------------------------------------------------------
# ---- Initializing Time Stamps -----------------------------------------------------
# -----------------------------------------------------------------------------------

{

# inventory contraints: start production before inventory runs out

inv.A = cbind(diag(rep(1, nrow(runout))), rep(0, nrow(runout)))
inv.b = runout$periods
inv.sign = rep("<=", nrow(runout))

# demand contraints: ensure enough product is produced to cover the demand across the cycle length

ti = sapply(1:(nrow(runout) - 1), function(i) -rates$avgP[i] / rates$avgD[i])
tiplus1 = sapply(1:(nrow(runout) - 1), function(i) rates$avgP[i] / rates$avgD[i])

ti.A = cbind(diag(ti), rep(0, nrow(runout) - 1), rep(-1, nrow(runout) - 1))
tiplus1.A = cbind(rep(0, nrow(runout) - 1), diag(tiplus1), rep(0, nrow(runout) - 1))

dem.A = ti.A + tiplus1.A
dem.b = rep(0, nrow(runout) - 1)
dem.sign = rep(">=", nrow(runout) - 1)

rm(ti, tiplus1, ti.A, tiplus1.A)

# nonnegativity constraints: all variables must be nonnegative

nonneg.A = diag(rep(1, nrow(runout) + 1))
nonneg.b = rep(0, nrow(runout) + 1)
nonneg.sign = rep(">=", nrow(runout) + 1)

# vector obj: objective function coefficients

obj = c(rep(0, nrow(runout)), 1)

# matrix A: constraint coefficients

A = rbind(inv.A, dem.A, nonneg.A)
rm(inv.A, dem.A, nonneg.A)

# vector b: righthand side constants

b = c(inv.b, dem.b, nonneg.b)
rm(inv.b, dem.b, nonneg.b)

# vector signs: contraint equalities

signs = c(inv.sign, dem.sign, nonneg.sign)
rm(inv.sign, dem.sign, nonneg.sign)

# lets solve for the initial estimates of start times per SKU and cycle length

times = lp(direction = "max", objective.in = obj, const.mat = A, const.dir = signs, const.rhs = b)
backup.solution = times

rm(A, b, obj, signs)

}

# -----------------------------------------------------------------------------------
# ---- Converging on Cycle Length ---------------------------------------------------
# -----------------------------------------------------------------------------------

{

# ---- initialize parameters ----

# solution.history keeps track of the vector of variable values, where the last variable 'T' is the objective value
	# dont change this
	
solution.history = data.table(matrix(rep(NA, length(times$solution)), nrow = 1))
setnames(solution.history, c(sapply(1:(length(times$solution) - 1), function(i) paste0("t", i)), "T"))

# done is the while loop control parameter
	# don't change this
	
done = FALSE

# threshold is the condition parameter for converging on a cycle length, 'T'
# for example: threshold = 0.01 means that the while loop ends when there is at most, a 1% difference between successive 'T' values
	# feel free to change this
	
threshold = 0.01

# it.limit is the control parameter for dealing with oscillation about convergence
	# sometimes the average demand and average production of solution n makes solution (n + 1) change significantly, and vis versa, creating large oscillation behavior
	# sometimes the threshold value is so small that it can't satisfy the convergence condition
# so after it.limit iterations, we will compute the median to filter out low value cycle lengths and then pick the most recent larger cycle length
	# feel free to change this
	
it.limit = 10

# ---- solve cycle length ----

while(done == FALSE)
{
	if("it" %in% ls())
	{
		it = it + 1
		
	} else
	{
		it = 1
	}
	
	# -------------------------------------------------------------------------------
	# ---- Checking for Convergence -------------------------------------------------
	# -------------------------------------------------------------------------------
	
	# append the current solution vector to the solution history
	
	solution.current = data.table(matrix(times$solution, nrow = 1))
	setnames(solution.current, c(sapply(1:(length(times$solution) - 1), function(i) paste0("t", i)), "T"))
	solution.history = rbind(solution.history, solution.current)
	
	# is this the first iteration?
	
	if(is.na(solution.history$t1[1]))
	{
		solution.history = solution.history[-1,]
		pdiff = 1
		
	} else
	{
		# compute the percent difference between successive cycle lengths
		
		pdiff = abs((solution.history$T[nrow(solution.history)] - solution.history$T[nrow(solution.history) - 1]) / solution.history$T[nrow(solution.history) - 1])	
	}

	# -------------------------------------------------------------------------------
	# ---- Computing Production & Demand Rates --------------------------------------
	# -------------------------------------------------------------------------------
	
	if(pdiff > threshold & it < it.limit)
	{
		# initial estimate of cycle.length

		cycle.length = min(max(DT.copy$Period), times$solution[length(times$solution)])
		times = times$solution[-length(times$solution)]

		# initial estimate of avg production rates

		# registerDoParallel(cores = cores) 

		# rates = foreach(i = 1:nrow(runout)) %dopar%
		rates = foreach(i = 1:nrow(runout)) %do%
		{
			require(foreach)
			require(data.table)
			
			# compute average production
			
			if(i < nrow(runout))
			{
				production = DT.copy[Period > 0 & Resource == runout$Resource[i] & SKU == runout$SKU[i], Production]
				avgP = int(x = production, from = times[i], to = times[i + 1]) / (times[i + 1] - times[i])
				
			} else
			{
				production = DT.copy[Period > 0 & Resource == runout$Resource[i] & SKU == runout$SKU[i], Production]
				avgP = int(x = production, from = times[i], to = min(max(DT.copy$Period), times[i] + cycle.length)) / cycle.length
			}
			
			# compute average demand
			
			demand = DT.copy[Period > 0 & Resource == runout$Resource[i] & SKU == runout$SKU[i], Demand]
			avgD = int(x = demand, from = times[i], to = min(max(DT.copy$Period), times[i] + cycle.length)) / cycle.length
			
			if(avgD == 0)
			{
				avgD = 1e-14
			}
			
			# build a table of the results
			
			results = data.table(SKU = runout$SKU[i], Resource = runout$Resource[i], avgP = avgP, avgD = avgD)

			return(results)
		}

		# registerDoSEQ()

		rates = rbindlist(rates)

		# the average demand cannot be higher than the max usage of average production
		# this is so that the LP will provide a solution
		# backorders will be tracked and pushed forward one period if neccessary

		rates[, avgD := ifelse(avgD > (max.usage * avgP), (max.usage * avgP), avgD)]
	
		rm(times, cycle.length)

		# -------------------------------------------------------------------------------
		# ---- Computing Time Stamps ----------------------------------------------------
		# -------------------------------------------------------------------------------

		# inventory contraints: start production before inventory runs out

		inv.A = cbind(diag(rep(1, nrow(runout))), rep(0, nrow(runout)))
		inv.b = runout$periods
		inv.sign = rep("<=", nrow(runout))

		# demand contraints: ensure enough product is produced to cover the demand across the cycle length

		ti = sapply(1:(nrow(runout) - 1), function(i) -rates$avgP[i] / rates$avgD[i])
		tiplus1 = sapply(1:(nrow(runout) - 1), function(i) rates$avgP[i] / rates$avgD[i])

		ti.A = cbind(diag(ti), rep(0, nrow(runout) - 1), rep(-1, nrow(runout) - 1))
		tiplus1.A = cbind(rep(0, nrow(runout) - 1), diag(tiplus1), rep(0, nrow(runout) - 1))

		dem.A = ti.A + tiplus1.A
		dem.b = rep(0, nrow(runout) - 1)
		dem.sign = rep(">=", nrow(runout) - 1)

		rm(ti, tiplus1, ti.A, tiplus1.A)

		# nonnegativity constraints: all variables must be nonnegative

		nonneg.A = diag(rep(1, nrow(runout) + 1))
		nonneg.b = rep(0, nrow(runout) + 1)
		nonneg.sign = rep(">=", nrow(runout) + 1)

		# vector obj: objective function coefficients

		obj = c(rep(0, nrow(runout)), 1)

		# matrix A: constraint coefficients

		A = rbind(inv.A, dem.A, nonneg.A)
		rm(inv.A, dem.A, nonneg.A)

		# vector b: righthand side constants

		b = c(inv.b, dem.b, nonneg.b)
		rm(inv.b, dem.b, nonneg.b)

		# vector signs: contraint equalities

		signs = c(inv.sign, dem.sign, nonneg.sign)
		rm(inv.sign, dem.sign, nonneg.sign)

		# lets solve for the start times per SKU and cycle length

		times = lp(direction = "max", objective.in = obj, const.mat = A, const.dir = signs, const.rhs = b)
		
		rm(A, b, obj, signs)
		
		if(times$solution[length(times$solution)] == 0)
		{
			solution.history = data.table(matrix(backup.solution$solution, nrow = 1))
			setnames(solution.history, c(sapply(1:(length(backup.solution$solution) - 1), function(i) paste0("t", i)), "T"))
			done = TRUE
		}
		
	} else
	{
		if(pdiff > threshold & it >= it.limit)
		{
			solution.history = solution.history[T > median(T)]
		}
	
		done = TRUE
	}
}

rm(solution.current, it, it.limit, i, j, results, pdiff, avgD, avgP, demand, done, production, threshold, times)

}

# -----------------------------------------------------------------------------------
# ---- Updating Master Schedule -----------------------------------------------------
# -----------------------------------------------------------------------------------

{

# master.schedule will include the time when each SKU should start and end production, and the quantity of resources to be used for each SKU across the entire 26 period plan

if("master.schedule" %in% ls())
{
	# extract the best solution from solution.history (ie. the last one)
	
	chosen.solution = as.numeric(solution.history[nrow(solution.history)])
	
	# update the cycle length in the case it is longer than the remaining periods

	chosen.solution[length(chosen.solution)] = min(max(DT$Period), chosen.solution[length(chosen.solution)])
	
	# lets update chosen.solution given that the end.time of the last cycle may have ended in the middle of a period (ie. the previous cycle length wasn't an integer value)
	# this means that the first period of this cycle wouldn't actually be 1 unit of time
	
	times = chosen.solution
	distort = ceiling(max(master.schedule$end.time, na.rm = TRUE)) - max(master.schedule$end.time, na.rm = TRUE)
	
	if(distort > 0)
	{
		# registerDoParallel(cores = cores)
		
		# duration = foreach(i = 2:(length(times) - 1)) %dopar%
		duration = foreach(i = 2:(length(times) - 1)) %do%
		{
			require(foreach)
			
			# did SKU i spend any time in period 1?
			
			if(times[i-1] < 1)
			{
				# distort the amount of time SKU i spent in period 1
				
				D = distort * ifelse(times[i] > 1, 1 - times[i-1], times[i] - times[i-1])
				
				# include the amount of time SKU i didn't spend in period 1
				
				D = D + ifelse(times[i] > 1, times[i] - 1, 0)
				
			} else
			{
				# don't distort anything becuase SKU i didn't spend any time in period 1
				
				D = times[i] - times[i-1]
			}
			
			result = data.table(D = D)
			
			return(result)
		}
		
		# registerDoSEQ()
		
		duration = rbindlist(duration)
		
		# distort the cycle.length
		
		cycle.length = ifelse(times[length(times)] > 1, times[length(times)] - 1 + distort, times[length(times)] * distort)
		
		# rebuild the times vector to include all distorted times
		
		times = c(0, sapply(1:nrow(duration), function(j) sum(duration$D[1:j])), cycle.length)
		
		rm(cycle.length, D, duration)
	}
		
	# extract the start times for every SKU that will be produced during this cycle
	
	start.time = sapply(times[-length(times)], function(i) ifelse(i <= times[length(times)], i, NA))
	start.time = start.time + max(master.schedule$end.time, na.rm = TRUE)
	
	# extract the end times for every SKU that will be produced during this cycle
	
	end.time = sapply(times[-1], function(i) ifelse(i > times[length(times)], times[length(times)], i))
	end.time = sapply(1:length(start.time), function(i) ifelse(is.na(start.time[i]), NA, end.time[i]))
	end.time = end.time + max(master.schedule$end.time, na.rm = TRUE)
	
	# create the time schedule for this cycle
	
	update.schedule = data.table(Cycle = rep(max(master.schedule$Cycle) + 1, nrow(SKUID)), 
								SKU = SKUID[order(ID),SKU],
								SKUID = SKUID[order(ID),ID],
								start.time = start.time,
								end.time = end.time,
								Inventory = 0)
	
	# lets compute the quantity of resources used for this cycle (ie. quanity of products)
	# given that DT always contains the updated resource levels, we will need to use the current.solution values which map to DT
	# therefore we need start.times and end.times in terms of DT
	
	rm(start.time, end.time)
	
	# extract the start times for every SKU that will be produced during this cycle
	
	start.time = sapply(chosen.solution[-length(chosen.solution)], function(i) ifelse(i <= chosen.solution[length(chosen.solution)], i, NA))

	# extract the end times for every SKU that will be produced during this cycle
	
	end.time = sapply(chosen.solution[-1], function(i) ifelse(i > chosen.solution[length(chosen.solution)], chosen.solution[length(chosen.solution)], i))
	end.time = sapply(1:length(start.time), function(i) ifelse(is.na(start.time[i]), NA, end.time[i]))	

	# create the time schedule for DT
	
	DT.schedule = data.table(Cycle = rep(max(master.schedule$Cycle) + 1, nrow(SKUID)), 
								SKU = SKUID[order(ID),SKU],
								SKUID = SKUID[order(ID),ID],
								start.time = start.time,
								end.time = end.time)
	
	# registerDoParallel(cores = cores) 
	
	# Qty = foreach(i = 1:max(ResourceID$ID)) %dopar%
	Qty = foreach(i = 1:max(ResourceID$ID)) %do%
	{
		require(foreach)
		require(data.table)
		
		# lets add a column of resource i to DT.schedule so we can compute the qty of resources used
		
		results = data.table(DT.schedule[, Resource := rep(i, nrow(DT.schedule))])
		
		# lets extract the production capacities for resource i

		production = data.table(DT[Period > 0 & Resource == i, .(SKU, Production)])

		# now lets integrate along those production capacities from the start.time and end.time of each SKU
		
		# Quantity = foreach(j = 1:nrow(results)) %dopar%
		Quantity = foreach(j = 1:nrow(results)) %do%
		{
			require(foreach)
			require(data.table)
			
			val = data.table(val = int(x = production[SKU == results$SKUID[j], Production], from = ifelse(is.na(results$start.time[j]), 0, results$start.time[j]), to = ifelse(is.na(results$end.time[j]), 0, results$end.time[j])))
			return(val)
		}
		
		Quantity = rbindlist(Quantity)
		
		results[, Quantity := floor(Quantity$val)]
		return(results)
	}

	# registerDoSEQ()
	
	# lets replace DT.schedule with Qty becuase it has all the same information as DT.schedule but with a resource usage column
	
	DT.schedule = rbindlist(Qty)
	
	# lets remove SKUID and ResourceID columns beucase the named SKU and Resource versions are more appropriate for a master schedule
	
	DT.schedule[, SKUID := NULL]
	
	setkey(DT.schedule, Resource)
	setkey(ResourceID, ID)
	
	DT.schedule = ResourceID[DT.schedule]
	DT.schedule[, ID := NULL]
	
	# lets combine the start.time, end.time, and Inventory columns of update.schedule onto DT.schedule
	
	DT.schedule[, start.time := NULL]
	DT.schedule[, end.time := NULL]
	update.schedule[, Cycle := NULL]
	update.schedule[, SKUID := NULL]
	
	setkey(DT.schedule, SKU)
	setkey(update.schedule, SKU)
	
	DT.schedule = update.schedule[DT.schedule]
	DT.schedule = DT.schedule[order(Resource, start.time)]
	
	setcolorder(DT.schedule, colnames(master.schedule))

	# append DT.schedule to master.schedule
	
	master.schedule = rbind(master.schedule, DT.schedule)

	rm(distort, val, DT.schedule, times, update.schedule, start.time, end.time, production, Quantity, i, j, results, Qty)
	
} else
{
	# extract the best solution from solution.history (ie. the last one)
	
	chosen.solution = as.numeric(solution.history[nrow(solution.history)])

	# update the cycle length in the case it is longer than the remaining periods

	chosen.solution[length(chosen.solution)] = min(max(dat$Period), chosen.solution[length(chosen.solution)])
	
	# extract the start times for every SKU that will be produced during this cycle
	
	start.time = sapply(chosen.solution[-length(chosen.solution)], function(i) ifelse(i <= chosen.solution[length(chosen.solution)], i, NA))

	# extract the end times for every SKU that will be produced during this cycle
	
	end.time = sapply(chosen.solution[-1], function(i) ifelse(i > chosen.solution[length(chosen.solution)], chosen.solution[length(chosen.solution)], i))
	end.time = sapply(1:length(start.time), function(i) ifelse(is.na(start.time[i]), NA, end.time[i]))

	# create the time schedule for this cycle
	
	master.schedule = data.table(Cycle = rep(1, nrow(SKUID)), 
								SKU = SKUID[order(ID),SKU],
								SKUID = SKUID[order(ID),ID],
								start.time = start.time,
								end.time = end.time,
								Inventory = 0)
	
	# compute the quantity of resources used for this cycle (ie. quanity of products)
	
	# registerDoParallel(cores = cores) 

	# Qty = foreach(i = 1:max(ResourceID$ID)) %dopar%
	Qty = foreach(i = 1:max(ResourceID$ID)) %do%
	{
		require(foreach)
		require(data.table)
		
		# lets add a column of resource i to master.schedule so we can compute the qty of resources used
		
		results = data.table(master.schedule[, Resource := rep(i, nrow(master.schedule))])
		
		# lets extract the production capacities for resource i

		production = data.table(dat[Period > 0 & Resource == i, .(SKU, Production)])

		# now lets integrate along those production capacities from the start.time and end.time of each SKU
		
		# Quantity = foreach(j = 1:nrow(results)) %dopar%
		Quantity = foreach(j = 1:nrow(results)) %do%
		{
			require(foreach)
			require(data.table)
			
			val = data.table(val = int(x = production[SKU == results$SKUID[j], Production], from = ifelse(is.na(results$start.time[j]), 0, results$start.time[j]), to = ifelse(is.na(results$end.time[j]), 0, results$end.time[j])))
			return(val)
		}
		
		Quantity = rbindlist(Quantity)
		
		results[, Quantity := floor(Quantity$val)]
		return(results)
	}

	# registerDoSEQ()
	
	# lets replace master.schedule with Qty becuase it has all the same information as master.schedule but with a resource usage column
	
	master.schedule = rbindlist(Qty)
	
	# lets remove SKUID and ResourceID columns beucase the named SKU and Resource versions are more appropriate for a master schedule
	
	master.schedule[, SKUID := NULL]
	
	setkey(master.schedule, Resource)
	setkey(ResourceID, ID)
	
	master.schedule = ResourceID[master.schedule]
	master.schedule[, ID := NULL]
	
	rm(start.time, end.time, production, Quantity, i, j, results, Qty, val)
}

rm(solution.history)

}

# -----------------------------------------------------------------------------------
# ---- Updating Inventory -----------------------------------------------------------
# -----------------------------------------------------------------------------------

{

# extract the production information from the most recent cycle

cycle.production = data.table(master.schedule[Cycle == max(Cycle)])
cycle.production[, Inventory := NULL]

# convert the SKU column in cycle.production to the SKU ID values

setkey(cycle.production, SKU)
setkey(SKUID, SKU)

cycle.production = SKUID[cycle.production]
cycle.production[, SKU := ID]
cycle.production[, ID := NULL]

# convert the Resource column in cycle.production to the Resource ID values

setkey(cycle.production, Resource)
setkey(ResourceID, Resource)

cycle.production = ResourceID[cycle.production]
cycle.production[, Resource := ID]
cycle.production[, ID := NULL]

# lets order cycle.production by SKU and Resource

cycle.production = cycle.production[order(Resource, SKU)]

# lets compute the new inventory levels

cycle.start = chosen.solution[1]
cycle.end = chosen.solution[length(chosen.solution)]

# registerDoParallel(cores = cores) 

# Inv = foreach(i = 1:nrow(cycle.production)) %dopar%
Inv = foreach(i = 1:nrow(cycle.production)) %do%
{
	require(foreach)
	require(data.table)
	
	results = data.table(Resource = cycle.production$Resource[i],
							SKU = cycle.production$SKU[i],
							Inv = cycle.production$Quantity[i] + DT[Period == 0 & SKU == cycle.production$SKU[i] & Resource == cycle.production$Resource[i], Demand] - floor(int(x = DT[Period > 0 & SKU == cycle.production$SKU[i] & Resource == cycle.production$Resource[i], Demand], from = cycle.start, to = cycle.end)))
	return(results)
}

# registerDoSEQ()

Inv = rbindlist(Inv)

cycle.production[, Inv := Inv$Inv]

rm(Inv, i, results)

# lets update DT by removing the periods of demand that have been satisfied by the most recent cycle
# lets also update period index values for the computation of the next cycle

if(floor(cycle.end) > 0)
{
	DT = DT[!(Period %in% 1:floor(cycle.end))]
	DT[Period > 0, Period := Period - floor(cycle.end)]
}

# reduce the demand and production values in period 1 
	# if this cycle length had decimal values, then these demand and production values will be reduced

reduction = cycle.end - floor(cycle.end)

DT[Period == 1, Production := floor(Production * (1 - reduction))]
DT[Period == 1, Demand := ceiling(Demand * (1 - reduction))]

# lets update the Demand column for period 0 in DT with the computed inventory values from the Inv column in cycle.production

# lets order DT by period so we can easily extract period 0 which corresponds to inventory

DT = DT[order(Period)]

# inv.val contains the inventory values for period 0

# registerDoParallel(cores = cores) 

# inv.val = foreach(i = 1:nrow(cycle.production)) %dopar%
inv.val = foreach(i = 1:nrow(cycle.production)) %do%
{
	require(foreach)
	require(data.table)
	
	results = data.table(Inv = cycle.production[Resource == DT$Resource[i] & SKU == DT$SKU[i], Inv])
	
	return(results)
}

# registerDoSEQ()

inv.val = rbindlist(inv.val)

# dem.val contains the demand values for periods after period 0

dem.val = DT[Period > 0, Demand]
DT[, Demand := c(inv.val$Inv, dem.val)]

# lets check to see if any of the values in period 0 of column Demand are negative
# this would indicate that there were unsatisfied orders, so lets remove them from inventory

if(nrow(DT[Period == 0 & Demand < 0]) > 0)
{
	# extract the lostorders
	
	lostorders = data.table(DT[Period == 0 & Demand < 0])
	
	# convert the negative inventory to zero
	
	DT[Period == 0 & Demand < 0, Demand := 0]
	
	rm(lostorders)
}

# lets add the ending inventory levels to the master schedule

master.schedule[Cycle == max(Cycle), Inventory := cycle.production$Inv]

rm(chosen.solution, i, inv.val, dem.val, reduction, cycle.end, cycle.start, cycle.production, cores)

}

# -----------------------------------------------------------------------------------
# ---- End Condition ----------------------------------------------------------------
# -----------------------------------------------------------------------------------

if("diagnostics" %in% ls())
{
	diagnostics = rbind(diagnostics, data.table(Cycle = max(master.schedule$Cycle), end.time = max(master.schedule$end.time, na.rm = TRUE), Period = max(DT$Period)))
	
} else
{
	diagnostics = data.table(Cycle = max(master.schedule$Cycle), end.time = max(master.schedule$end.time, na.rm = TRUE), Period = max(DT$Period))
}

if(max(DT$Period) == 0)
{
	complete = TRUE
}

}

}

# -----------------------------------------------------------------------------------
# ---- Verify Production Numbers ----------------------------------------------------
# -----------------------------------------------------------------------------------

{

# verify production levels

production = data.table(dat[Period > 0, .(Resource, SKU, Production)])

setkey(production, SKU)
setkey(SKUID, ID)

production = SKUID[production]
production[, ID := NULL]

production[, Resource := ifelse(Resource == 1, "Resource1", "Resource2")]

# registerDoParallel(cores = cores) 

# check = foreach(i = 1:nrow(master.schedule)) %dopar%
check = foreach(i = 1:nrow(master.schedule)) %do%
{
	require(foreach)
	require(data.table)
	
	result = data.table(Cycle = master.schedule$Cycle[i], 
						SKU = master.schedule$SKU[i],
						Resource = master.schedule$Resource[i],
						Quantity.Check = int(x = production[Resource == master.schedule$Resource[i] & SKU == master.schedule$SKU[i], Production], from = ifelse(is.na(master.schedule$start.time[i]), 0, master.schedule$start.time[i]), to = ifelse(is.na(master.schedule$end.time[i]), 0, master.schedule$end.time[i])))

	return(result)
}

# registerDoSEQ()

check = rbindlist(check)

master.schedule[, Quantity.Actual := floor(check$Quantity.Check)]
master.schedule[, Quantity.Diff := Quantity.Actual - Quantity]
master.schedule[Quantity.Diff != 0]

}

# -----------------------------------------------------------------------------------
# ---- Plotting Master Schedule -----------------------------------------------------
# -----------------------------------------------------------------------------------

{

# ---- plot inventory levels -------------------------------------------

# registerDoParallel(cores = cores) 

# END = foreach(i = 1:nrow(master.schedule)) %dopar%
END = foreach(i = 1:nrow(master.schedule)) %do%
{
	require(foreach)
	require(data.table)
	
	result = max(master.schedule[Cycle == master.schedule$Cycle[i], end.time], na.rm = TRUE)
	return(result)
}

# registerDoSEQ()

master.schedule[, END := unlist(END)]

inv.plot = ggplot(data = master.schedule, aes(x = END, y = Inventory, color = Resource, group = Resource)) +
			geom_point(size = 3) +
			geom_line(size = 1) + 
			geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
			facet_wrap(~SKU) +
			ggtitle("Inventory Levels") + 
			labs(x = "Period", y = "Units", color = "Resource") + 
			theme_bw(base_size = 25) +
			theme(legend.position = "top", legend.key.size = unit(.25, "in"), plot.title = element_text(hjust = 0.5)) +
			guides(color = guide_legend(override.aes = list(size = 10, linetype = 0)))

inv.plot

# ---- plot production order -------------------------------------------

master.schedule[, duration.time := end.time - start.time]
master.schedule[, Cycle.Name := factor(paste("Cycle", Cycle), levels = unique(paste("Cycle", Cycle)))]
master.schedule[, Order := rep(1:nrow(SKUID), max(Cycle))]

prod.plot = ggplot(data = master.schedule, aes(x = Order, y = duration.time, fill = SKU, group = SKU)) +
			geom_bar(stat = "identity", position = "dodge") +
			geom_text(aes(label = SKU), vjust = -0.5, size = 5, color = "black") +
			ylim(c(0, 1.75)) +
			facet_wrap(~Cycle.Name) +
			ggtitle("Production Order") + 
			labs(x = "Order", y = "Duration (Periods)", fill = "SKU") + 
			theme_bw(base_size = 20) +
			theme(legend.position = "top", legend.key.size = unit(.25, "in"), plot.title = element_text(hjust = 0.5)) +
			guides(fill = guide_legend(override.aes = list(size = 10, linetype = 0)))
		
prod.plot

}

# -----------------------------------------------------------------------------------
# ---- Summary of Master Schedule ---------------------------------------------------
# -----------------------------------------------------------------------------------

# summary tables

diagnostics
master.schedule[Inventory < 0]

# summary plots

inv.plot
prod.plot


