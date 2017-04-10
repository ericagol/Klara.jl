type GibbsFactor <: Factor
  cliques::Vector{Vector{Symbol}}
  logpotentials::FunctionVector
  assignments::Vector{Pair{Symbol, Vector{Symbol}}}
  transforms::FunctionVector
  variables::Vector{Symbol}
  variabletypes::Vector{DataType}
  support::RealPairVector
  reparametrize::Vector{Symbol} # :none or :log
  ofvariable::Dict{Symbol, Integer}
end

num_cliques(f::GibbsFactor) = length(f.cliques)

num_transforms(f::GibbsFactor) = length(f.assignments)

num_vertices(f::GibbsFactor) = length(f.variables)

GibbsFactor(
  cliques::Vector{Vector{Symbol}},
  logpotentials::FunctionVector,
  assignments::Vector{Pair{Symbol, Vector{Symbol}}},
  transforms::FunctionVector,
  variables::Vector{Symbol},
  variabletypes::Vector{DataType},
  support::RealPairVector,
  reparametrize::Vector{Symbol},
  n::Integer=length(variables)
) =
  GibbsFactor(
    cliques,
    logpotentials,
    assignments,
    transforms,
    variables,
    variabletypes,
    support,
    reparametrize,
    Dict(zip(variables, 1:n))
  )

GibbsFactor(
  cliques::Vector{Vector{Symbol}},
  logpotentials::FunctionVector,
  variables::Vector{Symbol},
  assignments::Vector{Pair{Symbol, Vector{Symbol}}}=Pair{Symbol, Vector{Symbol}}[],
  transforms::FunctionVector=Function[],
  n::Integer=length(variables)
) =
  GibbsFactor(cliques, logpotentials, assignments, transforms, variables, DataType[], RealPair[], Symbol[], n)

function GibbsFactor(
  cliques::Vector{Vector{Symbol}},
  logpotentials::FunctionVector,
  assignments::Vector{Pair{Symbol, Vector{Symbol}}}=Pair{Symbol, Vector{Symbol}}[],
  transforms::FunctionVector=Function[]
)
  variables = isempty(assignments) ? unique(vcat(cliques...)) : unique(vcat(cliques..., [a.first for a in assignments]))

  GibbsFactor(
    cliques,
    logpotentials,
    assignments,
    transforms,
    variables,
    DataType[],
    RealPair[],
    Symbol[],
    length(variables)
  )
end

# Defined constructors that take only variables as their input
# To do: constructors for i) joint input of variables and variabletypes, ii) input of variabletypes only

function GibbsFactor(
  cliques::Vector{Vector{Symbol}},
  logpotentials::FunctionVector,
  variabletypes::Dict{Symbol, DataType},
  support::Dict{Symbol, RealPair},
  reparametrize::Dict{Symbol, Symbol},
  n::Integer=length(variabletypes)
)
  local vkeys::Vector{Symbol} = Array(Symbol, n)
  local vvalues::Vector{DataType} = Array(DataType, n)
  local vsupport::RealPairVector = Array(RealPair, n)
  local vtransform::Vector{Symbol} = Array(Symbol, n)
  local i::Int64 = 1

  for (k, v) in variabletypes
    vkeys[i] = k
    vvalues[i] = v
    vsupport[i] = get(support, k, Pair(-Inf, Inf))
    vtransform[i] = get(reparametrize, k, :none)
    i += 1
  end

  return GibbsFactor(cliques, logpotentials, vkeys, vvalues, vsupport, vtransform, n)
end

GibbsFactor(
  cliques::Vector{Vector{Symbol}},
  logpotentials::FunctionVector,
  variabletypes::Dict;
  support::Dict=Dict{Symbol, RealPair}(),
  reparametrize::Dict=Dict{Symbol, Symbol}(),
  n::Integer=length(variabletypes)
) =
  GibbsFactor(
    cliques,
    logpotentials,
    convert(Dict{Symbol, DataType}, variabletypes),
    convert(Dict{Symbol, RealPair}, support),
    convert(Dict{Symbol, Symbol}, reparametrize),
    n
  )

function codegen_logtarget(f::GibbsFactor, i::Integer, nc::Integer=num_cliques(f))
  local body = [:(_state.logtarget = 0.)]
  local lpargs::Vector

  for j in 1:nc
    if in(f.variables[i], f.cliques[j])
      lpargs = []

      for v in f.cliques[j]
        if v == f.variables[i]
          push!(lpargs, :(_state.value))
        else
          push!(lpargs, :(_states[$(f.ofvariable[v])].value))
        end
      end

      push!(body, :(_state.logtarget += f.logpotentials[$j]($(lpargs...))))
    end
  end

  @gensym logtarget

  quote
    function $logtarget(_state::$(default_state_type(f.variabletypes[i])), _states::VariableStateVector)
      $(body...)
    end
  end
end

function generate_variables(f::GibbsFactor, nc::Integer=num_cliques(f), nv::Integer=num_vertices(f))
  local variables::VariableVector = Array(Variable, nv)

  for i in 1:nv
    if issubtype(f.variabletypes[i], Parameter)
      variables[i] = f.variabletypes[i](f.variables[i], signature=:low, logtarget=eval(codegen_logtarget(f, i, nc)))
    else
      variables[i] = f.variabletypes[i](f.variables[i])
    end
  end

  return variables
end
