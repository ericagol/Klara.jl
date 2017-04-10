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
  variables::Vector{Symbol};
  assignments::Vector{Pair{Symbol, Vector{Symbol}}}=Pair{Symbol, Vector{Symbol}}[],
  transforms::FunctionVector=Function[],
  n::Integer=length(variables)
) =
  GibbsFactor(cliques, logpotentials, assignments, transforms, variables, DataType[], RealPair[], Symbol[], n)

function GibbsFactor(
  cliques::Vector{Vector{Symbol}},
  logpotentials::FunctionVector;
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

function GibbsFactor(
  cliques::Vector{Vector{Symbol}},
  logpotentials::FunctionVector,
  variables::Vector{Symbol},
  variabletypes::Dict{Symbol, DataType};
  assignments::Vector{Pair{Symbol, Vector{Symbol}}}=Pair{Symbol, Vector{Symbol}}[],
  transforms::FunctionVector=Function[],
  support::Dict{Symbol, RealPair}=Dict{Symbol, RealPair}(),
  reparametrize::Dict{Symbol, Symbol}=Dict{Symbol, Symbol}(),
  n::Integer=length(variables)
)
  local vtypes::Vector{DataType} = Array(DataType, n)
  local vsupport::RealPairVector = Array(RealPair, n)
  local vreparametrize::Vector{Symbol} = Array(Symbol, n)

  for (v, i) in zip(variables, 1:n)
    vtypes[i] = variabletypes[v]
    vsupport[i] = get(support, v, Pair(-Inf, Inf))
    vreparametrize[i] = get(reparametrize, v, :none)
  end

  GibbsFactor(cliques, logpotentials, assignments, transforms, variables, vtypes, vsupport, vreparametrize, n)
end

GibbsFactor(
  cliques::Vector{Vector{Symbol}},
  logpotentials::FunctionVector,
  variables::Vector{Symbol},
  variabletypes::Dict;
  assignments::Vector{Pair{Symbol, Vector{Symbol}}}=Pair{Symbol, Vector{Symbol}}[],
  transforms::FunctionVector=Function[],
  support::Dict=Dict{Symbol, RealPair}(),
  reparametrize::Dict=Dict{Symbol, Symbol}(),
  n::Integer=length(variables)
) =
  GibbsFactor(
    cliques,
    logpotentials,
    variables,
    convert(Dict{Symbol, DataType}, variabletypes),
    assignments=assignments,
    transforms=transforms,
    support=convert(Dict{Symbol, RealPair}, support),
    reparametrize=convert(Dict{Symbol, Symbol}, reparametrize),
    n=n
  )

function GibbsFactor(
  cliques::Vector{Vector{Symbol}},
  logpotentials::FunctionVector,
  variabletypes::Dict{Symbol, DataType};
  assignments::Vector{Pair{Symbol, Vector{Symbol}}}=Pair{Symbol, Vector{Symbol}}[],
  transforms::FunctionVector=Function[],
  support::Dict{Symbol, RealPair}=Dict{Symbol, RealPair}(),
  reparametrize::Dict{Symbol, Symbol}=Dict{Symbol, Symbol}(),
  n::Integer=length(variabletypes)
)
  local variables::Vector{Symbol} = Array(Symbol, n)
  local vtypes::Vector{DataType} = Array(DataType, n)
  local vsupport::RealPairVector = Array(RealPair, n)
  local vreparametrize::Vector{Symbol} = Array(Symbol, n)
  local i::Int64 = 1

  for (k, t) in variabletypes
    variables[i] = k
    vtypes[i] = t
    vsupport[i] = get(support, k, Pair(-Inf, Inf))
    vreparametrize[i] = get(reparametrize, k, :none)
    i += 1
  end

  return GibbsFactor(cliques, logpotentials, assignments, transforms, variables, vtypes, vsupport, vreparametrize, n)
end

GibbsFactor(
  cliques::Vector{Vector{Symbol}},
  logpotentials::FunctionVector,
  variabletypes::Dict;
  assignments::Vector{Pair{Symbol, Vector{Symbol}}}=Pair{Symbol, Vector{Symbol}}[],
  transforms::FunctionVector=Function[],
  support::Dict=Dict{Symbol, RealPair}(),
  reparametrize::Dict=Dict{Symbol, Symbol}(),
  n::Integer=length(variabletypes)
) =
  GibbsFactor(
    cliques,
    logpotentials,
    convert(Dict{Symbol, DataType}, variabletypes),
    assignments=assignments,
    transforms=transforms,
    support=convert(Dict{Symbol, RealPair}, support),
    reparametrize=convert(Dict{Symbol, Symbol}, reparametrize),
    n=n
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
