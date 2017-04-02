type GibbsFactor <: Factor
  cliques::Vector{Vector{Symbol}}
  logpotentials::FunctionVector
  vertices::Vector{Symbol}
  vertextypes::Vector{DataType}
  ofvertex::Dict{Symbol, Integer}
end

num_cliques(f::GibbsFactor) = length(f.cliques)

num_vertices(f::GibbsFactor) = length(f.vertices)

GibbsFactor(
  cliques::Vector{Vector{Symbol}},
  logpotentials::FunctionVector,
  vertices::Vector{Symbol},
  vertextypes::Vector{DataType},
  n::Integer=length(vertices)
) =
  GibbsFactor(cliques, logpotentials, vertices, vertextypes, Dict(zip(vertices, 1:n)))

GibbsFactor(cliques::Vector{Vector{Symbol}}, logpotentials::FunctionVector, vertices::Vector{Symbol}) =
  GibbsFactor(cliques, logpotentials, vertices, DataType[])

GibbsFactor(cliques::Vector{Vector{Symbol}}, logpotentials::FunctionVector) =
  GibbsFactor(cliques, logpotentials, unique(vcat(cliques...)))

function GibbsFactor(
  cliques::Vector{Vector{Symbol}},
  logpotentials::FunctionVector,
  vertextypes::Dict{Symbol, DataType},
  n::Integer=length(vertextypes)
)
  local vkeys::Vector{Symbol} = Array(Symbol, n)
  local vvals::Vector{Symbol} = Array(Symbol, n)
  local i::Int64 = 1

  for (k, v) in vertextypes
    vkeys[i] = k
    vvals[i] = v
    i += 1
  end

  return GibbsFactor(cliques, logpotentials, vkeys, vvals, n)
end

function codegen_logtarget(f::GibbsFactor, i::Integer, nc::Integer=num_cliques(f))
  local body = [:(_state.logtarget = 0.)]
  local lpargs::Vector

  for j in 1:nc
    if in(f.vertices[i], f.cliques[j])
      lpargs = []

      for v in f.cliques[j]
        if v == f.vertices[i]
          push!(lpargs, :(_state.value))
        else
          push!(lpargs, :(_states[$(f.ofvertex[v])].value))
        end
      end

      push!(body, :(_state.logtarget += f.logpotentials[$j]($(lpargs...))))
    end
  end

  @gensym logtarget

  quote
    function $logtarget(_state::$(default_state_type(f.vertextypes[i])), _states::VariableStateVector)
      $(body...)
    end
  end
end

function generate_variables(f::GibbsFactor, nc::Integer=num_cliques(f), nv::Integer=num_vertices(f))
  local variables::VariableVector = Array(Variable, nv)

  for i in 1:nv
    if issubtype(f.vertextypes[i], Parameter)
      variables[i] = f.vertextypes[i](f.vertices[i], signature=:low, logtarget=eval(codegen_logtarget(f, i, nc)))
    else
      variables[i] = f.vertextypes[i](f.vertices[i])
    end
  end

  return variables
end
