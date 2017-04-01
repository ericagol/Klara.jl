type GibbsFactor <: Factor
  cliques::Vector{Vector{Symbol}}
  potentials::Vector{Function}
  vertices::Vector{Symbol}
  types::Vector{Symbol}
  ofvertex::Dict{Symbol, Integer}
end

GibbsFactor(
  cliques::Vector{Vector{Symbol}},
  potentials::Vector{Function},
  vertices::Vector{Symbol},
  types::Vector{Symbol},
  n::Integer=length(vertices)
) =
  GibbsFactor(cliques, potentials, vertices, types, Dict(zip(vertices, 1:n)))

GibbsFactor(cliques::Vector{Vector{Symbol}}, potentials::Vector{Function}, vertices::Vector{Symbol}) =
  GibbsFactor(cliques, potentials, vertices, Symbol[])

GibbsFactor(cliques::Vector{Vector{Symbol}}, potentials::Vector{Function}) =
  GibbsFactor(cliques, potentials, unique(vcat(cliques...)))

function GibbsFactor(
  cliques::Vector{Vector{Symbol}},
  potentials::Vector{Function},
  types::Dict{Symbol, Symbol},
  n::Integer=length(types)
)
  local vkeys::Vector{Symbol} = Array(Symbol, n)
  local vvals::Vector{Symbol} = Array(Symbol, n)
  local i::Int64 = 1

  for (k, v) in types
    vkeys[i] = k
    vvals[i] = v
    i += 1
  end

  GibbsFactor(cliques, potentials, vkeys, vvals, n)
end
