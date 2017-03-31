type GibbsFactor <: Factor
  cliques::Vector{Vector{Symbol}}
  potentials::Vector{Function}
  keys::Vector{Symbol}
end
