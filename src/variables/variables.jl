### Sampleability

abstract Sampleability

type Deterministic <: Sampleability end

type Random <: Sampleability end

### Abstract variable

abstract Variable{S<:Sampleability}

typealias VariableVector{V<:Variable} Vector{V}

eltype{S<:Sampleability}(::Type{Variable{S}}) = S

vertex_key(v::Variable) = v.key
vertex_index(v::Variable) = v.index
is_indexed(v::Variable) = v.index > 0 ? true : false

keys(variables::VariableVector) = Symbol[v.key for v in variables]
indexes(variables::VariableVector) = Integer[v.index for v in variables]

sort_by_index(vs::VariableVector) = vs[[v.index for v in vs]]

function default_state(v::Variable, v0, outopts::Dict=Dict())
  local vstate::VariableState

  if isa(v0, VariableState)
    vstate = v0
  elseif isa(v0, Number) ||
    (isa(v0, Vector) && issubtype(eltype(v0), Number)) ||
    (isa(v0, Matrix) && issubtype(eltype(v0), Number))
    if isa(v, Parameter)
      vstate = default_state(v, v0, outopts)
    else
      vstate = default_state(v, v0)
    end
  else
    error("Variable state or state value of type $(typeof(v0)) not valid")
  end

  vstate
end

default_state(v::VariableVector, v0::Vector, outopts::Vector) =
  VariableState[default_state(v[i], v0[i], outopts[i]) for i in 1:length(v0)]

function default_state(v::VariableVector, v0::Vector, outopts::Vector, dpindex::IntegerVector)
  opts = fill(Dict(), length(v0))
  for i in 1:length(dpindex)
    opts[dpindex[i]] = outopts[i]
  end
  default_state(v, v0, opts)
end

show(io::IO, v::Variable) = print(io, "Variable [$(v.index)]: $(v.key) ($(typeof(v)))")

### Deterministic Variable subtypes include Constant, Data and Transformation

## Constant

type Constant <: Variable{Deterministic}
  key::Symbol
  index::Integer
end

Constant(key::Symbol) = Constant(key, 0)

default_state{N<:Number}(variable::Constant, value::N) = BasicUnvVariableState(value)
default_state{N<:Number}(variable::Constant, value::Vector{N}) = BasicMuvVariableState(value)
default_state{N<:Number}(variable::Constant, value::Matrix{N}) = BasicMavVariableState(value)

show(io::IO, ::Type{Constant}) = print(io, "Constant")

dotshape(variable::Constant) = "trapezium"

## Hyperparameter

typealias Hyperparameter Constant

## Data

type Data <: Variable{Deterministic}
  key::Symbol
  index::Integer
  update!::Union{Function, Void}
end

Data(key::Symbol, index::Integer) = Data(key, index, nothing)
Data(key::Symbol, update::Union{Function, Void}) = Data(key, 0, update)
Data(key::Symbol) = Data(key, 0, nothing)

default_state{N<:Number}(variable::Data, value::N) = BasicUnvVariableState(value)
default_state{N<:Number}(variable::Data, value::Vector{N}) = BasicMuvVariableState(value)
default_state{N<:Number}(variable::Data, value::Matrix{N}) = BasicMavVariableState(value)

show(io::IO, ::Type{Data}) = print(io, "Data")

dotshape(variable::Data) = "box"

## Transformation

type Transformation <: Variable{Deterministic}
  key::Symbol
  index::Integer
  transform::Union{Function, Void}
end

Transformation(key::Symbol, index::Integer=0; signature::Symbol=:high, args...) =
  Transformation(key, Val{signature}, index; args...)

Transformation(key::Symbol, ::Type{Val{:low}}, index::Integer=0; transform::Union{Function, Void}=nothing) =
  Transformation(key, index, transform)

function Transformation(
  key::Symbol,
  ::Type{Val{:high}},
  index::Integer=0;
  transform::Union{Function, Void}=nothing,
  nkeys::Integer=1
)
  @assert nkeys > 0 "nkeys must be positive for transformations, got $nkeys"
  Transformation(
    key,
    index,
    if isa(transform, Function)
      eval(
        codegen_lowlevel_variable_method(transform, statearg=false, vfarg=true, nkeys=nkeys)
      )
    else
      nothing
    end
  )
end

default_state{N<:Number}(variable::Transformation, value::N) = BasicUnvVariableState(value)
default_state{N<:Number}(variable::Transformation, value::Vector{N}) = BasicMuvVariableState(value)
default_state{N<:Number}(variable::Transformation, value::Matrix{N}) = BasicMavVariableState(value)

show(io::IO, ::Type{Transformation}) = print(io, "Transformation")

dotshape(variable::Transformation) = "polygon"
