### Sampleability

abstract Sampleability

type Deterministic <: Sampleability end

type Random <: Sampleability end

### Variable

abstract Variable{S<:Sampleability}

Base.eltype{S<:Sampleability}(::Type{Variable{S}}) = S

vertex_key(v::Variable) = v.key
vertex_index(v::Variable) = v.index
is_indexed(v::Variable) = v.index > 0 ? true : false

Base.keys{V<:Variable}(variables::Vector{V}) = Symbol[v.key for v in variables]

Base.convert(::Type{KeyVertex}, v::Variable) = KeyVertex{Symbol}(v.index, v.key)
Base.convert(::Type{Vector{KeyVertex}}, v::Vector{Variable}) = KeyVertex{Symbol}[convert(KeyVertex, i) for i in v]

function codegen_internal_variable_method(f::Function, r::Vector{Symbol}, nkeys::Int=0, vfarg::Bool=false)
  body::Expr
  fargs::Union{Expr, Vector}
  rvalues::Expr

  if nkeys == 0
    fargs = [:(_state.value)]
  elseif nkeys > 0
    if vfarg
      fargs = [Expr(:ref, :Any, [:(_states[$i].value) for i in 1:nkeys]...)]
    else
      fargs = [:(_state.value), Expr(:ref, :Any, [:(_states[$i].value) for i in 1:nkeys]...)]
    end
  else
    error("nkeys must be positive")
  end

  nr = length(r)
  if nr == 0
    body = :($(f)($(fargs...)))
  elseif nr == 1
    rvalues = Expr(:., :_state, QuoteNode(r[1]))
    body = :($rvalues = $(f)($(fargs...)))
  elseif nr > 1
    rvalues = Expr(:tuple, [Expr(:., :_state, QuoteNode(r[i])) for i in 1:nr]...)
    body = :($rvalues = $(f)($(fargs...)))
  else
    error("Vector of return symbols must have one or more elements")
  end

  @gensym internal_variable_method

  quote
    function $internal_variable_method(_state, _states)
      $(body)
    end
  end
end

Base.show(io::IO, v::Variable) = print(io, "Variable [$(v.index)]: $(v.key) ($(typeof(v)))")

### Deterministic Variable subtypes

## Constant

immutable Constant <: Variable{Deterministic}
  key::Symbol
  index::Int
end

Constant(key::Symbol) = Constant(key, 0)

default_state{N<:Number}(variable::Constant, value::N) = BasicUnvVariableState(value)
default_state{N<:Number}(variable::Constant, value::Vector{N}) = BasicMuvVariableState(value)
default_state{N<:Number}(variable::Constant, value::Matrix{N}) = BasicMavVariableState(value)

Base.show(io::IO, ::Type{Constant}) = print(io, "Constant")
Base.writemime(io::IO, ::MIME"text/plain", t::Type{Constant}) = show(io, t)

## Hyperparameter

typealias Hyperparameter Constant

## Data

immutable Data <: Variable{Deterministic}
  key::Symbol
  index::Int
  update::Union{Function, Void}
end

Data(key::Symbol, index::Int) = Data(key, index, nothing)
Data(key::Symbol, update::Union{Function, Void}) = Data(key, 0, update)
Data(key::Symbol) = Data(key, 0, nothing)

default_state{N<:Number}(variable::Data, value::N) = BasicUnvVariableState(value)
default_state{N<:Number}(variable::Data, value::Vector{N}) = BasicMuvVariableState(value)
default_state{N<:Number}(variable::Data, value::Matrix{N}) = BasicMavVariableState(value)

Base.show(io::IO, ::Type{Data}) = print(io, "Data")
Base.writemime(io::IO, ::MIME"text/plain", t::Type{Data}) = show(io, t)

## Transformation

immutable Transformation{S<:VariableState} <: Variable{Deterministic}
  key::Symbol
  index::Int
  transform::Function
  states::Vector{S}
end

Transformation{S<:VariableState}(key::Symbol, transform::Function, states::Vector{S}=VariableState[]) =
  Transformation(key, 0, transform, states)

default_state{N<:Number}(variable::Transformation, value::N) = BasicUnvVariableState(value)
default_state{N<:Number}(variable::Transformation, value::Vector{N}) = BasicMuvVariableState(value)
default_state{N<:Number}(variable::Transformation, value::Matrix{N}) = BasicMavVariableState(value)

Base.show(io::IO, ::Type{Transformation}) = print(io, "Transformation")
Base.writemime(io::IO, ::MIME"text/plain", t::Type{Transformation}) = show(io, t)

### Random Variable subtypes

## Abstract Parameter types

abstract Parameter{S<:ValueSupport, F<:VariateForm} <: Variable{Random}

value_support{S<:ValueSupport, F<:VariateForm}(::Type{Parameter{S, F}}) = S
variate_form{S<:ValueSupport, F<:VariateForm}(::Type{Parameter{S, F}}) = F

function check_support{P<:Parameter, S<:ParameterState}(parameter::P, state::S)
  if value_support(parameter) != value_support(state)
    warn("Value support of parameter ($(value_support(parameter))) and of ($(value_support(state))) not in agreement")
  end

  if variate_form(parameter) != variate_form(state)
    error("Variate form of parameter ($(variate_form(parameter))) and of ($(variate_form(state))) not in agreement")
  end
end