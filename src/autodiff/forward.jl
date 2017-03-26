function codegen_autodiff_function(::Type{Val{:forward}}, ::Type{Val{:derivative}}, f::Function)
  @gensym autodiff_function
  quote
    function $autodiff_function(_x::Real)
      getfield(ForwardDiff, :derivative)($f, _x)
    end
  end
end

function codegen_autodiff_function(::Type{Val{:forward}}, ::Type{Val{:gradient}}, f::Function)
  body = []

  push!(body, :(getfield(ForwardDiff, :gradient!)(_result, $f, _x, _cfg)))

  push!(body, :(return DiffBase.gradient(_result)))

  @gensym autodiff_function
  quote
    function $autodiff_function(_result::DiffBase.DiffResult, _x::Vector, _cfg::ForwardDiff.GradientConfig)
      $(body...)
    end
  end
end

function codegen_autodiff_target(::Type{Val{:forward}}, ::Type{Val{:hessian}}, f::Function)
  body = []

  push!(body, :(getfield(ForwardDiff, :hessian!)(_result, $f, _x)))
  # push!(body, :(getfield(ForwardDiff, :hessian!)(_result, $f, _x, _cfg)))

  push!(body, :(return -DiffBase.hessian(_result)))

  @gensym autodiff_target
  quote
    function $autodiff_target(_result::DiffBase.DiffResult, _x::Vector, _cfg::ForwardDiff.HessianConfig)
      $(body...)
    end
  end
end

function codegen_autodiff_uptofunction(::Type{Val{:forward}}, ::Type{Val{:derivative}}, f::Function)
  @gensym autodiff_uptofunction
  quote
    function $autodiff_uptofunction(_x::Real)
      result = DiffBase.DiffResult(_x, _x)
      getfield(ForwardDiff, :derivative!)(result, $f, _x)
      return DiffBase.value(result), DiffBase.derivative(result)
    end
  end
end

function codegen_autodiff_uptofunction(::Type{Val{:forward}}, ::Type{Val{:gradient}}, f::Function)
  body = []

  push!(body, :(getfield(ForwardDiff, :gradient!)(_result, $f, _x, _cfg)))

  push!(body, :(return DiffBase.value(_result), DiffBase.gradient(_result)))

  @gensym autodiff_uptofunction
  quote
    function $autodiff_uptofunction(_result::DiffBase.DiffResult, _x::Vector, _cfg::ForwardDiff.GradientConfig)
      $(body...)
    end
  end
end

function codegen_autodiff_uptotarget(::Type{Val{:forward}}, ::Type{Val{:hessian}}, f::Function)
  body = []

  push!(body, :(getfield(ForwardDiff, :hessian!)(_result, $f, _x)))
  # push!(body, :(getfield(ForwardDiff, :hessian!)(_result, $f, _x, _cfg)))

  push!(body, :(return DiffBase.value(_result), DiffBase.gradient(_result), -DiffBase.hessian(_result)))

  @gensym autodiff_uptotarget
  quote
    function $autodiff_uptotarget(_result::DiffBase.DiffResult, _x::Vector, _cfg::ForwardDiff.HessianConfig)
      $(body...)
    end
  end
end
