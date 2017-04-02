typealias IntegerVector{I<:Integer} Vector{I}
typealias RealVector{N<:Real} Vector{N}
typealias RealMatrix{N<:Real} Matrix{N}

typealias RealLowerTriangular{T<:Real, S<:AbstractMatrix} LowerTriangular{T, S}

typealias FunctionVector{F<:Function} Vector{F}

multivecs{T}(::Type{T}, n::Int) = [T[] for _ =1:n]
