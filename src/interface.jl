const AllInfinities = Union{Infinity, RealInfinity, ComplexInfinity, InfiniteCardinal}
const AllRealInfinities = Union{Infinity, RealInfinity, ComplexInfinity{<:Integer}}
const IntegerInfinities = Union{Infinity, RealInfinity, ComplexInfinity{<:Integer}, InfiniteCardinal}
const ExtendedComplex{T} = Union{Complex{T}, ComplexInfinity{T}}

iszero(::AllInfinities) = false
isinf(::AllInfinities) = true
isfinite(::AllInfinities) = false

promote_rule(::Type{Infinity}, ::Type{RealInfinity}) = RealInfinity # not detected by CodeCov. Removing this results in failed tests.
promote_rule(::Type{Infinity}, ::Type{ComplexInfinity{T}}) where T = ComplexInfinity{T}
promote_rule(::Type{RealInfinity}, ::Type{ComplexInfinity{T}}) where T = ComplexInfinity{T}
promote_rule(::Type{ComplexInfinity{T}}, ::Type{RealInfinity}) where T<:Integer = ComplexInfinity{T}
promote_rule(::Type{ComplexInfinity{T}}, ::Type{ComplexInfinity{S}}) where {T, S} = ComplexInfinity{promote_type(T, S)}