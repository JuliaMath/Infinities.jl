const AllInfinities = Union{Infinity, RealInfinity, ComplexInfinity, InfiniteCardinal}
const AllRealInfinities = Union{Infinity, RealInfinity, ComplexInfinity{<:Integer}}

iszero(::AllInfinities) = false
isinf(::AllInfinities) = true
isfinite(::AllInfinities) = false