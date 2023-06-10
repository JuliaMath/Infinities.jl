const AllInfinities = Union{Infinity, RealInfinity, ComplexInfinity, InfiniteCardinal}
const AllRealInfinities = Union{Infinity, RealInfinity, ComplexInfinity{<:Integer}}
const IntegerInfinities = Union{Infinity, RealInfinity, ComplexInfinity{<:Integer}, InfiniteCardinal}

iszero(::AllInfinities) = false
isinf(::AllInfinities) = true
isfinite(::AllInfinities) = false