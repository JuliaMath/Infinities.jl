# ==
@inline _eq(x, y::InfiniteCardinal) = x == ∞ && y == ℵ₀
@inline _eq(x, y::AllInfinities) = isinf(x) && angle(y) == angle(x)
@inline _infeq(x, y) = _eq(x, y)
@inline _infeq(x::InfiniteCardinal, y) = _eq(y, x)
@inline _infeq(x::InfiniteCardinal, y::InfiniteCardinal) = !(x<y) & !(y<x)
==(x::AllInfinities, y::Number) = _eq(y, x)
==(y::Number, x::AllInfinities) = _eq(y, x)
==(x::AllInfinities, y::AllInfinities) = _infeq(x, y)

# isless
isless(x::AllRealInfinities, y::AllRealInfinities) = signbit(x) && !signbit(y)
@generated isless(::InfiniteCardinal{N}, ::InfiniteCardinal{M}) where {N,M} = :($(isless(N, M)))
for Typ in (Number, Real, AbstractFloat)
    @eval begin
        isless(x::AllRealInfinities, y::$Typ) = (signbit(y); signbit(x) && y ≠ -∞)
        isless(x::$Typ, y::AllRealInfinities) = (signbit(x); !signbit(y) && x ≠ ∞)
    end
end
for Typ in (Number, Real, AbstractFloat, AllRealInfinities)
    @eval begin
        isless(::InfiniteCardinal, x::$Typ) = false
        isless(x::$Typ, y::InfiniteCardinal) = isless(x, ∞) || isless(ℵ₀, y)
    end
end
isless(::InfiniteCardinal{0}, ::InfiniteCardinal{0}) = false

# minmax, <, ≤
@inline _max(x, y) = ifelse(y < x, x, y)
@inline _min(x, y) = ifelse(y < x, y, x)
@inline _le(x, y) = x < y || x == y
for (op, fop) in ((:max, :_max), (:min, :_min), (:<, :isless), (:≤, :_le))
    for Typ in (Real, )
        @eval begin
            $op(x::AllInfinities, y::$Typ) = $fop(x, y)
            $op(x::$Typ, y::AllInfinities) = $fop(x, y)
        end
    end
    @eval $op(x::AllInfinities, y::AllInfinities) = $fop(x, y)
end
        