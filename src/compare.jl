# The direction of an infinity, as a precision-independent angle.
# `angle` evaluates π in the precision of its argument, so `angle(-Inf32) ≠ angle(-Inf)`.
_angle(x::Real) = angle(RealInfinity(signbit(x)))
_angle(x::Number) = angle(x)

# ==
@inline _eq(x, y::InfiniteCardinal) = x == ∞ && y == ℵ₀
@inline _eq(x, y::AllInfinities) = isinf(x) && angle(y) == _angle(x)
@inline _infeq(x, y) = _eq(x, y)
@inline _infeq(x::InfiniteCardinal, y) = _eq(y, x)
@inline _infeq(x::InfiniteCardinal, y::InfiniteCardinal) = !(x<y) & !(y<x)
==(x::AllInfinities, y::Number) = _eq(y, x)
==(y::Number, x::AllInfinities) = _eq(y, x)
==(x::AllInfinities, y::AllInfinities) = _infeq(x, y)

# isless
# `isless` is the sort order. `NaN` sorts after every other value, infinities included.
isless(x::AllRealInfinities, y::AllRealInfinities) = signbit(x) && !signbit(y)
@generated isless(::InfiniteCardinal{N}, ::InfiniteCardinal{M}) where {N,M} = :($(isless(N, M)))
# The leading `signbit` call discards its result. It is there to reject a non-real `Number`.
for Typ in (Number, Real, AbstractFloat)
    @eval begin
        isless(x::AllRealInfinities, y::$Typ) = (signbit(y); isnan(y) || signbit(x) && y ≠ -∞)
        isless(x::$Typ, y::AllRealInfinities) = (signbit(x); !isnan(x) && !signbit(y) && x ≠ ∞)
    end
end
for Typ in (Number, Real, AbstractFloat, AllRealInfinities)
    @eval begin
        isless(::InfiniteCardinal, x::$Typ) = isnan(x)
        isless(x::$Typ, y::InfiniteCardinal) = isless(x, ∞) || isless(ℵ₀, y)
    end
end
isless(::InfiniteCardinal{0}, ::InfiniteCardinal{0}) = false

# minmax, <, ≤
# `<`, `max` and `min` use the numeric comparison, not the sort order. They differ at `NaN`:
# it compares false against everything and propagates through `max` and `min`.
@inline _lt(x, y) = !isnan(x) && !isnan(y) && isless(x, y)
@inline _le(x, y) = x < y || x == y
@inline _max(x, y) = isnan(x) ? x : isnan(y) ? y : ifelse(_lt(y, x), x, y)
@inline _min(x, y) = isnan(x) ? x : isnan(y) ? y : ifelse(_lt(y, x), y, x)
for (op, fop) in ((:max, :_max), (:min, :_min), (:<, :_lt), (:≤, :_le))
    for Typ in (Real, )
        @eval begin
            $op(x::AllInfinities, y::$Typ) = $fop(x, y)
            $op(x::$Typ, y::AllInfinities) = $fop(x, y)
        end
    end
    @eval $op(x::AllInfinities, y::AllInfinities) = $fop(x, y)
end
