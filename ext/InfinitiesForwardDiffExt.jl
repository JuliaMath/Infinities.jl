module InfinitiesForwardDiffExt

using Infinities: Infinity, RealInfinity, NotANumber
import ForwardDiff: Dual, Partials, value, partials

function symbolic_dual(::Dual{Tag}, primal, tangents::NTuple{N, Any}) where {Tag, N}
    Dual{Tag, Real, N}(primal, Partials{N, Real}(tangents))
end

for Typ in (Infinity, RealInfinity, NotANumber)
    for op in (:^, :min, :max, :(==), :isequal, :<, :<=, :isless)
        @eval Base.$op(dual::Dual, inf::$Typ) = invoke($op, Tuple{Dual, Real}, dual, inf)
        @eval Base.$op(inf::$Typ, dual::Dual) = invoke($op, Tuple{Real, Dual}, inf, dual)
    end

    for (op, forward, reverse) in ((:+, :tangent, :tangent),
        (:-, :tangent, :(-tangent)), (:*, :(tangent * inf), :(tangent * inf)),
        (:/, :(tangent / inf), :(-(primal / value(dual)) * tangent)))
        @eval function Base.$op(dual::Dual, inf::$Typ)
            primal = $op(value(dual), inf)
            symbolic_dual(dual, primal, map(tangent -> $forward, Tuple(partials(dual))))
        end
        @eval function Base.$op(inf::$Typ, dual::Dual)
            primal = $op(inf, value(dual))
            symbolic_dual(dual, primal, map(tangent -> $reverse, Tuple(partials(dual))))
        end
    end

    for op in (:mod, :rem)
        @eval function Base.$op(dual::Dual, inf::$Typ)
            primal = $op(value(dual), inf)
            symbolic_dual(dual, primal, map(tangent -> isnan(primal) ? NotANumber() : tangent, Tuple(partials(dual))))
        end
        @eval Base.$op(inf::$Typ, dual::Dual) =
            symbolic_dual(dual, $op(inf, value(dual)), map(_ -> NotANumber(), Tuple(partials(dual))))
    end

    @eval begin
        Dual(inf::$Typ) = Dual{Nothing}(inf, ())
        Dual{Tag}(inf::$Typ) where {Tag} = Dual{Tag}(inf, ())
        Dual{Tag, Value}(inf::$Typ) where {Tag, Value} = Dual{Tag, Value, 0}(inf)
        Dual{Tag, Value, N}(inf::$Typ) where {Tag, Value, N} =
            Dual{Tag, Value, N}(convert(Value, inf), zero(Partials{N, Value}))
        Dual{Tag}(inf::$Typ, tangents::Partials{N, Value}) where {Tag, N, Value} =
            Dual{Tag, Real, N}(inf, Partials{N, Real}(Tuple(tangents)))
        Dual{Tag}(inf::Value, tangents::Partials{N, Value}) where {Tag, N, Value<:$Typ} =
            Dual{Tag, Real, N}(inf, Partials{N, Real}(Tuple(tangents)))
    end
end

end