for Typ in (Rational, BigInt, BigFloat)
    for (op, fop) in ((:<, :isless), (:≤, :_le))
        @eval $op(x::InfiniteCardinal, y::$Typ) = $fop(x, y)
        @eval $op(x::$Typ, y::InfiniteCardinal) = $fop(x, y)
    end
end
for Typ in (Rational, BigInt, BigFloat, Complex, AbstractIrrational)
    @eval ==(x::AllInfinities, y::$Typ) = _eq(y, x)
    @eval ==(x::$Typ, y::AllInfinities) = _eq(x, y)
end