for Typ in (Rational, BigInt, BigFloat)
    for (op, fop) in ((:<, :isless), (:≤, :_le))
        @eval $op(x::InfiniteCardinal, y::$Typ) = $fop(x, y)
        @eval $op(x::$Typ, y::InfiniteCardinal) = $fop(x, y)
    end
end
for Typ in (Rational, BigInt, BigFloat, Complex)
    @eval ==(x::InfiniteCardinal, y::$Typ) = _eq(y, x)
    @eval ==(x::$Typ, y::InfiniteCardinal) = _eq(x, y)
end