for Typ in (Base.TwicePrecision, AbstractChar, Complex)
    @eval begin
        RealInfinity(x::$Typ) = throw(MethodError(RealInfinity, x))
        ComplexInfinity{T}(x::$Typ) where T<:Real = ComplexInfinity(T(x))
    end
end
ComplexInfinity{T}(x::ComplexInfinity{T}) where T<:Real = x

for Typ in (Rational, BigInt, BigFloat)
    for (op, fop) in ((:<, :_lt), (:≤, :_le))
        @eval $op(x::InfiniteCardinal, y::$Typ) = $fop(x, y)
        @eval $op(x::$Typ, y::InfiniteCardinal) = $fop(x, y)
    end
end

for Typ in (Rational, BigInt, BigFloat, Complex, AbstractIrrational)
    @eval ==(x::AllInfinities, y::$Typ) = _eq(y, x)
    @eval ==(x::$Typ, y::AllInfinities) = _eq(x, y)
end

for Typ in (Complex, Rational, Complex{Bool}, Integer)
    # `_add` and `_mul` dispatch on the infinity being second; `_sub` and `_div` delegate to them.
    for (op, fop) in ((:+, :_add), (:*, :_mul))
        @eval $op(x::AllInfinities, y::$Typ) = $fop(y, x)
        @eval $op(x::$Typ, y::AllInfinities) = $fop(x, y)
    end
    for (op, fop) in ((:-, :_sub), (:/, :_div))
        @eval $op(x::AllInfinities, y::$Typ) = $fop(x, y)
        @eval $op(x::$Typ, y::AllInfinities) = $fop(x, y)
    end
end

^(x::RealInfinity, y::Rational) = _infpow(infpromote(x, y)...)

for Typ in (Rational, )
    @eval mod(::IntegerInfinities, ::$Typ) = NotANumber()
    @eval mod(x::$Typ, y::IntegerInfinities) = _mod(x, y)
    @eval rem(::InfiniteCardinal, ::$Typ) = NotANumber()
    @eval rem(x::$Typ, ::IntegerInfinities) = x
    for op in (:fld, :cld, :div)
        @eval $op(x::InfiniteCardinal, y::$Typ) = _inffcd(x, y)
    end
    @eval div(x::$Typ, ::IntegerInfinities) = _divinf(x)
    @eval fld(x::$Typ, ::IntegerInfinities) = _fldinf(x)
    @eval cld(x::$Typ, ::IntegerInfinities) = _cldinf(x)
end

divrem(x::BigInt, y::IntegerInfinities) = (div(x, y), rem(x, y))

# an `InfiniteCardinal` is an `Integer`, for which `Base` has its own `isapprox`
isapprox(x::InfiniteCardinal, y::Integer; kwargs...) = x == y
isapprox(x::Integer, y::InfiniteCardinal; kwargs...) = x == y
isapprox(x::InfiniteCardinal, y::InfiniteCardinal; kwargs...) = x == y