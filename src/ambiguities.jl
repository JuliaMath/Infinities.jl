for Typ in (Base.TwicePrecision, AbstractChar, Complex)
    @eval begin
        RealInfinity(x::$Typ) = throw(MethodError(RealInfinity, x))
        ComplexInfinity{T}(x::$Typ) where T<:Real = throw(MethodError(ComplexInfinity{T}, x))
    end
end
ComplexInfinity{T}(x::ComplexInfinity{T}) where T<:Real = x

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

for Typ in (Complex, Rational, Complex{Bool}, Integer)
    @eval +(x::AllInfinities, y::$Typ) = _add(y, x)
    @eval +(x::$Typ, y::AllInfinities) = _add(x, y)
    @eval -(x::AllInfinities, y::$Typ) = _sub(x, y)
    @eval -(x::$Typ, y::AllInfinities) = _sub(x, y)
    @eval *(x::AllInfinities, y::$Typ) = _mul(y, x)
    @eval *(x::$Typ, y::AllInfinities) = _mul(x, y)
end

for Typ in (Rational, )
    @eval mod(::IntegerInfinities, ::$Typ) = NotANumber()
    @eval mod(x::$Typ, y::IntegerInfinities) = _mod(x, y)
    for op in (:fld, :cld, :div)
        @eval $op(x::InfiniteCardinal, y::$Typ) = _inffcd(x, y)
    end
    @eval div(::T, ::IntegerInfinities) where T <: $Typ = _divinf(T)
    @eval fld(x::$Typ, ::IntegerInfinities) = _fldinf(x)
    @eval cld(x::$Typ, ::IntegerInfinities) = _cldinf(x)
end