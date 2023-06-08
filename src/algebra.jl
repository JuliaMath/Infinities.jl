for Typ in (Base.TwicePrecision, AbstractChar, Complex)
    @eval begin
        RealInfinity(x::$Typ) = throw(MethodError(RealInfinity, x))
        ComplexInfinity{T}(x::$Typ) where T<:Real = throw(MethodError(ComplexInfinity{T}, x))
    end
end
ComplexInfinity{T}(x::ComplexInfinity{T}) where T<:Real = x

for op in (:<, :isless)
    @eval begin
        $op(s::RealInfinity, ::Infinity) = signbit(s)
        $op(s::RealInfinity, ::InfiniteCardinal{0}) = s < ∞
        $op(x::RealInfinity, ::InfiniteCardinal) = true
        $op(::Infinity, ::InfiniteCardinal{0}) = false
        $op(::Infinity, ::InfiniteCardinal) = true
        $op(x::InfiniteCardinal, ::Infinity) = false
        $op(x::InfiniteCardinal, ::RealInfinity) = false
        $op(::Infinity, ::RealInfinity) = false
        $op(s::ComplexInfinity{Bool}, ::Infinity) = signbit(s)
        $op(::Infinity, ::ComplexInfinity{Bool}) = false
        $op(::InfiniteCardinal{0}, ::InfiniteCardinal{0}) = false
        $op(::InfiniteCardinal, ::InfiniteCardinal{0}) = false
        $op(x::ComplexInfinity{Bool}, y::RealInfinity) = signbit(x) && !signbit(y)
        $op(x::RealInfinity, y::ComplexInfinity{Bool}) = signbit(x) && !signbit(y)
    end
end

for Typ in (Number, Complex, AbstractIrrational, BigFloat, ComplexInfinity, BigInt, Rational)
    @eval begin
        ==(x::Infinity, y::$Typ) = isinf(y) && angle(y) == angle(x)
        ==(x::RealInfinity, y::$Typ) = isinf(y) && angle(y) == angle(x)
        ==(y::$Typ, x::RealInfinity) = x == y
        ==(y::$Typ, x::Infinity) = x == y
        ==(::InfiniteCardinal, y::$Typ) = ∞ == y
        ==(x::$Typ, ::InfiniteCardinal) = x == ∞
    end
end
==(::InfiniteCardinal, y::RealInfinity) = ∞ == y
==(x::RealInfinity, ::InfiniteCardinal) = x == ∞

const RealInfinityList = (Infinity, RealInfinity, InfiniteCardinal, ComplexInfinity{Bool})
for T1 in RealInfinityList
    for T2 in RealInfinityList
        @eval mod(::$T1, ::$T2) = NotANumber()
        @eval -(x::$T1, y::$T2) = x + (-y)
    end
    for T2 in (Complex, ComplexInfinity, Real, Rational, Complex{Bool}, Number)
        @eval begin
            -(x::$T1, y::$T2) = x + (-y)
            -(x::$T2, y::$T1) = x + (-y)
        end
    end
end
