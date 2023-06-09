for Typ in (Base.TwicePrecision, AbstractChar, Complex)
    @eval begin
        RealInfinity(x::$Typ) = throw(MethodError(RealInfinity, x))
        ComplexInfinity{T}(x::$Typ) where T<:Real = throw(MethodError(ComplexInfinity{T}, x))
    end
end
ComplexInfinity{T}(x::ComplexInfinity{T}) where T<:Real = x

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
