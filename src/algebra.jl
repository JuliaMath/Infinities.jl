for Typ in (Base.TwicePrecision, AbstractChar, Complex)
    @eval begin
        RealInfinity(x::$Typ) = throw(MethodError(RealInfinity, x))
        ComplexInfinity{T}(x::$Typ) where T<:Real = throw(MethodError(ComplexInfinity{T}, x))
    end
end
ComplexInfinity{T}(x::ComplexInfinity{T}) where T<:Real = x

# addition
@inline toinf(x) = RealInfinity(signbit(x))
@inline toinf(x::Complex) = ComplexInfinity(angle(x))
@inline _infadd(x, y) = angle(x) == angle(y) ? max(x, y) : throw(ArgumentError("Angles must be the same to add ∞"))
@inline __add(x, y::AllInfinities) = isinf(x) ? _infadd(toinf(x), y) : y
@inline __add(x, ::InfiniteCardinal) = x + ∞
@inline __add(x::Integer, y::InfiniteCardinal) = max(x, y)
@inline _add(x, y) = __add(infpromote(x, y)...)
+(x::Number, y::AllInfinities) = _add(x, y)
+(x::AllInfinities, y::Number) = _add(y, x)
+(x::AllInfinities, y::AllInfinities) = _add(x, y)
+(x::InfiniteCardinal, y::AllInfinities) = _add(y, x)

for T1 in allinfinitylist
    for T2 in allinfinitylist
        @eval mod(::$T1, ::$T2) = NotANumber()
        @eval -(x::$T1, y::$T2) = x + (-y)
    end
    for T2 in (Complex, Real, Rational, Complex{Bool}, Number)
        @eval begin
            mod(::$T1, ::$T2) = NotANumber()
            function mod(x::$T2, y::$T1)
                signbit(x) == signbit(y) || throw(ArgumentError("mod($x,$y) is unbounded"))
                x
            end
            -(x::$T1, y::$T2) = x + (-y)
            -(x::$T2, y::$T1) = x + (-y)
        end
    end
end
mod(::InfiniteCardinal{0}, ::InfiniteCardinal{0}) = NotANumber()
