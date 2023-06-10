for Typ in (Base.TwicePrecision, AbstractChar, Complex)
    @eval begin
        RealInfinity(x::$Typ) = throw(MethodError(RealInfinity, x))
        ComplexInfinity{T}(x::$Typ) where T<:Real = throw(MethodError(ComplexInfinity{T}, x))
    end
end
ComplexInfinity{T}(x::ComplexInfinity{T}) where T<:Real = x

# sign
+(::Infinity) = RealInfinity()
-(::Infinity) = RealInfinity(true)
-(y::RealInfinity) = RealInfinity(!y.signbit)
-(y::ComplexInfinity{B}) where B<:Integer = sign(y) == 1 ? ComplexInfinity(one(B)) : ComplexInfinity(zero(B))
+(x::InfiniteCardinal) = x
-(::InfiniteCardinal) = -∞

# addition
@inline toinf(x) = RealInfinity(signbit(x))
@inline toinf(x::Complex) = ComplexInfinity(angle(x))
@inline toinf(x::ComplexInfinity) = x
@inline _infadd(x, y) = angle(x) == angle(y) ? y : throw(ArgumentError("Angles must be the same to add ∞"))
@inline __add(x, y::AllInfinities) = isinf(x) ? _infadd(toinf(x), y) : y
@inline __add(x, ::InfiniteCardinal) = x + ∞
@inline __add(x::Integer, y::InfiniteCardinal) = max(x, y)
@inline _add(x, y) = __add(infpromote(x, y)...)
+(x::Number, y::AllInfinities) = _add(x, y)
+(x::AllInfinities, y::Number) = _add(y, x)
+(x::AllInfinities, y::AllInfinities) = _add(x, y)
+(x::InfiniteCardinal, y::AllInfinities) = _add(y, x)

# subtraction
@inline _sub(x, y) = x + (-y)
-(x::Number, y::AllInfinities) = _sub(x, y)
-(x::AllInfinities, y::Number) = _sub(x, y)
-(x::AllInfinities, y::AllInfinities) = _sub(x, y)

# multiplication
@inline _sb(x) = signbit(x)
@inline _sb(x::Complex) = angle(x)/π # overloading `signbit` causes type piracy 
@inline __mul(x, y::AllInfinities) = RealInfinity(_sb(x) ⊻ _sb(y))
@inline __mul(x, y::ComplexInfinity) = ComplexInfinity(_sb(x) + _sb(y))
@inline __mul(x, y::ComplexInfinity{Bool}) = ComplexInfinity(_sb(x) ⊻ _sb(y))
@inline __mul(x::Complex, y::ComplexInfinity{Bool}) = ComplexInfinity(_sb(x) + _sb(y))
@inline __mul(x::Integer, y::InfiniteCardinal) = x > 0 ? y : throw(ArgumentError("Cannot multiply $x * $y"))
@inline _mul(x, y) = iszero(x) ? throw(ArgumentError("Cannot multiply $x * $y")) : __mul(infpromote(x, y)...)
*(x::Number, y::AllInfinities) = _mul(x, y)
*(x::AllInfinities, y::Number) = _mul(y, x)
*(x::AllInfinities, y::AllInfinities) = _mul(x, y)
*(x::InfiniteCardinal, y::InfiniteCardinal) = max(x, y)
# just conventions somehow
*(::Infinity, y::InfiniteCardinal) = y
*(x::InfiniteCardinal, ::Infinity) = x
*(::Infinity, ::Infinity) = ∞

for T1 in allinfinitylist
    for T2 in allinfinitylist
        @eval mod(::$T1, ::$T2) = NotANumber()
    end
    for T2 in (Complex, Real, Rational, Complex{Bool}, Number)
        @eval begin
            mod(::$T1, ::$T2) = NotANumber()
            function mod(x::$T2, y::$T1)
                signbit(x) == signbit(y) || throw(ArgumentError("mod($x,$y) is unbounded"))
                x
            end
        end
    end
end
mod(::InfiniteCardinal{0}, ::InfiniteCardinal{0}) = NotANumber()
