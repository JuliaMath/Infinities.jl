@inline infpromote(x, y) = Base._promote(x, y)
@inline infpromote(x::ExtendedComplex, y::AllInfinities) = (x, ComplexInfinity(y))
@inline infpromote(x::ExtendedComplex, y::ComplexInfinity) = Base._promote(x, y)
@inline infpromote(x::Real, ::InfiniteCardinal) = (x, ∞)
@inline infpromote(x::Integer, y::InfiniteCardinal) = (x, y)
@inline infpromote(x::RealInfinity, y::Union{Integer, Rational}) = (x, float(y))
@inline infpromote(x::Union{Integer, Rational}, y::RealInfinity) = (float(x), y)
@inline infpromote(x::RealInfinity, ::InfiniteCardinal) = (x, ∞)
# `Base` promotes every `Real` to `BigFloat`, which would convert the infinity away.
@inline infpromote(x::BigFloat, y::Union{Infinity,RealInfinity}) = (x, y)
@inline infpromote(x::Union{Infinity,RealInfinity}, y::BigFloat) = (x, y)


# sign
+(::Infinity) = RealInfinity()
-(::Infinity) = RealInfinity(true)
-(y::RealInfinity) = RealInfinity(!signbit(y))
-(y::ComplexInfinity{B}) where B<:Integer = sign(y) == 1 ? ComplexInfinity(one(B)) : ComplexInfinity(zero(B))
-(y::ComplexInfinity) = ComplexInfinity(mod(y.signbit + 1, 2))
+(x::InfiniteCardinal) = x
-(::InfiniteCardinal) = -∞


# addition
@inline toinf(x) = RealInfinity(signbit(x))
@inline toinf(x::Complex) = ComplexInfinity(angle(x))
@inline toinf(x::ComplexInfinity) = x

@inline _infadd(x, y) = angle(x) == angle(y) ? y : NotANumber()

@inline __add(x, y::AllInfinities) = isinf(x) ? _infadd(toinf(x), y) : y
@inline __add(x::Integer, y::InfiniteCardinal) = max(x, y)

# A `NaN` argument makes the result undefined. Types with no `NaN` fold the test away.
@inline _add(x, y) = isnan(x) ? NotANumber() : __add(infpromote(x, y)...)

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
@inline _sb(x::ComplexInfinity) = x.signbit # the whole angle, not just its sign

@inline __mul(x, y::AllInfinities) = RealInfinity(_sb(x) ⊻ _sb(y))
@inline __mul(x, y::ComplexInfinity) = ComplexInfinity(_sb(x) + _sb(y))
@inline __mul(x, y::ComplexInfinity{Bool}) = ComplexInfinity(_sb(x) ⊻ _sb(y))
@inline __mul(x::Complex, y::ComplexInfinity{Bool}) = ComplexInfinity(_sb(x) + _sb(y))
@inline __mul(x::Integer, y::InfiniteCardinal) = x > 0 ? y : throw(ArgumentError("Cannot multiply $x * $y"))

@inline function _mul(x, y)
    isnan(x) && return NotANumber()
    iszero(x) && return NotANumber()
    __mul(infpromote(x, y)...)
end

*(x::Number, y::AllInfinities) = _mul(x, y)
*(x::AllInfinities, y::Number) = _mul(y, x)
*(x::AllInfinities, y::AllInfinities) = _mul(x, y)
*(x::InfiniteCardinal, y::InfiniteCardinal) = max(x, y)

# just conventions somehow
*(::Infinity, y::InfiniteCardinal) = y
*(x::InfiniteCardinal, ::Infinity) = x
*(::Infinity, ::Infinity) = ∞


# division
# `\` needs nothing of its own, `Base` defining it as `y / x`.
@inline _div(x, y) = x * inv(y)

/(x::AllInfinities, y::Number) = _div(x, y)
/(x::Number, y::AllInfinities) = _div(x, y)
/(x::AllInfinities, y::AllInfinities) = NotANumber()

# mod
@inline function _mod(x::Real, y::IntegerInfinities)
    isnan(x) && return NotANumber()
    signbit(x) == signbit(y) || throw(ArgumentError("mod($x,$y) is unbounded"))
    x
end
mod(x::Real, y::IntegerInfinities) = _mod(x, y)
mod(::IntegerInfinities, ::Real) = NotANumber()
mod(::IntegerInfinities, ::IntegerInfinities) = NotANumber()

# rem, divrem
# `rem` keeps the sign of the dividend, so unlike `mod` it stays bounded either way.
rem(x::Real, ::IntegerInfinities) = isnan(x) ? NotANumber() : x
rem(::IntegerInfinities, ::Real) = NotANumber()
rem(::IntegerInfinities, ::IntegerInfinities) = NotANumber()
# `Base` computes the remainder of two `Integer`s as `a - div(a,b)*b`, which an `InfiniteCardinal` cannot evaluate.
divrem(x::Real, y::IntegerInfinities) = (div(x, y), rem(x, y))
divrem(x::IntegerInfinities, y::Real) = (div(x, y), rem(x, y))
divrem(x::IntegerInfinities, y::IntegerInfinities) = (div(x, y), rem(x, y))

# fld, cld, div
_divinf(x) = isnan(x) ? NotANumber() : zero(x)
_fldinf(x) = isnan(x) ? NotANumber() : signbit(x) ? -one(x) : zero(x)
_cldinf(x) = isnan(x) ? NotANumber() : signbit(x) ? zero(x) : one(x)
div(x::Real, ::IntegerInfinities) = _divinf(x)
fld(x::Real, ::IntegerInfinities) = _fldinf(x)
cld(x::Real, ::IntegerInfinities) = _cldinf(x)

_inffcd(x, y) = isnan(y) ? NotANumber() : signbit(y) ? -x : x
for OP in (:fld,:cld,:div)
    @eval begin
        $OP(x::IntegerInfinities, y::Real) = _inffcd(x, y)
        $OP(::IntegerInfinities, ::IntegerInfinities) = NotANumber()
    end
end

# power
# Although the base implementation can cover these cases, it can change overtime and yield inconsistent results.
# ref: https://github.com/JuliaMath/Infinities.jl/actions/runs/19993302836/
_infpow(::PositiveInfinity, p) = isnan(p) ? NotANumber() : ifelse(iszero(p), one(p), ifelse(p > 0, +∞, +zero(p)))
function _infpow(x::NegativeInfinity, p)
    isnan(p) && return NotANumber()
    !isinteger(p) && throw(Base.Math.throw_exp_domainerror(x))
    iszero(p) && return one(p)
    isodd(p) && return ifelse(p > 0, -∞, -zero(p))
    return ifelse(p > 0, +∞, +zero(p))
end
^(x::RealInfinity, p::Real) = _infpow(infpromote(x, p)...)
^(x::RealInfinity, p::Integer) = _infpow(infpromote(x, p)...)

# inv
inv(::Union{Infinity,InfiniteCardinal}) = 0
inv(x::RealInfinity) = inv(float(x))
inv(x::ComplexInfinity) = zero(ComplexF64)


# NotANumber
# Anything computed from an undefined value is undefined again, as it is for `NaN`.
for op in (:+, :-, :*, :/, :^, :div, :fld, :cld, :mod, :rem, :min, :max)
    for Typ in NotANumberRivals
        @eval $op(x::NotANumber, ::$Typ) = x
        @eval $op(::$Typ, y::NotANumber) = y
    end
    for Typ in NotANumberComplexRivals
        @eval $op(::NotANumber, ::$Typ) = complex(NotANumber(), NotANumber())
        @eval $op(::$Typ, ::NotANumber) = complex(NotANumber(), NotANumber())
    end
    @eval $op(x::NotANumber, ::NotANumber) = x
end
for Typ in NotANumberRivals
    @eval divrem(x::NotANumber, ::$Typ) = (x, x)
    @eval divrem(::$Typ, y::NotANumber) = (y, y)
end
divrem(x::NotANumber, ::NotANumber) = (x, x)
# `Base` has its own `^(::Number, ::Integer)`, which a literal exponent also routes through.
^(x::NotANumber, ::Integer) = x
^(::Integer, y::NotANumber) = y
^(x::NotANumber, ::Rational) = x
^(::Irrational{:ℯ}, y::NotANumber) = y
for f in (:+, :-, :abs, :inv, :sign, :conj)
    @eval $f(x::NotANumber) = x
end