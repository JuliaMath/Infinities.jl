@inline infpromote(x, y) = Base._promote(x, y)
@inline infpromote(x::ExtendedComplex, y::AllInfinities) = (x, ComplexInfinity(y))
@inline infpromote(x::ExtendedComplex, y::ComplexInfinity) = Base._promote(x, y)
@inline infpromote(x::Real, ::InfiniteCardinal) = (x, ∞)
@inline infpromote(x::Integer, y::InfiniteCardinal) = (x, y)
@inline infpromote(x::RealInfinity, y::Union{Integer, Rational}) = (x, float(y))
@inline infpromote(x::Union{Integer, Rational}, y::RealInfinity) = (float(x), y)
@inline infpromote(x::RealInfinity, ::InfiniteCardinal) = (x, ∞)


# sign
+(::Infinity) = RealInfinity()
-(::Infinity) = RealInfinity(true)
-(y::RealInfinity) = RealInfinity(!signbit(y))
-(y::ComplexInfinity{B}) where B<:Integer = sign(y) == 1 ? ComplexInfinity(one(B)) : ComplexInfinity(zero(B))
+(x::InfiniteCardinal) = x
-(::InfiniteCardinal) = -∞


# addition
@inline toinf(x) = RealInfinity(signbit(x))
@inline toinf(x::Complex) = ComplexInfinity(angle(x))
@inline toinf(x::ComplexInfinity) = x

@inline _infadd(x, y) = angle(x) == angle(y) ? y : throw(ArgumentError("Angles must be the same to add ∞"))

@inline __add(x, y::AllInfinities) = isinf(x) ? _infadd(toinf(x), y) : y
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


# mod
@inline function _mod(x::Real, y::IntegerInfinities)
    signbit(x) == signbit(y) || throw(ArgumentError("mod($x,$y) is unbounded"))
    x
end
mod(x::Real, y::IntegerInfinities) = _mod(x, y)
mod(::IntegerInfinities, ::Real) = NotANumber()
mod(::IntegerInfinities, ::IntegerInfinities) = NotANumber()

# fld, cld, div
_divinf(T) = zero(T)
_fldinf(x) = signbit(x) ? -one(x) : zero(x)
_cldinf(x) = signbit(x) ? zero(x) : one(x)
div(::T, ::IntegerInfinities) where T <: Real = _divinf(T)
fld(x::Real, ::IntegerInfinities) = _fldinf(x)
cld(x::Real, ::IntegerInfinities) = _cldinf(x)

_inffcd(x, y) = signbit(y) ? -x : x
for OP in (:fld,:cld,:div)
    @eval begin
        $OP(x::IntegerInfinities, y::Real) = _inffcd(x, y)
        $OP(::IntegerInfinities, ::IntegerInfinities) = NotANumber()
    end
end

# power
# Although the base implementation can cover these cases, it can change overtime and yield inconsistent results.
# ref: https://github.com/JuliaMath/Infinities.jl/actions/runs/19993302836/
_infpow(::PositiveInfinity, p) = ifelse(iszero(p), one(p), ifelse(p > 0, +∞, +zero(p)))
function _infpow(x::NegativeInfinity, p)
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