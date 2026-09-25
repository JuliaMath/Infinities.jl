module InfinitiesStaticExt

using Infinities: Infinity, PositiveInfinity, NegativeInfinity, InfiniteCardinal, NotANumber
using Infinities: AllInfinities, AllRealInfinities, IntegerInfinities, OrderedInfinities
using Infinities: ComplexInfinity, RealInfinity
using Static: Static, dynamic, StaticNumber, StaticInteger, StaticFloat64, StaticInt, True

for Typ in (Infinity, PositiveInfinity, NegativeInfinity, NotANumber)
    @eval begin
        Static.static(x::$Typ) = x
        Static.is_static(::Type{$Typ}) = True()
        Static.known(::Type{$Typ}) = $Typ()
    end
end

Static.static(x::InfiniteCardinal) = x
Static.is_static(::Type{InfiniteCardinal{N}}) where {N} = True()
Static.known(::Type{InfiniteCardinal{N}}) where {N} = InfiniteCardinal{N}()

for (ops, Types, StaticTypes) in (
    ((:+, :-, :*, :/, :(==)), (AllInfinities,), (StaticNumber,)),
    ((:isequal,), (NotANumber,), (StaticNumber,)),
    ((:div, :fld, :cld, :divrem), (IntegerInfinities,), (StaticNumber,)),
    ((:<, :<=, :>, :>=), (OrderedInfinities,), (StaticInteger, StaticFloat64)),
    ((:isless,), (AllRealInfinities, InfiniteCardinal, NotANumber), (StaticInteger, StaticFloat64)),
    ((:min, :max), (OrderedInfinities, NotANumber), (StaticInteger, StaticFloat64)),
    ((:mod,), (IntegerInfinities, NotANumber), (StaticNumber,)),
    ((:rem,), (IntegerInfinities, NotANumber), (StaticInteger, StaticFloat64)),
    ((:*,), (InfiniteCardinal,), (StaticInt{0},)),
), op in ops, Typ in Types, StaticTyp in StaticTypes
    @eval Base.$op(x::$Typ, y::$StaticTyp) = $op(x, dynamic(y))
    @eval Base.$op(x::$StaticTyp, y::$Typ) = $op(dynamic(x), y)
end

Base.:^(x::RealInfinity, y::StaticNumber) = x^dynamic(y)

end