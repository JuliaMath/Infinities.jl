using Dates: Dates, Year, Quarter, Month, Week, Day, Hour, Minute, Second,
             Millisecond, Microsecond, Nanosecond, Date, DateTime, Time

struct Meters
    value::Rational{Int}
end
Base.isless(a::Meters, b::Meters) = isless(a.value, b.value)
Base.zero(::Type{Meters}) = Meters(0)
Infinities.@archimedean_magnitude ±∞ Meters

# bounded below by absolute zero
struct Kelvin
    value::Rational{Int}
end
Base.isless(a::Kelvin, b::Kelvin) = isless(a.value, b.value)
Base.zero(::Type{Kelvin}) = Kelvin(0)
Infinities.@archimedean_magnitude +∞ Kelvin

# bounded above by zero
struct Depth
    value::Rational{Int}
end
Base.isless(a::Depth, b::Depth) = isless(a.value, b.value)
Base.zero(::Type{Depth}) = Depth(0)
Infinities.@archimedean_magnitude -∞ Depth

struct TaggedInfinity <: RealInfinity
    negative::Bool
    # The default constructor accepting `Any` is ambiguous with `(::Type{<:Real})(::RealInfinity)`.
    TaggedInfinity(negative::Bool) = new(negative)
end
Base.signbit(x::TaggedInfinity) = x.negative

@testset "Archimedean types" begin
    @testset "declared with the macro" begin
        @test Meters(1) < Meters(2)
        @test -∞ < Meters(3) < ∞ && Meters(3) < +∞ && Meters(3) ≤ ∞ && ∞ ≥ Meters(3)
        @test !(∞ < Meters(3)) && !(Meters(3) < -∞) && Meters(3) ≠ ∞
        @test max(Meters(3), ∞) ≡ ∞ && min(Meters(3), -∞) ≡ -∞ && min(Meters(3), ∞) ≡ Meters(3)
        @test isequal(sort([∞, Meters(2), -∞, Meters(1)]), [-∞, Meters(1), Meters(2), ∞])
        @test Meters(3) + ∞ ≡ ∞ + Meters(3) ≡ ∞ - Meters(3) ≡ ∞ && Meters(3) - ∞ ≡ -∞
        @test Meters(3) - (-∞) ≡ +∞ && -∞ + Meters(3) ≡ Meters(3) + (-∞) ≡ -∞
        # a cardinal counts elements, it is no end of a scale
        @test_throws MethodError Meters(3) < ℵ₀
    end

    @testset "bounded on one side" begin
        for value in (Kelvin(300), Depth(-5))
            @test -∞ < value < ∞
        end

        for (value, end_infinity) in ((Kelvin(300), ∞), (Kelvin(300), +∞), (Depth(-5), -∞))
            @test value + end_infinity ≡ end_infinity + value ≡ end_infinity - value ≡ end_infinity
        end
        for (value, subtrahend, result) in (
            (Kelvin(300), -∞, +∞), (Depth(-5), ∞, -∞), (Depth(-5), +∞, -∞),
        )
            @test value - subtrahend ≡ result
        end

        # Operations producing the other infinity throw a `MethodError`.
        for (value, end_infinity) in ((Kelvin(300), +∞), (Depth(-5), -∞)),
            infinity in (end_infinity, TaggedInfinity(signbit(end_infinity))),
            operation in (
                (scale_value, infinity) -> scale_value - infinity,
                (scale_value, infinity) -> scale_value + (-infinity),
                (scale_value, infinity) -> -infinity + scale_value,
                (scale_value, infinity) -> -infinity - scale_value,
            )

            @test_throws MethodError operation(value, infinity)
        end
        for (value, end_infinity) in ((Kelvin(300), +∞), (Depth(-5), -∞))
            tagged, opposite = TaggedInfinity(signbit(end_infinity)), TaggedInfinity(!signbit(end_infinity))
            @test value + tagged ≡ tagged + value ≡ tagged - value ≡ value - opposite ≡ end_infinity
            @test_throws MethodError value - tagged
            @test_throws MethodError value + opposite
            @test_throws MethodError opposite + value
            @test_throws MethodError opposite - value
        end
        @test_throws ArgumentError macroexpand(@__MODULE__, :(Infinities.@archimedean Kelvin +∞))
        @test_throws ArgumentError macroexpand(@__MODULE__, :(Infinities.@archimedean ∞ Kelvin))
        @test_throws ArgumentError macroexpand(@__MODULE__, :(Infinities.@archimedean +∞))
        @test_throws ArgumentError macroexpand(@__MODULE__, :(Infinities.@archimedean_magnitude +∞))
        @test_throws ArgumentError macroexpand(@__MODULE__, :(Infinities.@archimedean_magnitude Kelvin -∞))
    end

    @testset "magnitudes" begin
        @test Meters(3) * ∞ ≡ ∞ * Meters(3) ≡ Meters(-3) * -∞ ≡ +∞
        @test Meters(-3) * ∞ ≡ -∞ * Meters(3) ≡ Meters(3) * -∞ ≡ -∞
        @test Meters(0) * ∞ ≡ -∞ * Meters(0) ≡ NotANumber()
        @test Meters(3) / ∞ ≡ Meters(-3) / -∞ ≡ zero(Meters)
        @test Meters(-3) ÷ ∞ ≡ fld(Meters(-3), ∞) ≡ cld(Meters(3), -∞) ≡ zero(Meters)
        @test div(Meters(3), ∞, RoundNearest) ≡ div(Meters(-3), -∞, RoundUp) ≡ zero(Meters)
        @test_throws MethodError ∞ / Meters(3)
        @test rem(Meters(-3), ∞) ≡ rem(Meters(-3), -∞) ≡ mod(Meters(-3), -∞) ≡ Meters(-3)
        @test mod(Meters(3), ∞) ≡ Meters(3) && mod(Meters(0), -∞) ≡ Meters(0)
        @test divrem(Meters(-3), ∞) ≡ (zero(Meters), Meters(-3))
        @test fldmod(Meters(3), ∞) ≡ (zero(Meters), Meters(3))
        @test_throws ArgumentError mod(Meters(-3), ∞)
        @test_throws ArgumentError mod(Meters(3), -∞)
        @test_throws ArgumentError fldmod(Meters(-3), ∞)

        @test Kelvin(300) * ∞ ≡ +∞ * Kelvin(300) ≡ +∞ && Kelvin(0) * ∞ ≡ NotANumber()
        @test Depth(-5) * ∞ ≡ +∞ * Depth(-5) ≡ -∞
        @test Kelvin(300) / ∞ ≡ zero(Kelvin) && Depth(-5) / +∞ ≡ zero(Depth)
        @test fld(Kelvin(300), ∞) ≡ zero(Kelvin) && fld(Depth(-5), +∞) ≡ zero(Depth)
        @test mod(Kelvin(300), ∞) ≡ Kelvin(300) && rem(Depth(-5), +∞) ≡ Depth(-5)
        @test_throws ArgumentError mod(Depth(-5), ∞)
        # a negative factor leaves a scale bounded at zero
        for value in (Kelvin(300), Depth(-5)), negative in (-∞, TaggedInfinity(true))
            @test_throws MethodError value * negative
            @test_throws MethodError negative * value
            @test_throws MethodError value / negative
            @test_throws MethodError value ÷ negative
            @test_throws MethodError rem(value, negative)
            @test_throws MethodError mod(value, negative)
        end
        positive = TaggedInfinity(false)
        @test Kelvin(300) * positive ≡ positive * Kelvin(300) ≡ +∞ && Depth(-5) * positive ≡ -∞
        @test Kelvin(300) / positive ≡ Kelvin(300) ÷ positive ≡ zero(Kelvin)
        @test rem(Depth(-5), positive) ≡ Depth(-5) && mod(Kelvin(300), positive) ≡ Kelvin(300)
        @test_throws ArgumentError mod(Depth(-5), positive)
    end

    @testset "Dates extension" begin
        ext = Base.get_extension(Infinities, :InfinitiesDatesExt)
        @test !isnothing(ext)
        @test isempty(Test.detect_ambiguities(Infinities, Dates, ext))
        @test Year(1) > Quarter(1) > Month(1)
        @test Week(1) > Day(1) > Hour(1)
        @test_throws MethodError Year(1) < Week(1)
        for x in (Year(14), Quarter(2), Month(5), Week(2), Day(-3), Hour(14), Minute(30),
                  Second(1), Millisecond(5), Microsecond(2), Nanosecond(1),
                  Date(2026, 9, 25), DateTime(2026, 9, 25, 12))
            @test -∞ < x < ∞ && x < +∞ && !(∞ ≤ x) && !(x ≤ -∞)
            @test max(x, ∞) ≡ ∞ && min(x, -∞) ≡ -∞ && isequal(min(x, ∞), x)
            @test x + ∞ ≡ ∞ + x ≡ ∞ - x ≡ ∞ && x - ∞ ≡ -∞ + x ≡ -∞
        end
        @test Day(-3) * -∞ ≡ ∞ * Year(14) ≡ -∞ * Hour(-2) ≡ +∞ * Nanosecond(1) ≡ +∞
        @test Month(5) / ∞ ≡ Month(0) && Second(0) * ∞ ≡ NotANumber()
        @test Day(3) ÷ ∞ ≡ fld(Day(-3), ∞) ≡ cld(Day(3), ∞) ≡ Day(0)
        @test rem(Day(-3), ∞) ≡ mod(Day(-3), -∞) ≡ Day(-3) && divrem(Day(3), ∞) ≡ (Day(0), Day(3))
        # a position cannot be scaled
        @test_throws MethodError Date(2026, 9, 25) * ∞
        @test_throws MethodError DateTime(2026, 9, 25, 12) / ∞
        # a time of day wraps at midnight
        @test_throws MethodError Time(12) < ∞
    end
end
