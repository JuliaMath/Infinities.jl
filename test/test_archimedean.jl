using Dates: Dates, Year, Quarter, Month, Week, Day, Hour, Minute, Second,
             Millisecond, Microsecond, Nanosecond, Date, DateTime, Time

struct Meters
    value::Rational{Int}
end
Base.isless(a::Meters, b::Meters) = isless(a.value, b.value)
Infinities.@archimedean ±∞ Meters

# bounded below by absolute zero
struct Kelvin
    value::Rational{Int}
end
Base.isless(a::Kelvin, b::Kelvin) = isless(a.value, b.value)
Infinities.@archimedean +∞ Kelvin

# bounded above by zero
struct Depth
    value::Rational{Int}
end
Base.isless(a::Depth, b::Depth) = isless(a.value, b.value)
Infinities.@archimedean -∞ Depth

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

        # Operations producing the other infinity have no method.
        for (value, end_infinity) in ((Kelvin(300), +∞), (Depth(-5), -∞)),
            operation in (
                (scale_value, infinity) -> scale_value - infinity,
                (scale_value, infinity) -> scale_value + (-infinity),
                (scale_value, infinity) -> -infinity + scale_value,
                (scale_value, infinity) -> -infinity - scale_value,
            )

            @test_throws MethodError operation(value, end_infinity)
        end
        @test_throws ArgumentError macroexpand(@__MODULE__, :(Infinities.@archimedean Kelvin +∞))
        @test_throws ArgumentError macroexpand(@__MODULE__, :(Infinities.@archimedean ∞ Kelvin))
        @test_throws ArgumentError macroexpand(@__MODULE__, :(Infinities.@archimedean +∞))
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
        # a time of day wraps at midnight
        @test_throws MethodError Time(12) < ∞
    end
end
