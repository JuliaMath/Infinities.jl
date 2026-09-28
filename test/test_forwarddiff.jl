using ForwardDiff: ForwardDiff, Dual, value, partials

@testset "ForwardDiff extension" begin
    @test isempty(Test.detect_ambiguities(Base.get_extension(Infinities, :InfinitiesForwardDiffExt)))
    same = (actual, expected) -> typeof(actual) === typeof(expected) && isequal(actual, expected)
    for inf in (∞, +∞, -∞, NotANumber()), Scalar in (Float32, Float64, BigFloat)
        for input in (0, 2, -2, Inf, -Inf, NaN), reverse in (false, true),
            op in (+, -, *, /, mod, rem, min, max)
            primal, tangents = Scalar(input), (Scalar(0), Scalar(1), Scalar(-1))
            dual = Dual(primal, tangents...)
            args = reverse ? (inf, dual) : (dual, inf)
            if op === mod && !reverse && !isnan(primal) && !isnan(inf) && signbit(primal) != signbit(inf)
                @test_throws ArgumentError mod(primal, inf)
                @test_throws ArgumentError op(args...)
                continue
            end
            expected = op((reverse ? (inf, primal) : (primal, inf))...)
            result = op(args...)
            @test same(value(result), expected)
            expected_partials = map(tangents) do tangent
                op === (+) && return tangent
                op === (-) && return reverse ? -tangent : tangent
                op === (*) && return tangent * inf
                op === (/) && return reverse ? -(expected / primal) * tangent : tangent / inf
                op in (mod, rem) && return reverse || isnan(expected) ? NotANumber() : tangent
                reference = op((reverse ? (Scalar(inf), dual) : (dual, Scalar(inf)))...)
                return partials(reference)[findfirst(isequal(tangent), tangents)]
            end
            @test all(same.(Tuple(partials(result)), expected_partials))
        end
        for op in (isless, isequal, ==, <, <=, >, >=), input in (2, Inf, -Inf, NaN), tangent in (0, 1)
            dual = Dual(Scalar(input), Scalar(tangent))
            @test op(dual, inf) === op(dual, float(inf))
            @test op(inf, dual) === op(float(inf), dual)
        end
        for constructor in (Dual, Dual{Nothing}, Dual{Nothing, Real}, Dual{Nothing, Real, 2})
            @test value(constructor(inf)) === inf
        end
        for constructor in (Dual{Nothing, Scalar}, Dual{Nothing, Scalar, 2})
            @test same(value(constructor(inf)), Scalar(inf)) && iszero(partials(constructor(inf)))
        end
    end
    for (op, expected) in ((*, +∞), (+, 1.0), (/, 0.0), (min, 1.0), (max, 0.0), (mod, 1.0), (rem, 1.0))
        @test ForwardDiff.derivative(input -> op(input, ∞), 2.0) === expected
    end
    @test value((Dual(2.0, 1.0) + ∞) + 3.0) === ∞
    @test ForwardDiff.derivative(input -> ForwardDiff.derivative(inner -> inner^2 + ∞, input), 2.0) === 2.0
    @test isequal(ForwardDiff.gradient(input -> input[1] * ∞, [2.0, 3.0]), [+∞, NotANumber()])
end