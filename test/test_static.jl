using Static: Static, static, dynamic, is_static, known, eq, lt, True, False

@testset "Static extension" begin
    @test !isnothing(Base.get_extension(Infinities, :InfinitiesStaticExt))
    @test isempty(Test.detect_ambiguities(Infinities, Static, Base.get_extension(Infinities, :InfinitiesStaticExt)))
    singletons = (∞, +∞, -∞, ℵ₀, ℵ₁, InfiniteCardinal{2}(), NotANumber())
    for value in (singletons..., (∞, -∞, ℵ₀))
        for op in (static, dynamic)
            @test @inferred(op(value)) === value
        end
        for input in (value, typeof(value))
            @test @inferred(is_static(input)) === True()
            @test @inferred(known(input)) === value
        end
    end
    for (op, reference) in ((eq, ==), (lt, <), (+, +), (*, *)),
        first in (∞, -∞, NotANumber(), static(0), static(2), static(NaN), static(Inf)), second in singletons

        @test @inferred(op(first, second)) === static(reference(dynamic(first), second))
    end
    @test @inferred((-∞)^static(2)) === +∞
    for Typ in (RealInfinity, ComplexInfinity)
        @test is_static(Typ) === False()
        @test isnothing(known(Typ))
    end
    @test_throws ErrorException static(ComplexInfinity())

    operators = (+, -, *, /, div, fld, cld, mod, rem, divrem, min, max, isless, ==, <, <=, >, >=, isequal)
    for inf in (∞, +∞, -∞, ℵ₀, ℵ₁, NotANumber(), ComplexInfinity(), im*∞),
        value in (false, true, 0, 2, -2, 0.0, -0.0, 1.5, -1.5, NaN, Inf, -Inf),
        args in ((inf, static(value)), (static(value), inf))

        unsupported = inf isa ComplexInfinity ?
            (div, fld, cld, mod, rem, divrem, isless, (isnan(value) ? () : (min, max, <, <=, >, >=))...) : ()
        unbounded = args[2] === inf && inf isa Union{Infinities.Infinity, RealInfinity, InfiniteCardinal} &&
                    !isnan(value) && signbit(value) != signbit(inf) ? (mod,) : ()
        invalid = inf isa InfiniteCardinal && value isa Integer && value < 0 ? (unbounded..., *) : unbounded
        for op in setdiff(operators, unsupported, invalid)
            @test isequal(op(args...), op(map(dynamic, args)...))
        end
        for (exception, ops) in ((MethodError, unsupported), (ArgumentError, invalid)), op in ops
            @test_throws exception op(args...)
        end
    end
    for inf in (+∞, -∞), exponent in (false, true, -2, 0, 2, 3, -1.5, 1.5, NaN, Inf)
        if inf isa NegativeInfinity && !isnan(exponent) && !isinteger(exponent)
            @test_throws DomainError inf^static(exponent)
        else
            @test isequal(inf^static(exponent), inf^exponent)
        end
    end
end