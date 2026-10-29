@testset "ambiguities" begin
    @test_throws InexactError Bool(∞)
    @test_throws InexactError Bool(+∞)
    @test_throws InexactError Bool(-∞)

    @test_throws MethodError RealInfinity(Base.TwicePrecision(1.0))
    @test_throws MethodError RealInfinity(im)
    @test ComplexInfinity(Base.TwicePrecision(1.0)) ≡ ComplexInfinity(-∞)
    @test_throws MethodError ComplexInfinity(im)

    for inf in (∞,+∞,ℵ₀)
        @test mod(inf, 1//2) ≡ NotANumber()
        @test mod(1//2, inf) ≡ 1//2
        @test fld(1//2, inf) == 0
        @test cld(1//2, inf) == 1
        @test div(1//2, inf) == 0
        @test fld(inf, 1//2) ≡ cld(inf, 1//2) ≡ div(inf, 1//2) == inf
        @test fld(inf, ∞) ≡ fld(inf, +∞) ≡ fld(inf, ℵ₀) ≡ NotANumber()
    end
    # `Base` defines no integer operation for a `Complex`, so neither does a `ComplexInfinity` take one
    for op in (mod, fld, cld, div)
        @test_throws MethodError op(ComplexInfinity(), 1//2)
        @test_throws MethodError op(1//2, ComplexInfinity())
    end

    @testset "rational power" begin
        # zero
        @test (+∞)^(0//1) ≡ (-∞)^(0//1) ≡ 1.0

        # positive even/odd/fraction
        @test (+∞)^(2//1) ≡ (-∞)^(2//1) ≡ +∞
        @test (+∞)^(1//1) ≡ +∞
        @test (-∞)^(1//1) ≡ -∞
        @test (+∞)^(1//2) ≡ +∞
        @test_throws DomainError (-∞)^(1//2)

        # negative even/odd/fraction
        @test (+∞)^(-2.0) ≡ (-∞)^(-2.0) ≡ 0.0
        @test (+∞)^(-1//1) ≡ 0.0
        @test (-∞)^(-1//1) ≡ -0.0
        @test (+∞)^(-1//2) ≡ 0.0
        @test_throws DomainError (-∞)^(-1//2)

        # irrational
        @test (+∞)^π ≡ +∞
        @test_throws DomainError (-∞)^π
    end
end