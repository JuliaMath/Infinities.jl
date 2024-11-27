@testset "ambiguities" begin
    @test_throws InexactError Bool(∞)
    @test_throws InexactError Bool(+∞)
    @test_throws InexactError Bool(-∞)

    @test_throws MethodError RealInfinity(Base.TwicePrecision(1.0))
    @test_throws MethodError RealInfinity(im)
    @test ComplexInfinity{Float64}(Base.TwicePrecision(1.0)) == ComplexInfinity(1)
    @test_throws MethodError ComplexInfinity(im)

    for inf in (∞,+∞,ℵ₀,ComplexInfinity())
        @test mod(inf, 1//2) ≡ NotANumber()
        @test mod(1//2, inf) ≡ 1//2
        @test fld(1//2, inf) == 0
        @test cld(1//2, inf) == 1
        @test div(1//2, inf) == 0
        @test fld(inf, 1//2) ≡ cld(inf, 1//2) ≡ div(inf, 1//2) ≡ inf
        @test fld(inf, ∞) ≡ fld(inf, +∞) ≡ fld(inf, ℵ₀) ≡ fld(inf, ComplexInfinity()) ≡ NotANumber()
    end
end