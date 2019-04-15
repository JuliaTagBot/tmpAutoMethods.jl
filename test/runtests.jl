using Test
using AutoMethods: @auto, auto

@testset "throw on non-functions" begin
    for e = (:(f[] = 1),
             :(f = 1),
             :(macro(x) end),
             :(f(x)),
             )
        @test_throws ArgumentError auto(e)
    end
end

@auto f1(x:=1, y) = (x, y)
@auto function g1(x:=1, y) return (x, y) end

@auto f2(x::Int:=1, y:='a') = (x, y)
@auto function g2(x::Int:=1, y:='a') return (x, y) end

@auto f3(::Type{Int}, x:=1) = (Int, x)
@auto function g3(::Type{Int}, x:=1) return (Int, x) end

@auto f4(::Type{Int}:=Int, x:=1) = (Int, x)
@auto function g4(::Type{Int}:=Int, x:=1) return (Int, x) end

@auto f5(x::Bool:=false, y=0) = (x, y)
@auto function g5(x::Bool:=false, y=0) return (x, y) end

@auto f6(x::Bool:=false, y...) = (x, y)
@auto function g6(x::Bool:=false, y...) return (x, y) end

@auto f7(x::Bool:=false, y::Int...) = (x, y)
@auto function g7(x::Bool:=false, y::Int...) return (x, y) end

@testset "default values with :=" begin
    for (h1, h2, h3, h4, h5, h6, h7) = ((f1, f2, f3, f4, f5, f6, f7),
                                        (g1, g2, g3, g4, g5, g6, g7))
        @test h1(1, 2) == (1, 2)
        @test h1(2)    == (1, 2)

        @test h2(1, 2) == (1, 2)
        @test h2('q')  == (1, 'q')
        @test h2(1)    == (1, 'a')
        @test h2()     == (1, 'a')

        @test h3(Int, 2) == (Int, 2)
        @test h3(Int)    == (Int, 1)
        @test h4(Int, 2) == (Int, 2)
        @test h4(Int)    == (Int, 1)
        @test h4(2)      == (Int, 2)
        @test h4()       == (Int, 1)

        @test h5(true, 1) == (true, 1)
        @test h5(true)    == (true, 0)
        @test h5(1)       == (false, 1)
        @test h5()        == (false, 0)

        for hh = (h6, h7)
            @test hh(true, 1, 2) == (true, (1, 2))
            @test hh(true)       == (true, ())
            @test hh(1, 2)       == (false, (1, 2))
            @test hh()           == (false, ())
        end
    end
end
