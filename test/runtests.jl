using Test
using InteractiveUtils: @which
using AutoMethods: @auto, auto

x ≜ y = typeof(x) == typeof(y) && x == y

@testset "throw on non-functions" begin
    for e = (:(f[] = 1),
             :(f = 1),
             :(macro(x) end),
             :(f(x)),
             )
        @test_throws ArgumentError auto(@__MODULE__, e)
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

# correct handling of multiple where clauses
@auto f8(x::X, y::Y := 1) where X where Y = (X, Y)
@auto function g8(x::X, y::Y := 1) where X where Y return (X, Y) end

# correct handling of conditions in where clauses
@auto f9(x::X:=true, y::Y:=1) where {X<:Bool, Int<:Y<:Signed} = (X,Y)
@auto function g9(x::X:=true, y::Y:=1) where {X<:Bool, Int<:Y<:Signed} return (X,Y) end

@testset "default values with :=" begin
    for (h1, h2, h3, h4, h5, h6, h7, h8, h9) = ((f1, f2, f3, f4, f5, f6, f7, f8, f9),
                                                (g1, g2, g3, g4, g5, g6, g7, g8, g9))
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

        @test length(methods(h8)) == 2
        @test h8(1) == (Int, Int)
        @test h8(0x1, 0x2) == (UInt8, UInt8)

        @test h9(false, 2) == (Bool, Int)
        @test h9(2)        == (Bool, Int)
        @test h9(false)    == (Bool, Int)
        @test h9()         == (Bool, Int)
    end
end


@auto          p1(x::X) where {X=(UInt,Int)} =      x => X
@auto function q1(x::X) where {X=(UInt,Int)} return x => X end

@auto          p2(x::X := 1, y::Vector{Y}) where {X=[UInt,Int],Y <: Union{Int,UInt}} =      (x, y)
@auto function q2(x::X := 1, y::Vector{Y}) where {X=[UInt,Int],Y <: Union{Int,UInt}} return (x, y) end

@auto          p3(x::X := 1, y::DataType := typeof(x)) where {X=(Int, UInt)} =      (x, y)
@auto function q3(x::X := 1, y::DataType := typeof(x)) where {X=(Int, UInt)} return (x, y) end

@auto          p4(x::T) where {X<:Integer, T=(X, Type{X}, Float64)} =      x
@auto function q4(x::T) where {X<:Integer, T=(X, Type{X}, Float64)} return x end

@auto          p5(x::(X, Type{X}, Float64)) where {X<:Integer} =      x
@auto function q5(x::(X, Type{X}, Float64)) where {X<:Integer} return x end

@auto          p6(x::(Int,UInt)=1) =      x
@auto function q6(x::(Int,UInt)=1) return x end

@auto          p7(x::(Int,UInt8)...) =      x
@auto function q7(x::(Int,UInt8)...) return x end

@testset "type lists" begin
    for (r1, r2, r3, r4, r5, r6, r7) = ((p1, p2, p3, p4, p5, p6, p7),
                                        (q1, q2, q3, q4, q5, q6, q7))

        @test length(methods(r1)) == 2
        @test r1(1)       == (1 => Int)
        @test r1(UInt(1)) == (1 => UInt)
        @test_throws MethodError r1(0x1)
        @test_throws MethodError r1(true)

        @test length(methods(r2)) == 3
        @test r2([1])             ≜ (1, [1])
        @test r2(UInt[1])         ≜ (1, UInt[1])
        @test r2(UInt(1), [1])    ≜ (UInt(1), [1])
        @test r2(Int(1), UInt[1]) ≜ (1, UInt[1])

        @test length(methods(r3)) == 6
        @test r3(UInt(2), Bool) ≜ (UInt(2), Bool)
        @test r3(Bool)          ≜ (1, Bool)
        @test r3(2)             ≜ (2, Int)
        @test r3()              ≜ (1, Int)

        for rr = (r4, r5)
            @test length(methods(rr)) == 3
            @test rr(Int8(2)) === Int8(2)
            @test rr(UInt)    === UInt
            @test rr(1.2)     === 1.2
            @test  isa((@which rr(1)).sig,   UnionAll)
            @test !isa((@which rr(1.2)).sig, UnionAll) # check that 'where X' has been removed
            @test_throws MethodError rr(Float64)
        end

        @test length(methods(r6)) == 3
        @test r6(2) == r6(UInt(2)) == 2
        @test r6()  === 1

        @test r7(1, 2)     === (1, 2)
        @test r7(0x1, 0x2) === (0x1, 0x2)
        @test_throws MethodError r7(1, 0x2) # could change
    end
end
