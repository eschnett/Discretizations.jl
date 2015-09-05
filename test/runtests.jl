using Discretizations
using Base.Test



# typesame
@test typesame(Int,Int) === Int
@test_throws MethodError typesame(Int,Float64)
@test_throws MethodError typesame(Float64,Int)
@test_throws MethodError typesame(Integer,Int)
@test_throws MethodError typesame(Int,Integer)

# tupletypes
@test tupletypes(Tuple{}) === ()
@test tupletypes(Tuple{Int}) === (Int,)
@test tupletypes(Tuple{Int,Char}) === (Int,Char)
@test_throws MethodError tupletypes(1)
@test_throws MethodError tupletypes((1,2))
@test_throws MethodError tupletypes(Int)
@test_throws MethodError tupletypes((Int,Char))



# scalars

function test_Scalar(S::Type)
    z = zero(S)
    e = one(S)
    @test -z == z
    @test -(-e) == e
    @test z + z == z
    @test z + e == e
    @test e + z == e
    @test z * z == z
    @test z * e == z
    @test e * z == z
    @test e * e == e
    @test -e * -e == e
end
function test_Scalars()
    test_Scalar(Bool)
    test_Scalar(Int)
    test_Scalar(Float64)
    test_Scalar(Complex128)
    # test_Scalar(Matrix{Float64})
end
test_Scalars()



# AbstractVS

function test_AbstractVS()
    @test_throws ErrorException veltype(AbstractVS)
    @test_throws ErrorException veltype(AbstractVS{Int})
    @test_throws ErrorException vnewtype(AbstractVS, Float64)
    @test_throws ErrorException vnewtype(AbstractVS{Int}, Float64)
    @test_throws ErrorException vnull(AbstractVS)
    @test_throws ErrorException vnull(AbstractVS{Int})
end
test_AbstractVS()



# EmptyVS

function test_EmptyVS(S::Type)
    V = EmptyVS{S}
    @test veltype(V) === S
    @test veltype(vnewtype(V, Int)) === Int

    z = vnull(V)
    @test z == V()
    @test vdim(z) == zero(S)
    @test vscale(one(S), z) == z
    @test vadd(z, z) == z

    @test typeof(collect(z)) === Vector{S}
    @test collect(z) == []

    @test "$z" == "VS{$S}[]"

    @test done(z, start(z))
    @test_throws BoundsError next(z, start(z))
end
function test_EmptyVSs()
    # test_EmptyVS(Bool)
    test_EmptyVS(Int)
    test_EmptyVS(Float64)
    test_EmptyVS(Complex128)
end
test_EmptyVSs()



# ScalarVS

function test_ScalarVS(S::Type)
    V = ScalarVS{S}
    @test veltype(V) === S
    @test veltype(vnewtype(V, Int)) === Int

    z = vnull(V)
    @test z.elt == zero(S)
    @test vdim(z) == 1
    @test vscale(one(S), z) == z
    @test vadd(z, z) == z
    e = V(one(S))
    @test vscale(zero(S), e) == z
    @test vscale(one(S), e) == e
    @test vscale(2*one(S), e).elt == 2*one(S)
    @test vadd(z, e) == e
    @test vadd(e, z) == e
    @test vadd(e, e).elt == 2*one(S)

    @test collect(z) == S[zero(S)]
    @test collect(e) == S[one(S)]

    @test "$z" == "VS{$S}[$(zero(S))]"
end
function test_ScalarVSs()
    # test_ScalarVS(Bool)
    test_ScalarVS(Int)
    test_ScalarVS(Float64)
    test_ScalarVS(Complex128)
end
test_ScalarVSs()



# ProductVS

function test_ProductVS(S::Type, V1::Type, V2::Type, e1, e2)
    e1::V1, e2::V2
    V = ProductVS{S,V1,V2}
    @test veltype(V) === S
    @test veltype(vnewtype(V, Int)) === Int

    z = vnull(V)
    @test z.v1 == vnull(V1)
    @test z.v2 == vnull(V2)
    @test vdim(z) == vdim(vnull(V1)) + vdim(vnull(V2))

    @test vscale(one(S), z) == z
    @test vadd(z, z) == z
    e = V(e1, e2)
    @test vscale(zero(S), e) == z
    @test vscale(one(S), e) == e
    @test vscale(2*one(S), e).v1 == vscale(2, e1)
    @test vscale(2*one(S), e).v2 == vscale(2, e2)
    @test vadd(z, e) == e
    @test vadd(e, z) == e
    @test vadd(e, e).v1 == vadd(e1, e1)
    @test vadd(e, e).v2 == vadd(e2, e2)

    @test typeof(collect(z)) === Vector{S}
    @test collect(z) == [collect(vnull(V1)); collect(vnull(V2))]
    @test typeof(collect(e)) === Vector{S}
    @test collect(e) == [collect(e1); collect(e2)]

    @test "$z" == "VS{$S}[$(vnull(V1)),$(vnull(V2))]"
end
function test_ProductVSs()
    V0I = EmptyVS{Int}
    V1I = ScalarVS{Int}
    e0i = V0I()
    e1i = V1I(1)

    test_ProductVS(Int, V0I, V0I, e0i, e0i)
    test_ProductVS(Int, V0I, V1I, e0i, e1i)
    test_ProductVS(Int, V1I, V0I, e1i, e0i)
    test_ProductVS(Int, V1I, V1I, e1i, e1i)

    V2I = ProductVS{Int, V1I, V1I}
    e2i = V2I(e1i, e1i)
    test_ProductVS(Int, V2I, V2I, e2i, e2i)

    V1F = ScalarVS{Float64}
    e1f = V1F(1)
    test_ProductVS(Float64, V1F, V1F, e1f, e1f)

    V1C = ScalarVS{Complex128}
    e1c = V1C(1+im)
    test_ProductVS(Complex128, V1C, V1C, e1c, e1c)
end
test_ProductVSs()



# MultiProductVS

function test_MultiProductVS(S::Type, VS::Type, es::Tuple)
    es::VS
    V = MultiProductVS{S,VS}
    @test veltype(V) === S
    @test veltype(vnewtype(V, Int)) === Int

    z = vnull(V)
    @test z.vs == map(vnull, tupletypes(VS))
    @test vdim(z) == mapreduce(vdim, +, 0, map(vnull, tupletypes(VS)))

    @test vscale(one(S), z) == z
    @test vadd(z, z) == z
    e = V(es)
    @test vscale(zero(S), e) == z
    @test vscale(one(S), e) == e
    @test vscale(2*one(S), e).vs == map(x->vscale(2, x), es)
    @test vadd(z, e) == e
    @test vadd(e, z) == e
    @test vadd(e, e).vs == map(vadd, es, es)

    @test typeof(collect(z)) === Vector{S}
    @test collect(z) == vcat(map(collect, map(vnull, tupletypes(VS)))...)
    @test typeof(collect(e)) === Vector{S}
    @test collect(e) == vcat(map(collect, es)...)

    zs = join(map(string, map(vnull, tupletypes(VS))), ",")
    @test "$z" == "VS{$S}[$zs]"
end
function test_MultiProductVSs()
    V0I = EmptyVS{Int}
    V1I = ScalarVS{Int}
    e0i = V0I()
    e1i = V1I(1)

    test_MultiProductVS(Int, Tuple{V0I, V0I}, (e0i, e0i))
    test_MultiProductVS(Int, Tuple{V0I, V1I}, (e0i, e1i))
    test_MultiProductVS(Int, Tuple{V1I, V0I}, (e1i, e0i))
    test_MultiProductVS(Int, Tuple{V1I, V1I}, (e1i, e1i))

    test_MultiProductVS(Int, Tuple{}, ())
    test_MultiProductVS(Int, Tuple{V0I}, (e0i,))
    test_MultiProductVS(Int, Tuple{V1I}, (e1i,))
    test_MultiProductVS(Int, Tuple{V1I, V1I, V1I}, (e1i, e1i, e1i))

    # TODO
end
test_MultiProductVSs()



# PowerVS

function test_PowerVS(S::Type, V1::Type, D::Integer, n::Tuple, e1)
    e1::V1
    V = PowerVS{S,V1,D}
    @test veltype(V) === S
    @test veltype(vnewtype(V, Int)) === Int

    z = vnull(V)
    @test vdim(z) == 0^D * vdim(vnull(V1))
    z = vnull(V, ntuple(d->1,D))
    @test vdim(z) == vdim(vnull(V1))

    z = vnull(V, n)
    if prod(n)>0
        @test z.v1[1] == vnull(V1)
        @test z.v1[(end+1)÷2] == vnull(V1)
        @test z.v1[end] == vnull(V1)
    end
    @test vdim(z) == prod(n) * vdim(vnull(V1))
    @test vscale(one(S), z) == z
    @test vadd(z, z) == z

    e = V(deepcopy(z.v1))
    if prod(n)>0
        e.v1[1] = e1
    end
    @test vscale(zero(S), e) == z
    @test vscale(one(S), e) == e
    if prod(n)>0
        @test vscale(2*one(S), e).v1[1] == vscale(2, e1)
        if D>0
            @test vscale(2*one(S), e).v1[2] == vnull(V1)
            @test vscale(2*one(S), e).v1[end] == vnull(V1)
        end
    end
    @test vadd(z, e) == e
    @test vadd(e, z) == e
    if prod(n)>0
        @test vadd(e, e).v1[1] == vadd(e1, e1)
        if D>0
            @test vadd(e, e).v1[2] == vnull(V1)
            @test vadd(e, e).v1[end] == vnull(V1)
        end
    end

    @test typeof(collect(z)) === Vector{S}
    @test collect(z) == repeat(collect(vnull(V1)), outer=[prod(n)])
    @test typeof(collect(e)) === Vector{S}
    if prod(n)>0
        @test collect(e) == vcat(collect(e1),
                                 repeat(collect(vnull(V1)), outer=[prod(n)-1]))
    else
        @test collect(e) == []
    end

    zs = "$(vnull(V1))"
    if prod(n)>0
        @test "$z" == "VS{$S}[" * (zs*",")^(prod(n)-1) * zs * "]"
    else
        @test "$z" == "VS{$S}[]"
    end
end
function test_PowerVSs()
    # test_PowerVS(Bool)
    V0I = EmptyVS{Int}
    V1I = ScalarVS{Int}
    V2I = ProductVS{Int, V1I, V1I}
    e0i = V0I()
    e1i = V1I(1)
    e2i = V2I(e1i, e1i)

    for n in (0,3)
        test_PowerVS(Int, V0I, 1, (n,), e0i)
        test_PowerVS(Int, V1I, 1, (n,), e1i)
        test_PowerVS(Int, V0I, 1, (n,), e0i)
        test_PowerVS(Int, V1I, 1, (n,), e1i)
        test_PowerVS(Int, V2I, 0, (), e2i)
        test_PowerVS(Int, V2I, 1, (n,), e2i)
        test_PowerVS(Int, V2I, 2, (n,n), e2i)
        test_PowerVS(Int, V2I, 2, (n,n+1), e2i)
    end

    test_PowerVS(Float64, ScalarVS{Float64}, 1, (3,), ScalarVS{Float64}(1))
    test_PowerVS(Complex128, ScalarVS{Complex128}, 2, (3,4),
                 ScalarVS{Complex128}(1+im))
end
test_PowerVSs()
