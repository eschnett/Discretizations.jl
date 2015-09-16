# tupletypes
@test tupletypes(Tuple{}) === ()
@test tupletypes(Tuple{Int}) === (Int,)
@test tupletypes(Tuple{Int,Char}) === (Int,Char)
@test_throws MethodError tupletypes(1)
@test_throws MethodError tupletypes((1,2))
@test_throws MethodError tupletypes(Int)
@test_throws MethodError tupletypes((Int,Char))



if map(+, 1, 1) == [2]
    import Base: map
    # This function is missing in Base
    @generated function map(f, x::Number, ys::Number...)
        Expr(:call, :f, :x, [:(ys[$n]) for n in 1:length(ys)]...)
    end
end
@test map(+, 1, 1) == 2



# scalars

function test_Scalar(S::Type)
    @test istrait(AbstractScalar{S})
    z = sconst(S, 0)
    e = sconst(S, 1)
    n = sconst(S, -1)
    @test smul(n, z) == z
    @test smul(n, smul(n, e)) == e
    @test sadd(z, z) == z
    @test sadd(z, e) == e
    @test sadd(e, z) == e
    @test smul(z, z) == z
    @test smul(z, e) == z
    @test smul(e, z) == z
    @test smul(e, e) == e
    @test smul(smul(n, e), smul(n, e)) == e

    t = sadd(e, e)
    for a in (z,e,n,t)
        # neutral element
        @test sadd(a, z) == a
        @test sadd(z, a) == a
        # neutral element
        @test smul(a, e) == a
        @test smul(e, a) == a
        # zero
        @test smul(a, z) == z
        @test smul(z, a) == z
    end

    for a in (z,e,n,t), b in (z,e,n,t)
        # commutativity
        @test smul(a, b) == smul(b, a)
        @test sadd(a, b) == sadd(b, a)
        # special cases
        @test smuladd(a, b, z) == smul(a, b)
        @test smuladd(a, e, b) == sadd(a, b)
        @test smuladd(e, a, b) == sadd(a, b)
    end

    for a in (z,e,n,t), b in (z,e,n,t), c in (z,e,n,t)
        # associativity
        @test smul(smul(a, b), c) == smul(a, smul(b, c))
        @test sadd(sadd(a, b), c) == sadd(a, sadd(b, c))
        # distributivity
        @test smul(sadd(a, b), c) == sadd(smul(a, c), smul(b, c))
        # definition
        @test smuladd(a, b, c) == sadd(smul(a, b), c)
    end
end
function test_Scalars()
    test_Scalar(Bool)
    test_Scalar(Int)
    test_Scalar(Float64)
    test_Scalar(Complex128)
    test_Scalar(Matrix{Bool})
    test_Scalar(Matrix{Int})
    test_Scalar(Matrix{Float64})
    test_Scalar(Matrix{Complex128})
end
test_Scalars()



# AbstractVS

function test_AbstractVS()
end
test_AbstractVS()



# Scalars as vector space

function test_ScalarVector(V::Type)
    S = V
    @test istrait(AbstractScalar{S})
    @test istrait(AbstractVS{S})

    @test veltype(V) === S
    @test veltype(vnewtype(V, Int)) === Int

    z = vnull(V)
    e = V(sconst(S, 1))
    @test z == sconst(S, 0)
    @test e == sconst(S, 1)

    @test "$z" == "$(sconst(S, 0))"

    @test collect(z) == collect(sconst(S, 0))
    @test collect(e) == collect(sconst(S, 1))

    @test map(smuladd, e, e, z) == e

    @test vdim(V) == 1
    @test vdim(z) == 1
    @test vscale(sconst(S, 1), z) == z
    @test vadd(z, z) == z
    @test vscale(sconst(S, 0), e) == z
    @test vscale(sconst(S, 1), e) == e
    if S ∉ (Bool, Matrix{Bool})
        @test vscale(sconst(S, 2), e) == sconst(S, 2)
    end
    @test vadd(z, e) == e
    @test vadd(e, z) == e
    if S ∉ (Bool, Matrix{Bool})
        @test vadd(e, e) == sconst(S, 2)
    end
end
function test_ScalarVectors()
    test_ScalarVector(Bool)
    test_ScalarVector(Int)
    test_ScalarVector(Float64)
    test_ScalarVector(Complex128)
    test_ScalarVector(Matrix{Bool})
    test_ScalarVector(Matrix{Int})
    test_ScalarVector(Matrix{Float64})
    test_ScalarVector(Matrix{Complex128})
end
test_ScalarVectors()



# EmptyVS

function test_EmptyVS(S::Type)
    @test istrait(AbstractScalar{S})
    V = EmptyVS{S}
    @test istrait(AbstractVS{V})

    @test veltype(V) === S
    @test veltype(vnewtype(V, Int)) === Int

    z = vnull(V)
    @test z == V()

    @test "$z" == "VS{$S}[]"

    @test done(z, start(z))
    @test_throws BoundsError next(z, start(z))
    @test eltype(z) === S
    @test length(z) == 0

    @test typeof(collect(z)) === Vector{S}
    @test collect(z) == []

    @test map(smuladd, z, z, z) == z

    @test vdim(V) == 0
    @test vdim(z) == 0
    @test vscale(S(1), z) == z
    @test vadd(z, z) == z
end
function test_EmptyVSs()
    test_EmptyVS(Bool)
    test_EmptyVS(Int)
    test_EmptyVS(Float64)
    test_EmptyVS(Complex128)
end
test_EmptyVSs()



# ScalarVS

function test_ScalarVS(S::Type)
    @test istrait(AbstractScalar{S})
    V = ScalarVS{S}
    @test istrait(AbstractVS{V})

    @test veltype(V) === S
    @test veltype(vnewtype(V, Int)) === Int

    z = vnull(V)
    e = V(sconst(S, 1))
    @test z.elt == sconst(S, 0)
    @test e.elt == sconst(S, 1)

    @test "$z" == "VS{$S}[$(sconst(S, 0))]"

    array(x) = collect((x,))
    @test collect(z) == array(sconst(S, 0))
    @test collect(e) == array(sconst(S, 1))

    @test map(smuladd, e, e, z) == e

    @test vdim(V) == 1
    @test vdim(z) == 1
    @test vscale(sconst(S, 1), z) == z
    @test vadd(z, z) == z
    @test vscale(sconst(S, 0), e) == z
    @test vscale(sconst(S, 1), e) == e
    if S ∉ (Bool, Matrix{Bool})
        @test vscale(sconst(S, 2), e).elt == sconst(S, 2)
    end
    @test vadd(z, e) == e
    @test vadd(e, z) == e
    if S ∉ (Bool, Matrix{Bool})
        @test vadd(e, e).elt == sconst(S, 2)
    end
end
function test_ScalarVSs()
    test_ScalarVS(Bool)
    test_ScalarVS(Int)
    test_ScalarVS(Float64)
    test_ScalarVS(Complex128)
    test_ScalarVS(Matrix{Bool})
    test_ScalarVS(Matrix{Int})
    test_ScalarVS(Matrix{Float64})
    test_ScalarVS(Matrix{Complex128})
end
test_ScalarVSs()



# ProductVS

function test_ProductVS{V1,V2}(::Type{V1}, ::Type{V2}, e1::V1, e2::V2)
    S = veltype(V1)
    @assert veltype(V2) === S
    @test istrait(AbstractScalar{S})

    V = ProductVS{V1,V2}
    @test istrait(AbstractVS{V})
    @test veltype(V) === S
    @test veltype(vnewtype(V, Int)) === Int

    z = vnull(V)
    e = V(e1, e2)
    @test z.v1 == vnull(V1)
    @test z.v2 == vnull(V2)

    @test "$z" == "VS{$S}[$(vnull(V1)),$(vnull(V2))]"

    @test typeof(collect(z)) === Vector{S}
    @test collect(z) == [collect(vnull(V1)); collect(vnull(V2))]
    @test typeof(collect(e)) === Vector{S}
    @test collect(e) == [collect(e1); collect(e2)]

    @test map(smuladd, e, e, z) == e

    @test vdim(V) == vdim(V1) + vdim(V2)
    @test vdim(z) == vdim(vnull(V1)) + vdim(vnull(V2))

    @test vscale(sconst(S, 1), z) == z
    @test vadd(z, z) == z
    @test vscale(sconst(S, 0), e) == z
    @test vscale(sconst(S, 1), e) == e
    if S ∉ (Bool, Matrix{Bool})
        @test vscale(sconst(S, 2), e).v1 == vscale(sconst(S, 2), e1)
        @test vscale(sconst(S, 2), e).v2 == vscale(sconst(S, 2), e2)
    end
    @test vadd(z, e) == e
    @test vadd(e, z) == e
    @test vadd(e, e).v1 == vadd(e1, e1)
    @test vadd(e, e).v2 == vadd(e2, e2)
end
function test_ProductVSs()
    V0I = EmptyVS{Int}
    V1I = ScalarVS{Int}
    e0i = V0I()
    e1i = V1I(1)

    test_ProductVS(V0I, V0I, e0i, e0i)
    test_ProductVS(V0I, V1I, e0i, e1i)
    test_ProductVS(V1I, V0I, e1i, e0i)
    test_ProductVS(V1I, V1I, e1i, e1i)

    V2I = ProductVS{V1I, V1I}
    e2i = V2I(e1i, e1i)
    test_ProductVS(V2I, V2I, e2i, e2i)

    V1B = ScalarVS{Bool}
    e1b = V1B(1)
    test_ProductVS(V1B, V1B, e1b, e1b)

    V1F = ScalarVS{Float64}
    e1f = V1F(1)
    test_ProductVS(V1F, V1F, e1f, e1f)

    V1C = ScalarVS{Complex128}
    e1c = V1C(1)
    test_ProductVS(V1C, V1C, e1c, e1c)

    # TODO: Matrix{...}
end
test_ProductVSs()



# MultiProductVS

function test_MultiProductVS{VS}(::Type{VS}, es::VS)
    @assert nfields(VS) > 0
    S = veltype(fieldtype(VS,1))
    for d in 1:nfields(VS)
        @assert veltype(fieldtype(VS,d)) === S
    end
    @test istrait(AbstractScalar{S})
    V = MultiProductVS{VS}
    @test istrait(AbstractVS{V})
    @test veltype(V) === S
    @test veltype(vnewtype(V, Int)) === Int

    z = vnull(V)
    e = V(es)
    @test z.vs == map(vnull, tupletypes(VS))
    @test e.vs == es

    zs = join(map(string, map(vnull, tupletypes(VS))), ",")
    @test "$z" == "VS{$S}[$zs]"

    @test typeof(collect(z)) === Vector{S}
    @test collect(z) == vcat(map(collect, map(vnull, tupletypes(VS)))...)
    @test typeof(collect(e)) === Vector{S}
    @test collect(e) == vcat(map(collect, es)...)

    @test map(smuladd, e, e, z) == e

    @test vdim(V) == sum(vdim, tupletypes(VS))
    @test vdim(z) == sum(vdim, map(vnull, tupletypes(VS)))

    @test vscale(sconst(S, 1), z) == z
    @test vadd(z, z) == z
    @test vscale(sconst(S, 0), e) == z
    @test vscale(sconst(S, 1), e) == e
    if S ∉ (Bool, Matrix{Bool})
        @test vscale(sconst(S, 2), e).vs == map(x->vscale(sconst(S, 2), x), es)
    end
    @test vadd(z, e) == e
    @test vadd(e, z) == e
    @test vadd(e, e).vs == map(vadd, es, es)
end
function test_MultiProductVSs()
    V0I = EmptyVS{Int}
    V1I = ScalarVS{Int}
    e0i = V0I()
    e1i = V1I(1)

    test_MultiProductVS(Tuple{V0I, V0I}, (e0i, e0i))
    test_MultiProductVS(Tuple{V0I, V1I}, (e0i, e1i))
    test_MultiProductVS(Tuple{V1I, V0I}, (e1i, e0i))
    test_MultiProductVS(Tuple{V1I, V1I}, (e1i, e1i))

    test_MultiProductVS(Tuple{V0I}, (e0i,))
    test_MultiProductVS(Tuple{V1I}, (e1i,))
    test_MultiProductVS(Tuple{V1I, V1I, V1I}, (e1i, e1i, e1i))

    # TODO
end
test_MultiProductVSs()



# PowerVS

function test_PowerVS{V1}(::Type{V1}, D::Integer, n::Tuple, e1::V1)
    S = veltype(V1)
    @test istrait(AbstractScalar{S})
    V = PowerVS{V1,D}
    @test istrait(AbstractVS{V})
    @test veltype(V) === S
    @test veltype(vnewtype(V, Int)) === Int

    z0 = vnull(V)
    @test vdim(z0) == 0^D * vdim(vnull(V1))
    z1 = vnull(V, ntuple(d->1,D))
    @test vdim(z1) == vdim(vnull(V1))

    z = vnull(V, n)
    e = V(deepcopy(z.vs))
    if prod(n)>0
        e.vs[1] = e1
    end
    if prod(n)>0
        @test z.vs[1] == vnull(V1)
        @test z.vs[(end+1)÷2] == vnull(V1)
        @test z.vs[end] == vnull(V1)
    end

    zs = "$(vnull(V1))"
    if prod(n)>0
        @test "$z" == "VS{$S}[" * (zs*",")^(prod(n)-1) * zs * "]"
    else
        @test "$z" == "VS{$S}[]"
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

    @test map(smuladd, e, e, z) == e

    @test vdim(V) == (D==0 ? vdim(V1) : vdim(V1)==0 ? 0 : -1)
    @test vdim(z) == prod(n) * vdim(vnull(V1))
    @test vscale(sconst(S, 1), z) == z
    @test vadd(z, z) == z

    @test vscale(sconst(S, 0), e) == z
    @test vscale(sconst(S, 1), e) == e
    if prod(n)>0
        @test vscale(sconst(S, 2), e).vs[1] == vscale(sconst(S, 2), e1)
        if D>0
            @test vscale(sconst(S, 2), e).vs[2] == vnull(V1)
            @test vscale(sconst(S, 2), e).vs[end] == vnull(V1)
        end
    end
    @test vadd(z, e) == e
    @test vadd(e, z) == e
    if prod(n)>0
        @test vadd(e, e).vs[1] == vadd(e1, e1)
        if D>0
            @test vadd(e, e).vs[2] == vnull(V1)
            @test vadd(e, e).vs[end] == vnull(V1)
        end
    end
end
function test_PowerVSs()
    # test_PowerVS(Bool)
    V0I = EmptyVS{Int}
    V1I = ScalarVS{Int}
    V2I = ProductVS{V1I, V1I}
    e0i = V0I()
    e1i = V1I(1)
    e2i = V2I(e1i, e1i)

    for n in (0,3)
        test_PowerVS(V0I, 1, (n,), e0i)
        test_PowerVS(V1I, 1, (n,), e1i)
        test_PowerVS(V0I, 1, (n,), e0i)
        test_PowerVS(V1I, 1, (n,), e1i)
        test_PowerVS(V2I, 0, (), e2i)
        test_PowerVS(V2I, 1, (n,), e2i)
        test_PowerVS(V2I, 2, (n,n), e2i)
        test_PowerVS(V2I, 2, (n,n+1), e2i)
    end

    test_PowerVS(ScalarVS{Float64}, 1, (3,), ScalarVS{Float64}(1))
    test_PowerVS(ScalarVS{Complex128}, 2, (3,4), ScalarVS{Complex128}(1))
end
test_PowerVSs()
