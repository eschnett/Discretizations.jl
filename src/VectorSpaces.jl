"""
Vector spaces
"""
module VectorSpaces

using Traits

import Base: show
import Base: ==, isequal, hash
import Base: start, next, done, eltype, length
import Base: map

export show
export ==, isequal, hash
export start, next, done, eltype, length
export map



# TODO:
# - Check out packages "Interfaces", "Traits", "Typeclass"
# - Use generated functions, especially for MultiProductVS



export tupletypes
"Decompose a tuple type into a tuple of types"
tupletypes{T<:Tuple}(::Type{T}) = ntuple(d->fieldtype(T,d), nfields(T))



if map(+, 1, 1) == [2]
    import Base: map
    export map
    # This function is missing in Base
    @generated function map(f, x::Number, ys::Number...)
        Expr(:call, :f, :x, [:(ys[$n]) for n in 1:length(ys)]...)
    end
end
@assert map(+, 1, 1) == 2



# This function is not in Base
import Base: mod
export mod
function mod{T<:Integer}(i::Rational{T}, ::Type{Rational{Bool}})
    mod(num(i), Bool) // Bool(den(i))
end
@assert mod(0//1, Rational{Bool}) == false//true
@assert mod(1//1, Rational{Bool}) == true//true



# Scalars must support these operations:
#     +(S) -> S
#     -(S) -> S
#     +(S,S) -> S
#     -(S,S) -> S
#     *(S,S) -> S
#     zero(S) -> S
#     one(S) -> S

export AbstractScalar
export sconst, sadd, smul, smuladd

@traitdef AbstractScalar{S} begin
    sconst(Type{S}, Integer) -> S
    sadd(S,S) -> S
    smul(S,S) -> S
end

@traitfn smuladd{S; AbstractScalar{S}}(a::S, b::S, c::S) = sadd(smul(a, b), c)

sconst(::Type{Bool}, i::Integer) = Bool(abs(i)) # 0=false, +-1=true
sadd(a::Bool, b::Bool) = a | b
smul(a::Bool, b::Bool) = a & b

sconst(::Type{Rational{Bool}}, i::Integer) = Rational{Bool}(abs(i))
function sadd(a::Rational{Bool}, b::Rational{Bool})
    mod(sadd(Rational{Int}(a), Rational{Int}(b)), Rational{Bool})
end
function smul(a::Rational{Bool}, b::Rational{Bool})
    mod(smul(Rational{Int}(a), Rational{Int}(b)), Rational{Bool})
end

sconst{S<:Number}(::Type{S}, i::Integer) = S(i)
sadd{S<:Number}(a::S, b::S) = a + b
smul{S<:Number}(a::S, b::S) = a * b

sconst(::Type{Matrix{Bool}}, i::Integer) = diagm(sconst(Bool, i))
sadd(a::Matrix{Bool}, b::Matrix{Bool}) = a | b
smul(a::Matrix{Bool}, b::Matrix{Bool}) = Matrix{Bool}(a * b)

function sconst(::Type{Matrix{Rational{Bool}}}, i::Integer)
    diagm(sconst(Rational{Bool}, i))
end
function sadd(a::Matrix{Rational{Bool}}, b::Matrix{Rational{Bool}})
    RB, RI = Rational{Bool}, Rational{Int}
    map(i->mod(i, RB), sadd(Matrix{RI}(a), Matrix{RI}(b)))::Matrix{RB}
end
function smul(a::Matrix{Rational{Bool}}, b::Matrix{Rational{Bool}})
    RB, RI = Rational{Bool}, Rational{Int}
    map(i->mod(i, RB), smul(Matrix{RI}(a), Matrix{RI}(b)))::Matrix{RB}
end

sconst{S<:Number}(::Type{Matrix{S}}, i::Integer) = diagm(sconst(S, i))
sadd{S<:Number}(a::Matrix{S}, b::Matrix{S}) = a + b
smul{S<:Number}(a::Matrix{S}, b::Matrix{S}) = a * b



# Abstract vector space definition

export AbstractVS
export veltype, vnewtype, vdim, vnull, vdir, vscale, vadd

# Note: We don't want S as explicit argument, since we want to be able
# to use AbstractVS even if only V is known
@traitdef AbstractVS{V} begin
    S = veltype(V)
    @constraints begin
        istrait(AbstractScalar{S})
    end

    veltype(Type{V}) -> Type{S}
    vnewtype{R}(Type{V}, Type{R}) -> Type
    vdim(Type{V}) -> Int
    vdim(V) -> Int

    vnull(Type{V}) -> V
    # vconst(Type{V}, S) -> V
    vdir(Type{V}, Integer) -> V
    # vmake: create vector from collection
    
    vscale(S, V) -> V
    vadd(V, V) -> V
end



# TODO: This is currently broken in Traits.jl; wait for resolution
# @traitfn eltype{V; AbstractVS{V}}(dummy::Type{V}) = veltype(V)
@traitfn length{V; AbstractVS{V}}(x::V) = vdim(x)

# TODO: map
# TODO: mapreduce, reduce



# Scalars as vector space

_svtypes = [Bool,
            Int8, Int16, Int32, Int64, Int128, BigInt,
            Rational{Bool},
            Rational{Int8}, Rational{Int16}, Rational{Int32}, Rational{Int64},
            Rational{Int128}, Rational{BigInt},
            Float16, Float32, Float64, BigFloat]
_svtypes = vcat(_svtypes, [Complex{T} for T in _svtypes])
_svtypes = vcat(_svtypes, [Matrix{T} for T in _svtypes])
ScalarVector = Union{_svtypes...}

veltype{S<:ScalarVector}(::Type{S}) = S
# vnewtype{S<:ScalarVector,R}(::Type{S}, ::Type{R}) = R
vnewtype{S<:ScalarVector,R}(::Type{S}, R0::Type{R}) = R0
vdim{S<:ScalarVector}(::Type{S}) = 1
vdim(x::ScalarVector) = vdim(typeof(x))

vnull{S<:ScalarVector}(::Type{S}) = sconst(S, 0)
vdir{S<:ScalarVector}(::Type{S}, i::Integer) = sconst(S, 1)
vscale{S<:ScalarVector}(a::S, x::S) = smul(a, x)
vadd{S<:ScalarVector}(x::S, y::S) = sadd(x, y)



# The trivial vector space

export EmptyVS

immutable EmptyVS{S}
    function EmptyVS()
        _check_EmptyVS(S)
        new()
    end
end

@generated function _check_EmptyVS{S}(::Type{S})
    @assert istrait(AbstractScalar{S})
    :nothing
end

function show(io::IO, x::EmptyVS)
    S = veltype(typeof(x))
    print(io, "VS{$S}[]")
end

start(x::EmptyVS) = nothing
next(x::EmptyVS, state) = throw(BoundsError(x))
done(x::EmptyVS, state) = true
eltype{S}(::Type{EmptyVS{S}}) = veltype(EmptyVS{S})

@generated function map(f, x::EmptyVS, ys::EmptyVS...)
    S = veltype(x)
    TS = map(veltype, ys)
    quote
        R = typeof(f($(sconst(S,0)), $([sconst(T,0) for T in TS]...)))
        EmptyVS{R}()
    end
end

veltype{S}(::Type{EmptyVS{S}}) = S
vnewtype{S,R}(::Type{EmptyVS{S}}, ::Type{R}) = EmptyVS{R}
vdim{S}(::Type{EmptyVS{S}}) = 0
vdim(x::EmptyVS) = vdim(typeof(x))

vnull{S}(::Type{EmptyVS{S}}) = EmptyVS{S}()
vdir{S}(::Type{EmptyVS{S}}, i::Integer) = throw(BoundsError(i))
vscale{S}(a::S, x::EmptyVS{S}) = EmptyVS{S}()
vadd{S}(x::EmptyVS{S}, y::EmptyVS{S}) = EmptyVS{S}()



# A scalar vector space

export ScalarVS

immutable ScalarVS{S}
    elt::S
    function ScalarVS(elt)
        _check_ScalarVS(S)
        new(elt)
    end
end

@generated function _check_ScalarVS{S}(::Type{S})
    @assert istrait(AbstractScalar{S})
    :nothing
end

function show(io::IO, x::ScalarVS)
    S = veltype(typeof(x))
    print(io, "VS{$S}[")
    show(io, x.elt)
    print(io, "]")
end

=={S}(x::ScalarVS{S}, y::ScalarVS{S}) = x.elt == y.elt
isequal{S}(x::ScalarVS{S}, y::ScalarVS{S}) = isequal(x.elt, y.elt)
hash(x::ScalarVS, h::UInt) = hash(typeof(x), hash(x.elt, h))

start(x::ScalarVS) = false
next(x::ScalarVS, state) = (x.elt, true)
done(x::ScalarVS, state) = state
eltype{S}(::Type{ScalarVS{S}}) = veltype(ScalarVS{S})

@generated function map(f, x::ScalarVS, ys::ScalarVS...)
    quote
        r = f(x.elt, $([:(ys[$d].elt) for d in 1:length(ys)]...))
        ScalarVS{typeof(r)}(r)
    end
end

veltype{S}(::Type{ScalarVS{S}}) = S
vnewtype{S,R}(::Type{ScalarVS{S}}, ::Type{R}) = ScalarVS{R}
vdim{S}(::Type{ScalarVS{S}}) = 1
vdim(x::ScalarVS) = vdim(typeof(x))

vnull{S}(::Type{ScalarVS{S}}) = ScalarVS{S}(sconst(S, 0))
vdir{S}(::Type{ScalarVS{S}}, i::Integer) = ScalarVS{S}(sconst(S, 1))
vscale{S}(a::S, x::ScalarVS{S}) = ScalarVS{S}(smul(a, x.elt))
vadd{S}(x::ScalarVS{S}, y::ScalarVS{S}) = ScalarVS{S}(sadd(x.elt, y.elt))



# TODO: Sum (i.e. union) of two vector spaces?



# Product of two vector spaces

export ProductVS

immutable ProductVS{V1,V2}
    v1::V1
    v2::V2
    function ProductVS(v1, v2)
        _check_ProductVS(V1, V2)
        new(v1, v2)
    end
end

@generated function _check_ProductVS{V1,V2}(::Type{V1}, ::Type{V2})
    @assert istrait(AbstractVS{V1})
    @assert istrait(AbstractVS{V2})
    @assert veltype(V1) === veltype(V2)
    :nothing
end

function show(io::IO, x::ProductVS)
    S = veltype(typeof(x))
    print(io, "VS{$S}[")
    show(io, x.v1)
    print(io, ",")
    show(io, x.v2)
    print(io, "]")
end

function =={V1,V2}(x::ProductVS{V1,V2}, y::ProductVS{V1,V2})
    x.v1 == y.v1 && x.v2 == y.v2
end
function isequal{V1,V2}(x::ProductVS{V1,V2}, y::ProductVS{V1,V2})
    isequal(x.v1, y.v1) && isequal(x.v2, y.v2)
end
hash(x::ProductVS, h::UInt) = hash(typeof(x), hash(x.v1, hash(x.v2, h)))

start(x::ProductVS) = (start(x.v1), start(x.v2))
function next(x::ProductVS, state)
    S = veltype(typeof(x))
    s1,s2 = state
    e::S
    if !done(x.v1,s1)
        e,s1 = next(x.v1,s1)
    else
        e,s2 = next(x.v2,s2)
    end
    (e, (s1,s2))
end
function done(x::ProductVS, state)
    s1,s2 = state
    done(x.v1,s1) && done(x.v2,s2)
end
eltype{V1,V2}(::Type{ProductVS{V1,V2}}) = veltype(ProductVS{V1,V2})

@generated function map(f, x::ProductVS, ys::ProductVS...)
    V = x
    S = veltype(V)
    for y in ys
        W = y
        @assert vnewtype(W, S) === V
    end
    quote
        r1 = map(f, x.v1, $([:(ys[$d].v1) for d in 1:length(ys)]...))
        r2 = map(f, x.v2, $([:(ys[$d].v2) for d in 1:length(ys)]...))
        ProductVS{typeof(r1), typeof(r2)}(r1, r2)
    end
end

veltype{V1,V2}(::Type{ProductVS{V1,V2}}) = veltype(V1)
function vnewtype{V1,V2,R}(::Type{ProductVS{V1,V2}}, ::Type{R})
    ProductVS{vnewtype(V1,R), vnewtype(V2,R)}
end
function vdim{V1,V2}(::Type{ProductVS{V1,V2}})
    # TODO: improve performance
    if vdim(V1)<0 || vdim(V2)<0 return -1 end
    return vdim(V1) + vdim(V2)
end
vdim(x::ProductVS) = vdim(x.v1) + vdim(x.v2)

vnull{V1,V2}(::Type{ProductVS{V1,V2}}) = ProductVS{V1,V2}(vnull(V1), vnull(V2))
function vdir{V1,V2}(::Type{ProductVS{V1,V2}}, i::Integer)
    v1 = vnull(V1)
    l1 = vdim(v1)
    if i <= l1
        ProductVS{V1,V2}(vdir(V1, i), vnull(V2))
    else
        ProductVS{V1,V2}(v1, vdir(V2, i-l1))
    end
end
function vscale(a, x::ProductVS)
    V = typeof(x)
    S = veltype(V)
    a::S
    V(vscale(a, x.v1), vscale(a, x.v2))
end
function vadd{V1,V2}(x::ProductVS{V1,V2}, y::ProductVS{V1,V2})
    ProductVS{V1,V2}(vadd(x.v1, y.v1), vadd(x.v2, y.v2))
end



# Products with multiple factors, represented as tuples

export MultiProductVS

immutable MultiProductVS{VS}
    vs::VS
    function MultiProductVS(vs)
        _check_MultiProductVS(VS)
        new(vs)
    end
end

@generated function _check_MultiProductVS{VS}(::Type{VS})
    @assert VS <: Tuple
    @assert nfields(VS) > 0
    S = veltype(fieldtype(VS,1))
    for d in 1:nfields(VS)
        V = fieldtype(VS,d)
        @assert istrait(AbstractVS{V})
        @assert veltype(V) === S
    end
    :nothing
end

_get_types{VS}(::Type{MultiProductVS{VS}}) = VS

@generated function show(io::IO, x::MultiProductVS)
    V = x
    VS = _get_types(V)
    S = veltype(x)
    stmts = []
    push!(stmts, :(print(io, "VS{$($S)}[")))
    for d in 1:nfields(VS)
        if d>1
            push!(stmts, :(print(io, ",")))
        end
        push!(stmts, :(show(io, x.vs[$d])))
    end
    push!(stmts, :(print(io, "]")))
    Expr(:block, stmts...)
end

@generated function =={VS}(x::MultiProductVS{VS}, y::MultiProductVS{VS})
    Expr(:&&, [:(x.vs[$d] == y.vs[$d]) for d in 1:nfields(VS)]...)
end
@generated function isequal{VS}(x::MultiProductVS{VS}, y::MultiProductVS{VS})
    Expr(:&&, [:(isequal(x.vs[$d], y.vs[$d])) for d in 1:nfields(VS)]...)
end
@generated function hash(x::MultiProductVS, h::UInt)
    V = x
    VS = _get_types(x)
    expr = :h
    for d in 1:nfields(VS)
        expr = :(hash(x.vs[d], $expr))
    end
    :(hash(typeof(x), $expr))
end

"advance outer iterator while inner iterator is done"
function _advance(x::MultiProductVS, st,el,sti)
    while !done(x.vs,st) && done(el,sti)
        el,st = next(x.vs,st)
        sti = start(el)
    end
    @assert done(x.vs,st) || !done(el,sti)
    st,el,sti
end
function start(x::MultiProductVS)
    st = start(x.vs)
    @assert !done(x.vs,st)
    # if done(x.vs,st)
    #     return st,nothing,nothing
    # end
    el,st = next(x.vs,st)
    sti = start(el)
    _advance(x, st,el,sti)
end
function next(x::MultiProductVS, state)
    st,el,sti = state
    @assert !done(x, state)                 # precondition
    @assert !done(el,sti)                   # invariant
    eli,sti = next(el,sti)
    eli, _advance(x, st,el,sti)
end
function done(x::MultiProductVS, state)
    st,el,sti = state
    # done(x.vs,st) && (el===nothing || done(el,sti))
    @assert el!==nothing
    done(x.vs,st) && done(el,sti)
end
eltype{VS}(V::Type{MultiProductVS{VS}}) = veltype(V)

@generated function map(f, x::MultiProductVS, ys::MultiProductVS...)
    V = x
    VS = _get_types(V)
    S = veltype(V)
    for y in ys
        W = y
        @assert vnewtype(W, S) === V
    end
    quote
        r = tuple($([:(map(f, x.vs[$d],
                           $([:(ys[$i].vs[$d]) for i in 1:nfields(ys)]...)))
                     for d in 1:length(VS)]...))
        MultiProductVS{typeof(r)}(r)
    end
end

veltype{VS}(::Type{MultiProductVS{VS}}) = veltype(fieldtype(VS,1))
function vnewtype{VS,R}(::Type{MultiProductVS{VS}}, ::Type{R})
    RS = map(V->vnewtype(V,R), tupletypes(VS))
    V = MultiProductVS{Tuple{RS...}}
end
function vdim{VS}(::Type{MultiProductVS{VS}})
    # TODO: improve performance
    if any(collect(map(vdim, tupletypes(VS))) .< 0) return -1 end
    sum(map(vdim, tupletypes(VS)))::Int
end
@generated function vdim{VS}(x::MultiProductVS{VS})
    quote
        +($([:(vdim(x.vs[$d])) for d in 1:nfields(VS)]...))
    end
end

function vdir{VS}(::Type{MultiProductVS{VS}}, i::Integer)
    v0 = vnull(MultiProductVS{VS})
    l = 0
    for d in 1:nfields(VS)
        ld = vdim(v0.vs[d])
        if l < i <= l+ld
            rs = tuple([d2==d ? vdir(fieldtype(VS, d), i-l) : v0.vs[d2]
                        for d2 in 1:nfields(VS)]...)
            return MultiProductVS{VS}(rs)
        end
        l += ld
    end
    throw(BoundsError(i))
end

@generated function vnull{VS}(::Type{MultiProductVS{VS}})
    quote
        rs = tuple($(map(T->:(vnull($T)), tupletypes(VS))...))
        MultiProductVS{$VS}(rs)
    end
end
@generated function vscale{VS}(a, x::MultiProductVS{VS})
    V = x
    S = veltype(V)
    @assert a === S
    quote
        rs = tuple($([:(vscale(a, x.vs[$d])) for d in 1:nfields(VS)]...))
        MultiProductVS{$VS}(rs)
    end
end
@generated function vadd{VS}(x::MultiProductVS{VS}, y::MultiProductVS{VS})
    quote
        rs = tuple($([:(vadd(x.vs[$d], y.vs[$d])) for d in 1:nfields(VS)]...))
        MultiProductVS{$VS}(rs)
    end
end



# Power of a vector space

export PowerVS

immutable PowerVS{V1,D}
    # TODO: Allow arbitrary array types
    vs::Array{V1,D}
    function PowerVS(vs)
        _check_PowerVS(V1)
        new(vs)
    end
end

@generated function _check_PowerVS{V1}(::Type{V1})
    @assert istrait(AbstractVS{V1})
    :nothing
end

_get_eltype{V1,D}(::Type{PowerVS{V1,D}}) = V1
_get_ndims{V1,D}(::Type{PowerVS{V1,D}}) = D

function show(io::IO, x::PowerVS)
    V = typeof(x)
    S = veltype(V)
    print(io, "VS{$S}[")
    for i in eachindex(x.vs)
        if i>1 print(io, ",") end
        show(io, x.vs[i])
    end
    print(io, "]")
end

=={V1,D}(x::PowerVS{V1,D}, y::PowerVS{V1,D}) = x.vs == y.vs
isequal{V1,D}(x::PowerVS{V1,D}, y::PowerVS{V1,D}) = isequal(x.vs, y.vs)
hash(x::PowerVS, h::UInt) = hash(typeof(x), hash(x.vs, h))

"advance outer iterator while inner iterator is done"
function _advance(x::PowerVS, st,el,sti)
    while !done(x.vs,st) && done(el,sti)
        el,st = next(x.vs,st)
        sti = start(el)
    end
    @assert done(x.vs,st) || !done(el,sti)
    st,el,sti
end
function start(x::PowerVS)
    st = start(x.vs)
    if done(x.vs,st)
        # there are no inner elements
        return st,nothing,nothing # TODO: make this type stable
    end
    el,st = next(x.vs,st)
    sti = start(el)
    _advance(x, st,el,sti)
end
function next(x::PowerVS, state)
    st,el,sti = state
    @assert !done(x, state)                 # precondition
    @assert !done(el,sti)                   # invariant
    eli,sti = next(el,sti)
    eli, _advance(x, st,el,sti)
end
function done(x::PowerVS, state)
    st,el,sti = state
    done(x.vs,st) && (el===nothing || done(el,sti))
end
eltype{V1,D}(::Type{PowerVS{V1,D}}) = veltype(PowerVS{V1,D})

@generated function map(f, x::PowerVS, ys::PowerVS...)
    V = x
    S = veltype(V)
    WS = ys
    for W in WS
        @assert vnewtype(W, S) === V
    end
    TS = map(veltype, WS)
    V1 = _get_eltype(V)
    D = _get_ndims(V)
    quote
        R = typeof(f($(sconst(S,0)),
                     $([sconst(veltype(T),0) for T in TS]...)))
        W1 = vnewtype($V1, R)
        rs = similar(x.vs, W1)
        @inbounds @simd for i in eachindex(x.vs)
            rs[i] = map(f, x.vs[i],
                        $([:(ys[$d].vs[i]) for d in 1:nfields(ys)]...))
        end
        PowerVS{W1,$D}(rs)
    end
end

veltype{V1,D}(::Type{PowerVS{V1,D}}) = veltype(V1)
vnewtype{V1,D,R}(::Type{PowerVS{V1,D}}, ::Type{R}) = PowerVS{vnewtype(V1,R),D}
function vdim{V1,D}(::Type{PowerVS{V1,D}})
    # TODO: Once arbitrary array types are allowed, handle case where
    # array length is fixed
    if D==0 return vdim(V1) end
    if vdim(V1)==0 return 0 end
    -1
end
vdim(x::PowerVS) = mapreduce(vdim, +, 0, x.vs)::Int

function vnull{V1,D}(::Type{PowerVS{V1,D}},
                     sz::NTuple{D,Integer}=ntuple(d->0,D))
    r = Array{V1}(sz)
    @inbounds @simd for i in eachindex(r)
        r[i] = vnull(V1)
    end
    PowerVS{V1,D}(r)
end
function vdir{V1,D}(::Type{PowerVS{V1,D}}, i::Integer)
    rs = vnull(PowerVS{V1,D})
    l = 0
    for d in eachindex(rs.vs)
        ld = vdim(rs.vs[d])
        if l < i <= l+ld
            rs.vs[d] = vdir(V1, i-l)
            return rs
        end
        l += ld
    end
    throw(BoundsError(i))
end
function vscale(a, x::PowerVS)
    V = typeof(x)
    S = veltype(V)
    a::S
    xs = x.vs
    rs = similar(xs)
    @inbounds @simd for i in eachindex(rs)
        rs[i] = vscale(a, xs[i])
    end
    V(rs)
end
function vadd{V1,D}(x::PowerVS{V1,D}, y::PowerVS{V1,D})
    xs = x.vs
    ys = y.vs
    @assert size(xs) == size(ys)
    rs = similar(xs)
    @inbounds @simd for i in eachindex(rs)
        rs[i] = vadd(xs[i], ys[i])
    end
    PowerVS{V1,D}(rs)
end

end
