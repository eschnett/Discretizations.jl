# """
# Vector spaces
# """
# module VectorSpaces

using Traits

import Base: show
import Base: ==, isequal, hash
import Base: start, next, done, eltype, length

export show
export ==, isequal, hash
export start, next, done, eltype, length



# TODO:
# - Check out packages "Interfaces", "Traits", "Typeclass"
# - Use generated functions, especially for MultiProductVS



export typesame
"Ensure that both types are equal, and return this type"
function typesame{T}(T1::Type{T}, T2::Type{T})
    T1
end



export tupletypes
"Decompose a tuple type into a tuple of types"
tupletypes{U<:Tuple}(T::Type{U}) = ntuple(d->fieldtype(T,d), nfields(T))



# Scalars must support these operations:
#     +(S) -> S
#     -(S) -> S
#     +(S,S) -> S
#     -(S,S) -> S
#     *(S,S) -> S
#     zero(S) -> S
#     one(S) -> S

export AbstractScalar
export sconst, sadd, smul

@traitdef AbstractScalar{S} begin
    sconst(Type{S}, Integer) -> S
    sadd(S,S) -> S
    smul(S,S) -> S
end

sconst(::Type{Bool}, i::Integer) = Bool(abs(i)) # 0=false, +-1=true
sadd(a::Bool, b::Bool) = a | b
smul(a::Bool, b::Bool) = a & b

sconst{S<:Number}(::Type{S}, i::Integer) = S(i)
sadd{S<:Number}(a::S, b::S) = a + b
smul{S<:Number}(a::S, b::S) = a * b

sconst(::Type{Matrix{Bool}}, i::Integer) = diagm(sconst(Bool, i))
sadd(a::Matrix{Bool}, b::Matrix{Bool}) = a | b
smul(a::Matrix{Bool}, b::Matrix{Bool}) = Matrix{Bool}(a * b)

sconst{S<:Number}(::Type{Matrix{S}}, i::Integer) = diagm(sconst(S, i))
sadd{S<:Number}(a::Matrix{S}, b::Matrix{S}) = a + b
smul{S<:Number}(a::Matrix{S}, b::Matrix{S}) = a * b



# Abstract vector space definition

export AbstractVS
export veltype, vnewtype, vdim, vnull, vscale, vadd

# Note: We don't want S as explicit argument, since we want to be able
# to use AbstractVS even if only V is known
@traitdef AbstractVS{V} begin
    S = veltype(V)
    @constraints begin
        istrait(AbstractScalar{S})
    end

    veltype(Type{V}) -> Type{S}
    vnewtype(Type{V}, Type) -> Type
    vdim(V) -> Int

    vnull(Type{V}) -> V
    vscale(S, V) -> V
    vadd(V, V) -> V
end



# TODO: This is currently broken in Traits.jl; wait for resolution
# @traitfn eltype{V; AbstractVS{V}}(dummy::Type{V}) = veltype(V)
@traitfn length{V; AbstractVS{V}}(x::V) = vdim(x)

# TODO: map
# TODO: mapreduce, reduce



# The trivial vector space

export EmptyVS

immutable EmptyVS{S} end

function show(io::IO, x::EmptyVS)
    S = veltype(typeof(x))
    print(io, "VS{$S}[]")
end

start(x::EmptyVS) = nothing
next(x::EmptyVS, state) = throw(BoundsError(x))
done(x::EmptyVS, state) = true
eltype{S}(V::Type{EmptyVS{S}}) = veltype(V)

veltype{S}(::Type{EmptyVS{S}}) = S
vnewtype{S}(::Type{EmptyVS{S}}, R::Type) = EmptyVS{R}
vdim(x::EmptyVS) = 0
vnull{S}(V::Type{EmptyVS{S}}) = V()
vscale{S}(a::S, x::EmptyVS{S}) = EmptyVS{S}()
vadd{S}(x::EmptyVS{S}, y::EmptyVS{S}) = EmptyVS{S}()



# A scalar vector space

export ScalarVS

immutable ScalarVS{S}
    elt::S
    ScalarVS(elt) = new(elt)
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
eltype{S}(V::Type{ScalarVS{S}}) = veltype(V)

veltype{S}(V::Type{ScalarVS{S}}) = S
vnewtype{S}(V::Type{ScalarVS{S}}, R::Type) = ScalarVS{R}
vdim(x::ScalarVS) = 1
vnull{S}(V::Type{ScalarVS{S}}) = V(sconst(S, 0))
vscale{S}(a::S, x::ScalarVS{S}) = ScalarVS{S}(smul(a, x.elt))
vadd{S}(x::ScalarVS{S}, y::ScalarVS{S}) = ScalarVS{S}(sadd(x.elt, y.elt))



# TODO: Sum (i.e. union) of two vector spaces?



# Product of two vector spaces

export ProductVS

immutable ProductVS{V1,V2}
    v1::V1
    v2::V2
    function ProductVS(v1, v2)
        @assert istrait(AbstractVS{V1})
        @assert istrait(AbstractVS{V2})
        @assert veltype(V1) === veltype(V2)
        new(v1, v2)
    end
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
eltype{V1,V2}(V::Type{ProductVS{V1,V2}}) = veltype(V)

veltype{V1,V2}(::Type{ProductVS{V1,V2}}) = veltype(V1)
function vnewtype{V1,V2}(::Type{ProductVS{V1,V2}}, R::Type)
    ProductVS{vnewtype(V1,R), vnewtype(V2,R)}
end
vdim(x::ProductVS) = vdim(x.v1) + vdim(x.v2)

vnull{V1,V2}(V::Type{ProductVS{V1,V2}}) = V(vnull(V1), vnull(V2))
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
        @assert VS <: Tuple
        @assert nfields(VS) > 0
        S = veltype(fieldtype(VS,1))
        for d in 1:nfields(VS)
            V = fieldtype(VS,d)
            @assert istrait(AbstractVS{V})
            @assert veltype(V) === S
        end
        new(vs)
    end
end

function show(io::IO, x::MultiProductVS)
    S = veltype(typeof(x))
    print(io, "VS{$S}[")
    for d in 1:length(x.vs)
        if d>1 print(io, ",") end
        show(io, x.vs[d])
    end
    print(io, "]")
end

function =={VS}(x::MultiProductVS{VS}, y::MultiProductVS{VS})
    all(map(==, x.vs, y.vs))
end
function isequal{VS}(x::MultiProductVS{VS}, y::MultiProductVS{VS})
    all(map(isequal, x.vs, y.vs))
end
function hash(x::MultiProductVS, h::UInt)
    for i in 1:length(x.vs)
        h = hash(x.vs[i], h)
    end
    hash(typeof(x), h)
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
    if done(x.vs,st)
        # there are no inner elements
        return st,nothing,nothing
    end
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
    done(x.vs,st) && (el===nothing || done(el,sti))
end
eltype{VS}(V::Type{MultiProductVS{VS}}) = veltype(V)

veltype{VS}(::Type{MultiProductVS{VS}}) = veltype(fieldtype(VS,1))
function vnewtype{VS}(::Type{MultiProductVS{VS}}, R::Type)
    RS = map(V->vnewtype(V,R), tupletypes(VS))
    MultiProductVS{Tuple{RS...}}
end
vdim(x::MultiProductVS) = sum(vdim, x.vs)::Int

vnull{VS}(V::Type{MultiProductVS{VS}}) = V(map(vnull, tupletypes(VS)))
function vscale(a, x::MultiProductVS)
    V = typeof(x)
    S = veltype(V)
    a::S
    V(map(x->vscale(a, x), x.vs))
end
function vadd{VS}(x::MultiProductVS{VS}, y::MultiProductVS{VS})
    MultiProductVS{VS}(map(vadd, x.vs, y.vs))
end



# Power of a vector space

export PowerVS

immutable PowerVS{V1,D}
    v1::Array{V1,D}
    function PowerVS(v1)
        @assert istrait(AbstractVS{V1})
        new(v1)
    end
end

function show(io::IO, x::PowerVS)
    V = typeof(x)
    S = veltype(V)
    print(io, "VS{$S}[")
    for i in eachindex(x.v1)
        if i>1 print(io, ",") end
        show(io, x.v1[i])
    end
    print(io, "]")
end

=={V1,D}(x::PowerVS{V1,D}, y::PowerVS{V1,D}) = x.v1 == y.v1
isequal{V1,D}(x::PowerVS{V1,D}, y::PowerVS{V1,D}) = isequal(x.v1, y.v1)
hash(x::PowerVS, h::UInt) = hash(typeof(x), hash(x.v1, h))

"advance outer iterator while inner iterator is done"
function _advance(x::PowerVS, st,el,sti)
    while !done(x.v1,st) && done(el,sti)
        el,st = next(x.v1,st)
        sti = start(el)
    end
    @assert done(x.v1,st) || !done(el,sti)
    st,el,sti
end
function start(x::PowerVS)
    st = start(x.v1)
    if done(x.v1,st)
        # there are no inner elements
        return st,nothing,nothing # TODO: make this type stable
    end
    el,st = next(x.v1,st)
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
    done(x.v1,st) && (el===nothing || done(el,sti))
end
eltype{V1,D}(V::Type{PowerVS{V1,D}}) = veltype(V)

veltype{V1,D}(V::Type{PowerVS{V1,D}}) = veltype(V1)
vnewtype{V1,D}(V::Type{PowerVS{V1,D}}, R::Type) = PowerVS{vnewtype(V1,R),D}
vdim(x::PowerVS) = mapreduce(vdim, +, 0, x.v1)::Int

function vnull{V1,D}(V::Type{PowerVS{V1,D}},
                     sz::NTuple{D,Integer}=ntuple(d->0,D))
    r = Array{V1}(sz)
    @inbounds @simd for i in eachindex(r)
        r[i] = vnull(V1)
    end
    V(r)
end
function vscale(a, x::PowerVS)
    V = typeof(x)
    S = veltype(V)
    a::S
    r = similar(x.v1)
    @inbounds @simd for i in eachindex(r)
        r[i] = vscale(a, x.v1[i])
    end
    V(r)
end
function vadd{V1,D}(x::PowerVS{V1,D}, y::PowerVS{V1,D})
    @assert size(x.v1) == size(y.v1)
    r = similar(x.v1)
    @inbounds @simd for i in eachindex(r)
        r[i] = vadd(x.v1[i], y.v1[i])
    end
    PowerVS{V1,D}(r)
end

# end
