# """
# Vector spaces
# """
# module VectorSpaces

import Base: show
import Base: ==, isequal, hash
import Base: start, next, done, eltype, length

export show
export ==
export start, next, done, eltype, length

using Base.Test



# TODO:
# Check out package "Typeclass"
# Use generated functions, especially for MultiProductVS



export typesame
"Ensure that both types are equal, and return this type"
function typesame{T}(T1::Type{T}, T2::Type{T})
    T1
end



export tupletypes
"Decompose a tuple type into a tuple of types"
tupletypes{U<:Tuple}(T::Type{U}) = ntuple(d->fieldtype(T,d), nfields(T))



# A Scalar needs to support the following operations:
#     -(a)
#     +(a,b)
#     *(a,b)
#     zero(S)
#     one(S)

# Note: Bool is not a scalar in this sense, since typeof(Bool+Bool)!=Bool



# Abstract vector space definition

export AbstractVS
export veltype, vnewtype, vnull, vscale, vadd, vdim

abstract AbstractVS{S}

veltype{T<:AbstractVS}(V::Type{T}) = error("veltype unimplemented for type $T")
vnewtype{T<:AbstractVS}(V::Type{T}, R::Type) =
    error("vnewtype unimplemented for type $T")
vnull{T<:AbstractVS}(V::Type{T}) = error("vnull unimplemented for type $T")

vdim(x::AbstractVS) = error("vdim unimplemented for type $(typeof(x))")
vscale(a, x::AbstractVS) = error("vscale unimplemented for type $(typeof(x))")
vadd(x::AbstractVS, y::AbstractVS) =
    error("vscale unimplemented for types $(typeof(x)) and $(typeof(y))")

eltype{T<:AbstractVS}(V::Type{T}) = veltype(T)
length(x::AbstractVS) = vdim(x)



# The trivial vector space

export EmptyVS

immutable EmptyVS{S} <: AbstractVS{S} end

veltype{S}(V::Type{EmptyVS{S}}) = S
vnewtype{S}(V::Type{EmptyVS{S}}, R::Type) = EmptyVS{R}
vnull{S}(V::Type{EmptyVS{S}}) = V()
vdim(x::EmptyVS) = 0
vscale{S}(a, x::EmptyVS{S}) = EmptyVS{typeof(a * zero(S))}()
vadd{S,T}(x::EmptyVS{S}, y::EmptyVS{T}) = EmptyVS{typeof(zero(S) + zero(T))}()

show{S}(io::IO, x::EmptyVS{S}) = print(io, "VS{$S}[]")

start(x::EmptyVS) = nothing
next(x::EmptyVS, state) = throw(BoundsError(x))
done(x::EmptyVS, state) = true



# A scalar vector space

export ScalarVS

immutable ScalarVS{S} <: AbstractVS{S}
    elt::S
    ScalarVS(elt) = new(elt)
end

veltype{S}(V::Type{ScalarVS{S}}) = S
vnewtype{S}(V::Type{ScalarVS{S}}, R::Type) = ScalarVS{R}
vnull{S}(V::Type{ScalarVS{S}}) = V(zero(S))
vdim(x::ScalarVS) = 1
function vscale(a, x::ScalarVS)
    r = a * x.elt
    ScalarVS{typeof(r)}(r)
end
function vadd(x::ScalarVS, y::ScalarVS)
    r = x.elt + y.elt
    ScalarVS{typeof(r)}(r)
end

function show{S}(io::IO, x::ScalarVS{S})
    print(io, "VS{$S}[")
    show(io, x.elt)
    print(io, "]")
end

start(x::ScalarVS) = false
next(x::ScalarVS, state) = (x.elt, true)
done(x::ScalarVS, state) = state



# TODO: Sum (i.e. union) of two vector spaces?



# Product of two vector spaces

export ProductVS

immutable ProductVS{S,V1,V2} <: AbstractVS{S}
    v1::V1
    v2::V2
    function ProductVS(v1, v2)
        # TODO: Use concepts
        @assert V1 <: AbstractVS{S}
        @assert V2 <: AbstractVS{S}
        new(v1, v2)
    end
end

veltype{S,V1,V2}(V::Type{ProductVS{S,V1,V2}}) = S
vnewtype{S,V1,V2}(V::Type{ProductVS{S,V1,V2}}, R::Type) = ProductVS{R,V1,V2}
vnull{S,V1,V2}(V::Type{ProductVS{S,V1,V2}}) = V(vnull(V1), vnull(V2))
vdim(x::ProductVS) = vdim(x.v1) + vdim(x.v2)
function vscale(a, x::ProductVS)
    r1,r2 = vscale(a, x.v1), vscale(a, x.v2)
    R1,R2 = typeof(r1), typeof(r2)
    S = typesame(veltype(R1), veltype(R2))
    ProductVS{S,R1,R2}(r1, r2)
end
function vadd(x::ProductVS, y::ProductVS)
    r1,r2 = vadd(x.v1, y.v1), vadd(x.v2, y.v2)
    R1,R2 = typeof(r1), typeof(r2)
    S = typesame(veltype(R1), veltype(R2))
    ProductVS{S,R1,R2}(r1, r2)
end

function show{S,V1,V2}(io::IO, x::ProductVS{S,V1,V2})
    print(io, "VS{$S}[")
    show(io, x.v1)
    print(io, ",")
    show(io, x.v2)
    print(io, "]")
end

start(x::ProductVS) = (start(x.v1), start(x.v2))
function next{S,V1,V2}(x::ProductVS{S,V1,V2}, state)
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



# Products with multiple factors, represented as tuples

export MultiProductVS

immutable MultiProductVS{S,VS} <: AbstractVS{S}
    vs::VS
    function MultiProductVS(vs)
        @assert VS <: Tuple
        for d in 1:nfields(VS)
            @assert fieldtype(VS,d) <: AbstractVS{S} 
        end
        new(vs)
    end
end

veltype{S,VS}(V::Type{MultiProductVS{S,VS}}) = S
vnewtype{S,VS}(V::Type{MultiProductVS{S,VS}}, R::Type) = MultiProductVS{R,VS}
vnull{S,VS}(V::Type{MultiProductVS{S,VS}}) = V(map(vnull, tupletypes(VS)))
vdim(x::MultiProductVS) = mapreduce(vdim, +, 0, x.vs)
function vscale(a, x::MultiProductVS)
    rs = map(v->vscale(a, v), x.vs)
    RS = map(typeof, rs)
    if isempty(RS)
        S = typeof(a * zero(veltype(typeof(x))))
    else
        S = mapreduce(veltype, typesame, RS)
    end
    MultiProductVS{S,Tuple{RS...}}(rs)
end
# TODO

function show{S,VS}(io::IO, x::MultiProductVS{S,VS})
    print(io, "VS{$S}[")
    for d in 1:length(x.vs)
        if d>1 print(io, ",") end
        show(io, x.vs[d])
    end
    print(io, "]")
end

# TODO



# Power of a vector space

export PowerVS

immutable PowerVS{S,V1,D} <: AbstractVS{S}
    v1::Array{V1,D}
    function PowerVS(v1)
        V1 <: AbstractVS{S}
        new(v1)
    end
end

veltype{S,V1,D}(V::Type{PowerVS{S,V1,D}}) = S
vnewtype{S,V1,D}(V::Type{PowerVS{S,V1,D}}, R::Type) = PowerVS{R,V1,D}
function vnull{S,V1,D}(V::Type{PowerVS{S,V1,D}},
                       sz::NTuple{D,Integer}=ntuple(d->0,D))
    r = Array{V1}(sz)
    @inbounds @simd for i in eachindex(r)
        r[i] = vnull(V1)
    end
    V(r)
end
vdim(x::PowerVS) = mapreduce(vdim, +, 0, x.v1)
function vscale{S,V1,D}(a, x::PowerVS{S,V1,D})
    R = typeof(a * zero(S))
    W1 = vnewtype(V1,R)
    r = similar(x.v1, W1)
    @inbounds @simd for i in eachindex(r)
        r[i] = vscale(a, x.v1[i])
    end
    PowerVS{R,W1,D}(r)
end
function vadd{S1,V1,S2,V2,D}(x::PowerVS{S1,V1,D}, y::PowerVS{S2,V2,D})
    R = typeof(zero(S1) + zero(S2))
    W1 = vnewtype(V1,R)
    @assert size(x.v1) == size(y.v1)
    r = similar(x.v1, W1)
    @inbounds @simd for i in eachindex(r)
        r[i] = vadd(x.v1[i], y.v1[i])
    end
    PowerVS{R,W1,D}(r)
end

function show{S,V1,D}(io::IO, x::PowerVS{S,V1,D})
    print(io, "VS{$S}[")
    for i in eachindex(x.v1)
        show(io, x.v1[i])
        if i>1 print(io, ",") end
    end
    print(io, "]")
end

==(x::PowerVS, y::PowerVS) = x.v1 == y.v1
isequal(x::PowerVS, y::PowerVS) = isequal(x.v1, y.v1)
hash(x::PowerVS, h::UInt) = hash(typeof(x), hash(x.v1, hash(x.v2, h)))

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
        st,nothing,nothing      # TODO: make this type-stable
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

# end
