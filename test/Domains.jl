# function test_Point{P}(::Type{P})
#     T = eltype(P)
#     D = length(P)
#     o = origin(P)
#     @test isa(o, P)
#     @test all(o .== T(0))
# end
# function test_points()
#     test_Point(Point{Int8,0})
#     test_Point(Point{Int,1})
#     test_Point(Point{BigFloat,2})
#     test_Point(Point{Float64,4})
# end
# test_points()



# @test istrait(Domain{EmptyDomain{Int,2}})
# @test istrait(Domain{BoxDomain{Float64,3}})
