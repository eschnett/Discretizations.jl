__precompile__()

"""
Tools for discretizations
"""
module Discretizations

using Reexport

include("VectorSpaces.jl")
include("Domains.jl")

@reexport using .VectorSpaces
@reexport using .Domains

end
