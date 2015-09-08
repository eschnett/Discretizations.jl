# Discretizations.jl

Abstractions for discrete representations of fields to solve PDEs

[![Build Status](https://travis-ci.org/eschnett/Discretizations.jl.svg?branch=master)](https://travis-ci.org/eschnett/Discretizations.jl)
[![Build status](https://ci.appveyor.com/api/projects/status/oqolvv6ymosytrf0?svg=true)](https://ci.appveyor.com/project/eschnett/discretizations-jl)
[![Coverage Status](https://coveralls.io/repos/eschnett/Discretizations.jl/badge.svg?branch=master&service=github)](https://coveralls.io/github/eschnett/Discretizations.jl?branch=master)

This Julia package contains abstractions to represent discretizations
of fields, as needed to solve PDEs. Currently, this is mostly an
implementation of `VectorSpace`, with `Domain` and `Field` to follow.
