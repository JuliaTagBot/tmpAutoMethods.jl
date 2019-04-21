module AutoMethods
export @auto


macro auto(e)
    definitions = auto(__module__, e)
    esc(Expr(:block, definitions...))
end

function auto(mod::Module, e::Expr)
    e.head in [:(=), :function] && e.args[1] isa Expr ||
        throw(ArgumentError("expected function definition"))

    wherevars = []
    while e.args[1].head == :where
        append!(wherevars, e.args[1].args[2:end])
        e.args[1] = e.args[1].args[1] # delete where
    end
    e.args[1] isa Expr && e.args[1].head == :call ||
        throw(ArgumentError("expected function definition"))

    fname = e.args[1].args[1]
    fargs = @view e.args[1].args[2:end] # this will be mutated
    defaults = [] # mapping: arg indice => default value
    argnames = []
    for i = eachindex(fargs)
        if fargs[i] isa Expr && fargs[i].head == :(:=)
            push!(defaults, i => fargs[i].args[2])
            fargs[i] = fargs[i].args[1] # remove `:= default` part
        end
        push!(argnames, getargname(fargs[i]))
    end
    linefile = e.args[2].args[1]
    @assert linefile isa LineNumberNode
    defs = pushfirst!(gendefinitions(fname, fargs, defaults, argnames, linefile, wherevars),
                      e => wherevars)
    wherewrap!.(Iterators.flatten(whereexpand.(Ref(mod), defs)))
end

function gendefinitions(fname, fargs, defaults, argnames, linefile, wherevars)
    length(defaults) > 31 &&
        throw(ArgumentError(
            "number of defaults ($(length(defs))) exceeded maximum (31)"))

    definitions = []
    for n = 1:(2^length(defaults)-1)
        sig = copy(fargs)
        an = copy(argnames)
        for b = 31:-1:1
            if n & (1 << (b-1)) != 0
                idx, defval = defaults[b]
                an[idx]::Symbol
                # this type assert catches the case where we have an expression like `f(x... := 1) = ...`
                # this case is handled fine with normal default values (as `x...` must be the last parameter)
                # so there is no strong need to re-implement it with `:=`
                deleteat!(sig, idx)
                # handle cases with references to previous args,
                # like f(x:=1, y:=typeof(x))
                replacesym!(@view(sig[idx:end]), # not idx+1, as idx was just deleted
                            an[idx] => defval)
                # we replace an[idx] => defval, but also all other positions on the right in `an`,
                # because they might contain references to an[idx], like in the example above (where
                # `an` can contain :(typeof(x)) and `x` is being replaced by 1)
                replacesym!(@view(an[idx:end]), an[idx] => defval)
            end
        end
        newdef = :($fname($(sig...)) = $fname($(an...)))
        newvars = wherevars[findtypevars(newdef, wherevars)]
        @assert newdef.args[2].args[1] isa LineNumberNode
        newdef.args[2].args[1] = linefile
        push!(definitions, newdef => newvars)
    end
    definitions
end

function whereexpand(mod::Module, (newdef, newvars)::Pair)
    newdefs = []
    loopidx = findall(v -> v isa Expr && v.head == :(=), newvars)
    loopvars = Symbol[newvars[idx].args[1] for idx = loopidx]
    for vals = Iterators.product((Core.eval(mod, newvars[idx].args[2]) for idx = loopidx)...)
        def = copy(newdef)
        for i = eachindex(vals)
            replacesym!([def], loopvars[i] => vals[i])
        end
        push!(newdefs, def)
    end
    [(def => newvars[[i for i in eachindex(newvars) if i ∉ loopidx]]) for def in newdefs]
end

function replacesym!(args::AbstractArray, (sym, val)::Pair)
    for i in eachindex(args)
        if args[i] isa Expr
            replacesym!(args[i].args, sym => val)
        elseif args[i] === sym
            args[i] = val
        end
    end
end

function wherewrap!((def, wherevars))
    if !isempty(wherevars)
        def.args[1] = Expr(:where, def.args[1], wherevars...)
    end
    def
end

findtypevars(e::Expr, vars) = findtypevars!(falses(length(vars)), e, vars)

function findtypevars!(found, e::Expr, vars)
    for a in e.args
        all(found) && break # handles well empty case
        findtypevars!(found, a, vars)
    end
    found
end

function findtypevars!(found, e, vars)
    i = findfirst(==(e) ∘ gettypevar, vars)
    i !== nothing && (found[i] = true)
    found
end

# extract typevar in where expressions, like :X from :(Int <: X <: Integer)
gettypevar(varexpr::Symbol) = varexpr

function gettypevar(varexpr::Expr)
    if varexpr.head in (:>:,:<:)
        varexpr.args[1]::Symbol
    elseif varexpr.head == :comparison # e.g. :(X <: Y <: Z)
        length(varexpr.args) != 5 && throw(ArgumentError("unsupported definition"))
        varexpr.args[3]::Symbol
    elseif varexpr.head == :(=)
        varexpr.args[1]::Symbol # this typevar is temporary and will be replaced
    else
        throw(ArgumentError("unsupported definition"))
    end
end

function getargname(arg::Expr)
    if arg.head == :(::)
        if length(arg.args) == 2
            arg.args[1]
        else # length == 1
            arg1 = arg.args[1]
            arg1 isa Expr && arg1.head == :curly && arg1.args[1] == :Type ||
                throw(ArgumentError("unsupported argument"))
            arg1.args[2]
        end
    elseif arg.head == :kw
        getargname(arg.args[1])
    elseif arg.head == :...
        Expr(:..., getargname(arg.args[1]))
    else
        throw(ArgumentError("unsupported definition"))
    end
end

getargname(arg::Symbol) = arg
getargname(arg) = throw(ArgumentError("unsupported definition"))


end # module
