new_var = {
FOR v1 a, v2 b WHERE conditions(a, b)
IF a==b
    TAKE u1 a, u2 b
    PUT a*b
ELSE
    TAKE ...
}

var_name = conditem( # always as 'x' which is reserved name for otput variable
    for=c(a="v1", b="v2"),
    if=quote(conditions(x, a, b)),   # ERROR if 'x' does not exist yet!
    take=c(a="v3", b="v4"),
    where=quote(cond(x, a, b)),      # `== if`  by default
    put=quote(fun(x, a, b))          # taken from `take`
)


##
variable[input_idx(x, a, b)] <- new_value(out_index, x, a, b)


##


##