local function foo(a, b)
    local x = 7
    return function (c, d)
        return function (e, f)
            return a + b + e + f + x;
        end
    end
end

foo(1, 2)(3, 4)(5, 6)

--[[

RESUTL 21

-- BYTECODE -- closure.lua:4-6
0001    UGET     2   0      ; a
0002    UGET     3   1      ; b
0003    ADDVV    2   2   3
0004    ADDVV    2   2   0
0005    ADDVV    2   2   1
0006    UGET     3   2      ; x
0007    ADDVV    2   2   3
0008    RET1     2   2

-- BYTECODE -- closure.lua:3-7
0001    FNEW     2   0      ; closure.lua:4
0002    UCLO     0 => 0003
0003 => RET1     2   2

-- BYTECODE -- closure.lua:1-8
0001    KSHORT   2   7
0002    FNEW     3   0      ; closure.lua:3
0003    UCLO     0 => 0004
0004 => RET1     3   2

-- BYTECODE -- closure.lua:0-13
0001    FNEW     0   0      ; closure.lua:1
0002    MOV      1   0
0003    KSHORT   2   1
0004    KSHORT   3   2
0005    CALL     1   2   3
0006    KSHORT   2   3
0007    KSHORT   3   4
0008    CALL     1   2   3
0009    KSHORT   2   5
0010    KSHORT   3   6
0011    CALL     1   1   3
0012    RET0     0   1

]]--
