local function bar()
  local a = 1;
  return function () a = 5; return a; end;
end;
bar();


-- BYTECODE -- closure.lua:3-3
0001    USETN    0   0      ; a ; 5
0002    UGET     0   0      ; a
0003    RET1     0   2

-- BYTECODE -- closure.lua:1-4
0001    KSHORT   0   1
0002    FNEW     1   0      ; closure.lua:3
0003    UCLO     0 => 0004
0004 => RET1     1   2

-- BYTECODE -- closure.lua:0-6
0001    FNEW     0   0      ; closure.lua:1
0002    MOV      1   0
0003    CALL     1   1   1
0004    RET0     0   1
