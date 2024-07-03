{- Test for Program -}
module TestProgram where

import Program

p0, p1, p2, p3, p4, p5, p6, p7 :: Program.T

p0 = fromString  ("\
\read k;\
\read n;\
\m := 1;\
\while n-m do\
\  begin\
\    if m - m/k*k then\
\      skip;\
\    else\
\      write m;\
\    m := m + 1;\
\  end")

p1 = fromString  ("\
\read n;\
\read b;\
\m := 1;\
\s := 0;\
\p := 1;\
\while n do\
\  begin\
\    q := n/b;\
\    r := n - q*b;\
\    write r;\
\    s := p*r+s;\                    
\    p := p*10;\
\    n :=q;\
\  end\
\write s;")

s1 = "\
\read n;\
\read b;\
\m := 1;\
\s := 0;\
\p := 1;\
\while n do\
\  begin\
\    q := n/b;\
\    r := n - q*b;\
\    write r;\
\    s := p*r+s;\                    
\    p := p*10;\
\    n :=q;\
\  end\
\write s;"

sp = putStr (toString p0)

sp1 = putStr (toString p1)

p2 = fromString (toString p0)

p3 = fromString (toString p1)

rp0 = Program.exec p0 [3,16] ;;Expected output: [3,6,9,12,15]

rp1 = Program.exec p1 [1024, 2] ;;Expected output: [0,0,0,0,0,0,0,0,0,0,1,10000000000]

s4 = "\
\read a;\
\read b;\
\-- a comment\n\
\s := 3;\
\while a do\
\  begin\
\    c := a^s;\
\    d := 2^a;\
\    write c;\
\    write d;\                    
\    a := a-1;\
\  end\
\write a;"

p4 = fromString s4

rp4 = Program.exec p4 [4,4] ;;Expected output: [64,16,27,8,8,4,1,2,0]


s5 = "begin read p; while p do begin write p; p := p - 1; end end"

p5 = fromString s5

rp5 = Program.exec p5 [3] ;; Expected output: [3,2,1]



s6 = "begin read a; read b; write a; write b; end"
p6 = fromString s6
rp6 = Program.exec p6 [2, 4] ;; Expected output: [2, 4]. Current result!!: [4, 2]


s7 = "read a; read b; write a; write b;"
p7 = fromString s7
rp7 = Program.exec p6 [2, 4] ;; Expected output: [2, 4]. Current result!!: [4, 2]