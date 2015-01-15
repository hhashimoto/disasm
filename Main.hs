import Test.HUnit
import System.IO

tests = TestList
    [ -- DATA TRANSFER
      -- MOV = Move:
      -- Register/Memory to/from Register [100010dw] [mod reg r/m]
      -- 0b10001000 0x88
      -- mode = 00, 00_000_000 ~ 00_111_111
      -- reg -> ignore(=000 fixed)
      -- r/m -> use(=000 -> 111)
    , "mov d=from w=byte mode=00 reg=AL r/m=000" ~: disasm "8800"     ~?= "mov [bx+si],al"
    , "mov d=from w=byte mode=00 reg=AL r/m=001" ~: disasm "8801"     ~?= "mov [bx+di],al"
    , "mov d=from w=byte mode=00 reg=AL r/m=010" ~: disasm "8802"     ~?= "mov [bp+si],al"
    , "mov d=from w=byte mode=00 reg=AL r/m=011" ~: disasm "8803"     ~?= "mov [bp+di],al"
    , "mov d=from w=byte mode=00 reg=AL r/m=100" ~: disasm "8804"     ~?= "mov [si],al"
    , "mov d=from w=byte mode=00 reg=AL r/m=101" ~: disasm "8805"     ~?= "mov [di],al"
    , "mov d=from w=byte mode=00 reg=AL r/m=110" ~: disasm "88068807" ~?= "mov [0x788],al"
    , "mov d=from w=byte mode=00 reg=AL r/m=111" ~: disasm "8807"     ~?= "mov [bx],al"
      -- mode = 01
    , "mov d=from w=byte mode=01 reg=AL r/m=000" ~: disasm "884001"   ~?= "mov [bx+si+0x1],al"
    , "mov d=from w=byte mode=01 reg=AL r/m=001" ~: disasm "884101"   ~?= "mov [bx+di+0x1],al"
    , "mov d=from w=byte mode=01 reg=AL r/m=010" ~: disasm "884201"   ~?= "mov [bp+si+0x1],al"
    , "mov d=from w=byte mode=01 reg=AL r/m=011" ~: disasm "884301"   ~?= "mov [bp+di+0x1],al"
    , "mov d=from w=byte mode=01 reg=AL r/m=100" ~: disasm "884401"   ~?= "mov [si+0x1],al"
    , "mov d=from w=byte mode=01 reg=AL r/m=101" ~: disasm "884501"   ~?= "mov [di+0x1],al"
    , "mov d=from w=byte mode=01 reg=AL r/m=110" ~: disasm "884601"   ~?= "mov [bp+0x1],al"
    , "mov d=from w=byte mode=01 reg=AL r/m=111" ~: disasm "884701"   ~?= "mov [bx+0x1],al"
      -- mode = 10
      -- 00000000  88800123          mov [bx+si+0x2301],al
      -- 00000004  88810123          mov [bx+di+0x2301],al
      -- 00000008  88820123          mov [bp+si+0x2301],al
      -- 0000000C  88830123          mov [bp+di+0x2301],al
      -- 00000010  88840123          mov [si+0x2301],al
      -- 00000014  88850123          mov [di+0x2301],al
      -- 00000018  88860123          mov [bp+0x2301],al
      -- 0000001C  88870123          mov [bx+0x2301],al
    , "mov d=from w=byte mode=10 reg=AL r/m=000" ~: disasm "88800123"   ~?= "mov [bx+si+0x2301],al"
    , "mov d=from w=byte mode=10 reg=AL r/m=001" ~: disasm "88810123"   ~?= "mov [bx+di+0x2301],al"
    , "mov d=from w=byte mode=10 reg=AL r/m=010" ~: disasm "88820123"   ~?= "mov [bp+si+0x2301],al"
    , "mov d=from w=byte mode=10 reg=AL r/m=011" ~: disasm "88830123"   ~?= "mov [bp+di+0x2301],al"
    , "mov d=from w=byte mode=10 reg=AL r/m=100" ~: disasm "88840123"   ~?= "mov [si+0x2301],al"
    , "mov d=from w=byte mode=10 reg=AL r/m=101" ~: disasm "88850123"   ~?= "mov [di+0x2301],al"
    , "mov d=from w=byte mode=10 reg=AL r/m=110" ~: disasm "88860123"   ~?= "mov [bp+0x2301],al"
    , "mov d=from w=byte mode=10 reg=AL r/m=111" ~: disasm "88870123"   ~?= "mov [bx+0x2301],al"
      -- mode = 11
      -- 00000000  88C0              mov al,al
      -- 00000002  88C1              mov cl,al
      -- 00000004  88C2              mov dl,al
      -- 00000006  88C3              mov bl,al
      -- 00000008  88C4              mov ah,al
      -- 0000000A  88C5              mov ch,al
      -- 0000000C  88C6              mov dh,al
      -- 0000000E  88C7              mov bh,al
    , "mov d=from w=byte mode=11 reg=AL r/m=000" ~: disasm "88C0"   ~?= "mov al,al"
    , "mov d=from w=byte mode=11 reg=AL r/m=001" ~: disasm "88C1"   ~?= "mov cl,al"
    , "mov d=from w=byte mode=11 reg=AL r/m=010" ~: disasm "88C2"   ~?= "mov dl,al"
    , "mov d=from w=byte mode=11 reg=AL r/m=011" ~: disasm "88C3"   ~?= "mov bl,al"
    , "mov d=from w=byte mode=11 reg=AL r/m=100" ~: disasm "88C4"   ~?= "mov ah,al"
    , "mov d=from w=byte mode=11 reg=AL r/m=101" ~: disasm "88C5"   ~?= "mov ch,al"
    , "mov d=from w=byte mode=11 reg=AL r/m=110" ~: disasm "88C6"   ~?= "mov dh,al"
    , "mov d=from w=byte mode=11 reg=AL r/m=111" ~: disasm "88C7"   ~?= "mov bh,al"
      -- 0b10001001 0x89(d=from, w=word)
      -- 0b10001010 0x8A(d=to, w=byte)
      -- 0b10001011 0x8B(d=to, w=word)
    ]

main = do
    runTestText (putTextToHandle stderr False) tests
