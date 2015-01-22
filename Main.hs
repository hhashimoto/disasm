import Test.HUnit
import System.IO
import Data.Char -- digitToInt

-- decimal int -> binary string
int2bin :: Int -> String
int2bin x = reverse $ f x
    where
      f 0 = ""
      f n = n `mod` 2 : f (n `div` 2)

-- hex byte -> binary byte
hex2bin :: String -> String
hex2bin (x1:x2:_) = hex2bin x1 ++ hex2bin x2
hex2bin x = int2bin $ digitToInt x

-- disasm internal
disasmI :: String -> String
disasmI ['1', '0', '0', '1', '0', '0', '1'] = "mov "

-- disasm receive hex string and output assembly statement
disasm :: String -> String
--disasm (x1:x2:xs) = [x1, x2] xs
--disasm _ = ""

tests = TestList
    [ -- DATA TRANSFER

      -- MOV = Move:
      -- Register/Memory to/from Register [100010dw] [mod reg r/m]
      --
      -- 0b10001000 0x88
      -- mode = 00, 00_000_000 ~ 00_111_111
      -- reg -> ignore(=000 fixed)
      -- r/m -> use(=000 -> 111)
      -- == 0b10001000 0x88(d=from, w=byte)
    , "mov d=from w=byte mode=00 reg=AL r/m=000" ~: disasm "8800"     ~?= "mov [bx+si],al"
    , "mov d=from w=byte mode=00 reg=AL r/m=001" ~: disasm "8801"     ~?= "mov [bx+di],al"
    , "mov d=from w=byte mode=00 reg=AL r/m=010" ~: disasm "8802"     ~?= "mov [bp+si],al"
    , "mov d=from w=byte mode=00 reg=AL r/m=011" ~: disasm "8803"     ~?= "mov [bp+di],al"
    , "mov d=from w=byte mode=00 reg=AL r/m=100" ~: disasm "8804"     ~?= "mov [si],al"
    , "mov d=from w=byte mode=00 reg=AL r/m=101" ~: disasm "8805"     ~?= "mov [di],al"
    , "mov d=from w=byte mode=00 reg=AL r/m=110" ~: disasm "88068807" ~?= "mov [0x788],al"
    , "mov d=from w=byte mode=00 reg=AL r/m=111" ~: disasm "8807"     ~?= "mov [bx],al"
      -- == 0b10001001 0x89(d=from, w=word)
      -- 00000000  8900              mov [bx+si],ax
    , "mov d=from w=word mode=00 reg=AX r/m=000" ~: disasm "8900"     =?= "mov [bx+si],ax"
      --
      -- == 0b10001010 0x8A(d=to, w=byte)
      -- 00000000  8A00              mov al,[bx+si]
      -- 00000002  8A01              mov al,[bx+di]
      -- 00000004  8A02              mov al,[bp+si]
      -- 00000006  8A03              mov al,[bp+di]
      -- 00000008  8A04              mov al,[si]
      -- 0000000A  8A05              mov al,[di]
      -- 0000000C  8A06008A          mov al,[0x8a00]
    , "mov d=to w=byte mode=00 reg=AL r/m=000"   ~: disasm "8A00"     =?= "mov al,[bx+si]"
    , "mov d=to w=byte mode=00 reg=AL r/m=001"   ~: disasm "8A01"     =?= "mov al,[bx+di]"
    , "mov d=to w=byte mode=00 reg=AL r/m=010"   ~: disasm "8A02"     =?= "mov al,[bp+si]"
    , "mov d=to w=byte mode=00 reg=AL r/m=011"   ~: disasm "8A03"     =?= "mov al,[bp+di]"
    , "mov d=to w=byte mode=00 reg=AL r/m=100"   ~: disasm "8A04"     =?= "mov al,[si]"
    , "mov d=to w=byte mode=00 reg=AL r/m=101"   ~: disasm "8A05"     =?= "mov al,[di]"
    , "mov d=to w=byte mode=00 reg=AL r/m=110"   ~: disasm "8A06008A" =?= "mov al,[0x8a00]"
      --
      -- == 0b10001011 0x8B(d=to, w=word)
      -- 00000000  8B00              mov ax,[bx+si]
    , "mov d=to w=word mode=00 reg=AX r/m=000"   ~: disasm "8B00"     =?= "mov ax,[bx+si]"
      --
      --
      -- mode = 01
      -- == 0b10001000 0x88(d=from, w=byte)
    , "mov d=from w=byte mode=01 reg=AL r/m=000" ~: disasm "884001"   ~?= "mov [bx+si+0x1],al"
    , "mov d=from w=byte mode=01 reg=AL r/m=001" ~: disasm "884101"   ~?= "mov [bx+di+0x1],al"
    , "mov d=from w=byte mode=01 reg=AL r/m=010" ~: disasm "884201"   ~?= "mov [bp+si+0x1],al"
    , "mov d=from w=byte mode=01 reg=AL r/m=011" ~: disasm "884301"   ~?= "mov [bp+di+0x1],al"
    , "mov d=from w=byte mode=01 reg=AL r/m=100" ~: disasm "884401"   ~?= "mov [si+0x1],al"
    , "mov d=from w=byte mode=01 reg=AL r/m=101" ~: disasm "884501"   ~?= "mov [di+0x1],al"
    , "mov d=from w=byte mode=01 reg=AL r/m=110" ~: disasm "884601"   ~?= "mov [bp+0x1],al"
    , "mov d=from w=byte mode=01 reg=AL r/m=111" ~: disasm "884701"   ~?= "mov [bx+0x1],al"
      -- mode = 10
      -- == 0b10001000 0x88(d=from, w=byte)
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
      -- == 0b10001000 0x88(d=from, w=byte)
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

      -- MOV = Move:
      -- Immediate to Register/Memory [1100011w] [mod 0 0 0 r/m] [data] [data if w = 1]
      --
      -- mode = 00
      -- == 11000110 0xc6(w=byte)
      -- = 0xc6 mod 000 r/m data
      -- = 0xc6 00000 000 data
      -- 00000000  C60000            mov byte [bx+si],0x0
      -- 00000003  C60100            mov byte [bx+di],0x0
      -- 00000006  C60200            mov byte [bp+si],0x0
      -- 00000009  C60300            mov byte [bp+di],0x0
      -- 0000000C  C60400            mov byte [si],0x0
      -- 0000000F  C60500            mov byte [di],0x0
      -- 00000012  C60600C607        mov byte [0xc600],0x7
      -- 00000017  C60700            mov byte [bx],0x0
    , "mov w=byte mode=00 reg=AL r/m=000" ~: disasm "C60000"     ~?= "mov byte [bx+si],0x0"
    , "mov w=byte mode=00 reg=AL r/m=001" ~: disasm "C60100"     ~?= "mov byte [bx+di],0x0"
    , "mov w=byte mode=00 reg=AL r/m=010" ~: disasm "C60200"     ~?= "mov byte [bp+si],0x0"
    , "mov w=byte mode=00 reg=AL r/m=011" ~: disasm "C60300"     ~?= "mov byte [bp+di],0x0"
    , "mov w=byte mode=00 reg=AL r/m=100" ~: disasm "C60400"     ~?= "mov byte [si],0x0"
    , "mov w=byte mode=00 reg=AL r/m=101" ~: disasm "C60500"     ~?= "mov byte [di],0x0"
    , "mov w=byte mode=00 reg=AL r/m=110" ~: disasm "C60600C607" ~?= "mov byte [0xc600],0x7"
    , "mov w=byte mode=00 reg=AL r/m=111" ~: disasm "C60700"     ~?= "mov byte [bx],0x0"
      --
      -- mode = 01
      -- = 0xc6 01 000 000(-> 111)
    , "mov w=byte mode=01 reg=AL r/m=000" ~: disasm "C64000"     ~?= "mov byte [bx+si],0x0"
      --
      --
      --
      -- MOV = Move:
      -- Immediate to Register [1011 w reg] [data] [data if w = 1]
      --
      -- MOV = Move:
      -- Memory to Accumulator [1010000w] [addr-low] [addr-high]
      --
      -- MOV = Move:
      -- Accumulator to Memory [1010001w] [addr-low] [addr-high]
      --
      -- MOV = Move:
      -- Register/Memory to Segment Register [10001110] [mod 0 reg r/m]
      --
      -- MOV = Move:
      -- Segment Register to Register/Memory [10001100] [mod 0 reg r/m]
    ]

main = do
    runTestText (putTextToHandle stderr False) tests
