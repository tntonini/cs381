--Zachary Taylor: taylozac
--Thomas Tonini:  toninit
----------------------------------------
--                                    --
--               Task 1               --
--                                    --
----------------------------------------
type Var   = String
type Macro = String

-- Pen Status Data Type
data Mode
   = Up
   | Down
  deriving (Eq, Show)


-- Expression Data Type
data Expr
   = Var String
   | Num_ Int
   | Add Expr Expr
  deriving (Eq, Show)

-- Program Data Type
type Prog = [Cmd]

-- Command Data Type
data Cmd
   = Pen Mode
   | Move Expr Expr
   | Define Macro [Expr] Prog
   | Call Cmd
  deriving (Eq, Show)
----------------------------------------


----------------------------------------
--                                    --
--               Task 2               --
--                                    --
----------------------------------------

-- Line Macro Definition
--
-- define line (x1, y1, x2, y2) {
--   pen up; move (x1, x2);
--   pen down; move (x2, y2);
--   pen up
-- }
--

x1 :: Expr
x1 = Num_ 1

y1 :: Expr
y1 = Num_ 2

x2 :: Expr
x2 = Num_ 3

y2 :: Expr
y2 = Num_ 4


line :: Cmd
line = Define "line" [x1, y1, x2, y2]
              [
              Pen Up,
              Move x1 y1,
              Pen Down,
              Move x2 y2,
              Pen Up
              ]
----------------------------------------


----------------------------------------
--                                    --
--               Task 3               --
--                                    --
----------------------------------------
--
-- Nix Macro Definition
--
-- define nix (x, y, w, h) {
--   line (x-w/2, y-h/2, x+w/2, y+h/2);
--   line (x-w/2, y+h/2, x+w/2, y-h/2);
-- }
--

nix :: Cmd
nix = Define "nix" []
             [
             Call line
             ]

x :: Expr
x = Num_ 1

y :: Expr
y = Num_ 2

w :: Expr
w = Num_ 3

h :: Expr
h = Num_ 4

-- nix has to invoke the line macro to draw two seperate diaganol lines creating a X shape
-- to accomplish this we must call line twice passing it 4 variables each time to be able to
-- draw the two straight seperate lines
-- nix = Define "nix" [Var x, Var y, Var w, Var h]
--       [
--       Call "line" [Var x, Var y, Add (Var x) (Var w), Add (Var y) (Var h)],
--       Call "line" [Var x, Add (Var y) (Var h), Add (Var x) (Var w), Var y]
--       ]

-- nix :: Cmd
-- nix = Define "nix" [x, y, w, h] [
--       Call "line" [ x, y, Add  x  w, Add  y  h],
--       Call "line" [Add  x  w, y, x, Add  y  h]]


----------------------------------------
--                                    --
--               Task 4               --
--                                    --
----------------------------------------

steps :: Int -> Prog
steps 0 = []
steps x = [
           Pen Up,
           Move (Num_ x) (Num_ x),
           Pen Down,
           Move (Num_ (x-1)) (Num_ x),
           Move (Num_ (x-1)) (Num_ (x-1))
          ]
          ++ steps (x-1)



----------------------------------------
--                                    --
--               Task 5               --
--                                    --
----------------------------------------

prog_macros :: Cmd -> [Macro]
prog_macros (Define m _ _) = [m]
prog_macros _              = []

macros :: Prog -> [Macro]
macros []      = []
macros (m : p) = (prog_macros m) ++ (macros p)




----------------------------------------
--                                    --
--               Task 6               --
--                                    --
----------------------------------------

expr_val :: [Expr] -> String
expr_val []          = ""
expr_val [Var s]     = s
expr_val [Num_ i]    = show i
expr_val [Add e1 e2] = expr_val [e1] ++ "+" ++ expr_val [e2]
expr_val (e : r)     = expr_val [e] ++ "," ++ expr_val r

extract_macro :: Cmd -> String
extract_macro (Define m _ _) = m

pretty :: Prog -> String
pretty [] = ""
pretty (Pen Up : p)     = "pen up;\n" ++ pretty p
pretty (Pen Down : p)   = "pen down;\n" ++ pretty p
pretty (Move e1 e2 : p) = "move (" ++ expr_val [e1] ++ "," ++ expr_val [e2] ++ ");\n" ++ pretty p
pretty ((Define m e mp) : p)   = "define " ++ m ++ " (" ++ expr_val e ++ ") {\n" ++ pretty mp ++ "}\n" ++ pretty p

pretty (Call c : p) = "call (" ++ pretty [c] ++ ");\n" ++ pretty p


-- Checking this works
expr_var :: Expr
expr_var = Var "test"

-- Checking this works
expr_num :: Expr
expr_num = Num_ 100

-- Checking this works
expr_add :: Expr
expr_add = Add expr_var expr_num

-- Checking this works
cmd_write :: Cmd
cmd_write = Pen Down

-- Checking this works
cmd_move :: Cmd
cmd_move = Move (Num_ 1) (Num_ 2)

-- Checking this works
cmd_define :: Cmd
cmd_define = Define "test_macro" [x1, x2] [cmd_move, cmd_move]

cmd_call :: Cmd
cmd_call = Call cmd_write

-- Checking this works
test_prog = [
             cmd_define,
             Move (Num_ 1) (Num_ 2),
             Define "anutha one" [] [],
             Pen Down,
             Pen Up,
             Define "last macro" [] []
            ]
