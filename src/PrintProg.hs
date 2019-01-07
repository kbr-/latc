module PrintProg where

import Data.List

import AbsLatte

pProg :: Program a -> String
pProg (Program _ tds) = intercalate "\n" $ map pTopDef tds

pTopDef :: TopDef a -> String
pTopDef (FnDef _ typ ident args body) =
    pType typ ++ " " ++ pId ident ++ "(" ++ pArgs args ++ ") " ++ pBlock body

pType :: Type a -> String
pType (Int _) = "int"

pId :: Ident -> String
pId (Ident x) = x

pArgs :: [Arg a] -> String
pArgs = intercalate ", " . map pArg

pArg :: Arg a -> String
pArg (Arg _ typ ident) = pType typ ++ " " ++ pId ident

pBlock :: Block a -> String
pBlock (Block _ stmts) = "{\n" ++ intercalate "\n" (map pStmt stmts) ++ "\n}"

pStmt :: Stmt a -> String
pStmt (Decl _ typ items) = pType typ ++ " " ++ intercalate ", " (map pItem items) ++ ";"
pStmt (SExp _ exp) = pExp exp ++ ";"
pStmt (Ret _ exp) = "return " ++ pExp exp ++ ";"

pItem :: Item a -> String
pItem (NoInit _ ident) = pId ident
pItem (Init _ ident exp) = pId ident ++ " = " ++ pExp exp

pExp :: Expr a -> String
pExp (EVar _ ident) = pId ident
pExp (ELitInt _ x) = show x
pExp (EApp _ ident as) = pId ident ++ "(" ++ intercalate ", " (map pExp as) ++ ")"
pExp (EMul _ e1 op e2) = "(" ++ pExp e1 ++ " " ++ pMulOp op ++ " " ++ pExp e2 ++ ")"
pExp (EAdd _ e1 op e2) = "(" ++ pExp e1 ++ " " ++ pAddOp op ++ " " ++ pExp e2 ++ ")"

pAddOp :: AddOp a -> String
pAddOp (Plus _) = "+"
pAddOp (Minus _) = "-"

pMulOp :: MulOp a -> String
pMulOp (Times _) = "*"
pMulOp (Div _) = "/"
pMulOp (Mod _) = "%"
