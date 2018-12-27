{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Asm.Generate where

import Control.Arrow
import Control.Exception
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Functor.Identity
import Data.Monoid
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

import qualified Quad as Q

type Entry = String

type Reg = String

type Offset = Int

newtype Memloc = Memloc { offset :: Int }
    deriving (Eq, Ord)

data Arg
    = Reg Reg
    | Mem Offset
    | Const Int

newtype Env = Env
    { crossVarLocs :: M.Map Q.Var Memloc }

data SEnv = SEnv
    { memLocs :: [Memloc]
    , nextOffset :: Offset
    , savedRegs :: S.Set Reg
    }

fun :: [([Q.Quad], S.Set Q.Var)]
--     ^ List of basic blocks and sets of variables alive at the end of each block
    -> [Q.Var]
--     ^ The function's arguments
    -> String
--     ^ The function's name
    -> [Entry]
--     ^ Assembly instructions for this function
fun blocks args name =
    [ label name
    , pushl (reg ebp)
    , movl (reg esp) (reg ebp)
    , subl (con memSize) (reg esp)
    ]
    <> map (pushl . reg) savedRegs'
    <> code
    <> retValCode
    <> map (popl . reg) savedRegs'
    <>
    [ addl (con memSize) (reg esp)
    , popl (reg ebp)
    , ret
    ]
  where
    memSize = nextOffset
    savedRegs' = S.toList savedRegs
    code = concat codes
    retValCode = case retType of -- TODO
        _         -> let retLoc = M.lookup Q.retVar crossVarLocs
                      in assert (isJust retLoc) $ [movl (var $ fromJust retLoc) (reg eax)]
        otherwise -> []
    ret = case retType of
        _         -> "retl"
        otherwise -> "ret"
    retType = undefined

    (codes, SEnv{..})
        = flip runReader Env{..} . flip runStateT SEnv
            { memLocs    = memLocs'
            , nextOffset = nextOffset'
            , savedRegs  = S.empty
            }
            . forM blocks $ \(b, aliveBegin) ->
                let (b', aliveEnd) = uses b aliveBegin
                 in block b' aliveBegin aliveEnd

    (crossVarLocs, memLocs', nextOffset')
        = foldr (\v (cvls, mls, off) ->
                ( (if M.member v crossVarLocs' then id else M.insert v (Memloc off)) $ cvls
                , Memloc off : mls
                , off - 4))
            (crossVarLocs', memLocs'', -4) $ S.toList crossVars
    (crossVarLocs', memLocs'', _)
        = foldr (\v (cvls, mls, off) ->
                ( (if S.member v crossVars then M.insert v (Memloc off) else id) $ cvls
                , Memloc off : mls
                , off - 4))
            (M.empty, [], 4 * (1 + length args)) $ reverse args
    crossVars = S.unions . map (snd . uncurry uses) $ blocks

-- Compute uses of each variable after each quad and set of vars alive at the beginning of block
-- given the set of vars alive at the end of the block
uses :: [Q.Quad] -> S.Set Q.Var -> ([(Q.Quad, M.Map Q.Var [Int])], S.Set Q.Var)
uses qs aliveEnd = (reverse *** (M.keysSet . fst)) . flip runState start . forM (reverse qs) $ \q -> do
    (u, i) <- get
    put $ (update i q u, i-1)
    pure (q, u)
  where
    update :: Int -> Q.Quad -> M.Map Q.Var [Int] -> M.Map Q.Var [Int]
    update i = \case
        Q.Assign v e         -> flip (foldr $ use i) (vars e) . kill v
        Q.CondJump a1 _ a2 _ -> flip (foldr $ use i) $ vars' [a1, a2]
        Q.Exp e              -> flip (foldr $ use i) $ vars e
        _                    -> id

    use :: Int -> Q.Var -> M.Map Q.Var [Int] -> M.Map Q.Var [Int]
    use i v = M.insertWith (<>) v [i]

    kill :: Q.Var -> M.Map Q.Var [Int] -> M.Map Q.Var [Int]
    kill v = M.delete v

    vars :: Q.Exp -> [Q.Var]
    vars = \case
        Q.BinInt a1 _ a2 -> vars' [a1, a2]
        Q.AddStr a1 a2   -> vars' [a1, a2]
        Q.Val a          -> vars' [a]
        Q.Call _ as      -> vars' as

    vars' :: [Q.Arg] -> [Q.Var]
    vars' = flip (>>=) $ \case
        Q.Var v -> [v]
        _       -> []

    start :: (M.Map Q.Var [Int], Int)
    start = (M.fromSet (const [cnt + 1]) aliveEnd, cnt)

    cnt :: Int
    cnt = length qs

block :: [(Q.Quad, M.Map Q.Var [Int])]
--       ^ List of quads and remaining uses of each variable after each quad
--         represented as positions in the list
      -> S.Set Q.Var
--       ^ Variables alive at the beginning of this block
      -> S.Set Q.Var
--       ^ Variables alive at the end of this block
      -> StateT SEnv (Reader Env) [Entry]
--       ^ Assembly instructions for this block
block qs aliveBegin aliveEnd = undefined

label :: String -> Entry
label = undefined

pushl :: Arg -> Entry
pushl = undefined

movl :: Arg -> Arg -> Entry
movl = undefined

subl :: Arg -> Arg -> Entry
subl = undefined

addl :: Arg -> Arg -> Entry
addl = undefined

popl :: Arg -> Entry
popl = undefined

var :: Memloc -> Arg
var = undefined

reg :: Reg -> Arg
reg = undefined

con :: Int -> Arg
con = undefined

ebp :: Reg
ebp = undefined

esp :: Reg
esp = undefined

eax :: Reg
eax = undefined
