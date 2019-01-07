{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ExistentialQuantification #-}
module Asm.Generate where

import Control.Arrow
import Control.Exception
import Control.Monad.Extra
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.RWS.Strict
import Control.Lens
import Data.Functor.Identity
import Data.Monoid
import Data.Maybe
import Data.Function
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace

import qualified Quad as Q
import qualified PrintQuad as P
import Intermediate.Liveness

type Entry = String

newtype Reg = Reg { regName :: String }
    deriving (Eq, Ord, Show)

type Offset = Integer

newtype Memloc = Memloc { offset :: Integer }
    deriving (Eq, Ord, Show)

data Arg
    = AReg Reg
    | AMem Memloc
    | AConst Integer
    | ALab Q.Label

newtype FunEnv = FunEnv
    { crossVarLocs :: M.Map Q.Var Memloc
--    ^ Memory locations of cross variables, i.e. variables alive between blocks
    }

data BlockEnv = BlockEnv
    { endLoc   :: M.Map Q.Var Memloc
    , blockLen :: Int
    }

data FunSt = FunSt
    { _memLocs    :: [Memloc]
    , _nextOffset :: Offset
    , _savedRegs  :: S.Set Reg
    }

data BlockSt = BlockSt
    { _loc     :: M.Map Q.Var ([Memloc], Maybe Reg)
    , _memVars :: M.Map Memloc [Q.Var]
    , _regVars :: M.Map Reg [Q.Var]
    , _funSt   :: FunSt
    }

type Z = RWS BlockEnv [Entry] BlockSt

makeLenses ''FunSt
makeLenses ''BlockSt

data TopDef = FunDef
    { blocks :: [([Q.Quad], S.Set Q.Var)]
    , rets   :: Bool
    , name   :: String
    , args   :: [Q.Var]
    }

class IsLoc a where
    addVar :: MonadState BlockSt m => Q.Var -> a -> m ()
    delVar :: MonadState BlockSt m => Q.Var -> a -> m ()
    getVars :: MonadState BlockSt m => a -> m [Q.Var]

instance IsLoc Memloc where
    addVar = addVarM
    delVar = delVarM
    getVars = getVarsM

instance IsLoc Reg where
    addVar = addVarR
    delVar = delVarR
    getVars = getVarsR

data Loc = forall l. IsLoc l => Loc l

instance IsLoc Loc where
    addVar v (Loc l) = addVar v l
    delVar v (Loc l) = delVar v l
    getVars (Loc l) = getVars l

program :: Q.Consts -> [TopDef] -> [Entry]
program consts ds = globlSection <> concatMap topDef ds <> constSection
  where
    topDef FunDef{..} = fun blocks rets args name
    globlSection = [globl "main"]
    constSection = concatMap (\(s, l) -> [label l, asciz s]) consts

fun :: [([Q.Quad], S.Set Q.Var)]
--     ^ List of basic blocks and sets of variables alive at the end of each block
    -> Bool
--     ^ Whether the function returns a value
    -> [Q.Var]
--     ^ The function's arguments
    -> String
--     ^ The function's name
    -> [Entry]
--     ^ Assembly instructions for this function
fun blocks rets args name =
    [ label $ Q.Label name
    , pushl (reg ebp)
    , movl (reg esp) (reg ebp)
    , subl (con memSize) (reg esp)
    ]
    <> map (pushl . reg) savedRegs
    <> code
    <> retValCode
    <> map (popl . reg) (reverse savedRegs)
    <>
    [ addl (con memSize) (reg esp)
    , popl (reg ebp)
    , retl
    ]
  where
    memSize = abs _nextOffset - 4
    savedRegs = S.toList _savedRegs
    retValCode = if not rets then [] else
        let retLoc = M.lookup Q.retVar crossVarLocs
         in assert (isJust retLoc) $ [movl (mem $ fromJust retLoc) (reg eax)]

    (FunSt{..}, code) = execRWS
        (forM blocks $ \(b, aliveEnd) ->
            let (b', aliveBegin) = nextUses b aliveEnd in block b' aliveBegin aliveEnd)
            FunEnv{..}
            FunSt
                { _memLocs    = memLocs
                , _nextOffset = nextOffset
                , _savedRegs  = S.empty
                }

    -- Allocate a memloc for every argument and every cross variable.
    -- If a variable is both an argument and a cross variable, allocate only one memloc.
    (crossVarLocs, memLocs, nextOffset) = foldr
        (\v (cvls, mls, off) ->
            if M.member v crossVarLocs'
                then (cvls, mls, off)
                else (M.insert v (Memloc off) cvls, Memloc off : mls, off - 4))
        (crossVarLocs', memLocs', -4) $ S.toList crossVars
    (crossVarLocs', memLocs', _) = foldr
        (\v (cvls, mls, off) ->
            ( (if S.member v crossVars then M.insert v (Memloc off) else id) $ cvls
            , Memloc off : mls
            , off - 4))
        (M.empty, [], fromIntegral $ 4 * (1 + length args)) args
    crossVars = (S.insert Q.retVar) . S.unions . map (snd . uncurry nextUses) $ blocks

block ::
    [(Q.Quad, M.Map Q.Var Use)]
--  ^ List of quads and next use of each variable after each quad
--    represented as positions in the list
 -> S.Set Q.Var
--  ^ Variables alive at the beginning of this block
 -> S.Set Q.Var
--  ^ Variables alive at the end of this block
 -> RWS FunEnv [Entry] FunSt ()
block qs aliveBegin aliveEnd = do
    cvls <- reader crossVarLocs
    let (startLoc, startMemVars, endLoc) = M.foldrWithKey (\v l (sL, sM, eL) ->
            ( if S.member v aliveBegin then M.insert v ([l], Nothing) sL else sL
            , if S.member v aliveBegin then M.insert l [v] sM else sM
            , if S.member v aliveEnd then M.insert v l eL else eL)) (M.empty, M.empty, M.empty) cvls
        blockLen = length qs
    desc <- BlockSt startLoc startMemVars M.empty <$> get
    let (_funSt -> s, code) = execRWS (mapM (uncurry quad) qs *> saveEndLoc) BlockEnv{..} desc
    tell code
    put s

deb :: Z ()
deb = do
   BlockSt{..} <- get
   traceM $ "desc:\n" <> "loc:\n" <> show _loc <> "\nregVars:\n" <> show _regVars <> "\nmemVars:\n" <> show _memVars <> "\n"

quad ::
    Q.Quad
--  ^ Quad to generate assembly for
 -> M.Map Q.Var Use
--  ^ Use of each variable after this quad
 -> Z ()
quad q nextUses = {- deb <* traceM ("quad: " ++ P.printQuad q) <* -}case q of
    Q.Assign v (Q.BinInt v1 op v2) | not $ elem op [Q.Div, Q.Mod] -> do
        let avs' = considerDead (Q.Var v) avs
            avs'' = if v1 == v2 then avs' else considerAlive v2 avs'
        when (alive avs v) $ do
            r <- chooseRegister [] avs'' (Just v) $ case v1 of { Q.Var v' -> Just v'; _ -> Nothing }
            whenM (dirtyReg avs'' r) $ spill avs' r
            unlessM ((== Just r) <$> getReg' v1) $ do
                m <- locate v1
                emitC (movl m (reg r)) $ P.printArg v1
            clear r
            m <- if v1 == v2 then pure $ reg r else locate v2

            emitC (oper op m (reg r)) $ P.printQuad q
            remove v
            addVar v r

            when (calleeSave r) $ save r
        freeDesc v1
        freeDesc v2
    Q.Assign v (Q.BinInt v1 op v2) -> do
        let avs' = considerDead (Q.Var v) avs
            avs'' = if v1 == v2 then avs' else considerAlive v2 avs'
            avs''' = considerAlive v1 avs''
        when (alive avs v) $ do
            inEax <- (== Just eax) <$> getReg' v1

            m2 <- locate v2 >>= \case
                m@(AConst _) -> do
                    r <- chooseRegister [eax, edx] avs''' Nothing Nothing
                    whenM (dirtyReg avs''' r) $ spill avs' r

                    emit $ movl m (reg r)
                    clear r

                    pure $ reg r
                m@(AReg r') | r' == eax || r' == edx -> do
                    r <- chooseRegister [eax, edx] avs''' Nothing Nothing
                    whenM (dirtyReg avs''' r) $ spill avs' r

                    emit $ movl m (reg r)
                    move r' r

                    pure $ reg r
                m -> pure m

            whenM (dirtyReg avs'' eax) $ spill avs' eax
            -- so that vars from eax don't get spilled over from edx
            clear eax

            unless inEax $ do
                m <- locate v1
                emitC (movl m (reg eax)) $ P.printArg v1

            whenM (dirtyReg avs'' edx) $ spill avs' edx

            emit $ cdq
            emitC (idivl m2) $ P.printQuad q
            remove v
            clear eax
            clear edx
            addVar v $ case op of
                Q.Div -> eax
                Q.Mod -> edx
        freeDesc v1
        freeDesc v2
    Q.Assign v (Q.Load l) -> do
        let avs' = considerDead (Q.Var v) avs
        when (alive avs v) $ do
            r <- chooseRegister [] avs' (Just v) Nothing
            whenM (dirtyReg avs' r) $ spill avs r

            emitC (leal (lab l) (reg r)) $ P.printArg (Q.Var v)
            remove v
            clear r
            addVar v r
    Q.Assign v (Q.Val (Q.Var v')) -> do
        when (alive avs v && not (v == v')) $ do
            remove v
            mapM_ (addVar v) =<< getLocs v'
        freeDesc $ Q.Var v'
    Q.Assign v (Q.Val (Q.ConstI i)) -> do
        when (alive avs v) $ do
            m <- chooseMem [v]

            emitC (movl (con i) (mem m)) $ P.printQuad q
            remove v
            clear m
            addVar v m
    Q.Assign v (Q.Call f as) -> do
        let avs' = considerDead (Q.Var v) avs
        call avs' f as
        when (alive avs v) $ do
            remove v
            addVar v eax
    Q.Jump l -> saveEndLoc *> emit (jmp l)
    Q.Mark l -> emit $ label l
    Q.CondJump v1 op v2 l -> do
        let avs' = considerAlive v2 avs
        m1 <- locate v1
        m2 <- locate v2
        (m1, m2) <- case (m1, m2) of
            (AMem _, AMem _) -> do
                r <- chooseRegister [] avs' (Just $ case v1 of Q.Var v -> v) Nothing
                whenM (dirtyReg avs' r) $ spill avs r
                AMem m <- locate v1

                emitC (movl (mem m) (reg r)) $ P.printArg v1
                copy m r

                when (calleeSave r) $ save r

                (reg r, ) <$> locate v2
            (AConst _, _) -> do
                r <- chooseRegister [] avs' Nothing Nothing
                whenM (dirtyReg avs' r) $ spill avs r

                emit $ movl m1 (reg r)
                clear r

                when (calleeSave r) $ save r

                (reg r, ) <$> locate v2
            (_, _) -> pure (m1, m2)
        emitC (cmpl m2 m1) $ P.printQuad q
        freeDesc v1
        freeDesc v2
        saveEndLoc
        emit $ jop op l
    Q.Exp (Q.Call f as) -> call avs f as
    Q.Exp _ -> pure ()
  where
    avs :: S.Set Q.Var
    avs = M.keysSet nextUses

    considerAlive :: Q.Arg -> S.Set Q.Var -> S.Set Q.Var
    considerAlive (Q.Var v) = S.insert v
    considerAlive _         = id

    considerDead :: Q.Arg -> S.Set Q.Var -> S.Set Q.Var
    considerDead (Q.Var v) = S.delete v
    considerDead _         = id

    call :: S.Set Q.Var -> Q.Fun -> [Q.Arg] -> Z ()
    call vs f as = do
        forM_ (reverse as) $ \a -> do
            v <- locate a
            emitC (pushl v) $ P.printArg a
        forM_ as $ freeDesc
        mapM_ (spill vs) =<< filterM (dirtyReg vs) callerSaveRegs
        emit $ calll f
        when (not $ null as) $
            emit $ addl (con $ fromIntegral $ 4 * length as) (reg esp)
        mapM_ clear callerSaveRegs

    chooseRegister ::
        [Reg]
    --  ^ Exclude these registers
     -> S.Set Q.Var
    --  ^ These vars need to be saved
     -> Maybe Q.Var
    --  ^ Optimize choice of register if this variable lives through a function call
     -> Maybe Q.Var
    --  ^ Optimize choice if this variable is already in reg
     -> Z Reg
    chooseRegister exclude alives finalVar immediateVar =
        last <$> sortWithM (\r -> sequence
            [ fromEnum . not <$> dirtyReg alives r
            , fromEnum . (== Just r) <$> getReg'' immediateVar
            , fromEnum <$> isFree r
            , pure $ (\b -> (fromEnum . if b then id else not) (calleeSave r)) $ passesCall' finalVar
            , minimum . (999999:) . catMaybes . map getNextUse <$> getVars r -- temporary solution...  I hope
            ]) (generalRegs \\ exclude)

    freeDesc :: MonadState BlockSt m => Q.Arg -> m ()
    freeDesc (Q.Var v) = unless (alive avs v) $ remove v
    freeDesc _ = pure ()

    calleeSave :: Reg -> Bool
    calleeSave = flip elem [ebx, esi, edi]

    save :: Reg -> Z ()
    save r = zoom funSt $ savedRegs %= S.insert r

    passesCall' :: Maybe Q.Var -> Bool
    passesCall' (Just v) = nextUses ^? at v . _Just . passesCall ^. non False
    passesCall' Nothing  = False

    getReg'' :: MonadState BlockSt m => Maybe Q.Var -> m (Maybe Reg)
    getReg'' Nothing = pure Nothing
    getReg'' (Just v) = getReg v

    getNextUse :: Q.Var -> Maybe Int
    getNextUse v = nextUses ^? at v . _Just . nextUse

saveEndLoc :: Z ()
saveEndLoc = do
    endLoc <- reader endLoc
    let alives = M.keysSet endLoc
    regs <- (callerSaveRegs <>) <$> getSavedRegs
    regs' <- filterM (fmap not . dirtyReg alives) regs
    r1 <- case regs' of
        r:_ -> pure r
        [] -> do
            spill alives eax
            clear eax
            pure eax
    let r2 = head $ regs \\ [r1]
    juggle r2 r1
    flip M.traverseWithKey endLoc $ moveLoc r1 r2
    pure ()
  where
    -- Move a variable to its final destination if needed, using the two given registers
    moveLoc :: Reg -> Reg -> Q.Var -> Memloc -> Z ()
    moveLoc r1 r2 v l = assert . (\(v1, v2) -> null v1 && null v2) <$> ((,) <$> getVars r1 <*> getVars r2) <*> do
        (ls, r) <- getLoc v
        assert (not (null ls) || isJust r) $ when (not $ elem l ls) $ do
            r1 <- case r of
                Nothing -> do
                    let l' = head ls
                    emit $ movl (mem l') (reg r1)
                    copy l' r1
                    pure r1
                Just r -> pure r

            emit $ "# save " <> v <> " (end of block)"
            saveRegToMem r1 l r2
            juggle r2 r1

    juggle :: Reg -> Reg -> Z ()
    --        ^ Move a variable from this reg (if any) to its final destination (if it's not there) and clear the reg
    juggle r1 r2 = assert . null <$> getVars r2 <*> do
        vs <- getVars r1
        varsToMove vs >>= \case
            [] -> clear r1
            (v,l):_ -> do
                emit $ "# save " <> v <> " (end of block)"
                saveRegToMem r1 l r2
                juggle r2 r1

    -- move contents of reg to a memloc using the other reg as backup for the memloc's contents and clear the reg
    saveRegToMem :: Reg -> Memloc -> Reg -> Z ()
    saveRegToMem r l r' = do
            whenM (not . null <$> (varsToMove =<< getVars l)) $ do
                emit $ movl (mem l) (reg r')
                copy l r'

            emit (movl (reg r) (mem l))
            copy r l

            clear r

    varsToMove :: [Q.Var] -> Z [(Q.Var, Memloc)]
    varsToMove = filterM (\(v,l) -> not . elem l <$> getMems v) <=< mapM (\v -> (v, ) <$> getEndLoc v)

    getEndLoc :: Q.Var -> Z Memloc
    getEndLoc v = reader endLoc <&> \el -> let l = el ^. at v in assert (isJust l) $ fromJust l

    getSavedRegs :: Z [Reg]
    getSavedRegs = zoom funSt $ use $ savedRegs . to S.toList

emit :: MonadWriter [Entry] m => Entry -> m ()
emit = tell . pure

emitC :: MonadWriter [Entry] m => Entry -> String -> m ()
emitC e c = tell . pure $ e <> " # " <> c

callerSaveRegs :: [Reg]
callerSaveRegs = [eax, ecx, edx]

-- dirtyVars :: (MonadState BlockSt m, IsLoc a) => S.Set Q.Var -> a -> m [Q.Var]
-- dirtyVars vs r = getVars r >>= filterM (dirtyVar vs)

spill :: S.Set Q.Var -> Reg -> Z ()
--       ^ When choosing memory, aim to put these vars in their final destinations
spill vs r = do
    dvs <- filterM (dirtyVar vs) =<< getVars r
    m <- chooseMem dvs

    emitC (movl (reg r) (mem m)) $ "save vars: " <> P.pVars dvs
    move r m

chooseMem :: [Q.Var] -> Z Memloc
--           ^ Prefer memory locations which are final destinations (after the block) of one of these vars
chooseMem vs = do
    mems <- use $ funSt . memLocs
    mems' <- filterM (\m -> getVars m >>= allM
        (\v -> orM [any (/= m) <$> getMems v, isJust <$> getReg v])) mems
    case mems' of
        []    -> newMem
        mems' -> last <$> sortWithM (\m -> sequence
            [ anyM (\v -> andM [{-reachesEnd v, -}(== Just m) . M.lookup v <$> reader endLoc]) vs
            , not . elem m . M.elems <$> reader endLoc
            ]) mems'

-- reachesEnd :: MonadReader BlockEnv m => Q.Var -> m Bool
-- reachesEnd v = reader blockLen <&> \i ->
--     nextUses ^? at v . _Just . lastUse . to (== i + 1) ^. non False

newMem :: Z Memloc
newMem = zoom funSt $ do
    m <- Memloc <$> use nextOffset
    nextOffset -= 4
    memLocs %= (m :)
    pure m

--locate :: MonadState BlockSt m => Q.Arg -> m Arg
locate :: Q.Arg -> Z Arg
locate (Q.ConstI i) = pure $ AConst i
locate (Q.Var v) = getReg v >>= \case
    Just r -> pure $ reg r
    _      -> getMems v >>= \m -> assert (not $ null m) $ pure $ mem $ head m

dirtyVar :: MonadState BlockSt m => S.Set Q.Var -> Q.Var -> m Bool
dirtyVar alives v = if not $ alive alives v then pure False else
    null . fst . (\l -> assert (isJust l) $ fromJust l) <$> (use $ loc . at v)

dirtyReg :: MonadState BlockSt m => S.Set Q.Var -> Reg -> m Bool
dirtyReg alives r = getVars r >>= anyM (dirtyVar alives)

alive :: S.Set Q.Var -> Q.Var -> Bool
alive = flip S.member

remove :: MonadState BlockSt m => Q.Var -> m ()
remove v = do
    mapM_ (delVar v) =<< getLocs v
    assert . (== ([], Nothing)) <$> getLoc v <*> pure ()

clear :: (MonadState BlockSt m, IsLoc a) => a -> m ()
clear l = do
    mapM_ (flip delVar l) =<< getVars l
    assert . (== []) <$> getVars l <*> pure ()

copy :: (MonadState BlockSt m, IsLoc a, IsLoc b) => a -> b -> m ()
copy s d = do
    clear d
    mapM_ (flip addVar d) =<< getVars s

move :: (MonadState BlockSt m, IsLoc a, IsLoc b) => a -> b -> m ()
move s d = do
    clear d
    vs <- getVars s
    clear s
    mapM_ (flip addVar d) vs

addVarR :: MonadState BlockSt m => Q.Var -> Reg -> m ()
addVarR v r = do
    regVars . at r . non [] %= (union [v])
    assert . isNothing <$> getReg v <*> setReg (Just r) v

delVarR :: MonadState BlockSt m => Q.Var -> Reg -> m ()
delVarR v r = do
    regVars . ix r %= delete v
    assert . (== Just r) <$> getReg v <*> setReg Nothing v

getVarsR :: MonadState BlockSt m => Reg -> m [Q.Var]
getVarsR r = use $ regVars . at r . non []

addVarM :: MonadState BlockSt m => Q.Var -> Memloc -> m ()
addVarM v m = do
    memVars . at m . non [] %= (union [v])
    addMem m v

delVarM :: MonadState BlockSt m => Q.Var -> Memloc -> m ()
delVarM v m = do
    memVars . ix m %= delete v
    delMem m v

getVarsM :: MonadState BlockSt m => Memloc -> m [Q.Var]
getVarsM m = use $ memVars . at m . non []

getReg' :: MonadState BlockSt m => Q.Arg -> m (Maybe Reg)
getReg' (Q.Var v) = getReg v
getReg' _         = pure Nothing

getReg :: MonadState BlockSt m => Q.Var -> m (Maybe Reg)
getReg = fmap snd . getLoc

setReg :: MonadState BlockSt m => Maybe Reg -> Q.Var -> m ()
setReg r v = loc . at v . non ([], Nothing) %= second (const r)

getMems :: MonadState BlockSt m => Q.Var -> m [Memloc]
getMems = fmap fst . getLoc

addMem :: MonadState BlockSt m => Memloc -> Q.Var -> m ()
addMem m v = loc . at v . non ([], Nothing) . _1 %= (union [m])

delMem :: MonadState BlockSt m => Memloc -> Q.Var -> m ()
delMem m v = loc . ix v . _1 %= delete m

getLocs :: MonadState BlockSt m => Q.Var -> m [Loc]
getLocs v = (\(m, r) -> catMaybes [Loc <$> r] <> map Loc m) <$> getLoc v

getLoc :: MonadState BlockSt m => Q.Var -> m ([Memloc], Maybe Reg)
getLoc v = use $ loc . at v . non ([], Nothing)

setLoc :: MonadState BlockSt m => ([Memloc], Maybe Reg) -> Q.Var -> m ()
setLoc l v = loc . at v . non ([], Nothing) .= l

isFree :: MonadState BlockSt m => Reg -> m Bool
isFree r = null <$> getVarsR r

label :: Q.Label -> Entry
label = (<> ":") . Q.name

jmp :: Q.Label -> Entry
jmp = ("jmp " <>) . Q.name

jop :: Q.RelOp -> Q.Label -> Entry
jop op l = (case op of
    Q.LT -> "jl"
    Q.LE -> "jle"
    Q.GT -> "jg"
    Q.GE -> "jge"
    Q.EQ -> "je"
    Q.NE -> "jne") <> " " <> Q.name l

pushl :: Arg -> Entry
pushl = unOp "pushl"

popl :: Arg -> Entry
popl = unOp "popl"

idivl :: Arg -> Entry
idivl = unOp "idivl"

movl :: Arg -> Arg -> Entry
movl = binOp "movl"

leal :: Arg -> Arg -> Entry
leal = binOp "leal"

addl :: Arg -> Arg -> Entry
addl = oper Q.Plus

subl :: Arg -> Arg -> Entry
subl = oper Q.Minus

cmpl :: Arg -> Arg -> Entry
cmpl = binOp "cmpl"

oper :: Q.BinOp -> Arg -> Arg -> Entry
oper op = binOp (case op of
    Q.Plus  -> "addl"
    Q.Minus -> "subl"
    Q.Times -> "imull"
    Q.Xor   -> "xorl")

calll :: Q.Fun -> Entry
calll f = "calll " <> f

cdq :: Entry
cdq = "cdq"

binOp :: String -> Arg -> Arg -> Entry
binOp op a1 a2 = op <> " " <> printArg a1 <> ", " <> printArg a2

unOp :: String -> Arg -> Entry
unOp op a = op <> " " <> printArg a

printArg :: Arg -> String
printArg = \case
    AReg (Reg s)      -> "%" <> s
    AMem (Memloc off) -> show off <> "(%ebp)"
    AConst i          -> "$" <> show i
    ALab l            -> Q.name l

retl :: Entry
retl = "retl"

asciz :: String -> Entry
asciz s = ".asciz " <> s

globl :: String -> Entry
globl s = ".globl " <> s

mem :: Memloc -> Arg
mem = AMem

reg :: Reg -> Arg
reg = AReg

con :: Integer -> Arg
con = AConst

lab :: Q.Label -> Arg
lab = ALab

generalRegs :: [Reg]
generalRegs = [eax, ecx, edx, ebx, esi, edi]

ebp :: Reg
ebp = Reg "ebp"

esp :: Reg
esp = Reg "esp"

eax :: Reg
eax = Reg "eax"

ecx :: Reg
ecx = Reg "ecx"

edx :: Reg
edx = Reg "edx"

ebx :: Reg
ebx = Reg "ebx"

esi :: Reg
esi = Reg "esi"

edi :: Reg
edi = Reg "edi"

sortWithM :: (Applicative m, Ord b) => (a -> m b) -> [a] -> m [a]
sortWithM f l = (map fst . sortBy (compare `on` snd)) <$> traverse (\x -> (x, ) <$> f x) l
