{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
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

import qualified Quad as Q

type Entry = String

newtype Reg = Reg { name :: String }
    deriving (Eq, Ord)

type Offset = Integer

newtype Memloc = Memloc { offset :: Integer }
    deriving (Eq, Ord)

data Arg
    = AReg Reg
    | AMem Memloc
    | AConst Integer

newtype Env = Env
    { crossVarLocs :: M.Map Q.Var Memloc
--    ^ Memory locations of cross variables, i.e. variables alive between blocks
    }

data SEnv = SEnv
    { _memLocs    :: [Memloc]
    , _nextOffset :: Offset
    , _savedRegs  :: S.Set Reg
    }

data BEnv = BEnv
    { endLoc   :: M.Map Q.Var Memloc
    , blockLen :: Int
    }

data Desc = Desc
    { _loc      :: M.Map Q.Var ([Memloc], Maybe Reg)
    , _memVars  :: M.Map Memloc [Q.Var]
    , _regVars  :: M.Map Reg [Q.Var]
    , _funState :: SEnv
    }

data Use = Use
    { _nextUse    :: Int
    , _lastUse    :: Int
    , _passesCall :: Bool
    }

makeLenses ''SEnv
makeLenses ''Desc
makeLenses ''Use

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
    [ label name
    , pushl (reg ebp)
    , movl (reg esp) (reg ebp)
    , subl (con memSize) (reg esp)
    ]
    <> map (pushl . reg) savedRegs
    <> code
    <> retValCode
    <> map (popl . reg) savedRegs
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

    (SEnv{..}, code) = execRWS
        (forM blocks $ \(b, aliveEnd) ->
            let (b', aliveBegin) = nextUses b aliveEnd in block b' aliveBegin aliveEnd)
            Env{..}
            SEnv
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
        (M.empty, [], fromIntegral $ 4 * (1 + length args)) $ reverse args
    crossVars = (S.insert Q.retVar) . S.unions . map (snd . uncurry nextUses) $ blocks

-- Compute next Uses of each variable after each quad and set of vars alive at the beginning of block
-- given the set of vars alive at the end of the block
nextUses :: [Q.Quad] -> S.Set Q.Var -> ([(Q.Quad, M.Map Q.Var Use)], S.Set Q.Var)
nextUses qs aliveEnd = (reverse *** (M.keysSet . fst)) . flip runState start . forM (reverse qs) $ \q -> do
    (u, i) <- get
    let u' = if isCall q then markCall u else u
    put $ (update i q u', i-1)
    pure (q, u)
  where
    update :: Int -> Q.Quad -> M.Map Q.Var Use -> M.Map Q.Var Use
    update i = \case
        Q.Assign v e         -> flip (foldr $ use i) (vars e) . kill v
        Q.CondJump a1 _ a2 _ -> flip (foldr $ use i) $ vars' [a1, a2]
        Q.Exp e              -> flip (foldr $ use i) $ vars e
        _                    -> id

    use :: Int -> Q.Var -> M.Map Q.Var Use -> M.Map Q.Var Use
    use i v = at v %~ maybe (Just $ Use i i False) (Just . (nextUse .~ i))

    kill :: Q.Var -> M.Map Q.Var Use -> M.Map Q.Var Use
    kill = M.delete

    isCall :: Q.Quad -> Bool
    isCall = \case
        Q.Assign _ (Q.Call _ _) -> True
        Q.Exp (Q.Call _ _)      -> True
        _                       -> False

    markCall :: M.Map Q.Var Use -> M.Map Q.Var Use
    markCall = M.map $ passesCall .~ True

    vars :: Q.Exp -> [Q.Var]
    vars = \case
        Q.BinInt a1 _ a2 -> vars' [a1, a2]
        Q.AddStr a1 a2   -> vars' [a1, a2]
        Q.Val a          -> vars' [a]
        Q.Call _ as      -> vars' as

    vars' :: [Q.Arg] -> [Q.Var]
    vars' = concatMap $ \case
        Q.Var v -> [v]
        _       -> []

    start :: (M.Map Q.Var Use, Int)
    start = (M.fromSet (const $ Use (cnt + 1) (cnt + 1) False) aliveEnd, cnt)

    cnt :: Int
    cnt = length qs

block ::
    [(Q.Quad, M.Map Q.Var Use)]
--  ^ List of quads and next use of each variable after each quad
--    represented as positions in the list
 -> S.Set Q.Var
--  ^ Variables alive at the beginning of this block
 -> S.Set Q.Var
--  ^ Variables alive at the end of this block
 -> RWS Env [Entry] SEnv ()
block qs aliveBegin aliveEnd = do
    cvls <- reader crossVarLocs
    let (startLoc, startMemVars, endLoc) = M.foldrWithKey (\v l (sL, sM, eL) ->
            ( if S.member v aliveBegin then M.insert v ([l], Nothing) sL else sL
            , if S.member v aliveBegin then M.insert l [v] sM else sM
            , if S.member v aliveEnd then M.insert v l eL else eL)) (M.empty, M.empty, M.empty) cvls
        blockLen = length qs
    desc <- Desc startLoc startMemVars M.empty <$> get
    let (Desc{..}, code) = execRWS (mapM (uncurry quad) qs) BEnv{..} desc
    tell code
    flip M.traverseWithKey endLoc $ \v l ->
        let (ls, r) = M.findWithDefault ([], Nothing) v _loc
         in assert (not (null ls) || isJust r) $
            when (not $ elem l ls) $
                if isJust r then
                    emit $ movl (reg $ fromJust r) (mem l)
                else do
                    emit $ movl (mem $ head ls) (reg ecx)
                    emit $ movl (reg ecx) (mem l)
    put $ _funState

emit :: MonadWriter [Entry] m => Entry -> m ()
emit = tell . pure

type Z = RWS BEnv [Entry] Desc

quad ::
    Q.Quad
--  ^ Quad to generate assembly for
 -> M.Map Q.Var Use
--  ^ Use of each variable after this quad
 -> Z ()
quad q nextUses = case q of
    Q.Assign v (Q.BinInt v1 op v2) | op /= Q.Div && op /= Q.Mod -> do
        let avs' = considerDead (Q.Var v) avs
            avs'' = if v1 == v2 then avs' else considerAlive v2 avs'
        when (alive avs v) $ do
            r <- chooseRegister avs'' $ Just v
            whenM (dirtyReg avs'' r) $ spill avs' r
            unlessM ((== Just r) <$> getReg' v1) $ do
                m <- locate v1
                emit $ movl m (reg r)
            m <- locate v2
            emit $ oper op m (reg r)

            mapM_ (setReg Nothing) =<< getVarsR r
            setVarsR [v] r
            setLoc ([], Just r) v

            when (calleeSave r) $ save r
        freeDesc v1
        freeDesc v2
    Q.Assign v (Q.BinInt v1 op v2) -> do
        let avs' = considerDead (Q.Var v) avs
            avs'' = if v1 == v2 then avs' else considerAlive v2 avs'
        when (alive avs v) $ do
            whenM (dirtyReg avs'' eax) $ spill avs' eax
            unlessM ((== Just eax) <$> getReg' v1) $ do
                m <- locate v1
                emit $ movl m (reg eax)
            whenM (dirtyReg avs'' edx) $ spill avs' edx
            emit $ cdq
            m <- locate v2
            emit $ idivl m

            mapM_ (setReg Nothing) =<< getVarsR eax
            mapM_ (setReg Nothing) =<< getVarsR edx
            let r = case op of
                        Q.Div -> eax
                        Q.Mod -> edx
            setVarsR [v] r
            setLoc ([], Just r) v
        freeDesc v1
        freeDesc v2
    Q.Assign v (Q.Val (Q.Var v')) -> do
        when (alive avs v) $ do
            mapM_ (addVarR v) =<< getReg v'
            mapM_ (addVarM v) =<< getMems v'
            flip setLoc v =<< getLoc v'
        freeDesc $ Q.Var v'
    Q.Assign v (Q.Val (Q.ConstI i)) -> do
        when (alive avs v) $ do
            m <- chooseMem [v]
            emit $ movl (con i) (mem m)
            setLoc ([m], Nothing) v
    Q.Assign v (Q.Call f as) -> do
        let avs' = considerDead (Q.Var v) avs
        call avs' f as
        when (alive avs v) $ do
            setVarsR [v] eax
            setLoc ([], Just eax) v
    Q.Jump l -> emit $ jmp l
    Q.Mark l -> emit $ label l
    Q.CondJump v1 op v2 l -> do
        let avs' = considerAlive v2 avs
        m1 <- locate v1
        m2 <- locate v2
        (m1, m2) <- case (m1, m2) of
            (AMem _, AMem _) -> do
                r <- chooseRegister avs' (Just $ case v1 of Q.Var v -> v)
                whenM (dirtyReg avs' r) $ spill avs r
                AMem m <- locate v1
                emit $ movl (mem m) (reg r)

                mapM_ (setReg Nothing) =<< getVarsR r
                setVarsR [] r
                mapM_ (\v -> setReg (Just r) v *> addVarR v r) =<< getVarsM m

                when (calleeSave r) $ save r

                (reg r, ) <$> locate v2
            (AConst _, _) -> do
                r <- chooseRegister avs' Nothing
                whenM (dirtyReg avs' r) $ spill avs r
                emit $ movl m1 (reg r)

                mapM_ (setReg Nothing) =<< getVarsR r
                setVarsR [] r

                when (calleeSave r) $ save r

                (reg r, ) <$> locate v2
            (_, _) -> pure (m1, m2)
        emit $ cmpl m2 m1
        emit $ jop op l
        freeDesc v1
        freeDesc v2
    Q.Exp (Q.Call f as) -> call avs f as
    Q.Exp _ -> pure ()
  where
    avs :: S.Set Q.Var
    avs = M.keysSet nextUses

    alive :: S.Set Q.Var -> Q.Var -> Bool
    alive = flip S.member

    considerAlive :: Q.Arg -> S.Set Q.Var -> S.Set Q.Var
    considerAlive (Q.Var v) = S.insert v
    considerAlive _         = id

    considerDead :: Q.Arg -> S.Set Q.Var -> S.Set Q.Var
    considerDead (Q.Var v) = S.delete v
    considerDead _         = id

    call :: S.Set Q.Var -> Q.Fun -> [Q.Arg] -> Z ()
    call vs f as = do
        forM_ (reverse as) $ \a -> locate a >>= (emit . pushl)
        forM_ as $ freeDesc
        mapM_ (spill avs) =<< filterM (dirtyReg vs) callerSaveRegs
        emit $ calll f
        emit $ addl (con $ fromIntegral $ 4 * length as) (reg esp)

    spill :: S.Set Q.Var -> Reg -> Z ()
    --       ^ When choosing memory, aim to put these vars in their final destinations
    spill vs r = do
        m <- chooseMem =<< filterM (dirtyVar vs) =<< getVarsR r
        emit $ movl (reg r) (mem m)

        mapM_ (delMem m) =<< getVarsM m
        setVarsM [] m

        mapM_ (\v -> addMem m v *> addVarM v m) =<< getVarsR r

    chooseRegister :: MonadState Desc m => S.Set Q.Var -> Maybe Q.Var -> m Reg
    --                                     ^ These vars need to be saved
    chooseRegister vs v = last <$> sortWithM (\r -> sequence
        [ fromEnum <$> isFree r
        , fromEnum . not <$> dirtyReg vs r
        , pure $ (\b -> (fromEnum . if b then id else not) (calleeSave r)) $ passesCall' v
        , minimum . (999999:) . catMaybes . map getNextUse <$> getVarsR r -- temporary solution...  I hope
        ]) generalRegs

    chooseMem :: [Q.Var] -> Z Memloc
    --           ^ Prefer memory locations which are final destinations (after the block) of one of these vars
    chooseMem vs = do
        mems <- use $ funState . memLocs
        mems' <- filterM (\m -> getVarsM m >>= allM
            (\v -> orM [any (/= m) <$> getMems v, isJust <$> getReg v])) mems
        case mems' of
            []    -> newMem
            mems' -> last <$> sortWithM (\m -> sequence
                [ anyM (\v -> andM [reachesEnd v, (== Just m) . M.lookup v <$> reader endLoc]) vs
                , not . elem m . M.elems <$> reader endLoc
                ]) mems'

    dirtyReg :: MonadState Desc m => S.Set Q.Var -> Reg -> m Bool
    dirtyReg vs r = getVarsR r >>= anyM (dirtyVar vs)

    dirtyVar :: MonadState Desc m => S.Set Q.Var -> Q.Var -> m Bool
    dirtyVar vs v = if not $ alive vs v then pure False else
        null . fst . (\l -> assert (M.member v l) $ l M.! v) <$> use loc

    locate :: MonadState Desc m => Q.Arg -> m Arg
    locate (Q.ConstI i) = pure $ AConst i
    locate (Q.Var v) = getReg v >>= \case
        Just r -> pure $ reg r
        _      -> getMems v >>= \m -> assert (not $ null m) $ pure $ mem $ head m

    reachesEnd :: MonadReader BEnv m => Q.Var -> m Bool
    reachesEnd v = reader blockLen <&> \i ->
        nextUses ^? at v . _Just . lastUse . to (== i + 1) ^. non False

    newMem :: Z Memloc
    newMem = zoom funState $ do
        m <- Memloc <$> use nextOffset
        nextOffset += 4
        memLocs %= (m :)
        pure m

    freeDesc :: MonadState Desc m => Q.Arg -> m ()
    freeDesc (Q.Var v) = unless (alive avs v) $ do
        mapM_ (delVarR v) =<< getReg v
        mapM_ (delVarM v) =<< getMems v
        setLoc ([], Nothing) v
    freeDesc _ = pure ()

    calleeSave :: Reg -> Bool
    calleeSave = flip elem [ebx, esi, edi]

    callerSaveRegs :: [Reg]
    callerSaveRegs = [eax, ecx, edx]

    save :: Reg -> Z ()
    save r = zoom funState $ savedRegs %= S.insert r

    isFree :: MonadState Desc m => Reg -> m Bool
    isFree r = null <$> getVarsR r

    passesCall' :: Maybe Q.Var -> Bool
    passesCall' (Just v) = nextUses ^? at v . _Just . passesCall ^. non False
    passesCall' Nothing  = False

    getNextUse :: Q.Var -> Maybe Int
    getNextUse v = nextUses ^? at v . _Just . nextUse

    getVarsR :: MonadState Desc m => Reg -> m [Q.Var]
    getVarsR r = use $ regVars . at r . non []

    addVarR :: MonadState Desc m => Q.Var -> Reg -> m ()
    addVarR v r = regVars . at r . non [] %= (union [v])

    setVarsR :: MonadState Desc m => [Q.Var] -> Reg -> m ()
    setVarsR vs r = regVars . at r ?= vs

    delVarR :: MonadState Desc m => Q.Var -> Reg -> m ()
    delVarR v r = regVars . ix r %= delete v

    getVarsM :: MonadState Desc m => Memloc -> m [Q.Var]
    getVarsM m = use $ memVars . at m . non []

    addVarM :: MonadState Desc m => Q.Var -> Memloc -> m ()
    addVarM v m = memVars . at m . non [] %= (union [v])

    setVarsM :: MonadState Desc m => [Q.Var] -> Memloc -> m ()
    setVarsM vs m = memVars %= M.insert m vs

    delVarM :: MonadState Desc m => Q.Var -> Memloc -> m ()
    delVarM v m = memVars . ix m %= delete v

    getReg' :: MonadState Desc m => Q.Arg -> m (Maybe Reg)
    getReg' (Q.Var v) = getReg v
    getReg' _         = pure Nothing

    getReg :: MonadState Desc m => Q.Var -> m (Maybe Reg)
    getReg = fmap snd . getLoc

    setReg :: MonadState Desc m => Maybe Reg -> Q.Var -> m ()
    setReg r v = loc . at v . non ([], Nothing) %= second (const r)

    getMems :: MonadState Desc m => Q.Var -> m [Memloc]
    getMems = fmap fst . getLoc

    addMem :: MonadState Desc m => Memloc -> Q.Var -> m ()
    addMem m v = loc . at v . non ([], Nothing) . _1 %= (union [m])

    delMem :: MonadState Desc m => Memloc -> Q.Var -> m ()
    delMem m v = loc . ix v . _1 %= delete m

    getLoc :: MonadState Desc m => Q.Var -> m ([Memloc], Maybe Reg)
    getLoc v = use $ loc . at v . non ([], Nothing)

    setLoc :: MonadState Desc m => ([Memloc], Maybe Reg) -> Q.Var -> m ()
    setLoc l v = loc . at v . non ([], Nothing) .= l

label :: String -> Entry
label = (<> ":")

jmp :: Q.Label -> Entry
jmp = ("jmp " <>)

jop :: Q.RelOp -> Q.Label -> Entry
jop op l = (case op of
    Q.LT -> "jl"
    Q.LE -> "jle"
    Q.GT -> "jg"
    Q.GE -> "jge"
    Q.EQ -> "je"
    Q.NE -> "jne") <> " " <> l

pushl :: Arg -> Entry
pushl = unOp "pushl"

popl :: Arg -> Entry
popl = unOp "popl"

idivl :: Arg -> Entry
idivl = unOp "idivl"

movl :: Arg -> Arg -> Entry
movl = binOp "movl"

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
    Q.Times -> "mull"
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

retl :: Entry
retl = "retl"

mem :: Memloc -> Arg
mem = AMem

reg :: Reg -> Arg
reg = AReg

con :: Integer -> Arg
con = AConst

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
