{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
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
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Common
import Debug.Trace

import qualified Quad as Q
import qualified PrintQuad as P
import Intermediate.Liveness
import Asm.Entry

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
    [(Q.Quad, Uses)]
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

quad :: Q.Quad -> Uses -> Z ()
quad q uses = {- deb <* traceM ("quad: " ++ P.printQuad q) <* -}case q of
    Q.Assign v (Q.Val (Q.Var v')) -> do
        when (alive alives v && not (v == v')) $ do
            remove v
            mapM_ (addVar v) =<< getLocs v'
        freeDesc alives v'
    Q.Assign v e -> do
        when (alive alives v || hasEffects e) $ do
            l <- expr e (Just v) (P.printQuad q) (M.delete v uses)
            when (alive alives v) $ do
                remove v
                addVar v l
        mapM_ (freeDesc alives) $ vars e
    Q.Jump l -> saveEndLoc *> emit (jmp l)
    Q.Mark l -> emit $ label l
    Q.Exp e -> do
        when (hasEffects e) $
            expr e Nothing (P.printQuad q) uses *> pure ()
        mapM_ (freeDesc alives) $ vars e
    Q.CondJump v1 op v2 l -> do
        let uses' = useVar' v2 uses
            alives' = M.keysSet uses'
        (m1, m2) <- (,) <$> locate v1 <*> locate v2 >>= \case
            (AMem _, AMem _) -> (,) <$> (reg <$> moveToReg [] alives v1 uses') <*> locate v2
            (AConst _, _) -> (,) <$> (reg <$> moveToReg [] alives v1 uses') <*> locate v2
            m -> pure m
        emitWithComment (cmpl m2 m1) $ P.printQuad q
        mapM_ (freeDesc alives) $ vars' [v1, v2]
        saveEndLoc
        emit $ jop op l
  where
    alives :: S.Set Q.Var
    alives = M.keysSet uses

    hasEffects :: Q.Exp -> Bool
    hasEffects (Q.Call _ _) = True
    hasEffects _          = False

moveToReg :: [Reg] -> S.Set Q.Var -> Q.Arg -> Uses -> Z Reg
moveToReg exclude alives v uses = do
    r <- chooseRegister exclude Nothing Nothing uses
    whenM (dirtyReg (M.keysSet uses) r) $ spill alives r

    m <- locate v
    emitWithComment (movl m (reg r)) $ P.printArg v
    case m of
        AConst _ -> clear r
        AReg r'  -> move r' r
        AMem m   -> copy m r

    saveReg r

    pure r

expr :: Q.Exp -> Maybe Q.Var -> String -> Uses -> Z Loc
expr e finalVar comment uses = case e of
    Q.BinInt v1 op v2 | not $ elem op [Q.Div, Q.Mod] -> do
        let uses' = if v1 == v2 then uses else useVar' v2 uses
            alives' = M.keysSet uses'

        r <- chooseRegister [] finalVar (case v1 of { Q.Var v' -> Just v'; _ -> Nothing }) uses'
        whenM (dirtyReg alives' r) $ spill alives r
        unlessM ((== Just r) <$> getReg' v1) $ do
            m <- locate v1
            emitWithComment (movl m (reg r)) $ P.printArg v1
        clear r
        m <- if v1 == v2 then pure $ reg r else locate v2

        emitWithComment (oper op m (reg r)) comment

        saveReg r

        pure $ Loc r
    Q.BinInt v1 op v2 -> do
        let uses' = if v1 == v2 then uses else useVar' v2 uses
            uses'' = useVar' v1 uses'
            alives' = M.keysSet uses'
            alives'' = M.keysSet uses''
        inEax <- (== Just eax) <$> getReg' v1

        m2 <- locate v2 >>= \case
            m@(AMem _)                             -> pure m
            m@(AReg r') | not $ elem r' [eax, edx] -> pure m
            _                                      -> reg <$> moveToReg [eax, edx] alives v2 uses''

        whenM (dirtyReg alives' eax) $ spill alives eax
        -- so that vars from eax don't get spilled over from edx
        clear eax

        unless inEax $ do
            m <- locate v1
            emitWithComment (movl m (reg eax)) $ P.printArg v1

        whenM (dirtyReg alives' edx) $ spill alives edx

        emit $ cdq
        clear edx
        emitWithComment (idivl m2) comment
        pure $ Loc $ case op of
            Q.Div -> eax
            Q.Mod -> edx
    Q.Load l -> do
        r <- chooseRegister [] finalVar Nothing uses
        whenM (dirtyReg alives r) $ spill alives r

        emitWithComment (leal (lab l) (reg r)) comment
        clear r

        saveReg r

        pure $ Loc r
    Q.Val (Q.ConstI i) -> do
        m <- chooseMem $ catMaybes [finalVar]

        emitWithComment (movl (con i) (mem m)) comment
        clear m

        pure $ Loc m
    Q.Call f as -> do
        forM_ (reverse as) $ \a -> do
            v <- locate a
            emitWithComment (pushl v) $ P.printArg a
        mapM_ (freeDesc alives) $ vars' as
        mapM_ (spill alives) =<< filterM (dirtyReg alives) callerSaveRegs
        emit $ calll f
        when (not $ null as) $
            emit $ addl (con $ fromIntegral $ 4 * length as) (reg esp)
        mapM_ clear callerSaveRegs
        pure $ Loc eax
  where
    alives :: S.Set Q.Var
    alives = M.keysSet uses

useVar' v = case v of
    Q.Var v -> useVar 0 v
    _       -> id


chooseRegister ::
    [Reg]
--  ^ Exclude these registers
 -> Maybe Q.Var
--  ^ Optimize choice of register if this variable lives through a function call
 -> Maybe Q.Var
--  ^ Optimize choice if this variable is already in reg
 -> Uses
 -> Z Reg
chooseRegister exclude finalVar immediateVar uses =
    last <$> sortWithM (\r -> sequence
        [ fromEnum . not <$> dirtyReg alives r
        , fromEnum . (== Just r) <$> getReg'' immediateVar
        , fromEnum <$> isFree r
        , pure $ (\b -> (fromEnum . if b then id else not) (calleeSave r)) $ passesCall' finalVar
        , minimum . (999999:) . catMaybes . map getNextUse <$> getVars r -- temporary solution...  I hope
        ]) (generalRegs \\ exclude)
  where
    alives = M.keysSet uses

    getReg'' :: MonadState BlockSt m => Maybe Q.Var -> m (Maybe Reg)
    getReg'' Nothing = pure Nothing
    getReg'' (Just v) = getReg v

    getNextUse :: Q.Var -> Maybe Int
    getNextUse v = uses ^? at v . _Just . nextUse

    passesCall' :: Maybe Q.Var -> Bool
    passesCall' (Just v) = uses ^? at v . _Just . passesCall ^. non False
    passesCall' Nothing  = False

calleeSave :: Reg -> Bool
calleeSave = flip elem [ebx, esi, edi]

saveReg :: Reg -> Z ()
saveReg r = when (calleeSave r) $ zoom funSt $ savedRegs %= S.insert r

freeDesc :: MonadState BlockSt m => S.Set Q.Var -> Q.Var -> m ()
freeDesc alives v = unless (alive alives v) $ remove v

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

            emitComment $ "save " <> v <> " (end of block)"
            regToMem r1 l r2
            juggle r2 r1

    juggle :: Reg -> Reg -> Z ()
    --        ^ Move a variable from this reg (if any) to its final destination (if it's not there) and clear the reg
    juggle r1 r2 = assert . null <$> getVars r2 <*> do
        vs <- getVars r1
        varsToMove vs >>= \case
            [] -> clear r1
            (v,l):_ -> do
                emitComment $ "save " <> v <> " (end of block)"
                regToMem r1 l r2
                juggle r2 r1

    -- move contents of reg to a memloc using the other reg as backup for the memloc's contents and clear the reg
    regToMem :: Reg -> Memloc -> Reg -> Z ()
    regToMem r l r' = do
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

callerSaveRegs :: [Reg]
callerSaveRegs = [eax, ecx, edx]

generalRegs :: [Reg]
generalRegs = [eax, ecx, edx, ebx, esi, edi]

spill :: S.Set Q.Var -> Reg -> Z ()
--       ^ When choosing memory, aim to put these vars in their final destinations
spill vs r = do
    dvs <- filterM (dirtyVar vs) =<< getVars r
    m <- chooseMem dvs

    emitWithComment (movl (reg r) (mem m)) $ "save vars: " <> P.pVars dvs
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

locate :: MonadState BlockSt m => Q.Arg -> m Arg
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
    regVars . at r . non [] %= delete v
    assert . (== Just r) <$> getReg v <*> setReg Nothing v

getVarsR :: MonadState BlockSt m => Reg -> m [Q.Var]
getVarsR r = use $ regVars . at r . non []

addVarM :: MonadState BlockSt m => Q.Var -> Memloc -> m ()
addVarM v m = do
    memVars . at m . non [] %= (union [v])
    addMem m v

delVarM :: MonadState BlockSt m => Q.Var -> Memloc -> m ()
delVarM v m = do
    memVars . at m . non [] %= delete v
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
delMem m v = loc . at v . non ([], Nothing) . _1 %= delete m

getLocs :: MonadState BlockSt m => Q.Var -> m [Loc]
getLocs v = (\(m, r) -> catMaybes [Loc <$> r] <> map Loc m) <$> getLoc v

getLoc :: MonadState BlockSt m => Q.Var -> m ([Memloc], Maybe Reg)
getLoc v = use $ loc . at v . non ([], Nothing)

setLoc :: MonadState BlockSt m => ([Memloc], Maybe Reg) -> Q.Var -> m ()
setLoc l v = loc . at v . non ([], Nothing) .= l

isFree :: MonadState BlockSt m => Reg -> m Bool
isFree r = null <$> getVarsR r
